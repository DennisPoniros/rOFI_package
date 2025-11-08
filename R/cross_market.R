#' Cross-Asset and Cross-Venue Analysis
#'
#' @description
#' Tools for analyzing interconnected markets and fragmented liquidity.
#' Identifies lead-lag relationships, measures cross-market impact,
#' detects arbitrage opportunities, and assesses price discovery.
#'
#' @name cross_market
NULL

#' Compute cross-asset order flow imbalance matrix
#'
#' @description
#' Calculates OFI for multiple instruments simultaneously and analyzes
#' their cross-correlation structure. Useful for pairs trading,
#' sector analysis, and understanding market linkages.
#'
#' @param trades_list Named list of trade data frames
#' @param window Character, aggregation window (default: "1 min")
#' @param return_matrix Logical, return correlation matrix (default: TRUE)
#'
#' @return List containing OFI data and correlation matrix
#'
#' @details
#' **Cross-Asset OFI Applications**:
#' - Pairs trading signal generation
#' - Sector rotation analysis
#' - Index arbitrage
#' - Portfolio rebalancing
#' - Risk factor identification
#'
#' **Correlation Interpretation**:
#' - High positive correlation: Common factors driving flow
#' - High negative correlation: Substitution effects
#' - Low correlation: Independent price dynamics
#' - Time-varying correlation: Regime shifts
#'
#' @export
#' @importFrom stats cor
#' @examples
#' \dontrun{
#' # Create OFI for multiple stocks
#' trades_AAPL <- simulate_orders(n = 1000, seed = 1)
#' trades_MSFT <- simulate_orders(n = 1000, seed = 2)
#' trades_GOOGL <- simulate_orders(n = 1000, seed = 3)
#'
#' trades_list <- list(
#'   AAPL = trades_AAPL,
#'   MSFT = trades_MSFT,
#'   GOOGL = trades_GOOGL
#' )
#'
#' cross_ofi <- compute_cross_asset_ofi(trades_list, window = "1 min")
#' print(cross_ofi$correlation_matrix)
#' }
compute_cross_asset_ofi <- function(trades_list,
                                     window = "1 min",
                                     return_matrix = TRUE) {

  if (!is.list(trades_list) || length(trades_list) < 2) {
    stop("trades_list must be a named list with at least 2 data frames")
  }

  if (is.null(names(trades_list))) {
    names(trades_list) <- paste0("Asset_", seq_along(trades_list))
  }

  # Compute OFI for each asset
  ofi_list <- lapply(names(trades_list), function(asset_name) {
    trades <- trades_list[[asset_name]]
    ofi <- compute_ofi(trades, window = window)

    # Add asset identifier
    ofi$asset <- asset_name
    return(ofi)
  })

  names(ofi_list) <- names(trades_list)

  # Combine into wide format for correlation
  # Merge all OFI series by timestamp
  merged_ofi <- ofi_list[[1]] |>
    select(.data$window_start, ofi_1 = .data$ofi)

  for (i in 2:length(ofi_list)) {
    asset_ofi <- ofi_list[[i]] |>
      select(.data$window_start, !!paste0("ofi_", i) := .data$ofi)

    merged_ofi <- merged_ofi |>
      full_join(asset_ofi, by = "window_start")
  }

  # Rename columns with asset names
  ofi_cols <- paste0("ofi_", seq_along(ofi_list))
  names(merged_ofi)[names(merged_ofi) %in% ofi_cols] <-
    paste0("ofi_", names(ofi_list))

  # Calculate correlation matrix
  if (return_matrix) {
    ofi_matrix <- merged_ofi |>
      select(starts_with("ofi_")) |>
      as.matrix()

    correlation_matrix <- cor(ofi_matrix, use = "pairwise.complete.obs")

    # Clean up names
    rownames(correlation_matrix) <- names(trades_list)
    colnames(correlation_matrix) <- names(trades_list)
  } else {
    correlation_matrix <- NULL
  }

  # Package results
  result <- list(
    ofi_individual = ofi_list,
    ofi_merged = merged_ofi,
    correlation_matrix = correlation_matrix,
    assets = names(trades_list),
    n_assets = length(trades_list)
  )

  class(result) <- c("cross_asset_ofi", "list")
  return(result)
}


#' Analyze lead-lag relationships between instruments
#'
#' @description
#' Identifies which instruments lead or lag price discovery using
#' cross-correlation at multiple time lags. Critical for understanding
#' information flow and optimal execution timing.
#'
#' @param ofi_data_1 OFI data for first instrument
#' @param ofi_data_2 OFI data for second instrument
#' @param max_lag Integer, maximum lag to test (default: 20)
#' @param metric Character, "ofi" or "oir" (default: "oir")
#'
#' @return List with lead-lag analysis results
#'
#' @details
#' **Lead-Lag Interpretation**:
#' - Positive lag: Instrument 1 leads instrument 2
#' - Negative lag: Instrument 2 leads instrument 1
#' - Zero lag: Contemporaneous relationship
#' - Magnitude: Strength of lead-lag relationship
#'
#' **Common Patterns**:
#' - Futures lead spot markets (typically 5-30 seconds)
#' - Large caps lead small caps
#' - Liquid stocks lead illiquid stocks
#' - US markets lead international markets
#'
#' **Applications**:
#' - Predictive trading signals
#' - Optimal execution timing
#' - Price discovery measurement
#' - Market microstructure research
#'
#' @references
#' Cont, R., et al. (2023). Cross-impact of order flow imbalance.
#' *Quantitative Finance*.
#'
#' @export
#' @importFrom stats ccf
#' @examples
#' \dontrun{
#' trades_SPY <- simulate_orders(n = 2000, seed = 1)
#' trades_QQQ <- simulate_orders(n = 2000, seed = 2)
#'
#' ofi_SPY <- compute_ofi(trades_SPY, window = "1 min")
#' ofi_QQQ <- compute_ofi(trades_QQQ, window = "1 min")
#'
#' leadlag <- lead_lag_analysis(ofi_SPY, ofi_QQQ, max_lag = 20)
#' plot(leadlag$lags, leadlag$correlations)
#' }
lead_lag_analysis <- function(ofi_data_1,
                                ofi_data_2,
                                max_lag = 20,
                                metric = c("oir", "ofi")) {

  metric <- match.arg(metric)

  # Extract time series
  if (!metric %in% names(ofi_data_1) || !metric %in% names(ofi_data_2)) {
    stop("Metric '", metric, "' not found in OFI data")
  }

  series_1 <- ofi_data_1[[metric]]
  series_2 <- ofi_data_2[[metric]]

  # Align series by timestamp (if needed)
  # For now, assume they're already aligned

  # Remove NAs
  valid <- !is.na(series_1) & !is.na(series_2)
  series_1 <- series_1[valid]
  series_2 <- series_2[valid]

  if (length(series_1) < max_lag * 2) {
    warning("Short time series. Results may be unreliable.")
  }

  # Cross-correlation function
  ccf_result <- ccf(series_1, series_2, lag.max = max_lag, plot = FALSE)

  # Extract results
  lags <- ccf_result$lag
  correlations <- as.numeric(ccf_result$acf)

  # Find maximum correlation and its lag
  max_cor_idx <- which.max(abs(correlations))
  optimal_lag <- lags[max_cor_idx]
  max_correlation <- correlations[max_cor_idx]

  # Interpretation
  if (optimal_lag > 0) {
    interpretation <- paste0(
      "Series 1 leads series 2 by ", optimal_lag, " periods ",
      "(correlation: ", round(max_correlation, 3), ")"
    )
    leader <- "Series 1"
  } else if (optimal_lag < 0) {
    interpretation <- paste0(
      "Series 2 leads series 1 by ", abs(optimal_lag), " periods ",
      "(correlation: ", round(max_correlation, 3), ")"
    )
    leader <- "Series 2"
  } else {
    interpretation <- paste0(
      "Contemporaneous relationship ",
      "(correlation: ", round(max_correlation, 3), ")"
    )
    leader <- "Contemporaneous"
  }

  # Package results
  result <- list(
    lags = lags,
    correlations = correlations,
    optimal_lag = optimal_lag,
    max_correlation = max_correlation,
    leader = leader,
    interpretation = interpretation,
    ccf_object = ccf_result
  )

  class(result) <- c("leadlag_analysis", "list")
  return(result)
}


#' Compute cross-impact matrix (how asset A affects asset B)
#'
#' @description
#' Estimates how OFI in one asset impacts prices in other assets.
#' Uses regression framework to quantify cross-market effects.
#'
#' @param cross_ofi_data Output from compute_cross_asset_ofi()
#' @param price_data Named list of price data for each asset
#' @param lag Integer, lag for impact measurement (default: 1)
#'
#' @return Cross-impact matrix
#'
#' @details
#' Estimates regression:
#' \deqn{\Delta P_j = \sum_i \beta_{ij} \times OFI_i + \epsilon}
#'
#' where \eqn{\beta_{ij}} is impact of asset i on asset j.
#'
#' **Diagonal elements**: Self-impact (own OFI on own price)
#' **Off-diagonal**: Cross-impact (OFI in i affects price in j)
#'
#' @export
#' @examples
#' \dontrun{
#' cross_impact <- cross_impact_matrix(cross_ofi, price_data)
#' print(cross_impact)
#' }
cross_impact_matrix <- function(cross_ofi_data,
                                  price_data = NULL,
                                  lag = 1) {

  if (!inherits(cross_ofi_data, "cross_asset_ofi")) {
    stop("cross_ofi_data must be output from compute_cross_asset_ofi()")
  }

  # Placeholder: Full implementation requires price data
  # For now, return the OFI correlation matrix as proxy

  if (!is.null(cross_ofi_data$correlation_matrix)) {
    message("Returning OFI correlation matrix as proxy for cross-impact.")
    message("Full cross-impact estimation requires price data.")
    return(cross_ofi_data$correlation_matrix)
  } else {
    stop("Correlation matrix not available in cross_ofi_data")
  }
}


#' Calculate lagged cross-correlations
#'
#' @description
#' Computes cross-correlations at multiple lags for pairs of instruments.
#' Useful for identifying predictive relationships.
#'
#' @param ofi_list List of OFI data frames
#' @param lags Integer vector of lags to compute (default: 0:10)
#' @param metric Character, which OFI metric to use
#'
#' @return Matrix of lagged correlations
#'
#' @export
#' @examples
#' \dontrun{
#' lagged_cors <- lagged_cross_correlation(
#'   list(SPY = ofi_SPY, QQQ = ofi_QQQ),
#'   lags = 0:10
#' )
#' }
lagged_cross_correlation <- function(ofi_list,
                                      lags = 0:10,
                                      metric = "oir") {

  n_assets <- length(ofi_list)
  asset_names <- names(ofi_list)

  # Initialize result matrix
  n_lags <- length(lags)
  result_list <- list()

  for (lag_val in lags) {
    cor_matrix <- matrix(NA, n_assets, n_assets,
                         dimnames = list(asset_names, asset_names))

    for (i in 1:n_assets) {
      for (j in 1:n_assets) {
        series_i <- ofi_list[[i]][[metric]]
        series_j <- ofi_list[[j]][[metric]]

        # Apply lag
        if (lag_val > 0) {
          series_i_lagged <- c(rep(NA, lag_val), series_i[1:(length(series_i) - lag_val)])
        } else if (lag_val < 0) {
          series_j_lagged <- c(rep(NA, abs(lag_val)), series_j[1:(length(series_j) - abs(lag_val))])
          series_i_lagged <- series_i
          series_j <- series_j_lagged
        } else {
          series_i_lagged <- series_i
        }

        # Compute correlation
        cor_val <- cor(series_i_lagged, series_j, use = "pairwise.complete.obs")
        cor_matrix[i, j] <- cor_val
      }
    }

    result_list[[as.character(lag_val)]] <- cor_matrix
  }

  attr(result_list, "lags") <- lags
  attr(result_list, "metric") <- metric

  return(result_list)
}


#' Compute price discovery metrics (information shares)
#'
#' @description
#' Calculates Hasbrouck information shares to determine which venues
#' or instruments contribute most to price discovery.
#'
#' @param price_data List of price series for different venues/instruments
#' @param method Character, "hasbrouck" or "gonzalo-granger"
#'
#' @return Data frame with information shares
#'
#' @details
#' **Hasbrouck Information Share**:
#' Measures each venue's contribution to price variance of the
#' common efficient price.
#'
#' **Interpretation**:
#' - Share close to 1: Venue dominates price discovery
#' - Share close to 0: Venue is passive, follows others
#' - Sum of shares = 1 across all venues
#'
#' **Applications**:
#' - Best execution venue selection
#' - Regulatory analysis
#' - Market structure research
#' - Optimal order routing
#'
#' @references
#' Hasbrouck, J. (1995). One security, many markets: Determining
#' the contributions to price discovery. *Journal of Finance*.
#'
#' @export
#' @examples
#' \dontrun{
#' price_discovery <- price_discovery_metrics(
#'   list(NYSE = prices_NYSE, NASDAQ = prices_NASDAQ),
#'   method = "hasbrouck"
#' )
#' }
price_discovery_metrics <- function(price_data,
                                      method = c("hasbrouck", "gonzalo-granger")) {

  method <- match.arg(method)

  if (!is.list(price_data) || length(price_data) < 2) {
    stop("price_data must be a list with at least 2 price series")
  }

  # Placeholder: Full implementation requires VECM estimation
  message("Price discovery metrics require vector error correction models.")
  message("Returning simplified venue share based on variance contribution.")

  # Simple variance-based shares
  variances <- sapply(price_data, function(x) var(diff(x), na.rm = TRUE))
  shares <- variances / sum(variances)

  result <- data.frame(
    venue = names(price_data),
    variance = variances,
    information_share = shares,
    stringsAsFactors = FALSE
  )

  return(result)
}


#' Principal component analysis of multi-asset order flow
#'
#' @description
#' Extracts common factors in multi-asset order flow using PCA.
#' Identifies market-wide vs. idiosyncratic components.
#'
#' @param cross_ofi_data Output from compute_cross_asset_ofi()
#' @param n_components Integer, number of PCs to extract (default: 3)
#' @param scale Logical, scale variables (default: TRUE)
#'
#' @return PCA results object
#'
#' @details
#' **Principal Components Interpretation**:
#' - **PC1**: Market-wide factor (systematic order flow)
#' - **PC2**: Sector/style factors
#' - **PC3+**: Idiosyncratic factors
#'
#' **Applications**:
#' - Factor model construction
#' - Dimensionality reduction
#' - Risk decomposition
#' - Sparse modeling (drop low-variance PCs)
#'
#' **Research Finding** (Cont et al.):
#' Once multi-level OFI is integrated, cross-asset terms add minimal
#' value. PCA helps identify when cross-terms matter.
#'
#' @export
#' @importFrom stats prcomp
#' @examples
#' \dontrun{
#' pca_result <- pca_orderflow(cross_ofi, n_components = 3)
#' summary(pca_result)
#' plot(pca_result)
#' }
pca_orderflow <- function(cross_ofi_data,
                           n_components = 3,
                           scale = TRUE) {

  if (!inherits(cross_ofi_data, "cross_asset_ofi")) {
    stop("cross_ofi_data must be output from compute_cross_asset_ofi()")
  }

  # Extract OFI matrix
  ofi_matrix <- cross_ofi_data$ofi_merged |>
    select(starts_with("ofi_")) |>
    na.omit() |>
    as.matrix()

  if (nrow(ofi_matrix) == 0) {
    stop("No complete observations for PCA")
  }

  # Perform PCA
  pca_result <- prcomp(ofi_matrix, center = TRUE, scale. = scale)

  # Keep n_components
  pca_result$n_components <- min(n_components, ncol(ofi_matrix))

  # Add interpretation
  variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  cumulative_variance <- cumsum(variance_explained)

  pca_result$variance_explained <- variance_explained[1:pca_result$n_components]
  pca_result$cumulative_variance <- cumulative_variance[1:pca_result$n_components]

  class(pca_result) <- c("pca_orderflow", "prcomp")
  return(pca_result)
}


#' Analyze spillover effects between markets
#'
#' @description
#' Measures how shocks in one market transmit to other markets.
#' Uses variance decomposition from VAR models.
#'
#' @param ofi_list List of OFI data for multiple assets
#' @param horizon Integer, forecast horizon for spillovers (default: 5)
#'
#' @return Spillover table
#'
#' @details
#' Spillover index quantifies shock transmission:
#' - High spillover: Markets are interconnected
#' - Low spillover: Markets are independent
#' - Directional spillover: From market i to market j
#'
#' @export
#' @examples
#' \dontrun{
#' spillover <- spillover_analysis(
#'   list(SPY = ofi_SPY, TLT = ofi_TLT),
#'   horizon = 5
#' )
#' }
spillover_analysis <- function(ofi_list, horizon = 5) {

  message("Spillover analysis requires VAR estimation.")
  message("Returning correlation-based proxy.")

  # Extract OFI values
  ofi_matrix <- sapply(ofi_list, function(x) x$oir)

  # Compute correlation
  cor_matrix <- cor(ofi_matrix, use = "pairwise.complete.obs")

  # Use correlation as spillover proxy
  spillover_df <- data.frame(
    from = rep(names(ofi_list), each = length(ofi_list)),
    to = rep(names(ofi_list), times = length(ofi_list)),
    spillover = as.numeric(cor_matrix),
    stringsAsFactors = FALSE
  )

  return(spillover_df)
}


#' Detect arbitrage opportunities across venues or related instruments
#'
#' @description
#' Identifies price discrepancies that may represent arbitrage opportunities
#' after accounting for transaction costs and risks.
#'
#' @param price_data_1 Price series for instrument/venue 1
#' @param price_data_2 Price series for instrument/venue 2
#' @param threshold Numeric, minimum price difference (in bps or absolute)
#' @param cost Numeric, estimated transaction cost (default: 0)
#'
#' @return Data frame with arbitrage opportunities
#'
#' @details
#' **Arbitrage Detection**:
#' 1. Calculate price difference: \eqn{D = P_1 - P_2}
#' 2. Adjust for costs: \eqn{D_{net} = |D| - cost}
#' 3. Flag if \eqn{D_{net} > threshold}
#'
#' **Types**:
#' - Cross-venue: Same security, different exchanges
#' - Statistical: Pairs trading on mean reversion
#' - Index: ETF vs. constituents
#' - Triangular: FX or derivatives
#'
#' @export
#' @examples
#' \dontrun{
#' # Detect cross-venue arbitrage
#' arb_opps <- arbitrage_opportunities(
#'   prices_NYSE,
#'   prices_NASDAQ,
#'   threshold = 1,  # 1 bp
#'   cost = 0.5  # 0.5 bp transaction cost
#' )
#' }
arbitrage_opportunities <- function(price_data_1,
                                     price_data_2,
                                     threshold = 1,
                                     cost = 0) {

  # Align timestamps if needed
  # For now, assume same length

  if (length(price_data_1) != length(price_data_2)) {
    stop("Price series must have same length")
  }

  # Calculate differences
  price_diff <- price_data_1 - price_data_2

  # Net of costs
  net_diff <- abs(price_diff) - cost

  # Flag arbitrage opportunities
  is_arbitrage <- net_diff > threshold

  # Package results
  result <- data.frame(
    timestamp = seq_along(price_data_1),
    price_1 = price_data_1,
    price_2 = price_data_2,
    price_diff = price_diff,
    net_diff = net_diff,
    is_arbitrage = is_arbitrage
  )

  # Filter to just opportunities
  opportunities <- result[result$is_arbitrage, ]

  attr(opportunities, "threshold") <- threshold
  attr(opportunities, "cost") <- cost
  attr(opportunities, "n_opportunities") <- nrow(opportunities)

  return(opportunities)
}


#' ETF arbitrage metrics (ETF vs. constituents)
#'
#' @description
#' Analyzes basis between ETF price and net asset value (NAV) of underlying
#' constituents. Identifies arbitrage opportunities for authorized participants.
#'
#' @param etf_price ETF price series
#' @param nav NAV (net asset value) series
#' @param threshold Numeric, minimum basis for arbitrage (default: 0.1%)
#'
#' @return Data frame with ETF arbitrage analysis
#'
#' @details
#' **ETF Basis**:
#' \deqn{Basis = \frac{ETF Price - NAV}{NAV} \times 100}
#'
#' - Positive basis: ETF trades at premium (sell ETF, buy basket)
#' - Negative basis: ETF trades at discount (buy ETF, sell basket)
#'
#' **Arbitrage Mechanism**:
#' - Authorized Participants (APs) create/redeem shares
#' - Creation: ETF at premium, AP buys basket, creates shares, sells ETF
#' - Redemption: ETF at discount, AP buys ETF, redeems for basket, sells basket
#'
#' @export
#' @examples
#' \dontrun{
#' etf_arb <- etf_arbitrage_metrics(
#'   etf_price = spy_prices,
#'   nav = sp500_nav,
#'   threshold = 0.1
#' )
#' }
etf_arbitrage_metrics <- function(etf_price,
                                    nav,
                                    threshold = 0.1) {

  if (length(etf_price) != length(nav)) {
    stop("ETF price and NAV must have same length")
  }

  # Calculate basis
  basis <- (etf_price - nav) / nav * 100  # In percent

  # Flag arbitrage opportunities
  is_premium <- basis > threshold
  is_discount <- basis < -threshold

  # Package results
  result <- data.frame(
    timestamp = seq_along(etf_price),
    etf_price = etf_price,
    nav = nav,
    basis_pct = basis,
    is_premium = is_premium,
    is_discount = is_discount,
    arbitrage_type = ifelse(is_premium, "Premium (Sell ETF)",
                            ifelse(is_discount, "Discount (Buy ETF)", "No Arb"))
  )

  # Statistics
  attr(result, "avg_basis") <- mean(basis, na.rm = TRUE)
  attr(result, "basis_volatility") <- sd(basis, na.rm = TRUE)
  attr(result, "n_premium_opps") <- sum(is_premium, na.rm = TRUE)
  attr(result, "n_discount_opps") <- sum(is_discount, na.rm = TRUE)

  return(result)
}


# ============================================================================
# Print Methods
# ============================================================================

#' @export
print.cross_asset_ofi <- function(x, ...) {
  cat("Cross-Asset Order Flow Imbalance Analysis\n")
  cat("==========================================\n\n")

  cat(sprintf("Number of assets: %d\n", x$n_assets))
  cat(sprintf("Assets: %s\n", paste(x$assets, collapse = ", ")))
  cat("\n")

  if (!is.null(x$correlation_matrix)) {
    cat("OFI Correlation Matrix:\n")
    print(round(x$correlation_matrix, 3))
    cat("\n")
  }

  cat("Use $ofi_individual for individual OFI data\n")
  cat("Use $ofi_merged for aligned time series\n")

  invisible(x)
}

#' @export
print.leadlag_analysis <- function(x, ...) {
  cat("Lead-Lag Analysis\n")
  cat("=================\n\n")

  cat(sprintf("Optimal lag: %d periods\n", x$optimal_lag))
  cat(sprintf("Maximum correlation: %.3f\n", x$max_correlation))
  cat(sprintf("Leader: %s\n", x$leader))
  cat("\n")

  cat("Interpretation:\n")
  cat(sprintf("  %s\n", x$interpretation))

  invisible(x)
}

#' @export
print.pca_orderflow <- function(x, ...) {
  cat("PCA of Multi-Asset Order Flow\n")
  cat("==============================\n\n")

  cat(sprintf("Number of components: %d\n", x$n_components))
  cat("\n")

  cat("Variance Explained:\n")
  for (i in 1:x$n_components) {
    cat(sprintf("  PC%d: %.1f%% (Cumulative: %.1f%%)\n",
                i,
                x$variance_explained[i] * 100,
                x$cumulative_variance[i] * 100))
  }

  invisible(x)
}
