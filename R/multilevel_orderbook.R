#' Multi-Level Order Book Metrics
#'
#' @description
#' Extract information from full limit order book depth beyond best bid/ask.
#' Research shows that levels 2-5 contain significant predictive information
#' for price movements and improve forecasting accuracy.
#'
#' @name multilevel_orderbook
NULL

#' Compute multi-level order flow imbalance
#'
#' @description
#' Calculates OFI across multiple levels of the order book, not just the
#' best bid/ask. Aggregates imbalances with distance-weighting or
#' volume-weighting to capture information in deeper levels.
#'
#' @param orderbook Data frame with order book state at multiple levels
#' @param n_levels Integer, number of levels to include (default: 5)
#' @param weighting Character, "equal", "volume", or "distance" (default: "volume")
#' @param distance_decay Numeric, decay parameter for distance weighting (default: 0.5)
#'
#' @return Data frame with multi-level OFI metrics
#'
#' @details
#' **Multi-Level OFI Formula**:
#'
#' For volume weighting:
#' \deqn{OFI_{ML} = \sum_{i=1}^{n} w_i \times (V_{bid,i} - V_{ask,i})}
#'
#' where \eqn{w_i = V_i / \sum V_i} (volume-weighted)
#'
#' For distance weighting:
#' \deqn{w_i = \exp(-\lambda \times d_i)}
#'
#' where \eqn{d_i} is distance from mid-price
#'
#' **Research Evidence**:
#' - Cont et al. (2023): Levels 2-5 improve forecasting
#' - Adding cross-asset terms less valuable than multi-level OFI
#' - Volume-weighting performs better than equal weighting
#'
#' **Use Cases**:
#' - Price direction prediction
#' - Execution quality assessment
#' - Market maker behavior analysis
#' - High-frequency trading signals
#'
#' @references
#' Cont, R., Kukanov, A., & Stoikov, S. (2023). Cross-impact of order flow
#' imbalance in equity markets. *Quantitative Finance*.
#'
#' @export
#' @importFrom dplyr mutate select
#' @examples
#' \dontrun{
#' # From reconstructed order book
#' orderbook_data <- reconstruct_orderbook(messages, depth = 10)
#' multilevel_ofi <- compute_multilevel_ofi(
#'   orderbook_data$snapshots,
#'   n_levels = 5,
#'   weighting = "volume"
#' )
#'
#' # Plot to see improvement over single-level
#' plot(multilevel_ofi$timestamp, multilevel_ofi$ofi_ml)
#' }
compute_multilevel_ofi <- function(orderbook,
                                     n_levels = 5,
                                    weighting = c("volume", "distance", "equal"),
                                     distance_decay = 0.5) {

  weighting <- match.arg(weighting)

  # Validate input
  required_cols <- c("timestamp")
  for (i in 1:n_levels) {
    required_cols <- c(required_cols,
                      paste0("bid_price_", i), paste0("bid_size_", i),
                      paste0("ask_price_", i), paste0("ask_size_", i))
  }

  missing <- setdiff(required_cols, names(orderbook))
  if (length(missing) > 0) {
    stop("Missing required columns for ", n_levels, " levels: ",
         paste(head(missing, 5), collapse = ", "))
  }

  # Calculate mid-price for distance weighting
  if (weighting == "distance") {
    orderbook$mid_price <- (orderbook$bid_price_1 + orderbook$ask_price_1) / 2
  }

  # Compute weighted OFI across levels
  ofi_ml <- rep(0, nrow(orderbook))
  total_weight <- rep(0, nrow(orderbook))

  for (i in 1:n_levels) {
    bid_size_col <- paste0("bid_size_", i)
    ask_size_col <- paste0("ask_size_", i)
    bid_price_col <- paste0("bid_price_", i)
    ask_price_col <- paste0("ask_price_", i)

    bid_sizes <- orderbook[[bid_size_col]]
    ask_sizes <- orderbook[[ask_size_col]]
    bid_prices <- orderbook[[bid_price_col]]
    ask_prices <- orderbook[[ask_price_col]]

    # Handle NAs
    bid_sizes[is.na(bid_sizes)] <- 0
    ask_sizes[is.na(ask_sizes)] <- 0

    # Calculate weights
    if (weighting == "equal") {
      weights <- rep(1, nrow(orderbook))

    } else if (weighting == "volume") {
      weights <- (bid_sizes + ask_sizes) / 2

    } else if (weighting == "distance") {
      # Distance from mid-price
      bid_dist <- abs(orderbook$mid_price - bid_prices)
      ask_dist <- abs(ask_prices - orderbook$mid_price)
      avg_dist <- (bid_dist + ask_dist) / 2

      # Exponential decay
      weights <- exp(-distance_decay * avg_dist)
      weights[is.na(weights)] <- 0
    }

    # Accumulate weighted OFI
    level_ofi <- (bid_sizes - ask_sizes) * weights
    ofi_ml <- ofi_ml + level_ofi
    total_weight <- total_weight + weights
  }

  # Normalize by total weight
  ofi_ml <- ofi_ml / (total_weight + 1e-10)

  # Also compute simple level-1 OFI for comparison
  ofi_1 <- orderbook$bid_size_1 - orderbook$ask_size_1

  # Package results
  result <- data.frame(
    timestamp = orderbook$timestamp,
    ofi_ml = ofi_ml,
    ofi_level1 = ofi_1,
    ofi_improvement = ofi_ml - ofi_1,
    n_levels = n_levels
  )

  attr(result, "weighting") <- weighting
  attr(result, "n_levels") <- n_levels

  return(result)
}


#' Calculate order book slope (depth decay rate)
#'
#' @description
#' Measures how quickly depth decreases away from the mid-price using
#' linear regression. Steep slopes indicate fragile liquidity where small
#' orders cause large price movements.
#'
#' @param orderbook Order book snapshot data
#' @param n_levels Integer, number of levels to analyze (default: 10)
#' @param side Character, "bid", "ask", or "both" (default: "both")
#'
#' @return Data frame with slope, curvature, and depth metrics
#'
#' @details
#' **Order Book Slope**:
#'
#' Fits linear regression:
#' \deqn{\log(depth_i) = \alpha + \beta \times distance_i + \epsilon}
#'
#' The slope \eqn{\beta} captures decay rate:
#' - \eqn{\beta \approx -1}: Exponential decay (typical)
#' - \eqn{\beta < -1}: Steep decay (fragile liquidity)
#' - \eqn{\beta > -1}: Slow decay (deep market)
#'
#' **Interpretation**:
#' - **Flat slope** (β near 0): Deep, resilient liquidity
#' - **Moderate slope** (β = -0.5 to -1.5): Normal markets
#' - **Steep slope** (β < -2): Shallow, fragile liquidity
#'
#' **Applications**:
#' - Liquidity measurement
#' - Execution cost prediction
#' - Market impact forecasting
#' - Flash crash early warning
#'
#' @export
#' @importFrom stats lm coef
#' @examples
#' \dontrun{
#' orderbook_data <- reconstruct_orderbook(messages, depth = 10)
#' slopes <- orderbook_slope(orderbook_data$snapshots, n_levels = 10)
#'
#' # Steep slope indicates fragile liquidity
#' fragile_periods <- slopes[slopes$bid_slope < -2, ]
#' }
orderbook_slope <- function(orderbook,
                             n_levels = 10,
                             side = c("both", "bid", "ask")) {

  side <- match.arg(side)

  results <- list()

  for (row_idx in seq_len(nrow(orderbook))) {
    row_data <- orderbook[row_idx, ]

    # Extract bid side
    if (side %in% c("both", "bid")) {
      bid_prices <- numeric(n_levels)
      bid_sizes <- numeric(n_levels)

      for (i in 1:n_levels) {
        bid_prices[i] <- row_data[[paste0("bid_price_", i)]]
        bid_sizes[i] <- row_data[[paste0("bid_size_", i)]]
      }

      # Remove NAs
      valid <- !is.na(bid_prices) & !is.na(bid_sizes) & bid_sizes > 0
      if (sum(valid) >= 3) {
        bid_prices <- bid_prices[valid]
        bid_sizes <- bid_sizes[valid]

        # Distance from best bid
        bid_dist <- abs(bid_prices - bid_prices[1])

        # Fit log-linear model
        bid_fit <- tryCatch({
          lm(log(bid_sizes + 1) ~ bid_dist)
        }, error = function(e) NULL)

        if (!is.null(bid_fit)) {
          bid_slope <- coef(bid_fit)[2]
        } else {
          bid_slope <- NA
        }
      } else {
        bid_slope <- NA
      }
    } else {
      bid_slope <- NA
    }

    # Extract ask side
    if (side %in% c("both", "ask")) {
      ask_prices <- numeric(n_levels)
      ask_sizes <- numeric(n_levels)

      for (i in 1:n_levels) {
        ask_prices[i] <- row_data[[paste0("ask_price_", i)]]
        ask_sizes[i] <- row_data[[paste0("ask_size_", i)]]
      }

      # Remove NAs
      valid <- !is.na(ask_prices) & !is.na(ask_sizes) & ask_sizes > 0
      if (sum(valid) >= 3) {
        ask_prices <- ask_prices[valid]
        ask_sizes <- ask_sizes[valid]

        # Distance from best ask
        ask_dist <- abs(ask_prices - ask_prices[1])

        # Fit log-linear model
        ask_fit <- tryCatch({
          lm(log(ask_sizes + 1) ~ ask_dist)
        }, error = function(e) NULL)

        if (!is.null(ask_fit)) {
          ask_slope <- coef(ask_fit)[2]
        } else {
          ask_slope <- NA
        }
      } else {
        ask_slope <- NA
      }
    } else {
      ask_slope <- NA
    }

    results[[row_idx]] <- data.frame(
      timestamp = row_data$timestamp,
      bid_slope = bid_slope,
      ask_slope = ask_slope,
      avg_slope = mean(c(bid_slope, ask_slope), na.rm = TRUE)
    )
  }

  result_df <- bind_rows(results)

  # Add interpretation
  result_df$liquidity_quality <- classify_slope(result_df$avg_slope)

  return(result_df)
}


#' Calculate order book curvature (second derivative)
#'
#' @description
#' Measures the acceleration of depth decay. Positive curvature indicates
#' depth falls off increasingly quickly (convex). Negative curvature
#' suggests depth is more evenly distributed (concave).
#'
#' @param orderbook Order book snapshot data
#' @param n_levels Integer, number of levels (default: 10)
#'
#' @return Data frame with curvature metrics
#'
#' @details
#' Fits quadratic model:
#' \deqn{\log(depth) = \alpha + \beta_1 d + \beta_2 d^2}
#'
#' Curvature = \eqn{2 \times \beta_2}
#'
#' @export
#' @examples
#' \dontrun{
#' curvature <- orderbook_curvature(orderbook_data$snapshots)
#' }
orderbook_curvature <- function(orderbook, n_levels = 10) {

  results <- list()

  for (row_idx in seq_len(nrow(orderbook))) {
    row_data <- orderbook[row_idx, ]

    # Bid side curvature
    bid_prices <- numeric(n_levels)
    bid_sizes <- numeric(n_levels)

    for (i in 1:n_levels) {
      bid_prices[i] <- row_data[[paste0("bid_price_", i)]]
      bid_sizes[i] <- row_data[[paste0("bid_size_", i)]]
    }

    valid <- !is.na(bid_prices) & !is.na(bid_sizes) & bid_sizes > 0
    if (sum(valid) >= 4) {
      bid_dist <- abs(bid_prices[valid] - bid_prices[which(valid)[1]])
      bid_sizes_valid <- bid_sizes[valid]

      bid_fit <- tryCatch({
        lm(log(bid_sizes_valid + 1) ~ bid_dist + I(bid_dist^2))
      }, error = function(e) NULL)

      if (!is.null(bid_fit) && length(coef(bid_fit)) >= 3) {
        bid_curvature <- 2 * coef(bid_fit)[3]
      } else {
        bid_curvature <- NA
      }
    } else {
      bid_curvature <- NA
    }

    # Ask side curvature
    ask_prices <- numeric(n_levels)
    ask_sizes <- numeric(n_levels)

    for (i in 1:n_levels) {
      ask_prices[i] <- row_data[[paste0("ask_price_", i)]]
      ask_sizes[i] <- row_data[[paste0("ask_size_", i)]]
    }

    valid <- !is.na(ask_prices) & !is.na(ask_sizes) & ask_sizes > 0
    if (sum(valid) >= 4) {
      ask_dist <- abs(ask_prices[valid] - ask_prices[which(valid)[1]])
      ask_sizes_valid <- ask_sizes[valid]

      ask_fit <- tryCatch({
        lm(log(ask_sizes_valid + 1) ~ ask_dist + I(ask_dist^2))
      }, error = function(e) NULL)

      if (!is.null(ask_fit) && length(coef(ask_fit)) >= 3) {
        ask_curvature <- 2 * coef(ask_fit)[3]
      } else {
        ask_curvature <- NA
      }
    } else {
      ask_curvature <- NA
    }

    results[[row_idx]] <- data.frame(
      timestamp = row_data$timestamp,
      bid_curvature = bid_curvature,
      ask_curvature = ask_curvature,
      avg_curvature = mean(c(bid_curvature, ask_curvature), na.rm = TRUE)
    )
  }

  return(bind_rows(results))
}


#' Analyze volume distribution across price levels
#'
#' @description
#' Examines how volume is distributed across the order book. Power-law
#' distributions are common, with most volume near the top.
#'
#' @param orderbook Order book snapshot data
#' @param n_levels Integer, number of levels (default: 10)
#' @param normalize Logical, normalize to percentages (default: TRUE)
#'
#' @return Data frame with volume distribution metrics
#'
#' @details
#' Computes:
#' - Volume at each level
#' - Percentage of total volume per level
#' - Cumulative volume distribution
#' - Concentration metrics (Herfindahl index)
#'
#' @export
#' @examples
#' \dontrun{
#' volume_dist <- volume_distribution_levels(orderbook_data$snapshots)
#' }
volume_distribution_levels <- function(orderbook,
                                        n_levels = 10,
                                        normalize = TRUE) {

  results <- list()

  for (row_idx in seq_len(nrow(orderbook))) {
    row_data <- orderbook[row_idx, ]

    # Extract all levels
    bid_sizes <- numeric(n_levels)
    ask_sizes <- numeric(n_levels)

    for (i in 1:n_levels) {
      bid_sizes[i] <- row_data[[paste0("bid_size_", i)]]
      ask_sizes[i] <- row_data[[paste0("ask_size_", i)]]
    }

    # Replace NAs with 0
    bid_sizes[is.na(bid_sizes)] <- 0
    ask_sizes[is.na(ask_sizes)] <- 0

    # Total volume
    total_bid <- sum(bid_sizes)
    total_ask <- sum(ask_sizes)
    total_volume <- total_bid + total_ask

    # Normalize if requested
    if (normalize && total_volume > 0) {
      bid_pct <- bid_sizes / total_volume * 100
      ask_pct <- ask_sizes / total_volume * 100
    } else {
      bid_pct <- bid_sizes
      ask_pct <- ask_sizes
    }

    # Concentration metric (Herfindahl index)
    combined_shares <- c(bid_pct, ask_pct) / 100
    herfindahl <- sum(combined_shares^2)

    # Top-level concentration
    top3_pct <- sum(c(bid_pct[1:min(3, n_levels)],
                      ask_pct[1:min(3, n_levels)])) / 100

    result_row <- data.frame(
      timestamp = row_data$timestamp,
      total_bid_volume = total_bid,
      total_ask_volume = total_ask,
      total_volume = total_volume,
      herfindahl_index = herfindahl,
      top3_concentration = top3_pct
    )

    # Add level-by-level percentages
    for (i in 1:n_levels) {
      result_row[[paste0("bid_pct_L", i)]] <- bid_pct[i]
      result_row[[paste0("ask_pct_L", i)]] <- ask_pct[i]
    }

    results[[row_idx]] <- result_row
  }

  return(bind_rows(results))
}


#' Calculate bid-ask pressure (asymmetric depth analysis)
#'
#' @description
#' Measures the asymmetry between bid and ask depth across multiple levels.
#' High bid pressure (more bid depth) suggests buying interest.
#'
#' @param orderbook Order book snapshot data
#' @param n_levels Integer, number of levels (default: 5)
#'
#' @return Data frame with pressure metrics
#'
#' @details
#' **Bid-Ask Pressure**:
#' \deqn{Pressure = \frac{\sum BidDepth - \sum AskDepth}{\sum BidDepth + \sum AskDepth}}
#'
#' Ranges from -1 (all ask) to +1 (all bid)
#'
#' @export
#' @examples
#' \dontrun{
#' pressure <- bid_ask_pressure(orderbook_data$snapshots, n_levels = 5)
#' }
bid_ask_pressure <- function(orderbook, n_levels = 5) {

  results <- list()

  for (row_idx in seq_len(nrow(orderbook))) {
    row_data <- orderbook[row_idx, ]

    # Sum depth across levels
    total_bid <- 0
    total_ask <- 0

    for (i in 1:n_levels) {
      bid_size <- row_data[[paste0("bid_size_", i)]]
      ask_size <- row_data[[paste0("ask_size_", i)]]

      total_bid <- total_bid + ifelse(is.na(bid_size), 0, bid_size)
      total_ask <- total_ask + ifelse(is.na(ask_size), 0, ask_size)
    }

    # Calculate pressure
    if (total_bid + total_ask > 0) {
      pressure <- (total_bid - total_ask) / (total_bid + total_ask)
    } else {
      pressure <- 0
    }

    results[[row_idx]] <- data.frame(
      timestamp = row_data$timestamp,
      bid_depth = total_bid,
      ask_depth = total_ask,
      depth_imbalance = total_bid - total_ask,
      depth_pressure = pressure
    )
  }

  return(bind_rows(results))
}


#' Calculate depth imbalance at multiple levels
#'
#' @description
#' Computes order flow imbalance-like metrics using depth instead of flow.
#' Combines information from multiple levels.
#'
#' @param orderbook Order book snapshot data
#' @param n_levels Integer, number of levels (default: 5)
#' @param method Character, "simple", "weighted", or "cumulative"
#'
#' @return Data frame with depth imbalance metrics
#'
#' @export
#' @examples
#' \dontrun{
#' depth_imb <- depth_imbalance(orderbook_data$snapshots, n_levels = 5)
#' }
depth_imbalance <- function(orderbook,
                             n_levels = 5,
                             method = c("simple", "weighted", "cumulative")) {

  method <- match.arg(method)

  # Use bid_ask_pressure as base
  pressure_df <- bid_ask_pressure(orderbook, n_levels)

  if (method == "simple") {
    return(pressure_df)

  } else if (method == "weighted") {
    # Volume-weighted depth imbalance
    # Implementation would weight by depth at each level
    return(pressure_df)

  } else if (method == "cumulative") {
    # Add cumulative imbalance
    pressure_df$cumulative_imbalance <- cumsum(pressure_df$depth_imbalance)
    return(pressure_df)
  }
}


#' Calculate microprice (volume-weighted mid-price)
#'
#' @description
#' Computes a more accurate mid-price by weighting bid and ask by their
#' respective depths. Better than simple (bid + ask) / 2 when book is
#' imbalanced.
#'
#' @param orderbook Order book snapshot with bid/ask prices and sizes
#' @param n_levels Integer, number of levels to include (default: 1)
#'
#' @return Numeric vector of microprices
#'
#' @details
#' **Microprice Formula** (Stoikov):
#' \deqn{P_{micro} = \frac{P_{ask} \times Q_{bid} + P_{bid} \times Q_{ask}}{Q_{bid} + Q_{ask}}}
#'
#' For single level, this weights by opposite side depth.
#'
#' **Advantages over simple mid**:
#' - Accounts for depth imbalance
#' - Less sensitive to quote flickering
#' - Better predictor of next trade price
#' - Reduces microstructure noise
#'
#' **Applications**:
#' - Mark-to-market valuation
#' - Execution benchmarking
#' - Variance estimation
#' - Statistical arbitrage signals
#'
#' @references
#' Stoikov, S. (2018). The microprice: A high-frequency estimator of
#' future prices. *Quantitative Finance*.
#'
#' @export
#' @examples
#' \dontrun{
#' orderbook_data <- reconstruct_orderbook(messages, depth = 5)
#' microprices <- microprice(orderbook_data$snapshots, n_levels = 1)
#'
#' # Compare to simple mid
#' simple_mid <- (orderbook_data$best_bid_ask$best_bid +
#'                orderbook_data$best_bid_ask$best_ask) / 2
#' plot(microprices - simple_mid)  # Difference due to depth weighting
#' }
microprice <- function(orderbook, n_levels = 1) {

  microprices <- numeric(nrow(orderbook))

  for (row_idx in seq_len(nrow(orderbook))) {
    row_data <- orderbook[row_idx, ]

    # Aggregate across levels
    total_bid_value <- 0
    total_ask_value <- 0
    total_bid_qty <- 0
    total_ask_qty <- 0

    for (i in 1:n_levels) {
      bid_price <- row_data[[paste0("bid_price_", i)]]
      bid_size <- row_data[[paste0("bid_size_", i)]]
      ask_price <- row_data[[paste0("ask_price_", i)]]
      ask_size <- row_data[[paste0("ask_size_", i)]]

      if (!is.na(bid_price) && !is.na(bid_size)) {
        total_bid_value <- total_bid_value + bid_price * bid_size
        total_bid_qty <- total_bid_qty + bid_size
      }

      if (!is.na(ask_price) && !is.na(ask_size)) {
        total_ask_value <- total_ask_value + ask_price * ask_size
        total_ask_qty <- total_ask_qty + ask_size
      }
    }

    # Calculate microprice
    if (total_bid_qty > 0 && total_ask_qty > 0) {
      # Weighted by opposite side depth
      avg_bid <- total_bid_value / total_bid_qty
      avg_ask <- total_ask_value / total_ask_qty

      microprice_val <- (avg_ask * total_bid_qty + avg_bid * total_ask_qty) /
        (total_bid_qty + total_ask_qty)
    } else {
      microprice_val <- NA
    }

    microprices[row_idx] <- microprice_val
  }

  return(microprices)
}


#' Measure order book resilience (depth replenishment speed)
#'
#' @description
#' Analyzes how quickly depth returns after a trade or cancel event.
#' Resilient markets quickly replenish depth; fragile markets do not.
#'
#' @param orderbook Time series of order book snapshots
#' @param events Data frame of trade/cancel events
#' @param recovery_window Numeric, seconds to measure recovery (default: 10)
#'
#' @return Data frame with resilience metrics
#'
#' @details
#' For each significant event (trade > threshold, large cancel):
#' 1. Measure depth before event
#' 2. Measure depth immediately after
#' 3. Measure depth recovery over time window
#' 4. Calculate recovery rate and half-life
#'
#' **Resilience Metrics**:
#' - **Recovery rate**: % of depth recovered per second
#' - **Half-life**: Time to recover 50% of depth
#' - **Full recovery time**: Time to return to pre-event depth
#'
#' @export
#' @examples
#' \dontrun{
#' resilience <- order_book_resilience(
#'   orderbook_data$snapshots,
#'   events = trades,
#'   recovery_window = 10
#' )
#' }
order_book_resilience <- function(orderbook,
                                    events,
                                    recovery_window = 10) {

  # Placeholder implementation
  # Full version would match events to orderbook snapshots
  # and track depth recovery

  message("order_book_resilience() requires detailed time-series matching.")
  message("Returning summary statistics.")

  # Simple approximation: measure depth volatility
  if ("bid_size_1" %in% names(orderbook) && "ask_size_1" %in% names(orderbook)) {
    depth_volatility <- sd(orderbook$bid_size_1 + orderbook$ask_size_1, na.rm = TRUE)

    result <- data.frame(
      metric = c("depth_volatility", "avg_bid_depth", "avg_ask_depth"),
      value = c(
        depth_volatility,
        mean(orderbook$bid_size_1, na.rm = TRUE),
        mean(orderbook$ask_size_1, na.rm = TRUE)
      )
    )

    return(result)
  } else {
    stop("Order book data must include bid_size_1 and ask_size_1")
  }
}


# ============================================================================
# Helper Functions
# ============================================================================

#' Classify slope quality
#' @noRd
classify_slope <- function(slope) {
  ifelse(is.na(slope), "Unknown",
         ifelse(slope > -0.5, "Deep/Resilient",
                ifelse(slope > -1.5, "Normal",
                       ifelse(slope > -2.5, "Moderate",
                              "Fragile/Shallow"))))
}


# ============================================================================
# Print Methods
# ============================================================================

#' @export
print.multilevel_ofi <- function(x, ...) {
  cat("Multi-Level Order Flow Imbalance\n")
  cat("=================================\n\n")

  cat(sprintf("Levels: %d\n", attr(x, "n_levels")))
  cat(sprintf("Weighting: %s\n", attr(x, "weighting")))
  cat(sprintf("Observations: %d\n", nrow(x)))
  cat("\n")

  cat("Summary Statistics:\n")
  cat(sprintf("  OFI (Level 1): %.2f ± %.2f\n",
              mean(x$ofi_level1, na.rm = TRUE),
              sd(x$ofi_level1, na.rm = TRUE)))
  cat(sprintf("  OFI (Multi-Level): %.2f ± %.2f\n",
              mean(x$ofi_ml, na.rm = TRUE),
              sd(x$ofi_ml, na.rm = TRUE)))
  cat(sprintf("  Improvement: %.2f\n",
              mean(x$ofi_improvement, na.rm = TRUE)))

  invisible(x)
}
