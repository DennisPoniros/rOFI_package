#' Estimate Kyle's Lambda (Price Impact Coefficient)
#'
#' @description
#' Estimates Kyle's lambda, which measures the price impact per unit of order flow.
#' Lambda represents the adverse selection cost and is a key market microstructure parameter.
#'
#' @param data Trade-level data with timestamp, side, size, and price columns
#' @param window Aggregation window (default: "1 min")
#' @param method Estimation method: "regression", "hasbrouck", or "amihud" (default: "regression")
#' @param price_change_type Price change calculation: "midpoint", "trade", or "quote" (default: "midpoint")
#'
#' @return A list with:
#'   - lambda: estimated price impact coefficient
#'   - std_error: standard error of the estimate
#'   - r_squared: R-squared of the regression (if applicable)
#'   - method: method used
#'   - interpretation: text interpretation
#'
#' @details
#' Kyle's lambda (λ) from Kyle (1985) represents the price impact of order flow:
#'
#' ΔP_t = λ * OFI_t + ε_t
#'
#' Where:
#' - ΔP_t = price change in period t
#' - OFI_t = order flow imbalance in period t
#' - λ = price impact per unit of signed volume
#'
#' Methods:
#' - "regression": Simple OLS regression of price changes on OFI
#' - "hasbrouck": Hasbrouck (1991) VAR-based approach
#' - "amihud": Amihud (2002) illiquidity measure adaptation
#'
#' Higher lambda indicates:
#' - Greater adverse selection costs
#' - Lower market liquidity
#' - Higher information asymmetry
#'
#' @export
#' @importFrom stats lm coef summary.lm
#' @importFrom dplyr mutate lag group_by summarise first last
#' @importFrom lubridate floor_date
#' @importFrom rlang .data
#'
#' @examples
#' # Generate data with realistic price impact
#' trades <- simulate_orders(n = 2000, seed = 123, imb = 0.2)
#'
#' # Estimate Kyle's lambda
#' kyle <- kyle_lambda_estimation(trades, window = "1 min")
#' print(kyle$lambda)
#' print(kyle$interpretation)
#'
#' # Compare different methods
#' kyle_reg <- kyle_lambda_estimation(trades, method = "regression")
#' kyle_amihud <- kyle_lambda_estimation(trades, method = "amihud")
kyle_lambda_estimation <- function(data,
                                   window = "1 min",
                                   method = c("regression", "hasbrouck", "amihud"),
                                   price_change_type = c("midpoint", "trade", "quote")) {

  method <- match.arg(method)
  price_change_type <- match.arg(price_change_type)

  # Validate required columns
  required <- c("timestamp", "side", "size", "price")
  if (!all(required %in% names(data))) {
    stop("Data must contain columns: ", paste(required, collapse = ", "))
  }

  # Compute OFI
  ofi_data <- compute_ofi(data, window = window, price_weighted = FALSE)

  # Compute price changes by window
  price_by_window <- data |>
    mutate(window_start = lubridate::floor_date(.data$timestamp, window)) |>
    group_by(.data$window_start) |>
    summarise(
      price_first = first(.data$price),
      price_last = last(.data$price),
      price_mean = mean(.data$price),
      .groups = "drop"
    )

  # Merge with OFI
  combined <- ofi_data |>
    left_join(price_by_window, by = "window_start")

  # Calculate price changes based on type
  combined <- combined |>
    mutate(
      price_change = switch(price_change_type,
        midpoint = (.data$price_last - .data$price_first),
        trade = (.data$price_last - lag(.data$price_last)),
        quote = (.data$price_mean - lag(.data$price_mean))
      )
    )

  # Remove NAs
  combined <- combined |>
    filter(!is.na(.data$ofi), !is.na(.data$price_change))

  if (nrow(combined) < 10) {
    stop("Insufficient data points for estimation. Need at least 10 windows.")
  }

  # Estimate lambda based on method
  result <- switch(method,
    regression = estimate_lambda_regression(combined),
    hasbrouck = estimate_lambda_hasbrouck(combined),
    amihud = estimate_lambda_amihud(combined)
  )

  # Add interpretation
  result$method <- method
  result$interpretation <- interpret_kyle_lambda(result$lambda)

  class(result) <- c("kyle_lambda", "list")
  result
}


#' Estimate lambda using OLS regression
#' @noRd
estimate_lambda_regression <- function(data) {
  # Simple linear regression: ΔP ~ OFI
  model <- lm(price_change ~ ofi, data = data)
  model_summary <- summary(model)

  list(
    lambda = as.numeric(coef(model)["ofi"]),
    std_error = model_summary$coefficients["ofi", "Std. Error"],
    r_squared = model_summary$r.squared,
    n_obs = nrow(data)
  )
}


#' Estimate lambda using Hasbrouck approach (simplified)
#' @noRd
estimate_lambda_hasbrouck <- function(data) {
  # Simplified Hasbrouck: uses lagged OFI as well
  model <- lm(price_change ~ ofi + lag(ofi), data = data)
  model_summary <- summary(model)

  # Lambda is the immediate impact coefficient
  list(
    lambda = as.numeric(coef(model)["ofi"]),
    std_error = model_summary$coefficients["ofi", "Std. Error"],
    r_squared = model_summary$r.squared,
    n_obs = nrow(data)
  )
}


#' Estimate lambda using Amihud measure
#' @noRd
estimate_lambda_amihud <- function(data) {
  # Amihud (2002): |ΔP| / Volume
  # Modified to use signed OFI
  lambda <- mean(abs(data$price_change) / (abs(data$ofi) + 1e-10), na.rm = TRUE)

  list(
    lambda = lambda,
    std_error = sd(abs(data$price_change) / (abs(data$ofi) + 1e-10), na.rm = TRUE) / sqrt(nrow(data)),
    r_squared = NA_real_,
    n_obs = nrow(data)
  )
}


#' Interpret Kyle's lambda value
#' @noRd
interpret_kyle_lambda <- function(lambda) {
  if (is.na(lambda)) {
    return("Unable to estimate lambda")
  }

  if (abs(lambda) < 0.0001) {
    "Very low price impact (highly liquid market)"
  } else if (abs(lambda) < 0.001) {
    "Low price impact (liquid market)"
  } else if (abs(lambda) < 0.01) {
    "Moderate price impact (normal liquidity)"
  } else if (abs(lambda) < 0.1) {
    "High price impact (lower liquidity)"
  } else {
    "Very high price impact (illiquid market)"
  }
}


#' Estimate Temporary and Permanent Price Impact
#'
#' @description
#' Decomposes price changes into temporary (transient) and permanent (informational)
#' components following Hasbrouck (1991) and Hendershott & Seasholes (2007).
#'
#' @param data Trade-level data with timestamp, side, size, and price columns
#' @param window Aggregation window (default: "1 min")
#' @param decay_periods Number of periods to measure impact decay (default: 5)
#'
#' @return A list with:
#'   - temporary_impact: temporary price impact coefficient
#'   - permanent_impact: permanent price impact coefficient
#'   - half_life: half-life of temporary component (in periods)
#'   - pct_temporary: percentage of impact that is temporary
#'   - decay_profile: impact decay over time
#'
#' @details
#' Price impact can be decomposed into:
#'
#' Temporary Impact: Liquidity-driven price pressure that reverts
#' Permanent Impact: Information-driven price change that persists
#'
#' The model:
#' ΔP_t = α (permanent) + β (temporary) * OFI_t + ε_t
#'
#' Temporary impact typically decays exponentially over subsequent periods.
#' High temporary impact suggests:
#' - Price pressure from uninformed trading
#' - Need for market making
#'
#' High permanent impact suggests:
#' - Informed trading
#' - Price discovery
#'
#' @export
#' @importFrom stats lm coef
#' @importFrom dplyr mutate lag lead
#'
#' @examples
#' trades <- simulate_orders(n = 2000, seed = 123, drift = 0.05)
#'
#' # Estimate impact decomposition
#' impact <- estimate_price_impact(trades, window = "1 min")
#' print(paste("Temporary:", round(impact$temporary_impact, 5)))
#' print(paste("Permanent:", round(impact$permanent_impact, 5)))
#' print(paste("Half-life:", impact$half_life, "periods"))
#'
#' # Plot decay profile
#' plot(1:length(impact$decay_profile), impact$decay_profile,
#'      type = "b", xlab = "Periods After Trade",
#'      ylab = "Impact Remaining", main = "Price Impact Decay")
estimate_price_impact <- function(data,
                                  window = "1 min",
                                  decay_periods = 5) {

  # Validate inputs
  required <- c("timestamp", "side", "size", "price")
  if (!all(required %in% names(data))) {
    stop("Data must contain columns: ", paste(required, collapse = ", "))
  }

  if (decay_periods < 1 || decay_periods > 20) {
    stop("decay_periods must be between 1 and 20")
  }

  # Compute OFI
  ofi_data <- compute_ofi(data, window = window)

  # Get price data
  price_by_window <- data |>
    mutate(window_start = lubridate::floor_date(.data$timestamp, window)) |>
    group_by(.data$window_start) |>
    summarise(
      price_start = first(.data$price),
      price_end = last(.data$price),
      .groups = "drop"
    )

  combined <- ofi_data |>
    left_join(price_by_window, by = "window_start")

  # Calculate immediate and future price changes
  combined <- combined |>
    mutate(
      price_change_0 = .data$price_end - lag(.data$price_end),  # Immediate
      price_change_final = lead(.data$price_end, n = decay_periods) - lag(.data$price_end)  # After decay
    )

  # Remove NAs
  combined <- combined |>
    filter(!is.na(.data$ofi),
           !is.na(.data$price_change_0),
           !is.na(.data$price_change_final))

  if (nrow(combined) < 20) {
    stop("Insufficient data. Need at least 20 windows with ", decay_periods, " periods of future data.")
  }

  # Estimate impacts
  # Temporary = immediate impact - permanent impact
  # Permanent = impact that persists after decay_periods

  model_immediate <- lm(price_change_0 ~ ofi, data = combined)
  model_permanent <- lm(price_change_final ~ ofi, data = combined)

  immediate_coef <- as.numeric(coef(model_immediate)["ofi"])
  permanent_coef <- as.numeric(coef(model_permanent)["ofi"])
  temporary_coef <- immediate_coef - permanent_coef

  # Calculate decay profile
  decay_profile <- numeric(decay_periods)
  for (i in 1:decay_periods) {
    combined_i <- combined |>
      mutate(price_change_i = lead(.data$price_end, n = i) - lag(.data$price_end)) |>
      filter(!is.na(.data$price_change_i))

    if (nrow(combined_i) > 10) {
      model_i <- lm(price_change_i ~ ofi, data = combined_i)
      decay_profile[i] <- as.numeric(coef(model_i)["ofi"])
    } else {
      decay_profile[i] <- NA
    }
  }

  # Calculate half-life (periods until impact halves)
  if (temporary_coef > 0) {
    target <- immediate_coef - temporary_coef / 2
    half_life_idx <- which(decay_profile <= target)[1]
    half_life <- if (!is.na(half_life_idx)) half_life_idx else decay_periods
  } else {
    half_life <- NA
  }

  # Calculate percentage temporary
  pct_temporary <- if (immediate_coef != 0) {
    100 * abs(temporary_coef) / abs(immediate_coef)
  } else {
    NA_real_
  }

  result <- list(
    temporary_impact = temporary_coef,
    permanent_impact = permanent_coef,
    immediate_impact = immediate_coef,
    half_life = half_life,
    pct_temporary = pct_temporary,
    decay_profile = decay_profile,
    decay_periods = decay_periods,
    n_obs = nrow(combined)
  )

  class(result) <- c("price_impact", "list")
  result
}


#' Compute VPIN (Volume-Synchronized Probability of Informed Trading)
#'
#' @description
#' Calculates VPIN (Easley et al., 2012), a real-time measure of order flow toxicity
#' and the probability of informed trading. High VPIN values indicate toxic order flow
#' that may precede adverse price movements.
#'
#' @param data Trade-level data with timestamp, side, and size columns
#' @param n_buckets Number of volume buckets to use (default: 50)
#' @param bucket_size Target volume per bucket (default: NULL, auto-calculated)
#' @param lookback Number of buckets for VPIN calculation (default: 50)
#'
#' @return A tibble with:
#'   - bucket_id: volume bucket identifier
#'   - bucket_end_time: timestamp when bucket filled
#'   - vpin: VPIN value (0 to 1)
#'   - buy_volume: total buy volume in bucket
#'   - sell_volume: total sell volume in bucket
#'   - abs_ofi: absolute order flow imbalance
#'
#' @details
#' VPIN formula:
#'
#' VPIN_t = (1/n) * Σ |V_buy - V_sell| / V_total
#'
#' Where the sum is over the last n volume buckets.
#'
#' Interpretation:
#' - VPIN ∈ [0, 1]
#' - VPIN = 0: Perfectly balanced order flow
#' - VPIN = 1: Completely one-sided order flow
#' - VPIN > 0.5: Toxic order flow, potential informed trading
#' - VPIN > 0.7: High probability of informed trading
#' - VPIN > 0.9: Extreme toxicity, potential flash crash conditions
#'
#' Used for:
#' - Flash crash prediction
#' - Market making risk management
#' - Liquidity assessment
#' - High-frequency trading strategy evaluation
#'
#' @export
#' @importFrom dplyr mutate group_by summarise n cumsum
#' @importFrom tibble tibble
#' @importFrom rlang .data
#'
#' @examples
#' # Generate high-frequency data
#' trades <- simulate_orders(n = 5000, lambda = 50, seed = 123)
#'
#' # Calculate VPIN
#' vpin_data <- compute_vpin(trades, n_buckets = 50, lookback = 50)
#'
#' # Identify toxic periods
#' toxic <- vpin_data[vpin_data$vpin > 0.7, ]
#' print(paste(nrow(toxic), "toxic periods detected"))
#'
#' # Plot VPIN over time
#' plot(vpin_data$bucket_end_time, vpin_data$vpin,
#'      type = "l", xlab = "Time", ylab = "VPIN",
#'      main = "Volume-Synchronized Probability of Informed Trading")
#' abline(h = 0.5, col = "orange", lty = 2)
#' abline(h = 0.7, col = "red", lty = 2)
compute_vpin <- function(data,
                        n_buckets = 50,
                        bucket_size = NULL,
                        lookback = 50) {

  # Validate inputs
  required <- c("timestamp", "side", "size")
  if (!all(required %in% names(data))) {
    stop("Data must contain columns: ", paste(required, collapse = ", "))
  }

  if (n_buckets < 2) {
    stop("n_buckets must be at least 2")
  }

  if (lookback < 1 || lookback > n_buckets) {
    stop("lookback must be between 1 and n_buckets")
  }

  # Sort by timestamp
  data <- data |>
    arrange(.data$timestamp)

  # Calculate bucket size if not provided
  total_volume <- sum(data$size, na.rm = TRUE)
  if (is.null(bucket_size)) {
    bucket_size <- total_volume / n_buckets
  }

  # Assign volume buckets
  data <- data |>
    mutate(
      cum_volume = cumsum(.data$size),
      bucket_id = ceiling(.data$cum_volume / bucket_size)
    )

  # Calculate buy/sell volume per bucket
  bucket_stats <- data |>
    group_by(.data$bucket_id) |>
    summarise(
      bucket_end_time = max(.data$timestamp),
      buy_volume = sum(.data$size[.data$side == "B"], na.rm = TRUE),
      sell_volume = sum(.data$size[.data$side == "S"], na.rm = TRUE),
      total_volume = sum(.data$size, na.rm = TRUE),
      .groups = "drop"
    )

  # Calculate absolute OFI
  bucket_stats <- bucket_stats |>
    mutate(abs_ofi = abs(.data$buy_volume - .data$sell_volume))

  # Calculate VPIN using rolling window
  n_complete_buckets <- nrow(bucket_stats)

  if (n_complete_buckets < lookback) {
    warning("Insufficient buckets for VPIN calculation. Need at least ", lookback,
            " buckets, have ", n_complete_buckets)
    lookback <- max(1, n_complete_buckets)
  }

  vpin_values <- numeric(n_complete_buckets)

  for (i in lookback:n_complete_buckets) {
    window_data <- bucket_stats[(i - lookback + 1):i, ]
    total_abs_ofi <- sum(window_data$abs_ofi)
    total_vol <- sum(window_data$total_volume)
    vpin_values[i] <- total_abs_ofi / (total_vol + 1e-10)
  }

  # Set early values to NA
  vpin_values[1:(lookback - 1)] <- NA

  bucket_stats$vpin <- vpin_values

  # Return results
  result <- bucket_stats |>
    select(.data$bucket_id, .data$bucket_end_time, .data$vpin,
           .data$buy_volume, .data$sell_volume, .data$abs_ofi, .data$total_volume)

  attr(result, "bucket_size") <- bucket_size
  attr(result, "lookback") <- lookback

  result
}


#' Print method for Kyle's lambda results
#' @export
print.kyle_lambda <- function(x, ...) {
  cat("Kyle's Lambda Estimation\n")
  cat("========================\n\n")

  cat("Method:", x$method, "\n")
  cat("Lambda:", format(x$lambda, scientific = TRUE, digits = 4), "\n")
  cat("Std Error:", format(x$std_error, scientific = TRUE, digits = 4), "\n")

  if (!is.na(x$r_squared)) {
    cat("R-squared:", round(x$r_squared, 4), "\n")
  }

  cat("N observations:", x$n_obs, "\n\n")

  cat("Interpretation:\n")
  cat(" ", x$interpretation, "\n")

  invisible(x)
}


#' Print method for price impact results
#' @export
print.price_impact <- function(x, ...) {
  cat("Price Impact Decomposition\n")
  cat("==========================\n\n")

  cat("Immediate impact:", format(x$immediate_impact, scientific = TRUE, digits = 4), "\n")
  cat("Permanent impact:", format(x$permanent_impact, scientific = TRUE, digits = 4),
      sprintf("(%.1f%% of total)\n", 100 - x$pct_temporary))
  cat("Temporary impact:", format(x$temporary_impact, scientific = TRUE, digits = 4),
      sprintf("(%.1f%% of total)\n", x$pct_temporary))

  if (!is.na(x$half_life)) {
    cat("Half-life:", x$half_life, "periods\n")
  }

  cat("\nDecay periods analyzed:", x$decay_periods, "\n")
  cat("N observations:", x$n_obs, "\n")

  invisible(x)
}


#' Calculate Effective Spread
#'
#' @description
#' Computes the effective spread, which measures the round-trip transaction cost
#' for liquidity demanders. The effective spread captures both the quoted spread
#' and price improvement/deterioration.
#'
#' @param data Trade-level data with timestamp, side, size, and price columns
#' @param midpoint_method Method to compute midpoint: "rolling", "same_second", or "vwap"
#' @param window Window for midpoint calculation if using rolling (default: "1 sec")
#'
#' @return A tibble with effective spread metrics:
#'   - timestamp: trade timestamp
#'   - side: trade side
#'   - price: trade price
#'   - midpoint: estimated midpoint at trade time
#'   - effective_spread: 2 * |price - midpoint|
#'   - signed_spread: 2 * (price - midpoint) * side_indicator
#'   - relative_spread: spread as percentage of midpoint
#'
#' @details
#' Effective Spread = 2 * |Trade Price - Midpoint|
#'
#' The factor of 2 represents the round-trip cost (buy and sell).
#'
#' Lower effective spread indicates:
#' - Better execution quality
#' - Higher liquidity
#' - Lower transaction costs
#'
#' Components:
#' - Quoted spread: Posted bid-ask spread
#' - Price improvement: Trading inside the spread (negative component)
#' - Price impact: Adverse price movement (positive component)
#'
#' @export
#' @importFrom dplyr mutate lag lead
#' @importFrom lubridate floor_date
#' @importFrom zoo rollmean
#' @importFrom rlang .data
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123)
#'
#' # Calculate effective spread
#' spreads <- compute_effective_spread(trades, midpoint_method = "rolling")
#'
#' # Summary statistics
#' summary(spreads$effective_spread)
#' summary(spreads$relative_spread)
#'
#' # Compare buy vs sell spreads
#' by(spreads$effective_spread, spreads$side, mean)
compute_effective_spread <- function(data,
                                     midpoint_method = c("rolling", "same_second", "vwap"),
                                     window = "1 sec") {

  midpoint_method <- match.arg(midpoint_method)

  # Validate inputs
  required <- c("timestamp", "side", "price")
  if (!all(required %in% names(data))) {
    stop("Data must contain columns: ", paste(required, collapse = ", "))
  }

  # Sort by timestamp
  data <- data |>
    arrange(.data$timestamp)

  # Estimate midpoint based on method
  data <- switch(midpoint_method,
    rolling = {
      # Use rolling average of prices
      n_roll <- min(5, nrow(data))
      data |>
        mutate(midpoint = zoo::rollmean(.data$price, k = n_roll,
                                        fill = .data$price, align = "center"))
    },
    same_second = {
      # Average prices within same second
      data |>
        mutate(
          time_bucket = lubridate::floor_date(.data$timestamp, "1 sec")
        ) |>
        group_by(.data$time_bucket) |>
        mutate(midpoint = mean(.data$price)) |>
        ungroup() |>
        select(-.data$time_bucket)
    },
    vwap = {
      # Volume-weighted average if size available
      if ("size" %in% names(data)) {
        data |>
          mutate(
            time_bucket = lubridate::floor_date(.data$timestamp, window)
          ) |>
          group_by(.data$time_bucket) |>
          mutate(midpoint = sum(.data$price * .data$size) / sum(.data$size)) |>
          ungroup() |>
          select(-.data$time_bucket)
      } else {
        stop("VWAP method requires 'size' column")
      }
    }
  )

  # Calculate side indicator (1 for buy, -1 for sell)
  data <- data |>
    mutate(
      side_indicator = ifelse(.data$side == "B", 1, -1)
    )

  # Calculate effective spread components
  data <- data |>
    mutate(
      price_diff = .data$price - .data$midpoint,
      effective_spread = 2 * abs(.data$price_diff),
      signed_spread = 2 * .data$price_diff * .data$side_indicator,
      relative_spread = 100 * .data$effective_spread / (.data$midpoint + 1e-10)
    )

  # Select relevant columns
  result_cols <- c("timestamp", "side", "price", "midpoint",
                   "effective_spread", "signed_spread", "relative_spread")

  if ("size" %in% names(data)) {
    result_cols <- c(result_cols, "size")
  }

  data |>
    select(all_of(result_cols))
}


#' Decompose Spread Components
#'
#' @description
#' Decomposes the effective spread into adverse selection and realized spread
#' components following Huang & Stoll (1996) and Glosten & Harris (1988).
#'
#' @param data Trade-level data with timestamp, side, size, and price columns
#' @param horizon Number of periods to measure price reversion (default: 5)
#' @param window Aggregation window (default: "1 sec")
#'
#' @return A list with:
#'   - adverse_selection: adverse selection component (information cost)
#'   - realized_spread: realized spread component (liquidity provision profit)
#'   - price_impact: measure of permanent price impact
#'   - pct_adverse_selection: percentage due to adverse selection
#'   - data: tibble with trade-level decomposition
#'
#' @details
#' Spread Decomposition:
#'
#' Effective Spread = Realized Spread + Adverse Selection Cost
#'
#' Where:
#' - Realized Spread: Profit to liquidity providers (temporary component)
#' - Adverse Selection: Cost of trading with informed traders (permanent component)
#'
#' Calculation:
#' - Adverse Selection = 2 * (Midpoint_{t+k} - Midpoint_t) * Direction
#' - Realized Spread = Effective Spread - Adverse Selection
#'
#' High adverse selection indicates:
#' - Presence of informed traders
#' - Higher risk for market makers
#' - Need for wider spreads
#'
#' @export
#' @importFrom dplyr mutate lag lead arrange group_by summarise
#' @importFrom stats median
#'
#' @examples
#' trades <- simulate_orders(n = 2000, seed = 123, drift = 0.02)
#'
#' # Decompose spread
#' decomp <- decompose_spread(trades, horizon = 5)
#'
#' print(paste("Adverse Selection:", round(decomp$adverse_selection, 5)))
#' print(paste("Realized Spread:", round(decomp$realized_spread, 5)))
#' print(paste("% Adverse Selection:", round(decomp$pct_adverse_selection, 1), "%"))
#'
#' # Examine distribution
#' hist(decomp$data$adverse_selection_cost,
#'      main = "Distribution of Adverse Selection Costs",
#'      xlab = "Adverse Selection per Trade")
decompose_spread <- function(data,
                             horizon = 5,
                             window = "1 sec") {

  # Validate inputs
  required <- c("timestamp", "side", "price")
  if (!all(required %in% names(data))) {
    stop("Data must contain columns: ", paste(required, collapse = ", "))
  }

  if (horizon < 1 || horizon > 100) {
    stop("horizon must be between 1 and 100")
  }

  # Sort by timestamp
  data <- data |>
    arrange(.data$timestamp)

  # Calculate effective spread first
  spread_data <- compute_effective_spread(data, midpoint_method = "rolling")

  # Calculate future midpoint (after horizon periods)
  spread_data <- spread_data |>
    mutate(
      midpoint_future = lead(.data$midpoint, n = horizon),
      side_indicator = ifelse(.data$side == "B", 1, -1)
    )

  # Calculate adverse selection component
  # This is the permanent price impact
  spread_data <- spread_data |>
    mutate(
      price_impact = 2 * (.data$midpoint_future - .data$midpoint) * .data$side_indicator,
      adverse_selection_cost = .data$price_impact,
      realized_spread_trade = .data$effective_spread - .data$adverse_selection_cost
    )

  # Remove NAs from lead/lag operations
  complete_data <- spread_data |>
    filter(!is.na(.data$midpoint_future))

  if (nrow(complete_data) < 10) {
    stop("Insufficient data for decomposition. Need at least ", horizon + 10, " trades.")
  }

  # Calculate average components
  adverse_selection_avg <- mean(complete_data$adverse_selection_cost, na.rm = TRUE)
  realized_spread_avg <- mean(complete_data$realized_spread_trade, na.rm = TRUE)
  effective_spread_avg <- mean(complete_data$effective_spread, na.rm = TRUE)

  # Calculate percentage
  pct_adverse <- if (effective_spread_avg != 0) {
    100 * adverse_selection_avg / effective_spread_avg
  } else {
    NA_real_
  }

  result <- list(
    adverse_selection = adverse_selection_avg,
    realized_spread = realized_spread_avg,
    effective_spread = effective_spread_avg,
    price_impact = mean(complete_data$price_impact, na.rm = TRUE),
    pct_adverse_selection = pct_adverse,
    horizon = horizon,
    n_trades = nrow(complete_data),
    data = complete_data
  )

  class(result) <- c("spread_decomposition", "list")
  result
}


#' Print method for spread decomposition results
#' @export
print.spread_decomposition <- function(x, ...) {
  cat("Spread Decomposition Analysis\n")
  cat("==============================\n\n")

  cat("Horizon:", x$horizon, "periods\n")
  cat("N trades:", x$n_trades, "\n\n")

  cat("Average Effective Spread:", format(x$effective_spread, scientific = TRUE, digits = 4), "\n")
  cat("Average Realized Spread: ", format(x$realized_spread, scientific = TRUE, digits = 4), "\n")
  cat("Average Adverse Selection:", format(x$adverse_selection, scientific = TRUE, digits = 4),
      sprintf(" (%.1f%% of spread)\n", x$pct_adverse_selection))

  cat("\nInterpretation:\n")
  if (x$pct_adverse_selection > 70) {
    cat("  High adverse selection - significant informed trading present\n")
  } else if (x$pct_adverse_selection > 40) {
    cat("  Moderate adverse selection - some informed trading\n")
  } else if (x$pct_adverse_selection > 0) {
    cat("  Low adverse selection - mostly uninformed flow\n")
  } else {
    cat("  Negative adverse selection - possible overreaction or noise\n")
  }

  invisible(x)
}
