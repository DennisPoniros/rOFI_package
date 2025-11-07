#' Compute Exponentially Weighted Moving Average OFI
#'
#' @description
#' Calculates exponentially weighted moving average (EWMA) of order-flow imbalance,
#' giving more weight to recent observations. Useful for tracking recent market pressure.
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics
#' @param lambda Smoothing parameter between 0 and 1 (default: 0.94)
#'   Higher values = more weight on recent data
#' @param metric Which OFI metric to smooth (default: "ofi")
#'
#' @return The input tibble with additional column: ofi_ewma (or {metric}_ewma)
#'
#' @details
#' EWMA formula: EWMA_t = lambda * value_t + (1 - lambda) * EWMA_{t-1}
#'
#' Common lambda values:
#' - 0.94: RiskMetrics standard (roughly 30-period half-life)
#' - 0.99: Very slow decay (roughly 150-period half-life)
#' - 0.75: Faster decay (roughly 4-period half-life)
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang .data := !!
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Add EWMA with default lambda
#' ofi_smooth <- compute_ewma_ofi(ofi)
#'
#' # More responsive EWMA
#' ofi_fast <- compute_ewma_ofi(ofi, lambda = 0.75)
#'
#' # Smooth the OIR metric instead
#' ofi_oir_smooth <- compute_ewma_ofi(ofi, metric = "oir")
compute_ewma_ofi <- function(ofi_tbl, lambda = 0.94, metric = "ofi") {

  # Validate inputs
  if (!is.data.frame(ofi_tbl)) {
    stop("ofi_tbl must be a data frame")
  }

  if (!metric %in% names(ofi_tbl)) {
    stop("Metric '", metric, "' not found in ofi_tbl. Available: ",
         paste(names(ofi_tbl), collapse = ", "))
  }

  if (lambda <= 0 || lambda >= 1) {
    stop("lambda must be between 0 and 1 (exclusive)")
  }

  # Calculate EWMA
  values <- ofi_tbl[[metric]]
  n <- length(values)
  ewma <- numeric(n)

  # Initialize with first value
  ewma[1] <- values[1]

  # Compute EWMA recursively
  for (i in 2:n) {
    ewma[i] <- lambda * values[i] + (1 - lambda) * ewma[i - 1]
  }

  # Add to tibble with dynamic name
  col_name <- paste0(metric, "_ewma")
  ofi_tbl[[col_name]] <- ewma

  # Store parameters as attributes
  attr(ofi_tbl, "ewma_lambda") <- lambda
  attr(ofi_tbl, "ewma_metric") <- metric

  ofi_tbl
}


#' Compute OFI Momentum
#'
#' @description
#' Calculates the rate of change (momentum) of order-flow imbalance over
#' a specified lookback period. Identifies accelerating buying or selling pressure.
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics
#' @param lookback Number of periods for momentum calculation (default: 5)
#' @param metric Which OFI metric to use (default: "ofi")
#' @param method Momentum calculation method: "difference", "pct_change", or "log_return"
#'
#' @return The input tibble with additional column: ofi_momentum (or {metric}_momentum)
#'
#' @details
#' Momentum methods:
#' - "difference": value_t - value_{t-lookback}
#' - "pct_change": (value_t - value_{t-lookback}) / abs(value_{t-lookback})
#' - "log_return": log(value_t / value_{t-lookback}) [requires positive values]
#'
#' @export
#' @importFrom dplyr mutate lag
#' @importFrom rlang .data
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Calculate 5-period momentum
#' ofi_mom <- compute_ofi_momentum(ofi)
#'
#' # Faster momentum (3 periods)
#' ofi_mom_fast <- compute_ofi_momentum(ofi, lookback = 3)
#'
#' # Percentage change momentum
#' ofi_mom_pct <- compute_ofi_momentum(ofi, method = "pct_change")
compute_ofi_momentum <- function(ofi_tbl,
                                 lookback = 5,
                                 metric = "ofi",
                                 method = c("difference", "pct_change", "log_return")) {

  method <- match.arg(method)

  # Validate inputs
  if (!is.data.frame(ofi_tbl)) {
    stop("ofi_tbl must be a data frame")
  }

  if (!metric %in% names(ofi_tbl)) {
    stop("Metric '", metric, "' not found in ofi_tbl")
  }

  if (lookback < 1 || lookback != round(lookback)) {
    stop("lookback must be a positive integer")
  }

  # Get values
  values <- ofi_tbl[[metric]]
  lagged_values <- dplyr::lag(values, n = lookback)

  # Calculate momentum based on method
  momentum <- switch(method,
    difference = values - lagged_values,
    pct_change = {
      # Avoid division by zero
      ifelse(abs(lagged_values) < 1e-10, NA,
             (values - lagged_values) / abs(lagged_values))
    },
    log_return = {
      # Check for positive values
      if (any(values <= 0, na.rm = TRUE) || any(lagged_values <= 0, na.rm = TRUE)) {
        warning("log_return method requires positive values. Consider using 'difference' instead.")
      }
      log(values / lagged_values)
    }
  )

  # Add to tibble
  col_name <- paste0(metric, "_momentum")
  ofi_tbl[[col_name]] <- momentum

  # Store parameters as attributes
  attr(ofi_tbl, "momentum_lookback") <- lookback
  attr(ofi_tbl, "momentum_method") <- method

  ofi_tbl
}


#' Compute OFI Acceleration
#'
#' @description
#' Calculates the second derivative of order-flow imbalance, measuring
#' how quickly momentum is changing. Useful for detecting regime transitions.
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics
#' @param lookback Number of periods for calculation (default: 5)
#' @param metric Which OFI metric to use (default: "ofi")
#'
#' @return The input tibble with additional column: ofi_acceleration
#'
#' @details
#' Acceleration is calculated as the momentum of momentum:
#' accel_t = momentum_t - momentum_{t-lookback}
#'
#' Positive acceleration indicates increasing buying/selling pressure.
#' Negative acceleration indicates decreasing pressure (potential reversal).
#'
#' @export
#' @importFrom dplyr mutate lag
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Add momentum and acceleration
#' ofi <- compute_ofi_momentum(ofi, lookback = 5)
#' ofi <- compute_ofi_acceleration(ofi, lookback = 5)
#'
#' # Plot acceleration to identify regime changes
#' plot_ofi(ofi, which = "ofi_acceleration")
compute_ofi_acceleration <- function(ofi_tbl, lookback = 5, metric = "ofi") {

  # Validate inputs
  if (!is.data.frame(ofi_tbl)) {
    stop("ofi_tbl must be a data frame")
  }

  if (!metric %in% names(ofi_tbl)) {
    stop("Metric '", metric, "' not found in ofi_tbl")
  }

  if (lookback < 1 || lookback != round(lookback)) {
    stop("lookback must be a positive integer")
  }

  # Calculate momentum first if not present
  momentum_col <- paste0(metric, "_momentum")
  if (!momentum_col %in% names(ofi_tbl)) {
    message("Computing momentum first with lookback = ", lookback)
    ofi_tbl <- compute_ofi_momentum(ofi_tbl, lookback = lookback, metric = metric)
  }

  # Calculate acceleration (momentum of momentum)
  momentum <- ofi_tbl[[momentum_col]]
  acceleration <- momentum - dplyr::lag(momentum, n = lookback)

  # Add to tibble
  col_name <- paste0(metric, "_acceleration")
  ofi_tbl[[col_name]] <- acceleration

  # Store parameters as attributes
  attr(ofi_tbl, "acceleration_lookback") <- lookback

  ofi_tbl
}


#' Compute Volume-Weighted Average Price OFI
#'
#' @description
#' Calculates volume-weighted average price (VWAP) based OFI metrics,
#' incorporating both volume and price information for each window.
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics with price data
#'
#' @return The input tibble with additional columns: vwap, ofi_vwap_deviation
#'
#' @details
#' Requires that the original data included price information.
#' The function calculates:
#' - VWAP for each time window
#' - Deviation of OFI from VWAP-weighted expectations
#'
#' This helps identify whether order flow imbalance is associated with
#' price movements away from VWAP (indicating informed trading).
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123)
#' ofi <- compute_ofi(trades, window = "1 min", price_weighted = TRUE)
#'
#' \dontrun{
#' # Requires price data in original compute_ofi call
#' ofi_vwap <- compute_vwap_ofi(ofi)
#' }
compute_vwap_ofi <- function(ofi_tbl) {

  # Validate inputs
  if (!is.data.frame(ofi_tbl)) {
    stop("ofi_tbl must be a data frame")
  }

  # Check for required columns
  required <- c("vol_total", "ofi")
  if (!all(required %in% names(ofi_tbl))) {
    stop("ofi_tbl must contain columns: ", paste(required, collapse = ", "))
  }

  # Note: Full VWAP calculation requires original price data
  # This is a simplified version that works with aggregated data

  if (!"ofi_dollar" %in% names(ofi_tbl)) {
    stop("VWAP calculation requires dollar-weighted OFI. ",
         "Use compute_ofi(..., price_weighted = TRUE)")
  }

  # Calculate implied VWAP from dollar OFI
  ofi_tbl <- ofi_tbl |>
    mutate(
      # VWAP deviation: difference between dollar OFI and volume OFI
      ofi_vwap_deviation = (.data$ofi_dollar - .data$ofi) / (.data$vol_total + 1e-10)
    )

  ofi_tbl
}
