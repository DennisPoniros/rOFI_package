#' Compute Order-Flow Imbalance metrics
#'
#' @description 
#' Calculates various order-flow imbalance (OFI) metrics from standardized trade data.
#' Supports calendar-based windows, rolling windows, or event-count windows.
#'
#' @param data A tibble with columns: timestamp, side, size, and optionally price
#' @param window Character string for calendar windows (e.g., "1 min", "5 min", "1 hour")
#' @param rolling A lubridate period object for rolling windows (e.g., lubridate::dseconds(60))
#' @param n_ticks Integer for event-count based windows
#' @param price_weighted Logical, whether to compute price-weighted OFI (requires price column)
#' @param eps Small value to prevent division by zero in ratios (default: 1e-8)
#'
#' @return A tibble with OFI metrics for each time window:
#' - window_start: Start of the time window
#' - ofi: Simple order-flow imbalance (buy_size - sell_size)
#' - oir: Order imbalance ratio (buy - sell)/(buy + sell + eps)
#' - ofi_cum: Cumulative OFI
#' - B: Total buy volume
#' - S: Total sell volume  
#' - vol_total: Total volume (B + S)
#' - n_trades: Number of trades in window
#' - ofi_dollar: Dollar-weighted OFI (if price available)
#'
#' @details
#' Only one windowing method can be specified at a time (window, rolling, or n_ticks).
#' 
#' Calendar windows use lubridate::floor_date() for consistent bucketing.
#' Rolling windows calculate metrics for overlapping periods.
#' Event-count windows group by fixed number of trades regardless of time.
#'
#' @export
#' @importFrom dplyr group_by summarise mutate arrange ungroup n
#' @importFrom tidyr replace_na
#' @importFrom lubridate floor_date
#' @importFrom slider slide_period_dbl slide_index_dbl
#' @importFrom tibble tibble
#' @importFrom rlang .data
#'
#' @examples
#' # ===========================================
#' # Example 1: Basic OFI Calculation
#' # ===========================================
#'
#' # Generate sample data
#' trades <- simulate_orders(n = 1000, seed = 123)
#'
#' # Calculate OFI with 1-minute windows
#' ofi_1min <- compute_ofi(trades, window = "1 min")
#'
#' # View results
#' head(ofi_1min)
#'
#' # Check summary statistics
#' summary(ofi_1min$oir)  # Order Imbalance Ratio
#'
#' # ===========================================
#' # Example 2: Different Time Scales
#' # ===========================================
#'
#' # Fast: 30-second windows (more responsive, noisier)
#' ofi_30s <- compute_ofi(trades, window = "30 sec")
#'
#' # Medium: 1-minute windows
#' ofi_1m <- compute_ofi(trades, window = "1 min")
#'
#' # Slow: 5-minute windows (smoother, less responsive)
#' ofi_5m <- compute_ofi(trades, window = "5 min")
#'
#' # Compare number of windows
#' cat("30s windows:", nrow(ofi_30s), "\n")
#' cat("1m windows:", nrow(ofi_1m), "\n")
#' cat("5m windows:", nrow(ofi_5m), "\n")
#'
#' # ===========================================
#' # Example 3: Tick-Based Windows
#' # ===========================================
#'
#' # Group by number of trades instead of time
#' # Useful for markets with irregular trading activity
#' ofi_ticks <- compute_ofi(trades, n_ticks = 50)
#'
#' # Each window has exactly 50 trades
#' head(ofi_ticks)
#'
#' # ===========================================
#' # Example 4: Analyzing Results
#' # ===========================================
#'
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Find periods of strong buying pressure
#' strong_buy <- ofi[ofi$oir > 0.3, ]
#' cat("Periods with strong buying:", nrow(strong_buy), "\n")
#'
#' # Find periods of strong selling pressure
#' strong_sell <- ofi[ofi$oir < -0.3, ]
#' cat("Periods with strong selling:", nrow(strong_sell), "\n")
#'
#' # Calculate average OFI
#' avg_ofi <- mean(ofi$ofi)
#' cat("Average OFI:", round(avg_ofi, 2), "\n")
#'
#' # ===========================================
#' # Example 5: Rolling Windows (Advanced)
#' # ===========================================
#'
#' \dontrun{
#' # Overlapping 60-second windows
#' # Provides smoother transitions between periods
#' ofi_rolling <- compute_ofi(trades, rolling = lubridate::dseconds(60))
#' head(ofi_rolling)
#' }
compute_ofi <- function(data,
                       window = "1 min",
                       rolling = NULL,
                       n_ticks = NULL,
                       price_weighted = FALSE,
                       eps = 1e-8) {
  
  # Input validation
  if (!all(c("timestamp", "side", "size") %in% names(data))) {
    stop("Data must have columns: timestamp, side, size")
  }
  
  if (price_weighted && !"price" %in% names(data)) {
    stop("price_weighted = TRUE requires a 'price' column in data")
  }
  
  # Check windowing parameters (only one allowed)
  window_params <- c(!is.null(window), !is.null(rolling), !is.null(n_ticks))
  if (sum(window_params) > 1) {
    stop("Only one of 'window', 'rolling', or 'n_ticks' may be specified")
  }
  if (sum(window_params) == 0) {
    window <- "1 min"  # Default
  }
  
  # Sort data by timestamp
  data <- data |>
    arrange(.data$timestamp)
  
  # Choose computation method based on window type
  if (!is.null(n_ticks)) {
    result <- compute_ofi_ticks(data, n_ticks, eps, price_weighted)
  } else if (!is.null(rolling)) {
    result <- compute_ofi_rolling(data, rolling, eps, price_weighted)
  } else {
    result <- compute_ofi_calendar(data, window, eps, price_weighted)
  }
  
  # Add cumulative OFI
  result <- result |>
    mutate(ofi_cum = cumsum(.data$ofi))
  
  # Store parameters as attributes
  attr(result, "window_type") <- if (!is.null(n_ticks)) "ticks" else if (!is.null(rolling)) "rolling" else "calendar"
  attr(result, "window_param") <- if (!is.null(n_ticks)) n_ticks else if (!is.null(rolling)) rolling else window
  attr(result, "eps") <- eps
  
  result
}

#' Calculate OFI for calendar-based windows
#' @noRd
compute_ofi_calendar <- function(data, window, eps, price_weighted) {
  # Create time buckets
  data <- data |>
    mutate(window_start = lubridate::floor_date(.data$timestamp, window))
  
  # Calculate metrics by window
  result <- data |>
    group_by(.data$window_start) |>
    summarise(
      B = sum(.data$size[.data$side == "B"], na.rm = TRUE),
      S = sum(.data$size[.data$side == "S"], na.rm = TRUE),
      vol_total = sum(.data$size, na.rm = TRUE),
      n_trades = n(),
      .groups = "drop"
    )
  
  # Add OFI metrics
  result <- result |>
    mutate(
      ofi = .data$B - .data$S,
      oir = (.data$B - .data$S) / (.data$B + .data$S + eps)
    )
  
  # Add dollar OFI if prices available
  if (price_weighted && "price" %in% names(data)) {
    dollar_ofi <- data |>
      mutate(
        dollar_vol = .data$size * .data$price * ifelse(.data$side == "B", 1, -1)
      ) |>
      group_by(.data$window_start) |>
      summarise(
        ofi_dollar = sum(.data$dollar_vol, na.rm = TRUE),
        .groups = "drop"
      )
    
    result <- result |>
      left_join(dollar_ofi, by = "window_start")
  }
  
  result
}

#' Calculate OFI for rolling windows
#' @noRd
compute_ofi_rolling <- function(data, rolling, eps, price_weighted) {
  # Convert period to seconds for slider
  window_seconds <- as.numeric(rolling, units = "secs")
  
  # Helper function to calculate metrics for a window
  calc_window_metrics <- function(idx, data, window_seconds) {
    window_start <- data$timestamp[idx]
    window_end <- window_start + lubridate::seconds(window_seconds)
    
    window_data <- data[data$timestamp >= window_start & data$timestamp < window_end, ]
    
    if (nrow(window_data) == 0) {
      return(NULL)
    }
    
    B <- sum(window_data$size[window_data$side == "B"], na.rm = TRUE)
    S <- sum(window_data$size[window_data$side == "S"], na.rm = TRUE)
    
    list(
      window_start = window_start,
      B = B,
      S = S,
      vol_total = B + S,
      n_trades = nrow(window_data),
      ofi = B - S,
      oir = (B - S) / (B + S + eps)
    )
  }
  
  # Apply rolling window calculation
  metrics_list <- lapply(seq_len(nrow(data)), calc_window_metrics, 
                        data = data, window_seconds = window_seconds)
  
  # Remove NULL results and bind
  metrics_list <- metrics_list[!sapply(metrics_list, is.null)]
  result <- do.call(rbind, lapply(metrics_list, as.data.frame))
  result <- as_tibble(result)
  
  # Add dollar OFI if needed
  if (price_weighted && "price" %in% names(data)) {
    calc_dollar_ofi <- function(idx, data, window_seconds) {
      window_start <- data$timestamp[idx]
      window_end <- window_start + lubridate::seconds(window_seconds)
      
      window_data <- data[data$timestamp >= window_start & data$timestamp < window_end, ]
      
      if (nrow(window_data) == 0) return(NA)
      
      sum(window_data$size * window_data$price * 
          ifelse(window_data$side == "B", 1, -1), na.rm = TRUE)
    }
    
    result$ofi_dollar <- sapply(seq_len(nrow(data)), calc_dollar_ofi,
                               data = data, window_seconds = window_seconds)
  }
  
  result
}

#' Calculate OFI for tick-based windows
#' @noRd  
compute_ofi_ticks <- function(data, n_ticks, eps, price_weighted) {
  # Create tick groups
  data <- data |>
    mutate(
      tick_group = ceiling(row_number() / n_ticks)
    )
  
  # Calculate metrics by tick group
  result <- data |>
    group_by(.data$tick_group) |>
    summarise(
      window_start = min(.data$timestamp),
      B = sum(.data$size[.data$side == "B"], na.rm = TRUE),
      S = sum(.data$size[.data$side == "S"], na.rm = TRUE),
      vol_total = sum(.data$size, na.rm = TRUE),
      n_trades = n(),
      .groups = "drop"
    )
  
  # Add OFI metrics
  result <- result |>
    mutate(
      ofi = .data$B - .data$S,
      oir = (.data$B - .data$S) / (.data$B + .data$S + eps)
    ) |>
    select(-.data$tick_group)
  
  # Add dollar OFI if prices available
  if (price_weighted && "price" %in% names(data)) {
    dollar_ofi <- data |>
      mutate(
        dollar_vol = .data$size * .data$price * ifelse(.data$side == "B", 1, -1)
      ) |>
      group_by(.data$tick_group) |>
      summarise(
        ofi_dollar = sum(.data$dollar_vol, na.rm = TRUE),
        .groups = "drop"
      )
    
    result <- result |>
      mutate(tick_group = row_number()) |>
      left_join(dollar_ofi, by = "tick_group") |>
      select(-.data$tick_group)
  }
  
  result
}
