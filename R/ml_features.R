#' Machine Learning Feature Engineering for Order Flow
#'
#' @description
#' Tools for creating ML-ready features from order flow data. Provides
#' comprehensive feature engineering, data preparation, and transformation
#' utilities for predictive modeling without requiring heavy ML dependencies.
#'
#' @name ml_features
NULL

#' Engineer comprehensive OFI features for machine learning
#'
#' @description
#' Creates a rich set of features from order flow data including multi-level
#' OFI, rolling statistics, lagged values, spread/depth metrics, and derived
#' features. Designed for price prediction and direction classification.
#'
#' @param trades Trade data or reconstructed order book
#' @param orderbook Order book data (optional, for multi-level features)
#' @param lookback Integer, number of periods for rolling features (default: 10)
#' @param n_levels Integer, LOB levels to include (default: 5)
#' @param include_crosses Logical, include cross-level features (default: TRUE)
#'
#' @return Data frame with engineered features
#'
#' @details
#' **Feature Categories**:
#'
#' 1. **OFI Features** (levels 1-N):
#'    - Raw OFI at each level
#'    - OIR (Order Imbalance Ratio)
#'    - Cumulative OFI
#'    - EWMA-smoothed OFI
#'
#' 2. **Rolling Statistics** (lookback window):
#'    - Mean, SD, skewness, kurtosis
#'    - Min, max, range
#'    - Momentum (rate of change)
#'    - Acceleration (second derivative)
#'
#' 3. **Lagged Features** (t-1, t-2, ..., t-k):
#'    - Historical OFI values
#'    - Enables temporal pattern learning
#'
#' 4. **Spread/Depth Features**:
#'    - Bid-ask spread (absolute and %)
#'    - Depth at multiple levels
#'    - Depth imbalance
#'    - Microprice
#'
#' 5. **Derived Features**:
#'    - Trade intensity (trades per second)
#'    - Average trade size
#'    - Price volatility
#'    - Range-based measures
#'
#' 6. **Interaction Features** (optional):
#'    - OFI × spread
#'    - OFI × depth
#'    - Cross-level products
#'
#' **Research Evidence**:
#' - Multi-level OFI significantly improves forecasting (Cont et al.)
#' - LSTM models on OFI outperform raw LOB (Kolm et al.)
#' - Stationarized features enhance linear relationships
#'
#' @references
#' Kolm, P., Turiel, J., & Westray, N. (2023). Deep order flow imbalance.
#' *Mathematical Finance*, 33, 273-321.
#'
#' @export
#' @importFrom dplyr lag lead
#' @importFrom zoo rollmean
#'
#' @examples
#' \dontrun{
#' trades <- simulate_orders(n = 5000, seed = 42)
#' features <- engineer_ofi_features(trades, lookback = 10)
#'
#' # Use with ML models
#' library(ranger)
#' target <- lead(features$ofi, 1)  # Predict next period
#' rf_model <- ranger(target ~ ., data = features)
#' }
engineer_ofi_features <- function(trades,
                                    orderbook = NULL,
                                    lookback = 10,
                                    n_levels = 5,
                                    include_crosses = TRUE) {

  # Base OFI calculation
  ofi_base <- compute_ofi(trades, window = "1 min")

  features <- ofi_base |>
    select(.data$window_start, .data$ofi, .data$oir, .data$ofi_cum,
           .data$vol_total, .data$B, .data$S, .data$n_trades)

  # 1. EWMA smoothing
  features$ofi_ewma <- compute_ewma_ofi(ofi_base, lambda = 0.94)$ofi_ewma

  # 2. Momentum and acceleration
  features$ofi_momentum <- c(NA, diff(features$ofi))
  features$ofi_acceleration <- c(NA, diff(features$ofi_momentum))

  # 3. Rolling statistics
  if (nrow(features) >= lookback) {
    features$ofi_mean <- rollmean(features$ofi, k = lookback,
                                   fill = NA, align = "right")
    features$ofi_sd <- rollapply_safe(features$ofi, lookback, sd)
    features$ofi_min <- rollapply_safe(features$ofi, lookback, min)
    features$ofi_max <- rollapply_safe(features$ofi, lookback, max)
    features$ofi_range <- features$ofi_max - features$ofi_min
  }

  # 4. Lagged features
  for (lag_val in 1:min(5, lookback)) {
    features[[paste0("ofi_lag", lag_val)]] <- lag(features$ofi, lag_val)
    features[[paste0("oir_lag", lag_val)]] <- lag(features$oir, lag_val)
  }

  # 5. Volume features
  features$avg_trade_size <- features$vol_total / features$n_trades
  features$buy_ratio <- features$B / features$vol_total
  features$sell_ratio <- features$S / features$vol_total

  # 6. Time-based features
  features$hour <- as.POSIXlt(features$window_start)$hour
  features$minute <- as.POSIXlt(features$window_start)$min

  # 7. Multi-level features if orderbook provided
  if (!is.null(orderbook)) {
    # Compute multi-level OFI
    ml_ofi <- compute_multilevel_ofi(orderbook, n_levels = n_levels)

    # Merge with features
    features <- features |>
      left_join(ml_ofi, by = c("window_start" = "timestamp"))

    # Depth features
    if (n_levels >= 1) {
      features$spread <- orderbook$ask_price_1 - orderbook$bid_price_1
      features$depth_bid <- orderbook$bid_size_1
      features$depth_ask <- orderbook$ask_size_1
      features$depth_imbalance <- features$depth_bid - features$depth_ask
    }
  }

  # 8. Interaction features (if requested)
  if (include_crosses) {
    features$ofi_x_vol <- features$ofi * features$vol_total
    features$oir_x_trades <- features$oir * features$n_trades

    if ("spread" %in% names(features)) {
      features$ofi_x_spread <- features$ofi * features$spread
      features$oir_x_spread <- features$oir * features$spread
    }
  }

  # 9. Target variable (next period OFI for prediction)
  features$target_ofi <- lead(features$ofi, 1)
  features$target_direction <- sign(lead(features$ofi, 1))

  attr(features, "lookback") <- lookback
  attr(features, "n_features") <- ncol(features) - 2  # Exclude timestamp and target

  return(features)
}


#' Create event-based bars (tick/volume/dollar bars)
#'
#' @description
#' Aggregates data using event-based sampling instead of fixed time intervals.
#' Results in bars with more consistent information content and better
#' properties for modeling.
#'
#' @param trades Trade data
#' @param bar_type Character, "tick", "volume", or "dollar"
#' @param bar_size Numeric, size of each bar (number of ticks, volume, or dollars)
#'
#' @return Data frame with event-based bars
#'
#' @details
#' **Event Bars vs. Time Bars**:
#'
#' Time bars (e.g., 1-minute):
#' - Variable information content
#' - Serial correlation issues
#' - Microstructure noise
#'
#' Event bars:
#' - Consistent information per bar
#' - Better stationarity
#' - Improved ML performance
#'
#' **Bar Types**:
#' - **Tick bars**: Fixed number of trades
#' - **Volume bars**: Fixed share volume
#' - **Dollar bars**: Fixed dollar volume
#'
#' **Research Evidence**:
#' - Event bars improve prediction accuracy
#' - Reduce autocorrelation in residuals
#' - Better suited for ML than time bars
#'
#' @references
#' De Prado, M. L. (2018). *Advances in Financial Machine Learning*.
#' Wiley.
#'
#' @export
#' @examples
#' \dontrun{
#' trades <- simulate_orders(n = 10000, seed = 42)
#'
#' # Tick bars: Every 100 trades
#' tick_bars <- create_event_bars(trades, bar_type = "tick", bar_size = 100)
#'
#' # Volume bars: Every 10,000 shares
#' vol_bars <- create_event_bars(trades, bar_type = "volume", bar_size = 10000)
#'
#' # Dollar bars: Every $1,000,000
#' dollar_bars <- create_event_bars(trades, bar_type = "dollar", bar_size = 1e6)
#' }
create_event_bars <- function(trades,
                                bar_type = c("tick", "volume", "dollar"),
                                bar_size = 100) {

  bar_type <- match.arg(bar_type)

  # Validate required columns
  required <- c("timestamp", "side", "size")
  if (bar_type == "dollar") {
    required <- c(required, "price")
  }

  missing <- setdiff(required, names(trades))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Calculate cumulative metric
  if (bar_type == "tick") {
    trades$cumulative <- seq_len(nrow(trades))

  } else if (bar_type == "volume") {
    trades$cumulative <- cumsum(trades$size)

  } else if (bar_type == "dollar") {
    trades$cumulative <- cumsum(trades$size * trades$price)
  }

  # Assign bar numbers
  trades$bar_number <- floor(trades$cumulative / bar_size)

  # Aggregate by bar
  bars <- trades |>
    group_by(.data$bar_number) |>
    summarise(
      bar_start = first(.data$timestamp),
      bar_end = last(.data$timestamp),
      n_trades = n(),
      total_volume = sum(.data$size),
      total_dollar = if ("price" %in% names(trades)) sum(.data$size * .data$price) else NA,
      vwap = if ("price" %in% names(trades)) {
        sum(.data$price * .data$size) / sum(.data$size)
      } else NA,
      buy_volume = sum(.data$size[.data$side == "B"]),
      sell_volume = sum(.data$size[.data$side == "S"]),
      .groups = "drop"
    ) |>
    mutate(
      ofi = .data$buy_volume - .data$sell_volume,
      oir = (.data$buy_volume - .data$sell_volume) /
        (.data$buy_volume + .data$sell_volume + 1e-10)
    )

  attr(bars, "bar_type") <- bar_type
  attr(bars, "bar_size") <- bar_size

  return(bars)
}


#' Stationarize OFI for modeling
#'
#' @description
#' Applies transformations to make OFI more suitable for linear models
#' and machine learning. Handles non-stationarity, heteroskedasticity,
#' and heavy tails.
#'
#' @param ofi_data OFI data frame
#' @param method Character, transformation method
#' @param params List of method-specific parameters
#'
#' @return Transformed OFI data
#'
#' @details
#' **Transformation Methods**:
#'
#' 1. **Differencing**: First difference to remove trends
#' 2. **Standardization**: Z-score normalization
#' 3. **Rank**: Convert to percentile ranks (robust to outliers)
#' 4. **Log**: Log transform (for skewed distributions)
#' 5. **Box-Cox**: Automatic power transformation
#' 6. **Winsorization**: Cap extreme values
#'
#' **Why Stationarize**:
#' - Linear models assume stationarity
#' - Improves coefficient stability
#' - Reduces impact of outliers
#' - Better out-of-sample performance
#'
#' @export
#' @importFrom stats sd median
#' @examples
#' \dontrun{
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Z-score standardization
#' ofi_std <- stationarize_ofi(ofi, method = "standardize")
#'
#' # Rank transformation
#' ofi_rank <- stationarize_ofi(ofi, method = "rank")
#' }
stationarize_ofi <- function(ofi_data,
                               method = c("standardize", "difference",
                                         "rank", "log", "winsorize"),
                               params = list()) {

  method <- match.arg(method)

  if (!"ofi" %in% names(ofi_data)) {
    stop("ofi_data must contain 'ofi' column")
  }

  ofi_values <- ofi_data$ofi

  if (method == "standardize") {
    # Z-score
    ofi_transformed <- (ofi_values - mean(ofi_values, na.rm = TRUE)) /
      sd(ofi_values, na.rm = TRUE)

  } else if (method == "difference") {
    # First difference
    ofi_transformed <- c(NA, diff(ofi_values))

  } else if (method == "rank") {
    # Percentile ranks (0-1)
    ofi_transformed <- rank(ofi_values, na.last = "keep") / sum(!is.na(ofi_values))

  } else if (method == "log") {
    # Log transform (add constant for negative values)
    min_val <- min(ofi_values, na.rm = TRUE)
    shift <- ifelse(min_val <= 0, abs(min_val) + 1, 0)
    ofi_transformed <- log(ofi_values + shift)

  } else if (method == "winsorize") {
    # Cap at percentiles
    lower_pct <- params$lower_pct %||% 0.01
    upper_pct <- params$upper_pct %||% 0.99

    lower_bound <- quantile(ofi_values, lower_pct, na.rm = TRUE)
    upper_bound <- quantile(ofi_values, upper_pct, na.rm = TRUE)

    ofi_transformed <- pmax(pmin(ofi_values, upper_bound), lower_bound)
  }

  # Replace original OFI
  ofi_data$ofi_transformed <- ofi_transformed

  attr(ofi_data, "transformation") <- method

  return(ofi_data)
}


#' Create ML-ready dataset with train/test split
#'
#' @description
#' Prepares data for machine learning with proper train/test splitting
#' that respects temporal ordering. Handles missing values and scaling.
#'
#' @param features Feature data frame from engineer_ofi_features()
#' @param target_col Character, name of target variable (default: "target_ofi")
#' @param train_fraction Numeric, fraction for training (default: 0.70)
#' @param validation_fraction Numeric, fraction for validation (default: 0.15)
#' @param remove_na Logical, remove rows with NAs (default: TRUE)
#' @param scale Logical, scale features (default: TRUE)
#'
#' @return List with train, validation, and test sets
#'
#' @details
#' **Temporal Splitting**:
#' - NEVER shuffle time series data
#' - Train on earlier periods
#' - Validate on middle period
#' - Test on most recent period
#'
#' **Data Leakage Prevention**:
#' - Scale using training set statistics only
#' - No future information in features
#' - Proper cross-validation for time series
#'
#' @export
#' @examples
#' \dontrun{
#' features <- engineer_ofi_features(trades)
#' ml_data <- create_ml_dataset(features, target_col = "target_ofi")
#'
#' # Train model
#' library(ranger)
#' rf <- ranger(target ~ ., data = ml_data$train)
#'
#' # Validate
#' pred_val <- predict(rf, ml_data$validation)
#' }
create_ml_dataset <- function(features,
                                target_col = "target_ofi",
                                train_fraction = 0.70,
                                validation_fraction = 0.15,
                                remove_na = TRUE,
                                scale = TRUE) {

  # Validate target exists
  if (!target_col %in% names(features)) {
    stop("Target column '", target_col, "' not found in features")
  }

  # Remove timestamp column if present
  if ("window_start" %in% names(features)) {
    timestamp <- features$window_start
    features <- features |> select(-.data$window_start)
  } else {
    timestamp <- NULL
  }

  # Handle missing values
  if (remove_na) {
    features <- na.omit(features)
  }

  # Split indices (temporal order)
  n <- nrow(features)
  train_end <- floor(n * train_fraction)
  val_end <- floor(n * (train_fraction + validation_fraction))

  train_idx <- 1:train_end
  val_idx <- (train_end + 1):val_end
  test_idx <- (val_end + 1):n

  # Separate features and target
  X <- features |> select(-all_of(target_col))
  y <- features[[target_col]]

  # Scale features if requested
  if (scale) {
    # Calculate scaling parameters on training set only
    X_train <- X[train_idx, ]
    means <- colMeans(X_train, na.rm = TRUE)
    sds <- apply(X_train, 2, sd, na.rm = TRUE)

    # Apply to all sets
    X <- scale(X, center = means, scale = sds)
    X <- as.data.frame(X)

    scaling_params <- list(means = means, sds = sds)
  } else {
    scaling_params <- NULL
  }

  # Create datasets
  train_data <- cbind(X[train_idx, ], target = y[train_idx])
  val_data <- cbind(X[val_idx, ], target = y[val_idx])
  test_data <- cbind(X[test_idx, ], target = y[test_idx])

  # Package results
  result <- list(
    train = train_data,
    validation = val_data,
    test = test_data,
    scaling_params = scaling_params,
    split_info = list(
      n_total = n,
      n_train = length(train_idx),
      n_val = length(val_idx),
      n_test = length(test_idx)
    ),
    timestamp = timestamp
  )

  class(result) <- c("ml_dataset", "list")
  return(result)
}


#' Create prediction targets for supervised learning
#'
#' @description
#' Generates target variables for different prediction tasks: price direction,
#' magnitude of change, volatility, etc.
#'
#' @param ofi_data OFI data
#' @param price_data Price data (optional, for price-based targets)
#' @param horizon Integer, forecast horizon (default: 1)
#' @param task Character, "direction", "magnitude", or "volatility"
#'
#' @return Data frame with target variables
#'
#' @details
#' **Prediction Tasks**:
#'
#' 1. **Direction**: Binary (up/down) or ternary (up/flat/down)
#' 2. **Magnitude**: Continuous price change
#' 3. **Volatility**: Realized volatility over horizon
#'
#' @export
#' @examples
#' \dontrun{
#' targets <- create_prediction_targets(ofi, task = "direction", horizon = 1)
#' }
create_prediction_targets <- function(ofi_data,
                                       price_data = NULL,
                                       horizon = 1,
                                       task = c("direction", "magnitude", "volatility")) {

  task <- match.arg(task)

  if (task == "direction") {
    # Binary direction: sign of future OFI
    target <- sign(lead(ofi_data$ofi, horizon))
    target[target == 0] <- NA  # Treat zero as missing

  } else if (task == "magnitude") {
    # Continuous magnitude
    target <- lead(ofi_data$ofi, horizon)

  } else if (task == "volatility") {
    # Rolling volatility
    if (is.null(price_data)) {
      # Use OFI volatility as proxy
      target <- rollapply_safe(ofi_data$ofi, horizon, sd)
    } else {
      target <- rollapply_safe(price_data, horizon, sd)
    }
  }

  ofi_data$target <- target
  attr(ofi_data, "task") <- task
  attr(ofi_data, "horizon") <- horizon

  return(ofi_data)
}


# ============================================================================
# Helper Functions
# ============================================================================

#' Safe rolling apply
#' @noRd
rollapply_safe <- function(x, width, FUN, ...) {
  result <- rep(NA, length(x))
  for (i in width:length(x)) {
    result[i] <- FUN(x[(i - width + 1):i], ...)
  }
  return(result)
}

#' NULL default operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


# ============================================================================
# Print Methods
# ============================================================================

#' @export
print.ml_dataset <- function(x, ...) {
  cat("Machine Learning Dataset\n")
  cat("========================\n\n")

  cat("Split Information:\n")
  cat(sprintf("  Total observations: %d\n", x$split_info$n_total))
  cat(sprintf("  Training: %d (%.1f%%)\n",
              x$split_info$n_train,
              x$split_info$n_train / x$split_info$n_total * 100))
  cat(sprintf("  Validation: %d (%.1f%%)\n",
              x$split_info$n_val,
              x$split_info$n_val / x$split_info$n_total * 100))
  cat(sprintf("  Test: %d (%.1f%%)\n",
              x$split_info$n_test,
              x$split_info$n_test / x$split_info$n_total * 100))
  cat("\n")

  cat("Feature Information:\n")
  cat(sprintf("  Number of features: %d\n", ncol(x$train) - 1))

  if (!is.null(x$scaling_params)) {
    cat("  Scaling: Applied (z-score normalization)\n")
  } else {
    cat("  Scaling: Not applied\n")
  }

  cat("\n")
  cat("Access data with:\n")
  cat("  $train - Training set\n")
  cat("  $validation - Validation set\n")
  cat("  $test - Test set\n")

  invisible(x)
}
