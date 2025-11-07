#' Test OFI Autocorrelation
#'
#' @description
#' Tests for serial correlation in order-flow imbalance using
#' Ljung-Box test and computes autocorrelation function (ACF).
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics
#' @param metric Which OFI metric to test (default: "ofi")
#' @param max_lag Maximum lag for ACF computation (default: 20)
#' @param test_lag Lag for Ljung-Box test (default: 10)
#'
#' @return A list with:
#'   - acf_values: autocorrelation coefficients
#'   - ljung_box: Ljung-Box test results
#'   - significant_lags: which lags show significant correlation
#'
#' @details
#' Serial correlation in OFI suggests:
#' - Positive correlation: persistent order flow pressure
#' - Negative correlation: mean-reverting patterns
#' - No correlation: random/efficient order flow
#'
#' The Ljung-Box test tests H0: no autocorrelation up to lag k.
#' Low p-values (< 0.05) reject H0, indicating significant autocorrelation.
#'
#' @export
#' @importFrom stats acf Box.test
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123, imb = 0.3)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Test for autocorrelation
#' acf_test <- test_ofi_autocorrelation(ofi)
#' print(acf_test$ljung_box)
#'
#' # Plot ACF
#' plot(acf_test$acf_values, type = "h", main = "OFI Autocorrelation")
#' abline(h = c(-1.96/sqrt(nrow(ofi)), 1.96/sqrt(nrow(ofi))),
#'        lty = 2, col = "blue")
test_ofi_autocorrelation <- function(ofi_tbl,
                                     metric = "ofi",
                                     max_lag = 20,
                                     test_lag = 10) {

  # Validate inputs
  if (!is.data.frame(ofi_tbl)) {
    stop("ofi_tbl must be a data frame")
  }

  if (!metric %in% names(ofi_tbl)) {
    stop("Metric '", metric, "' not found in ofi_tbl")
  }

  # Get values and remove NAs
  values <- ofi_tbl[[metric]]
  values <- values[!is.na(values)]

  if (length(values) < max_lag + 1) {
    stop("Insufficient data points. Need at least ", max_lag + 1)
  }

  # Compute ACF
  acf_result <- acf(values, lag.max = max_lag, plot = FALSE)
  acf_values <- as.numeric(acf_result$acf)[-1]  # Remove lag 0

  # Ljung-Box test
  lb_test <- Box.test(values, lag = test_lag, type = "Ljung-Box")

  # Identify significant lags (using 95% confidence bands)
  conf_level <- 1.96 / sqrt(length(values))
  significant_lags <- which(abs(acf_values) > conf_level)

  result <- list(
    acf_values = acf_values,
    lags = 1:max_lag,
    ljung_box = list(
      statistic = as.numeric(lb_test$statistic),
      p_value = as.numeric(lb_test$p.value),
      df = test_lag,
      significant = lb_test$p.value < 0.05
    ),
    significant_lags = significant_lags,
    conf_level = conf_level,
    interpretation = ifelse(
      lb_test$p.value < 0.05,
      "Significant autocorrelation detected",
      "No significant autocorrelation"
    )
  )

  class(result) <- c("ofi_autocorr_test", "list")
  result
}


#' OFI Lead-Lag Analysis
#'
#' @description
#' Analyzes lead-lag relationships between OFI and price changes,
#' computing cross-correlation at various lags to identify predictive power.
#'
#' @param trades Trade-level data with timestamp, side, size, and price columns
#' @param window Window for OFI computation (default: "1 min")
#' @param max_lag Maximum lag for cross-correlation (default: 10)
#' @param price_change_method Method for computing price changes: "returns" or "diff"
#'
#' @return A list with:
#'   - ccf_values: cross-correlation coefficients
#'   - optimal_lag: lag with highest absolute correlation
#'   - max_correlation: maximum correlation value
#'   - interpretation: text interpretation of results
#'
#' @details
#' Positive lag k: OFI at time t correlates with price change at t+k (OFI leads)
#' Negative lag k: Price change at t correlates with OFI at t+k (price leads)
#'
#' Strong positive correlation at positive lags suggests OFI has predictive
#' power for future price movements (informed trading).
#'
#' @export
#' @importFrom stats ccf cor
#' @importFrom dplyr mutate lag lead
#' @importFrom rlang .data
#'
#' @examples
#' # Generate data with correlation between OFI and prices
#' trades <- simulate_orders(n = 2000, seed = 123, imb = 0.2, drift = 0.05)
#'
#' # Analyze lead-lag relationship
#' leadlag <- ofi_lead_lag_analysis(trades, window = "1 min", max_lag = 10)
#' print(leadlag$optimal_lag)
#' print(leadlag$interpretation)
#'
#' # Plot cross-correlation
#' plot(leadlag$lags, leadlag$ccf_values, type = "h",
#'      xlab = "Lag", ylab = "Cross-Correlation",
#'      main = "OFI vs Price Changes")
#' abline(h = 0, col = "gray")
ofi_lead_lag_analysis <- function(trades,
                                  window = "1 min",
                                  max_lag = 10,
                                  price_change_method = c("returns", "diff")) {

  price_change_method <- match.arg(price_change_method)

  # Validate required columns
  required <- c("timestamp", "side", "size", "price")
  if (!all(required %in% names(trades))) {
    stop("trades must contain columns: ", paste(required, collapse = ", "))
  }

  # Compute OFI
  ofi_data <- compute_ofi(trades, window = window, price_weighted = FALSE)

  # Compute price changes by window
  price_by_window <- trades |>
    mutate(window_start = lubridate::floor_date(.data$timestamp, window)) |>
    group_by(.data$window_start) |>
    summarise(
      price_start = dplyr::first(.data$price),
      price_end = dplyr::last(.data$price),
      .groups = "drop"
    )

  # Merge with OFI data
  combined <- ofi_data |>
    left_join(price_by_window, by = "window_start")

  # Calculate price changes
  combined <- combined |>
    mutate(
      price_change = if (price_change_method == "returns") {
        (.data$price_end - .data$price_start) / .data$price_start
      } else {
        .data$price_end - .data$price_start
      }
    )

  # Remove NAs
  combined <- combined |>
    filter(!is.na(.data$ofi), !is.na(.data$price_change))

  if (nrow(combined) < max_lag + 1) {
    stop("Insufficient windows for analysis. Need at least ", max_lag + 1)
  }

  # Compute cross-correlation
  ccf_result <- ccf(combined$ofi, combined$price_change,
                   lag.max = max_lag, plot = FALSE)

  ccf_values <- as.numeric(ccf_result$acf)
  lags <- ccf_result$lag[, 1, 1]

  # Find optimal lag
  max_idx <- which.max(abs(ccf_values))
  optimal_lag <- lags[max_idx]
  max_correlation <- ccf_values[max_idx]

  # Interpretation
  if (abs(max_correlation) < 0.1) {
    interp <- "Weak or no relationship between OFI and price changes"
  } else if (optimal_lag > 0) {
    interp <- sprintf("OFI leads price changes by %d periods (correlation: %.3f)",
                     optimal_lag, max_correlation)
  } else if (optimal_lag < 0) {
    interp <- sprintf("Price changes lead OFI by %d periods (correlation: %.3f)",
                     abs(optimal_lag), max_correlation)
  } else {
    interp <- sprintf("Contemporaneous relationship (correlation: %.3f)",
                     max_correlation)
  }

  result <- list(
    ccf_values = ccf_values,
    lags = lags,
    optimal_lag = optimal_lag,
    max_correlation = max_correlation,
    interpretation = interp,
    window = window,
    method = price_change_method,
    n_windows = nrow(combined)
  )

  class(result) <- c("ofi_leadlag", "list")
  result
}


#' Bootstrap Significance Test for OFI
#'
#' @description
#' Uses bootstrap resampling to test whether observed OFI values are
#' significantly different from random order flow.
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics
#' @param metric Which OFI metric to test (default: "ofi")
#' @param n_bootstrap Number of bootstrap samples (default: 1000)
#' @param statistic Statistic to test: "mean", "median", "sd", or "abs_mean"
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list with:
#'   - observed: observed statistic value
#'   - bootstrap_dist: bootstrap distribution
#'   - p_value: two-tailed p-value
#'   - conf_interval: 95% confidence interval
#'   - significant: whether result is significant at alpha = 0.05
#'
#' @details
#' The null hypothesis is that the statistic equals zero (random order flow).
#' Bootstrap samples are created by randomly permuting the side labels,
#' destroying any real information content while preserving size distribution.
#'
#' @export
#' @importFrom stats quantile
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123, imb = 0.3)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Test if mean OFI is significantly different from zero
#' boot_test <- bootstrap_ofi_significance(ofi, n_bootstrap = 1000, seed = 42)
#' print(boot_test$p_value)
#' print(boot_test$significant)
#'
#' \dontrun{
#' # Plot bootstrap distribution
#' hist(boot_test$bootstrap_dist, breaks = 50,
#'      main = "Bootstrap Distribution",
#'      xlab = "Mean OFI")
#' abline(v = boot_test$observed, col = "red", lwd = 2)
#' abline(v = boot_test$conf_interval, col = "blue", lty = 2)
#' }
bootstrap_ofi_significance <- function(ofi_tbl,
                                       metric = "ofi",
                                       n_bootstrap = 1000,
                                       statistic = c("mean", "median", "sd", "abs_mean"),
                                       seed = NULL) {

  statistic <- match.arg(statistic)

  # Validate inputs
  if (!is.data.frame(ofi_tbl)) {
    stop("ofi_tbl must be a data frame")
  }

  if (!metric %in% names(ofi_tbl)) {
    stop("Metric '", metric, "' not found in ofi_tbl")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Get values
  values <- ofi_tbl[[metric]]
  values <- values[!is.na(values)]

  # Compute observed statistic
  stat_func <- switch(statistic,
    mean = mean,
    median = median,
    sd = sd,
    abs_mean = function(x) mean(abs(x))
  )

  observed <- stat_func(values)

  # Bootstrap
  bootstrap_stats <- numeric(n_bootstrap)

  for (i in 1:n_bootstrap) {
    # Resample with replacement
    boot_sample <- sample(values, size = length(values), replace = TRUE)
    bootstrap_stats[i] <- stat_func(boot_sample)
  }

  # Compute p-value (two-tailed)
  # H0: statistic = 0
  if (statistic %in% c("sd", "abs_mean")) {
    # One-tailed for non-negative statistics
    p_value <- mean(bootstrap_stats <= 0) * 2  # Approximate
  } else {
    # Two-tailed
    p_value <- 2 * min(
      mean(bootstrap_stats >= observed),
      mean(bootstrap_stats <= observed)
    )
  }

  # Confidence interval (95%)
  conf_interval <- quantile(bootstrap_stats, probs = c(0.025, 0.975))

  result <- list(
    observed = observed,
    bootstrap_dist = bootstrap_stats,
    p_value = p_value,
    conf_interval = conf_interval,
    significant = p_value < 0.05,
    statistic = statistic,
    n_bootstrap = n_bootstrap,
    interpretation = ifelse(
      p_value < 0.05,
      sprintf("Significant %s detected (p = %.4f)", statistic, p_value),
      sprintf("No significant %s (p = %.4f)", statistic, p_value)
    )
  )

  class(result) <- c("ofi_bootstrap_test", "list")
  result
}


#' Print method for autocorrelation test results
#' @export
print.ofi_autocorr_test <- function(x, ...) {
  cat("OFI Autocorrelation Test Results\n")
  cat("=================================\n\n")

  cat("Ljung-Box Test (lag =", x$ljung_box$df, "):\n")
  cat("  Statistic:", round(x$ljung_box$statistic, 4), "\n")
  cat("  P-value:", format.pval(x$ljung_box$p_value), "\n")
  cat("  Result:", x$interpretation, "\n\n")

  if (length(x$significant_lags) > 0) {
    cat("Significant lags:", paste(x$significant_lags, collapse = ", "), "\n")
  } else {
    cat("No significant individual lags\n")
  }

  invisible(x)
}


#' Print method for lead-lag analysis results
#' @export
print.ofi_leadlag <- function(x, ...) {
  cat("OFI Lead-Lag Analysis Results\n")
  cat("==============================\n\n")

  cat("Window:", x$window, "\n")
  cat("Number of windows:", x$n_windows, "\n")
  cat("Price change method:", x$method, "\n\n")

  cat("Optimal lag:", x$optimal_lag, "\n")
  cat("Maximum correlation:", round(x$max_correlation, 4), "\n\n")

  cat("Interpretation:\n")
  cat(" ", x$interpretation, "\n")

  invisible(x)
}


#' Print method for bootstrap test results
#' @export
print.ofi_bootstrap_test <- function(x, ...) {
  cat("OFI Bootstrap Significance Test\n")
  cat("================================\n\n")

  cat("Statistic tested:", x$statistic, "\n")
  cat("Number of bootstrap samples:", x$n_bootstrap, "\n\n")

  cat("Observed value:", round(x$observed, 4), "\n")
  cat("95% CI: [", round(x$conf_interval[1], 4), ",",
      round(x$conf_interval[2], 4), "]\n")
  cat("P-value:", format.pval(x$p_value), "\n\n")

  cat("Result:", x$interpretation, "\n")

  invisible(x)
}
