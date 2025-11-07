#' Validate Trade Data Quality
#'
#' @description
#' Performs comprehensive validation checks on trade-level data before
#' OFI computation. Identifies common data quality issues.
#'
#' @param data A data frame containing trade-level events
#' @param time_col Name of the timestamp column (default: "timestamp")
#' @param side_col Name of the side column (default: "side")
#' @param size_col Name of the size column (default: "size")
#' @param price_col Name of the price column (optional, default: NULL)
#' @param strict Logical, whether to stop on warnings (default: FALSE)
#'
#' @return A list with validation results:
#'   - valid: logical, whether data passed all checks
#'   - issues: character vector of identified issues
#'   - stats: summary statistics
#'
#' @details
#' Validation checks include:
#' - Column existence
#' - Missing values
#' - Duplicate timestamps
#' - Non-chronological ordering
#' - Invalid side values
#' - Negative or zero sizes/prices
#' - Extreme outliers
#' - Timezone consistency
#'
#' @export
#' @importFrom dplyr arrange group_by summarise n
#' @importFrom lubridate is.POSIXct
#' @importFrom rlang .data sym
#'
#' @examples
#' # Valid data
#' trades <- simulate_orders(n = 1000, seed = 123)
#' validation <- validate_trade_data(trades)
#' print(validation)
#'
#' # Data with issues
#' bad_trades <- trades
#' bad_trades$size[1:10] <- -100  # Negative sizes
#' bad_trades$timestamp[50:60] <- NA  # Missing timestamps
#' validation <- validate_trade_data(bad_trades)
#' print(validation$issues)
validate_trade_data <- function(data,
                                time_col = "timestamp",
                                side_col = "side",
                                size_col = "size",
                                price_col = NULL,
                                strict = FALSE) {

  issues <- character(0)
  stats <- list()

  # Check 1: Column existence
  required_cols <- c(time_col, side_col, size_col)
  if (!is.null(price_col)) {
    required_cols <- c(required_cols, price_col)
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    return(list(valid = FALSE, issues = issues, stats = stats))
  }

  # Check 2: Data dimensions
  stats$n_rows <- nrow(data)
  stats$n_cols <- ncol(data)

  if (nrow(data) == 0) {
    issues <- c(issues, "Data frame is empty")
    return(list(valid = FALSE, issues = issues, stats = stats))
  }

  # Check 3: Missing values
  na_timestamp <- sum(is.na(data[[time_col]]))
  na_side <- sum(is.na(data[[side_col]]))
  na_size <- sum(is.na(data[[size_col]]))

  if (na_timestamp > 0) {
    issues <- c(issues, sprintf("%d missing timestamps (%.1f%%)",
                               na_timestamp, 100 * na_timestamp / nrow(data)))
  }
  if (na_side > 0) {
    issues <- c(issues, sprintf("%d missing side values (%.1f%%)",
                               na_side, 100 * na_side / nrow(data)))
  }
  if (na_size > 0) {
    issues <- c(issues, sprintf("%d missing size values (%.1f%%)",
                               na_size, 100 * na_size / nrow(data)))
  }

  if (!is.null(price_col)) {
    na_price <- sum(is.na(data[[price_col]]))
    if (na_price > 0) {
      issues <- c(issues, sprintf("%d missing price values (%.1f%%)",
                                 na_price, 100 * na_price / nrow(data)))
    }
  }

  # Check 4: Timestamp validity
  if (!lubridate::is.POSIXct(data[[time_col]])) {
    issues <- c(issues, "Timestamp column is not POSIXct format")
  } else {
    # Check chronological order
    if (any(diff(data[[time_col]]) < 0, na.rm = TRUE)) {
      issues <- c(issues, "Timestamps are not in chronological order")
    }

    # Check for timezone
    tz <- attr(data[[time_col]], "tzone")
    if (is.null(tz) || tz == "") {
      issues <- c(issues, "Timestamps lack timezone information")
    }
    stats$timezone <- tz %||% "unspecified"

    # Time range
    stats$time_range <- range(data[[time_col]], na.rm = TRUE)
    stats$duration <- difftime(stats$time_range[2], stats$time_range[1], units = "hours")
  }

  # Check 5: Duplicate timestamps
  dup_count <- sum(duplicated(data[[time_col]]))
  if (dup_count > 0) {
    issues <- c(issues, sprintf("%d duplicate timestamps (%.1f%%)",
                               dup_count, 100 * dup_count / nrow(data)))
  }
  stats$duplicates <- dup_count

  # Check 6: Side values
  unique_sides <- unique(data[[side_col]])
  valid_sides <- c("B", "S", "BUY", "SELL", "buy", "sell", 1, -1)

  if (is.numeric(data[[side_col]])) {
    invalid_sides <- unique_sides[!unique_sides %in% c(1, -1, NA)]
  } else {
    invalid_sides <- unique_sides[!toupper(as.character(unique_sides)) %in%
                                  c("B", "S", "BUY", "SELL", NA)]
  }

  if (length(invalid_sides) > 0) {
    issues <- c(issues, paste("Invalid side values:",
                             paste(invalid_sides, collapse = ", ")))
  }

  # Side distribution
  if (is.character(data[[side_col]]) || is.factor(data[[side_col]])) {
    side_counts <- table(data[[side_col]])
    stats$side_distribution <- side_counts
    buy_pct <- 100 * sum(toupper(as.character(data[[side_col]])) %in% c("B", "BUY"), na.rm = TRUE) / nrow(data)
    stats$buy_percentage <- buy_pct

    # Check for extreme imbalance
    if (buy_pct < 10 || buy_pct > 90) {
      issues <- c(issues, sprintf("Extreme side imbalance: %.1f%% buys", buy_pct))
    }
  }

  # Check 7: Size values
  sizes <- data[[size_col]]
  negative_sizes <- sum(sizes < 0, na.rm = TRUE)
  zero_sizes <- sum(sizes == 0, na.rm = TRUE)

  if (negative_sizes > 0) {
    issues <- c(issues, sprintf("%d negative sizes", negative_sizes))
  }
  if (zero_sizes > 0) {
    issues <- c(issues, sprintf("%d zero sizes", zero_sizes))
  }

  stats$size_range <- range(sizes, na.rm = TRUE)
  stats$size_mean <- mean(sizes, na.rm = TRUE)
  stats$size_median <- median(sizes, na.rm = TRUE)

  # Check for outliers using IQR method
  q1 <- quantile(sizes, 0.25, na.rm = TRUE)
  q3 <- quantile(sizes, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  outliers_size <- sum(sizes < (q1 - 3 * iqr) | sizes > (q3 + 3 * iqr), na.rm = TRUE)

  if (outliers_size > 0) {
    issues <- c(issues, sprintf("%d size outliers (>3 IQR, %.1f%%)",
                               outliers_size, 100 * outliers_size / nrow(data)))
  }
  stats$size_outliers <- outliers_size

  # Check 8: Price values (if provided)
  if (!is.null(price_col)) {
    prices <- data[[price_col]]
    negative_prices <- sum(prices <= 0, na.rm = TRUE)

    if (negative_prices > 0) {
      issues <- c(issues, sprintf("%d non-positive prices", negative_prices))
    }

    stats$price_range <- range(prices, na.rm = TRUE)
    stats$price_mean <- mean(prices, na.rm = TRUE)

    # Check for outliers
    q1_p <- quantile(prices, 0.25, na.rm = TRUE)
    q3_p <- quantile(prices, 0.75, na.rm = TRUE)
    iqr_p <- q3_p - q1_p
    outliers_price <- sum(prices < (q1_p - 3 * iqr_p) | prices > (q3_p + 3 * iqr_p), na.rm = TRUE)

    if (outliers_price > 0) {
      issues <- c(issues, sprintf("%d price outliers (>3 IQR, %.1f%%)",
                                 outliers_price, 100 * outliers_price / nrow(data)))
    }
  }

  # Final assessment
  valid <- length(issues) == 0

  if (strict && !valid) {
    stop("Data validation failed:\n  ", paste(issues, collapse = "\n  "))
  }

  result <- list(
    valid = valid,
    issues = if (length(issues) == 0) "No issues detected" else issues,
    stats = stats
  )

  class(result) <- c("trade_validation", "list")
  result
}


#' Detect Outliers in Trade Data
#'
#' @description
#' Identifies outliers in trade-level data using multiple statistical methods.
#' Returns flagged observations and summary statistics.
#'
#' @param data A data frame containing trade-level events
#' @param size_col Name of the size column (default: "size")
#' @param price_col Name of the price column (optional, default: NULL)
#' @param method Detection method: "iqr", "zscore", "mad", or "isolation" (default: "iqr")
#' @param threshold Threshold for outlier detection (method-specific)
#'   - IQR: multiplier for IQR (default: 3)
#'   - Z-score: number of standard deviations (default: 3)
#'   - MAD: number of median absolute deviations (default: 3)
#'
#' @return A list with:
#'   - outlier_idx: indices of outlier observations
#'   - n_outliers: number of outliers detected
#'   - pct_outliers: percentage of outliers
#'   - method: detection method used
#'
#' @export
#' @importFrom stats quantile median mad sd
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123)
#'
#' # Detect size outliers using IQR method
#' outliers <- detect_outliers_ofi(trades, method = "iqr")
#' print(paste(outliers$n_outliers, "outliers detected"))
#'
#' # More sensitive detection with Z-score
#' outliers_z <- detect_outliers_ofi(trades, method = "zscore", threshold = 2.5)
detect_outliers_ofi <- function(data,
                                size_col = "size",
                                price_col = NULL,
                                method = c("iqr", "zscore", "mad"),
                                threshold = 3) {

  method <- match.arg(method)

  if (!size_col %in% names(data)) {
    stop("Column '", size_col, "' not found in data")
  }

  values <- data[[size_col]]
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(list(
      outlier_idx = integer(0),
      n_outliers = 0,
      pct_outliers = 0,
      method = method
    ))
  }

  # Detect outliers based on method
  outlier_flags <- switch(method,
    iqr = {
      q1 <- quantile(values, 0.25)
      q3 <- quantile(values, 0.75)
      iqr <- q3 - q1
      lower <- q1 - threshold * iqr
      upper <- q3 + threshold * iqr
      data[[size_col]] < lower | data[[size_col]] > upper
    },
    zscore = {
      z_scores <- abs((values - mean(values)) / sd(values))
      z_scores > threshold
    },
    mad = {
      med <- median(values)
      mad_val <- mad(values)
      abs(values - med) / mad_val > threshold
    }
  )

  # Handle NAs
  outlier_flags[is.na(outlier_flags)] <- FALSE

  outlier_idx <- which(outlier_flags)

  list(
    outlier_idx = outlier_idx,
    n_outliers = length(outlier_idx),
    pct_outliers = 100 * length(outlier_idx) / nrow(data),
    method = method,
    threshold = threshold
  )
}


#' Clean Trade Data
#'
#' @description
#' Comprehensive cleaning of trade-level data, handling common issues
#' like duplicates, outliers, and invalid values.
#'
#' @param data A data frame containing trade-level events
#' @param time_col Name of the timestamp column (default: "timestamp")
#' @param side_col Name of the side column (default: "side")
#' @param size_col Name of the size column (default: "size")
#' @param price_col Name of the price column (optional, default: NULL)
#' @param remove_outliers Logical, whether to remove outliers (default: TRUE)
#' @param outlier_method Method for outlier detection (default: "iqr")
#' @param remove_duplicates Logical, whether to remove duplicate timestamps (default: TRUE)
#' @param sort Logical, whether to sort by timestamp (default: TRUE)
#'
#' @return A cleaned data frame with added attribute "cleaning_report"
#'
#' @export
#' @importFrom dplyr filter arrange distinct
#' @importFrom rlang .data sym
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123)
#'
#' # Add some artificial issues
#' trades$size[1:5] <- -100  # Negative sizes
#' trades <- rbind(trades, trades[1:10, ])  # Duplicates
#'
#' # Clean the data
#' trades_clean <- clean_trade_data(trades)
#'
#' # Check cleaning report
#' attr(trades_clean, "cleaning_report")
clean_trade_data <- function(data,
                             time_col = "timestamp",
                             side_col = "side",
                             size_col = "size",
                             price_col = NULL,
                             remove_outliers = TRUE,
                             outlier_method = "iqr",
                             remove_duplicates = TRUE,
                             sort = TRUE) {

  report <- list()
  n_original <- nrow(data)
  report$n_original <- n_original

  # Step 1: Remove rows with missing critical values
  n_before <- nrow(data)
  data <- data |>
    filter(!is.na(!!sym(time_col)),
           !is.na(!!sym(side_col)),
           !is.na(!!sym(size_col)))
  n_after <- nrow(data)
  report$n_removed_missing <- n_before - n_after

  # Step 2: Remove negative or zero sizes
  n_before <- nrow(data)
  data <- data |>
    filter(!!sym(size_col) > 0)
  n_after <- nrow(data)
  report$n_removed_invalid_size <- n_before - n_after

  # Step 3: Remove negative prices (if price column provided)
  if (!is.null(price_col) && price_col %in% names(data)) {
    n_before <- nrow(data)
    data <- data |>
      filter(!!sym(price_col) > 0)
    n_after <- nrow(data)
    report$n_removed_invalid_price <- n_before - n_after
  }

  # Step 4: Remove duplicates
  if (remove_duplicates) {
    n_before <- nrow(data)
    data <- data |>
      distinct(!!sym(time_col), .keep_all = TRUE)
    n_after <- nrow(data)
    report$n_removed_duplicates <- n_before - n_after
  }

  # Step 5: Sort by timestamp
  if (sort) {
    data <- data |>
      arrange(!!sym(time_col))
    report$sorted <- TRUE
  }

  # Step 6: Remove outliers
  if (remove_outliers) {
    outlier_result <- detect_outliers_ofi(data, size_col = size_col, method = outlier_method)
    if (outlier_result$n_outliers > 0) {
      data <- data[-outlier_result$outlier_idx, ]
      report$n_removed_outliers <- outlier_result$n_outliers
      report$outlier_method <- outlier_method
    } else {
      report$n_removed_outliers <- 0
    }
  }

  # Final stats
  report$n_final <- nrow(data)
  report$pct_retained <- 100 * nrow(data) / n_original

  # Attach report
  attr(data, "cleaning_report") <- report

  data
}


#' Print method for trade validation results
#' @export
print.trade_validation <- function(x, ...) {
  cat("Trade Data Validation Results\n")
  cat("==============================\n\n")

  cat("Status:", if (x$valid) "PASSED" else "FAILED", "\n\n")

  if (!x$valid && is.character(x$issues)) {
    cat("Issues detected:\n")
    for (issue in x$issues) {
      cat("  -", issue, "\n")
    }
    cat("\n")
  } else if (x$valid) {
    cat("No issues detected\n\n")
  }

  if (length(x$stats) > 0) {
    cat("Summary Statistics:\n")
    cat("  Rows:", x$stats$n_rows, "\n")
    if (!is.null(x$stats$time_range)) {
      cat("  Time range:", format(x$stats$time_range[1]), "to",
          format(x$stats$time_range[2]), "\n")
      cat("  Duration:", round(as.numeric(x$stats$duration), 2), "hours\n")
    }
    if (!is.null(x$stats$size_mean)) {
      cat("  Size: mean =", round(x$stats$size_mean, 2),
          ", median =", round(x$stats$size_median, 2), "\n")
    }
    if (!is.null(x$stats$buy_percentage)) {
      cat("  Buy percentage:", round(x$stats$buy_percentage, 1), "%\n")
    }
  }

  invisible(x)
}


#' NULL default operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
