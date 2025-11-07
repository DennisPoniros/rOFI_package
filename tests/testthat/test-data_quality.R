test_that("validate_trade_data works with valid data", {
  # Generate valid data
  trades <- simulate_orders(n = 500, seed = 123)

  validation <- validate_trade_data(trades)

  expect_s3_class(validation, "trade_validation")
  expect_true(validation$valid)
  expect_type(validation$issues, "character")
  expect_type(validation$stats, "list")

  # Check stats
  expect_equal(validation$stats$n_rows, 500)
  expect_true(!is.null(validation$stats$time_range))
  expect_true(!is.null(validation$stats$size_range))
})

test_that("validate_trade_data detects missing values", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Add missing values
  trades$timestamp[1:10] <- NA
  trades$size[20:25] <- NA

  validation <- validate_trade_data(trades)

  expect_false(validation$valid)
  expect_true(any(grepl("missing timestamps", validation$issues)))
  expect_true(any(grepl("missing size", validation$issues)))
})

test_that("validate_trade_data detects invalid sizes", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Add negative sizes
  trades$size[1:10] <- -100

  validation <- validate_trade_data(trades)

  expect_false(validation$valid)
  expect_true(any(grepl("negative sizes", validation$issues)))
})

test_that("validate_trade_data detects duplicate timestamps", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Add duplicates
  trades <- rbind(trades, trades[1:50, ])

  validation <- validate_trade_data(trades)

  expect_false(validation$valid)
  expect_true(any(grepl("duplicate timestamps", validation$issues)))
  expect_equal(validation$stats$duplicates, 50)
})

test_that("validate_trade_data detects invalid side values", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Add invalid side
  trades$side[1:10] <- "X"

  validation <- validate_trade_data(trades)

  expect_false(validation$valid)
  expect_true(any(grepl("Invalid side values", validation$issues)))
})

test_that("validate_trade_data strict mode works", {
  trades <- simulate_orders(n = 500, seed = 123)
  trades$size[1] <- -100

  # Non-strict should not error
  expect_no_error(validate_trade_data(trades, strict = FALSE))

  # Strict should error
  expect_error(validate_trade_data(trades, strict = TRUE), "validation failed")
})

test_that("validate_trade_data handles price column", {
  trades <- simulate_orders(n = 500, seed = 123)

  # With valid prices
  validation <- validate_trade_data(trades, price_col = "price")
  expect_true(validation$valid)

  # With invalid prices
  trades$price[1:10] <- -50
  validation <- validate_trade_data(trades, price_col = "price")
  expect_false(validation$valid)
  expect_true(any(grepl("non-positive prices", validation$issues)))
})

test_that("detect_outliers_ofi works with IQR method", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Add some extreme outliers
  trades$size[1:5] <- 10000

  outliers <- detect_outliers_ofi(trades, method = "iqr", threshold = 3)

  expect_type(outliers, "list")
  expect_true(outliers$n_outliers >= 5)
  expect_true(1 %in% outliers$outlier_idx)
  expect_equal(outliers$method, "iqr")
})

test_that("detect_outliers_ofi works with zscore method", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Add outliers
  trades$size[1:3] <- 5000

  outliers <- detect_outliers_ofi(trades, method = "zscore", threshold = 3)

  expect_type(outliers, "list")
  expect_true(outliers$n_outliers > 0)
  expect_equal(outliers$method, "zscore")
})

test_that("detect_outliers_ofi works with MAD method", {
  trades <- simulate_orders(n = 500, seed = 123)

  outliers <- detect_outliers_ofi(trades, method = "mad")

  expect_type(outliers, "list")
  expect_equal(outliers$method, "mad")
})

test_that("detect_outliers_ofi handles edge cases", {
  # Empty data after removing NAs
  trades <- data.frame(
    timestamp = Sys.time() + 1:10,
    side = rep("B", 10),
    size = rep(NA, 10)
  )

  outliers <- detect_outliers_ofi(trades)
  expect_equal(outliers$n_outliers, 0)

  # No outliers
  trades <- simulate_orders(n = 100, seed = 123)
  outliers <- detect_outliers_ofi(trades, threshold = 10)  # Very lenient
  expect_equal(outliers$n_outliers, 0)
})

test_that("clean_trade_data removes invalid data", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Add various issues
  trades$size[1:5] <- -100        # Negative
  trades$size[10:15] <- 0         # Zero
  trades$timestamp[20:25] <- NA   # Missing
  trades <- rbind(trades, trades[1:10, ])  # Duplicates

  n_original <- nrow(trades)

  trades_clean <- clean_trade_data(trades)

  expect_true(nrow(trades_clean) < n_original)
  expect_true(all(trades_clean$size > 0))
  expect_true(!any(is.na(trades_clean$timestamp)))
  expect_true(!any(duplicated(trades_clean$timestamp)))

  # Check cleaning report
  report <- attr(trades_clean, "cleaning_report")
  expect_type(report, "list")
  expect_equal(report$n_original, n_original)
  expect_true(report$n_removed_invalid_size > 0)
  expect_true(report$n_removed_missing > 0)
  expect_true(report$n_removed_duplicates > 0)
})

test_that("clean_trade_data handles outliers", {
  trades <- simulate_orders(n = 500, seed = 123)
  trades$size[1:5] <- 10000  # Extreme outliers

  # With outlier removal
  trades_clean <- clean_trade_data(trades, remove_outliers = TRUE)
  report <- attr(trades_clean, "cleaning_report")
  expect_true(report$n_removed_outliers >= 5)

  # Without outlier removal
  trades_clean2 <- clean_trade_data(trades, remove_outliers = FALSE)
  expect_true(5 %in% which(trades_clean2$size == 10000))
})

test_that("clean_trade_data sorting works", {
  trades <- simulate_orders(n = 100, seed = 123)

  # Shuffle timestamps
  shuffled <- trades[sample(nrow(trades)), ]

  # Clean with sorting
  cleaned <- clean_trade_data(shuffled, sort = TRUE)
  expect_true(all(diff(cleaned$timestamp) >= 0))

  # Clean without sorting
  cleaned2 <- clean_trade_data(shuffled, sort = FALSE, remove_duplicates = FALSE)
  expect_false(all(diff(cleaned2$timestamp) >= 0))
})

test_that("print.trade_validation works", {
  trades <- simulate_orders(n = 100, seed = 123)
  validation <- validate_trade_data(trades)

  # Should print without error
  expect_output(print(validation), "Trade Data Validation")
  expect_output(print(validation), "PASSED")
})
