test_that("kyle_lambda_estimation works with regression method", {
  # Generate data with price impact
  set.seed(42)
  trades <- simulate_orders(n = 1000, seed = 42, imb = 0.2, drift = 0.05)

  # Estimate Kyle's lambda
  kyle <- kyle_lambda_estimation(trades, window = "1 min", method = "regression")

  expect_s3_class(kyle, "kyle_lambda")
  expect_type(kyle$lambda, "double")
  expect_type(kyle$std_error, "double")
  expect_type(kyle$r_squared, "double")
  expect_equal(kyle$method, "regression")
  expect_type(kyle$interpretation, "character")

  # Lambda should be a reasonable number
  expect_true(!is.na(kyle$lambda))
  expect_true(!is.infinite(kyle$lambda))
})

test_that("kyle_lambda_estimation works with different methods", {
  trades <- simulate_orders(n = 1000, seed = 123, drift = 0.02)

  # Regression method
  kyle_reg <- kyle_lambda_estimation(trades, method = "regression")
  expect_equal(kyle_reg$method, "regression")

  # Hasbrouck method
  kyle_has <- kyle_lambda_estimation(trades, method = "hasbrouck")
  expect_equal(kyle_has$method, "hasbrouck")

  # Amihud method
  kyle_ami <- kyle_lambda_estimation(trades, method = "amihud")
  expect_equal(kyle_ami$method, "amihud")

  # All should return valid lambda
  expect_type(kyle_reg$lambda, "double")
  expect_type(kyle_has$lambda, "double")
  expect_type(kyle_ami$lambda, "double")
})

test_that("kyle_lambda_estimation handles missing columns", {
  trades <- simulate_orders(n = 100, seed = 123)
  trades$price <- NULL

  expect_error(
    kyle_lambda_estimation(trades),
    "must contain columns"
  )
})

test_that("kyle_lambda_estimation handles insufficient data", {
  trades <- simulate_orders(n = 20, seed = 123)  # Too few for good windows

  expect_error(
    kyle_lambda_estimation(trades, window = "1 min"),
    "Insufficient data"
  )
})

test_that("estimate_price_impact works correctly", {
  trades <- simulate_orders(n = 1500, seed = 123, drift = 0.05)

  impact <- estimate_price_impact(trades, window = "1 min", decay_periods = 5)

  expect_s3_class(impact, "price_impact")
  expect_type(impact$temporary_impact, "double")
  expect_type(impact$permanent_impact, "double")
  expect_type(impact$immediate_impact, "double")
  expect_type(impact$decay_profile, "double")
  expect_length(impact$decay_profile, 5)

  # Check relationships
  # Immediate impact should be sum of temporary and permanent
  expect_equal(
    impact$immediate_impact,
    impact$temporary_impact + impact$permanent_impact,
    tolerance = 1e-10
  )
})

test_that("estimate_price_impact validates inputs", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Invalid decay_periods
  expect_error(
    estimate_price_impact(trades, decay_periods = 0),
    "between 1 and 20"
  )

  expect_error(
    estimate_price_impact(trades, decay_periods = 25),
    "between 1 and 20"
  )
})

test_that("estimate_price_impact handles insufficient data", {
  trades <- simulate_orders(n = 50, seed = 123)

  expect_error(
    estimate_price_impact(trades, decay_periods = 10),
    "Insufficient data"
  )
})

test_that("compute_vpin works correctly", {
  # Generate high-frequency data
  trades <- simulate_orders(n = 2000, lambda = 50, seed = 123, imb = 0.3)

  vpin_data <- compute_vpin(trades, n_buckets = 50, lookback = 20)

  expect_s3_class(vpin_data, "tbl_df")
  expect_true("vpin" %in% names(vpin_data))
  expect_true("bucket_id" %in% names(vpin_data))
  expect_true("buy_volume" %in% names(vpin_data))
  expect_true("sell_volume" %in% names(vpin_data))

  # VPIN should be between 0 and 1
  vpin_values <- vpin_data$vpin[!is.na(vpin_data$vpin)]
  expect_true(all(vpin_values >= 0))
  expect_true(all(vpin_values <= 1))

  # Early values should be NA (before lookback)
  expect_true(all(is.na(vpin_data$vpin[1:19])))
})

test_that("compute_vpin with custom bucket_size", {
  trades <- simulate_orders(n = 1000, seed = 123)

  total_volume <- sum(trades$size)
  custom_bucket_size <- total_volume / 30

  vpin_data <- compute_vpin(trades, n_buckets = 30,
                             bucket_size = custom_bucket_size, lookback = 10)

  expect_true(nrow(vpin_data) > 0)
  expect_true("vpin" %in% names(vpin_data))
})

test_that("compute_vpin validates inputs", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Too few buckets
  expect_error(
    compute_vpin(trades, n_buckets = 1),
    "n_buckets must be at least 2"
  )

  # Invalid lookback
  expect_error(
    compute_vpin(trades, n_buckets = 50, lookback = 60),
    "between 1 and n_buckets"
  )
})

test_that("compute_vpin handles imbalanced flow", {
  # Generate highly imbalanced data
  trades <- simulate_orders(n = 1000, seed = 42, imb = 0.8)  # 80% buy bias

  vpin_data <- compute_vpin(trades, n_buckets = 30, lookback = 10)

  # High imbalance should result in high VPIN
  mean_vpin <- mean(vpin_data$vpin, na.rm = TRUE)
  expect_true(mean_vpin > 0.5)  # Should be elevated
})

test_that("compute_effective_spread works with rolling method", {
  trades <- simulate_orders(n = 500, seed = 123)

  spreads <- compute_effective_spread(trades, midpoint_method = "rolling")

  expect_s3_class(spreads, "tbl_df")
  expect_true("effective_spread" %in% names(spreads))
  expect_true("signed_spread" %in% names(spreads))
  expect_true("relative_spread" %in% names(spreads))
  expect_true("midpoint" %in% names(spreads))

  # Effective spread should be non-negative
  expect_true(all(spreads$effective_spread >= 0))

  # Relative spread should be reasonable percentages
  expect_true(all(spreads$relative_spread >= 0))
})

test_that("compute_effective_spread works with different methods", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Rolling method
  spreads_roll <- compute_effective_spread(trades, midpoint_method = "rolling")
  expect_true(nrow(spreads_roll) > 0)

  # Same second method
  spreads_sec <- compute_effective_spread(trades, midpoint_method = "same_second")
  expect_true(nrow(spreads_sec) > 0)

  # VWAP method
  spreads_vwap <- compute_effective_spread(trades, midpoint_method = "vwap")
  expect_true(nrow(spreads_vwap) > 0)
})

test_that("compute_effective_spread validates VWAP method", {
  trades <- simulate_orders(n = 100, seed = 123)
  trades$size <- NULL

  expect_error(
    compute_effective_spread(trades, midpoint_method = "vwap"),
    "VWAP method requires 'size' column"
  )
})

test_that("decompose_spread works correctly", {
  trades <- simulate_orders(n = 1500, seed = 123, drift = 0.03)

  decomp <- decompose_spread(trades, horizon = 5)

  expect_s3_class(decomp, "spread_decomposition")
  expect_type(decomp$adverse_selection, "double")
  expect_type(decomp$realized_spread, "double")
  expect_type(decomp$effective_spread, "double")
  expect_type(decomp$pct_adverse_selection, "double")
  expect_equal(decomp$horizon, 5)

  # Decomposition should sum correctly (approximately)
  # effective = realized + adverse
  total_computed <- decomp$realized_spread + decomp$adverse_selection
  expect_equal(total_computed, decomp$effective_spread, tolerance = 0.01)
})

test_that("decompose_spread validates inputs", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Invalid horizon
  expect_error(
    decompose_spread(trades, horizon = 0),
    "between 1 and 100"
  )

  expect_error(
    decompose_spread(trades, horizon = 150),
    "between 1 and 100"
  )
})

test_that("decompose_spread handles insufficient data", {
  trades <- simulate_orders(n = 30, seed = 123)

  expect_error(
    decompose_spread(trades, horizon = 10),
    "Insufficient data"
  )
})

test_that("print methods work for microstructure objects", {
  trades <- simulate_orders(n = 1000, seed = 123, drift = 0.02)

  # Kyle's lambda
  kyle <- kyle_lambda_estimation(trades)
  expect_output(print(kyle), "Kyle's Lambda")
  expect_output(print(kyle), "Lambda:")

  # Price impact
  impact <- estimate_price_impact(trades, decay_periods = 3)
  expect_output(print(impact), "Price Impact Decomposition")
  expect_output(print(impact), "Temporary impact:")

  # Spread decomposition
  decomp <- decompose_spread(trades, horizon = 5)
  expect_output(print(decomp), "Spread Decomposition")
  expect_output(print(decomp), "Adverse Selection:")
})

test_that("microstructure functions handle edge cases", {
  # Very small dataset
  trades_small <- simulate_orders(n = 100, seed = 123)

  # Should handle gracefully or error informatively
  expect_error(
    kyle_lambda_estimation(trades_small, window = "10 sec"),
    "Insufficient data|Need at least"
  )

  # Balanced order flow (no imbalance)
  trades_balanced <- simulate_orders(n = 500, seed = 42, imb = 0)
  vpin_balanced <- compute_vpin(trades_balanced, n_buckets = 20, lookback = 10)

  # VPIN should be low for balanced flow
  mean_vpin_balanced <- mean(vpin_balanced$vpin, na.rm = TRUE)
  expect_true(mean_vpin_balanced < 0.3)
})

test_that("microstructure functions handle NA values", {
  trades <- simulate_orders(n = 500, seed = 123)

  # Introduce some NAs
  trades$price[5:10] <- NA

  # Functions should handle NAs gracefully
  spreads <- compute_effective_spread(trades)
  # Should still produce results for valid data
  expect_true(nrow(spreads) > 0)
  expect_true(sum(!is.na(spreads$effective_spread)) > 0)
})
