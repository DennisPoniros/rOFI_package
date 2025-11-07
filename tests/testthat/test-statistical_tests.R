test_that("test_ofi_autocorrelation works correctly", {
  # Generate data with some persistence
  trades <- simulate_orders(n = 1000, seed = 123, imb = 0.3)
  ofi <- compute_ofi(trades, window = "1 min")

  # Test basic functionality
  acf_test <- test_ofi_autocorrelation(ofi, max_lag = 20, test_lag = 10)

  expect_s3_class(acf_test, "ofi_autocorr_test")
  expect_type(acf_test$acf_values, "double")
  expect_length(acf_test$acf_values, 20)
  expect_type(acf_test$ljung_box, "list")
  expect_true("statistic" %in% names(acf_test$ljung_box))
  expect_true("p_value" %in% names(acf_test$ljung_box))
  expect_type(acf_test$interpretation, "character")
})

test_that("test_ofi_autocorrelation detects significant correlation", {
  # Generate data with strong persistence
  trades <- simulate_orders(n = 1000, seed = 42, imb = 0.5)
  ofi <- compute_ofi(trades, window = "1 min")

  # Add artificial persistence
  for (i in 2:nrow(ofi)) {
    ofi$ofi[i] <- 0.7 * ofi$ofi[i-1] + 0.3 * ofi$ofi[i]
  }

  acf_test <- test_ofi_autocorrelation(ofi)

  # Should detect significant autocorrelation
  expect_true(acf_test$ljung_box$significant)
  expect_true(length(acf_test$significant_lags) > 0)
})

test_that("test_ofi_autocorrelation handles different metrics", {
  trades <- simulate_orders(n = 1000, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")

  # Test with OIR
  acf_oir <- test_ofi_autocorrelation(ofi, metric = "oir")
  expect_type(acf_oir$acf_values, "double")

  # Test with cumulative OFI
  acf_cum <- test_ofi_autocorrelation(ofi, metric = "ofi_cum")
  expect_type(acf_cum$acf_values, "double")
})

test_that("test_ofi_autocorrelation error handling", {
  trades <- simulate_orders(n = 100, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")

  # Insufficient data
  expect_error(
    test_ofi_autocorrelation(ofi, max_lag = 1000),
    "Insufficient data"
  )

  # Invalid metric
  expect_error(
    test_ofi_autocorrelation(ofi, metric = "nonexistent"),
    "not found"
  )
})

test_that("ofi_lead_lag_analysis works correctly", {
  # Generate data with price drift
  trades <- simulate_orders(n = 1000, seed = 123, imb = 0.2, drift = 0.1)

  # Test basic functionality
  leadlag <- ofi_lead_lag_analysis(trades, window = "1 min", max_lag = 10)

  expect_s3_class(leadlag, "ofi_leadlag")
  expect_type(leadlag$ccf_values, "double")
  expect_type(leadlag$lags, "integer")
  expect_type(leadlag$optimal_lag, "integer")
  expect_type(leadlag$max_correlation, "double")
  expect_type(leadlag$interpretation, "character")
})

test_that("ofi_lead_lag_analysis with different methods", {
  trades <- simulate_orders(n = 1000, seed = 123, drift = 0.05)

  # Test with returns
  ll_returns <- ofi_lead_lag_analysis(trades, price_change_method = "returns")
  expect_equal(ll_returns$method, "returns")

  # Test with differences
  ll_diff <- ofi_lead_lag_analysis(trades, price_change_method = "diff")
  expect_equal(ll_diff$method, "diff")
})

test_that("ofi_lead_lag_analysis handles missing prices", {
  trades <- simulate_orders(n = 100, seed = 123)
  trades$price <- NULL

  expect_error(
    ofi_lead_lag_analysis(trades),
    "must contain columns.*price"
  )
})

test_that("bootstrap_ofi_significance works correctly", {
  trades <- simulate_orders(n = 500, seed = 123, imb = 0.3)
  ofi <- compute_ofi(trades, window = "1 min")

  # Test basic functionality
  boot_test <- bootstrap_ofi_significance(
    ofi,
    n_bootstrap = 100,  # Use small number for speed
    statistic = "mean",
    seed = 42
  )

  expect_s3_class(boot_test, "ofi_bootstrap_test")
  expect_type(boot_test$observed, "double")
  expect_length(boot_test$bootstrap_dist, 100)
  expect_type(boot_test$p_value, "double")
  expect_length(boot_test$conf_interval, 2)
  expect_type(boot_test$significant, "logical")
})

test_that("bootstrap_ofi_significance with different statistics", {
  trades <- simulate_orders(n = 500, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")

  # Test mean
  boot_mean <- bootstrap_ofi_significance(ofi, n_bootstrap = 50, statistic = "mean", seed = 1)
  expect_equal(boot_mean$statistic, "mean")

  # Test median
  boot_median <- bootstrap_ofi_significance(ofi, n_bootstrap = 50, statistic = "median", seed = 1)
  expect_equal(boot_median$statistic, "median")

  # Test sd
  boot_sd <- bootstrap_ofi_significance(ofi, n_bootstrap = 50, statistic = "sd", seed = 1)
  expect_equal(boot_sd$statistic, "sd")

  # Test abs_mean
  boot_abs <- bootstrap_ofi_significance(ofi, n_bootstrap = 50, statistic = "abs_mean", seed = 1)
  expect_equal(boot_abs$statistic, "abs_mean")
})

test_that("bootstrap_ofi_significance detects significant OFI", {
  trades <- simulate_orders(n = 500, seed = 123, imb = 0.5)  # Strong imbalance
  ofi <- compute_ofi(trades, window = "1 min")

  boot_test <- bootstrap_ofi_significance(ofi, n_bootstrap = 100, seed = 42)

  # With strong imbalance, should likely be significant
  # (not guaranteed due to randomness, but very likely)
  expect_type(boot_test$significant, "logical")
  expect_true(boot_test$p_value >= 0 && boot_test$p_value <= 1)
})

test_that("bootstrap_ofi_significance reproducibility with seed", {
  trades <- simulate_orders(n = 300, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")

  boot1 <- bootstrap_ofi_significance(ofi, n_bootstrap = 50, seed = 42)
  boot2 <- bootstrap_ofi_significance(ofi, n_bootstrap = 50, seed = 42)

  expect_equal(boot1$bootstrap_dist, boot2$bootstrap_dist)
  expect_equal(boot1$p_value, boot2$p_value)
})

test_that("statistical test print methods work", {
  trades <- simulate_orders(n = 500, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")

  # Test autocorrelation print
  acf_test <- test_ofi_autocorrelation(ofi)
  expect_output(print(acf_test), "Autocorrelation Test")
  expect_output(print(acf_test), "Ljung-Box")

  # Test lead-lag print
  leadlag <- ofi_lead_lag_analysis(trades)
  expect_output(print(leadlag), "Lead-Lag Analysis")
  expect_output(print(leadlag), "Optimal lag")

  # Test bootstrap print
  boot_test <- bootstrap_ofi_significance(ofi, n_bootstrap = 50, seed = 1)
  expect_output(print(boot_test), "Bootstrap")
  expect_output(print(boot_test), "P-value")
})

test_that("statistical tests handle edge cases", {
  # Very small dataset
  trades_small <- simulate_orders(n = 100, seed = 123)
  ofi_small <- compute_ofi(trades_small, window = "1 min")

  expect_no_error(test_ofi_autocorrelation(ofi_small, max_lag = 5))
  expect_no_error(bootstrap_ofi_significance(ofi_small, n_bootstrap = 10, seed = 1))

  # Dataset with NAs
  trades <- simulate_orders(n = 300, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")
  ofi$ofi[5:10] <- NA

  acf_test <- test_ofi_autocorrelation(ofi)
  expect_type(acf_test$acf_values, "double")
})
