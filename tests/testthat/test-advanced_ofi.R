test_that("compute_ewma_ofi works correctly", {
  # Generate test data
  trades <- simulate_orders(n = 500, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")

  # Test basic functionality
  ofi_ewma <- compute_ewma_ofi(ofi, lambda = 0.9)

  expect_true("ofi_ewma" %in% names(ofi_ewma))
  expect_equal(nrow(ofi_ewma), nrow(ofi))
  expect_true(all(!is.na(ofi_ewma$ofi_ewma)))

  # Check EWMA properties
  expect_equal(ofi_ewma$ofi_ewma[1], ofi$ofi[1])  # First value should equal first OFI

  # Test different lambda values
  ofi_fast <- compute_ewma_ofi(ofi, lambda = 0.5)
  ofi_slow <- compute_ewma_ofi(ofi, lambda = 0.99)

  expect_true("ofi_ewma" %in% names(ofi_fast))
  expect_true("ofi_ewma" %in% names(ofi_slow))

  # Test with different metric
  ofi_oir_ewma <- compute_ewma_ofi(ofi, metric = "oir")
  expect_true("oir_ewma" %in% names(ofi_oir_ewma))

  # Test error handling
  expect_error(compute_ewma_ofi(ofi, lambda = 0), "lambda must be between")
  expect_error(compute_ewma_ofi(ofi, lambda = 1), "lambda must be between")
  expect_error(compute_ewma_ofi(ofi, metric = "nonexistent"), "not found")
})

test_that("compute_ofi_momentum works correctly", {
  # Generate test data
  trades <- simulate_orders(n = 500, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")

  # Test basic functionality
  ofi_mom <- compute_ofi_momentum(ofi, lookback = 5)

  expect_true("ofi_momentum" %in% names(ofi_mom))
  expect_equal(nrow(ofi_mom), nrow(ofi))

  # First `lookback` values should be NA
  expect_true(all(is.na(ofi_mom$ofi_momentum[1:5])))
  expect_true(!all(is.na(ofi_mom$ofi_momentum)))

  # Test different methods
  ofi_pct <- compute_ofi_momentum(ofi, method = "pct_change")
  ofi_diff <- compute_ofi_momentum(ofi, method = "difference")

  expect_true("ofi_momentum" %in% names(ofi_pct))
  expect_true("ofi_momentum" %in% names(ofi_diff))

  # Test different lookback
  ofi_mom_3 <- compute_ofi_momentum(ofi, lookback = 3)
  expect_true(all(is.na(ofi_mom_3$ofi_momentum[1:3])))

  # Test error handling
  expect_error(compute_ofi_momentum(ofi, lookback = 0), "positive integer")
  expect_error(compute_ofi_momentum(ofi, lookback = -1), "positive integer")
  expect_error(compute_ofi_momentum(ofi, metric = "nonexistent"), "not found")
})

test_that("compute_ofi_acceleration works correctly", {
  # Generate test data
  trades <- simulate_orders(n = 500, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")

  # Test basic functionality (should compute momentum first)
  expect_message(
    ofi_accel <- compute_ofi_acceleration(ofi, lookback = 5),
    "Computing momentum first"
  )

  expect_true("ofi_acceleration" %in% names(ofi_accel))
  expect_true("ofi_momentum" %in% names(ofi_accel))

  # Test when momentum already exists
  ofi_with_mom <- compute_ofi_momentum(ofi, lookback = 5)
  ofi_accel2 <- compute_ofi_acceleration(ofi_with_mom, lookback = 5)

  expect_true("ofi_acceleration" %in% names(ofi_accel2))

  # First 2*lookback values should be NA
  expect_true(sum(is.na(ofi_accel2$ofi_acceleration)) >= 10)

  # Test error handling
  expect_error(compute_ofi_acceleration(ofi, lookback = 0), "positive integer")
})

test_that("compute_vwap_ofi works correctly", {
  # Generate test data with prices
  trades <- simulate_orders(n = 500, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min", price_weighted = TRUE)

  # Test basic functionality
  ofi_vwap <- compute_vwap_ofi(ofi)

  expect_true("ofi_vwap_deviation" %in% names(ofi_vwap))
  expect_equal(nrow(ofi_vwap), nrow(ofi))

  # Test error handling - missing price-weighted OFI
  ofi_no_price <- compute_ofi(trades, window = "1 min", price_weighted = FALSE)
  expect_error(compute_vwap_ofi(ofi_no_price), "price_weighted = TRUE")
})

test_that("advanced OFI functions handle edge cases", {
  # Small dataset
  trades_small <- simulate_orders(n = 50, seed = 123)
  ofi_small <- compute_ofi(trades_small, window = "1 min")

  expect_no_error(compute_ewma_ofi(ofi_small))
  expect_no_error(compute_ofi_momentum(ofi_small, lookback = 2))

  # Dataset with NAs
  trades <- simulate_orders(n = 200, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")
  ofi$ofi[5:10] <- NA

  expect_no_error(compute_ewma_ofi(ofi))
  expect_no_error(compute_ofi_momentum(ofi))
})
