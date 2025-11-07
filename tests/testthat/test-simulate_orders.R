test_that("simulate_orders generates correct structure", {
  trades <- simulate_orders(n = 100, seed = 123)
  
  expect_s3_class(trades, "tbl_df")
  expect_equal(nrow(trades), 100)
  expect_equal(names(trades), c("timestamp", "side", "size", "price"))
  expect_true(lubridate::is.POSIXct(trades$timestamp))
  expect_true(all(trades$side %in% c("B", "S")))
  expect_true(all(trades$size > 0))
  expect_true(all(trades$price > 0))
})

test_that("simulate_orders respects seed for reproducibility", {
  trades1 <- simulate_orders(n = 50, seed = 42)
  trades2 <- simulate_orders(n = 50, seed = 42)
  
  expect_identical(trades1, trades2)
})

test_that("simulate_orders handles imbalance parameter", {
  # Strong buy bias
  buy_trades <- simulate_orders(n = 1000, imb = 0.8, seed = 123)
  n_buys <- sum(buy_trades$side == "B")
  n_sells <- sum(buy_trades$side == "S")
  
  expect_true(n_buys > n_sells)
  
  # Strong sell bias
  sell_trades <- simulate_orders(n = 1000, imb = -0.8, seed = 456)
  n_buys <- sum(sell_trades$side == "B")
  n_sells <- sum(sell_trades$side == "S")
  
  expect_true(n_sells > n_buys)
  
  # Balanced
  balanced_trades <- simulate_orders(n = 1000, imb = 0, seed = 789)
  n_buys <- sum(balanced_trades$side == "B")
  n_sells <- sum(balanced_trades$side == "S")
  
  # Should be roughly equal (with some randomness)
  expect_true(abs(n_buys - n_sells) < 100)
})

test_that("simulate_orders handles arrival rate (lambda)", {
  # High frequency
  hf_trades <- simulate_orders(n = 100, lambda = 60, seed = 123)
  duration_minutes <- as.numeric(diff(range(hf_trades$timestamp)), units = "mins")
  
  # Low frequency  
  lf_trades <- simulate_orders(n = 100, lambda = 1, seed = 123)
  duration_minutes_lf <- as.numeric(diff(range(lf_trades$timestamp)), units = "mins")
  
  # High frequency should have shorter duration
  expect_true(duration_minutes < duration_minutes_lf)
})

test_that("simulate_orders handles price dynamics", {
  # With positive drift
  up_trades <- simulate_orders(n = 500, drift = 1, vol = 0.1, seed = 123)
  price_change <- up_trades$price[500] - up_trades$price[1]
  expect_true(price_change > 0)  # Should trend upward
  
  # With negative drift
  down_trades <- simulate_orders(n = 500, drift = -1, vol = 0.1, seed = 123)
  price_change <- down_trades$price[500] - down_trades$price[1]
  expect_true(price_change < 0)  # Should trend downward
  
  # Zero volatility should have minimal price movement
  stable_trades <- simulate_orders(n = 100, drift = 0, vol = 0, seed = 123)
  price_range <- diff(range(stable_trades$price))
  expect_true(price_range < 1)  # Very small changes due to rounding
})

test_that("simulate_orders handles timezone", {
  trades_utc <- simulate_orders(n = 10, tz = "UTC", seed = 123)
  expect_equal(attr(trades_utc$timestamp, "tzone"), "UTC")
  
  trades_ny <- simulate_orders(n = 10, tz = "America/New_York", seed = 123)
  expect_equal(attr(trades_ny$timestamp, "tzone"), "America/New_York")
})

test_that("simulate_orders validates inputs", {
  expect_error(simulate_orders(n = -1), "n must be positive")
  expect_error(simulate_orders(n = 10, lambda = -1), "lambda must be positive")
  expect_error(simulate_orders(n = 10, imb = 2), "imb must be between -1 and 1")
  expect_error(simulate_orders(n = 10, price0 = -100), "price0 must be positive")
  expect_error(simulate_orders(n = 10, vol = -1), "vol must be non-negative")
})

test_that("simulate_orders timestamps are monotonic", {
  trades <- simulate_orders(n = 100, seed = 123)
  
  # Timestamps should be strictly increasing
  expect_true(all(diff(trades$timestamp) > 0))
})

test_that("generate_demo_data works", {
  demo <- generate_demo_data(n = 100)
  
  expect_s3_class(demo, "tbl_df")
  expect_equal(nrow(demo), 100)
  expect_equal(names(demo), c("timestamp", "side", "size", "price"))
  
  # Should always generate the same data (uses fixed seed)
  demo2 <- generate_demo_data(n = 100)
  expect_identical(demo, demo2)
})
