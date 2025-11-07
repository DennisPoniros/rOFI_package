test_that("compute_ofi handles calendar windows", {
  # Create simple test data
  trades <- tibble::tibble(
    timestamp = as.POSIXct(c(
      "2024-01-01 10:00:10",
      "2024-01-01 10:00:30", 
      "2024-01-01 10:01:10",
      "2024-01-01 10:01:30",
      "2024-01-01 10:02:10"
    ), tz = "UTC"),
    side = c("B", "B", "S", "B", "S"),
    size = c(100, 200, 150, 300, 250)
  )
  
  result <- compute_ofi(trades, window = "1 min")
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)  # 3 minutes of data
  expect_true(all(c("window_start", "ofi", "oir", "ofi_cum", "B", "S", "vol_total", "n_trades") %in% names(result)))
  
  # Check first window calculation
  expect_equal(result$B[1], 300)  # 100 + 200
  expect_equal(result$S[1], 0)
  expect_equal(result$ofi[1], 300)
  expect_equal(result$n_trades[1], 2)
})

test_that("compute_ofi calculates metrics correctly", {
  trades <- tibble::tibble(
    timestamp = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:00:01",
      "2024-01-01 10:00:02",
      "2024-01-01 10:00:03"
    ), tz = "UTC"),
    side = c("B", "B", "S", "S"),
    size = c(100, 150, 200, 50)
  )
  
  result <- compute_ofi(trades, window = "1 min", eps = 0.001)
  
  expect_equal(result$B[1], 250)     # 100 + 150
  expect_equal(result$S[1], 250)     # 200 + 50
  expect_equal(result$ofi[1], 0)     # 250 - 250
  expect_equal(result$vol_total[1], 500)  # 250 + 250
  
  # OIR calculation: (B - S) / (B + S + eps)
  expected_oir <- (250 - 250) / (250 + 250 + 0.001)
  expect_equal(result$oir[1], expected_oir)
})

test_that("compute_ofi handles price-weighted OFI", {
  trades <- tibble::tibble(
    timestamp = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:00:01",
      "2024-01-01 10:00:02"
    ), tz = "UTC"),
    side = c("B", "S", "B"),
    size = c(100, 200, 150),
    price = c(10, 11, 12)
  )
  
  result <- compute_ofi(trades, window = "1 min", price_weighted = TRUE)
  
  expect_true("ofi_dollar" %in% names(result))
  # Dollar OFI: (100*10*1) + (200*11*-1) + (150*12*1) = 1000 - 2200 + 1800 = 600
  expect_equal(result$ofi_dollar[1], 600)
})

test_that("compute_ofi handles tick-based windows", {
  trades <- simulate_orders(n = 100, seed = 123)
  
  result <- compute_ofi(trades, n_ticks = 20)
  
  expect_equal(nrow(result), 5)  # 100 trades / 20 per window = 5 windows
  expect_true(all(result$n_trades == 20))
})

test_that("compute_ofi handles rolling windows", {
  trades <- simulate_orders(n = 50, lambda = 30, seed = 123)
  
  result <- compute_ofi(trades, rolling = lubridate::dseconds(10))
  
  # Should have as many windows as trades (one starting at each trade)
  expect_true(nrow(result) > 0)
  expect_true(nrow(result) <= nrow(trades))
})

test_that("compute_ofi cumulative OFI is calculated correctly", {
  trades <- tibble::tibble(
    timestamp = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:01:00",
      "2024-01-01 10:02:00"
    ), tz = "UTC"),
    side = c("B", "B", "S"),
    size = c(100, 200, 150)
  )
  
  result <- compute_ofi(trades, window = "1 min")
  
  expect_equal(result$ofi_cum[1], result$ofi[1])
  expect_equal(result$ofi_cum[2], result$ofi[1] + result$ofi[2])
  expect_equal(result$ofi_cum[3], sum(result$ofi[1:3]))
})

test_that("compute_ofi validates input", {
  bad_data <- data.frame(time = Sys.time() + 1:5, volume = 100:104)
  expect_error(compute_ofi(bad_data), "Data must have columns: timestamp, side, size")
  
  trades <- simulate_orders(n = 10, seed = 123)
  trades$price <- NULL
  expect_error(
    compute_ofi(trades, price_weighted = TRUE),
    "price_weighted = TRUE requires a 'price' column"
  )
})

test_that("compute_ofi rejects multiple window parameters", {
  trades <- simulate_orders(n = 10, seed = 123)
  
  expect_error(
    compute_ofi(trades, window = "1 min", rolling = lubridate::dseconds(60)),
    "Only one of 'window', 'rolling', or 'n_ticks' may be specified"
  )
  
  expect_error(
    compute_ofi(trades, window = "1 min", n_ticks = 10),
    "Only one of 'window', 'rolling', or 'n_ticks' may be specified"
  )
})

test_that("compute_ofi stores attributes", {
  trades <- simulate_orders(n = 100, seed = 123)
  
  # Calendar window
  result1 <- compute_ofi(trades, window = "5 min", eps = 0.01)
  expect_equal(attr(result1, "window_type"), "calendar")
  expect_equal(attr(result1, "window_param"), "5 min")
  expect_equal(attr(result1, "eps"), 0.01)
  
  # Tick window
  result2 <- compute_ofi(trades, n_ticks = 25)
  expect_equal(attr(result2, "window_type"), "ticks")
  expect_equal(attr(result2, "window_param"), 25)
  
  # Rolling window
  period <- lubridate::dseconds(30)
  result3 <- compute_ofi(trades, rolling = period)
  expect_equal(attr(result3, "window_type"), "rolling")
  expect_equal(attr(result3, "window_param"), period)
})

test_that("compute_ofi handles edge cases", {
  # All buys
  all_buys <- tibble::tibble(
    timestamp = Sys.time() + 1:5,
    side = rep("B", 5),
    size = rep(100, 5)
  )
  
  result <- compute_ofi(all_buys, window = "1 hour")
  expect_equal(result$B[1], 500)
  expect_equal(result$S[1], 0)
  expect_equal(result$ofi[1], 500)
  
  # All sells
  all_sells <- tibble::tibble(
    timestamp = Sys.time() + 1:5,
    side = rep("S", 5),
    size = rep(100, 5)
  )
  
  result <- compute_ofi(all_sells, window = "1 hour")
  expect_equal(result$B[1], 0)
  expect_equal(result$S[1], 500)
  expect_equal(result$ofi[1], -500)
  
  # Empty window handling
  sparse_trades <- tibble::tibble(
    timestamp = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 10:05:00"  # 5 minutes later
    ), tz = "UTC"),
    side = c("B", "S"),
    size = c(100, 200)
  )
  
  result <- compute_ofi(sparse_trades, window = "1 min")
  # Should have 2 windows with data (minute 0 and minute 5)
  expect_equal(nrow(result), 2)
})
