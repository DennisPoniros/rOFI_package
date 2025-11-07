test_that("as_ofi handles standard input correctly", {
  # Create test data
  trades <- data.frame(
    timestamp = Sys.time() + 1:10,
    side = c("B", "S", "B", "B", "S", "B", "S", "S", "B", "B"),
    size = 100:109
  )
  
  result <- as_ofi(trades)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 10)
  expect_equal(names(result), c("timestamp", "side", "size"))
  expect_true(lubridate::is.POSIXct(result$timestamp))
})

test_that("as_ofi handles numeric side encoding", {
  trades <- data.frame(
    timestamp = Sys.time() + 1:5,
    side = c(1, -1, 1, 1, -1),
    size = 100:104
  )
  
  result <- as_ofi(trades)
  
  expect_equal(result$side, c("B", "S", "B", "B", "S"))
})

test_that("as_ofi handles various text encodings", {
  trades <- data.frame(
    timestamp = Sys.time() + 1:6,
    side = c("buy", "sell", "BUY", "SELL", "b", "s"),
    size = 100:105
  )
  
  result <- as_ofi(trades)
  
  expect_true(all(result$side %in% c("B", "S")))
})

test_that("as_ofi handles custom column names", {
  trades <- data.frame(
    time = Sys.time() + 1:5,
    direction = c("B", "S", "B", "S", "B"),
    volume = 100:104
  )
  
  result <- as_ofi(trades, 
                   time_col = "time",
                   side_col = "direction", 
                   size_col = "volume")
  
  expect_equal(names(result), c("timestamp", "side", "size"))
})

test_that("as_ofi handles price column", {
  trades <- data.frame(
    timestamp = Sys.time() + 1:5,
    side = c("B", "S", "B", "S", "B"),
    size = 100:104,
    price = c(100.5, 100.6, 100.4, 100.5, 100.7)
  )
  
  result <- as_ofi(trades, price_col = "price")
  
  expect_equal(names(result), c("timestamp", "side", "size", "price"))
  expect_equal(result$price, trades$price)
})

test_that("as_ofi handles missing values", {
  trades <- data.frame(
    timestamp = c(Sys.time() + 1:3, NA, Sys.time() + 5),
    side = c("B", "S", "B", "S", "B"),
    size = c(100, NA, 102, 103, 104)
  )
  
  expect_warning(
    result <- as_ofi(trades),
    "Dropped 2 rows with missing timestamp or size values"
  )
  
  expect_equal(nrow(result), 3)
})

test_that("as_ofi handles timezone correctly", {
  trades <- data.frame(
    timestamp = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:01:00")),
    side = c("B", "S"),
    size = c(100, 200)
  )
  
  expect_warning(
    result <- as_ofi(trades),
    "No timezone specified for timestamps, assuming UTC"
  )
  
  expect_equal(attr(result$timestamp, "tzone"), "UTC")
  
  # With specified timezone
  result2 <- as_ofi(trades, tz = "America/New_York")
  expect_equal(attr(result2$timestamp, "tzone"), "America/New_York")
})

test_that("as_ofi errors on invalid input", {
  # Missing required columns
  bad_data <- data.frame(time = Sys.time() + 1:5, volume = 100:104)
  expect_error(as_ofi(bad_data), "Missing required columns")
  
  # Invalid side values
  bad_sides <- data.frame(
    timestamp = Sys.time() + 1:3,
    side = c("X", "Y", "Z"),
    size = 100:102
  )
  expect_error(as_ofi(bad_sides), "Unknown side values")
  
  # Invalid numeric side values
  bad_numeric <- data.frame(
    timestamp = Sys.time() + 1:3,
    side = c(1, 2, 3),
    size = 100:102
  )
  expect_error(as_ofi(bad_numeric), "Numeric side values must be 1 \\(buy\\) or -1 \\(sell\\)")
})
