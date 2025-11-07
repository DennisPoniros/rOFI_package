test_that("plot_ofi creates valid ggplot objects", {
  trades <- simulate_orders(n = 100, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")
  
  # Default plot
  p1 <- plot_ofi(ofi)
  expect_s3_class(p1, "gg")
  expect_s3_class(p1, "ggplot")
  
  # Single metric
  p2 <- plot_ofi(ofi, which = "ofi")
  expect_s3_class(p2, "gg")
  
  # Multiple metrics without facets
  p3 <- plot_ofi(ofi, which = c("ofi", "oir"), facet = FALSE)
  expect_s3_class(p3, "gg")
})

test_that("plot_ofi handles different metrics", {
  trades <- simulate_orders(n = 100, seed = 123)
  trades$price <- 100 + runif(100, -5, 5)
  ofi <- compute_ofi(trades, window = "1 min", price_weighted = TRUE)
  
  # Test each metric individually
  metrics <- c("ofi", "oir", "ofi_cum", "vol_total", "B", "S", "ofi_dollar")
  
  for (metric in metrics) {
    p <- plot_ofi(ofi, which = metric)
    expect_s3_class(p, "gg", info = paste("Failed for metric:", metric))
  }
})

test_that("plot_ofi handles missing metrics gracefully", {
  trades <- simulate_orders(n = 100, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")  # No price, so no ofi_dollar
  
  expect_warning(
    plot_ofi(ofi, which = c("ofi", "ofi_dollar")),
    "Metrics not found in data: ofi_dollar"
  )
})

test_that("plot_ofi validates input", {
  bad_data <- data.frame(x = 1:10, y = 1:10)
  
  expect_error(
    plot_ofi(bad_data),
    "ofi_tbl must be output from compute_ofi()"
  )
  
  trades <- simulate_orders(n = 100, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")
  
  expect_error(
    plot_ofi(ofi, which = "nonexistent_metric"),
    "None of the requested metrics found"
  )
})

test_that("plot_ofi reference line works", {
  trades <- simulate_orders(n = 100, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")
  
  # With reference line
  p1 <- plot_ofi(ofi, which = "ofi", ref_line = 0)
  expect_s3_class(p1, "gg")
  
  # Without reference line
  p2 <- plot_ofi(ofi, which = "ofi", ref_line = NA)
  expect_s3_class(p2, "gg")
  
  # Custom reference line
  p3 <- plot_ofi(ofi, which = "ofi", ref_line = 100)
  expect_s3_class(p3, "gg")
})

test_that("plot_ofi titles and subtitles work", {
  trades <- simulate_orders(n = 100, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")
  
  p <- plot_ofi(ofi, 
                which = "ofi",
                title = "Custom Title",
                subtitle = "Custom Subtitle")
  
  expect_s3_class(p, "gg")
  
  # Check that labels contain the custom text
  labels <- p$labels
  expect_equal(labels$title, "Custom Title")
  expect_equal(labels$subtitle, "Custom Subtitle")
})

test_that("plot_ofi_dist creates valid plots", {
  trades <- simulate_orders(n = 500, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")
  
  # Histogram
  p1 <- plot_ofi_dist(ofi, metric = "oir", plot_type = "histogram")
  expect_s3_class(p1, "gg")
  
  # Density
  p2 <- plot_ofi_dist(ofi, metric = "ofi", plot_type = "density")
  expect_s3_class(p2, "gg")
  
  # Different bins
  p3 <- plot_ofi_dist(ofi, metric = "oir", bins = 50)
  expect_s3_class(p3, "gg")
})

test_that("plot_ofi_dist validates input", {
  trades <- simulate_orders(n = 100, seed = 123)
  ofi <- compute_ofi(trades, window = "1 min")
  
  expect_error(
    plot_ofi_dist(ofi, metric = "nonexistent"),
    "Metric 'nonexistent' not found"
  )
  
  # Create data with all NA values for a metric
  ofi_na <- ofi
  ofi_na$oir <- NA
  
  expect_error(
    plot_ofi_dist(ofi_na, metric = "oir"),
    "No non-NA values for metric"
  )
})

test_that("plot_ofi_dist handles different metric distributions", {
  # Create data with specific characteristics
  trades_balanced <- simulate_orders(n = 500, imb = 0, seed = 123)
  ofi_balanced <- compute_ofi(trades_balanced, window = "1 min")
  
  trades_bullish <- simulate_orders(n = 500, imb = 0.5, seed = 456)
  ofi_bullish <- compute_ofi(trades_bullish, window = "1 min")
  
  # Balanced should center around 0
  p1 <- plot_ofi_dist(ofi_balanced, metric = "oir")
  expect_s3_class(p1, "gg")
  
  # Bullish should have positive skew
  p2 <- plot_ofi_dist(ofi_bullish, metric = "oir")
  expect_s3_class(p2, "gg")
})

test_that("get_metric_label returns correct labels", {
  # Access internal function directly (for testing)
  get_label <- rOFI:::get_metric_label
  
  expect_equal(get_label("ofi"), "Order-Flow Imbalance")
  expect_equal(get_label("oir"), "Order Imbalance Ratio")
  expect_equal(get_label("ofi_cum"), "Cumulative OFI")
  expect_equal(get_label("vol_total"), "Total Volume")
  expect_equal(get_label("B"), "Buy Volume")
  expect_equal(get_label("S"), "Sell Volume")
  expect_equal(get_label("ofi_dollar"), "Dollar-Weighted OFI")
  expect_equal(get_label("n_trades"), "Number of Trades")
  
  # Unknown metric returns as-is
  expect_equal(get_label("unknown_metric"), "unknown_metric")
})
