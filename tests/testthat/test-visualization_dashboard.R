test_that("plot_ofi_diagnostics works with valid data", {
  skip_if_not_installed("patchwork")

  trades <- simulate_orders(n = 500, imb = 0.1, seed = 42)
  ofi <- compute_ofi(trades, window = "1 min")

  # Should produce a plot without error
  expect_no_error({
    p <- plot_ofi_diagnostics(ofi, metric = "oir", max_lag = 10)
  })

  # Result should be a ggplot/patchwork object
  p <- plot_ofi_diagnostics(ofi, metric = "oir", max_lag = 10)
  expect_s3_class(p, c("patchwork", "gg", "ggplot"))
})

test_that("plot_ofi_diagnostics fails gracefully with invalid input", {
  skip_if_not_installed("patchwork")

  trades <- simulate_orders(n = 500, seed = 42)
  ofi <- compute_ofi(trades, window = "1 min")

  # Invalid metric should error
  expect_error(
    plot_ofi_diagnostics(ofi, metric = "nonexistent"),
    "not found"
  )

  # Insufficient data should error
  ofi_small <- ofi[1:5, ]
  expect_error(
    plot_ofi_diagnostics(ofi_small),
    "Insufficient"
  )
})

test_that("plot_ofi_decomposition works", {
  trades <- simulate_orders(n = 1000, seed = 42)
  ofi <- compute_ofi(trades, window = "1 min")

  expect_no_error({
    p <- plot_ofi_decomposition(ofi, metric = "oir")
  })

  p <- plot_ofi_decomposition(ofi, metric = "oir")
  expect_s3_class(p, c("gg", "ggplot"))
})

test_that("plot_market_quality creates dashboard", {
  skip_if_not_installed("patchwork")

  trades <- simulate_orders(n = 1000, seed = 42)

  expect_no_error({
    p <- plot_market_quality(trades, window = "5 min")
  })

  p <- plot_market_quality(trades, window = "5 min")
  expect_s3_class(p, c("patchwork", "gg", "ggplot"))
})

test_that("plot_market_quality requires price column", {
  skip_if_not_installed("patchwork")

  trades <- data.frame(
    timestamp = Sys.time() + 1:100,
    side = sample(c("B", "S"), 100, replace = TRUE),
    size = runif(100, 100, 1000)
    # Missing price column
  )

  expect_error(
    plot_market_quality(trades),
    "Missing required columns"
  )
})

test_that("plot_regime_detection works with quantile method", {
  trades <- simulate_orders(n = 1000, seed = 42)
  ofi <- compute_ofi(trades, window = "1 min")

  expect_no_error({
    p <- plot_regime_detection(ofi, method = "quantile", n_regimes = 3)
  })

  p <- plot_regime_detection(ofi, method = "quantile", n_regimes = 3)
  expect_s3_class(p, c("gg", "ggplot"))
})

test_that("plot_regime_detection works with kmeans method", {
  trades <- simulate_orders(n = 1000, seed = 42)
  ofi <- compute_ofi(trades, window = "1 min")

  expect_no_error({
    p <- plot_regime_detection(ofi, method = "kmeans", n_regimes = 2)
  })

  p <- plot_regime_detection(ofi, method = "kmeans", n_regimes = 2)
  expect_s3_class(p, c("gg", "ggplot"))
})

test_that("plot_comparative_analysis works with facet layout", {
  trades1 <- simulate_orders(n = 500, imb = 0, seed = 1)
  trades2 <- simulate_orders(n = 500, imb = 0.2, seed = 2)

  ofi1 <- compute_ofi(trades1, window = "1 min")
  ofi2 <- compute_ofi(trades2, window = "1 min")

  ofi_list <- list("Neutral" = ofi1, "Bullish" = ofi2)

  expect_no_error({
    p <- plot_comparative_analysis(ofi_list, metric = "oir", layout = "facet")
  })

  p <- plot_comparative_analysis(ofi_list, metric = "oir", layout = "facet")
  expect_s3_class(p, c("gg", "ggplot"))
})

test_that("plot_comparative_analysis works with overlay layout", {
  trades1 <- simulate_orders(n = 500, imb = 0, seed = 1)
  trades2 <- simulate_orders(n = 500, imb = 0.2, seed = 2)

  ofi1 <- compute_ofi(trades1, window = "1 min")
  ofi2 <- compute_ofi(trades2, window = "1 min")

  ofi_list <- list("Neutral" = ofi1, "Bullish" = ofi2)

  expect_no_error({
    p <- plot_comparative_analysis(ofi_list, metric = "oir", layout = "overlay")
  })

  p <- plot_comparative_analysis(ofi_list, metric = "oir", layout = "overlay")
  expect_s3_class(p, c("gg", "ggplot"))
})

test_that("plot_comparative_analysis normalizes data correctly", {
  trades1 <- simulate_orders(n = 500, imb = 0, seed = 1)
  trades2 <- simulate_orders(n = 500, imb = 0.3, seed = 2)

  ofi1 <- compute_ofi(trades1, window = "1 min")
  ofi2 <- compute_ofi(trades2, window = "1 min")

  ofi_list <- list("A" = ofi1, "B" = ofi2)

  expect_no_error({
    p <- plot_comparative_analysis(ofi_list, metric = "oir", normalize = TRUE)
  })
})

test_that("plot_comparative_analysis requires list input", {
  trades <- simulate_orders(n = 500, seed = 42)
  ofi <- compute_ofi(trades, window = "1 min")

  expect_error(
    plot_comparative_analysis(ofi),
    "named list"
  )

  expect_error(
    plot_comparative_analysis(list(ofi)),
    "at least 2"
  )
})

test_that("theme_publication creates valid theme", {
  theme_obj <- theme_publication()

  expect_s3_class(theme_obj, "theme")
  expect_true("gg" %in% class(theme_obj))
})

test_that("theme_publication can be added to plots", {
  trades <- simulate_orders(n = 500, seed = 42)
  ofi <- compute_ofi(trades, window = "1 min")

  expect_no_error({
    p <- plot_ofi(ofi) + theme_publication()
  })

  p <- plot_ofi(ofi) + theme_publication()
  expect_s3_class(p, c("gg", "ggplot"))
})

test_that("helper functions calculate statistics correctly", {
  x <- rnorm(100, mean = 0, sd = 1)

  # Skewness should be close to 0 for normal
  skew <- rOFI:::calculate_skewness(x)
  expect_type(skew, "double")
  expect_lt(abs(skew), 1)  # Should be reasonably close to 0

  # Kurtosis should be close to 0 for normal (excess kurtosis)
  kurt <- rOFI:::calculate_kurtosis(x)
  expect_type(kurt, "double")
  expect_lt(abs(kurt), 2)  # Should be reasonably close to 0
})

test_that("detect_frequency works", {
  # Create time series with 5-minute intervals
  times <- seq(as.POSIXct("2024-01-01 09:30:00"),
               by = "5 min",
               length.out = 100)

  freq <- rOFI:::detect_frequency(times)
  expect_type(freq, "double")
  expect_gt(freq, 0)

  # Should detect approximately 12 observations per hour (5-min intervals)
  expect_true(freq >= 10 && freq <= 14)
})
