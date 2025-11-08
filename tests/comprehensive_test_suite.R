#!/usr/bin/env Rscript
# ==============================================================================
# rOFI Comprehensive Test Suite
# ==============================================================================
#
# This script tests all major functions across the 7 modules plus core functionality.
# Run this script to validate the entire package.
#
# Usage:
#   source("tests/comprehensive_test_suite.R")
#   # Or from command line:
#   Rscript tests/comprehensive_test_suite.R
#
# ==============================================================================

# Suppress warnings for cleaner output
options(warn = -1)

# Test tracking
tests_run <- 0
tests_passed <- 0
tests_failed <- 0
failed_tests <- list()

cat("\n")
cat("================================================================================\n")
cat("  rOFI COMPREHENSIVE TEST SUITE\n")
cat("  Testing all major functions across 7 modules + core functionality\n")
cat("================================================================================\n\n")

# ==============================================================================
# Helper Functions
# ==============================================================================

test_function <- function(test_name, test_code) {
  tests_run <<- tests_run + 1
  tryCatch({
    test_code
    tests_passed <<- tests_passed + 1
    cat(sprintf("  ✓ %s\n", test_name))
    return(TRUE)
  }, error = function(e) {
    tests_failed <<- tests_failed + 1
    failed_tests[[test_name]] <<- as.character(e)
    cat(sprintf("  ✗ %s: %s\n", test_name, e$message))
    return(FALSE)
  })
}

# ==============================================================================
# Setup: Load Package and Generate Test Data
# ==============================================================================

cat("SETUP: Loading package and generating test data...\n")

# Load the package (assumes devtools::load_all() or installed)
if (!requireNamespace("rOFI", quietly = TRUE)) {
  cat("  Warning: rOFI not installed. Attempting to load with devtools...\n")
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all()
  } else {
    stop("Cannot load rOFI package. Please install or run devtools::load_all()")
  }
} else {
  suppressPackageStartupMessages(library(rOFI))
}

# Generate comprehensive test data for all modules
set.seed(42)
test_data <- generate_test_data(
  n_trades = 5000,
  n_messages = 1000,
  n_snapshots = 100,
  n_levels = 10,
  seed = 42
)

# Extract components for easier access
test_trades <- test_data$trades
test_messages <- test_data$messages
test_orderbook <- test_data$orderbook
test_executions <- test_data$executions
test_multi_venue <- test_data$multi_venue

# Generate OFI for various tests
test_ofi <- compute_ofi(test_trades, window = "1 min")
test_ofi_5min <- compute_ofi(test_trades, window = "5 min")

cat("  ✓ Test data generated successfully\n\n")

# ==============================================================================
# MODULE 1: CORE OFI CALCULATIONS
# ==============================================================================

cat("MODULE 1: CORE OFI CALCULATIONS\n")

test_function("compute_ofi() - basic calculation", {
  result <- compute_ofi(test_trades, window = "1 min")
  stopifnot(inherits(result, "data.frame"))
  stopifnot(nrow(result) > 0)
  stopifnot(all(c("ofi", "oir", "ofi_cum") %in% names(result)))
})

test_function("compute_ofi() - price weighted", {
  result <- compute_ofi(test_trades, window = "1 min", price_weighted = TRUE)
  stopifnot("ofi_dollar" %in% names(result))
})

test_function("compute_ofi() - tick-based", {
  result <- compute_ofi(test_trades, n_ticks = 100)
  stopifnot(nrow(result) > 0)
})

test_function("compute_ewma_ofi()", {
  result <- compute_ewma_ofi(test_ofi, lambda = 0.94)
  stopifnot("oir_ewma" %in% names(result))
})

test_function("compute_ofi_momentum()", {
  result <- compute_ofi_momentum(test_ofi, lookback = 5)
  stopifnot("oir_momentum" %in% names(result))
})

test_function("compute_ofi_acceleration()", {
  result <- compute_ofi_acceleration(test_ofi, lookback = 5)
  stopifnot("oir_acceleration" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 2: ADVANCED VISUALIZATION DASHBOARD
# ==============================================================================

cat("MODULE 2: ADVANCED VISUALIZATION DASHBOARD\n")

test_function("plot_ofi_diagnostics()", {
  if (requireNamespace("patchwork", quietly = TRUE)) {
    result <- plot_ofi_diagnostics(test_ofi, metric = "oir", max_lag = 20)
    stopifnot(inherits(result, "ggplot") || inherits(result, "patchwork"))
  } else {
    cat("    (Skipped: patchwork not installed)\n")
  }
})

test_function("plot_ofi_decomposition()", {
  result <- plot_ofi_decomposition(test_ofi, metric = "oir")
  stopifnot(inherits(result, "ggplot"))
})

test_function("plot_market_quality()", {
  if (requireNamespace("patchwork", quietly = TRUE)) {
    result <- plot_market_quality(test_trades, window = "5 min")
    stopifnot(inherits(result, "ggplot") || inherits(result, "patchwork"))
  } else {
    cat("    (Skipped: patchwork not installed)\n")
  }
})

test_function("plot_regime_detection()", {
  result <- plot_regime_detection(test_ofi, method = "quantile", n_regimes = 3)
  stopifnot(inherits(result, "ggplot"))
})

test_function("plot_comparative_analysis()", {
  ofi_list <- list(asset1 = test_ofi[1:50, ], asset2 = test_ofi[51:100, ])
  result <- plot_comparative_analysis(ofi_list, metric = "oir", layout = "facet")
  stopifnot(inherits(result, "ggplot"))
})

test_function("theme_publication()", {
  result <- theme_publication()
  stopifnot(inherits(result, "theme"))
})

cat("\n")

# ==============================================================================
# MODULE 3: PRICE IMPACT MODELS
# ==============================================================================

cat("MODULE 3: PRICE IMPACT MODELS\n")

test_function("almgren_chriss_trajectory()", {
  result <- almgren_chriss_trajectory(
    Q = 100000, T_horizon = 60, lambda = 1e-6,
    sigma = 0.30, gamma = 0.1, eta = 0.05, n_steps = 20
  )
  stopifnot(inherits(result, "almgren_chriss_result"))
  stopifnot(all(c("trajectory", "expected_cost", "variance") %in% names(result)))
})

test_function("sqrt_impact()", {
  result <- sqrt_impact(Q = 50000, V = 1000000, sigma = 0.02, Y = 0.20, bps = TRUE)
  stopifnot(is.numeric(result))
  stopifnot(result > 0)
})

test_function("calibrate_sqrt_law()", {
  result <- calibrate_sqrt_law(test_executions)
  stopifnot(inherits(result, "sqrt_calibration"))
  stopifnot("Y_fitted" %in% names(result))
})

test_function("decompose_price_impact()", {
  result <- decompose_price_impact(test_trades, window = "1 min", decay_periods = 5)
  stopifnot(inherits(result, "price_impact_decomposition"))
  stopifnot(all(c("temporary_impact", "permanent_impact") %in% names(result)))
})

test_function("implementation_shortfall()", {
  result <- implementation_shortfall(
    execution_price = c(100.1, 100.2, 100.15),
    benchmark_price = 100,
    side = "buy",
    quantity = c(1000, 1500, 2000),
    bps = TRUE
  )
  stopifnot(is.numeric(result))
})

test_function("obizhaeva_wang_impact()", {
  result <- obizhaeva_wang_impact(
    Q = 100000, tau = 0.1, sigma = 0.02,
    gamma = 0.3, rho = 0.5, bps = TRUE
  )
  stopifnot(is.numeric(result))
  stopifnot(result > 0)
})

test_function("predict_execution_cost()", {
  result <- predict_execution_cost(
    Q = 50000, V = 1000000, sigma = 0.02,
    models = c("sqrt", "linear")
  )
  stopifnot(inherits(result, "execution_cost_prediction"))
  stopifnot(length(result$predictions) > 0)
})

cat("\n")

# ==============================================================================
# MODULE 4: PRODUCTION DATA PIPELINES
# ==============================================================================

cat("MODULE 4: PRODUCTION DATA PIPELINES\n")

test_function("classify_trades() - tick rule", {
  result <- classify_trades(test_trades, method = "tick")
  stopifnot("direction" %in% names(result))
})

test_function("validate_tick_data()", {
  result <- validate_tick_data(test_trades, strict = FALSE)
  stopifnot(inherits(result, "tick_validation"))
  stopifnot(all(c("passed", "issues", "warnings") %in% names(result)))
})

test_function("consolidate_venues()", {
  result <- consolidate_venues(test_multi_venue)
  stopifnot(nrow(result) > 0)
  stopifnot("nbbo" %in% names(result) || "consolidated" %in% class(result))
})

test_function("reconstruct_orderbook()", {
  result <- reconstruct_orderbook(test_messages, depth = 10, snapshot_freq = 100)
  stopifnot(inherits(result, "orderbook_reconstruction"))
  stopifnot("snapshots" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 5: REGULATORY SURVEILLANCE
# ==============================================================================

cat("MODULE 5: REGULATORY SURVEILLANCE\n")

test_function("detect_spoofing()", {
  result <- detect_spoofing(
    test_messages, cancel_threshold = 0.70,
    size_threshold = 0.90, time_window = 60
  )
  stopifnot(inherits(result, "spoofing_detection"))
  stopifnot("spoof_score" %in% names(result))
})

test_function("detect_layering()", {
  result <- detect_layering(
    test_messages, n_levels = 5,
    coordination_threshold = 0.70, min_layers = 3
  )
  stopifnot(inherits(result, "layering_detection"))
  stopifnot("layer_score" %in% names(result))
})

test_function("detect_quote_stuffing()", {
  result <- detect_quote_stuffing(
    test_messages, msg_rate_threshold = 100,
    burst_window = 1, cancel_ratio_threshold = 0.80
  )
  stopifnot(inherits(result, "quote_stuffing_detection"))
  stopifnot("stuffing_score" %in% names(result))
})

test_function("compute_order_to_trade_ratio()", {
  result <- compute_order_to_trade_ratio(test_messages, window = "1 min")
  stopifnot(inherits(result, "data.frame"))
  stopifnot("ratio" %in% names(result))
})

test_function("analyze_cancellation_patterns()", {
  result <- analyze_cancellation_patterns(test_messages, time_window = 60)
  stopifnot(inherits(result, "cancellation_analysis"))
  stopifnot("cancel_rate" %in% names(result))
})

test_function("surveillance_alert_system()", {
  result <- surveillance_alert_system(
    test_messages, sensitivity = "medium",
    algorithms = c("spoofing", "layering", "stuffing")
  )
  stopifnot(inherits(result, "surveillance_alerts"))
  stopifnot("alerts" %in% names(result))
  stopifnot("summary" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 6: MULTI-LEVEL ORDER BOOK METRICS
# ==============================================================================

cat("MODULE 6: MULTI-LEVEL ORDER BOOK METRICS\n")

test_function("compute_multilevel_ofi()", {
  result <- compute_multilevel_ofi(
    test_orderbook, n_levels = 5, weighting = "volume"
  )
  stopifnot(inherits(result, "multilevel_ofi"))
  stopifnot("ofi_multilevel" %in% names(result))
})

test_function("orderbook_slope()", {
  result <- orderbook_slope(test_orderbook, n_levels = 10, side = "both")
  stopifnot(inherits(result, "orderbook_slope"))
  stopifnot(all(c("bid_slope", "ask_slope") %in% names(result)))
})

test_function("orderbook_curvature()", {
  result <- orderbook_curvature(test_orderbook, n_levels = 10)
  stopifnot(inherits(result, "orderbook_curvature"))
  stopifnot("curvature" %in% names(result))
})

test_function("volume_distribution_levels()", {
  result <- volume_distribution_levels(test_orderbook, n_levels = 10)
  stopifnot(inherits(result, "data.frame"))
  stopifnot("level" %in% names(result))
})

test_function("bid_ask_pressure()", {
  result <- bid_ask_pressure(test_orderbook, n_levels = 5)
  stopifnot(inherits(result, "data.frame"))
  stopifnot("pressure" %in% names(result))
})

test_function("depth_imbalance()", {
  result <- depth_imbalance(test_orderbook, n_levels = 5)
  stopifnot(inherits(result, "data.frame"))
  stopifnot("imbalance" %in% names(result))
})

test_function("microprice()", {
  result <- microprice(test_orderbook, n_levels = 1)
  stopifnot(inherits(result, "data.frame"))
  stopifnot("microprice" %in% names(result))
})

test_function("order_book_resilience()", {
  result <- order_book_resilience(
    test_messages, event_type = "large_trade", recovery_window = 30
  )
  stopifnot(inherits(result, "resilience_analysis"))
  stopifnot("resilience_score" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 7: CROSS-MARKET ANALYSIS
# ==============================================================================

cat("MODULE 7: CROSS-MARKET ANALYSIS\n")

test_function("compute_cross_asset_ofi()", {
  trades_list <- list(
    asset1 = test_trades[1:2000, ],
    asset2 = test_trades[2001:4000, ]
  )
  result <- compute_cross_asset_ofi(trades_list, window = "1 min")
  stopifnot(inherits(result, "cross_asset_ofi"))
  stopifnot("correlation_matrix" %in% names(result))
})

test_function("lead_lag_analysis()", {
  ofi1 <- test_ofi[1:100, ]
  ofi2 <- test_ofi_5min[1:20, ]
  result <- lead_lag_analysis(ofi1, ofi2, max_lag = 10, metric = "oir")
  stopifnot(inherits(result, "lead_lag_result"))
  stopifnot("ccf" %in% names(result))
})

test_function("cross_impact_matrix()", {
  ofi_list <- list(
    asset1 = test_ofi[1:100, ],
    asset2 = test_ofi[1:100, ]
  )
  result <- cross_impact_matrix(ofi_list)
  stopifnot(inherits(result, "cross_impact"))
  stopifnot("impact_matrix" %in% names(result))
})

test_function("lagged_cross_correlation()", {
  result <- lagged_cross_correlation(
    test_ofi$oir[1:100], test_ofi$ofi[1:100], max_lag = 10
  )
  stopifnot(inherits(result, "data.frame"))
  stopifnot("lag" %in% names(result))
})

test_function("price_discovery_metrics()", {
  price_data <- list(
    venue1 = data.frame(time = 1:100, price = cumsum(rnorm(100, 0, 0.1)) + 100),
    venue2 = data.frame(time = 1:100, price = cumsum(rnorm(100, 0, 0.1)) + 100)
  )
  result <- price_discovery_metrics(price_data, method = "hasbrouck")
  stopifnot(inherits(result, "price_discovery"))
  stopifnot("information_shares" %in% names(result))
})

test_function("pca_orderflow()", {
  ofi_list <- list(
    asset1 = test_ofi[1:100, ],
    asset2 = test_ofi[1:100, ],
    asset3 = test_ofi[1:100, ]
  )
  result <- pca_orderflow(ofi_list, n_components = 2)
  stopifnot(inherits(result, "pca_orderflow"))
  stopifnot("variance_explained" %in% names(result))
})

test_function("spillover_analysis()", {
  ofi_matrix <- matrix(rnorm(300), ncol = 3)
  result <- spillover_analysis(ofi_matrix, max_lag = 5)
  stopifnot(inherits(result, "spillover_result"))
  stopifnot("spillover_index" %in% names(result))
})

test_function("arbitrage_opportunities()", {
  prices_list <- list(
    venue1 = data.frame(time = 1:100, price = cumsum(rnorm(100, 0, 0.01)) + 100),
    venue2 = data.frame(time = 1:100, price = cumsum(rnorm(100, 0, 0.01)) + 100.05)
  )
  result <- arbitrage_opportunities(prices_list, threshold = 0.1)
  stopifnot(inherits(result, "arbitrage_detection"))
  stopifnot("opportunities" %in% names(result))
})

test_function("etf_arbitrage_metrics()", {
  etf_price <- cumsum(rnorm(100, 0, 0.1)) + 100
  nav <- cumsum(rnorm(100, 0, 0.09)) + 100
  result <- etf_arbitrage_metrics(etf_price, nav, threshold = 0.1)
  stopifnot(inherits(result, "data.frame"))
  stopifnot("basis_bps" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 8: MACHINE LEARNING FEATURES
# ==============================================================================

cat("MODULE 8: MACHINE LEARNING FEATURES\n")

test_function("engineer_ofi_features()", {
  result <- engineer_ofi_features(
    test_trades, orderbook = test_orderbook,
    lookback = 5, n_levels = 3, include_crosses = TRUE
  )
  stopifnot(inherits(result, "data.frame"))
  stopifnot(ncol(result) > 10)  # Should have many features
})

test_function("create_event_bars() - tick bars", {
  result <- create_event_bars(test_trades, bar_type = "tick", bar_size = 100)
  stopifnot(inherits(result, "data.frame"))
  stopifnot(nrow(result) > 0)
})

test_function("create_event_bars() - volume bars", {
  result <- create_event_bars(test_trades, bar_type = "volume", bar_size = 10000)
  stopifnot(inherits(result, "data.frame"))
  stopifnot(nrow(result) > 0)
})

test_function("create_event_bars() - dollar bars", {
  result <- create_event_bars(test_trades, bar_type = "dollar", bar_size = 1000000)
  stopifnot(inherits(result, "data.frame"))
  stopifnot(nrow(result) > 0)
})

test_function("stationarize_ofi() - standardize", {
  result <- stationarize_ofi(test_ofi, method = "standardize")
  stopifnot(inherits(result, "data.frame"))
  stopifnot(abs(mean(result$oir, na.rm = TRUE)) < 0.1)  # Should be near 0
})

test_function("stationarize_ofi() - difference", {
  result <- stationarize_ofi(test_ofi, method = "difference")
  stopifnot(inherits(result, "data.frame"))
  stopifnot(nrow(result) == nrow(test_ofi) - 1)
})

test_function("create_ml_dataset()", {
  features <- data.frame(
    feature1 = rnorm(100),
    feature2 = rnorm(100),
    feature3 = rnorm(100),
    target = rnorm(100)
  )
  result <- create_ml_dataset(
    features, target_col = "target",
    train_fraction = 0.70, validation_fraction = 0.15
  )
  stopifnot(inherits(result, "ml_dataset"))
  stopifnot(all(c("train", "validation", "test") %in% names(result)))
})

test_function("create_prediction_targets()", {
  result <- create_prediction_targets(
    test_trades, target_type = "return",
    horizon = 5, log_returns = TRUE
  )
  stopifnot(inherits(result, "data.frame"))
  stopifnot("target" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 9: DATA QUALITY & VALIDATION
# ==============================================================================

cat("MODULE 9: DATA QUALITY & VALIDATION\n")

test_function("validate_trade_data()", {
  result <- validate_trade_data(test_trades)
  stopifnot(inherits(result, "trade_validation"))
  stopifnot("passed" %in% names(result))
})

test_function("clean_trade_data()", {
  result <- clean_trade_data(test_trades, remove_outliers = TRUE, remove_duplicates = TRUE)
  stopifnot(inherits(result, "data.frame"))
  stopifnot(nrow(result) > 0)
})

test_function("detect_outliers_ofi()", {
  result <- detect_outliers_ofi(test_trades, method = "iqr", threshold = 3)
  stopifnot(inherits(result, "outlier_detection"))
  stopifnot("n_outliers" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 10: STATISTICAL TESTING
# ==============================================================================

cat("MODULE 10: STATISTICAL TESTING\n")

test_function("test_ofi_autocorrelation()", {
  result <- test_ofi_autocorrelation(test_ofi, max_lag = 20)
  stopifnot(inherits(result, "ofi_acf_test"))
  stopifnot("acf_values" %in% names(result))
})

test_function("ofi_lead_lag_analysis()", {
  result <- ofi_lead_lag_analysis(test_trades, window = "1 min")
  stopifnot(inherits(result, "lead_lag_result"))
  stopifnot("interpretation" %in% names(result))
})

test_function("bootstrap_ofi_significance()", {
  result <- bootstrap_ofi_significance(test_ofi, n_bootstrap = 100)
  stopifnot(inherits(result, "bootstrap_test"))
  stopifnot("p_value" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 11: MARKET MICROSTRUCTURE (CORE)
# ==============================================================================

cat("MODULE 11: MARKET MICROSTRUCTURE (CORE)\n")

test_function("kyle_lambda_estimation()", {
  result <- kyle_lambda_estimation(test_trades, window = "1 min")
  stopifnot(inherits(result, "kyle_lambda"))
  stopifnot("lambda" %in% names(result))
})

test_function("estimate_price_impact()", {
  result <- estimate_price_impact(test_trades, window = "1 min", decay_periods = 5)
  stopifnot(inherits(result, "price_impact"))
  stopifnot(all(c("temporary_impact", "permanent_impact") %in% names(result)))
})

test_function("compute_vpin()", {
  result <- compute_vpin(test_trades, n_buckets = 50, lookback = 50)
  stopifnot(inherits(result, "data.frame"))
  stopifnot("vpin" %in% names(result))
})

test_function("compute_effective_spread()", {
  result <- compute_effective_spread(test_trades, midpoint_method = "rolling")
  stopifnot(inherits(result, "data.frame"))
  stopifnot("effective_spread" %in% names(result))
})

test_function("decompose_spread()", {
  result <- decompose_spread(test_trades, horizon = 5)
  stopifnot(inherits(result, "spread_decomposition"))
  stopifnot("pct_adverse_selection" %in% names(result))
})

cat("\n")

# ==============================================================================
# MODULE 12: BASIC VISUALIZATION
# ==============================================================================

cat("MODULE 12: BASIC VISUALIZATION\n")

test_function("plot_ofi()", {
  result <- plot_ofi(test_ofi, which = c("ofi", "oir"))
  stopifnot(inherits(result, "ggplot"))
})

test_function("plot_ofi_dist()", {
  result <- plot_ofi_dist(test_ofi, metric = "oir", plot_type = "histogram")
  stopifnot(inherits(result, "ggplot"))
})

cat("\n")

# ==============================================================================
# MODULE 13: DATA LOADING
# ==============================================================================

cat("MODULE 13: DATA LOADING & GENERATION\n")

test_function("simulate_orders()", {
  result <- simulate_orders(n = 1000, imb = 0.1, seed = 123)
  stopifnot(inherits(result, "data.frame"))
  stopifnot(nrow(result) == 1000)
  stopifnot(all(c("timestamp", "side", "size", "price") %in% names(result)))
})

test_function("as_ofi() - standardization", {
  custom_data <- data.frame(
    time = seq(Sys.time(), by = "1 sec", length.out = 100),
    direction = sample(c(1, -1), 100, replace = TRUE),
    volume = runif(100, 100, 1000),
    px = 100 + cumsum(rnorm(100, 0, 0.1))
  )
  result <- as_ofi(custom_data, time_col = "time", side_col = "direction",
                   size_col = "volume", price_col = "px")
  stopifnot(inherits(result, "data.frame"))
  stopifnot(all(c("timestamp", "side", "size", "price") %in% names(result)))
})

cat("\n")

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================

cat("================================================================================\n")
cat("  TEST SUMMARY\n")
cat("================================================================================\n\n")

pass_rate <- round(tests_passed / tests_run * 100, 1)

cat(sprintf("  Total Tests Run:    %d\n", tests_run))
cat(sprintf("  Tests Passed:       %d (%.1f%%)\n", tests_passed, pass_rate))
cat(sprintf("  Tests Failed:       %d\n", tests_failed))
cat("\n")

if (tests_failed > 0) {
  cat("FAILED TESTS:\n")
  cat("-------------\n")
  for (test_name in names(failed_tests)) {
    cat(sprintf("  • %s\n    Error: %s\n\n", test_name, failed_tests[[test_name]]))
  }
}

if (pass_rate == 100) {
  cat("🎉 ALL TESTS PASSED! 🎉\n")
  cat("\n")
  cat("The rOFI package is functioning correctly across all modules:\n")
  cat("  ✓ Core OFI Calculations\n")
  cat("  ✓ Advanced Visualization Dashboard\n")
  cat("  ✓ Price Impact Models\n")
  cat("  ✓ Production Data Pipelines\n")
  cat("  ✓ Regulatory Surveillance\n")
  cat("  ✓ Multi-Level Order Book Metrics\n")
  cat("  ✓ Cross-Market Analysis\n")
  cat("  ✓ Machine Learning Features\n")
  cat("  ✓ Data Quality & Validation\n")
  cat("  ✓ Statistical Testing\n")
  cat("  ✓ Market Microstructure (Core)\n")
  cat("  ✓ Visualization\n")
  cat("  ✓ Data Loading & Generation\n")
} else if (pass_rate >= 90) {
  cat("⚠️  MOSTLY PASSING (>=90%)\n")
  cat("Most functionality is working, but some tests failed.\n")
  cat("Review the failed tests above.\n")
} else if (pass_rate >= 75) {
  cat("⚠️  PARTIALLY PASSING (75-90%)\n")
  cat("Significant functionality is working, but multiple tests failed.\n")
  cat("Review the failed tests above and investigate issues.\n")
} else {
  cat("❌ MANY FAILURES (<75%)\n")
  cat("Critical issues detected. Please review all failed tests.\n")
}

cat("\n")
cat("================================================================================\n")
cat("  END OF TEST SUITE\n")
cat("================================================================================\n\n")

# Restore warnings
options(warn = 0)

# Return summary invisibly
invisible(list(
  tests_run = tests_run,
  tests_passed = tests_passed,
  tests_failed = tests_failed,
  pass_rate = pass_rate,
  failed_tests = failed_tests
))
