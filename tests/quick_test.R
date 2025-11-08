#!/usr/bin/env Rscript
# ==============================================================================
# rOFI Quick Test Script
# ==============================================================================
#
# A rapid smoke test for core functionality.
# Use this for quick validation during development.
#
# Usage:
#   source("tests/quick_test.R")
#   # Or: Rscript tests/quick_test.R
#
# For comprehensive testing, use: tests/comprehensive_test_suite.R
# ==============================================================================

cat("\n")
cat("========================================\n")
cat("  rOFI Quick Test\n")
cat("========================================\n\n")

# Load package
suppressPackageStartupMessages({
  if (!requireNamespace("rOFI", quietly = TRUE)) {
    if (requireNamespace("devtools", quietly = TRUE)) {
      devtools::load_all()
    } else {
      stop("Cannot load rOFI")
    }
  } else {
    library(rOFI)
  }
})

# Quick test helper
quick_test <- function(name, code) {
  cat(sprintf("Testing %s... ", name))
  tryCatch({
    code
    cat("✓\n")
    return(TRUE)
  }, error = function(e) {
    cat(sprintf("✗ (%s)\n", e$message))
    return(FALSE)
  })
}

# Generate test data
set.seed(42)
trades <- simulate_orders(n = 1000, imb = 0.1)
ofi <- compute_ofi(trades, window = "1 min")

# Run quick tests
cat("\nCore Functionality:\n")
quick_test("Data Generation", {
  stopifnot(nrow(trades) == 1000)
})

quick_test("OFI Calculation", {
  stopifnot(nrow(ofi) > 0)
  stopifnot("oir" %in% names(ofi))
})

quick_test("EWMA OFI", {
  result <- compute_ewma_ofi(ofi)
  stopifnot("oir_ewma" %in% names(result))
})

quick_test("OFI Visualization", {
  p <- plot_ofi(ofi)
  stopifnot(inherits(p, "ggplot"))
})

cat("\nAdvanced Features:\n")
quick_test("Price Impact", {
  result <- sqrt_impact(Q = 10000, V = 100000, sigma = 0.02)
  stopifnot(is.numeric(result))
})

quick_test("Almgren-Chriss", {
  result <- almgren_chriss_trajectory(Q = 10000, T_horizon = 60)
  stopifnot("trajectory" %in% names(result))
})

quick_test("Data Validation", {
  result <- validate_trade_data(trades)
  stopifnot("passed" %in% names(result))
})

quick_test("Statistical Testing", {
  result <- test_ofi_autocorrelation(ofi, max_lag = 10)
  stopifnot("acf_values" %in% names(result))
})

cat("\n✅ Quick test complete!\n")
cat("For comprehensive testing, run: source('tests/comprehensive_test_suite.R')\n\n")
