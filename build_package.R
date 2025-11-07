#!/usr/bin/env Rscript

# Build script for rOFI package development
# Run this script to build, check, and install the package

cat("=== rOFI Package Build Script ===\n\n")

# Load required packages
required_packages <- c("devtools", "roxygen2", "testthat", "pkgdown")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Installing %s...\n", pkg))
    install.packages(pkg)
  }
}

library(devtools)

# Set working directory to package root
if (basename(getwd()) != "rOFI") {
  if (file.exists("rOFI")) {
    setwd("rOFI")
  } else {
    stop("Please run this script from the parent directory of the rOFI package")
  }
}

cat("Working directory:", getwd(), "\n\n")

# Step 1: Generate documentation
cat("Step 1: Generating documentation with roxygen2...\n")
roxygen2::roxygenise()
cat("✓ Documentation generated\n\n")

# Step 2: Create demo data if needed
if (!file.exists("data/ofi_demo.rda")) {
  cat("Step 2: Creating demo dataset...\n")
  source("data-raw/make_demo_data.R")
  cat("✓ Demo data created\n\n")
} else {
  cat("Step 2: Demo data already exists\n\n")
}

# Step 3: Run tests
cat("Step 3: Running tests...\n")
test_results <- test()
if (!any(test_results$failed > 0)) {
  cat("✓ All tests passed\n\n")
} else {
  cat("⚠ Some tests failed\n\n")
}

# Step 4: Check package
cat("Step 4: Running R CMD check...\n")
check_results <- check(document = FALSE, cran = TRUE)
if (length(check_results$errors) == 0 && length(check_results$warnings) == 0) {
  cat("✓ Package check passed\n\n")
} else {
  cat("⚠ Check found issues - see output above\n\n")
}

# Step 5: Build package
cat("Step 5: Building package...\n")
build()
cat("✓ Package built\n\n")

# Step 6: Install package locally
cat("Step 6: Installing package...\n")
install()
cat("✓ Package installed\n\n")

# Step 7: Build vignettes (optional)
if (interactive()) {
  cat("Step 7: Building vignettes...\n")
  build_vignettes()
  cat("✓ Vignettes built\n\n")
}

# Final summary
cat("\n=== Build Summary ===\n")
cat("Package: rOFI\n")
cat("Version:", as.character(packageVersion("rOFI")), "\n")
cat("\nQuick test:\n")
cat("------------\n")

# Run a quick test
library(rOFI)
trades <- simulate_orders(n = 100, seed = 42)
ofi <- compute_ofi(trades, window = "1 min")
cat("✓ Generated", nrow(trades), "trades\n")
cat("✓ Computed OFI for", nrow(ofi), "time windows\n")
cat("✓ Mean OIR:", round(mean(ofi$oir), 4), "\n")

cat("\n=== Build Complete ===\n")
cat("\nTo use the package:\n")
cat("  library(rOFI)\n")
cat("  ?rOFI  # For package documentation\n")
cat("  vignette('getting-started', package = 'rOFI')  # For tutorial\n")
