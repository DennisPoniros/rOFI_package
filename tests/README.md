# rOFI Test Suite

This directory contains comprehensive testing infrastructure for the rOFI package.

## Test Files

### 1. `testthat/` Directory
Contains unit tests for all modules using the `testthat` framework. Run with:

```r
devtools::test()
# Or
testthat::test_dir("tests/testthat")
```

**Test files:**
- `test-visualization_dashboard.R` - Advanced visualization tests (60+ tests)
- `test-price_impact.R` - Price impact models tests (50+ tests)
- `test-surveillance.R` - Regulatory surveillance tests (70+ tests)
- `test-*.R` - Additional module tests

**Total: 180+ unit tests**

### 2. `comprehensive_test_suite.R`
A complete integration test suite covering all 80+ exported functions across 13 modules.

**Usage:**
```r
# From R console
source("tests/comprehensive_test_suite.R")

# From command line
Rscript tests/comprehensive_test_suite.R
```

**What it tests:**
- ✅ Core OFI Calculations (6 functions)
- ✅ Advanced Visualization Dashboard (6 functions)
- ✅ Price Impact Models (7 functions)
- ✅ Production Data Pipelines (4 functions)
- ✅ Regulatory Surveillance (6 functions)
- ✅ Multi-Level Order Book Metrics (8 functions)
- ✅ Cross-Market Analysis (9 functions)
- ✅ Machine Learning Features (6 functions)
- ✅ Data Quality & Validation (3 functions)
- ✅ Statistical Testing (3 functions)
- ✅ Market Microstructure Core (5 functions)
- ✅ Basic Visualization (2 functions)
- ✅ Data Loading & Generation (2 functions)

**Output:**
```
================================================================================
  rOFI COMPREHENSIVE TEST SUITE
  Testing all major functions across 7 modules + core functionality
================================================================================

MODULE 1: CORE OFI CALCULATIONS
  ✓ compute_ofi() - basic calculation
  ✓ compute_ofi() - price weighted
  ✓ compute_ofi() - tick-based
  ...

================================================================================
  TEST SUMMARY
================================================================================

  Total Tests Run:    67
  Tests Passed:       67 (100.0%)
  Tests Failed:       0

🎉 ALL TESTS PASSED! 🎉
```

### 3. `quick_test.R`
A rapid smoke test for core functionality. Use during development for fast validation.

**Usage:**
```r
source("tests/quick_test.R")
```

**What it tests:**
- Core OFI calculation
- Basic visualization
- Price impact models
- Data validation
- Statistical testing

**Typical runtime:** <5 seconds

## Testing Workflows

### During Development
```r
# Quick validation
source("tests/quick_test.R")
```

### Before Committing
```r
# Run unit tests
devtools::test()

# Run comprehensive suite
source("tests/comprehensive_test_suite.R")
```

### Before Release
```r
# Full package check
devtools::check()

# Run all tests
devtools::test()
source("tests/comprehensive_test_suite.R")

# Check test coverage
covr::package_coverage()
```

## Test Data

All test scripts generate synthetic data automatically:

```r
# Trade data
test_trades <- simulate_orders(n = 5000, imb = 0.1, seed = 42)

# OFI metrics
test_ofi <- compute_ofi(test_trades, window = "1 min")

# Message data (for surveillance/orderbook tests)
test_messages <- generate_mock_messages(n = 1000)

# Order book snapshots
test_orderbook <- generate_mock_orderbook(n_snapshots = 100)
```

No external data files required.

## Test Coverage

### Current Coverage: ~180+ Tests

| Module | Unit Tests | Integration Tests | Coverage |
|--------|-----------|-------------------|----------|
| Core OFI | 40+ | 6 | ✅ High |
| Visualization | 60+ | 6 | ✅ High |
| Price Impact | 50+ | 7 | ✅ High |
| Data Pipelines | 30+ | 4 | ✅ Good |
| Surveillance | 70+ | 6 | ✅ High |
| Multi-Level OFI | 30+ | 8 | ✅ Good |
| Cross-Market | 20+ | 9 | ✅ Good |
| ML Features | 25+ | 6 | ✅ Good |

### What's Tested

**Input Validation:**
- ✅ Invalid data types
- ✅ Missing values
- ✅ Edge cases (empty data, single row, etc.)
- ✅ Parameter bounds

**Functionality:**
- ✅ Correct calculations
- ✅ Output structure and types
- ✅ Metadata and attributes
- ✅ Print methods

**Integration:**
- ✅ Function chaining
- ✅ Cross-module compatibility
- ✅ Data pipeline end-to-end

## Adding New Tests

### For Unit Tests (testthat)

Create or edit `tests/testthat/test-<module>.R`:

```r
test_that("my_function works correctly", {
  # Setup
  test_data <- simulate_orders(n = 100)

  # Test
  result <- my_function(test_data, param = "value")

  # Assertions
  expect_s3_class(result, "data.frame")
  expect_true("column_name" %in% names(result))
  expect_equal(nrow(result), expected_rows)
})
```

### For Integration Tests

Add to `comprehensive_test_suite.R`:

```r
test_function("my_function()", {
  result <- my_function(test_data, param = "value")
  stopifnot(inherits(result, "expected_class"))
  stopifnot("expected_column" %in% names(result))
})
```

## Troubleshooting

### Tests Fail Locally

1. **Check R version:** Package requires R >= 4.0
2. **Update dependencies:** `devtools::install_deps()`
3. **Clean and reload:** `devtools::clean_dll()` then `devtools::load_all()`

### Specific Module Fails

1. Check if optional dependencies are installed (e.g., `patchwork` for visualization)
2. Review the error message in the test output
3. Run the failing test in isolation:
   ```r
   testthat::test_file("tests/testthat/test-<module>.R")
   ```

### Comprehensive Suite Slow

This is normal - it tests 67+ functions with realistic data. Expect 30-60 seconds runtime.

For faster iteration, use `quick_test.R` instead.

## Continuous Integration

Tests are run automatically on GitHub Actions for every commit and PR.

See `.github/workflows/R-CMD-check.yaml` for CI configuration.

## Test Philosophy

**rOFI follows these testing principles:**

1. **Comprehensive Coverage** - Test all exported functions
2. **Realistic Data** - Use synthetic data that mimics real market data
3. **Clear Errors** - Informative messages when tests fail
4. **Fast Feedback** - Quick tests for development, comprehensive for release
5. **No External Dependencies** - All test data generated programmatically

## Questions?

For questions about testing:
- Open an issue: https://github.com/DennisPoniros/rOFI_package/issues
- Check test examples in `testthat/` directory
- Review comprehensive test output for usage examples
