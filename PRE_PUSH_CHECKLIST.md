# Pre-Push Checklist for rOFI Package

## Before Pushing to GitHub

### ✅ Package Completeness
- [ ] All 4 core functions implemented (`compute_ofi`, `plot_ofi`, `simulate_orders`, `as_ofi`)
- [ ] All functions have roxygen2 documentation
- [ ] Examples included for each exported function
- [ ] Vignette is complete and renders correctly

### ✅ Testing
- [ ] All tests pass locally (`devtools::test()`)
- [ ] Package check passes (`devtools::check()`)
- [ ] No errors, warnings, or notes that need fixing

### ✅ Documentation
- [ ] README.md is complete with examples
- [ ] DESCRIPTION file has correct metadata
- [ ] NEWS.md documents initial release
- [ ] License file is present

### ✅ GitHub-Specific Files
- [ ] .gitignore properly configured
- [ ] GitHub Actions workflow included
- [ ] CONTRIBUTING.md present
- [ ] All URLs updated to use DennisPoniros/rOFI_package

## Quick Local Test

Run this in R before pushing:

```r
# Load all package functions
devtools::load_all("/path/to/rOFI")

# Run tests
devtools::test()

# Check package
devtools::check()

# Build and install locally
devtools::install()

# Test that it works
library(rOFI)
trades <- simulate_orders(n = 100, seed = 42)
ofi <- compute_ofi(trades, window = "1 min")
plot_ofi(ofi, which = "oir")
```

## Files That Should Be Present

```
rOFI/
├── .github/
│   └── workflows/
│       └── R-CMD-check.yaml
├── R/
│   ├── as_ofi.R
│   ├── compute_ofi.R
│   ├── plot_ofi.R
│   ├── rOFI-package.R
│   └── simulate_orders.R
├── data-raw/
│   └── make_demo_data.R
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── test-as_ofi.R
│       ├── test-compute_ofi.R
│       ├── test-plot_ofi.R
│       └── test-simulate_orders.R
├── vignettes/
│   └── getting-started.Rmd
├── .Rbuildignore
├── .gitignore
├── CONTRIBUTING.md
├── DESCRIPTION
├── LICENSE
├── NAMESPACE
├── NEWS.md
├── README.md
├── build_package.R
└── setup_github.sh
```

## Ready? Let's Go! 🚀

If all checks pass, you're ready to push to GitHub!
