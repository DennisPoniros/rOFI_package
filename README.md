# rOFI: Order-Flow Imbalance for R

<!-- badges: start -->
[![R-CMD-check](https://github.com/DennisPoniros/rOFI_package/workflows/R-CMD-check/badge.svg)](https://github.com/DennisPoniros/rOFI_package/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

`rOFI` is a beginner-friendly R package for computing and visualizing order-flow imbalance (OFI) patterns from trade-level data. It provides tools to calculate simple OFI, imbalance ratios, cumulative patterns, and dollar-weighted metrics, making market microstructure concepts accessible to students and analysts.

## Key Features

### Core Functionality
- 📊 **Multiple OFI Metrics**: Simple OFI, Order Imbalance Ratio (OIR), Cumulative OFI, and Dollar-weighted OFI
- ⏱️ **Flexible Time Windows**: Calendar-based, rolling, or tick-based aggregation
- 📈 **Built-in Visualizations**: Time-series plots and distribution analysis
- 🎲 **Synthetic Data Generation**: Create realistic order flow for testing and education
- 🔧 **Data Standardization**: Handle various trade data formats seamlessly

### Advanced Features (NEW in v0.1.0)
- 📉 **Advanced OFI Calculations**: EWMA smoothing, momentum, and acceleration metrics
- ✅ **Data Quality Tools**: Comprehensive validation, outlier detection, and automated cleaning
- 📊 **Statistical Testing**: Autocorrelation tests, lead-lag analysis, and bootstrap significance testing
- 🔬 **Market Microstructure Models**: Kyle's lambda, price impact decomposition, VPIN, and spread analysis
- 📁 **Data Import**: LOBSTER academic format, generic CSV, flexible column mapping, and validation

### Learning Resources
- 📚 **Comprehensive Vignettes**: Step-by-step guides from beginner to advanced
- 💡 **Detailed Examples**: Every function includes working examples
- ✨ **User-Friendly**: Clear error messages and helpful validation feedback

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("DennisPoniros/rOFI_package")
```

## Quick Start

```r
library(rOFI)

# Generate sample trade data
trades <- simulate_orders(
  n = 10000,           # Number of trades
  lambda = 5,          # Trades per minute
  imb = 0.1,          # Slight buy bias
  seed = 123          # For reproducibility
)

# Compute OFI metrics with 1-minute windows
ofi <- compute_ofi(trades, window = "1 min")

# Visualize the results
plot_ofi(ofi)
```

## Loading Your Own Data

```r
# From CSV files
trades <- read_trade_csv(
  "my_trades.csv",
  time_col = "timestamp",
  side_col = "side",
  size_col = "volume",
  price_col = "price"
)

# From LOBSTER academic format
trades <- read_lobster_trades(
  "AAPL_2024-01-15_message.csv",
  date = "2024-01-15"
)

# Preview before loading
preview_trade_file("my_data.csv")  # See structure first

# Validate your data
validation <- validate_trade_data(trades)
print(validation)
```

See the **"Data Preparation"** vignette for detailed guides on loading data from various sources.

## Core Functions

### `compute_ofi()` - Calculate OFI Metrics

```r
# Calendar windows (most common)
ofi_1min <- compute_ofi(trades, window = "1 min")
ofi_5min <- compute_ofi(trades, window = "5 min")

# Rolling windows
ofi_rolling <- compute_ofi(trades, rolling = lubridate::dseconds(60))

# Tick-based windows (every N trades)
ofi_ticks <- compute_ofi(trades, n_ticks = 100)

# Price-weighted OFI
ofi_dollar <- compute_ofi(trades, window = "1 min", price_weighted = TRUE)
```

### `plot_ofi()` - Visualize Patterns

```r
# Time series of multiple metrics
plot_ofi(ofi, which = c("ofi", "oir", "ofi_cum"))

# Single metric with custom styling
plot_ofi(ofi, 
         which = "oir",
         ref_line = 0,
         title = "Order Imbalance Ratio",
         subtitle = "5-minute windows")

# Distribution analysis
plot_ofi_dist(ofi, metric = "oir", plot_type = "histogram")
```

### `simulate_orders()` - Generate Synthetic Data

```r
# Balanced market
balanced <- simulate_orders(n = 1000, imb = 0)

# Bullish market with upward drift
bullish <- simulate_orders(
  n = 1000,
  imb = 0.3,          # 30% more buys than sells
  drift = 0.5,        # Positive price drift
  vol = 0.2           # Lower volatility
)

# High-frequency scenario
hft <- simulate_orders(
  n = 5000,
  lambda = 100,       # 100 trades per minute
  vol = 0.05          # Very low volatility
)
```

### `as_ofi()` - Standardize Your Data

```r
# Handle different column names
my_data <- data.frame(
  time = Sys.time() + 1:100,
  direction = sample(c(1, -1), 100, replace = TRUE),  # Numeric encoding
  volume = runif(100, 100, 1000),
  px = 100 + cumsum(rnorm(100, 0, 0.1))
)

clean_data <- as_ofi(
  my_data,
  time_col = "time",
  side_col = "direction",
  size_col = "volume",
  price_col = "px"
)
```

## Advanced Features

### Advanced OFI Calculations

```r
# Exponentially weighted moving average (EWMA) for smoothing
ofi_smooth <- compute_ewma_ofi(ofi, lambda = 0.94)

# Calculate momentum (rate of change)
ofi_with_momentum <- compute_ofi_momentum(ofi, lookback = 5)

# Calculate acceleration (second derivative)
ofi_with_accel <- compute_ofi_acceleration(ofi, lookback = 5)

# Chain multiple transformations
ofi_advanced <- ofi |>
  compute_ewma_ofi(lambda = 0.9) |>
  compute_ofi_momentum(lookback = 3) |>
  compute_ofi_acceleration(lookback = 3)
```

### Data Quality & Validation

```r
# Validate your trade data before analysis
validation <- validate_trade_data(trades)
print(validation)

# Automatically clean problematic data
trades_clean <- clean_trade_data(
  trades,
  remove_outliers = TRUE,
  remove_duplicates = TRUE
)

# Detect outliers with multiple methods
outliers <- detect_outliers_ofi(trades, method = "iqr", threshold = 3)
print(paste(outliers$n_outliers, "outliers detected"))
```

### Statistical Testing

```r
# Test for autocorrelation in OFI
acf_test <- test_ofi_autocorrelation(ofi, max_lag = 20)
print(acf_test)

# Analyze lead-lag relationship with prices
leadlag <- ofi_lead_lag_analysis(trades, window = "1 min")
print(leadlag$interpretation)

# Bootstrap significance test
boot_test <- bootstrap_ofi_significance(ofi, n_bootstrap = 1000)
print(paste("P-value:", boot_test$p_value))
```

### Market Microstructure Models

```r
# Kyle's Lambda - measure of price impact
kyle <- kyle_lambda_estimation(trades, window = "1 min")
print(kyle)

# Decompose price impact into temporary and permanent components
impact <- estimate_price_impact(trades, window = "1 min", decay_periods = 5)
print(paste("Permanent impact:", impact$permanent_impact))
print(paste("Temporary impact:", impact$temporary_impact))

# VPIN - Volume-synchronized probability of informed trading
vpin_data <- compute_vpin(trades, n_buckets = 50, lookback = 50)
toxic_periods <- vpin_data[vpin_data$vpin > 0.7, ]  # High toxicity
print(paste(nrow(toxic_periods), "toxic periods detected"))

# Effective spread analysis
spreads <- compute_effective_spread(trades, midpoint_method = "rolling")
print(paste("Mean effective spread:", mean(spreads$effective_spread)))

# Spread decomposition into adverse selection and realized spread
decomp <- decompose_spread(trades, horizon = 5)
print(paste("Adverse selection %:", decomp$pct_adverse_selection))
```

## Understanding OFI Metrics

The package computes several complementary metrics:

| Metric | Formula | Interpretation |
|--------|---------|----------------|
| **OFI** | Buy Volume - Sell Volume | Net order flow; positive = buy pressure |
| **OIR** | (B - S)/(B + S + ε) | Normalized imbalance; ranges [-1, 1] |
| **Cumulative OFI** | Σ OFI_t | Running total; shows trend |
| **Dollar OFI** | Σ (price × size × side) | Value-weighted flow |

## Example Workflow

```r
library(rOFI)
library(dplyr)
library(lubridate)

# 1. Load or generate data
trades <- simulate_orders(
  n = 10000,
  start = as.POSIXct("2024-01-15 09:30:00", tz = "America/New_York"),
  lambda = 10,
  imb = 0.05,
  seed = 42
)

# 2. Compute OFI at multiple time scales
ofi_1min <- compute_ofi(trades, window = "1 min")
ofi_5min <- compute_ofi(trades, window = "5 min")
ofi_15min <- compute_ofi(trades, window = "15 min")

# 3. Analyze the first hour
first_hour <- ofi_1min %>%
  filter(window_start < min(window_start) + hours(1))

# 4. Find periods of high imbalance
high_imbalance <- ofi_5min %>%
  filter(abs(oir) > 0.3) %>%
  arrange(desc(abs(oir)))

# 5. Visualize patterns
plot_ofi(ofi_5min, which = c("ofi", "oir", "ofi_cum"))

# 6. Examine distribution
plot_ofi_dist(ofi_5min, metric = "oir", plot_type = "density")
```

## Educational Use

This package is designed for teaching market microstructure concepts:

```r
# Demonstrate the effect of order imbalance
demo_balanced <- simulate_orders(n = 1000, imb = 0, seed = 1)
demo_bullish <- simulate_orders(n = 1000, imb = 0.3, seed = 1)
demo_bearish <- simulate_orders(n = 1000, imb = -0.3, seed = 1)

# Compare their OFI patterns
ofi_balanced <- compute_ofi(demo_balanced, window = "1 min")
ofi_bullish <- compute_ofi(demo_bullish, window = "1 min")
ofi_bearish <- compute_ofi(demo_bearish, window = "1 min")

# Visualize side-by-side
library(patchwork)
p1 <- plot_ofi(ofi_balanced, "oir", title = "Balanced Market")
p2 <- plot_ofi(ofi_bullish, "oir", title = "Bullish Market")
p3 <- plot_ofi(ofi_bearish, "oir", title = "Bearish Market")
p1 / p2 / p3
```

## Input Data Requirements

Your trade data should have:

- **timestamp**: POSIXct timestamps (timezone-aware recommended)
- **side**: Trade direction ("B"/"S", "buy"/"sell", or 1/-1)
- **size**: Trade size/volume (positive numeric)
- **price**: Execution price (optional, needed for dollar-weighted OFI)

The `as_ofi()` function handles various formats and encodings automatically.

## Performance Notes

- For large datasets (>1M trades), consider using larger time windows
- Rolling windows are computationally intensive; use sparingly on big data
- The package uses `dplyr` for efficient grouped operations
- Parallel processing can be added for very large datasets using `furrr`

## Learning Resources

`rOFI` includes comprehensive documentation to support your learning journey:

### Vignettes (Recommended Reading Order)

1. **Introduction to OFI** (`vignette("introduction-to-ofi", package = "rOFI")`)
   - Perfect for beginners
   - What is Order-Flow Imbalance?
   - Your first OFI analysis
   - Understanding the metrics
   - Common use cases

2. **Data Preparation Guide** (`vignette("data-preparation", package = "rOFI")`)
   - Loading data from various sources
   - LOBSTER academic format
   - Generic CSV files
   - Data validation and cleaning
   - Troubleshooting common issues

3. **Getting Started** (`vignette("getting-started", package = "rOFI")`)
   - Complete workflows
   - Advanced features
   - Market regime detection
   - Performance considerations

### Function Documentation

Every function has detailed documentation with examples:

```r
?compute_ofi              # Core OFI calculation
?read_trade_csv           # Data import
?validate_trade_data      # Data quality
?kyle_lambda_estimation   # Market microstructure
?test_ofi_autocorrelation # Statistical tests
```

### Quick Reference

```r
# See all available functions
library(help = "rOFI")

# Package overview
?rOFI

# List all vignettes
browseVignettes("rOFI")
```

### Getting Help

- 📖 Read the vignettes (start with "introduction-to-ofi")
- 💬 Check function documentation with `?function_name`
- 🐛 Report issues: https://github.com/DennisPoniros/rOFI_package/issues
- 📧 Questions: See DESCRIPTION file for maintainer contact

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Created as an Honors Option project for STAT 611
- Thanks to Dr. Teresa Gibson for mentorship
- Inspired by academic research in market microstructure

## References

- Cont, R., Kukanov, A., & Stoikov, S. (2014). The price impact of order book events. *Journal of Financial Econometrics*, 12(1), 47-88.
- Cartea, Á., Jaimungal, S., & Penalva, J. (2015). *Algorithmic and High-Frequency Trading*. Cambridge University Press.

## Contact

Dionysios Poniros - [your.email@example.com](mailto:your.email@example.com)

Project Link: [https://github.com/DennisPoniros/rOFI_package](https://github.com/DennisPoniros/rOFI_package)
