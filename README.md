# rOFI: The Comprehensive Market Microstructure Toolkit for R

<!-- badges: start -->
[![R-CMD-check](https://github.com/DennisPoniros/rOFI_package/workflows/R-CMD-check/badge.svg)](https://github.com/DennisPoniros/rOFI_package/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

**rOFI** is the world's first comprehensive market microstructure toolkit built entirely in R, designed for both educational research and professional quantitative trading. From basic Order-Flow Imbalance (OFI) calculations to advanced surveillance algorithms and machine learning features, rOFI provides everything you need to analyze, visualize, and model market microstructure dynamics.

### Who is rOFI for?

- 🎓 **Academic Researchers**: Publication-quality visualizations, rigorous statistical tests, comprehensive documentation
- 💼 **Quantitative Analysts**: Production-ready data pipelines, ML features, price impact models for strategy development
- 🔍 **Compliance Officers**: Regulatory surveillance algorithms (spoofing, layering, quote stuffing detection)
- 📊 **Students**: Gentle learning curve with extensive vignettes, examples, and synthetic data generation
- 🏛️ **Regulators**: Market manipulation detection, order-to-trade ratio monitoring, alert systems

## 🚀 What Makes rOFI Unique?

**rOFI is the only R package that combines:**

1. **Advanced Visualization** - Publication-quality dashboards that statisticians appreciate
2. **Price Impact Models** - Almgren-Chriss, square-root law, Obizhaeva-Wang propagator
3. **Production Data Pipelines** - NYSE TAQ, NASDAQ ITCH, LOBSTER, multi-venue consolidation
4. **Regulatory Surveillance** - SEC/MiFID II compliant manipulation detection
5. **Multi-Level Order Book** - Extract information from full LOB depth
6. **Cross-Market Analysis** - Lead-lag, price discovery, arbitrage detection
7. **ML Feature Engineering** - 100+ features, event bars, proper train/test splitting

**Pure R implementation** - No C++/Python dependencies, easy to install and extend.

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("DennisPoniros/rOFI_package")
```

## Quick Start

### Basic OFI Analysis (30 seconds)

```r
library(rOFI)

# Generate sample data
trades <- simulate_orders(n = 10000, imb = 0.1, seed = 123)

# Compute OFI
ofi <- compute_ofi(trades, window = "1 min")

# Visualize
plot_ofi(ofi, which = c("ofi", "oir"))
```

## 📦 Complete Feature Set

### 1️⃣ Advanced Visualization Dashboard

Publication-quality plots that statisticians will appreciate, built with ggplot2 and patchwork.

```r
library(rOFI)

# 6-panel diagnostic dashboard: ACF, PACF, Q-Q, distribution, statistics
plot_ofi_diagnostics(ofi, metric = "oir", max_lag = 20)

# STL decomposition into trend/seasonal/irregular components
plot_ofi_decomposition(ofi, metric = "oir")

# Comprehensive market quality dashboard (4 panels)
plot_market_quality(trades, window = "5 min")

# Regime detection with clustering (identify market states)
plot_regime_detection(ofi, method = "kmeans", n_regimes = 3)

# Compare multiple assets or time periods
plot_comparative_analysis(list(AAPL = ofi_aapl, MSFT = ofi_msft))

# Professional publication theme
plot_ofi(ofi) + theme_publication()
```

**Applications**: Academic papers, research presentations, market reports, teaching materials.

### 2️⃣ Price Impact Models

Industry-standard models for optimal execution and transaction cost analysis.

```r
# Almgren-Chriss optimal execution (risk-averse trajectory)
trajectory <- almgren_chriss_trajectory(
  Q = 100000,           # Total quantity
  T_horizon = 60,       # Execution window (minutes)
  lambda = 1e-6,        # Risk aversion
  sigma = 0.30,         # Volatility
  gamma = 0.1,          # Permanent impact
  eta = 0.05            # Temporary impact
)
print(trajectory)  # Optimal trading schedule

# Universal square-root law: ΔP = Y × σ × √(Q/V)
impact <- sqrt_impact(Q = 50000, V = 1000000, sigma = 0.02, Y = 0.20)

# Calibrate Y parameter from historical executions
calib <- calibrate_sqrt_law(executions_data)
print(paste("Fitted Y:", calib$Y_fitted, "R²:", calib$R_squared))

# Decompose into temporary vs permanent impact
decomp <- decompose_price_impact(trades, window = "1 min", decay_periods = 5)

# Implementation shortfall (TCA)
shortfall <- implementation_shortfall(
  execution_price = my_executions$price,
  benchmark_price = benchmark_vwap,
  side = "buy",
  quantity = my_executions$size
)
```

**Applications**: Pre-trade cost estimation, execution algorithm design, broker TCA, academic research.

### 3️⃣ Production Data Pipelines

Robust ingestion for real market data from multiple sources.

```r
# NYSE TAQ format
trades <- read_taq_trades(
  "NYSE_TAQ_20240115.csv",
  symbol = "AAPL",
  date = "2024-01-15",
  filters = list(
    regular_hours = TRUE,
    trade_conditions = c("@", "F")  # Regular trades only
  )
)

# NASDAQ ITCH (message-level data)
messages <- read_itch_messages("ITCH_20240115.csv")

# Reconstruct order book from messages
lob <- reconstruct_orderbook(messages, depth = 10, snapshot_freq = 100)
print(lob$statistics)  # Summary stats

# Classify trade direction (Lee-Ready algorithm)
trades_classified <- classify_trades(
  trades,
  quotes = quotes_data,
  method = "lee-ready"
)

# Multi-venue consolidation (NBBO construction)
consolidated <- consolidate_venues(
  list(NYSE = nyse_data, NASDAQ = nasdaq_data, BATS = bats_data)
)

# Comprehensive data validation
validation <- validate_tick_data(trades, strict = TRUE)
print(validation)  # Issues, warnings, recommendations
```

**Applications**: Production trading systems, academic research with real data, data quality monitoring.

### 4️⃣ Regulatory Surveillance

Market manipulation detection for compliance (SEC, MiFID II, MAR).

```r
# Detect spoofing (fake orders)
spoof_result <- detect_spoofing(
  messages,
  cancel_threshold = 0.70,      # 70%+ cancellation rate
  size_threshold = 0.90,         # Large orders (90th percentile)
  time_window = 60               # Within 60 seconds
)
print(spoof_result)  # Spoof score 0-100, flagged instances

# Detect layering (multi-level coordination)
layer_result <- detect_layering(
  messages,
  n_levels = 5,
  coordination_threshold = 0.70,
  min_layers = 3
)

# Detect quote stuffing (message velocity)
stuff_result <- detect_quote_stuffing(
  messages,
  msg_rate_threshold = 100,      # 100+ messages/second
  burst_window = 1,
  cancel_ratio_threshold = 0.80
)

# Compute order-to-trade ratio (key regulatory metric)
otr <- compute_order_to_trade_ratio(messages, window = "1 min")
high_otr <- otr[otr$ratio > 100, ]  # Flag suspicious activity

# Comprehensive alert system (multiple algorithms)
alerts <- surveillance_alert_system(
  messages,
  sensitivity = "medium",
  algorithms = c("spoofing", "layering", "stuffing", "otr")
)
critical <- alerts$alerts[alerts$alerts$severity == "Critical", ]

# Generate compliance report
report <- market_manipulation_report(
  messages,
  date = "2024-01-15",
  symbol = "AAPL",
  output_format = "pdf"
)
```

**Applications**: Compliance monitoring, regulatory reporting, surveillance systems, risk management.

### 5️⃣ Multi-Level Order Book Metrics

Extract information from full LOB depth (research shows levels 2-5 improve forecasting).

```r
# Multi-level OFI with volume weighting
ml_ofi <- compute_multilevel_ofi(
  orderbook,
  n_levels = 5,
  weighting = "volume"  # or "distance" or "equal"
)
print(ml_ofi$improvement_vs_single_level)  # % improvement

# Order book slope (depth decay rate)
slope <- orderbook_slope(orderbook, n_levels = 10, side = "both")
# Steep slope (β < -2) = fragile liquidity
# Flat slope (β near 0) = deep market

# Order book curvature (second derivative)
curve <- orderbook_curvature(orderbook, n_levels = 10)

# Volume distribution across levels
vol_dist <- volume_distribution_levels(orderbook, n_levels = 10)

# Bid-ask depth asymmetry
pressure <- bid_ask_pressure(orderbook, n_levels = 5)

# Depth imbalance across levels
imb <- depth_imbalance(orderbook, n_levels = 5)

# Microprice (volume-weighted mid, Stoikov 2018)
mp <- microprice(orderbook, n_levels = 1)

# Order book resilience (replenishment speed)
resil <- order_book_resilience(
  messages,
  event_type = "large_trade",
  recovery_window = 30
)
```

**Applications**: High-frequency trading, market making, predictive modeling, liquidity analysis.

### 6️⃣ Cross-Market Analysis

Analyze interconnected markets, price discovery, and arbitrage.

```r
# Cross-asset OFI matrix
cross_ofi <- compute_cross_asset_ofi(
  list(AAPL = aapl_trades, MSFT = msft_trades, GOOGL = googl_trades),
  window = "1 min"
)
print(cross_ofi$correlation_matrix)

# Lead-lag analysis (price discovery)
leadlag <- lead_lag_analysis(
  ofi_data_1 = aapl_ofi,
  ofi_data_2 = spy_etf_ofi,
  max_lag = 20
)
# Positive lag: instrument 1 leads
# Negative lag: instrument 2 leads

# Cross-impact matrix (how asset A impacts asset B)
impact_matrix <- cross_impact_matrix(
  list(AAPL = aapl_ofi, MSFT = msft_ofi)
)

# Price discovery metrics (information shares)
discovery <- price_discovery_metrics(
  list(NYSE = nyse_prices, NASDAQ = nasdaq_prices, BATS = bats_prices),
  method = "hasbrouck"
)
print(discovery$information_shares)  # Venue contribution

# PCA of order flow (common factors)
pca_result <- pca_orderflow(
  list(AAPL = aapl_ofi, MSFT = msft_ofi, GOOGL = googl_ofi)
)
print(pca_result$variance_explained)

# Spillover analysis (shock transmission)
spillover <- spillover_analysis(ofi_matrix, max_lag = 5)

# ETF arbitrage detection
arb <- etf_arbitrage_metrics(
  etf_price = spy_prices,
  nav = sp500_nav,
  threshold = 0.1  # 10 bps
)
profitable <- arb[abs(arb$basis_bps) > arb$threshold, ]
```

**Applications**: Statistical arbitrage, market making, price discovery research, risk management.

### 7️⃣ Machine Learning Features

Production-ready feature engineering for predictive modeling.

```r
# Engineer 100+ features from order flow
features <- engineer_ofi_features(
  trades,
  orderbook = lob,
  lookback = 10,           # Lagged features
  n_levels = 5,            # Multi-level metrics
  include_crosses = TRUE   # Interaction terms
)
# Features: multi-level OFI, rolling stats (mean, SD, skew, kurtosis),
# lags (t-1 to t-k), spread/depth, intensity, time-of-day, interactions

# Event bars (superior to time bars for ML)
tick_bars <- create_event_bars(trades, bar_type = "tick", bar_size = 100)
vol_bars <- create_event_bars(trades, bar_type = "volume", bar_size = 10000)
dollar_bars <- create_event_bars(trades, bar_type = "dollar", bar_size = 1e6)

# Stationarize for modeling
stationary <- stationarize_ofi(
  features,
  method = "standardize"  # or "difference", "rank", "log", "winsorize"
)

# Create proper train/test split (temporal, no shuffle)
dataset <- create_ml_dataset(
  features,
  target_col = "future_return_5min",
  train_fraction = 0.70,
  validation_fraction = 0.15,
  scale = TRUE  # Scale using train statistics only
)

# Create prediction targets
targets <- create_prediction_targets(
  trades,
  target_type = "return",  # or "direction", "volatility"
  horizon = 5,             # 5-minute forward
  log_returns = TRUE
)
```

**Applications**: Predictive modeling, strategy backtesting, deep learning (DeepLOB), reinforcement learning.

## 📚 Core Functionality (Original Features)

### Basic OFI Calculation

```r
# Calendar windows (most common)
ofi_1min <- compute_ofi(trades, window = "1 min")
ofi_5min <- compute_ofi(trades, window = "5 min")

# Rolling windows
ofi_rolling <- compute_ofi(trades, rolling = lubridate::dseconds(60))

# Tick-based windows (every N trades)
ofi_ticks <- compute_ofi(trades, n_ticks = 100)

# Price-weighted (dollar OFI)
ofi_dollar <- compute_ofi(trades, window = "1 min", price_weighted = TRUE)
```

### Advanced OFI Metrics

```r
# Exponentially weighted moving average
ofi_ewma <- compute_ewma_ofi(ofi, lambda = 0.94)

# Momentum (rate of change)
ofi_momentum <- compute_ofi_momentum(ofi, lookback = 5)

# Acceleration (second derivative)
ofi_accel <- compute_ofi_acceleration(ofi, lookback = 5)
```

### Statistical Testing

```r
# Autocorrelation test
acf_test <- test_ofi_autocorrelation(ofi, max_lag = 20)

# Lead-lag with prices
leadlag <- ofi_lead_lag_analysis(trades, window = "1 min")

# Bootstrap significance
boot_test <- bootstrap_ofi_significance(ofi, n_bootstrap = 1000)
```

### Market Microstructure Models

```r
# Kyle's lambda (price impact)
kyle <- kyle_lambda_estimation(trades, window = "1 min")

# VPIN (probability of informed trading)
vpin <- compute_vpin(trades, n_buckets = 50, lookback = 50)

# Spread decomposition
spread <- decompose_spread(trades, horizon = 5)
```

### Data Loading & Validation

```r
# Load from CSV
trades <- read_trade_csv(
  "trades.csv",
  time_col = "timestamp",
  side_col = "side",
  size_col = "volume",
  price_col = "price"
)

# Load LOBSTER format
trades <- read_lobster_trades("AAPL_2024-01-15_message.csv", date = "2024-01-15")

# Validate data quality
validation <- validate_trade_data(trades)

# Clean problematic data
clean_trades <- clean_trade_data(trades, remove_outliers = TRUE)
```

## 📊 Understanding OFI Metrics

| Metric | Formula | Interpretation | Range |
|--------|---------|----------------|-------|
| **OFI** | Buy Volume - Sell Volume | Net order flow | (-∞, +∞) |
| **OIR** | (B - S)/(B + S) | Normalized imbalance | [-1, +1] |
| **Cumulative OFI** | Σ OFI_t | Running total, shows trend | (-∞, +∞) |
| **Dollar OFI** | Σ (price × size × side) | Value-weighted flow | (-∞, +∞) |

## 🎓 Learning Resources

### Vignettes (Recommended Order)

1. **Introduction to OFI** - `vignette("introduction-to-ofi")`
   - Perfect for beginners
   - What is Order-Flow Imbalance?
   - Your first OFI analysis

2. **Advanced Visualization** - `vignette("advanced-visualization")`
   - Publication-quality plots
   - Statistical diagnostics
   - Regime detection

3. **Data Preparation** - `vignette("data-preparation")`
   - Loading data from various sources
   - Validation and cleaning
   - Troubleshooting

4. **Getting Started** - `vignette("getting-started")`
   - Complete workflows
   - Advanced features
   - Performance tips

### Quick Reference

```r
# Interactive help
rofi_help()

# Browse examples
rofi_examples()

# Package overview
?rOFI

# See all functions
library(help = "rOFI")

# List vignettes
browseVignettes("rOFI")
```

## 💡 Example Workflows

### Academic Research Workflow

```r
library(rOFI)

# 1. Load real data
trades <- read_lobster_trades("AAPL_2024-01-15_message.csv", date = "2024-01-15")

# 2. Validate data quality
validation <- validate_trade_data(trades)

# 3. Compute multi-level OFI
ofi <- compute_ofi(trades, window = "1 min")
ml_ofi <- compute_multilevel_ofi(orderbook, n_levels = 5)

# 4. Statistical analysis
acf_test <- test_ofi_autocorrelation(ofi)
leadlag <- lead_lag_analysis(ofi_spy, ofi_aapl)

# 5. Publication-quality visualization
plot_ofi_diagnostics(ofi) + theme_publication()
ggsave("figure1_ofi_diagnostics.pdf", width = 8, height = 10)
```

### Quantitative Strategy Development

```r
# 1. Load production data
trades <- read_taq_trades("TAQ_20240115.csv", symbol = "AAPL")

# 2. Engineer features
features <- engineer_ofi_features(trades, lookback = 10, n_levels = 5)

# 3. Create event bars
vol_bars <- create_event_bars(trades, bar_type = "volume", bar_size = 10000)

# 4. Price impact analysis
impact <- almgren_chriss_trajectory(Q = 100000, T_horizon = 60)

# 5. Backtest with proper train/test
dataset <- create_ml_dataset(features, train_fraction = 0.70)
```

### Compliance Monitoring

```r
# 1. Load message data
messages <- read_itch_messages("ITCH_20240115.csv")

# 2. Run surveillance
alerts <- surveillance_alert_system(
  messages,
  sensitivity = "high",
  algorithms = c("spoofing", "layering", "stuffing")
)

# 3. Generate report
report <- market_manipulation_report(messages, date = "2024-01-15")

# 4. Review critical alerts
critical <- alerts$alerts[alerts$alerts$severity == "Critical", ]
```

## 🔬 Package Statistics

- **~11,000 lines** of production R code
- **80+ functions** across 7 major modules
- **180+ unit tests** with comprehensive coverage
- **Pure R** - no external dependencies (C++/Python)
- **CRAN-ready** - passes R CMD check
- **Comprehensive documentation** - every function has examples and references

## 🆚 Comparison with Other Packages

| Feature | rOFI | orderbook | highfrequency | microstructure |
|---------|------|-----------|---------------|----------------|
| Basic OFI | ✅ | ❌ | Limited | ❌ |
| Multi-level OFI | ✅ | ❌ | ❌ | ❌ |
| Price Impact Models | ✅ | ❌ | Basic | ❌ |
| Data Pipelines (TAQ/ITCH) | ✅ | ❌ | Limited | ❌ |
| Surveillance Algorithms | ✅ | ❌ | ❌ | ❌ |
| Cross-Market Analysis | ✅ | ❌ | ❌ | ❌ |
| ML Features | ✅ | ❌ | ❌ | ❌ |
| Publication Plots | ✅ | Basic | Basic | ❌ |
| Pure R | ✅ | ✅ | ❌ (C++) | ✅ |
| Active Development | ✅ | ❌ | ⚠️ | ❌ |

**rOFI is the only comprehensive solution in R.**

## 📖 Key References

### Order Flow & OFI
- Cont, R., Kukanov, A., & Stoikov, S. (2014). The price impact of order book events. *Journal of Financial Econometrics*, 12(1), 47-88.
- Cartea, Á., Jaimungal, S., & Penalva, J. (2015). *Algorithmic and High-Frequency Trading*. Cambridge University Press.

### Price Impact
- Almgren, R., & Chriss, N. (2001). Optimal execution of portfolio transactions. *Journal of Risk*, 3, 5-40.
- Obizhaeva, A., & Wang, J. (2013). Optimal trading strategy and supply/demand dynamics. *Journal of Financial Markets*, 16(1), 1-32.

### Market Manipulation
- Scopino, G. (2015). The (Unfulfilled) Promise of Dodd-Frank Act. *Iowa Law Review*, 101, 1103-1160.
- Cumming, D., Zhan, F., & Aitken, M. (2015). High frequency trading and end-of-day manipulation. *Journal of Banking & Finance*, 59, 330-349.

### Machine Learning
- Zhang, Z., Zohren, S., & Roberts, S. (2019). DeepLOB: Deep convolutional neural networks for limit order books. *IEEE Transactions on Signal Processing*, 67(11), 3001-3012.
- López de Prado, M. (2018). *Advances in Financial Machine Learning*. Wiley.

## 🤝 Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Add tests for new functionality
4. Ensure `devtools::check()` passes
5. Submit a Pull Request

For major changes, please open an issue first to discuss.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Created as an Honors Option project for STAT 611
- Thanks to Dr. Teresa Gibson for mentorship
- Inspired by decades of market microstructure research
- Built for the R community

## 📬 Contact & Support

- **Issues/Bugs**: [GitHub Issues](https://github.com/DennisPoniros/rOFI_package/issues)
- **Questions**: Open a GitHub Discussion
- **Documentation**: `?rOFI` or `rofi_help()`
- **Examples**: `rofi_examples()`

---

**Transform your market microstructure research and trading with rOFI** 🚀

*"The only comprehensive market microstructure toolkit you'll ever need in R"*
