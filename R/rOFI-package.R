#' rOFI: The Comprehensive Market Microstructure Toolkit for R
#'
#' @description
#' rOFI is the world's first comprehensive market microstructure toolkit built
#' entirely in R, designed for both educational research and professional
#' quantitative trading. From basic Order-Flow Imbalance (OFI) calculations to
#' advanced surveillance algorithms and machine learning features, rOFI provides
#' everything you need to analyze, visualize, and model market microstructure
#' dynamics.
#'
#' \strong{Complete Feature Set (7 Major Modules):}
#'
#' \itemize{
#'   \item \strong{Advanced Visualization Dashboard}: Publication-quality plots,
#'         diagnostics, regime detection, STL decomposition
#'   \item \strong{Price Impact Models}: Almgren-Chriss, square-root law,
#'         Obizhaeva-Wang, TCA metrics
#'   \item \strong{Production Data Pipelines}: NYSE TAQ, NASDAQ ITCH, LOBSTER,
#'         multi-venue consolidation, trade classification
#'   \item \strong{Regulatory Surveillance}: Spoofing, layering, quote stuffing
#'         detection, alert systems, compliance reporting
#'   \item \strong{Multi-Level Order Book}: Multi-level OFI, slope, curvature,
#'         microprice, resilience metrics
#'   \item \strong{Cross-Market Analysis}: Lead-lag analysis, price discovery,
#'         arbitrage detection, spillover analysis
#'   \item \strong{ML Feature Engineering}: 100+ features, event bars,
#'         stationarization, proper train/test splitting
#' }
#'
#' \strong{Plus Core Functionality:}
#'
#' \itemize{
#'   \item Computing OFI metrics (simple, EWMA, VWAP-weighted, momentum)
#'   \item Loading data from various sources (CSV, LOBSTER academic format)
#'   \item Statistical testing (autocorrelation, lead-lag, bootstrap)
#'   \item Market microstructure analysis (Kyle's lambda, VPIN, spread decomposition)
#'   \item Data quality validation and cleaning
#' }
#'
#' @section Quick Start Guide:
#'
#' **New to OFI? Start here:**
#'
#' \preformatted{
#' library(rOFI)
#'
#' # Step 1: Generate sample data (or load your own)
#' trades <- simulate_orders(n = 1000, seed = 123)
#'
#' # Step 2: Compute OFI metrics
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Step 3: Visualize results
#' plot_ofi(ofi)
#'
#' # Step 4: Analyze the results
#' summary(ofi$oir)  # Order Imbalance Ratio
#'
#' # Get interactive help
#' rofi_help()       # Browse all available help topics
#' rofi_examples()   # See worked examples
#' }
#'
#' @section Learning Path:
#'
#' **Choose your path based on your goal:**
#'
#' \strong{Path 1: I'm new to OFI and want to understand the basics}
#' \preformatted{
#' # Read the introduction vignette
#' vignette("introduction-to-ofi", package = "rOFI")
#'
#' # Try the interactive tutorial
#' rofi_tutorial()
#'
#' # Explore examples
#' ?compute_ofi
#' ?plot_ofi
#' }
#'
#' \strong{Path 2: I have trade data and want to analyze it}
#' \preformatted{
#' # Learn about data requirements and loading
#' vignette("data-preparation", package = "rOFI")
#'
#' # Preview your data file
#' preview_trade_file("your_data.csv")
#'
#' # Load and validate
#' trades <- read_trade_csv("your_data.csv",
#'                          time_col = 1, side_col = 2,
#'                          size_col = 3, price_col = 4)
#' validate_trade_data(trades)
#'
#' # Compute OFI
#' ofi <- compute_ofi(trades, window = "1 min")
#' }
#'
#' \strong{Path 3: I want to do statistical analysis}
#' \preformatted{
#' # Compute OFI first
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Test for autocorrelation
#' test_ofi_autocorrelation(ofi)
#'
#' # Analyze lead-lag relationship with prices
#' ofi_lead_lag_analysis(ofi, trades$price)
#'
#' # Bootstrap significance test
#' bootstrap_ofi_significance(ofi, n_bootstrap = 1000)
#' }
#'
#' \strong{Path 4: I want advanced microstructure analysis}
#' \preformatted{
#' # Kyle's lambda (price impact)
#' kyle <- kyle_lambda_estimation(trades)
#'
#' # VPIN (probability of informed trading)
#' vpin <- compute_vpin(trades, n_buckets = 50)
#'
#' # Spread decomposition
#' spread <- decompose_spread(trades)
#'
#' # See all microstructure functions
#' help(package = "rOFI", topic = "Microstructure")
#' }
#'
#' @section All Functions by Category:
#'
#' \strong{ADVANCED VISUALIZATION DASHBOARD (NEW)}
#' \itemize{
#'   \item \code{\link{plot_ofi_diagnostics}}: 6-panel diagnostic dashboard (ACF, PACF, Q-Q, distribution)
#'   \item \code{\link{plot_ofi_decomposition}}: STL decomposition (trend/seasonal/irregular)
#'   \item \code{\link{plot_market_quality}}: 4-panel market quality dashboard
#'   \item \code{\link{plot_regime_detection}}: Market state identification via clustering
#'   \item \code{\link{plot_comparative_analysis}}: Multi-asset/multi-period comparison
#'   \item \code{\link{theme_publication}}: Professional publication theme
#' }
#'
#' \strong{PRICE IMPACT MODELS (NEW)}
#' \itemize{
#'   \item \code{\link{almgren_chriss_trajectory}}: Optimal execution with risk aversion
#'   \item \code{\link{sqrt_impact}}: Universal square-root law
#'   \item \code{\link{calibrate_sqrt_law}}: Fit Y parameter from executions
#'   \item \code{\link{decompose_price_impact}}: Temporary vs permanent impact
#'   \item \code{\link{implementation_shortfall}}: Implementation shortfall (TCA)
#'   \item \code{\link{obizhaeva_wang_impact}}: Propagator model
#'   \item \code{\link{predict_execution_cost}}: Model comparison
#' }
#'
#' \strong{PRODUCTION DATA PIPELINES (NEW)}
#' \itemize{
#'   \item \code{\link{read_taq_trades}}: NYSE TAQ format parser
#'   \item \code{\link{read_itch_messages}}: NASDAQ ITCH message parser
#'   \item \code{\link{reconstruct_orderbook}}: Rebuild LOB from messages
#'   \item \code{\link{classify_trades}}: Lee-Ready, EMO, tick rule
#'   \item \code{\link{consolidate_venues}}: Multi-venue consolidation
#'   \item \code{\link{validate_tick_data}}: Comprehensive validation
#' }
#'
#' \strong{REGULATORY SURVEILLANCE (NEW)}
#' \itemize{
#'   \item \code{\link{detect_spoofing}}: Fake order detection
#'   \item \code{\link{detect_layering}}: Multi-level manipulation
#'   \item \code{\link{detect_quote_stuffing}}: Message velocity detection
#'   \item \code{\link{compute_order_to_trade_ratio}}: OTR monitoring
#'   \item \code{\link{analyze_cancellation_patterns}}: Cancel behavior analysis
#'   \item \code{\link{surveillance_alert_system}}: Multi-algorithm alerts
#'   \item \code{\link{market_manipulation_report}}: Compliance reporting
#' }
#'
#' \strong{MULTI-LEVEL ORDER BOOK (NEW)}
#' \itemize{
#'   \item \code{\link{compute_multilevel_ofi}}: OFI across levels with weighting
#'   \item \code{\link{orderbook_slope}}: Depth decay rate
#'   \item \code{\link{orderbook_curvature}}: Second derivative
#'   \item \code{\link{volume_distribution_levels}}: Depth profiles
#'   \item \code{\link{bid_ask_pressure}}: Asymmetric depth
#'   \item \code{\link{depth_imbalance}}: Multi-level imbalance
#'   \item \code{\link{microprice}}: Volume-weighted mid
#'   \item \code{\link{order_book_resilience}}: Replenishment speed
#' }
#'
#' \strong{CROSS-MARKET ANALYSIS (NEW)}
#' \itemize{
#'   \item \code{\link{compute_cross_asset_ofi}}: Multi-instrument OFI matrix
#'   \item \code{\link{lead_lag_analysis}}: Price discovery via CCF
#'   \item \code{\link{cross_impact_matrix}}: Cross-asset impact
#'   \item \code{\link{lagged_cross_correlation}}: Time-lagged relationships
#'   \item \code{\link{price_discovery_metrics}}: Information shares
#'   \item \code{\link{pca_orderflow}}: Common factors extraction
#'   \item \code{\link{spillover_analysis}}: Shock transmission
#'   \item \code{\link{arbitrage_opportunities}}: Cross-market pricing
#'   \item \code{\link{etf_arbitrage_metrics}}: ETF vs NAV
#' }
#'
#' \strong{MACHINE LEARNING FEATURES (NEW)}
#' \itemize{
#'   \item \code{\link{engineer_ofi_features}}: 100+ feature generation
#'   \item \code{\link{create_event_bars}}: Tick/volume/dollar bars
#'   \item \code{\link{stationarize_ofi}}: Transformations for modeling
#'   \item \code{\link{create_ml_dataset}}: Proper train/test splits
#'   \item \code{\link{create_prediction_targets}}: Response variables
#' }
#'
#' \strong{Data Loading & Preparation}
#' \itemize{
#'   \item \code{\link{read_trade_csv}}: Load data from CSV files
#'   \item \code{\link{read_lobster_trades}}: Load LOBSTER academic format
#'   \item \code{\link{preview_trade_file}}: Preview file before loading
#'   \item \code{\link{as_ofi}}: Standardize trade data format
#'   \item \code{\link{simulate_orders}}: Generate synthetic data
#'   \item \code{\link{generate_demo_data}}: Generate demo dataset
#' }
#'
#' \strong{Data Quality & Validation}
#' \itemize{
#'   \item \code{\link{validate_trade_data}}: Comprehensive data validation
#'   \item \code{\link{clean_trade_data}}: Automated data cleaning
#'   \item \code{\link{detect_outliers_ofi}}: Outlier detection
#' }
#'
#' \strong{Core OFI Calculations}
#' \itemize{
#'   \item \code{\link{compute_ofi}}: Standard OFI calculation
#'   \item \code{\link{compute_ewma_ofi}}: Exponentially weighted OFI
#'   \item \code{\link{compute_vwap_ofi}}: Volume-weighted average price OFI
#'   \item \code{\link{compute_ofi_momentum}}: OFI rate of change
#'   \item \code{\link{compute_ofi_acceleration}}: OFI second derivative
#' }
#'
#' \strong{Statistical Testing}
#' \itemize{
#'   \item \code{\link{test_ofi_autocorrelation}}: Serial correlation tests
#'   \item \code{\link{ofi_lead_lag_analysis}}: Cross-correlation with prices
#'   \item \code{\link{bootstrap_ofi_significance}}: Bootstrap hypothesis tests
#' }
#'
#' \strong{Market Microstructure (Core)}
#' \itemize{
#'   \item \code{\link{kyle_lambda_estimation}}: Kyle's lambda (price impact)
#'   \item \code{\link{estimate_price_impact}}: Temporary/permanent impact
#'   \item \code{\link{compute_vpin}}: Volume-synchronized PIN
#'   \item \code{\link{compute_effective_spread}}: Transaction costs
#'   \item \code{\link{decompose_spread}}: Spread component analysis
#' }
#'
#' \strong{Visualization (Core)}
#' \itemize{
#'   \item \code{\link{plot_ofi}}: Time series plots of OFI metrics
#'   \item \code{\link{plot_ofi_dist}}: Distribution plots
#' }
#'
#' \strong{Interactive Help}
#' \itemize{
#'   \item \code{\link{rofi_help}}: Interactive help menu
#'   \item \code{\link{rofi_examples}}: Browse worked examples
#'   \item \code{\link{rofi_tutorial}}: Step-by-step tutorial
#'   \item \code{\link{rofi_cheatsheet}}: Quick reference guide
#' }
#'
#' @section Common Tasks:
#'
#' \strong{Task: Load my CSV file}
#' \preformatted{
#' # First, preview the file
#' preview_trade_file("mydata.csv")
#'
#' # Then load it (adjust column names/numbers as needed)
#' trades <- read_trade_csv("mydata.csv",
#'                          time_col = "timestamp",
#'                          side_col = "side",
#'                          size_col = "volume",
#'                          price_col = "price")
#'
#' # Validate the data
#' validate_trade_data(trades)
#' }
#'
#' \strong{Task: Calculate OFI at different time scales}
#' \preformatted{
#' # 30 second windows
#' ofi_30s <- compute_ofi(trades, window = "30 sec")
#'
#' # 1 minute windows
#' ofi_1m <- compute_ofi(trades, window = "1 min")
#'
#' # 5 minute windows
#' ofi_5m <- compute_ofi(trades, window = "5 min")
#'
#' # Every 100 trades (tick-based)
#' ofi_tick <- compute_ofi(trades, n_ticks = 100)
#' }
#'
#' \strong{Task: Test if OFI predicts price changes}
#' \preformatted{
#' # First compute OFI
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Extract price changes (you need to compute this from your data)
#' prices <- trades$price[seq(1, nrow(trades), by = 100)]
#' price_changes <- diff(prices)
#'
#' # Lead-lag analysis
#' result <- ofi_lead_lag_analysis(ofi, price_changes)
#' print(result)
#'
#' # Check if OFI leads prices (positive lag with high correlation)
#' }
#'
#' \strong{Task: Detect periods of high buying/selling pressure}
#' \preformatted{
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Find strong buying (OIR > 0.3)
#' strong_buy <- ofi[ofi$oir > 0.3, ]
#'
#' # Find strong selling (OIR < -0.3)
#' strong_sell <- ofi[ofi$oir < -0.3, ]
#'
#' # Visualize
#' plot_ofi(ofi, which = "oir")
#' }
#'
#' \strong{Task: Clean problematic data}
#' \preformatted{
#' # Validate first to see issues
#' validation <- validate_trade_data(trades)
#' print(validation)
#'
#' # Clean automatically
#' trades_clean <- clean_trade_data(trades,
#'                                  remove_outliers = TRUE,
#'                                  remove_duplicates = TRUE)
#'
#' # Check cleaning report
#' attr(trades_clean, "cleaning_report")
#' }
#'
#' @section Data Requirements:
#'
#' Your data must have these minimum columns:
#' \itemize{
#'   \item \strong{timestamp}: When the trade occurred (POSIXct or numeric)
#'   \item \strong{side}: Buy or Sell ("B"/"S", "BUY"/"SELL", 1/-1, etc.)
#'   \item \strong{size}: Trade size in shares/contracts (numeric)
#'   \item \strong{price}: Execution price (numeric, optional for basic OFI)
#' }
#'
#' See \code{vignette("data-preparation")} for detailed requirements.
#'
#' @section Troubleshooting:
#'
#' \strong{Error: "Could not parse datetime"}
#' \preformatted{
#' # Try specifying the date explicitly
#' trades <- read_trade_csv(file, time_col = "time",
#'                          time_format = "time_only",
#'                          date = "2024-01-15")
#' }
#'
#' \strong{Error: "Unknown side value"}
#' \preformatted{
#' # Check what values you have
#' unique(your_data$side_column)
#'
#' # Provide explicit mapping
#' trades <- read_trade_csv(file,
#'                          side_mapping = c("BID" = "B", "ASK" = "S"))
#' }
#'
#' \strong{Warning: "Data validation failed"}
#' \preformatted{
#' # See what's wrong
#' validation <- validate_trade_data(trades)
#' print(validation)
#'
#' # Clean the data
#' trades <- clean_trade_data(trades)
#' }
#'
#' \strong{No plots showing}
#' \preformatted{
#' # Make sure you have computed OFI first
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Then plot
#' plot_ofi(ofi)
#' }
#'
#' \strong{Need more help?}
#' \preformatted{
#' rofi_help()  # Interactive help
#' help(package = "rOFI")  # Package index
#' vignette(package = "rOFI")  # List all vignettes
#' }
#'
#' @section Package Vignettes:
#' \itemize{
#'   \item \code{vignette("introduction-to-ofi")}: Beginner-friendly introduction
#'   \item \code{vignette("advanced-visualization")}: Publication-quality plots and diagnostics
#'   \item \code{vignette("data-preparation")}: Loading and cleaning data
#'   \item \code{vignette("getting-started")}: Complete workflows
#' }
#'
#' @section Who is rOFI for?:
#' \itemize{
#'   \item \strong{Academic Researchers}: Publication-quality visualizations,
#'         rigorous statistical tests, comprehensive documentation
#'   \item \strong{Quantitative Analysts}: Production-ready data pipelines,
#'         ML features, price impact models for strategy development
#'   \item \strong{Compliance Officers}: Regulatory surveillance algorithms
#'         (spoofing, layering, quote stuffing detection)
#'   \item \strong{Students}: Gentle learning curve with extensive vignettes,
#'         examples, and synthetic data generation
#'   \item \strong{Regulators}: Market manipulation detection,
#'         order-to-trade ratio monitoring, alert systems
#' }
#'
#' @section Getting Help:
#' For questions and bug reports:
#' \itemize{
#'   \item GitHub Issues: \url{https://github.com/DennisPoniros/rOFI_package/issues}
#'   \item Interactive help: \code{rofi_help()}
#'   \item Function help: \code{?function_name}
#'   \item Examples: \code{rofi_examples()}
#' }
#'
#' @name rOFI
#' @import dplyr
#' @import ggplot2
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate floor_date is.POSIXct with_tz seconds dseconds
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom slider slide_period_dbl slide_index_dbl
#' @importFrom stats rexp rbinom rlnorm rnorm median
#' @importFrom rlang .data := !! sym
"_PACKAGE"
