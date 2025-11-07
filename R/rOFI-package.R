#' rOFI: Compute and Visualize Order-Flow Imbalance Patterns
#'
#' @description
#' Order-Flow Imbalance (OFI) measures the buying vs. selling pressure in
#' financial markets by analyzing trade-level data. This package provides a
#' comprehensive toolkit for:
#'
#' \itemize{
#'   \item Computing OFI metrics (simple, EWMA, VWAP-weighted, momentum)
#'   \item Loading data from various sources (CSV, LOBSTER academic format)
#'   \item Statistical testing (autocorrelation, lead-lag, bootstrap)
#'   \item Market microstructure analysis (Kyle's lambda, VPIN, spread decomposition)
#'   \item Data quality validation and cleaning
#'   \item Professional visualizations
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
#' \strong{Market Microstructure}
#' \itemize{
#'   \item \code{\link{kyle_lambda_estimation}}: Kyle's lambda (price impact)
#'   \item \code{\link{estimate_price_impact}}: Temporary/permanent impact
#'   \item \code{\link{compute_vpin}}: Volume-synchronized PIN
#'   \item \code{\link{compute_effective_spread}}: Transaction costs
#'   \item \code{\link{decompose_spread}}: Spread component analysis
#' }
#'
#' \strong{Visualization}
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
#'   \item \code{vignette("data-preparation")}: Loading and cleaning data
#'   \item \code{vignette("getting-started")}: Complete workflows
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
#' @docType package
#' @name rOFI
#' @import dplyr
#' @import ggplot2
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate floor_date is.POSIXct with_tz seconds dseconds
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom slider slide_period_dbl slide_index_dbl
#' @importFrom stats rexp rbinom rlnorm rnorm cumsum median
#' @importFrom rlang .data := !! sym
NULL

#' Sample order flow data
#'
#' A dataset containing simulated trade-level events for demonstration purposes.
#'
#' @format A tibble with 1000 rows and 4 variables:
#' \describe{
#'   \item{timestamp}{POSIXct timestamp of the trade}
#'   \item{side}{Character, "B" for buy or "S" for sell}
#'   \item{size}{Numeric, number of shares/contracts traded}
#'   \item{price}{Numeric, execution price}
#' }
#' @source Generated using \code{simulate_orders()} with seed = 42
"ofi_demo"
