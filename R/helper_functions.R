#' Interactive Help Menu
#'
#' @description
#' Opens an interactive help menu to explore rOFI package capabilities.
#' This function displays organized topics and allows you to access
#' relevant documentation without leaving R.
#'
#' @param topic Optional topic to jump to directly. If NULL (default),
#'   shows the main menu. Options: "basics", "data", "statistics",
#'   "microstructure", "visualization", "troubleshooting"
#'
#' @return Invisibly returns NULL. Prints help information to console.
#' @export
#'
#' @examples
#' \dontrun{
#' # Show main help menu
#' rofi_help()
#'
#' # Jump to specific topic
#' rofi_help("data")
#' rofi_help("statistics")
#' }
rofi_help <- function(topic = NULL) {
  if (is.null(topic)) {
    cat("\n")
    cat("=======================================================\n")
    cat("  rOFI Package - Interactive Help\n")
    cat("=======================================================\n\n")

    cat("What would you like to learn about?\n\n")

    cat("1. BASICS - Understanding OFI\n")
    cat("   rofi_help('basics')\n\n")

    cat("2. DATA - Loading and preparing your data\n")
    cat("   rofi_help('data')\n\n")

    cat("3. STATISTICS - Testing and analysis\n")
    cat("   rofi_help('statistics')\n\n")

    cat("4. MICROSTRUCTURE - Advanced market analysis\n")
    cat("   rofi_help('microstructure')\n\n")

    cat("5. VISUALIZATION - Plotting and exploration\n")
    cat("   rofi_help('visualization')\n\n")

    cat("6. TROUBLESHOOTING - Common issues\n")
    cat("   rofi_help('troubleshooting')\n\n")

    cat("Other helpful commands:\n")
    cat("  rofi_examples()    - Browse worked examples\n")
    cat("  rofi_tutorial()    - Step-by-step tutorial\n")
    cat("  rofi_cheatsheet()  - Quick reference\n")
    cat("  ?rOFI              - Full package documentation\n\n")

  } else if (topic == "basics") {
    cat("\n")
    cat("=======================================================\n")
    cat("  BASICS - Understanding OFI\n")
    cat("=======================================================\n\n")

    cat("What is OFI?\n")
    cat("  Order-Flow Imbalance measures buying vs selling pressure\n")
    cat("  in financial markets by looking at actual trades.\n\n")

    cat("Key Functions:\n")
    cat("  compute_ofi()  - Calculate OFI metrics\n")
    cat("  plot_ofi()     - Visualize results\n")
    cat("  simulate_orders() - Generate sample data\n\n")

    cat("Quick Start:\n")
    cat("  trades <- simulate_orders(n = 1000)\n")
    cat("  ofi <- compute_ofi(trades, window = '1 min')\n")
    cat("  plot_ofi(ofi)\n\n")

    cat("Learn More:\n")
    cat("  vignette('introduction-to-ofi')\n")
    cat("  ?compute_ofi\n")
    cat("  rofi_tutorial()\n\n")

  } else if (topic == "data") {
    cat("\n")
    cat("=======================================================\n")
    cat("  DATA - Loading and Preparing Your Data\n")
    cat("=======================================================\n\n")

    cat("Data Requirements:\n")
    cat("  Your data must have:\n")
    cat("    - timestamp: When the trade occurred\n")
    cat("    - side: Buy or Sell ('B'/'S', 'BUY'/'SELL', 1/-1)\n")
    cat("    - size: Trade size (shares/contracts)\n")
    cat("    - price: Execution price (optional)\n\n")

    cat("Loading Functions:\n")
    cat("  preview_trade_file()  - Preview before loading\n")
    cat("  read_trade_csv()      - Load CSV files\n")
    cat("  read_lobster_trades() - Load LOBSTER format\n")
    cat("  as_ofi()              - Standardize format\n\n")

    cat("Data Quality:\n")
    cat("  validate_trade_data() - Check for issues\n")
    cat("  clean_trade_data()    - Automated cleaning\n")
    cat("  detect_outliers_ofi() - Find outliers\n\n")

    cat("Example Workflow:\n")
    cat("  preview_trade_file('mydata.csv')\n")
    cat("  trades <- read_trade_csv('mydata.csv',\n")
    cat("                           time_col = 1,\n")
    cat("                           side_col = 2,\n")
    cat("                           size_col = 3)\n")
    cat("  validate_trade_data(trades)\n\n")

    cat("Learn More:\n")
    cat("  vignette('data-preparation')\n")
    cat("  ?read_trade_csv\n")
    cat("  ?validate_trade_data\n\n")

  } else if (topic == "statistics") {
    cat("\n")
    cat("=======================================================\n")
    cat("  STATISTICS - Testing and Analysis\n")
    cat("=======================================================\n\n")

    cat("Statistical Testing Functions:\n\n")

    cat("  test_ofi_autocorrelation()\n")
    cat("    Tests if OFI values are serially correlated\n")
    cat("    Uses Ljung-Box test\n\n")

    cat("  ofi_lead_lag_analysis()\n")
    cat("    Analyzes relationship between OFI and price changes\n")
    cat("    Determines if OFI leads or lags prices\n\n")

    cat("  bootstrap_ofi_significance()\n")
    cat("    Non-parametric hypothesis testing\n")
    cat("    Tests if OFI is significantly different from random\n\n")

    cat("Example Workflow:\n")
    cat("  ofi <- compute_ofi(trades, window = '1 min')\n")
    cat("  \n")
    cat("  # Test autocorrelation\n")
    cat("  autocorr <- test_ofi_autocorrelation(ofi)\n")
    cat("  print(autocorr)\n")
    cat("  \n")
    cat("  # Lead-lag with prices\n")
    cat("  leadlag <- ofi_lead_lag_analysis(ofi, prices)\n")
    cat("  print(leadlag)\n\n")

    cat("Learn More:\n")
    cat("  ?test_ofi_autocorrelation\n")
    cat("  ?ofi_lead_lag_analysis\n")
    cat("  ?bootstrap_ofi_significance\n\n")

  } else if (topic == "microstructure") {
    cat("\n")
    cat("=======================================================\n")
    cat("  MICROSTRUCTURE - Advanced Market Analysis\n")
    cat("=======================================================\n\n")

    cat("Market Microstructure Functions:\n\n")

    cat("  kyle_lambda_estimation()\n")
    cat("    Estimate Kyle's lambda (price impact coefficient)\n")
    cat("    Based on Kyle (1985) model\n\n")

    cat("  estimate_price_impact()\n")
    cat("    Decompose into temporary vs permanent impact\n")
    cat("    Based on Hasbrouck (1991)\n\n")

    cat("  compute_vpin()\n")
    cat("    Volume-synchronized PIN\n")
    cat("    Probability of informed trading (Easley et al. 2012)\n\n")

    cat("  decompose_spread()\n")
    cat("    Spread component analysis\n")
    cat("    Huang & Stoll (1996) decomposition\n\n")

    cat("  compute_effective_spread()\n")
    cat("    Measure transaction costs\n\n")

    cat("Example:\n")
    cat("  # Estimate price impact\n")
    cat("  kyle <- kyle_lambda_estimation(trades)\n")
    cat("  print(kyle)\n")
    cat("  \n")
    cat("  # Compute VPIN\n")
    cat("  vpin <- compute_vpin(trades, n_buckets = 50)\n")
    cat("  plot(vpin$vpin, type = 'l')\n\n")

    cat("Learn More:\n")
    cat("  ?kyle_lambda_estimation\n")
    cat("  ?compute_vpin\n")
    cat("  ?decompose_spread\n\n")

  } else if (topic == "visualization") {
    cat("\n")
    cat("=======================================================\n")
    cat("  VISUALIZATION - Plotting and Exploration\n")
    cat("=======================================================\n\n")

    cat("Plotting Functions:\n\n")

    cat("  plot_ofi()\n")
    cat("    Time series plots of OFI metrics\n")
    cat("    Options: 'ofi', 'oir', 'ofi_cum', 'volume'\n\n")

    cat("  plot_ofi_dist()\n")
    cat("    Distribution plots\n")
    cat("    Options: histogram, density, boxplot\n\n")

    cat("Examples:\n")
    cat("  # Time series of OFI\n")
    cat("  plot_ofi(ofi, which = 'ofi')\n")
    cat("  \n")
    cat("  # Cumulative OFI\n")
    cat("  plot_ofi(ofi, which = 'ofi_cum')\n")
    cat("  \n")
    cat("  # Distribution of OIR\n")
    cat("  plot_ofi_dist(ofi, metric = 'oir',\n")
    cat("                plot_type = 'histogram')\n\n")

    cat("Customization:\n")
    cat("  All plots use ggplot2, so you can add layers:\n")
    cat("  \n")
    cat("  library(ggplot2)\n")
    cat("  plot_ofi(ofi) + \n")
    cat("    theme_minimal() +\n")
    cat("    labs(title = 'My Custom Title')\n\n")

    cat("Learn More:\n")
    cat("  ?plot_ofi\n")
    cat("  ?plot_ofi_dist\n\n")

  } else if (topic == "troubleshooting") {
    cat("\n")
    cat("=======================================================\n")
    cat("  TROUBLESHOOTING - Common Issues\n")
    cat("=======================================================\n\n")

    cat("Issue: 'Could not parse datetime'\n")
    cat("  Solution: Specify time_format and date explicitly\n")
    cat("  Example:\n")
    cat("    trades <- read_trade_csv(file,\n")
    cat("                time_format = 'time_only',\n")
    cat("                date = '2024-01-15')\n\n")

    cat("Issue: 'Unknown side value'\n")
    cat("  Solution: Provide side_mapping parameter\n")
    cat("  Example:\n")
    cat("    trades <- read_trade_csv(file,\n")
    cat("                side_mapping = c('BID' = 'B', 'ASK' = 'S'))\n\n")

    cat("Issue: 'Data validation failed'\n")
    cat("  Solution: Use clean_trade_data()\n")
    cat("  Example:\n")
    cat("    validation <- validate_trade_data(trades)\n")
    cat("    print(validation)  # See what's wrong\n")
    cat("    trades <- clean_trade_data(trades)\n\n")

    cat("Issue: No plots showing\n")
    cat("  Solution: Ensure you computed OFI first\n")
    cat("  Example:\n")
    cat("    ofi <- compute_ofi(trades, window = '1 min')\n")
    cat("    plot_ofi(ofi)\n\n")

    cat("Issue: Package won't install\n")
    cat("  Make sure dependencies are installed:\n")
    cat("    install.packages(c('dplyr', 'tidyr', 'lubridate',\n")
    cat("                       'slider', 'ggplot2', 'tibble',\n")
    cat("                       'zoo', 'readr'))\n\n")

    cat("Still stuck?\n")
    cat("  - Check function help: ?function_name\n")
    cat("  - Browse examples: rofi_examples()\n")
    cat("  - Report bugs: https://github.com/DennisPoniros/rOFI_package/issues\n\n")

  } else {
    cat("Unknown topic. Available topics:\n")
    cat("  'basics', 'data', 'statistics', 'microstructure',\n")
    cat("  'visualization', 'troubleshooting'\n\n")
    cat("Usage: rofi_help('topic_name')\n")
  }

  invisible(NULL)
}


#' Browse Worked Examples
#'
#' @description
#' Displays curated examples for common rOFI tasks. Examples are
#' organized by complexity level and include complete, runnable code.
#'
#' @param category Optional category to filter examples. Options:
#'   "basic", "data", "analysis", "advanced". If NULL (default),
#'   shows all categories.
#'
#' @return Invisibly returns NULL. Prints examples to console.
#' @export
#'
#' @examples
#' \dontrun{
#' # Show all examples
#' rofi_examples()
#'
#' # Show only basic examples
#' rofi_examples("basic")
#'
#' # Show data loading examples
#' rofi_examples("data")
#' }
rofi_examples <- function(category = NULL) {
  cat("\n")
  cat("=======================================================\n")
  cat("  rOFI - Worked Examples\n")
  cat("=======================================================\n\n")

  if (is.null(category) || category == "basic") {
    cat("BASIC EXAMPLES\n")
    cat("-------------------------------------------------------\n\n")

    cat("Example 1: Your First OFI Analysis\n")
    cat("-----------------------------------\n")
    cat("library(rOFI)\n\n")
    cat("# Generate sample data\n")
    cat("trades <- simulate_orders(n = 1000, seed = 123)\n\n")
    cat("# Compute OFI\n")
    cat("ofi <- compute_ofi(trades, window = '1 min')\n\n")
    cat("# View results\n")
    cat("head(ofi)\n")
    cat("summary(ofi$oir)\n\n")
    cat("# Plot\n")
    cat("plot_ofi(ofi)\n\n\n")

    cat("Example 2: Different Time Windows\n")
    cat("----------------------------------\n")
    cat("# 30-second windows\n")
    cat("ofi_30s <- compute_ofi(trades, window = '30 sec')\n\n")
    cat("# 5-minute windows\n")
    cat("ofi_5m <- compute_ofi(trades, window = '5 min')\n\n")
    cat("# Tick-based (every 100 trades)\n")
    cat("ofi_tick <- compute_ofi(trades, n_ticks = 100)\n\n")
    cat("# Compare\n")
    cat("cat('30s windows:', nrow(ofi_30s), '\\n')\n")
    cat("cat('5m windows:', nrow(ofi_5m), '\\n')\n\n\n")

    cat("Example 3: Detecting Market Pressure\n")
    cat("-------------------------------------\n")
    cat("ofi <- compute_ofi(trades, window = '1 min')\n\n")
    cat("# Find strong buying pressure (OIR > 0.3)\n")
    cat("strong_buy <- ofi[ofi$oir > 0.3, ]\n")
    cat("cat('Strong buying periods:', nrow(strong_buy), '\\n')\n\n")
    cat("# Find strong selling pressure (OIR < -0.3)\n")
    cat("strong_sell <- ofi[ofi$oir < -0.3, ]\n")
    cat("cat('Strong selling periods:', nrow(strong_sell), '\\n')\n\n")
    cat("# Visualize\n")
    cat("plot_ofi(ofi, which = 'oir')\n\n\n")
  }

  if (is.null(category) || category == "data") {
    cat("DATA LOADING EXAMPLES\n")
    cat("-------------------------------------------------------\n\n")

    cat("Example 4: Loading CSV Data\n")
    cat("---------------------------\n")
    cat("# Preview first\n")
    cat("preview_trade_file('mydata.csv')\n\n")
    cat("# Load with column names\n")
    cat("trades <- read_trade_csv('mydata.csv',\n")
    cat("                         time_col = 'timestamp',\n")
    cat("                         side_col = 'side',\n")
    cat("                         size_col = 'volume',\n")
    cat("                         price_col = 'price')\n\n")
    cat("# Or with column numbers\n")
    cat("trades <- read_trade_csv('mydata.csv',\n")
    cat("                         time_col = 1,\n")
    cat("                         side_col = 2,\n")
    cat("                         size_col = 3,\n")
    cat("                         price_col = 4)\n\n\n")

    cat("Example 5: Data Validation and Cleaning\n")
    cat("----------------------------------------\n")
    cat("# Load data\n")
    cat("trades <- read_trade_csv('mydata.csv',\n")
    cat("                         time_col = 1,\n")
    cat("                         side_col = 2,\n")
    cat("                         size_col = 3)\n\n")
    cat("# Validate\n")
    cat("validation <- validate_trade_data(trades)\n")
    cat("print(validation)\n\n")
    cat("# Clean if needed\n")
    cat("if (!validation$valid) {\n")
    cat("  trades_clean <- clean_trade_data(trades,\n")
    cat("                                   remove_outliers = TRUE)\n")
    cat("  \n")
    cat("  # Check cleaning report\n")
    cat("  report <- attr(trades_clean, 'cleaning_report')\n")
    cat("  print(report)\n")
    cat("}\n\n\n")

    cat("Example 6: Custom Side Mappings\n")
    cat("--------------------------------\n")
    cat("# If your data uses 'BID'/'ASK' instead of 'B'/'S'\n")
    cat("trades <- read_trade_csv('data.csv',\n")
    cat("                         time_col = 'time',\n")
    cat("                         side_col = 'direction',\n")
    cat("                         size_col = 'qty',\n")
    cat("                         side_mapping = c('BID' = 'B',\n")
    cat("                                          'ASK' = 'S'))\n\n\n")
  }

  if (is.null(category) || category == "analysis") {
    cat("STATISTICAL ANALYSIS EXAMPLES\n")
    cat("-------------------------------------------------------\n\n")

    cat("Example 7: Testing OFI Autocorrelation\n")
    cat("---------------------------------------\n")
    cat("# Compute OFI\n")
    cat("ofi <- compute_ofi(trades, window = '1 min')\n\n")
    cat("# Test autocorrelation\n")
    cat("autocorr <- test_ofi_autocorrelation(ofi)\n")
    cat("print(autocorr)\n\n")
    cat("# Interpret results:\n")
    cat("# - High autocorrelation means OFI is persistent\n")
    cat("# - Low p-value means autocorrelation is significant\n\n\n")

    cat("Example 8: Lead-Lag Analysis\n")
    cat("----------------------------\n")
    cat("# You need price data for this\n")
    cat("trades <- simulate_orders(n = 1000, seed = 123)\n")
    cat("ofi <- compute_ofi(trades, window = '1 min')\n\n")
    cat("# Extract prices at window boundaries\n")
    cat("# (simplified - adjust for your data)\n")
    cat("prices <- trades$price[seq(1, nrow(trades), by = 100)]\n")
    cat("price_changes <- diff(prices)\n\n")
    cat("# Analyze lead-lag\n")
    cat("leadlag <- ofi_lead_lag_analysis(ofi, price_changes)\n")
    cat("print(leadlag)\n\n")
    cat("# Positive lag with high correlation means OFI leads prices\n\n\n")

    cat("Example 9: Bootstrap Significance\n")
    cat("----------------------------------\n")
    cat("ofi <- compute_ofi(trades, window = '1 min')\n\n")
    cat("# Test if OFI is significantly different from random\n")
    cat("bootstrap <- bootstrap_ofi_significance(ofi,\n")
    cat("                                        n_bootstrap = 1000)\n")
    cat("print(bootstrap)\n\n")
    cat("# Low p-value means OFI is significant\n\n\n")
  }

  if (is.null(category) || category == "advanced") {
    cat("ADVANCED EXAMPLES\n")
    cat("-------------------------------------------------------\n\n")

    cat("Example 10: Advanced OFI Metrics\n")
    cat("---------------------------------\n")
    cat("# Compute standard OFI\n")
    cat("ofi <- compute_ofi(trades, window = '1 min')\n\n")
    cat("# Add EWMA smoothing\n")
    cat("ofi <- compute_ewma_ofi(ofi, lambda = 0.94)\n\n")
    cat("# Add momentum\n")
    cat("ofi <- compute_ofi_momentum(ofi, periods = 3)\n\n")
    cat("# Add acceleration\n")
    cat("ofi <- compute_ofi_acceleration(ofi, periods = 3)\n\n")
    cat("# View all metrics\n")
    cat("head(ofi)\n\n\n")

    cat("Example 11: Market Microstructure Analysis\n")
    cat("-------------------------------------------\n")
    cat("# Estimate Kyle's lambda (price impact)\n")
    cat("kyle <- kyle_lambda_estimation(trades)\n")
    cat("print(kyle)\n")
    cat("cat('Price impact per unit volume:', kyle$lambda, '\\n')\n\n")
    cat("# Compute VPIN\n")
    cat("vpin <- compute_vpin(trades, n_buckets = 50)\n")
    cat("plot(vpin$vpin, type = 'l',\n")
    cat("     main = 'Volume-Synchronized PIN',\n")
    cat("     ylab = 'VPIN')\n\n")
    cat("# Spread decomposition\n")
    cat("spread <- decompose_spread(trades)\n")
    cat("print(spread)\n\n\n")

    cat("Example 12: Complete Analysis Workflow\n")
    cat("---------------------------------------\n")
    cat("# 1. Load and validate data\n")
    cat("trades <- read_trade_csv('data.csv',\n")
    cat("                         time_col = 1,\n")
    cat("                         side_col = 2,\n")
    cat("                         size_col = 3)\n")
    cat("trades <- clean_trade_data(trades)\n\n")
    cat("# 2. Compute OFI at multiple scales\n")
    cat("ofi_1m <- compute_ofi(trades, window = '1 min')\n")
    cat("ofi_5m <- compute_ofi(trades, window = '5 min')\n\n")
    cat("# 3. Statistical tests\n")
    cat("autocorr <- test_ofi_autocorrelation(ofi_1m)\n")
    cat("print(autocorr)\n\n")
    cat("# 4. Market microstructure\n")
    cat("kyle <- kyle_lambda_estimation(trades)\n")
    cat("vpin <- compute_vpin(trades)\n\n")
    cat("# 5. Visualize\n")
    cat("plot_ofi(ofi_1m, which = 'ofi')\n")
    cat("plot_ofi(ofi_1m, which = 'ofi_cum')\n")
    cat("plot_ofi_dist(ofi_1m, metric = 'oir')\n\n\n")
  }

  cat("-------------------------------------------------------\n")
  cat("TIP: Copy and paste these examples to try them!\n")
  cat("     Modify parameters to see how results change.\n\n")

  if (!is.null(category)) {
    cat("See all examples: rofi_examples()\n")
  } else {
    cat("Filter by category:\n")
    cat("  rofi_examples('basic')\n")
    cat("  rofi_examples('data')\n")
    cat("  rofi_examples('analysis')\n")
    cat("  rofi_examples('advanced')\n")
  }
  cat("\n")

  invisible(NULL)
}


#' Interactive Tutorial
#'
#' @description
#' Runs a step-by-step interactive tutorial that guides you through
#' the basics of using rOFI. Each step includes code that you can
#' copy and run.
#'
#' @param step Optional step number to jump to. If NULL (default),
#'   starts from the beginning.
#'
#' @return Invisibly returns NULL. Prints tutorial content to console.
#' @export
#'
#' @examples
#' \dontrun{
#' # Start tutorial from beginning
#' rofi_tutorial()
#'
#' # Jump to specific step
#' rofi_tutorial(step = 3)
#' }
rofi_tutorial <- function(step = NULL) {
  if (is.null(step)) {
    step <- 1
  }

  cat("\n")
  cat("=======================================================\n")
  cat("  rOFI Interactive Tutorial\n")
  cat("=======================================================\n\n")

  if (step == 1) {
    cat("STEP 1: Understanding Order-Flow Imbalance\n")
    cat("-------------------------------------------------------\n\n")

    cat("Order-Flow Imbalance (OFI) measures buying vs selling\n")
    cat("pressure in markets.\n\n")

    cat("Think of it like a tug-of-war:\n")
    cat("  Sellers <------●------> Buyers\n\n")

    cat("- Positive OFI: More buying pressure\n")
    cat("- Negative OFI: More selling pressure\n")
    cat("- Zero OFI: Balanced\n\n")

    cat("Formula:\n")
    cat("  OFI = Buy Volume - Sell Volume\n\n")

    cat("Next: rofi_tutorial(2)\n\n")

  } else if (step == 2) {
    cat("STEP 2: Generate Sample Data\n")
    cat("-------------------------------------------------------\n\n")

    cat("Before analyzing real data, let's create sample data:\n\n")

    cat("CODE TO RUN:\n")
    cat("------------\n")
    cat("library(rOFI)\n\n")
    cat("# Generate 1000 trades with slight buying pressure\n")
    cat("trades <- simulate_orders(n = 1000,\n")
    cat("                          imb = 0.1,    # 10% more buys\n")
    cat("                          seed = 123)   # Reproducible\n\n")
    cat("# Look at the data\n")
    cat("head(trades)\n\n")

    cat("You should see columns:\n")
    cat("  - timestamp: When trade occurred\n")
    cat("  - side: 'B' for buy, 'S' for sell\n")
    cat("  - size: Number of shares\n")
    cat("  - price: Execution price\n\n")

    cat("Next: rofi_tutorial(3)\n\n")

  } else if (step == 3) {
    cat("STEP 3: Compute OFI\n")
    cat("-------------------------------------------------------\n\n")
    cat("Now let's calculate OFI in 1-minute windows:\n\n")

    cat("CODE TO RUN:\n")
    cat("------------\n")
    cat("# Compute OFI (make sure you ran Step 2 first!)\n")
    cat("ofi <- compute_ofi(trades, window = '1 min')\n\n")
    cat("# View results\n")
    cat("head(ofi)\n\n")

    cat("The output contains:\n")
    cat("  - window_start: Start of each time window\n")
    cat("  - ofi: Buy volume - Sell volume\n")
    cat("  - oir: Order Imbalance Ratio (-1 to 1)\n")
    cat("  - B: Buy volume\n")
    cat("  - S: Sell volume\n")
    cat("  - vol_total: Total volume\n")
    cat("  - n_trades: Number of trades\n\n")

    cat("Next: rofi_tutorial(4)\n\n")

  } else if (step == 4) {
    cat("STEP 4: Visualize Results\n")
    cat("-------------------------------------------------------\n\n")

    cat("Let's plot the OFI to see patterns:\n\n")

    cat("CODE TO RUN:\n")
    cat("------------\n")
    cat("# Plot OFI over time\n")
    cat("plot_ofi(ofi)\n\n")
    cat("# Or plot specific metrics:\n")
    cat("plot_ofi(ofi, which = 'oir')      # Order Imbalance Ratio\n")
    cat("plot_ofi(ofi, which = 'ofi_cum')  # Cumulative OFI\n\n")

    cat("What to look for:\n")
    cat("  - Positive values: Buying pressure\n")
    cat("  - Negative values: Selling pressure\n")
    cat("  - Rising cumulative: Persistent buying\n")
    cat("  - Falling cumulative: Persistent selling\n\n")

    cat("Next: rofi_tutorial(5)\n\n")

  } else if (step == 5) {
    cat("STEP 5: Analyze the Results\n")
    cat("-------------------------------------------------------\n\n")

    cat("Let's find periods of strong buying/selling:\n\n")

    cat("CODE TO RUN:\n")
    cat("------------\n")
    cat("# Summary statistics\n")
    cat("summary(ofi$oir)\n\n")
    cat("# Find strong buying (OIR > 0.3)\n")
    cat("strong_buy <- ofi[ofi$oir > 0.3, ]\n")
    cat("cat('Strong buying periods:', nrow(strong_buy), '\\n')\n\n")
    cat("# Find strong selling (OIR < -0.3)\n")
    cat("strong_sell <- ofi[ofi$oir < -0.3, ]\n")
    cat("cat('Strong selling periods:', nrow(strong_sell), '\\n')\n\n")
    cat("# Distribution plot\n")
    cat("plot_ofi_dist(ofi, metric = 'oir')\n\n")

    cat("Next: rofi_tutorial(6)\n\n")

  } else if (step == 6) {
    cat("STEP 6: Try Different Time Windows\n")
    cat("-------------------------------------------------------\n\n")

    cat("OFI can be calculated at different time scales:\n\n")

    cat("CODE TO RUN:\n")
    cat("------------\n")
    cat("# Fast: 30-second windows\n")
    cat("ofi_30s <- compute_ofi(trades, window = '30 sec')\n")
    cat("cat('30s windows:', nrow(ofi_30s), '\\n')\n\n")
    cat("# Medium: 1-minute windows (what we used)\n")
    cat("ofi_1m <- compute_ofi(trades, window = '1 min')\n")
    cat("cat('1m windows:', nrow(ofi_1m), '\\n')\n\n")
    cat("# Slow: 5-minute windows\n")
    cat("ofi_5m <- compute_ofi(trades, window = '5 min')\n")
    cat("cat('5m windows:', nrow(ofi_5m), '\\n')\n\n")
    cat("# Compare\n")
    cat("plot_ofi(ofi_30s, which = 'ofi')\n")
    cat("plot_ofi(ofi_5m, which = 'ofi')\n\n")

    cat("Trade-off:\n")
    cat("  - Shorter windows: More responsive, noisier\n")
    cat("  - Longer windows: Smoother, slower to react\n\n")

    cat("Next: rofi_tutorial(7)\n\n")

  } else if (step == 7) {
    cat("STEP 7: What's Next?\n")
    cat("-------------------------------------------------------\n\n")

    cat("Congratulations! You've learned the basics of OFI.\n\n")

    cat("Here's what to explore next:\n\n")

    cat("1. Load Your Own Data\n")
    cat("   vignette('data-preparation')\n")
    cat("   ?read_trade_csv\n\n")

    cat("2. Statistical Testing\n")
    cat("   ?test_ofi_autocorrelation\n")
    cat("   ?ofi_lead_lag_analysis\n")
    cat("   rofi_examples('analysis')\n\n")

    cat("3. Advanced Features\n")
    cat("   ?compute_ewma_ofi\n")
    cat("   ?kyle_lambda_estimation\n")
    cat("   ?compute_vpin\n")
    cat("   rofi_examples('advanced')\n\n")

    cat("4. Browse All Features\n")
    cat("   rofi_help()\n")
    cat("   rofi_examples()\n")
    cat("   ?rOFI\n\n")

    cat("Happy analyzing!\n\n")

  } else {
    cat("Invalid step number. Tutorial has 7 steps.\n")
    cat("Start from beginning: rofi_tutorial()\n\n")
  }

  invisible(NULL)
}


#' Quick Reference Cheatsheet
#'
#' @description
#' Displays a concise cheatsheet of the most commonly used rOFI
#' functions and their parameters. Perfect for quick lookups.
#'
#' @param section Optional section to display. Options: "data",
#'   "compute", "plot", "test", "microstructure". If NULL (default),
#'   shows all sections.
#'
#' @return Invisibly returns NULL. Prints cheatsheet to console.
#' @export
#'
#' @examples
#' \dontrun{
#' # Show full cheatsheet
#' rofi_cheatsheet()
#'
#' # Show only data functions
#' rofi_cheatsheet("data")
#'
#' # Show only plotting functions
#' rofi_cheatsheet("plot")
#' }
rofi_cheatsheet <- function(section = NULL) {
  cat("\n")
  cat("=======================================================\n")
  cat("  rOFI Quick Reference Cheatsheet\n")
  cat("=======================================================\n\n")

  if (is.null(section) || section == "data") {
    cat("DATA LOADING & PREPARATION\n")
    cat("-------------------------------------------------------\n")
    cat("preview_trade_file('file.csv')\n")
    cat("  Preview file before loading\n\n")

    cat("read_trade_csv('file.csv', time_col, side_col, size_col)\n")
    cat("  Load CSV data\n")
    cat("  - time_col: Column name/number for timestamps\n")
    cat("  - side_col: Column for buy/sell indicator\n")
    cat("  - size_col: Column for trade size\n\n")

    cat("read_lobster_trades('file.csv', date, tz)\n")
    cat("  Load LOBSTER academic format\n\n")

    cat("validate_trade_data(trades)\n")
    cat("  Check data quality\n\n")

    cat("clean_trade_data(trades, remove_outliers = TRUE)\n")
    cat("  Automated cleaning\n\n")

    cat("simulate_orders(n = 1000, imb = 0, seed = NULL)\n")
    cat("  Generate synthetic data\n")
    cat("  - n: Number of trades\n")
    cat("  - imb: Imbalance (-1 to 1)\n\n\n")
  }

  if (is.null(section) || section == "compute") {
    cat("OFI COMPUTATION\n")
    cat("-------------------------------------------------------\n")
    cat("compute_ofi(trades, window = '1 min')\n")
    cat("  Calculate OFI metrics\n")
    cat("  - window: '30 sec', '1 min', '5 min', '1 hour'\n")
    cat("  - n_ticks: Use tick-based windows instead\n")
    cat("  - price_weighted: TRUE for VWAP weighting\n\n")

    cat("compute_ewma_ofi(ofi, lambda = 0.94)\n")
    cat("  Exponentially weighted moving average\n\n")

    cat("compute_ofi_momentum(ofi, periods = 3)\n")
    cat("  Rate of change\n\n")

    cat("compute_ofi_acceleration(ofi, periods = 3)\n")
    cat("  Second derivative\n\n")

    cat("compute_vwap_ofi(ofi, trades)\n")
    cat("  Volume-weighted average price OFI\n\n\n")
  }

  if (is.null(section) || section == "plot") {
    cat("VISUALIZATION\n")
    cat("-------------------------------------------------------\n")
    cat("plot_ofi(ofi, which = 'ofi')\n")
    cat("  Time series plot\n")
    cat("  - which: 'ofi', 'oir', 'ofi_cum', 'volume'\n\n")

    cat("plot_ofi_dist(ofi, metric = 'oir', plot_type = 'histogram')\n")
    cat("  Distribution plot\n")
    cat("  - metric: 'ofi', 'oir', 'B', 'S'\n")
    cat("  - plot_type: 'histogram', 'density', 'boxplot'\n\n\n")
  }

  if (is.null(section) || section == "test") {
    cat("STATISTICAL TESTING\n")
    cat("-------------------------------------------------------\n")
    cat("test_ofi_autocorrelation(ofi, max_lag = 20)\n")
    cat("  Test for serial correlation\n\n")

    cat("ofi_lead_lag_analysis(ofi, price_changes, max_lag = 10)\n")
    cat("  Cross-correlation with prices\n\n")

    cat("bootstrap_ofi_significance(ofi, n_bootstrap = 1000)\n")
    cat("  Non-parametric hypothesis test\n\n\n")
  }

  if (is.null(section) || section == "microstructure") {
    cat("MARKET MICROSTRUCTURE\n")
    cat("-------------------------------------------------------\n")
    cat("kyle_lambda_estimation(trades)\n")
    cat("  Kyle's lambda (price impact)\n\n")

    cat("estimate_price_impact(trades, horizon = 5)\n")
    cat("  Temporary vs permanent impact\n\n")

    cat("compute_vpin(trades, n_buckets = 50)\n")
    cat("  Volume-synchronized PIN\n\n")

    cat("decompose_spread(trades)\n")
    cat("  Spread component analysis\n\n")

    cat("compute_effective_spread(trades)\n")
    cat("  Transaction costs\n\n\n")
  }

  cat("QUICK WORKFLOW\n")
  cat("-------------------------------------------------------\n")
  cat("# 1. Load data\n")
  cat("trades <- read_trade_csv('data.csv', 1, 2, 3)\n\n")

  cat("# 2. Validate\n")
  cat("validate_trade_data(trades)\n\n")

  cat("# 3. Compute OFI\n")
  cat("ofi <- compute_ofi(trades, window = '1 min')\n\n")

  cat("# 4. Visualize\n")
  cat("plot_ofi(ofi)\n\n")

  cat("# 5. Analyze\n")
  cat("test_ofi_autocorrelation(ofi)\n\n")

  cat("-------------------------------------------------------\n")
  cat("For more help:\n")
  cat("  rofi_help()      - Interactive help menu\n")
  cat("  rofi_examples()  - Worked examples\n")
  cat("  rofi_tutorial()  - Step-by-step tutorial\n")
  cat("  ?rOFI            - Full documentation\n\n")

  invisible(NULL)
}
