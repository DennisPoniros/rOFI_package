#' Generate Comprehensive Test Data for All rOFI Modules
#'
#' @description
#' Educational function that generates realistic synthetic market data in all
#' formats required by rOFI functions. Perfect for learning, testing, and
#' demonstrations without needing real market data.
#'
#' @param n_trades Integer, number of trades to generate (default: 1000)
#' @param n_messages Integer, number of order messages to generate (default: 2000)
#' @param n_snapshots Integer, number of order book snapshots (default: 100)
#' @param n_levels Integer, number of order book levels (default: 10)
#' @param start_time POSIXct, start time for data (default: today 9:30 AM ET)
#' @param symbol Character, ticker symbol (default: "TEST")
#' @param seed Integer, random seed for reproducibility (default: NULL)
#'
#' @return A list with components:
#' \describe{
#'   \item{trades}{Trade data for basic OFI analysis}
#'   \item{messages}{Message-level data for surveillance and reconstruction}
#'   \item{orderbook}{Order book snapshots for multi-level metrics}
#'   \item{executions}{Execution records for price impact calibration}
#'   \item{multi_venue}{Multi-venue data for cross-market analysis}
#' }
#'
#' @details
#' This function generates all data formats needed to test and learn every
#' rOFI module:
#'
#' \strong{1. Trade Data} - For basic OFI, visualization, microstructure
#' \itemize{
#'   \item timestamp, side, size, price
#'   \item Use with: compute_ofi(), plot_ofi(), kyle_lambda_estimation()
#' }
#'
#' \strong{2. Message Data} - For surveillance and order book reconstruction
#' \itemize{
#'   \item message_type (add/cancel/execute), order_id, side, price, size
#'   \item Use with: detect_spoofing(), reconstruct_orderbook()
#' }
#'
#' \strong{3. Order Book Snapshots} - For multi-level metrics
#' \itemize{
#'   \item bid_price_1 to bid_price_N, ask_price_1 to ask_price_N, etc.
#'   \item Use with: compute_multilevel_ofi(), orderbook_slope()
#' }
#'
#' \strong{4. Execution Data} - For price impact calibration
#' \itemize{
#'   \item order_size, avg_daily_volume, realized_impact
#'   \item Use with: calibrate_sqrt_law()
#' }
#'
#' \strong{5. Multi-Venue Data} - For cross-market analysis
#' \itemize{
#'   \item Multiple venues with timestamps, prices, sizes
#'   \item Use with: consolidate_venues(), price_discovery_metrics()
#' }
#'
#' @export
#'
#' @examples
#' # Generate all test data formats
#' test_data <- generate_test_data(n_trades = 500, seed = 42)
#'
#' # Use trade data
#' ofi <- compute_ofi(test_data$trades, window = "1 min")
#' plot_ofi(ofi)
#'
#' # Use message data for surveillance
#' spoof <- detect_spoofing(test_data$messages)
#' print(spoof)
#'
#' # Use orderbook data for multi-level metrics
#' ml_ofi <- compute_multilevel_ofi(test_data$orderbook, n_levels = 5)
#' print(ml_ofi)
#'
#' # Use execution data for calibration
#' calib <- calibrate_sqrt_law(test_data$executions)
#' print(calib)
#'
#' @seealso
#' \code{\link{simulate_orders}} for basic trade data only
#'
#' @references
#' This function creates synthetic data that mimics real market microstructure
#' patterns for educational purposes. Data characteristics:
#' \itemize{
#'   \item Realistic spread dynamics
#'   \item Order arrival follows Poisson process
#'   \item Size follows log-normal distribution
#'   \item Price changes exhibit volatility clustering
#' }
generate_test_data <- function(n_trades = 1000,
                                n_messages = 2000,
                                n_snapshots = 100,
                                n_levels = 10,
                                start_time = NULL,
                                symbol = "TEST",
                                seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # Default start time: today at 9:30 AM ET
  if (is.null(start_time)) {
    start_time <- as.POSIXct(paste(Sys.Date(), "09:30:00"), tz = "America/New_York")
  }

  # ============================================================================
  # 1. TRADE DATA - For basic OFI, visualization, microstructure
  # ============================================================================

  trades <- simulate_orders(
    n = n_trades,
    start = start_time,
    lambda = 10,
    imb = 0.1,
    drift = 0.05,
    vol = 0.02,
    seed = if (!is.null(seed)) seed else NULL
  )

  # ============================================================================
  # 2. MESSAGE DATA - For surveillance and order book reconstruction
  # ============================================================================

  # Generate realistic message stream
  message_times <- start_time + cumsum(rexp(n_messages, rate = 20))

  # Message types: Add (40%), Cancel (35%), Execute (25%)
  msg_types <- sample(
    c("add", "cancel", "execute"),
    n_messages,
    replace = TRUE,
    prob = c(0.40, 0.35, 0.25)
  )

  # Standardize message types for compatibility
  message_type_map <- c(
    "add" = "A",
    "cancel" = "C",
    "execute" = "E"
  )

  base_price <- 100
  prices <- base_price + cumsum(rnorm(n_messages, 0, 0.01))

  messages <- tibble::tibble(
    timestamp = message_times,
    message_type = message_type_map[msg_types],
    order_id = 1:n_messages,
    side = sample(c("bid", "ask"), n_messages, replace = TRUE),
    price = round(prices, 2),
    size = round(rlnorm(n_messages, meanlog = 6, sdlog = 1)),
    level = sample(1:n_levels, n_messages, replace = TRUE)
  )

  # Add some spoofing patterns (for testing surveillance)
  if (n_messages >= 100) {
    # Create a spoofing episode: large orders followed by quick cancels
    spoof_start <- round(n_messages * 0.3)
    spoof_indices <- spoof_start:(spoof_start + 20)
    messages$size[spoof_indices] <- quantile(messages$size, 0.95)
    messages$message_type[spoof_indices[1:15]] <- "A"  # Add
    messages$message_type[spoof_indices[16:20]] <- "C"  # Quick cancel
  }

  # ============================================================================
  # 3. ORDER BOOK SNAPSHOTS - For multi-level metrics
  # ============================================================================

  snapshot_times <- seq(start_time, by = "10 sec", length.out = n_snapshots)
  orderbook_list <- list()

  for (i in seq_len(n_snapshots)) {
    mid_price <- base_price + cumsum(rnorm(1, 0, 0.01))
    spread <- runif(1, 0.01, 0.05)

    # Create bid and ask prices with realistic spacing
    bid_prices <- seq(
      mid_price - spread/2,
      mid_price - spread/2 - 0.1 * (n_levels - 1),
      length.out = n_levels
    )
    ask_prices <- seq(
      mid_price + spread/2,
      mid_price + spread/2 + 0.1 * (n_levels - 1),
      length.out = n_levels
    )

    # Generate sizes with depth decay
    bid_sizes <- round(rlnorm(n_levels, meanlog = 7, sdlog = 0.5) * exp(-0.1 * (1:n_levels)))
    ask_sizes <- round(rlnorm(n_levels, meanlog = 7, sdlog = 0.5) * exp(-0.1 * (1:n_levels)))

    # Create wide format for multi-level functions
    snapshot <- tibble::tibble(timestamp = snapshot_times[i])

    for (lev in 1:n_levels) {
      snapshot[[paste0("bid_price_", lev)]] <- round(bid_prices[lev], 2)
      snapshot[[paste0("bid_size_", lev)]] <- bid_sizes[lev]
      snapshot[[paste0("ask_price_", lev)]] <- round(ask_prices[lev], 2)
      snapshot[[paste0("ask_size_", lev)]] <- ask_sizes[lev]
    }

    orderbook_list[[i]] <- snapshot
  }

  orderbook <- dplyr::bind_rows(orderbook_list)

  # ============================================================================
  # 4. EXECUTION DATA - For price impact calibration
  # ============================================================================

  n_execs <- min(100, n_trades)
  executions <- tibble::tibble(
    timestamp = start_time + cumsum(rexp(n_execs, rate = 0.5)),
    symbol = symbol,
    order_size = round(rlnorm(n_execs, meanlog = 9, sdlog = 1)),
    avg_daily_volume = round(rlnorm(n_execs, meanlog = 15, sdlog = 0.5)),
    volatility = runif(n_execs, 0.01, 0.05),
    realized_impact = NA_real_
  )

  # Calculate realistic impact using square-root law
  executions$realized_impact <- 0.20 * executions$volatility *
    sqrt(executions$order_size / executions$avg_daily_volume)

  # Add some noise
  executions$realized_impact <- executions$realized_impact * rnorm(n_execs, 1, 0.2)

  # ============================================================================
  # 5. MULTI-VENUE DATA - For cross-market analysis
  # ============================================================================

  # Generate data for 3 venues (NYSE, NASDAQ, BATS)
  venues <- c("NYSE", "NASDAQ", "BATS")
  n_per_venue <- ceiling(n_trades / 3)

  multi_venue <- list()

  for (venue in venues) {
    venue_trades <- simulate_orders(
      n = n_per_venue,
      start = start_time,
      lambda = 10 + rnorm(1, 0, 2),  # Slight variation per venue
      imb = 0.05 + rnorm(1, 0, 0.05),
      drift = 0.05,
      vol = 0.02,
      seed = if (!is.null(seed)) seed + match(venue, venues) else NULL
    )

    venue_trades$venue <- venue
    venue_trades$symbol <- symbol

    multi_venue[[venue]] <- venue_trades
  }

  # ============================================================================
  # Return comprehensive test data
  # ============================================================================

  result <- list(
    trades = trades,
    messages = messages,
    orderbook = orderbook,
    executions = executions,
    multi_venue = multi_venue
  )

  class(result) <- c("rofi_test_data", "list")

  return(result)
}


#' Print method for rOFI test data
#'
#' @param x A rofi_test_data object
#' @param ... Additional arguments (ignored)
#'
#' @export
#' @keywords internal
print.rofi_test_data <- function(x, ...) {
  cat("\n")
  cat("rOFI Test Data Package\n")
  cat("======================\n\n")

  cat("Components:\n")
  cat(sprintf("  1. trades:       %d rows  - Use with compute_ofi(), plot_ofi()\n",
              nrow(x$trades)))
  cat(sprintf("  2. messages:     %d rows  - Use with detect_spoofing(), reconstruct_orderbook()\n",
              nrow(x$messages)))
  cat(sprintf("  3. orderbook:    %d rows  - Use with compute_multilevel_ofi(), orderbook_slope()\n",
              nrow(x$orderbook)))
  cat(sprintf("  4. executions:   %d rows  - Use with calibrate_sqrt_law()\n",
              nrow(x$executions)))
  cat(sprintf("  5. multi_venue:  %d items - Use with consolidate_venues(), price_discovery_metrics()\n",
              length(x$multi_venue)))

  cat("\nQuick Start:\n")
  cat("  # Basic OFI\n")
  cat("  ofi <- compute_ofi(test_data$trades, window = '1 min')\n\n")

  cat("  # Surveillance\n")
  cat("  spoof <- detect_spoofing(test_data$messages)\n\n")

  cat("  # Multi-level metrics\n")
  cat("  ml_ofi <- compute_multilevel_ofi(test_data$orderbook, n_levels = 5)\n\n")

  cat("See ?generate_test_data for more examples\n\n")

  invisible(x)
}
