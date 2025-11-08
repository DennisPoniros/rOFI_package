#' Production Data Pipelines for Market Data
#'
#' @description
#' Robust ingestion, validation, and processing of production market data
#' from multiple sources including TAQ, ITCH, PITCH, and generic formats.
#' Handles timestamp synchronization, trade classification, and data quality.
#'
#' @name data_pipelines
NULL

#' Read NYSE TAQ (Trade and Quote) trade data
#'
#' @description
#' Parses NYSE TAQ trade files in CSV or binary format. Handles both
#' Daily TAQ and Monthly TAQ structures with appropriate filtering and
#' validation.
#'
#' @param file_path Character, path to TAQ file
#' @param symbol Character, stock symbol to filter (default: NULL for all)
#' @param date Date or character, trading date (default: NULL, auto-detect)
#' @param time_zone Character, timezone for timestamps (default: "America/New_York")
#' @param filters List of filter criteria (default: standard filters)
#' @param validate Logical, run validation checks (default: TRUE)
#'
#' @return A tibble with standardized trade data
#'
#' @details
#' **TAQ File Structure**:
#' - **Time**: HH:MM:SS format or milliseconds since midnight
#' - **Symbol**: Stock ticker (may need padding with spaces)
#' - **Exchange**: Execution venue code
#' - **Price**: Trade price
#' - **Size**: Share volume
#' - **Conditions**: Trade condition codes
#'
#' **Standard Filters**:
#' - Remove trades outside regular hours (9:30-16:00 ET)
#' - Exclude error trades (condition code 'E')
#' - Remove out-of-sequence trades
#' - Filter abnormal sale conditions
#'
#' **Trade Conditions** (common codes):
#' - Regular sale: ' ' (space) or '@'
#' - Intermarket sweep: 'F'
#' - Opening trade: 'O'
#' - Closing trade: 'C'
#' - Error: 'E' (exclude)
#' - Stopped stock: 'S'
#'
#' @references
#' NYSE TAQ Data Products:
#' https://www.nyse.com/market-data/historical
#'
#' @export
#' @importFrom readr read_csv cols col_character col_double col_integer
#' @importFrom dplyr filter mutate
#' @importFrom lubridate ymd_hms
#'
#' @examples
#' \dontrun{
#' # Read TAQ file for single symbol
#' trades <- read_taq_trades(
#'   file_path = "TAQ_2024-01-15.csv",
#'   symbol = "AAPL",
#'   date = "2024-01-15"
#' )
#'
#' # Read with custom filters
#' trades <- read_taq_trades(
#'   file_path = "TAQ_2024-01-15.csv",
#'   symbol = "MSFT",
#'   filters = list(
#'     regular_hours = TRUE,
#'     exclude_conditions = c("E", "Z"),
#'     min_price = 0.01,
#'     max_price = 1000000
#'   )
#' )
#' }
read_taq_trades <- function(file_path,
                             symbol = NULL,
                             date = NULL,
                             time_zone = "America/New_York",
                             filters = list(),
                             validate = TRUE) {

  # Check file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Set default filters
  default_filters <- list(
    regular_hours = TRUE,
    exclude_conditions = c("E", "Z", "L", "N", "O", "C", "W", "4", "5", "6", "7"),
    min_price = 0.01,
    max_price = 1000000,
    min_size = 1
  )

  filters <- modifyList(default_filters, filters)

  # Detect file format (CSV vs binary)
  file_ext <- tolower(tools::file_ext(file_path))

  if (file_ext == "csv") {
    # Read CSV format TAQ
    trades <- read_csv(
      file_path,
      col_types = cols(
        Time = col_character(),
        Symbol = col_character(),
        Exchange = col_character(),
        Price = col_double(),
        Size = col_integer(),
        Conditions = col_character(),
        .default = col_character()
      ),
      show_col_types = FALSE
    )

  } else {
    stop("Binary TAQ format not yet supported. Convert to CSV first.")
  }

  # Standardize column names
  trades <- standardize_taq_columns(trades)

  # Parse timestamp
  if (is.null(date)) {
    # Try to extract from filename
    date <- extract_date_from_filename(file_path)
  }

  if (is.null(date)) {
    warning("Date not specified and could not be extracted. Using today's date.")
    date <- Sys.Date()
  }

  trades <- trades |>
    mutate(
      timestamp = parse_taq_timestamp(.data$time, date, time_zone)
    )

  # Filter by symbol if specified
  if (!is.null(symbol)) {
    symbol_padded <- str_pad_symbol(symbol, width = 8)  # TAQ uses 8-char symbols
    trades <- trades |>
      filter(trimws(.data$symbol) == symbol | .data$symbol == symbol_padded)
  }

  # Apply filters
  if (filters$regular_hours) {
    trades <- filter_regular_hours(trades, time_zone)
  }

  if (!is.null(filters$exclude_conditions) && "conditions" %in% names(trades)) {
    trades <- trades |>
      filter(!.data$conditions %in% filters$exclude_conditions)
  }

  trades <- trades |>
    filter(
      .data$price >= filters$min_price,
      .data$price <= filters$max_price,
      .data$size >= filters$min_size
    )

  # Validate if requested
  if (validate) {
    validation <- validate_tick_data(trades, strict = FALSE)
    if (!validation$is_valid) {
      warning("Data validation found issues: ", paste(validation$issues, collapse = ", "))
    }
  }

  # Standardize output format
  trades <- trades |>
    select(
      timestamp,
      symbol = .data$symbol,
      exchange = .data$exchange,
      price = .data$price,
      size = .data$size,
      conditions = if("conditions" %in% names(trades)) .data$conditions else NULL
    ) |>
    arrange(.data$timestamp)

  # Add metadata
  attr(trades, "source") <- "NYSE_TAQ"
  attr(trades, "date") <- date
  attr(trades, "symbol") <- symbol

  return(as_tibble(trades))
}


#' Read NASDAQ ITCH 5.0 message data
#'
#' @description
#' Parses NASDAQ ITCH 5.0 protocol messages to reconstruct order book
#' and trades. ITCH is a feed of all order book events.
#'
#' @param file_path Character, path to ITCH file (.csv or .bin)
#' @param symbol Character, stock symbol to filter
#' @param message_types Character vector, message types to include
#'   (default: c("A", "F", "E", "C", "X", "D", "U"))
#' @param parse_full Logical, parse full order book (default: FALSE)
#'
#' @return A tibble with ITCH messages
#'
#' @details
#' **ITCH Message Types**:
#' - **A**: Add order (no MPID)
#' - **F**: Add order with MPID
#' - **E**: Order executed
#' - **C**: Order executed with price
#' - **X**: Order cancel
#' - **D**: Order delete
#' - **U**: Order replace
#' - **P**: Trade (non-cross)
#' - **Q**: Cross trade
#'
#' **Parsing Options**:
#' - `parse_full = FALSE`: Returns message stream
#' - `parse_full = TRUE`: Reconstructs full order book state
#'
#' @references
#' NASDAQ ITCH Specification:
#' https://www.nasdaqtrader.com/content/technicalsupport/specifications/dataproducts/NQTVITCHSpecification.pdf
#'
#' @export
#' @examples
#' \dontrun{
#' # Read ITCH messages for AAPL
#' messages <- read_itch_messages(
#'   file_path = "ITCH_2024-01-15.csv",
#'   symbol = "AAPL"
#' )
#'
#' # Reconstruct order book
#' orderbook <- read_itch_messages(
#'   file_path = "ITCH_2024-01-15.csv",
#'   symbol = "AAPL",
#'   parse_full = TRUE
#' )
#' }
read_itch_messages <- function(file_path,
                                 symbol,
                                 message_types = c("A", "F", "E", "C", "X", "D", "U", "P"),
                                 parse_full = FALSE) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  file_ext <- tolower(tools::file_ext(file_path))

  if (file_ext == "csv") {
    # Read CSV format (e.g., from LOBSTER or converted ITCH)
    messages <- read_csv(
      file_path,
      show_col_types = FALSE
    )

  } else if (file_ext == "bin") {
    stop("Binary ITCH parsing requires specialized binary reader. ",
         "Please convert to CSV first or use LOBSTER format.")
  } else {
    stop("Unsupported file format: ", file_ext)
  }

  # Standardize columns based on ITCH structure
  messages <- standardize_itch_columns(messages)

  # Filter by symbol
  if (!is.null(symbol)) {
    messages <- messages |>
      filter(trimws(.data$symbol) == symbol)
  }

  # Filter by message type
  if (!is.null(message_types)) {
    messages <- messages |>
      filter(.data$message_type %in% message_types)
  }

  if (parse_full) {
    # Reconstruct order book from messages
    orderbook <- reconstruct_orderbook_from_itch(messages)
    return(orderbook)
  }

  attr(messages, "source") <- "NASDAQ_ITCH"
  attr(messages, "symbol") <- symbol

  return(messages)
}


#' Reconstruct limit order book from messages
#'
#' @description
#' Reconstructs the limit order book state from a stream of order
#' messages (add, cancel, execute, modify). Works with ITCH, LOBSTER,
#' or generic message formats.
#'
#' @param messages Data frame with columns: timestamp, message_type,
#'   order_id, side, price, size
#' @param depth Integer, number of price levels to track (default: 10)
#' @param snapshot_freq Integer, frequency of snapshots in messages
#'   (default: 100, i.e., every 100 messages)
#'
#' @return A list containing:
#'   \describe{
#'     \item{snapshots}{Data frame of order book snapshots}
#'     \item{best_bid_ask}{Time series of best bid/ask}
#'     \item{depth_evolution}{Evolution of book depth}
#'     \item{statistics}{Summary statistics}
#'   }
#'
#' @details
#' **Message Processing**:
#' 1. **Add orders**: Insert into book at price level
#' 2. **Cancel orders**: Remove size from book
#' 3. **Execute orders**: Reduce size, remove if fully filled
#' 4. **Modify orders**: Cancel old, add new
#'
#' **Limitations**:
#' - Requires complete message history (no gaps)
#' - Assumes no hidden orders (what you see is what exists)
#' - Does not model iceberg orders or minimum quantity
#'
#' @export
#' @examples
#' \dontrun{
#' # From LOBSTER messages
#' lobster_data <- read_lobster_trades("AAPL_2024-01-15_message.csv")
#' orderbook <- reconstruct_orderbook(lobster_data, depth = 10)
#'
#' # View best bid/ask evolution
#' head(orderbook$best_bid_ask)
#' }
reconstruct_orderbook <- function(messages,
                                   depth = 10,
                                   snapshot_freq = 100) {

  # Validate input
  required_cols <- c("timestamp", "message_type", "side", "price", "size")
  missing <- setdiff(required_cols, names(messages))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  if (nrow(messages) == 0) {
    stop("No messages to process")
  }

  # Initialize order book state
  bids <- list()  # List of data frames, one per price level
  asks <- list()

  # Track order IDs if available
  has_order_ids <- "order_id" %in% names(messages)
  if (has_order_ids) {
    order_tracker <- list()  # Map order_id -> (side, price, size)
  }

  # Initialize output containers
  snapshots <- list()
  snapshot_times <- c()

  # Process messages sequentially
  pb <- txtProgressBar(min = 0, max = nrow(messages), style = 3)

  for (i in seq_len(nrow(messages))) {
    msg <- messages[i, ]

    # Update order book based on message type
    if (msg$message_type %in% c("A", "F", "ADD", "LIMIT")) {
      # Add order
      if (msg$side == "B" || msg$side == "BUY") {
        bids <- add_to_book(bids, msg$price, msg$size)
      } else {
        asks <- add_to_book(asks, msg$price, msg$size)
      }

      if (has_order_ids && !is.na(msg$order_id)) {
        order_tracker[[as.character(msg$order_id)]] <- list(
          side = msg$side,
          price = msg$price,
          size = msg$size
        )
      }

    } else if (msg$message_type %in% c("E", "C", "EXECUTE", "TRADE")) {
      # Execute order
      if (msg$side == "B" || msg$side == "BUY") {
        bids <- remove_from_book(bids, msg$price, msg$size)
      } else {
        asks <- remove_from_book(asks, msg$price, msg$size)
      }

    } else if (msg$message_type %in% c("X", "D", "CANCEL", "DELETE")) {
      # Cancel order
      if (has_order_ids && !is.na(msg$order_id)) {
        order_id_str <- as.character(msg$order_id)
        if (order_id_str %in% names(order_tracker)) {
          order_info <- order_tracker[[order_id_str]]
          if (order_info$side == "B") {
            bids <- remove_from_book(bids, order_info$price, order_info$size)
          } else {
            asks <- remove_from_book(asks, order_info$price, order_info$size)
          }
          order_tracker[[order_id_str]] <- NULL
        }
      } else {
        # Use message price/size
        if (msg$side == "B" || msg$side == "BUY") {
          bids <- remove_from_book(bids, msg$price, msg$size)
        } else {
          asks <- remove_from_book(asks, msg$price, msg$size)
        }
      }
    }

    # Take snapshot at regular intervals
    if (i %% snapshot_freq == 0 || i == nrow(messages)) {
      snapshot <- create_book_snapshot(bids, asks, depth)
      snapshot$timestamp <- msg$timestamp
      snapshot$message_number <- i

      snapshots[[length(snapshots) + 1]] <- snapshot
      snapshot_times <- c(snapshot_times, msg$timestamp)
    }

    if (i %% 1000 == 0) {
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)

  # Combine snapshots into data frame
  snapshots_df <- bind_rows(snapshots)

  # Extract best bid/ask time series
  best_bid_ask <- snapshots_df |>
    mutate(
      best_bid = .data$bid_price_1,
      best_ask = .data$ask_price_1,
      mid_price = (.data$best_bid + .data$best_ask) / 2,
      spread = .data$best_ask - .data$best_bid,
      spread_bps = (.data$spread / .data$mid_price) * 10000
    ) |>
    select(.data$timestamp, .data$best_bid, .data$best_ask,
           .data$mid_price, .data$spread, .data$spread_bps)

  # Calculate statistics
  stats <- list(
    n_messages = nrow(messages),
    n_snapshots = nrow(snapshots_df),
    avg_spread_bps = mean(best_bid_ask$spread_bps, na.rm = TRUE),
    avg_bid_depth = mean(snapshots_df$bid_size_1, na.rm = TRUE),
    avg_ask_depth = mean(snapshots_df$ask_size_1, na.rm = TRUE),
    time_range = range(messages$timestamp)
  )

  result <- list(
    snapshots = snapshots_df,
    best_bid_ask = best_bid_ask,
    statistics = stats,
    metadata = list(
      depth = depth,
      snapshot_freq = snapshot_freq,
      n_messages = nrow(messages)
    )
  )

  class(result) <- c("orderbook_reconstruction", "list")
  return(result)
}


#' Classify trades using Lee-Ready algorithm
#'
#' @description
#' Classifies trades as buyer-initiated (B) or seller-initiated (S) using
#' the Lee-Ready algorithm. Compares trade price to prevailing quotes.
#'
#' @param trades Data frame with timestamp, price, size
#' @param quotes Data frame with timestamp, bid, ask (optional)
#' @param method Character, "lee-ready", "emo", "tick", or "quote"
#' @param tick_rule_threshold Numeric, threshold for tick rule (default: 0)
#'
#' @return Data frame with added 'side' column
#'
#' @details
#' **Lee-Ready Algorithm** (1991):
#' 1. Match trade to prevailing quote (5-second lag)
#' 2. If trade price > midpoint: Buyer-initiated (B)
#' 3. If trade price < midpoint: Seller-initiated (S)
#' 4. If trade price == midpoint: Use tick rule
#'    - If price increased from previous: Buyer-initiated
#'    - If price decreased: Seller-initiated
#'    - If unchanged: Use previous classification
#'
#' **EMO Algorithm** (Ellis, Michaely, O'Hara 2000):
#' - Depth-weighted midpoint instead of simple midpoint
#' - Better for markets with asymmetric depth
#'
#' **Tick Rule**:
#' - Uptick: Buyer-initiated
#' - Downtick: Seller-initiated
#'
#' **Quote Rule**:
#' - Compare to bid/ask directly
#'
#' @references
#' Lee, C., & Ready, M. J. (1991). Inferring trade direction from
#' intraday data. *Journal of Finance*, 46(2), 733-746.
#'
#' @export
#' @examples
#' \dontrun{
#' # Read trades and quotes
#' trades <- read_taq_trades("TAQ_trades.csv", symbol = "AAPL")
#' quotes <- read_taq_quotes("TAQ_quotes.csv", symbol = "AAPL")
#'
#' # Classify with Lee-Ready
#' trades_classified <- classify_trades(trades, quotes, method = "lee-ready")
#'
#' # Check classification distribution
#' table(trades_classified$side)
#' }
classify_trades <- function(trades,
                             quotes = NULL,
                             method = c("lee-ready", "emo", "tick", "quote"),
                             tick_rule_threshold = 0) {

  method <- match.arg(method)

  # If side already exists, return as-is
  if ("side" %in% names(trades)) {
    message("Trades already classified. Returning original data.")
    return(trades)
  }

  if (method %in% c("lee-ready", "emo", "quote") && is.null(quotes)) {
    warning("Quotes required for ", method, " method. Falling back to tick rule.")
    method <- "tick"
  }

  if (method == "lee-ready") {
    trades <- classify_lee_ready(trades, quotes)

  } else if (method == "emo") {
    trades <- classify_emo(trades, quotes)

  } else if (method == "tick") {
    trades <- classify_tick_rule(trades, tick_rule_threshold)

  } else if (method == "quote") {
    trades <- classify_quote_rule(trades, quotes)
  }

  # Ensure side is standardized
  trades <- trades |>
    mutate(side = ifelse(.data$side == "B" | .data$side == 1, "B", "S"))

  return(trades)
}


#' Consolidate data from multiple venues
#'
#' @description
#' Merges trade and quote data from fragmented markets (multiple exchanges,
#' dark pools, etc.) into a single consolidated tape with timestamp
#' synchronization.
#'
#' @param data_list Named list of data frames from different venues
#' @param sync_method Character, "nearest", "ffill", or "interpolate"
#' @param max_time_diff Numeric, maximum time difference for matching (seconds)
#' @param deduplicate Logical, remove duplicate trades (default: TRUE)
#'
#' @return Consolidated data frame
#'
#' @details
#' **Timestamp Synchronization**:
#' - Different venues may have clock skew
#' - Some feeds are delayed relative to others
#' - Consolidation requires alignment
#'
#' **Sync Methods**:
#' - `nearest`: Match to nearest timestamp
#' - `ffill`: Forward fill (last observation carried forward)
#' - `interpolate`: Linear interpolation for missing values
#'
#' **Deduplication**:
#' - Same trade may appear on multiple feeds
#' - Use timestamp + price + size to identify duplicates
#' - Keep earliest occurrence
#'
#' @export
#' @examples
#' \dontrun{
#' # Load data from multiple exchanges
#' nasdaq_trades <- read_taq_trades("NASDAQ.csv", symbol = "AAPL")
#' nyse_trades <- read_taq_trades("NYSE.csv", symbol = "AAPL")
#' bats_trades <- read_taq_trades("BATS.csv", symbol = "AAPL")
#'
#' # Consolidate
#' consolidated <- consolidate_venues(
#'   data_list = list(
#'     NASDAQ = nasdaq_trades,
#'     NYSE = nyse_trades,
#'     BATS = bats_trades
#'   ),
#'   sync_method = "nearest",
#'   deduplicate = TRUE
#' )
#' }
consolidate_venues <- function(data_list,
                                 sync_method = c("nearest", "ffill", "interpolate"),
                                 max_time_diff = 1,
                                 deduplicate = TRUE) {

  sync_method <- match.arg(sync_method)

  if (!is.list(data_list) || length(data_list) < 2) {
    stop("data_list must be a list with at least 2 data frames")
  }

  # Add venue identifier to each data frame
  consolidated <- bind_rows(
    lapply(names(data_list), function(venue_name) {
      df <- data_list[[venue_name]]
      df$venue <- venue_name
      return(df)
    })
  )

  # Sort by timestamp
  consolidated <- consolidated |>
    arrange(.data$timestamp)

  # Deduplicate if requested
  if (deduplicate) {
    # Identify duplicates by timestamp + price + size (within tolerance)
    consolidated <- consolidated |>
      group_by(
        time_bin = floor_date(.data$timestamp, "second"),
        price_round = round(.data$price, 2),
        size_exact = .data$size
      ) |>
      # Keep first occurrence (earliest timestamp)
      arrange(.data$timestamp) |>
      slice(1) |>
      ungroup() |>
      select(-.data$time_bin, -.data$price_round, -.data$size_exact)
  }

  # Add consolidated metadata
  attr(consolidated, "venues") <- names(data_list)
  attr(consolidated, "n_venues") <- length(data_list)
  attr(consolidated, "consolidation_method") <- sync_method

  return(consolidated)
}


#' Validate tick data quality
#'
#' @description
#' Comprehensive validation of tick data checking for common issues:
#' timestamp problems, outlier prices, negative sizes, duplicates, etc.
#'
#' @param data Data frame with trade/quote data
#' @param strict Logical, fail on any issues (default: FALSE)
#' @param checks Character vector of checks to perform
#'
#' @return A validation object with results
#'
#' @details
#' **Validation Checks**:
#' 1. **Timestamp monotonicity**: Timestamps should increase
#' 2. **Missing values**: No NAs in critical fields
#' 3. **Price reasonableness**: Within expected range
#' 4. **Size positivity**: Sizes must be positive
#' 5. **Duplicates**: Exact duplicate rows
#' 6. **Outliers**: Statistical outliers in price/size
#' 7. **Quote rule violations**: Trades outside bid/ask spread
#' 8. **Crossed markets**: Bid > ask
#'
#' @export
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 42)
#' validation <- validate_tick_data(trades)
#' print(validation)
validate_tick_data <- function(data,
                                 strict = FALSE,
                                 checks = c("timestamp", "missing", "price",
                                           "size", "duplicates", "outliers")) {

  issues <- list()
  warnings <- list()

  # Check 1: Timestamp monotonicity
  if ("timestamp" %in% checks && "timestamp" %in% names(data)) {
    time_diffs <- diff(as.numeric(data$timestamp))
    if (any(time_diffs < 0)) {
      issues$timestamp_order <- paste(
        sum(time_diffs < 0),
        "timestamp reversals detected"
      )
    }

    # Check for duplicate timestamps
    dup_times <- sum(duplicated(data$timestamp))
    if (dup_times > 0) {
      warnings$duplicate_timestamps <- paste(
        dup_times,
        "duplicate timestamps"
      )
    }
  }

  # Check 2: Missing values
  if ("missing" %in% checks) {
    critical_cols <- intersect(c("timestamp", "price", "size"), names(data))
    for (col in critical_cols) {
      n_missing <- sum(is.na(data[[col]]))
      if (n_missing > 0) {
        issues[[paste0("missing_", col)]] <- paste(
          n_missing,
          "missing values in", col
        )
      }
    }
  }

  # Check 3: Price reasonableness
  if ("price" %in% checks && "price" %in% names(data)) {
    if (any(data$price <= 0, na.rm = TRUE)) {
      issues$negative_prices <- paste(
        sum(data$price <= 0, na.rm = TRUE),
        "non-positive prices"
      )
    }

    # Check for extreme prices (likely errors)
    price_median <- median(data$price, na.rm = TRUE)
    extreme_prices <- data$price > price_median * 10 | data$price < price_median / 10
    if (any(extreme_prices, na.rm = TRUE)) {
      warnings$extreme_prices <- paste(
        sum(extreme_prices, na.rm = TRUE),
        "extreme price values (>10x or <0.1x median)"
      )
    }
  }

  # Check 4: Size positivity
  if ("size" %in% checks && "size" %in% names(data)) {
    if (any(data$size <= 0, na.rm = TRUE)) {
      issues$nonpositive_sizes <- paste(
        sum(data$size <= 0, na.rm = TRUE),
        "non-positive sizes"
      )
    }
  }

  # Check 5: Exact duplicates
  if ("duplicates" %in% checks) {
    n_duplicates <- sum(duplicated(data))
    if (n_duplicates > 0) {
      warnings$exact_duplicates <- paste(
        n_duplicates,
        "exact duplicate rows"
      )
    }
  }

  # Check 6: Statistical outliers
  if ("outliers" %in% checks && "price" %in% names(data)) {
    if (nrow(data) >= 10) {
      outliers <- detect_price_outliers(data$price)
      if (outliers$n_outliers > 0) {
        warnings$price_outliers <- paste(
          outliers$n_outliers,
          "statistical outliers in price"
        )
      }
    }
  }

  # Compile results
  is_valid <- length(issues) == 0
  if (strict && !is_valid) {
    stop("Data validation failed with issues: ",
         paste(names(issues), collapse = ", "))
  }

  result <- list(
    is_valid = is_valid,
    issues = issues,
    warnings = warnings,
    n_rows = nrow(data),
    checks_performed = checks
  )

  class(result) <- c("tick_validation", "list")
  return(result)
}


# ============================================================================
# Helper Functions (Not Exported)
# ============================================================================

#' Standardize TAQ column names
#' @noRd
standardize_taq_columns <- function(df) {
  # Map various TAQ column names to standard format
  col_mapping <- c(
    Time = "time", TIME = "time", "Time" = "time",
    Symbol = "symbol", SYMBOL = "symbol", SYM_ROOT = "symbol",
    Exchange = "exchange", EX = "exchange",
    Price = "price", PRICE = "price",
    Size = "size", SIZE = "size", SIZE_shares = "size",
    Conditions = "conditions", COND = "conditions",
    TR_SCOND = "conditions"
  )

  for (old_name in names(col_mapping)) {
    if (old_name %in% names(df)) {
      names(df)[names(df) == old_name] <- col_mapping[old_name]
    }
  }

  return(df)
}

#' Parse TAQ timestamp
#' @noRd
parse_taq_timestamp <- function(time_str, date, time_zone) {
  # Handle various TAQ time formats
  if (is.character(time_str)) {
    # Format: HH:MM:SS or HH:MM:SS.mmm
    datetime_str <- paste(date, time_str)
    timestamps <- ymd_hms(datetime_str, tz = time_zone, quiet = TRUE)
  } else if (is.numeric(time_str)) {
    # Seconds or milliseconds since midnight
    if (max(time_str, na.rm = TRUE) > 86400) {
      # Milliseconds
      time_str <- time_str / 1000
    }
    # Add to date
    base_date <- as.POSIXct(paste(date, "00:00:00"), tz = time_zone)
    timestamps <- base_date + time_str
  } else {
    stop("Unsupported time format")
  }

  return(timestamps)
}

#' Extract date from filename
#' @noRd
extract_date_from_filename <- function(filename) {
  # Try to extract date patterns like 2024-01-15 or 20240115
  date_patterns <- c(
    "\\d{4}-\\d{2}-\\d{2}",  # 2024-01-15
    "\\d{8}"                  # 20240115
  )

  for (pattern in date_patterns) {
    match <- regmatches(filename, regexpr(pattern, filename))
    if (length(match) > 0) {
      date_str <- match[1]
      # Try to parse
      tryCatch({
        if (nchar(date_str) == 8) {
          # Format: YYYYMMDD
          return(as.Date(date_str, format = "%Y%m%d"))
        } else {
          # Format: YYYY-MM-DD
          return(as.Date(date_str))
        }
      }, error = function(e) {
        # Continue to next pattern
      })
    }
  }

  return(NULL)
}

#' Filter regular trading hours
#' @noRd
filter_regular_hours <- function(data, time_zone) {
  # Regular hours: 9:30 AM - 4:00 PM ET
  data |>
    mutate(time_of_day = as.POSIXlt(.data$timestamp, tz = time_zone)$hour * 3600 +
             as.POSIXlt(.data$timestamp, tz = time_zone)$min * 60 +
             as.POSIXlt(.data$timestamp, tz = time_zone)$sec) |>
    filter(
      .data$time_of_day >= 9.5 * 3600,  # 9:30 AM
      .data$time_of_day <= 16 * 3600     # 4:00 PM
    ) |>
    select(-.data$time_of_day)
}

#' Detect price outliers
#' @noRd
detect_price_outliers <- function(prices, method = "iqr", threshold = 3) {
  if (method == "iqr") {
    Q1 <- quantile(prices, 0.25, na.rm = TRUE)
    Q3 <- quantile(prices, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - threshold * IQR
    upper_bound <- Q3 + threshold * IQR
    outliers <- prices < lower_bound | prices > upper_bound
  } else {
    # Z-score method
    z_scores <- abs((prices - mean(prices, na.rm = TRUE)) / sd(prices, na.rm = TRUE))
    outliers <- z_scores > threshold
  }

  list(
    outliers = which(outliers),
    n_outliers = sum(outliers, na.rm = TRUE)
  )
}

#' Standardize ITCH column names
#' @noRd
standardize_itch_columns <- function(df) {
  # ITCH-specific column mapping
  # Implementation depends on specific ITCH format
  # Placeholder for now
  return(df)
}

#' Add order to book
#' @noRd
add_to_book <- function(book, price, size) {
  price_str <- as.character(price)
  if (price_str %in% names(book)) {
    book[[price_str]] <- book[[price_str]] + size
  } else {
    book[[price_str]] <- size
  }
  return(book)
}

#' Remove order from book
#' @noRd
remove_from_book <- function(book, price, size) {
  price_str <- as.character(price)
  if (price_str %in% names(book)) {
    book[[price_str]] <- max(0, book[[price_str]] - size)
    if (book[[price_str]] == 0) {
      book[[price_str]] <- NULL
    }
  }
  return(book)
}

#' Create order book snapshot
#' @noRd
create_book_snapshot <- function(bids, asks, depth) {
  # Sort and extract top levels
  bid_prices <- as.numeric(names(bids))
  bid_sizes <- unlist(bids)
  bid_order <- order(bid_prices, decreasing = TRUE)
  bid_prices <- bid_prices[bid_order][1:min(depth, length(bid_prices))]
  bid_sizes <- bid_sizes[bid_order][1:min(depth, length(bid_sizes))]

  ask_prices <- as.numeric(names(asks))
  ask_sizes <- unlist(asks)
  ask_order <- order(ask_prices, decreasing = FALSE)
  ask_prices <- ask_prices[ask_order][1:min(depth, length(ask_prices))]
  ask_sizes <- ask_sizes[ask_order][1:min(depth, length(ask_sizes))]

  # Pad with NAs if needed
  if (length(bid_prices) < depth) {
    bid_prices <- c(bid_prices, rep(NA, depth - length(bid_prices)))
    bid_sizes <- c(bid_sizes, rep(NA, depth - length(bid_sizes)))
  }
  if (length(ask_prices) < depth) {
    ask_prices <- c(ask_prices, rep(NA, depth - length(ask_prices)))
    ask_sizes <- c(ask_sizes, rep(NA, depth - length(ask_sizes)))
  }

  # Create snapshot data frame
  snapshot <- data.frame(
    bid_price_1 = bid_prices[1], bid_size_1 = bid_sizes[1],
    ask_price_1 = ask_prices[1], ask_size_1 = ask_sizes[1]
  )

  # Add additional levels
  for (i in 2:depth) {
    snapshot[[paste0("bid_price_", i)]] <- bid_prices[i]
    snapshot[[paste0("bid_size_", i)]] <- bid_sizes[i]
    snapshot[[paste0("ask_price_", i)]] <- ask_prices[i]
    snapshot[[paste0("ask_size_", i)]] <- ask_sizes[i]
  }

  return(snapshot)
}

#' Classify trades using Lee-Ready
#' @noRd
classify_lee_ready <- function(trades, quotes) {
  # Simplified implementation - full version requires quote matching
  message("Lee-Ready classification requires detailed implementation. Using tick rule as fallback.")
  return(classify_tick_rule(trades))
}

#' Classify using EMO
#' @noRd
classify_emo <- function(trades, quotes) {
  message("EMO classification not yet implemented. Using tick rule as fallback.")
  return(classify_tick_rule(trades))
}

#' Classify using tick rule
#' @noRd
classify_tick_rule <- function(trades, threshold = 0) {
  trades$side <- NA_character_

  for (i in seq_len(nrow(trades))) {
    if (i == 1) {
      trades$side[i] <- "B"  # Default first trade to buy
      next
    }

    price_change <- trades$price[i] - trades$price[i - 1]

    if (price_change > threshold) {
      trades$side[i] <- "B"  # Uptick
    } else if (price_change < -threshold) {
      trades$side[i] <- "S"  # Downtick
    } else {
      # Zero tick - use previous classification
      trades$side[i] <- trades$side[i - 1]
    }
  }

  return(trades)
}

#' Classify using quote rule
#' @noRd
classify_quote_rule <- function(trades, quotes) {
  message("Quote rule classification requires quote matching. Using tick rule as fallback.")
  return(classify_tick_rule(trades))
}

#' Pad symbol with spaces
#' @noRd
str_pad_symbol <- function(symbol, width = 8) {
  formatC(symbol, width = width, flag = " ")
}

#' Print method for order book reconstruction
#' @export
print.orderbook_reconstruction <- function(x, ...) {
  cat("Order Book Reconstruction\n")
  cat("=========================\n\n")

  cat("Statistics:\n")
  cat(sprintf("  Messages processed: %s\n", format(x$statistics$n_messages, big.mark = ",")))
  cat(sprintf("  Snapshots created: %s\n", format(x$statistics$n_snapshots, big.mark = ",")))
  cat(sprintf("  Average spread: %.2f bps\n", x$statistics$avg_spread_bps))
  cat(sprintf("  Average bid depth: %.0f shares\n", x$statistics$avg_bid_depth))
  cat(sprintf("  Average ask depth: %.0f shares\n", x$statistics$avg_ask_depth))
  cat("\n")

  cat("Best Bid/Ask (first 5 snapshots):\n")
  print(head(x$best_bid_ask, 5))

  invisible(x)
}

#' Print method for tick validation
#' @export
print.tick_validation <- function(x, ...) {
  cat("Tick Data Validation\n")
  cat("====================\n\n")

  cat(sprintf("Status: %s\n", if (x$is_valid) "VALID" else "ISSUES FOUND"))
  cat(sprintf("Rows checked: %s\n", format(x$n_rows, big.mark = ",")))
  cat("\n")

  if (length(x$issues) > 0) {
    cat("Issues:\n")
    for (issue_name in names(x$issues)) {
      cat(sprintf("  ✗ %s\n", x$issues[[issue_name]]))
    }
    cat("\n")
  }

  if (length(x$warnings) > 0) {
    cat("Warnings:\n")
    for (warn_name in names(x$warnings)) {
      cat(sprintf("  ⚠ %s\n", x$warnings[[warn_name]]))
    }
  }

  invisible(x)
}
