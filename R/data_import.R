#' Read LOBSTER Trade Data
#'
#' @description
#' Reads trade data from LOBSTER (Limit Order Book System - The Efficient Reconstructor)
#' format, a widely-used academic dataset for high-frequency trading research.
#'
#' LOBSTER provides two files per stock-date:
#' - Message file: All events (trades, limit orders, cancellations)
#' - Orderbook file: Limit order book snapshots
#'
#' This function reads the message file and extracts trade events.
#'
#' @param file_path Path to LOBSTER message file (typically *_message_*.csv)
#' @param date Trading date (used for timestamp reconstruction)
#' @param tz Timezone for timestamps (default: "America/New_York")
#' @param filter_trades_only Logical, whether to return only executed trades (default: TRUE)
#'
#' @return A tibble with standardized columns:
#'   - timestamp: POSIXct timestamp
#'   - side: Trade side ("B" for buy, "S" for sell)
#'   - size: Trade size (shares)
#'   - price: Execution price
#'   - event_type: LOBSTER event type (if filter_trades_only = FALSE)
#'
#' @details
#' LOBSTER message file format (columns):
#' 1. Time (seconds from midnight)
#' 2. Event type (1=submission, 2=cancellation, 3=deletion, 4=execution visible, 5=execution hidden, 7=trading halt)
#' 3. Order ID
#' 4. Size (shares)
#' 5. Price (dollars)
#' 6. Direction (1=buy, -1=sell)
#'
#' Event types for trades:
#' - Type 4: Execution of visible limit order
#' - Type 5: Execution of hidden order
#'
#' LOBSTER data is available from: https://lobsterdata.com/
#'
#' @export
#' @importFrom readr read_csv cols col_double col_integer
#' @importFrom dplyr mutate filter select
#' @importFrom lubridate as_datetime ymd
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Read LOBSTER trade data for AAPL on 2024-01-15
#' trades <- read_lobster_trades(
#'   file_path = "AAPL_2024-01-15_34200000_57600000_message_10.csv",
#'   date = "2024-01-15"
#' )
#'
#' # Read all events (not just trades)
#' all_events <- read_lobster_trades(
#'   file_path = "AAPL_2024-01-15_34200000_57600000_message_10.csv",
#'   date = "2024-01-15",
#'   filter_trades_only = FALSE
#' )
#' }
read_lobster_trades <- function(file_path,
                                date,
                                tz = "America/New_York",
                                filter_trades_only = TRUE) {

  # Validate inputs
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Parse date
  trade_date <- tryCatch(
    lubridate::ymd(date),
    error = function(e) {
      stop("Invalid date format. Use 'YYYY-MM-DD' (e.g., '2024-01-15')")
    }
  )

  # Read LOBSTER message file
  # Suppress messages about column types
  message("Reading LOBSTER data from: ", basename(file_path))

  lobster_data <- readr::read_csv(
    file_path,
    col_names = c("time", "event_type", "order_id", "size", "price", "direction"),
    col_types = readr::cols(
      time = readr::col_double(),
      event_type = readr::col_integer(),
      order_id = readr::col_double(),
      size = readr::col_integer(),
      price = readr::col_double(),
      direction = readr::col_integer()
    ),
    show_col_types = FALSE
  )

  message("Loaded ", nrow(lobster_data), " events")

  # Convert time (seconds from midnight) to POSIXct timestamp
  lobster_data <- lobster_data |>
    mutate(
      timestamp = lubridate::as_datetime(trade_date) + .data$time,
      timestamp = lubridate::force_tz(.data$timestamp, tz)
    )

  # Filter for trade events if requested
  if (filter_trades_only) {
    # Event types 4 and 5 are executions
    lobster_data <- lobster_data |>
      filter(.data$event_type %in% c(4, 5))

    message("Filtered to ", nrow(lobster_data), " trade executions")
  }

  # Standardize side encoding (1 = buy, -1 = sell in LOBSTER)
  lobster_data <- lobster_data |>
    mutate(
      side = ifelse(.data$direction == 1, "B", "S")
    )

  # Select and reorder columns
  result_cols <- c("timestamp", "side", "size", "price")
  if (!filter_trades_only) {
    result_cols <- c(result_cols, "event_type", "order_id", "direction")
  }

  result <- lobster_data |>
    select(all_of(result_cols))

  # Add metadata as attributes
  attr(result, "source") <- "LOBSTER"
  attr(result, "date") <- as.character(trade_date)
  attr(result, "timezone") <- tz
  attr(result, "n_trades") <- nrow(result)

  message("✓ Successfully loaded LOBSTER data")

  result
}


#' Read Generic Trade Data from CSV
#'
#' @description
#' Reads trade-level data from a generic CSV file with flexible column mapping.
#' Provides helpful validation and automatic data type detection.
#'
#' @param file_path Path to CSV file
#' @param time_col Name or index of timestamp column (default: 1)
#' @param side_col Name or index of side/direction column (default: 2)
#' @param size_col Name or index of size/volume column (default: 3)
#' @param price_col Name or index of price column (default: 4, optional)
#' @param time_format Format of timestamp column: "unix", "datetime", "time_only", or lubridate format string
#' @param date Date for "time_only" format (e.g., "2024-01-15")
#' @param tz Timezone (default: "UTC")
#' @param side_mapping Named vector mapping file values to "B"/"S" (auto-detected if NULL)
#' @param validate Logical, whether to validate data after loading (default: TRUE)
#' @param ... Additional arguments passed to readr::read_csv()
#'
#' @return A tibble with standardized columns suitable for rOFI analysis
#'
#' @details
#' This function handles various common trade data formats:
#'
#' **Timestamp formats:**
#' - "unix": Unix epoch (seconds since 1970-01-01)
#' - "datetime": Standard datetime strings (auto-parsed)
#' - "time_only": Time of day (requires `date` parameter)
#' - Custom: Any lubridate format string (e.g., "\%Y-\%m-\%d \%H:\%M:\%S")
#'
#' **Side encoding auto-detection:**
#' - Recognizes: "B"/"S", "BUY"/"SELL", "buy"/"sell", 1/-1, "bid"/"ask"
#' - Custom mappings: Use `side_mapping` parameter
#'
#' **Validation:**
#' - Checks for required columns
#' - Validates data types
#' - Detects common issues (duplicates, missing values, non-chronological order)
#' - Can be disabled with `validate = FALSE` for speed
#'
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr select rename mutate
#' @importFrom lubridate as_datetime parse_date_time force_tz
#'
#' @examples
#' \dontrun{
#' # ===========================================
#' # Example 1: Preview Before Loading
#' # ===========================================
#'
#' # Always preview first to understand your data
#' preview_trade_file("my_trades.csv")
#'
#' # ===========================================
#' # Example 2: Simple CSV with Header Row
#' # ===========================================
#'
#' trades <- read_trade_csv(
#'   "my_trades.csv",
#'   time_col = "timestamp",
#'   side_col = "side",
#'   size_col = "volume",
#'   price_col = "price"
#' )
#'
#' # Validate the loaded data
#' validate_trade_data(trades)
#'
#' # ===========================================
#' # Example 3: CSV with Numeric Columns
#' # ===========================================
#'
#' # When your CSV has no header or you prefer column numbers
#' trades <- read_trade_csv(
#'   "trades_noheader.csv",
#'   time_col = 1,        # First column is timestamp
#'   side_col = 2,        # Second column is side
#'   size_col = 3,        # Third column is size
#'   price_col = 4,       # Fourth column is price
#'   col_names = FALSE    # No header row
#' )
#'
#' # ===========================================
#' # Example 4: Unix Timestamp Format
#' # ===========================================
#'
#' # If timestamps are in Unix epoch format
#' trades <- read_trade_csv(
#'   "unix_data.csv",
#'   time_col = 1,
#'   side_col = 2,
#'   size_col = 3,
#'   time_format = "unix",
#'   tz = "America/New_York"
#' )
#'
#' # ===========================================
#' # Example 5: Intraday Time-Only Data
#' # ===========================================
#'
#' # When data only has time (e.g., "09:30:01") but no date
#' trades <- read_trade_csv(
#'   "intraday.csv",
#'   time_col = "time",
#'   side_col = "direction",
#'   size_col = "qty",
#'   time_format = "time_only",
#'   date = "2024-01-15",  # Specify the trading date
#'   tz = "America/New_York"
#' )
#'
#' # ===========================================
#' # Example 6: Custom Side Mappings
#' # ===========================================
#'
#' # If your data uses BID/ASK instead of B/S
#' trades <- read_trade_csv(
#'   "data.csv",
#'   time_col = "timestamp",
#'   side_col = "direction",
#'   size_col = "volume",
#'   side_mapping = c("BID" = "B", "ASK" = "S")
#' )
#'
#' # Or if using numeric indicators
#' trades <- read_trade_csv(
#'   "data.csv",
#'   time_col = 1,
#'   side_col = 2,
#'   size_col = 3,
#'   side_mapping = c("1" = "B", "-1" = "S")
#' )
#'
#' # ===========================================
#' # Example 7: Complete Workflow
#' # ===========================================
#'
#' # Step 1: Preview
#' preview_trade_file("my_data.csv")
#'
#' # Step 2: Load
#' trades <- read_trade_csv(
#'   "my_data.csv",
#'   time_col = "timestamp",
#'   side_col = "side",
#'   size_col = "size",
#'   tz = "America/New_York"
#' )
#'
#' # Step 3: Validate
#' validation <- validate_trade_data(trades)
#' print(validation)
#'
#' # Step 4: Clean if needed
#' if (!validation$valid) {
#'   trades <- clean_trade_data(trades)
#' }
#'
#' # Step 5: Compute OFI
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # ===========================================
#' # Example 8: Different Data Providers
#' # ===========================================
#'
#' # Bloomberg Terminal exports
#' trades_bb <- read_trade_csv(
#'   "bloomberg.csv",
#'   time_col = "Time",
#'   side_col = "Side",
#'   size_col = "Size",
#'   time_format = "time_only",
#'   date = "2024-01-15",
#'   side_mapping = c("BID" = "B", "ASK" = "S")
#' )
#'
#' # Interactive Brokers (IB) TWS exports
#' # Note: IB often has separate Date and Time columns
#' # You may need to preprocess with read.csv() first
#' }
#'
#' @seealso
#' \code{\link{preview_trade_file}} to inspect files before loading,
#' \code{\link{validate_trade_data}} to check data quality,
#' \code{\link{clean_trade_data}} to fix common issues,
#' \code{\link{read_lobster_trades}} for LOBSTER format
read_trade_csv <- function(file_path,
                           time_col = 1,
                           side_col = 2,
                           size_col = 3,
                           price_col = 4,
                           time_format = c("datetime", "unix", "time_only", "custom"),
                           date = NULL,
                           tz = "UTC",
                           side_mapping = NULL,
                           validate = TRUE,
                           ...) {

  time_format <- match.arg(time_format)

  # Validate file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  message("Reading CSV from: ", basename(file_path))

  # Read CSV with automatic type detection
  data <- readr::read_csv(file_path, show_col_types = FALSE, ...)

  message("Loaded ", nrow(data), " rows, ", ncol(data), " columns")

  # Helper to get column by name or index
  get_col <- function(col_spec, data) {
    if (is.numeric(col_spec)) {
      if (col_spec > ncol(data)) {
        stop("Column index ", col_spec, " exceeds number of columns (", ncol(data), ")")
      }
      return(names(data)[col_spec])
    } else {
      if (!col_spec %in% names(data)) {
        stop("Column '", col_spec, "' not found in data")
      }
      return(col_spec)
    }
  }

  # Get actual column names
  time_col_name <- get_col(time_col, data)
  side_col_name <- get_col(side_col, data)
  size_col_name <- get_col(size_col, data)

  price_col_name <- NULL
  if (!is.null(price_col)) {
    price_col_name <- get_col(price_col, data)
  }

  # Extract columns
  result <- data |>
    select(
      time_raw = all_of(time_col_name),
      side_raw = all_of(side_col_name),
      size = all_of(size_col_name),
      if (!is.null(price_col_name)) price = all_of(price_col_name)
    )

  # Convert timestamp
  message("Converting timestamps (format: ", time_format, ")...")

  result <- result |>
    mutate(
      timestamp = convert_timestamp(.data$time_raw, time_format, date, tz)
    ) |>
    select(-.data$time_raw)

  # Standardize side encoding
  message("Standardizing side encoding...")

  if (is.null(side_mapping)) {
    # Auto-detect side mapping
    side_mapping <- detect_side_mapping(result$side_raw)
    message("Auto-detected side mapping: ", paste(names(side_mapping), "->", side_mapping, collapse = ", "))
  }

  result <- result |>
    mutate(
      side = map_side_values(.data$side_raw, side_mapping)
    ) |>
    select(-.data$side_raw)

  # Reorder columns
  col_order <- c("timestamp", "side", "size")
  if (!is.null(price_col_name)) {
    col_order <- c(col_order, "price")
  }

  result <- result |>
    select(all_of(col_order))

  # Validate if requested
  if (validate) {
    message("Validating data...")
    validation <- validate_trade_data(result, strict = FALSE)

    if (!validation$valid) {
      warning("Data validation found issues:\n  ",
              paste(validation$issues, collapse = "\n  "))
    } else {
      message("✓ Data validation passed")
    }
  }

  # Add metadata
  attr(result, "source") <- "CSV"
  attr(result, "file") <- basename(file_path)
  attr(result, "n_trades") <- nrow(result)

  message("✓ Successfully loaded ", nrow(result), " trades")

  result
}


#' Convert timestamp to POSIXct
#' @noRd
convert_timestamp <- function(time_raw, format, date = NULL, tz = "UTC") {
  result <- switch(format,
    unix = {
      lubridate::as_datetime(time_raw, tz = tz)
    },
    datetime = {
      # Try automatic parsing first
      ts <- lubridate::parse_date_time(time_raw,
                                       orders = c("ymd HMS", "mdy HMS", "dmy HMS",
                                                 "ymd HM", "mdy HM", "dmy HM"),
                                       tz = tz)
      if (all(is.na(ts))) {
        stop("Could not parse datetime. Consider specifying a custom format.")
      }
      ts
    },
    time_only = {
      if (is.null(date)) {
        stop("date parameter required for time_only format")
      }
      base_date <- lubridate::ymd(date)
      # Assume time_raw is in seconds or HH:MM:SS format
      if (is.numeric(time_raw)) {
        # Seconds from midnight
        lubridate::as_datetime(base_date) + time_raw
      } else {
        # Parse time string
        lubridate::parse_date_time(paste(date, time_raw),
                                   orders = "ymd HMS",
                                   tz = tz)
      }
    },
    custom = {
      stop("Custom format not yet implemented. Use datetime format with lubridate-compatible strings.")
    }
  )

  # Ensure timezone
  if (is.null(attr(result, "tzone")) || attr(result, "tzone") == "") {
    result <- lubridate::force_tz(result, tz)
  }

  result
}


#' Detect side mapping from data
#' @noRd
detect_side_mapping <- function(side_values) {
  unique_values <- unique(side_values)

  # Check for numeric encoding
  if (all(unique_values %in% c(1, -1, NA))) {
    return(c("1" = "B", "-1" = "S"))
  }

  # Convert to uppercase for matching
  unique_upper <- toupper(as.character(unique_values))

  # Define known mappings
  buy_labels <- c("B", "BUY", "BID")
  sell_labels <- c("S", "SELL", "ASK", "OFFER")

  # Create mapping
  mapping <- character()

  for (val in unique_values) {
    val_str <- as.character(val)
    val_upper <- toupper(val_str)

    if (val_upper %in% buy_labels) {
      mapping[val_str] <- "B"
    } else if (val_upper %in% sell_labels) {
      mapping[val_str] <- "S"
    } else {
      stop("Unknown side value: '", val_str, "'. Please provide side_mapping parameter.")
    }
  }

  mapping
}


#' Map side values using mapping
#' @noRd
map_side_values <- function(side_raw, mapping) {
  side_str <- as.character(side_raw)

  result <- character(length(side_str))

  for (i in seq_along(side_str)) {
    val <- side_str[i]

    if (is.na(val)) {
      result[i] <- NA_character_
    } else if (val %in% names(mapping)) {
      result[i] <- mapping[val]
    } else {
      # Try case-insensitive matching
      val_upper <- toupper(val)
      mapping_upper <- toupper(names(mapping))

      match_idx <- which(mapping_upper == val_upper)

      if (length(match_idx) > 0) {
        result[i] <- mapping[match_idx[1]]
      } else {
        stop("Side value '", val, "' not found in mapping")
      }
    }
  }

  result
}


#' Quick Preview of Trade Data File
#'
#' @description
#' Quickly inspect the structure and contents of a trade data file
#' without loading the entire dataset. Useful for determining column
#' mappings and data formats.
#'
#' @param file_path Path to data file
#' @param n_rows Number of rows to preview (default: 10)
#' @param show_summary Logical, whether to show summary statistics (default: TRUE)
#'
#' @return Invisibly returns the preview data
#'
#' @export
#' @importFrom readr read_csv
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' # Preview first 10 rows
#' preview_trade_file("my_data.csv")
#'
#' # Preview first 20 rows without summary
#' preview_trade_file("my_data.csv", n_rows = 20, show_summary = FALSE)
#' }
preview_trade_file <- function(file_path, n_rows = 10, show_summary = TRUE) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  cat("File Preview:", basename(file_path), "\n")
  cat(strrep("=", 60), "\n\n")

  # Read first n_rows
  data <- readr::read_csv(file_path, n_max = n_rows, show_col_types = FALSE)

  # Show dimensions
  cat("Dimensions:", nrow(data), "rows (preview),", ncol(data), "columns\n\n")

  # Show column names and types
  cat("Columns:\n")
  for (i in seq_along(data)) {
    col_type <- class(data[[i]])[1]
    cat(sprintf("  [%d] %-20s %s\n", i, names(data)[i], col_type))
  }
  cat("\n")

  # Show first few rows
  cat("First", min(n_rows, nrow(data)), "rows:\n")
  print(head(data, n_rows))
  cat("\n")

  # Summary statistics
  if (show_summary) {
    cat("Summary:\n")

    # Check for potential timestamp columns
    time_cols <- names(data)[sapply(data, function(x) {
      is.numeric(x) || inherits(x, "POSIXct") || inherits(x, "Date")
    })]

    if (length(time_cols) > 0) {
      cat("  Potential time columns:", paste(time_cols, collapse = ", "), "\n")
    }

    # Check for potential side columns
    side_candidates <- names(data)[sapply(data, function(x) {
      length(unique(x)) <= 10 && !is.numeric(x)
    })]

    if (length(side_candidates) > 0) {
      cat("  Potential side columns:", paste(side_candidates, collapse = ", "), "\n")

      for (col in side_candidates[1:min(2, length(side_candidates))]) {
        cat("    ", col, "values:", paste(unique(data[[col]]), collapse = ", "), "\n")
      }
    }

    cat("\n")
  }

  # Helpful tips
  cat("Tips for reading this file:\n")
  cat("  - Use column names or indices (e.g., time_col = 'timestamp' or time_col = 1)\n")
  cat("  - Specify time_format if timestamps aren't standard\n")
  cat("  - Use side_mapping if side values are non-standard\n")
  cat("\nExample:\n")
  cat("  read_trade_csv('", basename(file_path), "',\n", sep = "")
  cat("                 time_col = 1, side_col = 2, size_col = 3, price_col = 4)\n")

  invisible(data)
}
