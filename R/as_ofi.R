#' Coerce and standardize trade data for OFI computation
#'
#' @description 
#' Converts various trade data formats into a standardized tibble suitable 
#' for OFI calculations. Handles different side encodings, validates data types,
#' and ensures proper timezone handling.
#'
#' @param data A data frame containing trade-level events
#' @param side_map Named character vector mapping buy/sell labels (default: c(B="B", S="S"))
#' @param time_col Name of the timestamp column (default: "timestamp")
#' @param side_col Name of the side/direction column (default: "side")
#' @param size_col Name of the trade size column (default: "size")
#' @param price_col Name of the price column (optional, default: NULL)
#' @param tz Timezone to use if timestamps lack timezone info (default: NULL assumes UTC)
#'
#' @return A tibble with standardized columns: timestamp, side, size, and optionally price
#' 
#' @details
#' The function accepts various side encodings:
#' - Character: "B"/"S", "buy"/"sell", "BUY"/"SELL"
#' - Numeric: 1/-1 (1 for buy, -1 for sell)
#' 
#' Missing values in timestamp or size columns are dropped with a warning.
#' If timezone is not specified in the data and tz parameter is NULL, UTC is assumed with a warning.
#'
#' @export
#' @importFrom dplyr select rename mutate filter
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate is.POSIXct with_tz
#' @importFrom rlang .data := !! sym
#'
#' @examples
#' # Create sample data
#' trades <- data.frame(
#'   timestamp = Sys.time() + 1:10,
#'   side = c("B", "S", "B", "B", "S", "B", "S", "S", "B", "B"),
#'   size = runif(10, 100, 1000)
#' )
#' 
#' # Standardize the data
#' ofi_data <- as_ofi(trades)
#' 
#' # With numeric side encoding
#' trades_numeric <- data.frame(
#'   time = Sys.time() + 1:10,
#'   direction = c(1, -1, 1, 1, -1, 1, -1, -1, 1, 1),
#'   volume = runif(10, 100, 1000)
#' )
#' 
#' ofi_data <- as_ofi(
#'   trades_numeric,
#'   time_col = "time",
#'   side_col = "direction",
#'   size_col = "volume"
#' )
as_ofi <- function(data,
                   side_map = c(B = "B", S = "S"),
                   time_col = "timestamp",
                   side_col = "side",
                   size_col = "size",
                   price_col = NULL,
                   tz = NULL) {
  
  # Check required columns exist
  required_cols <- c(time_col, side_col, size_col)
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # Start building standardized data
  result <- data |>
    as_tibble()
  
  # Rename columns to standard names
  result <- result |>
    rename(
      timestamp = !!sym(time_col),
      side = !!sym(side_col),
      size = !!sym(size_col)
    )
  
  # Add price column if specified
  if (!is.null(price_col)) {
    if (!price_col %in% names(data)) {
      stop("Specified price column '", price_col, "' not found in data")
    }
    result <- result |>
      mutate(price = !!sym(price_col))
  }
  
  # Handle missing values
  n_before <- nrow(result)
  result <- result |>
    filter(!is.na(.data$timestamp), !is.na(.data$size))
  n_after <- nrow(result)
  
  if (n_before > n_after) {
    warning("Dropped ", n_before - n_after, " rows with missing timestamp or size values")
  }
  
  # Standardize timestamps
  if (!lubridate::is.POSIXct(result$timestamp)) {
    result$timestamp <- as.POSIXct(result$timestamp)
  }
  
  # Handle timezone
  if (is.null(attr(result$timestamp, "tzone")) || attr(result$timestamp, "tzone") == "") {
    if (is.null(tz)) {
      warning("No timezone specified for timestamps, assuming UTC")
      tz <- "UTC"
    }
    result$timestamp <- lubridate::with_tz(result$timestamp, tz)
  }
  
  # Standardize side encoding
  result <- standardize_side(result, side_map)
  
  # Select final columns
  final_cols <- c("timestamp", "side", "size")
  if ("price" %in% names(result)) {
    final_cols <- c(final_cols, "price")
  }
  
  result |>
    select(all_of(final_cols))
}

#' Internal function to standardize side encoding
#' @noRd
standardize_side <- function(data, side_map) {
  unique_sides <- unique(data$side)
  
  # Handle numeric encoding (1/-1)
  if (is.numeric(data$side)) {
    if (!all(unique_sides %in% c(1, -1, NA))) {
      stop("Numeric side values must be 1 (buy) or -1 (sell)")
    }
    data$side <- ifelse(data$side == 1, side_map["B"], side_map["S"])
    return(data)
  }
  
  # Handle character encoding
  data$side <- as.character(data$side)
  data$side <- toupper(data$side)
  
  # Common mappings
  buy_labels <- c("B", "BUY", "BID", side_map["B"])
  sell_labels <- c("S", "SELL", "ASK", "OFFER", side_map["S"])
  
  # Map to standard B/S
  data$side[data$side %in% buy_labels] <- side_map["B"]
  data$side[data$side %in% sell_labels] <- side_map["S"]
  
  # Check for unmapped values
  unmapped <- setdiff(unique(data$side), c(side_map["B"], side_map["S"], NA))
  if (length(unmapped) > 0) {
    stop("Unknown side values: ", paste(unmapped, collapse = ", "),
         ". Expected variations of buy/sell or B/S")
  }
  
  data
}
