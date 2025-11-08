#' Regulatory Surveillance and Market Manipulation Detection
#'
#' @description
#' Tools for detecting manipulative trading patterns including spoofing,
#' layering, and quote stuffing. Designed for compliance teams, regulators,
#' and market surveillance systems. Implements SEC/FINRA best practices and
#' MiFID II requirements.
#'
#' @name surveillance
NULL

#' Detect spoofing behavior in order flow
#'
#' @description
#' Identifies patterns consistent with spoofing: placing large orders with
#' no intent to execute, designed to move prices. Detects rapid cancellation
#' of non-marketable limit orders after opposite-side execution.
#'
#' @param messages Data frame with order messages (add, cancel, execute)
#' @param cancel_threshold Numeric, suspicious cancellation rate (default: 0.70)
#' @param size_threshold Numeric, minimum order size percentile (default: 0.90)
#' @param time_window Numeric, window for cancel analysis in seconds (default: 60)
#' @param min_orders Integer, minimum orders to flag (default: 5)
#' @param return_details Logical, return detailed analysis (default: FALSE)
#'
#' @return A list containing:
#'   \describe{
#'     \item{is_suspicious}{Logical, whether spoofing detected}
#'     \item{spoof_score}{Numeric 0-100, spoofing likelihood}
#'     \item{flagged_periods}{Data frame of suspicious time windows}
#'     \item{statistics}{Summary statistics}
#'     \item{alerts}{Data frame of specific alerts}
#'   }
#'
#' @details
#' **Spoofing Pattern Recognition**:
#'
#' Classic spoofing involves:
#' 1. **Large non-marketable limit orders** (bid below market or ask above)
#' 2. **High cancellation rate** (70-95% vs. normal 30-50%)
#' 3. **Quick cancellation** after opposite-side execution
#' 4. **Repeated pattern** across multiple instances
#'
#' **Detection Algorithm**:
#' - Identify large orders (size > 90th percentile)
#' - Track cancellation vs. execution rates
#' - Measure time to cancel after opposite-side activity
#' - Flag coordinated patterns
#'
#' **Red Flags**:
#' - Cancel rate > 70%
#' - Average time to cancel < 5 seconds
#' - Cancels concentrated after opposite-side trades
#' - Order size >> typical market activity
#'
#' **Regulatory Context**:
#' - SEC Rule 10b-5 (anti-fraud)
#' - Dodd-Frank Anti-Manipulation Authority
#' - MiFID II Market Abuse Regulation (MAR)
#' - JPMorgan $920M spoofing fine (2020)
#'
#' @references
#' SEC Spoofing Cases:
#' https://www.sec.gov/litigation/litreleases/2020/lr24790.htm
#'
#' CFTC Guidance on Disruptive Trading Practices
#'
#' @export
#' @importFrom dplyr group_by summarise filter mutate arrange
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#' # Analyze order messages for spoofing
#' messages <- read_itch_messages("AAPL_messages.csv", symbol = "AAPL")
#' spoof_result <- detect_spoofing(messages)
#'
#' if (spoof_result$is_suspicious) {
#'   print(spoof_result$flagged_periods)
#'   print(spoof_result$alerts)
#' }
#'
#' # Custom thresholds for conservative detection
#' spoof_result <- detect_spoofing(
#'   messages,
#'   cancel_threshold = 0.80,  # 80% cancellation rate
#'   size_threshold = 0.95,     # Top 5% of orders
#'   time_window = 30           # 30-second windows
#' )
#' }
detect_spoofing <- function(messages,
                             cancel_threshold = 0.70,
                             size_threshold = 0.90,
                             time_window = 60,
                             min_orders = 5,
                             return_details = FALSE) {

  # Validate input
  required_cols <- c("timestamp", "message_type", "side", "price", "size")
  missing <- setdiff(required_cols, names(messages))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  if (nrow(messages) < min_orders) {
    warning("Insufficient messages for spoofing detection")
    return(list(
      is_suspicious = FALSE,
      spoof_score = 0,
      flagged_periods = data.frame(),
      statistics = list(n_messages = nrow(messages)),
      alerts = data.frame()
    ))
  }

  # Identify large orders (potential spoofs)
  size_cutoff <- quantile(messages$size, probs = size_threshold, na.rm = TRUE)

  # Classify message types
  messages <- messages |>
    mutate(
      is_add = .data$message_type %in% c("A", "F", "ADD", "LIMIT"),
      is_cancel = .data$message_type %in% c("X", "D", "CANCEL", "DELETE"),
      is_execute = .data$message_type %in% c("E", "C", "EXECUTE", "TRADE"),
      is_large = .data$size >= size_cutoff
    )

  # Track order lifecycle if order IDs available
  if ("order_id" %in% names(messages)) {
    order_stats <- analyze_order_lifecycle(messages)
  } else {
    # Aggregate analysis without order IDs
    order_stats <- analyze_aggregate_patterns(messages, time_window)
  }

  # Calculate spoofing metrics
  add_orders <- sum(messages$is_add, na.rm = TRUE)
  cancel_orders <- sum(messages$is_cancel, na.rm = TRUE)
  execute_orders <- sum(messages$is_execute, na.rm = TRUE)

  large_adds <- sum(messages$is_add & messages$is_large, na.rm = TRUE)
  large_cancels <- sum(messages$is_cancel & messages$is_large, na.rm = TRUE)

  # Overall cancellation rate
  if (add_orders > 0) {
    cancel_rate <- cancel_orders / add_orders
    large_cancel_rate <- if (large_adds > 0) large_cancels / large_adds else 0
  } else {
    cancel_rate <- 0
    large_cancel_rate <- 0
  }

  # Order-to-trade ratio
  otr <- if (execute_orders > 0) (add_orders + cancel_orders) / execute_orders else Inf

  # Spoofing score (0-100)
  spoof_score <- calculate_spoof_score(
    cancel_rate = cancel_rate,
    large_cancel_rate = large_cancel_rate,
    otr = otr,
    order_stats = order_stats
  )

  # Flag as suspicious if score exceeds threshold
  is_suspicious <- (spoof_score >= 60) || (large_cancel_rate >= cancel_threshold)

  # Identify specific suspicious periods
  flagged_periods <- identify_suspicious_windows(
    messages,
    time_window,
    cancel_threshold,
    size_cutoff
  )

  # Generate alerts
  alerts <- generate_spoof_alerts(
    flagged_periods,
    cancel_rate,
    large_cancel_rate,
    otr
  )

  # Package results
  result <- list(
    is_suspicious = is_suspicious,
    spoof_score = spoof_score,
    flagged_periods = flagged_periods,
    statistics = list(
      n_messages = nrow(messages),
      n_adds = add_orders,
      n_cancels = cancel_orders,
      n_executes = execute_orders,
      cancel_rate = cancel_rate,
      large_cancel_rate = large_cancel_rate,
      order_to_trade_ratio = otr,
      large_order_cutoff = size_cutoff
    ),
    alerts = alerts,
    parameters = list(
      cancel_threshold = cancel_threshold,
      size_threshold = size_threshold,
      time_window = time_window
    )
  )

  if (return_details) {
    result$order_lifecycle <- order_stats
    result$message_data <- messages
  }

  class(result) <- c("spoofing_detection", "list")
  return(result)
}


#' Detect layering behavior in order book
#'
#' @description
#' Identifies layering: placing multiple orders at different price levels
#' to create false appearance of supply/demand, then canceling after
#' price moves. More sophisticated than simple spoofing.
#'
#' @param messages Order message data
#' @param n_levels Integer, number of price levels to analyze (default: 5)
#' @param coordination_threshold Numeric, correlation threshold (default: 0.70)
#' @param time_window Numeric, window for pattern detection (default: 60)
#' @param min_layers Integer, minimum coordinated layers (default: 3)
#'
#' @return Layering detection result object
#'
#' @details
#' **Layering Pattern**:
#' 1. Multiple orders at different price levels (typically 3-10 levels)
#' 2. Orders placed in quick succession (< 1 second)
#' 3. Coordinated cancellation (all canceled together)
#' 4. Cancellation triggered by opposite-side execution
#' 5. Pattern repeats throughout trading session
#'
#' **Detection Method**:
#' - Identify orders at multiple price levels
#' - Measure temporal clustering of placements
#' - Calculate cancellation correlation across levels
#' - Flag coordinated cancel patterns
#'
#' **vs. Spoofing**:
#' - Spoofing: Single large order
#' - Layering: Multiple orders across levels
#' - Layering is harder to detect but more effective
#'
#' @references
#' CFTC vs. Navinder Sarao (Flash Crash layering case)
#'
#' @export
#' @examples
#' \dontrun{
#' layering_result <- detect_layering(messages, n_levels = 5)
#' print(layering_result)
#' }
detect_layering <- function(messages,
                             n_levels = 5,
                             coordination_threshold = 0.70,
                             time_window = 60,
                             min_layers = 3) {

  # Validate input
  if (!"price" %in% names(messages)) {
    stop("Price column required for layering detection")
  }

  if (nrow(messages) < min_layers * 2) {
    return(create_null_layering_result("Insufficient data"))
  }

  # Bin orders by price level
  messages <- messages |>
    mutate(
      side_num = ifelse(.data$side == "B", 1, -1),
      is_add = .data$message_type %in% c("A", "F", "ADD", "LIMIT"),
      is_cancel = .data$message_type %in% c("X", "D", "CANCEL", "DELETE")
    )

  # Separate bid and ask analysis
  bid_messages <- messages |> filter(.data$side_num == 1)
  ask_messages <- messages |> filter(.data$side_num == -1)

  # Detect layering on each side
  bid_layering <- detect_side_layering(
    bid_messages,
    n_levels,
    coordination_threshold,
    time_window,
    min_layers,
    side = "bid"
  )

  ask_layering <- detect_side_layering(
    ask_messages,
    n_levels,
    coordination_threshold,
    time_window,
    min_layers,
    side = "ask"
  )

  # Combine results
  is_suspicious <- bid_layering$detected || ask_layering$detected

  layering_score <- max(bid_layering$score, ask_layering$score)

  # Package results
  result <- list(
    is_suspicious = is_suspicious,
    layering_score = layering_score,
    bid_analysis = bid_layering,
    ask_analysis = ask_layering,
    statistics = list(
      n_levels = n_levels,
      coordination_threshold = coordination_threshold,
      bid_layers_detected = bid_layering$n_layers,
      ask_layers_detected = ask_layering$n_layers
    )
  )

  class(result) <- c("layering_detection", "list")
  return(result)
}


#' Detect quote stuffing patterns
#'
#' @description
#' Identifies quote stuffing: flooding order books with rapid order
#' submissions and cancellations to create latency, gain speed advantage,
#' or manipulate prices through message volume.
#'
#' @param messages Order message data
#' @param msg_rate_threshold Numeric, messages per second threshold (default: 100)
#' @param burst_window Numeric, window for burst detection in seconds (default: 1)
#' @param min_bursts Integer, minimum bursts to flag (default: 3)
#' @param cancel_ratio_threshold Numeric, cancel/add ratio (default: 0.80)
#'
#' @return Quote stuffing detection result
#'
#' @details
#' **Quote Stuffing Characteristics**:
#' 1. **High message velocity**: 100-1000+ messages per second
#' 2. **Rapid cancellation**: Most orders canceled within milliseconds
#' 3. **Minimal execution**: Very low fill rate (<5%)
#' 4. **Repeated bursts**: Pattern occurs multiple times
#'
#' **Message Rate Thresholds**:
#' - Normal: 10-50 messages/second
#' - Active: 50-100 messages/second
#' - Suspicious: 100-500 messages/second
#' - Quote stuffing: 500+ messages/second
#'
#' **Detection Algorithm**:
#' - Calculate rolling message velocity
#' - Identify burst periods (velocity spikes)
#' - Measure cancellation rates during bursts
#' - Flag repeated burst patterns
#'
#' **Market Impact**:
#' - Creates latency for other participants
#' - Obscures true supply/demand
#' - May precede directional trades
#' - Can trigger circuit breakers
#'
#' **Regulatory**:
#' - SEC Rule 15c3-5 (Market Access Rule)
#' - Message throttles at exchanges
#' - Excessive message fees
#'
#' @export
#' @examples
#' \dontrun{
#' stuffing_result <- detect_quote_stuffing(messages, msg_rate_threshold = 100)
#' print(stuffing_result)
#' }
detect_quote_stuffing <- function(messages,
                                    msg_rate_threshold = 100,
                                    burst_window = 1,
                                    min_bursts = 3,
                                    cancel_ratio_threshold = 0.80) {

  if (nrow(messages) < 100) {
    return(create_null_stuffing_result("Insufficient data"))
  }

  # Sort by timestamp
  messages <- messages |>
    arrange(.data$timestamp)

  # Calculate message velocity (messages per second)
  time_diffs <- as.numeric(diff(messages$timestamp), units = "secs")

  # Handle zero or negative diffs
  time_diffs[time_diffs <= 0] <- 0.001  # 1 millisecond minimum

  # Rolling message rate
  messages$msg_rate <- c(NA, 1 / time_diffs)

  # Identify bursts (periods with high message rate)
  bursts <- messages |>
    mutate(
      is_burst = !is.na(.data$msg_rate) & .data$msg_rate >= msg_rate_threshold
    )

  # Find burst periods
  burst_periods <- identify_burst_periods(bursts, burst_window)

  # Calculate cancellation ratio during bursts
  if (nrow(burst_periods) > 0) {
    burst_cancel_ratios <- sapply(seq_len(nrow(burst_periods)), function(i) {
      period_msgs <- messages |>
        filter(
          .data$timestamp >= burst_periods$start_time[i],
          .data$timestamp <= burst_periods$end_time[i]
        )

      n_cancel <- sum(period_msgs$message_type %in% c("X", "D", "CANCEL", "DELETE"))
      n_add <- sum(period_msgs$message_type %in% c("A", "F", "ADD", "LIMIT"))

      if (n_add > 0) n_cancel / n_add else 0
    })

    burst_periods$cancel_ratio <- burst_cancel_ratios
  } else {
    burst_periods$cancel_ratio <- numeric(0)
  }

  # Flag suspicious bursts
  suspicious_bursts <- burst_periods |>
    filter(.data$cancel_ratio >= cancel_ratio_threshold)

  # Overall statistics
  n_bursts <- nrow(burst_periods)
  n_suspicious <- nrow(suspicious_bursts)

  is_suspicious <- n_suspicious >= min_bursts

  # Quote stuffing score
  max_rate <- max(messages$msg_rate, na.rm = TRUE)
  avg_burst_cancel <- if (nrow(burst_periods) > 0) {
    mean(burst_periods$cancel_ratio, na.rm = TRUE)
  } else {
    0
  }

  stuffing_score <- calculate_stuffing_score(
    max_rate = max_rate,
    n_bursts = n_bursts,
    avg_burst_cancel = avg_burst_cancel
  )

  # Package results
  result <- list(
    is_suspicious = is_suspicious,
    stuffing_score = stuffing_score,
    burst_periods = burst_periods,
    suspicious_bursts = suspicious_bursts,
    statistics = list(
      max_message_rate = max_rate,
      avg_message_rate = mean(messages$msg_rate, na.rm = TRUE),
      n_bursts = n_bursts,
      n_suspicious_bursts = n_suspicious,
      avg_burst_cancel_ratio = avg_burst_cancel
    ),
    parameters = list(
      msg_rate_threshold = msg_rate_threshold,
      burst_window = burst_window,
      cancel_ratio_threshold = cancel_ratio_threshold
    )
  )

  class(result) <- c("stuffing_detection", "list")
  return(result)
}


#' Calculate order-to-trade ratio
#'
#' @description
#' Computes the ratio of order messages (adds + cancels) to executed trades.
#' High ratios indicate potential manipulation or algorithmic activity.
#'
#' @param messages Order message data
#' @param window Character, aggregation window (default: "1 min")
#' @param return_timeseries Logical, return time series (default: TRUE)
#'
#' @return Data frame with OTR over time or summary statistics
#'
#' @details
#' **Order-to-Trade Ratio Interpretation**:
#' - **Normal market makers**: 3-10
#' - **Active algorithms**: 10-30
#' - **Suspicious activity**: 30-100
#' - **Likely manipulation**: 100+
#'
#' **Regulatory Thresholds**:
#' - Some exchanges charge fees for OTR > 100
#' - MiFID II monitoring for excessive order activity
#' - Pattern of high OTR with low execution suggests manipulation
#'
#' **Use Cases**:
#' - Surveillance for market abuse
#' - Exchange fee calculation
#' - Algorithm performance monitoring
#' - Market quality assessment
#'
#' @export
#' @importFrom lubridate floor_date
#' @examples
#' \dontrun{
#' otr <- compute_order_to_trade_ratio(messages, window = "1 min")
#' summary(otr$otr)
#' }
compute_order_to_trade_ratio <- function(messages,
                                          window = "1 min",
                                          return_timeseries = TRUE) {

  # Classify message types
  messages <- messages |>
    mutate(
      is_order_msg = .data$message_type %in% c("A", "F", "X", "D", "ADD", "CANCEL", "DELETE"),
      is_trade = .data$message_type %in% c("E", "C", "P", "EXECUTE", "TRADE")
    )

  if (return_timeseries) {
    # Aggregate by time window
    otr_series <- messages |>
      mutate(window = floor_date(.data$timestamp, window)) |>
      group_by(.data$window) |>
      summarise(
        n_order_msgs = sum(.data$is_order_msg),
        n_trades = sum(.data$is_trade),
        otr = ifelse(.data$n_trades > 0,
                     .data$n_order_msgs / .data$n_trades,
                     Inf),
        .groups = "drop"
      )

    return(otr_series)

  } else {
    # Overall statistics
    n_order_msgs <- sum(messages$is_order_msg)
    n_trades <- sum(messages$is_trade)
    otr <- if (n_trades > 0) n_order_msgs / n_trades else Inf

    return(list(
      otr = otr,
      n_order_msgs = n_order_msgs,
      n_trades = n_trades
    ))
  }
}


#' Analyze order cancellation patterns
#'
#' @description
#' Examines cancellation behavior for suspicious patterns including
#' rapid cancels, coordinated cancels, and cancel clustering.
#'
#' @param messages Order message data with order IDs
#' @param time_threshold Numeric, rapid cancel threshold in seconds (default: 5)
#' @param cluster_window Numeric, window for cancel clustering (default: 60)
#'
#' @return Cancellation pattern analysis
#'
#' @details
#' **Suspicious Cancellation Patterns**:
#' 1. **Rapid cancels**: Orders canceled within seconds of placement
#' 2. **Coordinated cancels**: Multiple orders canceled simultaneously
#' 3. **Strategic timing**: Cancels after opposite-side execution
#' 4. **High cancel rate**: 70-95% of orders canceled
#'
#' @export
#' @examples
#' \dontrun{
#' cancel_analysis <- analyze_cancellation_patterns(messages, time_threshold = 5)
#' print(cancel_analysis)
#' }
analyze_cancellation_patterns <- function(messages,
                                           time_threshold = 5,
                                           cluster_window = 60) {

  if (!"order_id" %in% names(messages)) {
    stop("order_id column required for cancellation pattern analysis")
  }

  # Track order lifecycle
  orders <- messages |>
    filter(.data$message_type %in% c("A", "F", "ADD", "LIMIT")) |>
    select(.data$order_id, add_time = .data$timestamp, .data$side, .data$price, .data$size)

  cancels <- messages |>
    filter(.data$message_type %in% c("X", "D", "CANCEL", "DELETE")) |>
    select(.data$order_id, cancel_time = .data$timestamp)

  # Match cancels to orders
  order_lifecycle <- orders |>
    left_join(cancels, by = "order_id") |>
    mutate(
      was_canceled = !is.na(.data$cancel_time),
      time_to_cancel = ifelse(.data$was_canceled,
                              as.numeric(difftime(.data$cancel_time, .data$add_time, units = "secs")),
                              NA)
    )

  # Identify rapid cancels
  rapid_cancels <- order_lifecycle |>
    filter(.data$was_canceled, .data$time_to_cancel <= time_threshold)

  # Cancel rate
  n_orders <- nrow(order_lifecycle)
  n_canceled <- sum(order_lifecycle$was_canceled, na.rm = TRUE)
  cancel_rate <- if (n_orders > 0) n_canceled / n_orders else 0

  # Rapid cancel rate
  n_rapid <- nrow(rapid_cancels)
  rapid_cancel_rate <- if (n_canceled > 0) n_rapid / n_canceled else 0

  # Average time to cancel
  avg_time_to_cancel <- mean(order_lifecycle$time_to_cancel, na.rm = TRUE)

  # Identify cancel clusters
  if (n_canceled > 0) {
    cancel_times <- sort(order_lifecycle$cancel_time[!is.na(order_lifecycle$cancel_time)])
    cancel_clusters <- identify_cancel_clusters(cancel_times, cluster_window)
  } else {
    cancel_clusters <- data.frame()
  }

  # Package results
  result <- list(
    cancel_rate = cancel_rate,
    rapid_cancel_rate = rapid_cancel_rate,
    avg_time_to_cancel = avg_time_to_cancel,
    n_orders = n_orders,
    n_canceled = n_canceled,
    n_rapid_cancels = n_rapid,
    rapid_cancels = rapid_cancels,
    cancel_clusters = cancel_clusters,
    order_lifecycle = order_lifecycle,
    is_suspicious = (cancel_rate > 0.70) || (rapid_cancel_rate > 0.50)
  )

  class(result) <- c("cancel_analysis", "list")
  return(result)
}


#' Generate surveillance alert system
#'
#' @description
#' Comprehensive surveillance system that runs multiple detection algorithms
#' and generates prioritized alerts for review.
#'
#' @param messages Order message data
#' @param sensitivity Character, "low", "medium", "high" (default: "medium")
#' @param algorithms Character vector of algorithms to run
#'
#' @return Surveillance alert object with prioritized findings
#'
#' @details
#' Runs multiple detection algorithms:
#' - Spoofing detection
#' - Layering detection
#' - Quote stuffing detection
#' - Order-to-trade ratio analysis
#' - Cancellation pattern analysis
#'
#' Alerts are prioritized:
#' - **Critical**: Multiple algorithms triggered, high scores
#' - **High**: Single algorithm with strong signal
#' - **Medium**: Elevated metrics, requires review
#' - **Low**: Minor anomalies
#'
#' @export
#' @examples
#' \dontrun{
#' alerts <- surveillance_alert_system(messages, sensitivity = "medium")
#' print(alerts)
#'
#' # Review critical alerts
#' critical <- alerts$alerts |> filter(priority == "Critical")
#' }
surveillance_alert_system <- function(messages,
                                       sensitivity = c("low", "medium", "high"),
                                       algorithms = c("spoofing", "layering",
                                                     "stuffing", "otr", "cancels")) {

  sensitivity <- match.arg(sensitivity)

  # Set thresholds based on sensitivity
  thresholds <- get_sensitivity_thresholds(sensitivity)

  results <- list()
  alerts <- list()

  # Run spoofing detection
  if ("spoofing" %in% algorithms) {
    results$spoofing <- tryCatch({
      detect_spoofing(
        messages,
        cancel_threshold = thresholds$spoof_cancel,
        size_threshold = thresholds$size_pct
      )
    }, error = function(e) {
      list(is_suspicious = FALSE, error = e$message)
    })

    if (results$spoofing$is_suspicious) {
      alerts$spoofing <- create_alert(
        type = "Spoofing",
        severity = classify_severity(results$spoofing$spoof_score),
        score = results$spoofing$spoof_score,
        details = sprintf("Cancel rate: %.1f%%, Score: %.0f",
                         results$spoofing$statistics$large_cancel_rate * 100,
                         results$spoofing$spoof_score)
      )
    }
  }

  # Run layering detection
  if ("layering" %in% algorithms) {
    results$layering <- tryCatch({
      detect_layering(messages, coordination_threshold = thresholds$coordination)
    }, error = function(e) {
      list(is_suspicious = FALSE, error = e$message)
    })

    if (results$layering$is_suspicious) {
      alerts$layering <- create_alert(
        type = "Layering",
        severity = classify_severity(results$layering$layering_score),
        score = results$layering$layering_score,
        details = sprintf("Score: %.0f, Bid layers: %d, Ask layers: %d",
                         results$layering$layering_score,
                         results$layering$statistics$bid_layers_detected,
                         results$layering$statistics$ask_layers_detected)
      )
    }
  }

  # Run quote stuffing detection
  if ("stuffing" %in% algorithms) {
    results$stuffing <- tryCatch({
      detect_quote_stuffing(
        messages,
        msg_rate_threshold = thresholds$msg_rate
      )
    }, error = function(e) {
      list(is_suspicious = FALSE, error = e$message)
    })

    if (results$stuffing$is_suspicious) {
      alerts$stuffing <- create_alert(
        type = "Quote Stuffing",
        severity = classify_severity(results$stuffing$stuffing_score),
        score = results$stuffing$stuffing_score,
        details = sprintf("Max rate: %.0f msg/s, Bursts: %d",
                         results$stuffing$statistics$max_message_rate,
                         results$stuffing$statistics$n_suspicious_bursts)
      )
    }
  }

  # Run OTR analysis
  if ("otr" %in% algorithms) {
    results$otr <- tryCatch({
      compute_order_to_trade_ratio(messages, return_timeseries = FALSE)
    }, error = function(e) {
      list(otr = 0, error = e$message)
    })

    if (!is.null(results$otr$otr) && results$otr$otr > thresholds$otr) {
      alerts$otr <- create_alert(
        type = "High Order-to-Trade Ratio",
        severity = if (results$otr$otr > 100) "High" else "Medium",
        score = min(100, results$otr$otr),
        details = sprintf("OTR: %.1f (threshold: %.0f)",
                         results$otr$otr, thresholds$otr)
      )
    }
  }

  # Combine alerts
  alerts_df <- bind_rows(alerts)

  # Overall risk score
  if (nrow(alerts_df) > 0) {
    risk_score <- calculate_composite_risk(results, alerts_df)
  } else {
    risk_score <- 0
  }

  # Package results
  surveillance_result <- list(
    alerts = alerts_df,
    risk_score = risk_score,
    n_alerts = nrow(alerts_df),
    detection_results = results,
    sensitivity = sensitivity,
    timestamp = Sys.time()
  )

  class(surveillance_result) <- c("surveillance_alerts", "list")
  return(surveillance_result)
}


#' Generate market manipulation compliance report
#'
#' @description
#' Creates comprehensive report for regulatory documentation including
#' detection results, flagged activity, and supporting evidence.
#'
#' @param surveillance_result Output from surveillance_alert_system()
#' @param symbol Character, stock symbol
#' @param date Date, trading date
#' @param format Character, "text", "html", or "pdf"
#'
#' @return Formatted compliance report
#'
#' @export
#' @examples
#' \dontrun{
#' alerts <- surveillance_alert_system(messages)
#' report <- market_manipulation_report(alerts, symbol = "AAPL", date = "2024-01-15")
#' cat(report)
#' }
market_manipulation_report <- function(surveillance_result,
                                        symbol = NULL,
                                        date = NULL,
                                        format = c("text", "html", "pdf")) {

  format <- match.arg(format)

  if (format == "text") {
    return(generate_text_report(surveillance_result, symbol, date))
  } else if (format == "html") {
    return(generate_html_report(surveillance_result, symbol, date))
  } else {
    stop("PDF format not yet implemented. Use 'text' or 'html'.")
  }
}


# ============================================================================
# Helper Functions (Not Exported)
# ============================================================================

#' Analyze order lifecycle
#' @noRd
analyze_order_lifecycle <- function(messages) {
  # Track each order from add to cancel/execute
  orders <- messages |>
    filter(.data$message_type %in% c("A", "F", "ADD", "LIMIT"))

  if (!"order_id" %in% names(orders)) {
    return(NULL)
  }

  lifecycle_stats <- orders |>
    group_by(.data$order_id) |>
    summarise(
      add_time = first(.data$timestamp),
      side = first(.data$side),
      size = first(.data$size),
      .groups = "drop"
    )

  # Add cancel/execute events
  outcomes <- messages |>
    filter(.data$message_type %in% c("X", "D", "E", "C", "CANCEL", "DELETE", "EXECUTE", "TRADE")) |>
    group_by(.data$order_id) |>
    summarise(
      outcome_time = first(.data$timestamp),
      outcome_type = first(.data$message_type),
      .groups = "drop"
    )

  lifecycle_stats <- lifecycle_stats |>
    left_join(outcomes, by = "order_id") |>
    mutate(
      time_to_outcome = as.numeric(difftime(.data$outcome_time, .data$add_time, units = "secs"))
    )

  return(lifecycle_stats)
}

#' Analyze aggregate patterns without order IDs
#' @noRd
analyze_aggregate_patterns <- function(messages, time_window) {
  # Simple time-windowed analysis
  messages |>
    mutate(window = floor_date(.data$timestamp, paste(time_window, "seconds"))) |>
    group_by(.data$window) |>
    summarise(
      n_adds = sum(.data$is_add),
      n_cancels = sum(.data$is_cancel),
      n_executes = sum(.data$is_execute),
      cancel_rate = .data$n_cancels / (.data$n_adds + 1e-10),
      .groups = "drop"
    )
}

#' Calculate spoofing score
#' @noRd
calculate_spoof_score <- function(cancel_rate, large_cancel_rate, otr, order_stats) {
  # Weighted scoring
  score <- 0

  # Cancel rate component (0-40 points)
  score <- score + min(40, cancel_rate * 50)

  # Large order cancel rate (0-40 points)
  score <- score + min(40, large_cancel_rate * 50)

  # Order-to-trade ratio (0-20 points)
  otr_score <- min(20, (otr - 10) / 5)
  score <- score + max(0, otr_score)

  return(min(100, score))
}

#' Identify suspicious time windows
#' @noRd
identify_suspicious_windows <- function(messages, time_window, cancel_threshold, size_cutoff) {
  messages |>
    mutate(window = floor_date(.data$timestamp, paste(time_window, "seconds"))) |>
    group_by(.data$window) |>
    summarise(
      n_large_adds = sum(.data$is_add & .data$is_large, na.rm = TRUE),
      n_large_cancels = sum(.data$is_cancel & .data$is_large, na.rm = TRUE),
      large_cancel_rate = .data$n_large_cancels / (.data$n_large_adds + 1e-10),
      .groups = "drop"
    ) |>
    filter(.data$large_cancel_rate >= cancel_threshold, .data$n_large_adds >= 2)
}

#' Generate spoof alerts
#' @noRd
generate_spoof_alerts <- function(flagged_periods, cancel_rate, large_cancel_rate, otr) {
  alerts <- data.frame()

  if (nrow(flagged_periods) > 0) {
    alerts <- flagged_periods |>
      mutate(
        alert_type = "Suspicious Cancellation Pattern",
        severity = ifelse(.data$large_cancel_rate > 0.90, "High", "Medium")
      ) |>
      select(.data$window, .data$alert_type, .data$severity, .data$large_cancel_rate)
  }

  return(alerts)
}

#' Detect layering on one side
#' @noRd
detect_side_layering <- function(messages, n_levels, coordination_threshold,
                                  time_window, min_layers, side) {
  if (nrow(messages) < min_layers * 2) {
    return(list(detected = FALSE, score = 0, n_layers = 0))
  }

  # Simplified layering detection - full version would track price levels
  # Count number of distinct price levels with orders
  price_levels <- messages |>
    filter(.data$is_add) |>
    distinct(.data$price) |>
    nrow()

  # Placeholder score
  score <- min(100, price_levels * 10)

  list(
    detected = price_levels >= min_layers,
    score = score,
    n_layers = price_levels,
    side = side
  )
}

#' Create null layering result
#' @noRd
create_null_layering_result <- function(reason) {
  list(
    is_suspicious = FALSE,
    layering_score = 0,
    bid_analysis = list(detected = FALSE, score = 0, n_layers = 0),
    ask_analysis = list(detected = FALSE, score = 0, n_layers = 0),
    statistics = list(reason = reason)
  )
}

#' Create null stuffing result
#' @noRd
create_null_stuffing_result <- function(reason) {
  list(
    is_suspicious = FALSE,
    stuffing_score = 0,
    burst_periods = data.frame(),
    suspicious_bursts = data.frame(),
    statistics = list(reason = reason)
  )
}

#' Calculate stuffing score
#' @noRd
calculate_stuffing_score <- function(max_rate, n_bursts, avg_burst_cancel) {
  score <- 0

  # Message rate component (0-50 points)
  score <- score + min(50, (max_rate - 100) / 10)

  # Burst frequency (0-30 points)
  score <- score + min(30, n_bursts * 5)

  # Cancel ratio (0-20 points)
  score <- score + min(20, avg_burst_cancel * 25)

  return(min(100, max(0, score)))
}

#' Identify burst periods
#' @noRd
identify_burst_periods <- function(bursts, burst_window) {
  burst_rows <- which(bursts$is_burst)

  if (length(burst_rows) == 0) {
    return(data.frame(
      start_time = as.POSIXct(character()),
      end_time = as.POSIXct(character()),
      n_messages = integer(),
      max_rate = numeric()
    ))
  }

  # Group consecutive bursts
  burst_groups <- cumsum(c(1, diff(burst_rows) > 1))

  burst_periods <- data.frame(burst_idx = burst_rows, group = burst_groups) |>
    group_by(.data$group) |>
    summarise(
      start_idx = min(.data$burst_idx),
      end_idx = max(.data$burst_idx),
      .groups = "drop"
    ) |>
    mutate(
      start_time = bursts$timestamp[.data$start_idx],
      end_time = bursts$timestamp[.data$end_idx],
      n_messages = .data$end_idx - .data$start_idx + 1,
      max_rate = sapply(seq_len(n()), function(i) {
        max(bursts$msg_rate[.data$start_idx[i]:.data$end_idx[i]], na.rm = TRUE)
      })
    ) |>
    select(.data$start_time, .data$end_time, .data$n_messages, .data$max_rate)

  return(burst_periods)
}

#' Identify cancel clusters
#' @noRd
identify_cancel_clusters <- function(cancel_times, cluster_window) {
  # Simplified clustering - count cancels in rolling windows
  # Full version would use hierarchical clustering

  data.frame(
    window = floor_date(cancel_times, paste(cluster_window, "seconds"))
  ) |>
    group_by(.data$window) |>
    summarise(n_cancels = n(), .groups = "drop") |>
    filter(.data$n_cancels >= 3) |>
    arrange(desc(.data$n_cancels))
}

#' Get sensitivity thresholds
#' @noRd
get_sensitivity_thresholds <- function(sensitivity) {
  if (sensitivity == "high") {
    list(
      spoof_cancel = 0.60,
      size_pct = 0.85,
      coordination = 0.60,
      msg_rate = 75,
      otr = 30
    )
  } else if (sensitivity == "medium") {
    list(
      spoof_cancel = 0.70,
      size_pct = 0.90,
      coordination = 0.70,
      msg_rate = 100,
      otr = 50
    )
  } else {  # low
    list(
      spoof_cancel = 0.80,
      size_pct = 0.95,
      coordination = 0.80,
      msg_rate = 150,
      otr = 100
    )
  }
}

#' Create alert
#' @noRd
create_alert <- function(type, severity, score, details) {
  data.frame(
    type = type,
    severity = severity,
    score = score,
    details = details,
    timestamp = Sys.time(),
    stringsAsFactors = FALSE
  )
}

#' Classify severity
#' @noRd
classify_severity <- function(score) {
  if (score >= 80) {
    "Critical"
  } else if (score >= 60) {
    "High"
  } else if (score >= 40) {
    "Medium"
  } else {
    "Low"
  }
}

#' Calculate composite risk
#' @noRd
calculate_composite_risk <- function(results, alerts_df) {
  if (nrow(alerts_df) == 0) return(0)

  # Weight by severity
  severity_weights <- c("Critical" = 1.0, "High" = 0.7, "Medium" = 0.4, "Low" = 0.2)

  weighted_scores <- alerts_df$score * severity_weights[alerts_df$severity]
  composite <- mean(weighted_scores, na.rm = TRUE)

  # Boost if multiple alert types
  if (nrow(alerts_df) >= 3) {
    composite <- composite * 1.2
  }

  return(min(100, composite))
}

#' Generate text report
#' @noRd
generate_text_report <- function(surveillance_result, symbol, date) {
  report <- paste0(
    "============================================\n",
    "MARKET MANIPULATION SURVEILLANCE REPORT\n",
    "============================================\n\n",
    "Symbol: ", symbol %||% "N/A", "\n",
    "Date: ", date %||% Sys.Date(), "\n",
    "Report Generated: ", Sys.time(), "\n",
    "Sensitivity: ", toupper(surveillance_result$sensitivity), "\n\n",
    "OVERALL RISK SCORE: ", round(surveillance_result$risk_score), "/100\n",
    "ALERTS GENERATED: ", surveillance_result$n_alerts, "\n\n"
  )

  if (surveillance_result$n_alerts > 0) {
    report <- paste0(report, "ALERTS:\n", paste(rep("-", 40), collapse = ""), "\n")

    for (i in seq_len(nrow(surveillance_result$alerts))) {
      alert <- surveillance_result$alerts[i, ]
      report <- paste0(
        report,
        sprintf("[%s] %s\n", alert$severity, alert$type),
        sprintf("  Score: %.0f\n", alert$score),
        sprintf("  Details: %s\n\n", alert$details)
      )
    }
  } else {
    report <- paste0(report, "No suspicious activity detected.\n")
  }

  return(report)
}

#' Generate HTML report
#' @noRd
generate_html_report <- function(surveillance_result, symbol, date) {
  # Placeholder - full HTML generation
  paste0(
    "<html><body>",
    "<h1>Market Manipulation Surveillance Report</h1>",
    "<p>Symbol: ", symbol %||% "N/A", "</p>",
    "<p>Date: ", date %||% Sys.Date(), "</p>",
    "<p>Risk Score: ", round(surveillance_result$risk_score), "/100</p>",
    "</body></html>"
  )
}

# Print methods
#' @export
print.spoofing_detection <- function(x, ...) {
  cat("Spoofing Detection Results\n")
  cat("==========================\n\n")
  cat(sprintf("Status: %s\n", if (x$is_suspicious) "SUSPICIOUS" else "Normal"))
  cat(sprintf("Spoofing Score: %.0f/100\n", x$spoof_score))
  cat(sprintf("Cancel Rate: %.1f%%\n", x$statistics$cancel_rate * 100))
  cat(sprintf("Large Order Cancel Rate: %.1f%%\n", x$statistics$large_cancel_rate * 100))
  cat(sprintf("Order-to-Trade Ratio: %.1f\n", x$statistics$order_to_trade_ratio))
  cat(sprintf("\nFlagged Periods: %d\n", nrow(x$flagged_periods)))
  invisible(x)
}

#' @export
print.surveillance_alerts <- function(x, ...) {
  cat("Surveillance Alert System\n")
  cat("=========================\n\n")
  cat(sprintf("Overall Risk Score: %.0f/100\n", x$risk_score))
  cat(sprintf("Total Alerts: %d\n", x$n_alerts))
  cat(sprintf("Sensitivity: %s\n\n", toupper(x$sensitivity)))

  if (x$n_alerts > 0) {
    cat("Alerts by Severity:\n")
    print(table(x$alerts$severity))
    cat("\n")
    print(x$alerts[, c("type", "severity", "score")])
  } else {
    cat("No alerts generated.\n")
  }
  invisible(x)
}
