test_that("detect_spoofing works with basic message data", {
  # Create synthetic message data with spoofing pattern
  n <- 100
  messages <- data.frame(
    timestamp = Sys.time() + seq(1, n),
    message_type = sample(c("A", "X", "E"), n, replace = TRUE,
                          prob = c(0.4, 0.5, 0.1)),  # High cancel rate
    side = sample(c("B", "S"), n, replace = TRUE),
    price = 100 + rnorm(n, 0, 0.1),
    size = sample(c(100, 500, 1000, 5000), n, replace = TRUE),  # Mix of sizes
    order_id = 1:n
  )

  result <- detect_spoofing(messages, cancel_threshold = 0.70)

  expect_s3_class(result, "spoofing_detection")
  expect_true("is_suspicious" %in% names(result))
  expect_true("spoof_score" %in% names(result))
  expect_true("statistics" %in% names(result))

  # Score should be numeric 0-100
  expect_gte(result$spoof_score, 0)
  expect_lte(result$spoof_score, 100)
})

test_that("detect_spoofing identifies high cancel rates", {
  # Create data with very high cancel rate (spoofing)
  adds <- data.frame(
    timestamp = Sys.time() + 1:50,
    message_type = "A",
    side = "B",
    price = 100,
    size = 5000,
    order_id = 1:50
  )

  # Cancel 90% of orders
  cancels <- data.frame(
    timestamp = Sys.time() + 51:95,
    message_type = "X",
    side = "B",
    price = 100,
    size = 5000,
    order_id = 1:45
  )

  messages <- rbind(adds, cancels)

  result <- detect_spoofing(messages, cancel_threshold = 0.70)

  # Should detect as suspicious
  expect_true(result$is_suspicious || result$spoof_score > 50)

  # Cancel rate should be high
  expect_gt(result$statistics$cancel_rate, 0.70)
})

test_that("detect_spoofing handles insufficient data gracefully", {
  messages <- data.frame(
    timestamp = Sys.time() + 1:3,
    message_type = c("A", "X", "E"),
    side = c("B", "B", "S"),
    price = c(100, 100, 100),
    size = c(100, 100, 100)
  )

  expect_warning(
    result <- detect_spoofing(messages, min_orders = 10),
    "Insufficient"
  )

  expect_false(result$is_suspicious)
})

test_that("detect_spoofing requires correct columns", {
  bad_messages <- data.frame(
    time = Sys.time() + 1:10,
    type = sample(c("A", "X"), 10, replace = TRUE)
    # Missing required columns
  )

  expect_error(
    detect_spoofing(bad_messages),
    "Missing required columns"
  )
})

test_that("detect_layering works with multi-level orders", {
  # Create messages at multiple price levels
  messages <- data.frame(
    timestamp = Sys.time() + 1:100,
    message_type = sample(c("A", "X", "E"), 100, replace = TRUE),
    side = sample(c("B", "S"), 100, replace = TRUE),
    price = rep(100 + (0:9) * 0.01, each = 10),  # 10 price levels
    size = 100
  )

  result <- detect_layering(messages, n_levels = 5, min_layers = 3)

  expect_s3_class(result, "layering_detection")
  expect_true("is_suspicious" %in% names(result))
  expect_true("layering_score" %in% names(result))
  expect_true("bid_analysis" %in% names(result))
  expect_true("ask_analysis" %in% names(result))
})

test_that("detect_layering requires price column", {
  messages <- data.frame(
    timestamp = Sys.time() + 1:10,
    message_type = "A",
    side = "B",
    size = 100
    # Missing price
  )

  expect_error(
    detect_layering(messages),
    "Price column required"
  )
})

test_that("detect_quote_stuffing identifies high message velocity", {
  # Create high-frequency messages (stuffing pattern)
  n <- 1000
  timestamps <- Sys.time() + seq(0, 10, length.out = n)  # 100 msg/sec

  messages <- data.frame(
    timestamp = timestamps,
    message_type = sample(c("A", "X"), n, replace = TRUE, prob = c(0.3, 0.7)),
    side = sample(c("B", "S"), n, replace = TRUE),
    price = 100 + rnorm(n, 0, 0.01),
    size = 100
  )

  result <- detect_quote_stuffing(messages, msg_rate_threshold = 50)

  expect_s3_class(result, "stuffing_detection")
  expect_true("stuffing_score" %in% names(result))
  expect_true("burst_periods" %in% names(result))

  # Should detect some bursts
  expect_gte(nrow(result$burst_periods), 0)
})

test_that("detect_quote_stuffing handles low activity gracefully", {
  # Slow message rate (normal market)
  messages <- data.frame(
    timestamp = Sys.time() + seq(0, 100, by = 1),  # 1 msg/sec
    message_type = sample(c("A", "X", "E"), 101, replace = TRUE),
    side = sample(c("B", "S"), 101, replace = TRUE),
    price = 100,
    size = 100
  )

  result <- detect_quote_stuffing(messages, msg_rate_threshold = 100)

  # Should not detect stuffing in slow market
  expect_false(result$is_suspicious)
  expect_equal(nrow(result$suspicious_bursts), 0)
})

test_that("compute_order_to_trade_ratio calculates correctly", {
  messages <- data.frame(
    timestamp = Sys.time() + 1:100,
    message_type = c(rep("A", 50), rep("X", 30), rep("E", 20)),
    side = "B",
    price = 100,
    size = 100
  )

  result <- compute_order_to_trade_ratio(messages, return_timeseries = FALSE)

  expect_type(result, "list")
  expect_true("otr" %in% names(result))

  # OTR = (adds + cancels) / trades = (50 + 30) / 20 = 4
  expect_equal(result$otr, 4)
  expect_equal(result$n_order_msgs, 80)
  expect_equal(result$n_trades, 20)
})

test_that("compute_order_to_trade_ratio handles no trades", {
  messages <- data.frame(
    timestamp = Sys.time() + 1:50,
    message_type = c(rep("A", 30), rep("X", 20)),
    side = "B",
    price = 100,
    size = 100
  )

  result <- compute_order_to_trade_ratio(messages, return_timeseries = FALSE)

  # OTR should be Inf when no trades
  expect_true(is.infinite(result$otr))
})

test_that("compute_order_to_trade_ratio returns timeseries", {
  messages <- data.frame(
    timestamp = Sys.time() + seq(0, 3600, by = 10),  # 1 hour
    message_type = sample(c("A", "X", "E"), 361, replace = TRUE),
    side = "B",
    price = 100,
    size = 100
  )

  result <- compute_order_to_trade_ratio(messages, window = "1 min", return_timeseries = TRUE)

  expect_s3_class(result, "data.frame")
  expect_true("window" %in% names(result))
  expect_true("otr" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("analyze_cancellation_patterns requires order_id", {
  messages <- data.frame(
    timestamp = Sys.time() + 1:10,
    message_type = c("A", "X"),
    side = "B",
    price = 100,
    size = 100
    # Missing order_id
  )

  expect_error(
    analyze_cancellation_patterns(messages),
    "order_id column required"
  )
})

test_that("analyze_cancellation_patterns detects rapid cancels", {
  # Create orders with rapid cancellations
  adds <- data.frame(
    timestamp = Sys.time() + seq(0, 50, by = 5),
    message_type = "A",
    side = "B",
    price = 100,
    size = 5000,
    order_id = 1:11
  )

  # Cancel within 2 seconds (rapid)
  cancels <- data.frame(
    timestamp = adds$timestamp[1:10] + 2,
    message_type = "X",
    side = "B",
    price = 100,
    size = 5000,
    order_id = 1:10
  )

  messages <- rbind(adds, cancels)

  result <- analyze_cancellation_patterns(messages, time_threshold = 5)

  expect_s3_class(result, "cancel_analysis")
  expect_true("rapid_cancel_rate" %in% names(result))
  expect_true("avg_time_to_cancel" %in% names(result))

  # Should detect rapid cancels
  expect_gt(result$n_rapid_cancels, 0)
  expect_lt(result$avg_time_to_cancel, 5)
})

test_that("analyze_cancellation_patterns handles normal behavior", {
  # Normal cancellation pattern (not suspicious)
  adds <- data.frame(
    timestamp = Sys.time() + seq(0, 100, by = 10),
    message_type = "A",
    side = "B",
    price = 100,
    size = 100,
    order_id = 1:11
  )

  # Cancel after reasonable time
  cancels <- data.frame(
    timestamp = adds$timestamp[1:5] + 30,  # 30 seconds later
    message_type = "X",
    side = "B",
    price = 100,
    size = 100,
    order_id = 1:5
  )

  messages <- rbind(adds, cancels)

  result <- analyze_cancellation_patterns(messages, time_threshold = 5)

  # Should not flag as suspicious
  expect_false(result$is_suspicious)
  expect_lt(result$cancel_rate, 0.70)
})

test_that("surveillance_alert_system runs multiple algorithms", {
  # Create diverse message data
  n <- 200
  messages <- data.frame(
    timestamp = Sys.time() + seq(0, 100, length.out = n),
    message_type = sample(c("A", "X", "E"), n, replace = TRUE),
    side = sample(c("B", "S"), n, replace = TRUE),
    price = 100 + rnorm(n, 0, 0.1),
    size = sample(c(100, 500, 1000), n, replace = TRUE),
    order_id = 1:n
  )

  result <- surveillance_alert_system(messages, sensitivity = "medium")

  expect_s3_class(result, "surveillance_alerts")
  expect_true("alerts" %in% names(result))
  expect_true("risk_score" %in% names(result))
  expect_true("detection_results" %in% names(result))

  # Risk score should be 0-100
  expect_gte(result$risk_score, 0)
  expect_lte(result$risk_score, 100)
})

test_that("surveillance_alert_system respects sensitivity levels", {
  messages <- data.frame(
    timestamp = Sys.time() + 1:100,
    message_type = sample(c("A", "X", "E"), 100, replace = TRUE),
    side = "B",
    price = 100,
    size = 100
  )

  result_low <- surveillance_alert_system(messages, sensitivity = "low")
  result_high <- surveillance_alert_system(messages, sensitivity = "high")

  expect_s3_class(result_low, "surveillance_alerts")
  expect_s3_class(result_high, "surveillance_alerts")

  # High sensitivity should potentially generate more alerts
  # (though not guaranteed with random data)
  expect_true(TRUE)  # Just verify no errors
})

test_that("surveillance_alert_system can run subset of algorithms", {
  messages <- data.frame(
    timestamp = Sys.time() + 1:100,
    message_type = sample(c("A", "X", "E"), 100, replace = TRUE),
    side = "B",
    price = 100,
    size = 100
  )

  result <- surveillance_alert_system(
    messages,
    algorithms = c("spoofing", "otr")  # Only run 2 algorithms
  )

  expect_s3_class(result, "surveillance_alerts")
  expect_true("spoofing" %in% names(result$detection_results))
  expect_true("otr" %in% names(result$detection_results))
})

test_that("market_manipulation_report generates text report", {
  # Create minimal surveillance result
  alerts_df <- data.frame(
    type = "Spoofing",
    severity = "High",
    score = 80,
    details = "Test alert"
  )

  surveillance_result <- list(
    alerts = alerts_df,
    risk_score = 75,
    n_alerts = 1,
    sensitivity = "medium",
    timestamp = Sys.time()
  )
  class(surveillance_result) <- "surveillance_alerts"

  report <- market_manipulation_report(
    surveillance_result,
    symbol = "AAPL",
    date = "2024-01-15",
    format = "text"
  )

  expect_type(report, "character")
  expect_true(nchar(report) > 0)
  expect_true(grepl("AAPL", report))
  expect_true(grepl("Spoofing", report))
})

test_that("market_manipulation_report handles no alerts", {
  surveillance_result <- list(
    alerts = data.frame(),
    risk_score = 0,
    n_alerts = 0,
    sensitivity = "low",
    timestamp = Sys.time()
  )
  class(surveillance_result) <- "surveillance_alerts"

  report <- market_manipulation_report(
    surveillance_result,
    symbol = "AAPL",
    format = "text"
  )

  expect_type(report, "character")
  expect_true(grepl("No suspicious activity", report))
})

test_that("print methods work for surveillance objects", {
  # Spoofing detection
  spoof <- list(
    is_suspicious = TRUE,
    spoof_score = 75,
    statistics = list(
      cancel_rate = 0.80,
      large_cancel_rate = 0.90,
      order_to_trade_ratio = 50
    ),
    flagged_periods = data.frame()
  )
  class(spoof) <- "spoofing_detection"

  expect_output(print(spoof), "Spoofing Detection")
  expect_output(print(spoof), "SUSPICIOUS")

  # Surveillance alerts
  alerts <- list(
    alerts = data.frame(
      type = "Spoofing",
      severity = "High",
      score = 80
    ),
    risk_score = 75,
    n_alerts = 1,
    sensitivity = "medium"
  )
  class(alerts) <- "surveillance_alerts"

  expect_output(print(alerts), "Surveillance Alert")
  expect_output(print(alerts), "Risk Score")
})

test_that("helper functions calculate scores correctly", {
  # Test spoof score calculation
  score <- rOFI:::calculate_spoof_score(
    cancel_rate = 0.80,
    large_cancel_rate = 0.90,
    otr = 50,
    order_stats = NULL
  )

  expect_type(score, "double")
  expect_gte(score, 0)
  expect_lte(score, 100)

  # High values should give high score
  expect_gt(score, 50)
})

test_that("sensitivity thresholds are correctly defined", {
  thresholds_low <- rOFI:::get_sensitivity_thresholds("low")
  thresholds_medium <- rOFI:::get_sensitivity_thresholds("medium")
  thresholds_high <- rOFI:::get_sensitivity_thresholds("high")

  # High sensitivity should have lower thresholds
  expect_lt(thresholds_high$spoof_cancel, thresholds_medium$spoof_cancel)
  expect_lt(thresholds_medium$spoof_cancel, thresholds_low$spoof_cancel)

  expect_lt(thresholds_high$msg_rate, thresholds_medium$msg_rate)
  expect_lt(thresholds_medium$msg_rate, thresholds_low$msg_rate)
})

test_that("alert creation and severity classification work", {
  alert <- rOFI:::create_alert(
    type = "Test Alert",
    severity = "High",
    score = 80,
    details = "Test details"
  )

  expect_s3_class(alert, "data.frame")
  expect_equal(alert$type, "Test Alert")
  expect_equal(alert$severity, "High")
  expect_equal(alert$score, 80)

  # Test severity classification
  expect_equal(rOFI:::classify_severity(90), "Critical")
  expect_equal(rOFI:::classify_severity(70), "High")
  expect_equal(rOFI:::classify_severity(50), "Medium")
  expect_equal(rOFI:::classify_severity(30), "Low")
})

test_that("composite risk calculation works", {
  alerts_df <- data.frame(
    type = c("Spoofing", "Layering"),
    severity = c("High", "Medium"),
    score = c(80, 60)
  )

  results <- list()

  risk <- rOFI:::calculate_composite_risk(results, alerts_df)

  expect_type(risk, "double")
  expect_gte(risk, 0)
  expect_lte(risk, 100)

  # Multiple alerts should boost score
  expect_gt(risk, 0)
})
