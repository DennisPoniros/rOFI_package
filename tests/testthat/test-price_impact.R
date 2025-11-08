test_that("almgren_chriss_trajectory generates valid trajectory", {
  trajectory <- almgren_chriss_trajectory(
    Q = 100000,
    T_horizon = 30,
    lambda = 1e-6,
    sigma = 0.30,
    n_steps = 20
  )

  expect_s3_class(trajectory, "almgren_chriss")
  expect_true("trajectory" %in% names(trajectory))
  expect_true("expected_cost" %in% names(trajectory))

  # Check trajectory dimensions
  expect_equal(nrow(trajectory$trajectory), 21)  # n_steps + 1

  # Check positions sum to Q
  final_executed <- tail(trajectory$trajectory$cumulative_executed, 1)
  expect_equal(final_executed, 100000, tolerance = 1)
})

test_that("almgren_chriss with zero risk aversion gives linear trajectory", {
  trajectory <- almgren_chriss_trajectory(
    Q = 100000,
    T_horizon = 30,
    lambda = 0,  # No risk aversion
    sigma = 0.30,
    n_steps = 10
  )

  # Should be approximately linear
  positions <- trajectory$trajectory$position_remaining
  # Check that positions decrease roughly linearly
  differences <- diff(positions)
  expect_true(sd(abs(differences)) < mean(abs(differences)) * 0.1)
})

test_that("almgren_chriss fails with invalid inputs", {
  expect_error(
    almgren_chriss_trajectory(Q = 0, T_horizon = 30),
    "non-zero"
  )

  expect_error(
    almgren_chriss_trajectory(Q = 100000, T_horizon = -10),
    "positive"
  )

  expect_error(
    almgren_chriss_trajectory(Q = 100000, T_horizon = 30, lambda = -1),
    "non-negative"
  )
})

test_that("sqrt_impact calculates impact correctly", {
  impact <- sqrt_impact(
    Q = 50000,
    V = 2000000,
    sigma = 0.02,
    Y = 0.20,
    bps = TRUE
  )

  expect_type(impact, "double")
  expect_gt(impact, 0)

  # Check formula: Y * sigma * sqrt(Q/V) * 10000
  expected <- 0.20 * 0.02 * sqrt(50000 / 2000000) * 10000
  expect_equal(impact, expected, tolerance = 1e-6)
})

test_that("sqrt_impact scales properly with order size", {
  V <- 2000000
  sigma <- 0.02

  impact_small <- sqrt_impact(Q = 10000, V = V, sigma = sigma)
  impact_large <- sqrt_impact(Q = 40000, V = V, sigma = sigma)

  # 4x size should give 2x impact (square-root)
  expect_equal(impact_large / impact_small, 2, tolerance = 0.01)
})

test_that("sqrt_impact fails with invalid inputs", {
  expect_error(sqrt_impact(Q = -1000, V = 1000000, sigma = 0.02), "positive")
  expect_error(sqrt_impact(Q = 1000, V = -1000000, sigma = 0.02), "positive")
  expect_error(sqrt_impact(Q = 1000, V = 1000000, sigma = -0.02), "positive")
})

test_that("calibrate_sqrt_law estimates Y parameter", {
  # Create synthetic data with known Y = 0.25
  set.seed(42)
  Y_true <- 0.25
  sigma <- 0.02
  V <- 2000000

  sizes <- c(10000, 25000, 50000, 75000, 100000)
  impacts <- Y_true * sigma * sqrt(sizes / V)

  executions <- data.frame(
    order_size = sizes,
    avg_daily_volume = V,
    realized_impact = impacts,
    volatility = sigma
  )

  calibration <- calibrate_sqrt_law(executions, exponent_fixed = TRUE)

  expect_s3_class(calibration, "sqrt_calibration")
  expect_equal(calibration$Y, Y_true, tolerance = 0.01)
  expect_gt(calibration$R_squared, 0.99)  # Should fit perfectly
})

test_that("calibrate_sqrt_law handles noisy data", {
  set.seed(42)
  Y_true <- 0.20
  sigma <- 0.02
  V <- 2000000

  sizes <- rep(c(10000, 25000, 50000, 75000, 100000), 2)
  impacts <- Y_true * sigma * sqrt(sizes / V) + rnorm(length(sizes), 0, 0.0001)

  executions <- data.frame(
    order_size = sizes,
    avg_daily_volume = V,
    realized_impact = impacts,
    volatility = sigma
  )

  calibration <- calibrate_sqrt_law(executions)

  expect_s3_class(calibration, "sqrt_calibration")
  # Should be close to true value despite noise
  expect_equal(calibration$Y, Y_true, tolerance = 0.05)
})

test_that("calibrate_sqrt_law requires sufficient data", {
  executions <- data.frame(
    order_size = c(10000, 20000),
    avg_daily_volume = 1000000,
    realized_impact = c(0.001, 0.0014),
    volatility = 0.02
  )

  expect_error(
    calibrate_sqrt_law(executions),
    "at least 3"
  )
})

test_that("decompose_price_impact separates temporary and permanent", {
  trades <- simulate_orders(n = 1000, seed = 42)
  decomp <- decompose_price_impact(trades, window = "1 min", decay_periods = 5)

  expect_s3_class(decomp, "impact_decomposition")
  expect_true("avg_temporary" %in% names(decomp))
  expect_true("avg_permanent" %in% names(decomp))
  expect_true("pct_permanent" %in% names(decomp))

  # Percentages should sum to 100
  expect_true(decomp$pct_permanent >= 0 && decomp$pct_permanent <= 100)

  # Should have valid decomposition data
  expect_true(nrow(decomp$decomposition) > 0)
})

test_that("decompose_price_impact handles different decay periods", {
  trades <- simulate_orders(n = 1000, seed = 42)

  decomp_short <- decompose_price_impact(trades, decay_periods = 3)
  decomp_long <- decompose_price_impact(trades, decay_periods = 10)

  expect_s3_class(decomp_short, "impact_decomposition")
  expect_s3_class(decomp_long, "impact_decomposition")

  # Both should produce valid results
  expect_true(!is.na(decomp_short$avg_temporary))
  expect_true(!is.na(decomp_long$avg_temporary))
})

test_that("decompose_price_impact requires sufficient data", {
  trades <- simulate_orders(n = 20, seed = 42)

  expect_error(
    decompose_price_impact(trades, window = "1 min", min_obs = 50),
    "Insufficient"
  )
})

test_that("implementation_shortfall calculates correctly for buys", {
  is_buy <- implementation_shortfall(
    execution_price = 100.15,
    benchmark_price = 100.00,
    side = "B",
    quantity = 10000,
    bps = TRUE
  )

  # Bought at 100.15 vs benchmark 100.00 = 15 bps cost
  expect_equal(is_buy, 15, tolerance = 0.1)
})

test_that("implementation_shortfall calculates correctly for sells", {
  is_sell <- implementation_shortfall(
    execution_price = 99.85,
    benchmark_price = 100.00,
    side = "S",
    quantity = 10000,
    bps = TRUE
  )

  # Sold at 99.85 vs benchmark 100.00 = 15 bps cost
  expect_equal(is_sell, 15, tolerance = 0.1)
})

test_that("implementation_shortfall handles good execution", {
  # Buy below benchmark (good execution)
  is_buy <- implementation_shortfall(
    execution_price = 99.90,
    benchmark_price = 100.00,
    side = "B",
    quantity = 10000,
    bps = TRUE
  )

  expect_lt(is_buy, 0)  # Negative means savings

  # Sell above benchmark (good execution)
  is_sell <- implementation_shortfall(
    execution_price = 100.10,
    benchmark_price = 100.00,
    side = "S",
    quantity = 10000,
    bps = TRUE
  )

  expect_lt(is_sell, 0)  # Negative means savings
})

test_that("implementation_shortfall handles different side formats", {
  # All these should work
  expect_no_error(implementation_shortfall(100, 100, "B", 1000))
  expect_no_error(implementation_shortfall(100, 100, "S", 1000))
  expect_no_error(implementation_shortfall(100, 100, "BUY", 1000))
  expect_no_error(implementation_shortfall(100, 100, "SELL", 1000))
  expect_no_error(implementation_shortfall(100, 100, "buy", 1000))
})

test_that("obizhaeva_wang_impact calculates correctly", {
  impact <- obizhaeva_wang_impact(Q = 50000, k = 0.1, alpha = 0.5)

  expect_type(impact, "double")
  expect_gt(impact, 0)

  # Check formula: k * sign(Q) * |Q|^alpha
  expected <- 0.1 * sqrt(50000)
  expect_equal(impact, expected, tolerance = 1e-6)
})

test_that("obizhaeva_wang_impact handles negative orders", {
  impact_buy <- obizhaeva_wang_impact(Q = 50000, k = 0.1, alpha = 0.5)
  impact_sell <- obizhaeva_wang_impact(Q = -50000, k = 0.1, alpha = 0.5)

  # Magnitudes should be equal
  expect_equal(abs(impact_buy), abs(impact_sell))
  # Signs should be opposite
  expect_equal(sign(impact_buy), -sign(impact_sell))
})

test_that("obizhaeva_wang_impact includes decay when requested", {
  impact_no_decay <- obizhaeva_wang_impact(Q = 50000, k = 0.1, alpha = 0.5,
                                            include_decay = FALSE)
  impact_with_decay <- obizhaeva_wang_impact(Q = 50000, k = 0.1, alpha = 0.5,
                                               tau = 10, include_decay = TRUE)

  # Decay should reduce impact
  expect_lt(abs(impact_with_decay), abs(impact_no_decay))
})

test_that("obizhaeva_wang_impact requires tau when using decay", {
  expect_error(
    obizhaeva_wang_impact(Q = 50000, k = 0.1, include_decay = TRUE),
    "tau must be positive"
  )
})

test_that("predict_execution_cost compares multiple models", {
  comparison <- predict_execution_cost(
    Q = 50000,
    V = 2000000,
    sigma = 0.02,
    T_horizon = 30
  )

  expect_s3_class(comparison, "data.frame")
  expect_equal(nrow(comparison), 4)  # 4 models
  expect_true("model" %in% names(comparison))
  expect_true("predicted_cost_bps" %in% names(comparison))

  # All models should predict positive cost
  expect_true(all(comparison$predicted_cost_bps > 0))
})

test_that("predict_execution_cost accepts custom parameters", {
  custom_params <- list(
    Y = 0.30,
    gamma = 0.15,
    eta = 0.08,
    lambda = 1e-5
  )

  comparison <- predict_execution_cost(
    Q = 50000,
    V = 2000000,
    sigma = 0.02,
    T_horizon = 30,
    params = custom_params
  )

  expect_s3_class(comparison, "data.frame")
  expect_equal(nrow(comparison), 4)
})

test_that("print methods work for impact objects", {
  # Almgren-Chriss
  ac <- almgren_chriss_trajectory(Q = 100000, T_horizon = 30)
  expect_output(print(ac), "Almgren-Chriss")
  expect_output(print(ac), "Implementation shortfall")

  # Square-root calibration (with synthetic data)
  executions <- data.frame(
    order_size = c(10000, 25000, 50000),
    avg_daily_volume = 2000000,
    realized_impact = c(0.0005, 0.0008, 0.0011),
    volatility = 0.02
  )
  calib <- calibrate_sqrt_law(executions)
  expect_output(print(calib), "Square-Root Law")
  expect_output(print(calib), "Estimated Y parameter")

  # Impact decomposition
  trades <- simulate_orders(n = 500, seed = 42)
  decomp <- decompose_price_impact(trades)
  expect_output(print(decomp), "Impact Decomposition")
  expect_output(print(decomp), "temporary impact")
})

test_that("almgren_chriss trajectory is front-loaded with high urgency", {
  patient <- almgren_chriss_trajectory(Q = 100000, T_horizon = 30, lambda = 1e-7)
  urgent <- almgren_chriss_trajectory(Q = 100000, T_horizon = 30, lambda = 1e-4)

  # Get fraction executed in first 25% of time
  patient_early <- patient$trajectory$fraction_complete[6] # 25% of 20 steps
  urgent_early <- urgent$trajectory$fraction_complete[6]

  # Urgent should execute more in early period
  expect_gt(urgent_early, patient_early)
})
