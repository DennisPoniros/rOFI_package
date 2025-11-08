#' Price Impact Models for Optimal Execution
#'
#' @description
#' Implements academic models for market impact, optimal execution,
#' and transaction cost analysis. Includes Almgren-Chriss framework,
#' square-root law, and propagator models.
#'
#' @name price_impact_models
NULL

#' Calculate optimal execution trajectory using Almgren-Chriss model
#'
#' @description
#' Computes the optimal trading trajectory that minimizes expected cost
#' subject to risk aversion. Balances market impact against execution risk.
#'
#' @param Q Numeric, total shares to execute (positive for buy, negative for sell)
#' @param T_horizon Numeric, execution horizon in seconds or minutes
#' @param lambda Numeric, risk aversion parameter (default: 1e-6)
#' @param sigma Numeric, volatility (annual, will be scaled to horizon)
#' @param gamma Numeric, permanent impact coefficient (default: 0.1)
#' @param eta Numeric, temporary impact coefficient (default: 0.05)
#' @param n_steps Integer, number of time steps (default: 20)
#' @param time_unit Character, "seconds" or "minutes" (default: "minutes")
#'
#' @return A list containing:
#'   \describe{
#'     \item{trajectory}{Data frame with time, position, and trade schedule}
#'     \item{expected_cost}{Expected execution cost}
#'     \item{expected_variance}{Variance of execution cost}
#'     \item{expected_shortfall}{Expected implementation shortfall}
#'     \item{parameters}{List of model parameters}
#'   }
#'
#' @details
#' The Almgren-Chriss model decomposes market impact into:
#' - **Permanent impact**: \eqn{\gamma \times v} (affects all future trades)
#' - **Temporary impact**: \eqn{\eta \times v/T} (affects only current trade)
#'
#' The optimal trajectory follows:
#' \deqn{x_t = Q \times \frac{\sinh(\kappa(T-t))}{\sinh(\kappa T)}}
#'
#' where \eqn{\kappa = \sqrt{\lambda \sigma^2 / \eta}} captures the trade-off
#' between execution risk and market impact.
#'
#' **Risk Aversion Interpretation**:
#' - \eqn{\lambda = 0}: No risk penalty, linear VWAP trajectory
#' - \eqn{\lambda \to \infty}: Maximum urgency, execute immediately
#' - Typical values: \eqn{10^{-6}} to \eqn{10^{-4}} for institutional traders
#'
#' @references
#' Almgren, R., & Chriss, N. (2001). Optimal execution of portfolio transactions.
#' *Journal of Risk*, 3, 5-40.
#'
#' @export
#' @examples
#' # Execute 100,000 shares over 30 minutes
#' trajectory <- almgren_chriss_trajectory(
#'   Q = 100000,
#'   T_horizon = 30,
#'   lambda = 1e-6,
#'   sigma = 0.30,      # 30% annual volatility
#'   gamma = 0.1,
#'   eta = 0.05,
#'   n_steps = 30,
#'   time_unit = "minutes"
#' )
#'
#' # View trade schedule
#' head(trajectory$trajectory)
#'
#' # Expected costs
#' cat("Expected cost:", trajectory$expected_cost, "\n")
#' cat("Cost std dev:", sqrt(trajectory$expected_variance), "\n")
#'
#' # Compare patient vs. urgent execution
#' patient <- almgren_chriss_trajectory(Q = 50000, T_horizon = 60, lambda = 1e-7)
#' urgent <- almgren_chriss_trajectory(Q = 50000, T_horizon = 60, lambda = 1e-5)
#'
#' cat("Patient cost:", patient$expected_cost, "\n")
#' cat("Urgent cost:", urgent$expected_cost, "\n")
almgren_chriss_trajectory <- function(Q,
                                       T_horizon,
                                       lambda = 1e-6,
                                       sigma = 0.30,
                                       gamma = 0.1,
                                       eta = 0.05,
                                       n_steps = 20,
                                       time_unit = c("minutes", "seconds")) {

  time_unit <- match.arg(time_unit)

  # Input validation
  if (Q == 0) stop("Q must be non-zero")
  if (T_horizon <= 0) stop("T_horizon must be positive")
  if (lambda < 0) stop("lambda must be non-negative")
  if (sigma <= 0) stop("sigma must be positive")
  if (gamma < 0 || eta < 0) stop("gamma and eta must be non-negative")
  if (n_steps < 2) stop("n_steps must be at least 2")

  # Scale volatility to appropriate time unit
  # Assume sigma is annual volatility
  if (time_unit == "minutes") {
    # Scale to per-minute volatility
    sigma_scaled <- sigma / sqrt(252 * 6.5 * 60)  # Trading minutes per year
  } else {
    # Scale to per-second volatility
    sigma_scaled <- sigma / sqrt(252 * 6.5 * 3600)  # Trading seconds per year
  }

  # Calculate kappa (urgency parameter)
  kappa <- sqrt(lambda * sigma_scaled^2 / eta)

  # Time grid
  dt <- T_horizon / n_steps
  times <- seq(0, T_horizon, length.out = n_steps + 1)

  # Optimal trajectory: x(t) = Q * sinh(kappa * (T - t)) / sinh(kappa * T)
  if (kappa < 1e-10) {
    # Linear trajectory when risk aversion is near zero
    positions <- Q * (1 - times / T_horizon)
  } else {
    positions <- Q * sinh(kappa * (T_horizon - times)) / sinh(kappa * T_horizon)
  }

  # Trade schedule (differences in positions)
  trades <- c(-diff(positions))
  trades <- c(trades, positions[length(positions)])  # Final liquidation

  # Cumulative trades
  cum_trades <- cumsum(trades)

  # Calculate expected cost
  # Permanent impact cost
  perm_cost <- gamma * sum(trades)^2 / 2

  # Temporary impact cost
  temp_cost <- eta * sum(trades^2) / (2 * dt)

  # Expected cost (implementation shortfall)
  expected_cost <- perm_cost + temp_cost

  # Calculate variance of cost
  # Variance from market risk
  variance_cost <- sigma_scaled^2 * sum((T_horizon - times[1:n_steps]) * trades^2)

  # Create trajectory data frame
  trajectory_df <- data.frame(
    time = times[1:(n_steps + 1)],
    position_remaining = positions,
    trade_size = c(trades, 0),  # Add 0 for final time point
    cumulative_executed = c(0, cum_trades),
    fraction_complete = c(0, cum_trades) / Q
  )

  # Package results
  result <- list(
    trajectory = trajectory_df,
    expected_cost = expected_cost,
    expected_variance = variance_cost,
    expected_shortfall = expected_cost,
    parameters = list(
      Q = Q,
      T_horizon = T_horizon,
      lambda = lambda,
      sigma = sigma,
      sigma_scaled = sigma_scaled,
      gamma = gamma,
      eta = eta,
      kappa = kappa,
      n_steps = n_steps,
      time_unit = time_unit
    )
  )

  class(result) <- c("almgren_chriss", "list")
  return(result)
}


#' Estimate market impact using square-root law
#'
#' @description
#' Implements the universal square-root impact law that appears across
#' markets: ΔP = Y × σ × √(Q/V), where Y ≈ 0.2 for equities.
#'
#' @param Q Numeric, order size (shares)
#' @param V Numeric, typical daily volume or volume during execution
#' @param sigma Numeric, volatility (daily or appropriate horizon)
#' @param Y Numeric, market-specific coefficient (default: 0.2 for equities)
#' @param bps Logical, return result in basis points (default: TRUE)
#'
#' @return Numeric, predicted price impact
#'
#' @details
#' The square-root law is an empirical regularity observed across markets:
#' - **Sublinear**: Doubling order size increases impact by only 41%
#' - **Universal**: Appears in equities, futures, FX, crypto
#' - **Dimensional analysis**: Units are consistent
#'
#' The exponent 0.5 can vary from 0.4 to 0.6 depending on market conditions.
#'
#' **Typical Y values by market**:
#' - Equities: 0.15 - 0.25
#' - Futures: 0.10 - 0.20
#' - FX: 0.05 - 0.15
#' - Crypto: 0.30 - 0.50 (higher due to less liquidity)
#'
#' @references
#' Almgren, R., Thum, C., Hauptmann, E., & Li, H. (2005). Direct estimation of
#' equity market impact. *Risk*, 18(7), 58-62.
#'
#' Kyle, A. S., & Obizhaeva, A. A. (2016). Market microstructure invariance:
#' Empirical hypotheses. *Econometrica*, 84(4), 1345-1404.
#'
#' @export
#' @examples
#' # Impact of 50,000 share order in stock with 2M daily volume
#' sqrt_impact(
#'   Q = 50000,
#'   V = 2000000,
#'   sigma = 0.02,  # 2% daily volatility
#'   Y = 0.20
#' )
#'
#' # Compare impact across different order sizes
#' sizes <- c(10000, 25000, 50000, 100000)
#' impacts <- sapply(sizes, function(q) {
#'   sqrt_impact(Q = q, V = 2000000, sigma = 0.02)
#' })
#'
#' data.frame(
#'   order_size = sizes,
#'   impact_bps = impacts,
#'   participation_rate = sizes / 2000000
#' )
sqrt_impact <- function(Q, V, sigma, Y = 0.20, bps = TRUE) {

  # Input validation
  if (Q <= 0) stop("Q must be positive")
  if (V <= 0) stop("V must be positive")
  if (sigma <= 0) stop("sigma must be positive")
  if (Y <= 0) stop("Y must be positive")

  # Calculate impact
  impact <- Y * sigma * sqrt(Q / V)

  # Convert to basis points if requested
  if (bps) {
    impact <- impact * 10000
  }

  return(impact)
}


#' Calibrate square-root law from historical executions
#'
#' @description
#' Estimates the Y parameter of the square-root law from historical
#' execution data using nonlinear regression.
#'
#' @param executions Data frame with columns: order_size, avg_daily_volume,
#'   realized_impact (in price units), volatility
#' @param initial_Y Numeric, initial guess for Y (default: 0.20)
#' @param exponent_fixed Logical, fix exponent at 0.5 (default: TRUE)
#'
#' @return A list containing:
#'   \describe{
#'     \item{Y}{Estimated Y parameter}
#'     \item{exponent}{Estimated exponent (if not fixed)}
#'     \item{R_squared}{Model fit quality}
#'     \item{predictions}{Predicted vs. actual impacts}
#'     \item{rmse}{Root mean squared error}
#'   }
#'
#' @details
#' Fits the model: \eqn{\Delta P = Y \times \sigma \times (Q/V)^{\beta}}
#'
#' If `exponent_fixed = FALSE`, estimates both Y and β. This tests
#' whether the square-root exponent holds in your data.
#'
#' @export
#' @importFrom stats nls predict
#' @examples
#' \dontrun{
#' # Create sample execution data
#' executions <- data.frame(
#'   order_size = c(10000, 25000, 50000, 75000, 100000),
#'   avg_daily_volume = 2000000,
#'   realized_impact = c(0.0005, 0.0008, 0.0012, 0.0014, 0.0016),
#'   volatility = 0.02
#' )
#'
#' # Calibrate model
#' calibration <- calibrate_sqrt_law(executions)
#' print(calibration$Y)
#' print(calibration$R_squared)
#' }
calibrate_sqrt_law <- function(executions,
                                 initial_Y = 0.20,
                                 exponent_fixed = TRUE) {

  # Validate input
  required_cols <- c("order_size", "avg_daily_volume", "realized_impact", "volatility")
  missing <- setdiff(required_cols, names(executions))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  if (nrow(executions) < 3) {
    stop("Need at least 3 executions to calibrate")
  }

  # Prepare data
  Q <- executions$order_size
  V <- executions$avg_daily_volume
  impact <- executions$realized_impact
  sigma <- executions$volatility

  # Remove any NAs
  valid <- complete.cases(Q, V, impact, sigma)
  Q <- Q[valid]
  V <- V[valid]
  impact <- impact[valid]
  sigma <- sigma[valid]

  if (length(Q) < 3) {
    stop("Insufficient valid data after removing NAs")
  }

  # Fit model
  if (exponent_fixed) {
    # Linear regression on log-transformed equation
    # log(impact / sigma) = log(Y) + 0.5 * log(Q/V)

    log_impact_scaled <- log(impact / sigma)
    log_Q_V <- log(Q / V)

    fit <- lm(log_impact_scaled ~ I(0.5 * log_Q_V))

    Y_est <- exp(coef(fit)[1])
    exponent_est <- 0.5

    # Predictions
    predictions <- Y_est * sigma * sqrt(Q / V)

  } else {
    # Estimate both Y and exponent using nonlinear least squares
    tryCatch({
      fit <- nls(
        impact ~ Y * sigma * (Q / V)^beta,
        start = list(Y = initial_Y, beta = 0.5),
        data = data.frame(Q = Q, V = V, impact = impact, sigma = sigma)
      )

      Y_est <- coef(fit)["Y"]
      exponent_est <- coef(fit)["beta"]

      # Predictions
      predictions <- Y_est * sigma * (Q / V)^exponent_est

    }, error = function(e) {
      stop("Nonlinear fit failed. Try exponent_fixed = TRUE. Error: ", e$message)
    })
  }

  # Calculate fit quality
  residuals <- impact - predictions
  ss_res <- sum(residuals^2)
  ss_tot <- sum((impact - mean(impact))^2)
  R_squared <- 1 - ss_res / ss_tot

  rmse <- sqrt(mean(residuals^2))

  # Package results
  result <- list(
    Y = Y_est,
    exponent = exponent_est,
    R_squared = R_squared,
    rmse = rmse,
    predictions = data.frame(
      actual_impact = impact,
      predicted_impact = predictions,
      residual = residuals,
      order_size = Q,
      participation_rate = Q / V
    ),
    model = fit
  )

  class(result) <- c("sqrt_calibration", "list")
  return(result)
}


#' Decompose price impact into temporary and permanent components
#'
#' @description
#' Separates observed price impact into temporary (mean-reverting) and
#' permanent (information-driven) components using lagged price changes.
#'
#' @param trades Trade data with timestamp, side, size, price
#' @param window Character, aggregation window (default: "1 min")
#' @param decay_periods Integer, number of periods for reversion (default: 5)
#' @param min_obs Integer, minimum observations for estimation (default: 10)
#'
#' @return A list containing:
#'   \describe{
#'     \item{decomposition}{Data frame with temporary and permanent impact}
#'     \item{avg_temporary}{Average temporary impact}
#'     \item{avg_permanent}{Average permanent impact}
#'     \item{pct_permanent}{Percentage of impact that is permanent}
#'     \item{decay_rate}{Estimated decay rate of temporary impact}
#'   }
#'
#' @details
#' The decomposition uses the methodology from Almgren et al. (2005):
#'
#' 1. **Immediate impact**: Price change at execution
#' 2. **Decay observation**: Price `decay_periods` later
#' 3. **Permanent impact**: Long-run price change
#' 4. **Temporary impact**: Immediate - Permanent
#'
#' High temporary impact suggests liquidity provision opportunities.
#' High permanent impact suggests informed trading or large information content.
#'
#' @references
#' Almgren, R., Thum, C., Hauptmann, E., & Li, H. (2005). Direct estimation
#' of equity market impact. *Risk*, 18(7), 58-62.
#'
#' @export
#' @importFrom dplyr group_by summarise mutate lead lag
#' @importFrom lubridate floor_date
#'
#' @examples
#' # Simulate trades and analyze impact
#' trades <- simulate_orders(n = 2000, seed = 42)
#' impact_decomp <- decompose_price_impact(trades, window = "1 min", decay_periods = 5)
#'
#' # View decomposition
#' print(impact_decomp$avg_temporary)
#' print(impact_decomp$avg_permanent)
#' print(impact_decomp$pct_permanent)
decompose_price_impact <- function(trades,
                                     window = "1 min",
                                     decay_periods = 5,
                                     min_obs = 10) {

  # Validate input
  required <- c("timestamp", "side", "size", "price")
  missing <- setdiff(required, names(trades))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Aggregate by window
  windowed_data <- trades |>
    mutate(window = floor_date(.data$timestamp, window)) |>
    group_by(.data$window) |>
    summarise(
      mid_price = mean(.data$price, na.rm = TRUE),
      total_signed_volume = sum(
        ifelse(.data$side == "B", .data$size, -1 * .data$size),
        na.rm = TRUE
      ),
      n_trades = n(),
      .groups = "drop"
    ) |>
    arrange(.data$window)

  if (nrow(windowed_data) < min_obs) {
    stop("Insufficient data after windowing (need at least ", min_obs, " windows)")
  }

  # Calculate price changes
  windowed_data <- windowed_data |>
    mutate(
      # Immediate price change
      price_change = .data$mid_price - lag(.data$mid_price),

      # Price change after decay period
      price_change_decayed = lead(.data$mid_price, decay_periods) - .data$mid_price,

      # Total impact from current window to decay period
      cumulative_impact = lead(.data$mid_price, decay_periods) - lag(.data$mid_price)
    )

  # Estimate permanent impact (what remains after decay)
  # Use absolute signed volume to handle buys and sells
  windowed_data <- windowed_data |>
    mutate(
      # Sign of order flow
      flow_sign = sign(.data$total_signed_volume),

      # Permanent impact: what remains after decay
      permanent_impact = .data$price_change_decayed * .data$flow_sign,

      # Temporary impact: immediate impact minus permanent
      temporary_impact = .data$price_change * .data$flow_sign - .data$permanent_impact
    )

  # Remove NAs introduced by lag/lead
  windowed_data_clean <- windowed_data |>
    filter(
      !is.na(.data$price_change),
      !is.na(.data$price_change_decayed),
      !is.na(.data$permanent_impact),
      !is.na(.data$temporary_impact)
    )

  if (nrow(windowed_data_clean) < min_obs) {
    warning("Many NAs after impact calculation. Results may be unreliable.")
  }

  # Calculate summary statistics
  avg_temporary <- mean(abs(windowed_data_clean$temporary_impact), na.rm = TRUE)
  avg_permanent <- mean(abs(windowed_data_clean$permanent_impact), na.rm = TRUE)

  total_impact <- avg_temporary + avg_permanent
  pct_permanent <- if (total_impact > 0) (avg_permanent / total_impact) * 100 else NA

  # Estimate decay rate
  # Fit exponential decay: temporary_t = temporary_0 * exp(-lambda * t)
  # Simplified: just look at average decay
  if (nrow(windowed_data_clean) > 0) {
    decay_rate <- mean(abs(windowed_data_clean$temporary_impact) /
                        abs(windowed_data_clean$price_change), na.rm = TRUE)
  } else {
    decay_rate <- NA
  }

  # Package results
  result <- list(
    decomposition = windowed_data_clean,
    avg_temporary = avg_temporary,
    avg_permanent = avg_permanent,
    pct_permanent = pct_permanent,
    decay_rate = decay_rate,
    decay_periods = decay_periods,
    n_observations = nrow(windowed_data_clean)
  )

  class(result) <- c("impact_decomposition", "list")
  return(result)
}


#' Calculate implementation shortfall (slippage)
#'
#' @description
#' Measures the cost of execution relative to a benchmark (arrival price,
#' decision price, or VWAP). This is the industry standard for TCA.
#'
#' @param execution_price Numeric, average execution price
#' @param benchmark_price Numeric, benchmark price (arrival, decision, or VWAP)
#' @param side Character, "B" for buy, "S" for sell
#' @param quantity Numeric, total quantity executed
#' @param bps Logical, return in basis points (default: TRUE)
#'
#' @return Numeric, implementation shortfall
#'
#' @details
#' Implementation shortfall measures how much worse your execution was
#' compared to a theoretical benchmark:
#'
#' - **For buys**: IS = (execution_price - benchmark_price) / benchmark_price
#' - **For sells**: IS = (benchmark_price - execution_price) / benchmark_price
#'
#' **Common benchmarks**:
#' - **Arrival price**: Price when decision was made (pre-trade)
#' - **Open/Close**: Session boundaries
#' - **VWAP**: Volume-weighted average price over period
#' - **TWAP**: Time-weighted average price
#'
#' Positive IS indicates cost (you did worse than benchmark).
#' Negative IS indicates savings (you did better than benchmark).
#'
#' @export
#' @examples
#' # Buy execution compared to arrival price
#' implementation_shortfall(
#'   execution_price = 100.15,
#'   benchmark_price = 100.00,
#'   side = "B",
#'   quantity = 10000
#' )
#'
#' # Sell execution compared to VWAP
#' implementation_shortfall(
#'   execution_price = 99.85,
#'   benchmark_price = 100.00,
#'   side = "S",
#'   quantity = 5000
#' )
implementation_shortfall <- function(execution_price,
                                      benchmark_price,
                                      side,
                                      quantity,
                                      bps = TRUE) {

  # Input validation
  if (execution_price <= 0) stop("execution_price must be positive")
  if (benchmark_price <= 0) stop("benchmark_price must be positive")
  if (!side %in% c("B", "S", "BUY", "SELL", "buy", "sell")) {
    stop("side must be 'B' or 'S'")
  }
  if (quantity <= 0) stop("quantity must be positive")

  # Normalize side
  side <- toupper(substr(side, 1, 1))

  # Calculate shortfall
  if (side == "B") {
    # For buys: paying more than benchmark is worse
    shortfall <- (execution_price - benchmark_price) / benchmark_price
  } else {
    # For sells: receiving less than benchmark is worse
    shortfall <- (benchmark_price - execution_price) / benchmark_price
  }

  # Convert to basis points if requested
  if (bps) {
    shortfall <- shortfall * 10000
  }

  return(shortfall)
}


#' Estimate market impact using Obizhaeva-Wang propagator model
#'
#' @description
#' Implements the propagator model where permanent impact depends on
#' accumulated position with power-law decay. More sophisticated than
#' simple linear impact.
#'
#' @param Q Numeric, order size
#' @param k Numeric, impact coefficient (default: 0.1)
#' @param alpha Numeric, impact exponent (default: 0.5 for square-root)
#' @param tau Numeric, decay time constant (optional)
#' @param include_decay Logical, include temporal decay (default: FALSE)
#'
#' @return Numeric, predicted price impact
#'
#' @details
#' The Obizhaeva-Wang model specifies:
#' \deqn{P_t = P_0 + k \times \text{sign}(q_t) \times |q_t|^\alpha}
#'
#' Where:
#' - \eqn{q_t} is accumulated position
#' - \eqn{\alpha \approx 0.5} gives square-root impact
#' - \eqn{k} is market-specific impact coefficient
#'
#' With decay, impact diminishes over time:
#' \deqn{G(\tau) = \tau^{-\beta}}
#'
#' where \eqn{\beta \approx 0.5} is typical.
#'
#' @references
#' Obizhaeva, A. A., & Wang, J. (2013). Optimal trading strategy and
#' supply/demand dynamics. *Journal of Financial Markets*, 16(1), 1-32.
#'
#' @export
#' @examples
#' # Standard square-root impact
#' obizhaeva_wang_impact(Q = 50000, k = 0.1, alpha = 0.5)
#'
#' # Linear impact (for comparison)
#' obizhaeva_wang_impact(Q = 50000, k = 0.1, alpha = 1.0)
#'
#' # With decay
#' obizhaeva_wang_impact(Q = 50000, k = 0.1, alpha = 0.5,
#'                       tau = 10, include_decay = TRUE)
obizhaeva_wang_impact <- function(Q,
                                   k = 0.1,
                                   alpha = 0.5,
                                   tau = NULL,
                                   include_decay = FALSE) {

  # Input validation
  if (Q == 0) stop("Q must be non-zero")
  if (k <= 0) stop("k must be positive")
  if (alpha <= 0) stop("alpha must be positive")

  # Calculate base impact
  impact <- k * sign(Q) * abs(Q)^alpha

  # Apply decay if requested
  if (include_decay) {
    if (is.null(tau) || tau <= 0) {
      stop("tau must be positive when include_decay = TRUE")
    }

    # Power-law decay with exponent 0.5
    decay_factor <- tau^(-0.5)
    impact <- impact * decay_factor
  }

  return(impact)
}


#' Predict execution cost using multiple impact models
#'
#' @description
#' Compares cost predictions from different market impact models
#' (square-root, linear, Almgren-Chriss) for a given execution scenario.
#'
#' @param Q Numeric, total shares to execute
#' @param V Numeric, daily volume or relevant volume benchmark
#' @param sigma Numeric, volatility
#' @param T_horizon Numeric, execution horizon (minutes)
#' @param params List of model parameters (optional)
#'
#' @return Data frame comparing model predictions
#'
#' @export
#' @examples
#' predict_execution_cost(
#'   Q = 50000,
#'   V = 2000000,
#'   sigma = 0.02,
#'   T_horizon = 30
#' )
predict_execution_cost <- function(Q, V, sigma, T_horizon, params = list()) {

  # Default parameters
  Y <- params$Y %||% 0.20
  gamma <- params$gamma %||% 0.1
  eta <- params$eta %||% 0.05
  lambda <- params$lambda %||% 1e-6

  # Square-root law prediction
  sqrt_cost <- sqrt_impact(Q = Q, V = V, sigma = sigma, Y = Y, bps = TRUE)

  # Linear impact prediction (for comparison)
  participation_rate <- Q / V
  linear_cost <- participation_rate * sigma * 10000  # Convert to bps

  # Almgren-Chriss prediction
  ac_result <- almgren_chriss_trajectory(
    Q = Q,
    T_horizon = T_horizon,
    lambda = lambda,
    sigma = sigma,
    gamma = gamma,
    eta = eta,
    n_steps = 20
  )
  ac_cost <- ac_result$expected_cost

  # Power-law (generalized)
  power_cost <- Y * sigma * (Q / V)^0.6 * 10000  # Exponent 0.6 instead of 0.5

  # Create comparison
  comparison <- data.frame(
    model = c("Square-root Law", "Linear Impact", "Almgren-Chriss", "Power Law (0.6)"),
    predicted_cost_bps = c(sqrt_cost, linear_cost, ac_cost, power_cost),
    model_type = c("Empirical", "Simple", "Optimal Execution", "Empirical"),
    assumptions = c(
      "Universal law, Y=0.2",
      "Proportional to participation",
      "Risk-averse optimization",
      "Steeper than square-root"
    )
  )

  comparison$predicted_cost_dollars <- comparison$predicted_cost_bps / 10000 *
    (Q * sigma * 100)  # Rough estimate

  return(comparison)
}


# ============================================================================
# Print Methods
# ============================================================================

#' @export
print.almgren_chriss <- function(x, ...) {
  cat("Almgren-Chriss Optimal Execution\n")
  cat("=================================\n\n")

  cat("Parameters:\n")
  cat(sprintf("  Total quantity: %s shares\n", format(x$parameters$Q, big.mark = ",")))
  cat(sprintf("  Execution horizon: %s %s\n",
              x$parameters$T_horizon, x$parameters$time_unit))
  cat(sprintf("  Risk aversion (lambda): %.2e\n", x$parameters$lambda))
  cat(sprintf("  Urgency parameter (kappa): %.4f\n", x$parameters$kappa))
  cat("\n")

  cat("Expected Costs:\n")
  cat(sprintf("  Implementation shortfall: $%.2f\n", x$expected_shortfall))
  cat(sprintf("  Cost std deviation: $%.2f\n", sqrt(x$expected_variance)))
  cat("\n")

  cat("Trajectory (first 5 steps):\n")
  print(head(x$trajectory, 5))

  invisible(x)
}


#' @export
print.sqrt_calibration <- function(x, ...) {
  cat("Square-Root Law Calibration\n")
  cat("===========================\n\n")

  cat(sprintf("Estimated Y parameter: %.4f\n", x$Y))
  cat(sprintf("Exponent: %.4f\n", x$exponent))
  cat(sprintf("R-squared: %.4f\n", x$R_squared))
  cat(sprintf("RMSE: %.6f\n", x$rmse))
  cat("\n")

  cat("Predictions (first 5):\n")
  print(head(x$predictions, 5))

  invisible(x)
}


#' @export
print.impact_decomposition <- function(x, ...) {
  cat("Price Impact Decomposition\n")
  cat("==========================\n\n")

  cat(sprintf("Average temporary impact: %.6f\n", x$avg_temporary))
  cat(sprintf("Average permanent impact: %.6f\n", x$avg_permanent))
  cat(sprintf("Permanent impact %%: %.2f%%\n", x$pct_permanent))
  cat(sprintf("Decay periods: %d\n", x$decay_periods))
  cat(sprintf("Observations: %d\n", x$n_observations))
  cat("\n")

  cat("Interpretation:\n")
  if (!is.na(x$pct_permanent)) {
    if (x$pct_permanent > 70) {
      cat("  - HIGH permanent impact: Strong information content in trades\n")
    } else if (x$pct_permanent < 30) {
      cat("  - HIGH temporary impact: Liquidity provision opportunities\n")
    } else {
      cat("  - BALANCED impact: Mix of information and liquidity effects\n")
    }
  }

  invisible(x)
}


# NULL default operator (if not already defined)
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
