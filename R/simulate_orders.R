#' Simulate synthetic order flow data
#'
#' @description 
#' Generates synthetic trade-level data with configurable arrival rates, 
#' buy/sell imbalance, and price dynamics. Useful for testing, examples,
#' and educational demonstrations.
#'
#' @param n Number of trade events to generate (default: 10000)
#' @param start Starting timestamp (default: Sys.time())
#' @param lambda Expected events per minute (Poisson arrival rate, default: 5)
#' @param imb Target mean imbalance between -1 and 1 (default: 0.1, slight buy bias)
#' @param price0 Initial price (default: 100)
#' @param drift Price drift per minute (default: 0)
#' @param vol Price volatility as percentage (default: 0.5)
#' @param tz Timezone for timestamps (default: "UTC")
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A tibble with columns: timestamp, side, size, price
#'
#' @details
#' Trade arrivals follow a Poisson process with rate lambda per minute.
#' Trade sides are determined by the imbalance parameter:
#' - imb = 0: equal probability of buys and sells
#' - imb > 0: more buys than sells
#' - imb < 0: more sells than buys
#' 
#' Trade sizes are drawn from a log-normal distribution.
#' Prices follow a simple random walk with optional drift and volatility.
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom lubridate seconds
#' @importFrom stats rexp rbinom rlnorm rnorm
#'
#' @examples
#' # Generate balanced order flow
#' trades <- simulate_orders(n = 1000, imb = 0, seed = 123)
#' 
#' # Generate order flow with buy pressure
#' bullish_trades <- simulate_orders(n = 1000, imb = 0.3, drift = 0.1)
#' 
#' # High-frequency data (50 trades per minute)
#' hft_trades <- simulate_orders(n = 5000, lambda = 50)
simulate_orders <- function(n = 10000,
                          start = Sys.time(),
                          lambda = 5,
                          imb = 0.1,
                          price0 = 100,
                          drift = 0,
                          vol = 0.5,
                          tz = "UTC",
                          seed = NULL) {
  
  # Validate inputs
  if (n <= 0) stop("n must be positive")
  if (lambda <= 0) stop("lambda must be positive")
  if (imb < -1 || imb > 1) stop("imb must be between -1 and 1")
  if (price0 <= 0) stop("price0 must be positive")
  if (vol < 0) stop("vol must be non-negative")
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Generate inter-arrival times (exponential distribution)
  # lambda is events per minute, so we work in seconds
  inter_arrivals <- rexp(n, rate = lambda / 60)
  arrival_times <- cumsum(inter_arrivals)
  
  # Create timestamps
  timestamps <- start + lubridate::seconds(arrival_times)
  
  # Ensure timezone
  if (!is.null(tz)) {
    timestamps <- lubridate::with_tz(timestamps, tz)
  }
  
  # Generate trade sides based on imbalance
  # Convert imbalance to buy probability
  buy_prob <- (1 + imb) / 2
  is_buy <- rbinom(n, 1, buy_prob)
  sides <- ifelse(is_buy == 1, "B", "S")
  
  # Generate trade sizes (log-normal distribution)
  # Mean size around 500, with some variability
  sizes <- round(rlnorm(n, meanlog = log(500), sdlog = 0.5))
  sizes[sizes == 0] <- 1  # Ensure no zero sizes
  
  # Generate prices (random walk with drift)
  # Price changes per trade (not per minute, so scale drift)
  minutes_elapsed <- arrival_times / 60
  drift_per_trade <- c(0, diff(minutes_elapsed)) * drift
  
  # Random component
  price_changes <- rnorm(n, mean = drift_per_trade, sd = vol/100 * price0)
  prices <- price0 + cumsum(price_changes)
  prices <- round(prices, 2)
  prices[prices <= 0] <- 0.01  # Ensure positive prices
  
  # Create output tibble
  tibble::tibble(
    timestamp = timestamps,
    side = sides,
    size = sizes,
    price = prices
  )
}

#' Generate small demo dataset
#' 
#' @description
#' Creates a small, reproducible dataset for examples and testing.
#' 
#' @param n Number of observations (default: 1000)
#' 
#' @return A tibble with synthetic order flow data
#' @export
#' 
#' @examples
#' demo_data <- generate_demo_data()
#' head(demo_data)
generate_demo_data <- function(n = 1000) {
  simulate_orders(
    n = n,
    start = as.POSIXct("2024-01-15 09:30:00", tz = "America/New_York"),
    lambda = 10,
    imb = 0.05,
    price0 = 150,
    drift = 0.02,
    vol = 0.3,
    seed = 42
  )
}
