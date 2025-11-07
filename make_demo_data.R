#!/usr/bin/env Rscript

# Script to generate and save demo dataset for rOFI package
# This should be run once to create the data/ofi_demo.rda file

library(tibble)
library(lubridate)

# Function to simulate orders (simplified version for data generation)
simulate_orders_internal <- function(n = 1000,
                                    start = as.POSIXct("2024-01-15 09:30:00", tz = "America/New_York"),
                                    lambda = 10,
                                    imb = 0.05,
                                    price0 = 150,
                                    drift = 0.02,
                                    vol = 0.3,
                                    seed = 42) {
  
  set.seed(seed)
  
  # Generate inter-arrival times
  inter_arrivals <- rexp(n, rate = lambda / 60)
  arrival_times <- cumsum(inter_arrivals)
  
  # Create timestamps
  timestamps <- start + seconds(arrival_times)
  
  # Generate trade sides
  buy_prob <- (1 + imb) / 2
  is_buy <- rbinom(n, 1, buy_prob)
  sides <- ifelse(is_buy == 1, "B", "S")
  
  # Generate trade sizes
  sizes <- round(rlnorm(n, meanlog = log(500), sdlog = 0.5))
  sizes[sizes == 0] <- 1
  
  # Generate prices
  minutes_elapsed <- arrival_times / 60
  drift_per_trade <- c(0, diff(minutes_elapsed)) * drift
  price_changes <- rnorm(n, mean = drift_per_trade, sd = vol/100 * price0)
  prices <- price0 + cumsum(price_changes)
  prices <- round(prices, 2)
  prices[prices <= 0] <- 0.01
  
  tibble(
    timestamp = timestamps,
    side = sides,
    size = sizes,
    price = prices
  )
}

# Generate the demo dataset
ofi_demo <- simulate_orders_internal()

# Create data directory if it doesn't exist
if (!dir.exists("../data")) {
  dir.create("../data")
}

# Save the dataset
save(ofi_demo, file = "../data/ofi_demo.rda", compress = "bzip2")

cat("Demo dataset created successfully with", nrow(ofi_demo), "rows\n")
cat("Saved to data/ofi_demo.rda\n")

# Display first few rows
print(head(ofi_demo))

# Summary statistics
cat("\nSummary statistics:\n")
cat("Date range:", format(range(ofi_demo$timestamp)), "\n")
cat("Buy trades:", sum(ofi_demo$side == "B"), "\n")
cat("Sell trades:", sum(ofi_demo$side == "S"), "\n")
cat("Price range:", range(ofi_demo$price), "\n")
cat("Size range:", range(ofi_demo$size), "\n")
