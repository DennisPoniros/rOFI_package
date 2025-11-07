#' rOFI: Compute and Visualize Order-Flow Imbalance Patterns
#'
#' @description
#' The rOFI package provides tools for computing and visualizing order-flow 
#' imbalance (OFI) signals from trade-level data. It includes functions for
#' calculating simple OFI, imbalance ratios, cumulative patterns, and 
#' dollar-weighted metrics, along with visualization tools and synthetic
#' data generation for educational purposes.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{compute_ofi}}: Calculate OFI metrics with various window types
#'   \item \code{\link{plot_ofi}}: Create time-series visualizations of OFI metrics
#'   \item \code{\link{simulate_orders}}: Generate synthetic order flow data
#'   \item \code{\link{as_ofi}}: Standardize trade data for OFI computation
#' }
#'
#' @section Getting Started:
#' \preformatted{
#' # Generate sample data
#' trades <- simulate_orders(n = 1000)
#' 
#' # Compute OFI metrics
#' ofi <- compute_ofi(trades, window = "1 min")
#' 
#' # Visualize results
#' plot_ofi(ofi)
#' }
#'
#' @docType package
#' @name rOFI
#' @import dplyr
#' @import ggplot2
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate floor_date is.POSIXct with_tz seconds dseconds
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom slider slide_period_dbl slide_index_dbl
#' @importFrom stats rexp rbinom rlnorm rnorm cumsum median
#' @importFrom rlang .data := !! sym
NULL

#' Sample order flow data
#'
#' A dataset containing simulated trade-level events for demonstration purposes.
#'
#' @format A tibble with 1000 rows and 4 variables:
#' \describe{
#'   \item{timestamp}{POSIXct timestamp of the trade}
#'   \item{side}{Character, "B" for buy or "S" for sell}
#'   \item{size}{Numeric, number of shares/contracts traded}
#'   \item{price}{Numeric, execution price}
#' }
#' @source Generated using \code{simulate_orders()} with seed = 42
"ofi_demo"
