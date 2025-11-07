#' Plot Order-Flow Imbalance metrics
#'
#' @description 
#' Creates time-series plots of OFI metrics with sensible defaults
#' and customizable options. Supports multiple metrics in a single plot
#' or faceted display.
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics
#' @param which Character vector of metrics to plot (default: c("ofi", "oir", "ofi_cum"))
#' @param ref_line Numeric value for horizontal reference line (default: 0)
#' @param facet Logical, whether to use facets for multiple metrics (default: TRUE)
#' @param title Plot title (optional)
#' @param subtitle Plot subtitle (optional)
#'
#' @return A ggplot2 object
#'
#' @details
#' Available metrics to plot:
#' - "ofi": Order-flow imbalance
#' - "oir": Order imbalance ratio
#' - "ofi_cum": Cumulative OFI
#' - "vol_total": Total volume
#' - "B": Buy volume
#' - "S": Sell volume
#' - "ofi_dollar": Dollar-weighted OFI (if available)
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_hline theme_minimal labs
#'   scale_y_continuous facet_wrap theme element_text
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter select all_of
#' @importFrom rlang .data
#'
#' @examples
#' # ===========================================
#' # Example 1: Basic Plotting
#' # ===========================================
#'
#' # Generate data and compute OFI
#' trades <- simulate_orders(n = 1000, seed = 123)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Plot default metrics (OFI, OIR, Cumulative OFI)
#' plot_ofi(ofi)
#'
#' # ===========================================
#' # Example 2: Plot Individual Metrics
#' # ===========================================
#'
#' # Just OFI (raw order-flow imbalance)
#' plot_ofi(ofi, which = "ofi")
#'
#' # Just OIR (normalized -1 to 1)
#' plot_ofi(ofi, which = "oir")
#'
#' # Just cumulative OFI (shows trend)
#' plot_ofi(ofi, which = "ofi_cum")
#'
#' # Total volume
#' plot_ofi(ofi, which = "vol_total")
#'
#' # ===========================================
#' # Example 3: Compare Multiple Metrics
#' # ===========================================
#'
#' # Compare OFI and volume
#' plot_ofi(ofi, which = c("ofi", "vol_total"))
#'
#' # Buy vs Sell volume
#' plot_ofi(ofi, which = c("B", "S"))
#'
#' # All metrics
#' plot_ofi(ofi, which = c("ofi", "oir", "ofi_cum", "vol_total"))
#'
#' # ===========================================
#' # Example 4: Customization
#' # ===========================================
#'
#' # Without facets (single panel)
#' plot_ofi(ofi, which = "ofi", facet = FALSE)
#'
#' # With custom title
#' plot_ofi(ofi,
#'          which = "ofi",
#'          title = "Order-Flow Imbalance Analysis",
#'          subtitle = "1-minute windows")
#'
#' # Change reference line
#' plot_ofi(ofi, which = "oir", ref_line = 0.2)
#'
#' # ===========================================
#' # Example 5: Advanced ggplot2 Customization
#' # ===========================================
#'
#' \dontrun{
#' # Add custom ggplot2 layers
#' library(ggplot2)
#'
#' plot_ofi(ofi, which = "ofi") +
#'   geom_hline(yintercept = c(-1000, 1000),
#'              linetype = "dashed",
#'              color = "red") +
#'   theme_minimal() +
#'   labs(title = "My Custom OFI Plot")
#'
#' # Different color scheme
#' plot_ofi(ofi, which = "oir") +
#'   scale_color_manual(values = c("darkblue"))
#' }
plot_ofi <- function(ofi_tbl,
                    which = c("ofi", "oir", "ofi_cum"),
                    ref_line = 0,
                    facet = TRUE,
                    title = NULL,
                    subtitle = NULL) {
  
  # Validate input
  if (!is.data.frame(ofi_tbl) || !"window_start" %in% names(ofi_tbl)) {
    stop("ofi_tbl must be output from compute_ofi()")
  }
  
  # Check requested metrics exist
  available_metrics <- intersect(which, names(ofi_tbl))
  if (length(available_metrics) == 0) {
    stop("None of the requested metrics found in ofi_tbl")
  }
  if (length(available_metrics) < length(which)) {
    missing <- setdiff(which, available_metrics)
    warning("Metrics not found in data: ", paste(missing, collapse = ", "))
  }
  
  # Prepare data for plotting
  plot_data <- ofi_tbl |>
    select(all_of(c("window_start", available_metrics)))
  
  # Create appropriate plot based on number of metrics
  if (length(available_metrics) == 1) {
    # Single metric plot
    p <- ggplot(plot_data, aes(x = .data$window_start, y = .data[[available_metrics[1]]])) +
      geom_line(color = "#2E86AB", linewidth = 0.8) +
      theme_minimal(base_size = 12) +
      labs(
        x = "Time",
        y = get_metric_label(available_metrics[1]),
        title = title %||% paste("Order-Flow Imbalance:", get_metric_label(available_metrics[1])),
        subtitle = subtitle
      )
    
    # Add reference line if not NA
    if (!is.na(ref_line)) {
      p <- p + geom_hline(yintercept = ref_line, linetype = "dashed", 
                         color = "gray40", alpha = 0.6)
    }
    
  } else {
    # Multiple metrics
    plot_data_long <- plot_data |>
      pivot_longer(cols = -window_start, names_to = "metric", values_to = "value") |>
      mutate(metric_label = sapply(.data$metric, get_metric_label))
    
    if (facet) {
      # Faceted plot
      p <- ggplot(plot_data_long, aes(x = .data$window_start, y = .data$value)) +
        geom_line(color = "#2E86AB", linewidth = 0.8) +
        facet_wrap(~ metric_label, scales = "free_y", ncol = 1) +
        theme_minimal(base_size = 11) +
        labs(
          x = "Time",
          y = NULL,
          title = title %||% "Order-Flow Imbalance Metrics",
          subtitle = subtitle
        )
      
      # Add reference line to each facet
      if (!is.na(ref_line)) {
        p <- p + geom_hline(yintercept = ref_line, linetype = "dashed",
                           color = "gray40", alpha = 0.6)
      }
      
    } else {
      # Single plot with multiple lines (only works well for similar scales)
      warning("Multiple metrics on single plot may have scaling issues. Consider facet = TRUE")
      
      p <- ggplot(plot_data_long, aes(x = .data$window_start, y = .data$value, 
                                      color = .data$metric_label)) +
        geom_line(linewidth = 0.8) +
        theme_minimal(base_size = 12) +
        labs(
          x = "Time",
          y = "Value",
          color = "Metric",
          title = title %||% "Order-Flow Imbalance Metrics",
          subtitle = subtitle
        ) +
        scale_color_manual(values = c("#2E86AB", "#A23B72", "#F18F01", 
                                     "#C73E1D", "#6A994E", "#BC4B51"))
      
      if (!is.na(ref_line)) {
        p <- p + geom_hline(yintercept = ref_line, linetype = "dashed",
                           color = "gray40", alpha = 0.6)
      }
    }
  }
  
  # Enhance theme
  p <- p + theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title = element_text(size = 11),
    legend.position = if (!facet && length(available_metrics) > 1) "bottom" else "none"
  )
  
  p
}

#' Plot distribution of OFI metrics
#'
#' @description
#' Creates histograms or density plots showing the distribution of OFI metrics.
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics  
#' @param metric Character, which metric to plot (default: "oir")
#' @param plot_type Character, "histogram" or "density" (default: "histogram")
#' @param bins Number of bins for histogram (default: 30)
#'
#' @return A ggplot2 object
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_vline
#'   theme_minimal labs scale_y_continuous
#' @importFrom stats median
#'
#' @examples
#' trades <- simulate_orders(n = 1000, seed = 123)
#' ofi <- compute_ofi(trades, window = "1 min")
#' 
#' # Distribution of imbalance ratios
#' plot_ofi_dist(ofi, metric = "oir")
#' 
#' # Density plot of OFI values
#' plot_ofi_dist(ofi, metric = "ofi", plot_type = "density")
plot_ofi_dist <- function(ofi_tbl, 
                         metric = "oir",
                         plot_type = c("histogram", "density"),
                         bins = 30) {
  
  plot_type <- match.arg(plot_type)
  
  # Validate metric exists
  if (!metric %in% names(ofi_tbl)) {
    stop("Metric '", metric, "' not found in ofi_tbl")
  }
  
  # Get metric values
  values <- ofi_tbl[[metric]]
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    stop("No non-NA values for metric '", metric, "'")
  }
  
  # Create base plot
  p <- ggplot(data.frame(value = values), aes(x = .data$value))
  
  # Add appropriate geom
  if (plot_type == "histogram") {
    p <- p + geom_histogram(bins = bins, fill = "#2E86AB", alpha = 0.7,
                           color = "white", linewidth = 0.5)
  } else {
    p <- p + geom_density(fill = "#2E86AB", alpha = 0.3, color = "#2E86AB",
                         linewidth = 1)
  }
  
  # Add median line
  med_val <- median(values)
  p <- p + geom_vline(xintercept = med_val, linetype = "dashed",
                     color = "#A23B72", linewidth = 0.8)
  
  # Add zero line if applicable
  if (min(values) < 0 && max(values) > 0) {
    p <- p + geom_vline(xintercept = 0, linetype = "dotted",
                       color = "gray40", alpha = 0.6)
  }
  
  # Enhance appearance
  p <- p +
    theme_minimal(base_size = 12) +
    labs(
      x = get_metric_label(metric),
      y = if (plot_type == "histogram") "Count" else "Density",
      title = paste("Distribution of", get_metric_label(metric)),
      subtitle = paste("Median:", round(med_val, 4))
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30")
    )
  
  p
}

#' Get human-readable metric labels
#' @noRd
get_metric_label <- function(metric) {
  labels <- c(
    ofi = "Order-Flow Imbalance",
    oir = "Order Imbalance Ratio",
    ofi_cum = "Cumulative OFI",
    vol_total = "Total Volume",
    B = "Buy Volume",
    S = "Sell Volume",
    ofi_dollar = "Dollar-Weighted OFI",
    n_trades = "Number of Trades"
  )
  
  labels[metric] %||% metric
}

#' NULL default operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
