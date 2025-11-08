#' Advanced Visualization Dashboard for Order Flow Analysis
#'
#' @description
#' Comprehensive visualization tools for market microstructure analysis,
#' designed for statisticians and quantitative researchers. Provides
#' publication-ready plots with proper confidence bands, diagnostic tests,
#' and multi-panel layouts.
#'
#' @name visualization_dashboard
NULL

#' Create comprehensive OFI diagnostic dashboard
#'
#' @description
#' Generates a multi-panel dashboard with statistical diagnostics including
#' time series plots, ACF/PACF, Q-Q plots, and distribution analysis.
#' Perfect for academic presentations and research papers.
#'
#' @param ofi_tbl A tibble from compute_ofi() containing OFI metrics
#' @param metric Character, which OFI metric to analyze (default: "oir")
#' @param max_lag Integer, maximum lag for ACF/PACF (default: 20)
#' @param conf_level Numeric, confidence level for bands (default: 0.95)
#' @param title Optional plot title
#'
#' @return A combined ggplot object (requires patchwork package)
#'
#' @export
#' @importFrom stats acf pacf qqnorm qqline
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_qq geom_qq_line
#'   theme_minimal labs annotate
#'
#' @examples
#' \dontrun{
#' library(patchwork)  # Required for dashboard layout
#'
#' trades <- simulate_orders(n = 5000, imb = 0.1, seed = 42)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Full diagnostic dashboard
#' plot_ofi_diagnostics(ofi, metric = "oir")
#'
#' # Focus on OFI with longer lag structure
#' plot_ofi_diagnostics(ofi, metric = "ofi", max_lag = 50)
#' }
plot_ofi_diagnostics <- function(ofi_tbl,
                                  metric = "oir",
                                  max_lag = 20,
                                  conf_level = 0.95,
                                  title = NULL) {

  # Check if patchwork is available
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for dashboard plots.\n",
         "Install with: install.packages('patchwork')")
  }

  # Validate input
  if (!metric %in% names(ofi_tbl)) {
    stop("Metric '", metric, "' not found in ofi_tbl")
  }

  values <- ofi_tbl[[metric]]
  values_clean <- values[!is.na(values)]

  if (length(values_clean) < 10) {
    stop("Insufficient non-NA values for diagnostics")
  }

  # 1. Time Series Plot with trend
  p1 <- plot_timeseries_with_trend(ofi_tbl, metric)

  # 2. ACF Plot
  p2 <- plot_acf_custom(values_clean, max_lag, conf_level,
                        title = paste("ACF:", get_metric_label(metric)))

  # 3. PACF Plot
  p3 <- plot_pacf_custom(values_clean, max_lag, conf_level,
                         title = paste("PACF:", get_metric_label(metric)))

  # 4. Q-Q Plot
  p4 <- plot_qq_custom(values_clean,
                       title = paste("Q-Q Plot:", get_metric_label(metric)))

  # 5. Distribution with stats
  p5 <- plot_distribution_with_stats(values_clean, metric)

  # 6. Summary statistics panel
  p6 <- create_stats_panel(values_clean, metric)

  # Combine into dashboard layout
  layout <- "
    AABBCC
    AABBCC
    DDEEFF
    DDEEFF
  "

  dashboard <- p1 + p2 + p3 + p4 + p5 + p6 +
    patchwork::plot_layout(design = layout) +
    patchwork::plot_annotation(
      title = title %||% paste("OFI Diagnostic Dashboard:", get_metric_label(metric)),
      theme = theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
    )

  return(dashboard)
}


#' Plot OFI time series decomposition
#'
#' @description
#' Decomposes OFI into trend, seasonal (intraday patterns), and irregular
#' components. Useful for identifying underlying patterns in order flow.
#'
#' @param ofi_tbl A tibble from compute_ofi()
#' @param metric Character, which metric to decompose (default: "oir")
#' @param freq Integer, seasonal frequency (default: NULL, auto-detect)
#'
#' @return A ggplot object with faceted decomposition
#'
#' @export
#' @importFrom stats stl ts frequency
#' @importFrom ggplot2 ggplot aes geom_line facet_grid theme_minimal
#'
#' @examples
#' trades <- simulate_orders(n = 10000, seed = 42)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Decompose OIR into components
#' plot_ofi_decomposition(ofi, metric = "oir")
plot_ofi_decomposition <- function(ofi_tbl, metric = "oir", freq = NULL) {

  if (!metric %in% names(ofi_tbl)) {
    stop("Metric '", metric, "' not found in ofi_tbl")
  }

  values <- ofi_tbl[[metric]]
  times <- ofi_tbl$window_start

  # Remove NAs
  na_idx <- !is.na(values)
  values <- values[na_idx]
  times <- times[na_idx]

  if (length(values) < 20) {
    stop("Insufficient data for decomposition (need at least 20 observations)")
  }

  # Auto-detect frequency if not specified
  if (is.null(freq)) {
    # Try to detect intraday patterns
    freq <- detect_frequency(times)
  }

  # Handle case where frequency is too low
  if (freq < 3) {
    warning("Frequency too low for STL decomposition, using simple moving average trend")
    return(plot_simple_decomposition(times, values, metric))
  }

  # Create time series and decompose
  ts_data <- ts(values, frequency = freq)

  tryCatch({
    decomp <- stl(ts_data, s.window = "periodic", robust = TRUE)

    # Extract components
    decomp_df <- data.frame(
      time = times,
      observed = values,
      trend = as.numeric(decomp$time.series[, "trend"]),
      seasonal = as.numeric(decomp$time.series[, "seasonal"]),
      remainder = as.numeric(decomp$time.series[, "remainder"])
    )

    # Reshape for plotting
    decomp_long <- decomp_df |>
      tidyr::pivot_longer(cols = -time, names_to = "component", values_to = "value") |>
      mutate(component = factor(.data$component,
                                levels = c("observed", "trend", "seasonal", "remainder"),
                                labels = c("Observed", "Trend", "Seasonal", "Remainder")))

    # Create plot
    p <- ggplot(decomp_long, aes(x = .data$time, y = .data$value)) +
      geom_line(color = "#2E86AB", linewidth = 0.6) +
      facet_grid(component ~ ., scales = "free_y") +
      theme_minimal(base_size = 11) +
      labs(
        x = "Time",
        y = NULL,
        title = paste("Time Series Decomposition:", get_metric_label(metric)),
        subtitle = paste("Frequency:", freq)
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        strip.text = element_text(face = "bold", size = 10)
      )

    return(p)

  }, error = function(e) {
    warning("STL decomposition failed, using simple moving average: ", e$message)
    return(plot_simple_decomposition(times, values, metric))
  })
}


#' Plot market quality dashboard
#'
#' @description
#' Comprehensive dashboard showing liquidity, spreads, efficiency, and
#' other market quality metrics over time. Requires trade-level data
#' with prices.
#'
#' @param trades Trade data with timestamp, side, size, price
#' @param window Character, time window for aggregation (default: "5 min")
#' @param title Optional plot title
#'
#' @return A combined ggplot object
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs
#' @importFrom dplyr group_by summarise mutate
#' @importFrom lubridate floor_date
#'
#' @examples
#' \dontrun{
#' library(patchwork)
#'
#' trades <- simulate_orders(n = 10000, seed = 42)
#' plot_market_quality(trades, window = "5 min")
#' }
plot_market_quality <- function(trades, window = "5 min", title = NULL) {

  # Check if patchwork is available
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required. Install with: install.packages('patchwork')")
  }

  # Validate required columns
  required <- c("timestamp", "side", "size", "price")
  missing <- setdiff(required, names(trades))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Aggregate by window
  market_metrics <- trades |>
    mutate(window = floor_date(.data$timestamp, window)) |>
    group_by(.data$window) |>
    summarise(
      # Volume metrics
      total_volume = sum(.data$size, na.rm = TRUE),
      n_trades = n(),
      avg_trade_size = mean(.data$size, na.rm = TRUE),

      # Price metrics
      price_range = max(.data$price) - min(.data$price),
      price_volatility = sd(.data$price, na.rm = TRUE),
      mid_price = mean(.data$price, na.rm = TRUE),

      # OFI metrics
      buy_volume = sum(.data$size[.data$side == "B"], na.rm = TRUE),
      sell_volume = sum(.data$size[.data$side == "S"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      ofi = .data$buy_volume - .data$sell_volume,
      oir = (.data$buy_volume - .data$sell_volume) /
        (.data$buy_volume + .data$sell_volume + 1e-10),
      spread_proxy = .data$price_range / .data$mid_price,  # Scaled range
      trade_intensity = .data$n_trades / as.numeric(difftime(
        lead(.data$window), .data$window, units = "secs"), na.rm = TRUE)
    )

  # Create individual plots
  p1 <- ggplot(market_metrics, aes(x = .data$window, y = .data$total_volume)) +
    geom_line(color = "#2E86AB", linewidth = 0.8) +
    theme_minimal() +
    labs(x = NULL, y = "Volume", title = "Trading Volume")

  p2 <- ggplot(market_metrics, aes(x = .data$window, y = .data$oir)) +
    geom_line(color = "#A23B72", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    theme_minimal() +
    labs(x = NULL, y = "OIR", title = "Order Imbalance Ratio")

  p3 <- ggplot(market_metrics, aes(x = .data$window, y = .data$spread_proxy)) +
    geom_line(color = "#F18F01", linewidth = 0.8) +
    theme_minimal() +
    labs(x = NULL, y = "Spread Proxy", title = "Effective Spread")

  p4 <- ggplot(market_metrics, aes(x = .data$window, y = .data$price_volatility)) +
    geom_line(color = "#C73E1D", linewidth = 0.8) +
    theme_minimal() +
    labs(x = "Time", y = "Volatility", title = "Price Volatility")

  # Combine
  dashboard <- (p1 + p2) / (p3 + p4) +
    patchwork::plot_annotation(
      title = title %||% "Market Quality Dashboard",
      theme = theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
    )

  return(dashboard)
}


#' Plot price impact curves with confidence bands
#'
#' @description
#' Visualizes the relationship between order size and price impact,
#' with confidence bands. Tests square-root law and linear impact models.
#'
#' @param trades Trade data with timestamp, side, size, price
#' @param impact_results Output from estimate_price_impact()
#' @param conf_level Confidence level for bands (default: 0.95)
#' @param show_sqrt Logical, show square-root law fit (default: TRUE)
#'
#' @return A ggplot object
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_line
#'   scale_x_continuous scale_y_continuous theme_minimal labs
#' @importFrom stats lm predict
#'
#' @examples
#' \dontrun{
#' trades <- simulate_orders(n = 5000, seed = 42)
#' impact <- estimate_price_impact(trades, window = "1 min")
#' plot_price_impact(trades, impact)
#' }
plot_price_impact <- function(trades, impact_results,
                               conf_level = 0.95, show_sqrt = TRUE) {

  # This is a placeholder - full implementation requires the price impact
  # estimation results to be structured appropriately

  message("plot_price_impact() requires price impact estimation results.")
  message("This function will be fully implemented with Priority 2: Price Impact Models")

  # Return a simple placeholder plot for now
  ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Price Impact Curves\n(Coming with Price Impact Models module)",
             size = 6, color = "gray50") +
    theme_void()
}


#' Plot regime detection based on volatility and order flow
#'
#' @description
#' Identifies and visualizes market regimes using clustering or
#' threshold-based methods. Useful for detecting high/low volatility
#' periods, imbalance regimes, and market stress.
#'
#' @param ofi_tbl A tibble from compute_ofi()
#' @param method Character, "quantile", "kmeans", or "hmm" (default: "quantile")
#' @param n_regimes Integer, number of regimes for clustering (default: 3)
#' @param vars Character vector of variables for regime detection
#'
#' @return A ggplot object with colored regimes
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_rect theme_minimal labs
#'   scale_fill_manual alpha
#' @importFrom stats kmeans quantile
#'
#' @examples
#' trades <- simulate_orders(n = 5000, imb = 0.1, seed = 42)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' # Simple quantile-based regimes
#' plot_regime_detection(ofi, method = "quantile")
#'
#' # K-means clustering
#' plot_regime_detection(ofi, method = "kmeans", n_regimes = 3)
plot_regime_detection <- function(ofi_tbl,
                                   method = c("quantile", "kmeans", "hmm"),
                                   n_regimes = 3,
                                   vars = c("oir", "vol_total")) {

  method <- match.arg(method)

  # Check variables exist
  missing_vars <- setdiff(vars, names(ofi_tbl))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }

  # Extract data
  regime_data <- ofi_tbl |>
    select(all_of(c("window_start", vars))) |>
    na.omit()

  if (nrow(regime_data) < n_regimes * 2) {
    stop("Insufficient data for regime detection")
  }

  # Detect regimes based on method
  if (method == "quantile") {
    # Simple quantile-based regimes on first variable
    var_values <- regime_data[[vars[1]]]
    breaks <- quantile(abs(var_values), probs = seq(0, 1, length.out = n_regimes + 1))
    regime_data$regime <- cut(abs(var_values), breaks = breaks,
                               labels = paste("Regime", 1:n_regimes),
                               include.lowest = TRUE)

  } else if (method == "kmeans") {
    # K-means clustering on specified variables
    cluster_vars <- regime_data[, vars, drop = FALSE]

    # Standardize
    cluster_vars_scaled <- scale(cluster_vars)

    # Cluster
    set.seed(42)  # For reproducibility
    km <- kmeans(cluster_vars_scaled, centers = n_regimes, nstart = 25)
    regime_data$regime <- factor(paste("Regime", km$cluster))

  } else if (method == "hmm") {
    stop("HMM regime detection not yet implemented. Use 'quantile' or 'kmeans'.")
  }

  # Create regime rectangles for background
  regime_spans <- regime_data |>
    mutate(regime_num = as.numeric(.data$regime)) |>
    mutate(regime_change = .data$regime_num != lag(.data$regime_num, default = -1)) |>
    mutate(regime_id = cumsum(.data$regime_change)) |>
    group_by(.data$regime_id) |>
    summarise(
      xmin = min(.data$window_start),
      xmax = max(.data$window_start),
      regime = first(.data$regime),
      .groups = "drop"
    )

  # Plot with regime backgrounds
  p <- ggplot() +
    # Regime backgrounds
    geom_rect(data = regime_spans,
              aes(xmin = .data$xmin, xmax = .data$xmax,
                  ymin = -Inf, ymax = Inf, fill = .data$regime),
              alpha = 0.2) +
    # OFI line on top
    geom_line(data = regime_data,
              aes(x = .data$window_start, y = .data[[vars[1]]]),
              color = "black", linewidth = 0.8) +
    scale_fill_manual(values = c("#6A994E", "#F18F01", "#C73E1D")) +
    theme_minimal(base_size = 12) +
    labs(
      x = "Time",
      y = get_metric_label(vars[1]),
      fill = "Regime",
      title = "Market Regime Detection",
      subtitle = paste("Method:", method, "|", "Regimes:", n_regimes)
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  return(p)
}


#' Plot comparative analysis across multiple assets or time periods
#'
#' @description
#' Creates small multiples comparing OFI patterns across different
#' assets, time periods, or market conditions. Ideal for cross-sectional
#' studies.
#'
#' @param ofi_list Named list of OFI tibbles to compare
#' @param metric Character, which metric to compare (default: "oir")
#' @param layout Character, "facet" or "overlay" (default: "facet")
#' @param normalize Logical, normalize values for comparison (default: FALSE)
#'
#' @return A ggplot object
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap theme_minimal labs
#' @importFrom dplyr bind_rows mutate
#'
#' @examples
#' # Compare different market conditions
#' trades_balanced <- simulate_orders(n = 1000, imb = 0, seed = 1)
#' trades_bullish <- simulate_orders(n = 1000, imb = 0.3, seed = 1)
#' trades_bearish <- simulate_orders(n = 1000, imb = -0.3, seed = 1)
#'
#' ofi_balanced <- compute_ofi(trades_balanced, window = "1 min")
#' ofi_bullish <- compute_ofi(trades_bullish, window = "1 min")
#' ofi_bearish <- compute_ofi(trades_bearish, window = "1 min")
#'
#' # Compare patterns
#' plot_comparative_analysis(
#'   list("Balanced" = ofi_balanced,
#'        "Bullish" = ofi_bullish,
#'        "Bearish" = ofi_bearish),
#'   metric = "oir"
#' )
plot_comparative_analysis <- function(ofi_list,
                                       metric = "oir",
                                       layout = c("facet", "overlay"),
                                       normalize = FALSE) {

  layout <- match.arg(layout)

  if (!is.list(ofi_list) || length(ofi_list) < 2) {
    stop("ofi_list must be a named list with at least 2 elements")
  }

  if (is.null(names(ofi_list))) {
    names(ofi_list) <- paste("Series", seq_along(ofi_list))
  }

  # Combine data
  combined_data <- bind_rows(
    lapply(names(ofi_list), function(name) {
      df <- ofi_list[[name]]
      if (!metric %in% names(df)) {
        warning("Metric '", metric, "' not found in ", name)
        return(NULL)
      }
      df |>
        select(all_of(c("window_start", metric))) |>
        mutate(series = name)
    })
  )

  if (nrow(combined_data) == 0) {
    stop("No valid data to plot")
  }

  # Normalize if requested
  if (normalize) {
    combined_data <- combined_data |>
      group_by(.data$series) |>
      mutate(!!sym(metric) := (.data[[metric]] - mean(.data[[metric]], na.rm = TRUE)) /
               sd(.data[[metric]], na.rm = TRUE)) |>
      ungroup()
  }

  # Create plot
  if (layout == "facet") {
    p <- ggplot(combined_data, aes(x = .data$window_start, y = .data[[metric]])) +
      geom_line(color = "#2E86AB", linewidth = 0.8) +
      facet_wrap(~ series, ncol = 1, scales = "free_y") +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      theme_minimal(base_size = 11) +
      labs(
        x = "Time",
        y = if (normalize) paste(get_metric_label(metric), "(Standardized)")
        else get_metric_label(metric),
        title = "Comparative OFI Analysis"
      )
  } else {
    # Overlay
    p <- ggplot(combined_data, aes(x = .data$window_start, y = .data[[metric]],
                                    color = .data$series)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      theme_minimal(base_size = 12) +
      labs(
        x = "Time",
        y = if (normalize) paste(get_metric_label(metric), "(Standardized)")
        else get_metric_label(metric),
        color = "Series",
        title = "Comparative OFI Analysis"
      ) +
      theme(legend.position = "bottom")
  }

  p <- p + theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10)
  )

  return(p)
}


#' Create publication-ready theme
#'
#' @description
#' A clean, professional ggplot2 theme suitable for academic publications
#' and presentations.
#'
#' @param base_size Base font size (default: 11)
#' @param base_family Font family (default: "")
#'
#' @return A ggplot2 theme object
#'
#' @export
#' @importFrom ggplot2 theme_minimal theme element_text element_line element_blank
#'
#' @examples
#' library(ggplot2)
#'
#' trades <- simulate_orders(n = 1000, seed = 42)
#' ofi <- compute_ofi(trades, window = "1 min")
#'
#' plot_ofi(ofi) + theme_publication()
theme_publication <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text
      plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0),
      plot.subtitle = element_text(size = rel(1), color = "gray30", hjust = 0),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.text = element_text(size = rel(0.9)),
      strip.text = element_text(face = "bold", size = rel(1)),
      legend.title = element_text(face = "bold", size = rel(1)),
      legend.text = element_text(size = rel(0.9)),

      # Grid
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),

      # Background
      plot.background = element_blank(),
      panel.background = element_blank(),

      # Margins
      plot.margin = margin(10, 10, 10, 10)
    )
}


# ============================================================================
# Helper Functions (Not Exported)
# ============================================================================

#' Plot time series with trend line
#' @noRd
plot_timeseries_with_trend <- function(ofi_tbl, metric) {
  data_df <- data.frame(
    time = as.numeric(ofi_tbl$window_start),
    value = ofi_tbl[[metric]]
  ) |>
    na.omit()

  p <- ggplot(data_df, aes(x = .data$time, y = .data$value)) +
    geom_line(color = "#2E86AB", linewidth = 0.8) +
    geom_smooth(method = "loess", se = TRUE, color = "#A23B72",
                fill = "#A23B72", alpha = 0.2, linewidth = 0.6) +
    theme_minimal() +
    labs(
      x = "Time",
      y = get_metric_label(metric),
      title = paste(get_metric_label(metric), "Over Time")
    ) +
    theme(plot.title = element_text(face = "bold", size = 11))

  return(p)
}

#' Custom ACF plot
#' @noRd
plot_acf_custom <- function(x, max_lag, conf_level, title = "ACF") {
  acf_result <- acf(x, lag.max = max_lag, plot = FALSE)

  ci <- qnorm((1 + conf_level) / 2) / sqrt(length(x))

  acf_df <- data.frame(
    lag = as.numeric(acf_result$lag),
    acf = as.numeric(acf_result$acf)
  )

  p <- ggplot(acf_df, aes(x = .data$lag, y = .data$acf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = .data$lag, yend = 0), color = "#2E86AB", linewidth = 1) +
    theme_minimal() +
    labs(x = "Lag", y = "ACF", title = title) +
    theme(plot.title = element_text(face = "bold", size = 11))

  return(p)
}

#' Custom PACF plot
#' @noRd
plot_pacf_custom <- function(x, max_lag, conf_level, title = "PACF") {
  pacf_result <- pacf(x, lag.max = max_lag, plot = FALSE)

  ci <- qnorm((1 + conf_level) / 2) / sqrt(length(x))

  pacf_df <- data.frame(
    lag = as.numeric(pacf_result$lag),
    pacf = as.numeric(pacf_result$acf)
  )

  p <- ggplot(pacf_df, aes(x = .data$lag, y = .data$pacf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = .data$lag, yend = 0), color = "#A23B72", linewidth = 1) +
    theme_minimal() +
    labs(x = "Lag", y = "PACF", title = title) +
    theme(plot.title = element_text(face = "bold", size = 11))

  return(p)
}

#' Custom Q-Q plot
#' @noRd
plot_qq_custom <- function(x, title = "Q-Q Plot") {
  qq_df <- data.frame(sample = sort(x))

  p <- ggplot(qq_df, aes(sample = .data$sample)) +
    geom_qq(color = "#2E86AB", alpha = 0.6) +
    geom_qq_line(color = "#C73E1D", linewidth = 0.8) +
    theme_minimal() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = title) +
    theme(plot.title = element_text(face = "bold", size = 11))

  return(p)
}

#' Distribution plot with summary statistics
#' @noRd
plot_distribution_with_stats <- function(x, metric) {
  df <- data.frame(value = x)

  p <- ggplot(df, aes(x = .data$value)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "#2E86AB", alpha = 0.6, color = "white") +
    geom_density(color = "#C73E1D", linewidth = 1) +
    geom_vline(xintercept = mean(x), linetype = "dashed",
               color = "#A23B72", linewidth = 0.8) +
    theme_minimal() +
    labs(
      x = get_metric_label(metric),
      y = "Density",
      title = "Distribution"
    ) +
    theme(plot.title = element_text(face = "bold", size = 11))

  return(p)
}

#' Create summary statistics panel
#' @noRd
create_stats_panel <- function(x, metric) {
  stats_text <- paste0(
    "Summary Statistics\n\n",
    sprintf("Mean: %.4f\n", mean(x)),
    sprintf("Median: %.4f\n", median(x)),
    sprintf("SD: %.4f\n", sd(x)),
    sprintf("Skewness: %.4f\n", calculate_skewness(x)),
    sprintf("Kurtosis: %.4f\n", calculate_kurtosis(x)),
    sprintf("Min: %.4f\n", min(x)),
    sprintf("Max: %.4f\n", max(x)),
    sprintf("N: %d", length(x))
  )

  p <- ggplot() +
    annotate("text", x = 0.1, y = 0.5, label = stats_text,
             hjust = 0, vjust = 0.5, size = 3.5, family = "mono") +
    theme_void() +
    labs(title = "Statistics") +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))

  return(p)
}

#' Calculate skewness
#' @noRd
calculate_skewness <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  sum((x - m)^3) / (n * s^3)
}

#' Calculate excess kurtosis
#' @noRd
calculate_kurtosis <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  sum((x - m)^4) / (n * s^4) - 3
}

#' Detect frequency for time series
#' @noRd
detect_frequency <- function(times) {
  # Calculate median time difference
  time_diffs <- as.numeric(diff(times), units = "secs")
  median_diff <- median(time_diffs, na.rm = TRUE)

  # Estimate observations per hour
  if (median_diff > 0) {
    obs_per_hour <- 3600 / median_diff
    # Round to reasonable frequency
    freq <- round(obs_per_hour)
    freq <- max(3, min(freq, 288))  # Between 3 and 288 (5-min bars in a day)
  } else {
    freq <- 12  # Default to hourly if can't detect
  }

  return(freq)
}

#' Simple decomposition using moving average
#' @noRd
plot_simple_decomposition <- function(times, values, metric) {
  # Calculate trend using moving average
  window_size <- min(7, max(3, length(values) %/% 10))
  trend <- rollmean(values, k = window_size, fill = NA, align = "center")

  # Remainder
  remainder <- values - trend

  decomp_df <- data.frame(
    time = times,
    observed = values,
    trend = trend,
    remainder = remainder
  )

  decomp_long <- decomp_df |>
    tidyr::pivot_longer(cols = -time, names_to = "component", values_to = "value") |>
    mutate(component = factor(.data$component,
                              levels = c("observed", "trend", "remainder"),
                              labels = c("Observed", "Trend", "Remainder")))

  p <- ggplot(decomp_long, aes(x = .data$time, y = .data$value)) +
    geom_line(color = "#2E86AB", linewidth = 0.6) +
    facet_grid(component ~ ., scales = "free_y") +
    theme_minimal(base_size = 11) +
    labs(
      x = "Time",
      y = NULL,
      title = paste("Time Series Decomposition:", get_metric_label(metric)),
      subtitle = "Simple Moving Average Method"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 10)
    )

  return(p)
}
