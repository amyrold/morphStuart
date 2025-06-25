#' Visualize Community Metrics Trends Over Time
#'
#' Creates time series plots showing how richness, evenness, and diversity
#' change through stratigraphic time. Supports both single plot with multiple
#' y-axes or separate subplot arrangements.
#'
#' @param metrics_data Output from calculate_community_metrics()
#' @param plot_type Character: "combined" for single plot with multiple y-axes, "faceted" for subplots
#' @param time_axis Character: "CSTRAT" for stratigraphic depth or "YEAR" for age
#' @param smooth_method Character: smoothing method for trend lines ("loess", "lm", "none")
#' @return ggplot object showing community trends
#'
#' @examples
#' trends_plot <- visualize_community_trends(community_metrics)
#' trends_faceted <- visualize_community_trends(community_metrics, plot_type = "faceted")
visualize_community_trends <- function(metrics_data, 
                                       plot_type = "combined",
                                       time_axis = "YEAR", 
                                       smooth_method = "loess") {
  if (!is.data.frame(metrics_data)) {
    stop("Input must be a data frame")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualization")
  }
  
  # Check if time axis is available
  if (!time_axis %in% names(metrics_data)) {
    available_axes <- intersect(c("CSTRAT", "YEAR"), names(metrics_data))
    if (length(available_axes) == 0) {
      stop("No time axis (CSTRAT or YEAR) found in data")
    }
    time_axis <- available_axes[1]
    warning(paste("Requested time axis not found, using", time_axis))
  }
  
  # Prepare data for plotting
  plot_data <- metrics_data %>%
    select(all_of(time_axis), richness, evenness, shannon_diversity) %>%
    filter(!is.na(.data[[time_axis]]))
  
  if (nrow(plot_data) == 0) {
    stop("No valid data for plotting")
  }
  
  # Create plots based on type
  if (plot_type == "combined") {
    plot <- create_combined_trends_plot(plot_data, time_axis, smooth_method)
  } else if (plot_type == "faceted") {
    plot <- create_faceted_trends_plot(plot_data, time_axis, smooth_method)
  } else {
    stop("plot_type must be 'combined' or 'faceted'")
  }
  
  return(plot)
}

#' Create Combined Trends Plot with Multiple Y-Axes
#'
#' @param plot_data Prepared data for plotting
#' @param time_axis Time variable name
#' @param smooth_method Smoothing method
#' @return ggplot object
create_combined_trends_plot <- function(plot_data, time_axis, smooth_method) {
  
  # Normalize metrics to 0-1 scale for comparable visualization
  plot_data_norm <- plot_data %>%
    mutate(
      richness_norm = scale_to_01(richness),
      evenness_norm = scale_to_01(evenness),
      shannon_norm = scale_to_01(shannon_diversity)
    ) %>%
    pivot_longer(
      cols = c(richness_norm, evenness_norm, shannon_norm),
      names_to = "metric",
      values_to = "normalized_value"
    ) %>%
    mutate(
      metric = case_when(
        metric == "richness_norm" ~ "Richness",
        metric == "evenness_norm" ~ "Evenness", 
        metric == "shannon_norm" ~ "Shannon Diversity"
      )
    )
  
  # Create base plot
  p <- ggplot(plot_data_norm, aes(x = .data[[time_axis]], y = normalized_value, color = metric)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_line(alpha = 0.6) +
    scale_color_manual(values = c("Richness" = "#2E8B57", "Evenness" = "#FF6347", "Shannon Diversity" = "#4169E1")) +
    labs(
      title = "Community Metrics Trends Over Time",
      x = ifelse(time_axis == "CSTRAT", "Stratigraphic Depth (cm)", "Age (years)"),
      y = "Normalized Value (0-1 scale)",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  # Add smoothing if requested
  if (smooth_method != "none") {
    p <- p + geom_smooth(method = smooth_method, se = TRUE, alpha = 0.2)
  }
  
  # Reverse x-axis if using CSTRAT (deeper = older)
  if (time_axis == "CSTRAT") {
    p <- p + scale_x_reverse()
  }
  
  return(p)
}

#' Create Faceted Trends Plot with Separate Subplots
#'
#' @param plot_data Prepared data for plotting
#' @param time_axis Time variable name  
#' @param smooth_method Smoothing method
#' @return ggplot object
create_faceted_trends_plot <- function(plot_data, time_axis, smooth_method) {
  
  # Prepare data for faceting
  plot_data_long <- plot_data %>%
    pivot_longer(
      cols = c(richness, evenness, shannon_diversity),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      metric = case_when(
        metric == "richness" ~ "Richness",
        metric == "evenness" ~ "Evenness",
        metric == "shannon_diversity" ~ "Shannon Diversity"
      ),
      metric = factor(metric, levels = c("Richness", "Evenness", "Shannon Diversity"))
    )
  
  # Create faceted plot
  p <- ggplot(plot_data_long, aes(x = .data[[time_axis]], y = value)) +
    geom_point(size = 2, alpha = 0.7, color = "steelblue") +
    geom_line(alpha = 0.6, color = "steelblue") +
    facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    labs(
      title = "Community Metrics Trends Over Time",
      x = ifelse(time_axis == "CSTRAT", "Stratigraphic Depth (cm)", "Age (years)"),
      y = "Metric Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add smoothing if requested
  if (smooth_method != "none") {
    p <- p + geom_smooth(method = smooth_method, se = TRUE, alpha = 0.2, color = "darkblue")
  }
  
  # Reverse x-axis if using CSTRAT (deeper = older)
  if (time_axis == "CSTRAT") {
    p <- p + scale_x_reverse()
  }
  
  return(p)
}

#' Scale Values to 0-1 Range
#'
#' @param x Numeric vector
#' @return Scaled vector between 0 and 1
scale_to_01 <- function(x) {
  if (all(is.na(x))) return(x)
  x_range <- range(x, na.rm = TRUE)
  if (x_range[1] == x_range[2]) return(rep(0.5, length(x)))
  (x - x_range[1]) / (x_range[2] - x_range[1])
}

#' Create Temporal Trends Correlation Plot
#'
#' Shows relationship between temporal distance and community turnover
#'
#' @param turnover_result Output from calculate_pairwise_turnover()
#' @return ggplot object or NULL if no temporal data
create_temporal_turnover_plot <- function(turnover_result) {
  temporal_trends <- extract_temporal_trends(turnover_result)
  
  if (is.null(temporal_trends)) {
    return(NULL)
  }
  
  # Calculate correlation
  correlation <- cor(temporal_trends$temporal_distance, temporal_trends$turnover, use = "complete.obs")
  
  p <- ggplot(temporal_trends, aes(x = temporal_distance, y = turnover)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(
      title = "Temporal Distance vs Community Turnover",
      subtitle = paste("Correlation:", round(correlation, 3)),
      x = "Temporal Distance (years)",
      y = paste("Community Turnover (", turnover_result$method, ")", sep = "")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  return(p)
}