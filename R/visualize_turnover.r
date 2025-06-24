#' Visualize Pairwise Turnover as Heatmap
#'
#' Creates a heatmap showing pairwise beta diversity (turnover) between all
#' time bins. Uses color intensity to represent dissimilarity values.
#'
#' @param turnover_result Output from calculate_pairwise_turnover()
#' @param color_palette Character: color scheme for heatmap ("viridis", "plasma", "Blues", "Reds")
#' @param show_values Logical: whether to display turnover values in cells
#' @param cluster_order Logical: whether to reorder rows/columns by similarity
#' @return ggplot object showing turnover heatmap
#'
#' @examples
#' heatmap <- visualize_turnover_heatmap(turnover_matrix)
#' heatmap_clustered <- visualize_turnover_heatmap(turnover_matrix, cluster_order = TRUE)
visualize_turnover_heatmap <- function(turnover_result, 
                                     color_palette = "viridis",
                                     show_values = FALSE,
                                     cluster_order = FALSE) {
  if (!is.list(turnover_result) || !"formatted_matrix" %in% names(turnover_result)) {
    stop("Input must be output from calculate_pairwise_turnover()")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualization")
  }
  
  turnover_matrix <- turnover_result$formatted_matrix
  
  # Apply clustering if requested
  if (cluster_order) {
    hclust_result <- hclust(turnover_result$distance_matrix)
    cluster_order_indices <- hclust_result$order
    turnover_matrix <- turnover_matrix[cluster_order_indices, cluster_order_indices]
  }
  
  # Convert matrix to long format for ggplot
  heatmap_data <- expand.grid(
    CSTRAT_1 = rownames(turnover_matrix),
    CSTRAT_2 = colnames(turnover_matrix),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      turnover = as.vector(turnover_matrix),
      CSTRAT_1 = factor(CSTRAT_1, levels = rownames(turnover_matrix)),
      CSTRAT_2 = factor(CSTRAT_2, levels = colnames(turnover_matrix))
    )
  
  # Create base heatmap
  p <- ggplot(heatmap_data, aes(x = CSTRAT_1, y = CSTRAT_2, fill = turnover)) +
    geom_tile() +
    labs(
      title = "Pairwise Community Turnover Between Time Bins",
      subtitle = paste("Method:", turnover_result$method, 
                       ifelse(turnover_result$binary, "(binary)", "(abundance)")),
      x = "CSTRAT (cm)",
      y = "CSTRAT (cm)",
      fill = "Turnover"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(angle = 0),
      panel.grid = element_blank(),
      aspect.ratio = 1
    )
  
  # Apply color palette
  p <- apply_color_palette(p, color_palette)
  
  # Add text values if requested
  if (show_values) {
    p <- p + geom_text(aes(label = round(turnover, 2)), 
                       color = "white", size = 3)
  }
  
  # Add clustering information if used
  if (cluster_order) {
    p <- p + labs(caption = "Rows/columns ordered by hierarchical clustering")
  }
  
  return(p)
}

#' Apply Color Palette to Heatmap
#'
#' @param plot ggplot object
#' @param palette Character string specifying color palette
#' @return ggplot object with color scale applied
apply_color_palette <- function(plot, palette) {
  
  if (palette %in% c("viridis", "plasma", "inferno", "magma", "cividis")) {
    if (!requireNamespace("viridisLite", quietly = TRUE)) {
      warning("viridisLite package not available, using default colors")
      return(plot + scale_fill_gradient(low = "white", high = "darkblue"))
    }
    return(plot + scale_fill_viridis_c(option = palette))
  } else if (palette == "Blues") {
    return(plot + scale_fill_gradient(low = "white", high = "darkblue"))
  } else if (palette == "Reds") {
    return(plot + scale_fill_gradient(low = "white", high = "darkred"))
  } else if (palette == "Greens") {
    return(plot + scale_fill_gradient(low = "white", high = "darkgreen"))
  } else {
    warning(paste("Unknown palette:", palette, "- using default"))
    return(plot + scale_fill_gradient(low = "white", high = "darkblue"))
  }
}

#' Create Turnover Distance Histogram
#'
#' Shows distribution of turnover values between time bins
#'
#' @param turnover_result Output from calculate_pairwise_turnover()
#' @return ggplot object showing turnover distribution
create_turnover_histogram <- function(turnover_result) {
  if (!is.list(turnover_result) || !"distance_matrix" %in% names(turnover_result)) {
    stop("Input must be output from calculate_pairwise_turnover()")
  }
  
  turnover_values <- as.vector(turnover_result$distance_matrix)
  
  p <- ggplot(data.frame(turnover = turnover_values), aes(x = turnover)) +
    geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean(turnover)), color = "red", linetype = "dashed", size = 1) +
    labs(
      title = "Distribution of Pairwise Turnover Values",
      subtitle = paste("Method:", turnover_result$method, "| Mean:", round(mean(turnover_values), 3)),
      x = "Turnover (Beta Diversity)",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  return(p)
}

#' Create Dendrogram from Turnover Matrix
#'
#' Shows hierarchical clustering of time bins based on community composition
#'
#' @param turnover_result Output from calculate_pairwise_turnover()
#' @param method Clustering method (default "complete")
#' @return ggplot object showing dendrogram
create_turnover_dendrogram <- function(turnover_result, method = "complete") {
  if (!requireNamespace("ggdendro", quietly = TRUE)) {
    warning("ggdendro package required for dendrogram visualization")
    return(NULL)
  }
  
  # Perform hierarchical clustering
  hclust_result <- hclust(turnover_result$distance_matrix, method = method)
  
  # Convert to dendrogram data
  dendro_data <- ggdendro::dendro_data(hclust_result)
  
  p <- ggplot() +
    geom_segment(data = dendro_data$segments, 
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = dendro_data$labels, 
              aes(x = x, y = y, label = label), 
              hjust = 1, angle = 90, size = 3) +
    labs(
      title = "Hierarchical Clustering of Time Bins",
      subtitle = paste("Based on", turnover_result$method, "dissimilarity"),
      y = "Distance",
      x = "CSTRAT"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank()
    )
  
  return(p)
}