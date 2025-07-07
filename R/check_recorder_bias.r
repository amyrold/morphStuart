#' Extract Recorder Mapping from Paleo Data
#'
#' Creates a mapping between LSPEC identifiers and the person who recorded
#' the microfossil count data, for use in recorder bias analysis.
#'
#' @param paleo_with_ids Paleo data with extracted IDs and Recorder column
#' @return Data frame with LSPEC to Recorder mapping
#'
#' @examples
#' recorder_map <- extract_recorder_mapping(paleo_with_ids)
extract_recorder_mapping <- function(paleo_with_ids) {
  if (!is.data.frame(paleo_with_ids)) {
    stop("Input must be a data frame")
  }
  
  required_cols <- c("Sample_ID", "LSPEC", "Recorder")
  missing_cols <- setdiff(required_cols, names(paleo_with_ids))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Extract unique LSPEC to Recorder mapping
  recorder_mapping <- paleo_with_ids %>%
    dplyr::select(Sample_ID, LSPEC, Recorder) %>%
    dplyr::filter(!is.na(LSPEC), !is.na(Recorder)) %>%
    dplyr::distinct()
  
  # Check for LSPECs with multiple recorders
  multi_recorder_check <- recorder_mapping %>%
    dplyr::group_by(LSPEC) %>%
    dplyr::summarise(n_recorders = dplyr::n_distinct(Recorder), .groups = "drop") %>%
    dplyr::filter(n_recorders > 1)
  
  if (nrow(multi_recorder_check) > 0) {
    warning(paste("Found", nrow(multi_recorder_check), "LSPECs with multiple recorders"))
  }
  
  # Take first recorder for each LSPEC if there are conflicts
  final_mapping <- recorder_mapping %>%
    dplyr::group_by(LSPEC) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(LSPEC, Recorder)
  
  return(as.data.frame(final_mapping))
}

#' Analyze Recorder Bias in Community Metrics
#'
#' Performs ANOVA analysis to test for systematic differences in community
#' metrics between different data recorders. Creates boxplots showing the
#' distribution of each metric by recorder, with CSTRAT depth color coding.
#'
#' @param community_metrics Output from calculate_community_metrics() (includes LSPEC and CSTRAT)
#' @param recorder_mapping Output from extract_recorder_mapping()
#' @return List containing ANOVA results, boxplots, and merged data
#'
#' @examples
#' bias_analysis <- analyze_recorder_bias(community_metrics, recorder_mapping)
analyze_recorder_bias <- function(community_metrics, recorder_mapping) {
  if (!is.data.frame(community_metrics) || !is.data.frame(recorder_mapping)) {
    stop("community_metrics and recorder_mapping must be data frames")
  }
  
  # Verify that community_metrics includes required columns
  if (!"LSPEC" %in% names(community_metrics)) {
    stop("community_metrics must contain 'LSPEC' column")
  }
  if (!"CSTRAT" %in% names(community_metrics)) {
    stop("community_metrics must contain 'CSTRAT' column")
  }
  
  # Direct join since community_metrics now preserves LSPEC
  metrics_with_recorder <- community_metrics %>%
    dplyr::left_join(recorder_mapping, by = "LSPEC") %>%
    dplyr::filter(!is.na(Recorder))
  
  if (nrow(metrics_with_recorder) == 0) {
    stop("No matching records found between community metrics and recorder mapping")
  }
  
  # Perform ANOVAs for each community metric
  anova_results <- list()
  metrics_to_test <- c("richness", "evenness", "shannon_diversity")
  
  for(metric in metrics_to_test) {
    if(metric %in% names(metrics_with_recorder)) {
      # Only perform ANOVA if there are at least 2 recorders with data
      recorder_counts <- table(metrics_with_recorder$Recorder)
      if (length(recorder_counts) >= 2 && all(recorder_counts >= 2)) {
        anova_formula <- as.formula(paste(metric, "~ Recorder"))
        anova_result <- aov(anova_formula, data = metrics_with_recorder)
        anova_summary <- summary(anova_result)
        
        # Extract key statistics
        anova_results[[metric]] <- list(
          summary = anova_summary,
          p_value = anova_summary[[1]][["Pr(>F)"]][1],
          f_statistic = anova_summary[[1]][["F value"]][1]
        )
      } else {
        anova_results[[metric]] <- list(
          summary = "Insufficient data for ANOVA (need ≥2 recorders with ≥2 samples each)",
          p_value = NA,
          f_statistic = NA
        )
      }
    }
  }
  
  # Prepare data for plotting with CSTRAT color coding
  plot_data <- metrics_with_recorder %>%
    dplyr::mutate(
      CSTRAT_group = factor(
        ifelse(CSTRAT < 500, "< 500 cm", "≥ 500 cm"),
        levels = c("< 500 cm", "≥ 500 cm")
      )
    )
  
  # Create boxplots for each metric
  plots <- list()
  
  for(metric in metrics_to_test) {
    if(metric %in% names(plot_data)) {
      # Format metric name for display
      metric_display <- switch(metric,
        "richness" = "Species Richness",
        "evenness" = "Species Evenness", 
        "shannon_diversity" = "Shannon Diversity",
        stringr::str_to_title(gsub("_", " ", metric))
      )
      
      # Add p-value to title if available
      title_text <- paste("Recorder Bias Analysis:", metric_display)
      if (!is.na(anova_results[[metric]]$p_value)) {
        p_val <- anova_results[[metric]]$p_value
        p_text <- if(p_val < 0.001) "p < 0.001" else paste("p =", round(p_val, 3))
        title_text <- paste(title_text, paste0("(", p_text, ")"))
      }
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Recorder, y = .data[[metric]])) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = Recorder), alpha = 0.7, outlier.shape = NA) +
        ggplot2::geom_point(ggplot2::aes(color = CSTRAT_group), 
                           position = ggplot2::position_jitter(width = 0.2), 
                           size = 2, alpha = 0.8) +
        ggplot2::scale_color_manual(
          values = c("< 500 cm" = "#E31A1C", "≥ 500 cm" = "#1F78B4"),
          name = "Stratigraphic Depth"
        ) +
        ggplot2::scale_fill_brewer(type = "qual", palette = "Set2", guide = "none") +
        ggplot2::labs(
          title = title_text,
          x = "Recorder",
          y = metric_display
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 12),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(size = 11),
          legend.text = ggplot2::element_text(size = 10)
        )
      
      plots[[metric]] <- p
    }
  }
  
  # Create summary table of results
  summary_table <- data.frame(
    Metric = names(anova_results),
    F_Statistic = sapply(anova_results, function(x) {
      if(is.numeric(x$f_statistic)) round(x$f_statistic, 3) else NA
    }),
    P_Value = sapply(anova_results, function(x) {
      if(is.numeric(x$p_value)) round(x$p_value, 4) else NA
    }),
    Significant = sapply(anova_results, function(x) {
      if(is.numeric(x$p_value)) x$p_value < 0.05 else NA
    }),
    stringsAsFactors = FALSE
  )
  
  # Report simplified joining process
  cat("Recorder Bias Analysis:\n")
  cat("Direct join via LSPEC - no intermediate mapping needed\n")
  cat("Community metrics with recorder data:", nrow(metrics_with_recorder), "samples\n")
  cat("Recorders in analysis:", length(unique(metrics_with_recorder$Recorder)), "\n")
  if (!all(is.na(plot_data$CSTRAT))) {
    cat("CSTRAT depth data available for color coding\n")
  } else {
    cat("No CSTRAT depth data available\n")
  }
  cat("\n")
  
  return(list(
    anova_results = anova_results,
    plots = plots,
    summary_table = summary_table,
    data = plot_data,
    sample_sizes = table(plot_data$Recorder)
  ))
}