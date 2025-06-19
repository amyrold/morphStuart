#' Report Visualization Functions
#'
#' Clean, modular functions to generate plots and visualizations
#' for the stickleback morphology and paleo-ecology analysis pipeline.

# ========================================================================= #
# MORPHOLOGY VISUALIZATIONS ----
# ========================================================================= #

#' Create Data Completeness Plot
#'
#' @param completeness_data Data frame with Variable and Completeness columns
#' @return ggplot object showing completeness by variable
completeness_plot <- function(completeness_data) {
  if(nrow(completeness_data) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "No completeness data available"), 
                       size = 5) +
             theme_void())
  }
  
  ggplot(completeness_data, aes(x = reorder(Variable, Completeness), y = Completeness)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(title = "Data Completeness by Variable", 
         x = "Variable", 
         y = "Completeness (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
}

# ========================================================================= #
# PALEO-ECOLOGY VISUALIZATIONS ----
# ========================================================================= #

#' Create Microfossil Type Distribution Plot
#'
#' @param type_summary Data frame with Microfossil_Type and Samples columns
#' @return ggplot object showing sample count by microfossil type
microfossil_type_plot <- function(type_summary) {
  if(nrow(type_summary) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "No microfossil data available"), 
                       size = 5) +
             theme_void())
  }
  
  ggplot(type_summary, aes(x = reorder(Microfossil_Type, Samples), y = Samples)) +
    geom_col(fill = "lightgreen", alpha = 0.8, color = "darkgreen") +
    coord_flip() +
    labs(title = "Sample Count by Microfossil Type", 
         x = "Microfossil Type", 
         y = "Number of Samples") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
}

# ========================================================================= #
# FIELD ORDER VISUALIZATIONS ----
# ========================================================================= #

#' Create Field Order Completeness Plot
#'
#' @param completeness_data Data frame with Column and Completeness columns
#' @return ggplot object showing completeness by field order column
fieldorder_completeness_plot <- function(completeness_data) {
  if(nrow(completeness_data) == 0 || completeness_data$Column[1] == "Data not available") {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "Field order completeness data not available"), 
                       size = 5) +
             theme_void())
  }
  
  ggplot(completeness_data, aes(x = reorder(Column, Completeness), y = Completeness)) +
    geom_col(fill = "steelblue", alpha = 0.7, color = "darkblue") +
    geom_text(aes(label = paste0(round(Completeness, 1), "%")), 
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = "Field Order Data Completeness by Column",
         x = "Column", 
         y = "Completeness (%)") +
    theme_minimal() +
    ylim(0, 105) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
}

#' Create Flag Category Breakdown Plot
#'
#' @param fieldorder_flagged Flagged field order data
#' @return ggplot object showing flagged records by category and priority
flag_breakdown_plot <- function(fieldorder_flagged) {
  if(is.null(fieldorder_flagged) || nrow(fieldorder_flagged) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "No records were flagged during processing"), 
                       size = 5) +
             theme_void())
  }
  
  flag_viz_data <- fieldorder_flagged %>%
    count(flag_category, review_priority) %>%
    arrange(flag_category, factor(review_priority, levels = c("Critical", "High", "Medium", "Low", "Informational")))
  
  ggplot(flag_viz_data, aes(x = flag_category, y = n, fill = review_priority)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("Critical" = "#d32f2f", "High" = "#f57c00", 
                                 "Medium" = "#fbc02d", "Low" = "#689f38", 
                                 "Informational" = "#1976d2")) +
    labs(title = "Flagged Records by Category and Priority",
         x = "Flag Category", 
         y = "Number of Records", 
         fill = "Priority") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
}

#' Create Stratigraphic Level Distribution Plot
#'
#' @param fieldorder_clean Clean field order data
#' @return ggplot object showing distribution of stratigraphic levels (CSTRAT)
stratigraphic_distribution_plot <- function(fieldorder_clean) {
  if(is.null(fieldorder_clean) || nrow(fieldorder_clean) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "Field order clean data not available"), 
                       size = 5) +
             theme_void())
  }
  
  if(sum(!is.na(fieldorder_clean$CSTRAT)) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "No CSTRAT data available for visualization"), 
                       size = 5) +
             theme_void())
  }
  
  ggplot(fieldorder_clean %>% filter(!is.na(CSTRAT)), aes(x = CSTRAT)) +
    geom_histogram(bins = 30, fill = "lightgreen", alpha = 0.7, color = "black") +
    labs(title = "Distribution of Stratigraphic Levels (CSTRAT)",
         x = "CSTRAT (cm)", 
         y = "Number of Records") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
}

#' Create Age Distribution Plot
#'
#' @param fieldorder_clean Clean field order data
#' @return ggplot object showing distribution of ages (YEAR)
age_distribution_plot <- function(fieldorder_clean) {
  if(is.null(fieldorder_clean) || nrow(fieldorder_clean) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "Field order clean data not available"), 
                       size = 5) +
             theme_void())
  }
  
  if(sum(!is.na(fieldorder_clean$YEAR)) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "No YEAR data available for visualization"), 
                       size = 5) +
             theme_void())
  }
  
  ggplot(fieldorder_clean %>% filter(!is.na(YEAR)), aes(x = YEAR)) +
    geom_histogram(bins = 30, fill = "lightcoral", alpha = 0.7, color = "black") +
    labs(title = "Distribution of Ages (YEAR)",
         x = "Age (years)", 
         y = "Number of Records") +
    theme_minimal() +
    scale_x_continuous(labels = scales::comma) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
}

#' Create Age-Depth Relationship Plot
#'
#' @param fieldorder_clean Clean field order data
#' @return ggplot object showing relationship between stratigraphic position and age
age_depth_relationship_plot <- function(fieldorder_clean) {
  if(is.null(fieldorder_clean) || nrow(fieldorder_clean) == 0) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "Field order clean data not available"), 
                       size = 5) +
             theme_void())
  }
  
  age_depth_data <- fieldorder_clean %>% 
    filter(!is.na(CSTRAT) & !is.na(YEAR))
  
  if(nrow(age_depth_data) <= 5) {
    return(ggplot() + 
             geom_text(aes(x = 1, y = 1, label = "Insufficient paired age-depth data for correlation analysis"), 
                       size = 5) +
             theme_void())
  }
  
  correlation <- cor(age_depth_data$CSTRAT, age_depth_data$YEAR, use = "complete.obs")
  
  ggplot(age_depth_data, aes(x = CSTRAT, y = YEAR)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
    labs(title = "Relationship Between Stratigraphic Position and Age",
         subtitle = paste("Correlation:", round(correlation, 3)),
         x = "CSTRAT (cm)", 
         y = "Age (years)") +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
}

# ========================================================================= #
# UTILITY FUNCTIONS ----
# ========================================================================= #

#' Save Plot to File
#'
#' @param plot ggplot object
#' @param filename Output filename 
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution in dots per inch
#' @return File path
save_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  if (!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  
  ggsave(filename, plot, width = width, height = height, dpi = dpi)
  return(filename)
}