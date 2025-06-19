#' Identify Overlapping Measurements Between Part and Counterpart
#'
#' Identifies fish specimens that have the same measurement recorded in both
#' the "part" (higher quality) and "counterpart" (lower quality) fossil sides.
#' Calculates relative differences and flags specimens exceeding threshold.
#' Works with quantitative variables only (continuous and count measurements).
#'
#' @param data Data frame with fish_id, part_type (P/C), and measurements
#' @param threshold Numeric threshold for relative differences (default 0.05 = 5%)
#' @return Data frame with flagged fish removed
#'
#' @examples
#' clean_data <- flag_counterpart_conflicts(morph_data, threshold = 0.05)
flag_counterpart_conflicts <- function(data, threshold = 0.05) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!all(c("fish_id", "part_type") %in% names(data))) {
    stop("Data must contain 'fish_id' and 'part_type' columns")
  }
  if (threshold < 0 || threshold > 1) {
    stop("Threshold must be between 0 and 1")
  }
  
  var_map <- variable_mapping()
  # Only use quantitative variables (continuous + count)
  analysis_vars <- c(var_map$continuous, var_map$count)
  
  part_data <- data %>% 
    filter(part_type == "P") %>%
    select(fish_id, part_type, row_id = n, Scale_10mm, all_of(analysis_vars))
  
  cpart_data <- data %>% 
    filter(part_type == "C") %>%
    select(fish_id, part_type, row_id = n, Scale_10mm, all_of(analysis_vars))
  
  # Aggregate part data
  part_agg <- part_data %>%
    pivot_longer(
      cols = all_of(analysis_vars),
      names_to = "measure",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    group_by(fish_id, measure) %>%
    summarize(
      # Use max for count variables, mean for continuous
      part_value = if(first(measure) %in% var_map$count) max(value) else mean(value),
      part_scale = mean(Scale_10mm, na.rm = TRUE),
      has_multiple_values = n() > 1,
      .groups = "drop"
    )
  
  # Aggregate counterpart data
  cpart_agg <- cpart_data %>%
    pivot_longer(
      cols = all_of(analysis_vars),
      names_to = "measure", 
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    group_by(fish_id, measure) %>%
    summarize(
      # Use max for count variables, mean for continuous
      cpart_value = if(first(measure) %in% var_map$count) max(value) else mean(value),
      cpart_scale = mean(Scale_10mm, na.rm = TRUE),
      has_multiple_values = n() > 1,
      .groups = "drop"
    )
  
  # Find overlapping measurements and calculate differences
  overlaps <- part_agg %>%
    inner_join(cpart_agg, by = c("fish_id", "measure")) %>%
    mutate(
      var_type = case_when(
        measure %in% var_map$continuous ~ "continuous",
        measure %in% var_map$count ~ "count",
        TRUE ~ "other"
      ),
      scale = coalesce((part_scale + cpart_scale) / 2, part_scale, cpart_scale),
      absolute_diff = abs(part_value - cpart_value),
      relative_diff = case_when(
        var_type == "continuous" & scale > 0 ~ absolute_diff / scale,
        var_type == "count" ~ absolute_diff / max(1, pmax(part_value, cpart_value)),
        TRUE ~ as.numeric(absolute_diff > 0)
      ),
      exceeds_threshold = case_when(
        var_type == "continuous" ~ relative_diff > threshold,
        var_type == "count" ~ absolute_diff > 0,  # Any difference for counts
        TRUE ~ FALSE
      ),
      multiple_measurements = has_multiple_values.x | has_multiple_values.y
    )
  
  # Identify fish to flag
  fish_to_flag <- overlaps %>%
    filter(exceeds_threshold) %>%
    select(fish_id) %>%
    distinct() %>%
    pull(fish_id)
  
  # Split data
  overlap_fish <- data %>%
    filter(fish_id %in% fish_to_flag)
  
  non_overlap_fish <- data %>%
    filter(!fish_id %in% fish_to_flag)
  
  # Save flagged fish for review
  if (!dir.exists("data/flagged")) {
    dir.create("data/flagged", recursive = TRUE)
  }
  write.csv(overlap_fish, file = "data/flagged/counterpart_conflicts.csv", row.names = FALSE)
  
  return(as.data.frame(non_overlap_fish))
}