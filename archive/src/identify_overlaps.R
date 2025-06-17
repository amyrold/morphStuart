#' Identify Overlapping Measurements Between Part and Counterpart
#'
#' Identifies fish specimens that have the same measurement recorded in both
#' the "part" (higher quality) and "counterpart" (lower quality) fossil sides.
#' Calculates relative differences and flags specimens exceeding threshold.
#'
#' @param data Data frame with fish_id, part_type (P/C), and measurements
#' @param threshold Numeric threshold for relative differences (default 0.05 = 5%)
#' @return List containing:
#'   - overlap_fish: Fish with measurements exceeding threshold
#'   - non_overlap_fish: Fish without concerning overlaps  
#'   - overlap_metrics: Detailed metrics for all overlapping measurements
#'
#' @examples
#' results <- identify_overlaps(morph_data, threshold = 0.05)
#' flagged_fish <- results$overlap_fish
identify_overlaps <- function(data, threshold = 0.05) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!all(c("fish_id", "part_type") %in% names(data))) {
    stop("Data must contain 'fish_id' and 'part_type' columns")
  }
  if (threshold < 0 || threshold > 1) {
    stop("Threshold must be between 0 and 1")
  }
  
  # Get variable mappings for appropriate handling by type
  var_map <- variable_mapping()
  
  # Separate part and counterpart data to avoid many-to-many joins
  part_data <- data %>% 
    filter(part_type == "P") %>%
    select(fish_id, part_type, row_id = n, Scale_10mm, all_of(var_map$all))
  
  cpart_data <- data %>% 
    filter(part_type == "C") %>%
    select(fish_id, part_type, row_id = n, Scale_10mm, all_of(var_map$all))
  
  # Aggregate values within each fish/part combination to handle duplicates
  part_agg <- part_data %>%
    pivot_longer(
      cols = all_of(var_map$all),
      names_to = "measure",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    group_by(fish_id, measure) %>%
    summarize(
      # Use max for binary variables, mean for others
      part_value = if(first(measure) %in% var_map$binary) max(value) else mean(value),
      part_scale = mean(Scale_10mm, na.rm = TRUE),
      has_multiple_values = n() > 1,
      .groups = "drop"
    )
  
  cpart_agg <- cpart_data %>%
    pivot_longer(
      cols = all_of(var_map$all),
      names_to = "measure", 
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    group_by(fish_id, measure) %>%
    summarize(
      cpart_value = if(first(measure) %in% var_map$binary) max(value) else mean(value),
      cpart_scale = mean(Scale_10mm, na.rm = TRUE),
      has_multiple_values = n() > 1,
      .groups = "drop"
    )
  
  # Join to find overlaps (now guaranteed one-to-one)
  overlaps <- part_agg %>%
    inner_join(cpart_agg, by = c("fish_id", "measure"))
  
  # Calculate differences with type-appropriate methods
  overlaps <- overlaps %>%
    mutate(
      var_type = case_when(
        measure %in% var_map$continuous ~ "continuous",
        measure %in% var_map$count ~ "count",
        measure %in% var_map$binary ~ "binary",
        TRUE ~ "other"
      ),
      # Use average scale for normalization
      scale = coalesce((part_scale + cpart_scale) / 2, part_scale, cpart_scale),
      absolute_diff = abs(part_value - cpart_value),
      
      # Calculate relative differences appropriate to variable type
      relative_diff = case_when(
        # For continuous: relative to 10mm scale
        var_type == "continuous" & scale > 0 ~ absolute_diff / scale,
        # For count: proportion of maximum value
        var_type == "count" ~ absolute_diff / max(1, pmax(part_value, cpart_value)),
        # For binary: simply different or not
        TRUE ~ as.numeric(absolute_diff > 0)
      ),
      
      # Flag based on variable type and threshold
      exceeds_threshold = case_when(
        var_type == "continuous" ~ relative_diff > threshold,
        var_type == "count" ~ absolute_diff > 0,  # Any count difference
        var_type == "binary" ~ FALSE,  # Don't flag binary (take max)
        TRUE ~ FALSE
      ),
      
      # Flag if either part had multiple measurements
      multiple_measurements = has_multiple_values.x | has_multiple_values.y
    )
  
  # Identify fish to flag based on exceeding thresholds
  fish_to_flag <- overlaps %>%
    filter(exceeds_threshold) %>%
    select(fish_id) %>%
    distinct() %>%
    pull(fish_id)
  
  # Split original data by flagged status
  overlap_fish <- data %>%
    filter(fish_id %in% fish_to_flag)
  
  non_overlap_fish <- data %>%
    filter(!fish_id %in% fish_to_flag)
  
  # Return results maintaining original data structure
  return(list(
    overlap_fish = as.data.frame(overlap_fish),
    non_overlap_fish = as.data.frame(non_overlap_fish), 
    overlap_metrics = as.data.frame(overlaps)
  ))
}