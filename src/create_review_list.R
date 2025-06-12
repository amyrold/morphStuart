#' Create Review List for Conflicting Measurements
#'
#' Creates prioritized lists of fish specimens requiring manual review due to
#' conflicting measurements between part and counterpart. Optionally filters
#' by threshold for focused review.
#'
#' @param overlap_results List output from identify_overlaps()
#' @param use_threshold Logical, whether to filter by threshold (default TRUE)
#' @return List containing summary table, detailed metrics, and filtering info
#'
#' @examples
#' review_list <- create_review_list(overlap_results, use_threshold = TRUE)
#' priority_fish <- review_list$summary
create_review_list <- function(overlap_results, use_threshold = TRUE) {
  # Validate input
  if (!is.list(overlap_results) || !"overlap_metrics" %in% names(overlap_results)) {
    stop("Input must be the list output from identify_overlaps()")
  }
  
  metrics <- overlap_results$overlap_metrics
  var_map <- variable_mapping()
  
  # Filter metrics based on threshold preference
  filtered_metrics <- if(use_threshold) {
    metrics %>% filter(exceeds_threshold == TRUE)
  } else {
    metrics
  }
  
  # Create summary by fish with meaningful grouping
  summary <- filtered_metrics %>%
    group_by(fish_id, var_type) %>%
    summarize(
      num_overlapping = n(),
      max_relative_diff = if_else(
        first(var_type) == "continuous", 
        max(relative_diff, na.rm = TRUE), 
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    # Summarize by fish across all variable types
    group_by(fish_id) %>%
    summarize(
      continuous_diffs = sum(var_type == "continuous"),
      count_diffs = sum(var_type == "count"), 
      binary_diffs = sum(var_type == "binary"),
      total_diffs = n(),
      max_relative_diff = max(max_relative_diff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Sort by priority (most differences first)
    arrange(desc(total_diffs), desc(continuous_diffs))
  
  # Create detailed breakdown for investigation
  details <- filtered_metrics %>%
    arrange(fish_id, var_type, desc(relative_diff))
  
  return(list(
    summary = as.data.frame(summary),
    details = as.data.frame(details),
    filtered_metrics = as.data.frame(filtered_metrics),
    use_threshold = use_threshold
  ))
}

#' Export Review Lists to CSV Files
#'
#' Exports review lists to CSV files for manual review by domain experts.
#'
#' @param review_results List output from create_review_list()
#' @param file_prefix Character prefix for output files (default "fish_review")
#' @param output_dir Optional output directory path
#' @return List of file paths created
#'
#' @examples
#' file_paths <- export_review_list(review_results, "priority_review", "output/")
export_review_list <- function(review_results, file_prefix = "fish_review", 
                               output_dir = NULL) {
  # Validate input
  if (!is.list(review_results) || !all(c("summary", "details") %in% names(review_results))) {
    stop("Input must be the list output from create_review_list()")
  }
  
  # Create output directory if specified
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    summary_path <- file.path(output_dir, paste0(file_prefix, "_summary.csv"))
    details_path <- file.path(output_dir, paste0(file_prefix, "_details.csv"))
  } else {
    summary_path <- paste0(file_prefix, "_summary.csv")
    details_path <- paste0(file_prefix, "_details.csv")
  }
  
  # Export files
  if (!require("readr", quietly = TRUE)) {
    # Fallback to base R if readr not available
    write.csv(review_results$summary, summary_path, row.names = FALSE)
    write.csv(review_results$details, details_path, row.names = FALSE)
  } else {
    readr::write_csv(review_results$summary, summary_path)
    readr::write_csv(review_results$details, details_path)
  }
  
  # Return file paths for confirmation
  return(list(
    summary = summary_path,
    details = details_path
  ))
}