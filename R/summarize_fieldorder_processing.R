#' Summarize Field Order Processing
#'
#' Creates comprehensive summary of the entire field order processing pipeline
#' from simple dataframes at each processing stage.
#'
#' @param formatted_data Original formatted data
#' @param duplicates_clean_data Clean data after duplicate processing
#' @param updated_data Data after incorporating additional updates
#' @param final_complete_data Complete data after missing data processing (requires CSTRAT)
#' @param input_file_name Name of original input file
#' @return List with processing summary statistics
#'
#' @examples
#' summary <- summarize_fieldorder_processing(formatted_data, duplicates_clean_data, updated_data, final_complete_data, "input.csv")
summarize_fieldorder_processing <- function(formatted_data, duplicates_clean_data, updated_data, final_complete_data, input_file_name) {
  
  # Calculate statistics from simple dataframes
  
  # Duplicate analysis statistics
  duplicate_lspecs <- formatted_data %>%
    dplyr::group_by(LSPEC) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::pull(LSPEC) %>%
    unique()
  
  # Calculate how many duplicates were merged vs flagged
  merged_duplicates <- duplicates_clean_data %>%
    dplyr::filter(n_records_merged > 1) %>%
    nrow()
  
  # Calculate flagged records from files if they exist
  flagged_duplicates <- 0
  if (file.exists("data/flagged/fieldorder_duplicate_conflicts.csv")) {
    flagged_duplicates <- nrow(read.csv("data/flagged/fieldorder_duplicate_conflicts.csv"))
  }
  
  # Calculate missing data statistics
  flagged_missing <- 0
  if (file.exists("data/flagged/fieldorder_missing_data.csv")) {
    flagged_missing <- nrow(read.csv("data/flagged/fieldorder_missing_data.csv"))
  }
  
  # Calculate completeness for final complete data
  if (nrow(final_complete_data) > 0) {
    completeness_pct <- final_complete_data %>%
      dplyr::summarise(
        pct_CSTRAT = round(100 * sum(!is.na(CSTRAT)) / n(), 1),
        pct_ISTRAT = round(100 * sum(!is.na(ISTRAT)) / n(), 1),
        pct_YEAR = round(100 * sum(!is.na(YEAR)) / n(), 1),
        pct_INT = round(100 * sum(!is.na(INT)) / n(), 1)
      )
  } else {
    completeness_pct <- data.frame(
      pct_CSTRAT = 0, pct_ISTRAT = 0, pct_YEAR = 0, pct_INT = 0
    )
  }
  
  summary <- list(
    processing_info = list(
      processing_date = Sys.time(),
      input_file = input_file_name,
      pipeline_version = "2.0_streamlined"
    ),
    record_counts = list(
      input_records = nrow(formatted_data),
      unique_input_lspecs = dplyr::n_distinct(formatted_data$LSPEC),
      after_duplicate_processing = nrow(duplicates_clean_data),
      final_complete_records = nrow(final_complete_data),
      total_flagged_records = flagged_duplicates + flagged_missing,
      data_retention_rate = round(100 * nrow(final_complete_data) / nrow(formatted_data), 1)
    ),
    data_quality = list(
      duplicate_conflicts = flagged_duplicates,
      missing_data_records = flagged_missing,
      complete_records = nrow(final_complete_data),
      completeness_percentages = list(
        cstrat = completeness_pct$pct_CSTRAT,
        istrat = completeness_pct$pct_ISTRAT,
        year = completeness_pct$pct_YEAR,
        int = completeness_pct$pct_INT
      )
    ),
    processing_steps = list(
      step1_formatting = "Standardized LSPEC IDs and column types",
      step2_duplicates = paste("Removed", flagged_duplicates, "conflicting duplicates,",
                               "merged", merged_duplicates, "clean duplicates"),
      step3_updates = "Processed additional data file for updates and new records",
      step4_missing_data = paste("Separated", nrow(final_complete_data), "complete records from",
                                 flagged_missing, "incomplete records")
    )
  )
  
  return(summary)
}