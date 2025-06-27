#' Summarize Field Order Processing
#'
#' Creates comprehensive summary of the entire field order processing pipeline.
#'
#' @param formatted_data Original formatted data
#' @param duplicates_result Result from process_fieldorder_duplicates()
#' @param final_result Result from handle_fieldorder_missing_data()
#' @param input_file_name Name of original input file
#' @return List with processing summary statistics
#'
#' @examples
#' summary <- summarize_fieldorder_processing(formatted_data, duplicates_result, final_result, "input.csv")
summarize_fieldorder_processing <- function(formatted_data, duplicates_result, final_result, input_file_name) {
  
  # Calculate completeness for final complete data
  if (nrow(final_result$complete) > 0) {
    completeness_pct <- final_result$complete %>%
      dplyr::summarise(
        across(c(CSTRAT, ISTRAT, YEAR, INT), ~100, .names = "pct_{.col}")
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
      pipeline_version = "2.0_simplified"
    ),
    record_counts = list(
      input_records = nrow(formatted_data),
      unique_input_lspecs = dplyr::n_distinct(formatted_data$LSPEC),
      after_duplicate_processing = nrow(duplicates_result$clean),
      final_complete_records = nrow(final_result$complete),
      total_flagged_records = nrow(duplicates_result$flagged) + nrow(final_result$incomplete),
      data_retention_rate = round(100 * nrow(final_result$complete) / nrow(formatted_data), 1)
    ),
    data_quality = list(
      duplicate_conflicts = nrow(duplicates_result$flagged),
      missing_data_records = nrow(final_result$incomplete),
      complete_records = nrow(final_result$complete),
      final_completeness = list(
        cstrat = completeness_pct$pct_CSTRAT,
        istrat = completeness_pct$pct_ISTRAT,
        year = completeness_pct$pct_YEAR,
        int = completeness_pct$pct_INT
      )
    ),
    processing_steps = list(
      step1_formatting = "Standardized LSPEC IDs and column types",
      step2_duplicates = paste("Removed", nrow(duplicates_result$flagged), "conflicting duplicates,",
                               "merged", sum(duplicates_result$clean$n_records_merged > 1), "clean duplicates"),
      step3_missing_data = paste("Separated", nrow(final_result$complete), "complete records from",
                                 nrow(final_result$incomplete), "incomplete records")
    )
  )
  
  return(summary)
}