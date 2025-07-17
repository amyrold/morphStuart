#' Handle Field Order Missing Data
#'
#' Identifies records with missing CSTRAT data and separates them into
#' complete records (ready for analysis) and incomplete records (flagged).
#' CSTRAT is the key variable needed for stratigraphic analysis.
#'
#' @param duplicates_clean_data Clean data from process_fieldorder_duplicates()
#' @return Data frame with complete records ready for analysis
#'
#' @examples
#' complete_data <- handle_fieldorder_missing_data(duplicates_clean_data)
handle_fieldorder_missing_data <- function(duplicates_clean_data) {
  if (!is.data.frame(duplicates_clean_data)) {
    stop("Input must be a data frame")
  }
  
  # Only CSTRAT is required for analysis
  # Add missing data indicators
  data_with_flags <- duplicates_clean_data %>%
    dplyr::mutate(
      missing_cstrat = is.na(CSTRAT)
    )
  
  # Separate complete from incomplete records based on CSTRAT only
  complete_records <- data_with_flags %>%
    dplyr::filter(!missing_cstrat) %>%
    dplyr::mutate(data_quality = "Complete") %>%
    dplyr::select(-missing_cstrat)  # Remove temporary column
  
  incomplete_records <- data_with_flags %>%
    dplyr::filter(missing_cstrat) %>%
    dplyr::mutate(
      data_quality = "Incomplete - missing CSTRAT",
      flag_reason = "Missing CSTRAT (required for analysis)",
      flag_category = "Missing required data",
      action_needed = "Exclude from analysis or find CSTRAT data"
    )
  
  # Save incomplete records if any exist
  if (nrow(incomplete_records) > 0) {
    if (!dir.exists("data/flagged")) {
      dir.create("data/flagged", recursive = TRUE)
    }
    write.csv(incomplete_records, "data/flagged/fieldorder_missing_data.csv", row.names = FALSE)
  }
  
  # Report processing results
  cat("Field Order Missing Data Processing:\n")
  cat("Input records:", nrow(duplicates_clean_data), "\n")
  cat("Complete records (with CSTRAT):", nrow(complete_records), "\n")
  cat("Incomplete records (missing CSTRAT):", nrow(incomplete_records), "\n")
  cat("Completion rate:", round(100 * nrow(complete_records) / nrow(duplicates_clean_data), 1), "%\n\n")
  
  return(as.data.frame(complete_records))
}