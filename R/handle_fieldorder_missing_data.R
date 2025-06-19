#' Handle Field Order Missing Data
#'
#' Identifies records with missing key data and separates them into
#' complete records (ready for analysis) and incomplete records (flagged).
#'
#' @param duplicates_result Result from process_fieldorder_duplicates()
#' @return List with $complete (analysis-ready data) and $incomplete (flagged records)
#'
#' @examples
#' result <- handle_fieldorder_missing_data(duplicates_processed)
#' complete_data <- result$complete
#' incomplete_data <- result$incomplete
handle_fieldorder_missing_data <- function(duplicates_result) {
  if (!is.list(duplicates_result) || !"clean" %in% names(duplicates_result)) {
    stop("Input must be result from process_fieldorder_duplicates()")
  }
  
  clean_data <- duplicates_result$clean
  key_columns <- c("CSTRAT", "ISTRAT", "YEAR", "INT")
  
  # Add missing data indicators
  data_with_flags <- clean_data %>%
    mutate(
      missing_count = rowSums(is.na(select(., all_of(key_columns)))),
      missing_columns = pmap_chr(
        select(., all_of(key_columns)),
        function(...) {
          values <- list(...)
          missing_cols <- key_columns[sapply(values, is.na)]
          if (length(missing_cols) == 0) return("None")
          paste(missing_cols, collapse = ", ")
        }
      ),
      completeness_score = 1 - (missing_count / length(key_columns))
    )
  
  # Separate complete from incomplete records
  complete_records <- data_with_flags %>%
    filter(missing_count == 0) %>%
    mutate(data_quality = "Complete") %>%
    select(-missing_count, -missing_columns)  # Remove temporary columns
  
  incomplete_records <- data_with_flags %>%
    filter(missing_count > 0) %>%
    mutate(
      data_quality = case_when(
        missing_count >= 3 ~ "Poor - most data missing",
        missing_count == 2 ~ "Fair - half data missing",
        missing_count == 1 ~ "Good - minimal missing data"
      ),
      flag_reason = paste("Missing data in:", missing_columns),
      flag_category = "Missing data",
      action_needed = case_when(
        missing_count >= 3 ~ "Consider excluding from analysis",
        missing_count >= 2 ~ "Use with caution",
        TRUE ~ "Monitor for analysis limitations"
      )
    )
  
  # Save incomplete records if any exist
  if (nrow(incomplete_records) > 0) {
    if (!dir.exists("data/flagged")) {
      dir.create("data/flagged", recursive = TRUE)
    }
    write.csv(incomplete_records, "data/flagged/fieldorder_missing_data.csv", row.names = FALSE)
  }
  
  return(list(
    complete = as.data.frame(complete_records),
    incomplete = as.data.frame(incomplete_records),
    summary = list(
      input_records = nrow(clean_data),
      complete_records = nrow(complete_records),
      incomplete_records = nrow(incomplete_records),
      completion_rate = round(100 * nrow(complete_records) / nrow(clean_data), 1)
    )
  ))
}