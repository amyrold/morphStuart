#' Create Flagged Field Order Dataset for Review
#'
#' Combines problematic duplicate records and records with missing data into
#' a comprehensive flagged dataset for manual review. Adds review priority
#' and categorization to help with quality control workflow.
#'
#' @param formatted_data Original formatted field order data frame
#' @param duplicate_analysis Results from identify_fieldorder_duplicates()
#' @param missing_data_records Results from identify_missing_data_records()
#' @return Data frame with all flagged records and review metadata
#'
#' @examples
#' flagged_data <- flag_fieldorder(formatted_data, duplicate_analysis, missing_data_records)
flag_fieldorder <- function(formatted_data, duplicate_analysis, missing_data_records) {
  if (!is.data.frame(formatted_data)) {
    stop("formatted_data must be a data frame")
  }
  if (!is.data.frame(duplicate_analysis)) {
    stop("duplicate_analysis must be a data frame")
  }
  if (!is.data.frame(missing_data_records)) {
    stop("missing_data_records must be a data frame")
  }
  
  # Get problematic LSPECs
  problematic_lspecs <- duplicate_analysis %>%
    filter(has_real_conflicts) %>%
    pull(LSPEC)
  
  # Create problematic records dataset
  problematic_records <- formatted_data %>%
    filter(LSPEC %in% problematic_lspecs) %>%
    mutate(
      L_SPEC_original = as.character(L_SPEC_original)  # Ensure consistent type
    ) %>%
    left_join(
      duplicate_analysis %>% 
        select(LSPEC, conflict_severity, conflict_summary, n_records),
      by = "LSPEC"
    ) %>%
    mutate(
      flag_reason = conflict_summary,
      flag_category = "Conflicting duplicates",
      needs_manual_review = TRUE,
      can_proceed_with_caution = FALSE,
      review_priority = case_when(
        conflict_severity == "High" ~ "Critical",
        conflict_severity == "Medium" ~ "High", 
        conflict_severity == "Low" ~ "Medium",
        TRUE ~ "Low"
      ),
      action_needed = "Manual review and resolution required",
      data_usability = "Unusable until resolved"
    )
  
  # Create missing data records dataset  
  missing_data_flagged <- missing_data_records %>%
    # Get L_SPEC_original from formatted_data for these records
    left_join(
      formatted_data %>% select(LSPEC, L_SPEC_original) %>% distinct(),
      by = "LSPEC"
    ) %>%
    mutate(
      L_SPEC_original = as.character(L_SPEC_original),  # Ensure consistent type
      flag_reason = paste("Missing data in:", missing_columns),
      flag_category = "Missing data",
      needs_manual_review = missing_count >= 3,
      can_proceed_with_caution = missing_count < 3,
      review_priority = case_when(
        missing_count >= 3 ~ "Medium",
        missing_count == 2 ~ "Low",
        TRUE ~ "Informational"
      ),
      action_needed = case_when(
        missing_count >= 3 ~ "Consider excluding or seek additional data",
        missing_count >= 2 ~ "Use with caution in analyses",
        TRUE ~ "Monitor for analysis limitations"
      ),
      conflict_severity = "None",
      conflict_summary = "No conflicts",
      n_records = 1
    )
  
  # Combine all flagged records
  flagged_records <- bind_rows(
    problematic_records,
    missing_data_flagged
  ) %>%
    arrange(
      factor(review_priority, levels = c("Critical", "High", "Medium", "Low", "Informational")),
      flag_category, 
      LSPEC
    ) %>%
    # Add final review columns
    mutate(
      processing_date = Sys.Date(),
      total_flags = nrow(.),
      flag_id = row_number(),
      exclude_from_analysis = flag_category == "Conflicting duplicates"
    )
  
  # Ensure all required columns are present and properly ordered
  required_cols <- c("LSPEC", "L_SPEC_original", "CSTRAT", "ISTRAT", "YEAR", "INT",
                     "flag_category", "flag_reason", "review_priority", "action_needed",
                     "needs_manual_review", "can_proceed_with_caution", "data_usability",
                     "exclude_from_analysis")
  
  flagged_records <- flagged_records %>%
    select(all_of(intersect(required_cols, names(.))), everything())
  
  return(as.data.frame(flagged_records))
}

#' Generate Field Order Processing Summary
#'
#' Creates comprehensive summary statistics for the entire field order processing
#' pipeline including input/output counts, quality metrics, and file references.
#'
#' @param formatted_data Formatted field order data
#' @param clean_data Clean field order data  
#' @param flagged_data Flagged field order data
#' @param input_file_name Name of original input file
#' @return List with comprehensive processing summary
#'
#' @examples
#' summary <- generate_fieldorder_summary(formatted_data, clean_data, flagged_data, "PitLMorph_fieldorder.csv")
generate_fieldorder_summary <- function(formatted_data, clean_data, flagged_data, input_file_name) {

  # Calculate key statistics
  n_input <- nrow(formatted_data)
  n_clean <- nrow(clean_data)
  n_flagged <- nrow(flagged_data)
  n_unique_lspecs_input <- n_distinct(formatted_data$LSPEC)
  n_unique_lspecs_clean <- n_distinct(clean_data$LSPEC)

  # Flag category breakdown
  flag_breakdown <- flagged_data %>%
    count(flag_category, review_priority) %>%
    arrange(flag_category, factor(review_priority, levels = c("Critical", "High", "Medium", "Low", "Informational")))

  # Completeness in clean data
  completeness_clean <- clean_data %>%
    summarise(
      across(c(CSTRAT, ISTRAT, YEAR, INT), ~sum(!is.na(.)) / n() * 100, .names = "pct_{.col}")
    )

  summary <- list(
    processing_info = list(
      processing_date = Sys.time(),
      input_file = input_file_name,
      pipeline_version = "1.0"
    ),
    record_counts = list(
      total_input_records = n_input,
      total_clean_records = n_clean,
      total_flagged_records = n_flagged,
      unique_lspecs_input = n_unique_lspecs_input,
      unique_lspecs_clean = n_unique_lspecs_clean,
      records_lost_to_conflicts = n_input - n_clean - n_flagged,
      data_retention_rate = round(100 * n_clean / n_input, 1)
    ),
    data_quality = list(
      completeness_percentages = list(
        cstrat = round(completeness_clean$pct_CSTRAT, 1),
        istrat = round(completeness_clean$pct_ISTRAT, 1),
        year = round(completeness_clean$pct_YEAR, 1),
        int = round(completeness_clean$pct_INT, 1)
      ),
      missing_data_records = sum(clean_data$has_missing_data),
      complete_records = sum(!clean_data$has_missing_data)
    ),
    flag_summary = list(
      flag_breakdown = flag_breakdown,
      critical_issues = sum(flagged_data$review_priority == "Critical"),
      high_priority_issues = sum(flagged_data$review_priority == "High"),
      needs_manual_review = sum(flagged_data$needs_manual_review),
      can_proceed_with_caution = sum(flagged_data$can_proceed_with_caution)
    )
  )

  return(summary)
}