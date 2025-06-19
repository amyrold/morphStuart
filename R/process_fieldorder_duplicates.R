#' Process Field Order Duplicates
#'
#' Identifies duplicate LSPECs, removes those with conflicting data,
#' and merges non-conflicting duplicates into single records.
#'
#' @param formatted_data Formatted field order data frame
#' @return List with $clean (processed data) and $flagged (conflicting records)
#'
#' @examples
#' result <- process_fieldorder_duplicates(formatted_data)
#' clean_data <- result$clean
#' flagged_data <- result$flagged
process_fieldorder_duplicates <- function(formatted_data) {
  if (!is.data.frame(formatted_data)) {
    stop("Input must be a data frame")
  }
  if (!"LSPEC" %in% names(formatted_data)) {
    stop("Data must contain 'LSPEC' column")
  }
  
  # Analyze duplicates and identify conflicts
  duplicate_analysis <- formatted_data %>%
    group_by(LSPEC) %>%
    filter(n() > 1) %>%
    summarise(
      n_records = n(),
      original_specs = paste(L_SPEC_original, collapse = ", "),
      # Check for actual conflicting values (not just missing)
      conflicting_CSTRAT = length(unique(CSTRAT[!is.na(CSTRAT)])) > 1,
      conflicting_ISTRAT = length(unique(ISTRAT[!is.na(ISTRAT)])) > 1,
      conflicting_YEAR = length(unique(YEAR[!is.na(YEAR)])) > 1,
      conflicting_INT = length(unique(INT[!is.na(INT)])) > 1,
      .groups = "drop"
    ) %>%
    mutate(
      has_conflicts = conflicting_CSTRAT | conflicting_ISTRAT | 
        conflicting_YEAR | conflicting_INT,
      conflict_columns = paste(c(
        if_else(conflicting_CSTRAT, "CSTRAT", ""),
        if_else(conflicting_ISTRAT, "ISTRAT", ""),
        if_else(conflicting_YEAR, "YEAR", ""),
        if_else(conflicting_INT, "INT", "")
      )[c(
        if_else(conflicting_CSTRAT, "CSTRAT", ""),
        if_else(conflicting_ISTRAT, "ISTRAT", ""),
        if_else(conflicting_YEAR, "YEAR", ""),
        if_else(conflicting_INT, "INT", "")
      ) != ""], collapse = ", ")
    )
  
  # Get LSPECs with conflicts
  conflicting_lspecs <- duplicate_analysis %>%
    filter(has_conflicts) %>%
    pull(LSPEC)
  
  # Extract conflicting records for flagging
  flagged_records <- formatted_data %>%
    filter(LSPEC %in% conflicting_lspecs) %>%
    left_join(
      duplicate_analysis %>% 
        select(LSPEC, conflict_columns, n_records),
      by = "LSPEC"
    ) %>%
    mutate(
      flag_reason = paste("Conflicting values in:", conflict_columns),
      flag_category = "Duplicate conflicts",
      action_needed = "Manual review and resolution required"
    )
  
  # Process clean data: remove conflicts and merge non-conflicting duplicates
  clean_data <- formatted_data %>%
    filter(!LSPEC %in% conflicting_lspecs) %>%
    # For non-conflicting duplicates, merge by taking first non-NA value
    group_by(LSPEC) %>%
    summarise(
      L_SPEC_original = paste(unique(L_SPEC_original), collapse = ", "),
      CSTRAT = first(CSTRAT[!is.na(CSTRAT)]),
      ISTRAT = first(ISTRAT[!is.na(ISTRAT)]),
      YEAR = first(YEAR[!is.na(YEAR)]),
      INT = first(INT[!is.na(INT)]),
      n_records_merged = n(),
      .groups = "drop"
    ) %>%
    arrange(LSPEC)
  
  # Save flagged records if any exist
  if (nrow(flagged_records) > 0) {
    if (!dir.exists("data/flagged")) {
      dir.create("data/flagged", recursive = TRUE)
    }
    write.csv(flagged_records, "data/flagged/fieldorder_duplicate_conflicts.csv", row.names = FALSE)
  }
  
  return(list(
    clean = as.data.frame(clean_data),
    flagged = as.data.frame(flagged_records),
    summary = list(
      total_input = nrow(formatted_data),
      clean_output = nrow(clean_data),
      flagged_output = nrow(flagged_records),
      conflicting_lspecs = length(conflicting_lspecs)
    )
  ))
}