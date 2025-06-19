#' Identify Duplicate LSPECs and Conflicts in Field Order Data
#'
#' Analyzes formatted field order data to identify duplicate LSPEC values and
#' determine whether they represent true conflicts (different values) or just
#' redundant entries with identical information.
#'
#' @param formatted_data Formatted field order data frame with LSPEC column
#' @return Data frame with duplicate analysis results including conflict flags
#'
#' @examples
#' duplicate_analysis <- identify_fieldorder_duplicates(formatted_data)
identify_fieldorder_duplicates <- function(formatted_data) {
  if (!is.data.frame(formatted_data)) {
    stop("Input must be a data frame")
  }
  if (!"LSPEC" %in% names(formatted_data)) {
    stop("Data must contain 'LSPEC' column")
  }
  
  # Identify duplicates and check for conflicts
  duplicate_analysis <- formatted_data %>%
    group_by(LSPEC) %>%
    filter(n() > 1) %>%
    summarise(
      n_records = n(),
      original_specs = paste(L_SPEC_original, collapse = ", "),
      unique_CSTRAT = n_distinct(CSTRAT, na.rm = TRUE),
      unique_ISTRAT = n_distinct(ISTRAT, na.rm = TRUE),
      unique_YEAR = n_distinct(YEAR, na.rm = TRUE),
      unique_INT = n_distinct(INT, na.rm = TRUE),
      # Check if values are actually different (not just missing)
      conflicting_CSTRAT = length(unique(CSTRAT[!is.na(CSTRAT)])) > 1,
      conflicting_ISTRAT = length(unique(ISTRAT[!is.na(ISTRAT)])) > 1,
      conflicting_YEAR = length(unique(YEAR[!is.na(YEAR)])) > 1,
      conflicting_INT = length(unique(INT[!is.na(INT)])) > 1,
      .groups = "drop"
    ) %>%
    mutate(
      has_real_conflicts = conflicting_CSTRAT | conflicting_ISTRAT | 
        conflicting_YEAR | conflicting_INT,
      conflict_severity = case_when(
        conflicting_CSTRAT + conflicting_ISTRAT + conflicting_YEAR + conflicting_INT >= 3 ~ "High",
        conflicting_CSTRAT + conflicting_ISTRAT + conflicting_YEAR + conflicting_INT >= 2 ~ "Medium", 
        has_real_conflicts ~ "Low",
        TRUE ~ "None"
      ),
      conflict_columns = paste(c(
        ifelse(conflicting_CSTRAT, "CSTRAT", ""),
        ifelse(conflicting_ISTRAT, "ISTRAT", ""),
        ifelse(conflicting_YEAR, "YEAR", ""),
        ifelse(conflicting_INT, "INT", "")
      )[c(
        ifelse(conflicting_CSTRAT, "CSTRAT", ""),
        ifelse(conflicting_ISTRAT, "ISTRAT", ""),
        ifelse(conflicting_YEAR, "YEAR", ""),
        ifelse(conflicting_INT, "INT", "")
      ) != ""], collapse = ", "),
      conflict_summary = case_when(
        !has_real_conflicts ~ "No conflicts - redundant entries",
        TRUE ~ paste("Conflicts in:", conflict_columns)
      )
    )
  
  return(as.data.frame(duplicate_analysis))
}

#' Get Lists of Problematic and Clean LSPECs
#'
#' Extracts lists of LSPEC values that have real conflicts vs those that are
#' clean duplicates or unique records.
#'
#' @param duplicate_analysis Results from identify_fieldorder_duplicates()
#' @param formatted_data Original formatted data frame
#' @return List containing problematic_lspecs and all_lspecs vectors
#'
#' @examples
#' lspec_classification <- classify_lspecs(duplicate_analysis, formatted_data)
classify_lspecs <- function(duplicate_analysis, formatted_data) {
  if (!is.data.frame(duplicate_analysis)) {
    stop("duplicate_analysis must be a data frame")
  }
  if (!is.data.frame(formatted_data)) {
    stop("formatted_data must be a data frame")
  }
  
  # Identify truly problematic duplicates (with actual conflicting values)
  problematic_lspecs <- duplicate_analysis %>%
    filter(has_real_conflicts) %>%
    pull(LSPEC)
  
  # Get all unique LSPECs for reference
  all_lspecs <- unique(formatted_data$LSPEC)
  
  # Get clean LSPECs (no conflicts)
  clean_lspecs <- setdiff(all_lspecs, problematic_lspecs)
  
  return(list(
    problematic_lspecs = problematic_lspecs,
    clean_lspecs = clean_lspecs,
    all_lspecs = all_lspecs,
    n_problematic = length(problematic_lspecs),
    n_clean = length(clean_lspecs),
    n_total = length(all_lspecs)
  ))
}