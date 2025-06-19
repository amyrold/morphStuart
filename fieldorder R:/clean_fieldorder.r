#' Create Clean Field Order Dataset
#'
#' Removes problematic LSPECs with conflicting duplicate values and deduplicates
#' clean records to create a final dataset suitable for analysis. Only removes
#' records with actual conflicts, preserving redundant but consistent entries.
#'
#' @param formatted_data Formatted field order data frame
#' @param problematic_lspecs Vector of LSPEC values with conflicts to remove
#' @return Clean data frame with one record per LSPEC, conflicts removed
#'
#' @examples
#' clean_data <- clean_fieldorder(formatted_data, problematic_lspecs)
clean_fieldorder <- function(formatted_data, problematic_lspecs) {
  if (!is.data.frame(formatted_data)) {
    stop("Input must be a data frame")
  }
  if (!"LSPEC" %in% names(formatted_data)) {
    stop("Data must contain 'LSPEC' column")
  }
  if (!is.vector(problematic_lspecs)) {
    stop("problematic_lspecs must be a vector")
  }
  
  # Remove problematic LSPECs and deduplicate clean ones
  clean_data <- formatted_data %>%
    filter(!LSPEC %in% problematic_lspecs) %>%
    # Remove original spec column for clean dataset
    select(-L_SPEC_original) %>%
    # Deduplicate: keep first occurrence since values are identical for clean duplicates
    distinct(LSPEC, .keep_all = TRUE) %>%
    # Add data quality flags
    mutate(
      data_quality = "Clean",
      has_missing_data = is.na(CSTRAT) | is.na(ISTRAT) | is.na(YEAR) | is.na(INT),
      missing_count = rowSums(is.na(select(., CSTRAT, ISTRAT, YEAR, INT))),
      completeness_score = 1 - (missing_count / 4)
    ) %>%
    arrange(LSPEC)
  
  return(as.data.frame(clean_data))
}

#' Identify Records with Missing Key Data
#'
#' Finds records in the clean dataset that have missing values in key columns.
#' These records are usable but may have limitations for certain analyses.
#'
#' @param clean_data Clean field order data frame
#' @return Data frame with records that have missing key data
#'
#' @examples
#' missing_data_records <- identify_missing_data_records(clean_data)
identify_missing_data_records <- function(clean_data) {
  if (!is.data.frame(clean_data)) {
    stop("Input must be a data frame")
  }
  
  key_columns <- c("CSTRAT", "ISTRAT", "YEAR", "INT")
  missing_cols <- intersect(key_columns, names(clean_data))
  
  if (length(missing_cols) == 0) {
    stop("Data must contain at least one of: CSTRAT, ISTRAT, YEAR, INT")
  }
  
  # Find records with any missing data in key columns
  records_with_missing <- clean_data %>%
    # Add row identifier for tracking
    mutate(row_id = row_number()) %>%
    # Check for missing values in key columns
    filter(rowSums(is.na(select(., all_of(missing_cols)))) > 0) %>%
    mutate(
      missing_CSTRAT = if("CSTRAT" %in% names(.)) is.na(CSTRAT) else FALSE,
      missing_ISTRAT = if("ISTRAT" %in% names(.)) is.na(ISTRAT) else FALSE,
      missing_YEAR = if("YEAR" %in% names(.)) is.na(YEAR) else FALSE,
      missing_INT = if("INT" %in% names(.)) is.na(INT) else FALSE,
      missing_columns = pmap_chr(
        list(missing_CSTRAT, missing_ISTRAT, missing_YEAR, missing_INT),
        function(c, i, y, int) {
          missing_cols <- c()
          if(c) missing_cols <- c(missing_cols, "CSTRAT")
          if(i) missing_cols <- c(missing_cols, "ISTRAT")
          if(y) missing_cols <- c(missing_cols, "YEAR")
          if(int) missing_cols <- c(missing_cols, "INT")
          paste(missing_cols, collapse = ", ")
        }
      ),
      data_usability = case_when(
        missing_count >= 3 ~ "Limited - most data missing",
        missing_count == 2 ~ "Moderate - half data missing", 
        missing_count == 1 ~ "Good - minimal missing data",
        TRUE ~ "Complete"
      )
    ) %>%
    select(-row_id) %>%
    arrange(desc(missing_count), LSPEC)
  
  return(as.data.frame(records_with_missing))
}