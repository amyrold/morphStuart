#' Format and Standardize Field Order Data
#'
#' Formats field order data that has already had IDs extracted.
#' Handles numeric column conversion and missing value indicators.
#'
#' @param data Field order data frame with LSPEC already extracted
#' @return Data frame with standardized columns and proper data types
#'
#' @examples
#' formatted_data <- format_fieldorder(order_with_ids)
format_fieldorder <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!"LSPEC" %in% names(data)) {
    stop("Data must contain 'LSPEC' column (run extract_ids() first)")
  }
  
  # Format and standardize the data
  formatted <- data %>%
    mutate(
      # Convert relevant columns to numeric with proper NA handling
      CSTRAT = suppressWarnings(as.numeric(CSTRAT)),
      ISTRAT = suppressWarnings(as.numeric(ISTRAT)), 
      YEAR = suppressWarnings(as.numeric(YEAR)),
      INT = suppressWarnings(as.numeric(INT))
    ) %>%
    # Select only the columns needed for analysis
    select(L..SPEC, LSPEC, CSTRAT, ISTRAT, YEAR, INT) %>%
    # Rename original column for clarity
    rename(L_SPEC_original = L..SPEC) %>%
    # Remove any rows where LSPEC couldn't be created
    filter(!is.na(LSPEC), LSPEC != "LNA")
  
  return(as.data.frame(formatted))
}

#' Generate Field Order Data Quality Summary
#'
#' Creates summary statistics for data quality assessment after formatting.
#' Used for validation and reporting purposes.
#'
#' @param formatted_data Formatted field order data frame
#' @return List with data quality metrics
#'
#' @examples
#' quality_stats <- get_fieldorder_quality_summary(formatted_data)
get_fieldorder_quality_summary <- function(formatted_data) {
  summary <- list(
    total_records = nrow(formatted_data),
    unique_lspecs = n_distinct(formatted_data$LSPEC),
    missing_cstrat = sum(is.na(formatted_data$CSTRAT)),
    missing_istrat = sum(is.na(formatted_data$ISTRAT)),
    missing_year = sum(is.na(formatted_data$YEAR)),
    missing_int = sum(is.na(formatted_data$INT)),
    any_missing_key_data = sum(is.na(formatted_data$CSTRAT) & 
                                 is.na(formatted_data$ISTRAT) & 
                                 is.na(formatted_data$YEAR)),
    completeness_percentages = list(
      cstrat = round(100 * sum(!is.na(formatted_data$CSTRAT)) / nrow(formatted_data), 1),
      istrat = round(100 * sum(!is.na(formatted_data$ISTRAT)) / nrow(formatted_data), 1),
      year = round(100 * sum(!is.na(formatted_data$YEAR)) / nrow(formatted_data), 1),
      int = round(100 * sum(!is.na(formatted_data$INT)) / nrow(formatted_data), 1)
    )
  )
  
  return(summary)
}