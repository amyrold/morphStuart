#' Merge Paleo Data with Field Order Stratigraphic Information
#'
#' Integrates stickleback paleo data with field order age/depth data using LSPEC identifiers.
#' Only includes records with complete linkage for rioja analysis.
#'
#' @param paleo_stickleback Stickleback paleo data with LSPEC identifiers
#' @param fieldorder_complete Complete field order data with stratigraphic information
#' @return Data frame with merged paleo and stratigraphic data
#'
#' @examples
#' integrated_data <- merge_paleo_with_fieldorder(paleo_stickleback, fieldorder_clean)
merge_paleo_with_fieldorder <- function(paleo_stickleback, fieldorder_complete) {
  if (!is.data.frame(paleo_stickleback) || !is.data.frame(fieldorder_complete)) {
    stop("Both inputs must be data frames")
  }
  if (!"LSPEC" %in% names(paleo_stickleback) || !"LSPEC" %in% names(fieldorder_complete)) {
    stop("Both datasets must contain 'LSPEC' column")
  }
  
  # Check for required field order columns
  required_fieldorder_cols <- c("LSPEC", "YEAR", "CSTRAT")
  missing_cols <- setdiff(required_fieldorder_cols, names(fieldorder_complete))
  if (length(missing_cols) > 0) {
    stop(paste("Field order data missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Prepare field order data for merging
  fieldorder_for_merge <- fieldorder_complete %>%
    select(LSPEC, YEAR, CSTRAT, ISTRAT, INT) %>%
    filter(!is.na(LSPEC))
  
  # Merge datasets
  merged_data <- paleo_stickleback %>%
    inner_join(fieldorder_for_merge, by = "LSPEC") %>%
    filter(!is.na(YEAR) | !is.na(CSTRAT))  # Keep records with at least age or depth
  
  # Quality checks
  original_samples <- length(unique(paleo_stickleback$Sample_ID))
  merged_samples <- length(unique(merged_data$Sample_ID))
  
  # Check for multiple field order records per LSPEC
  duplicate_check <- merged_data %>%
    group_by(LSPEC) %>%
    summarise(
      n_year_values = length(unique(YEAR[!is.na(YEAR)])),
      n_cstrat_values = length(unique(CSTRAT[!is.na(CSTRAT)])),
      .groups = "drop"
    ) %>%
    filter(n_year_values > 1 | n_cstrat_values > 1)
  
  if (nrow(duplicate_check) > 0) {
    warning(paste("Found", nrow(duplicate_check), "LSPECs with multiple age/depth values"))
  }
  
  # Sort by age (YEAR) for stratigraphic order
  merged_data <- merged_data %>%
    arrange(YEAR, CSTRAT, LSPEC)
  
  # Report merge results
  cat("Paleo-Field Order Integration:\n")
  cat("Original paleo samples:", original_samples, "\n")
  cat("Successfully merged samples:", merged_samples, "\n")
  cat("Integration rate:", round(100 * merged_samples / original_samples, 1), "%\n")
  cat("Records in merged dataset:", nrow(merged_data), "\n\n")
  
  return(as.data.frame(merged_data))
}