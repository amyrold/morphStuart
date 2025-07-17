#' Merge Morphology Data with Field Order Stratigraphic Information
#'
#' Integrates morphology data with field order age/depth data using LSPEC identifiers.
#' Flags specimens without CSTRAT depth data for review.
#'
#' @param morph_final Final morphology data with LSPEC identifiers
#' @param fieldorder_complete Complete field order data with stratigraphic information
#' @return Data frame with merged morphology and stratigraphic data (only records with CSTRAT)
#'
#' @examples
#' morph_integrated <- merge_morph_with_fieldorder(morph_final, fieldorder_complete)
merge_morph_with_fieldorder <- function(morph_final, fieldorder_complete) {
  if (!is.data.frame(morph_final) || !is.data.frame(fieldorder_complete)) {
    stop("Both inputs must be data frames")
  }
  if (!"LSPEC" %in% names(morph_final) || !"LSPEC" %in% names(fieldorder_complete)) {
    stop("Both datasets must contain 'LSPEC' column")
  }
  
  # Check for required field order columns
  required_fieldorder_cols <- c("LSPEC", "CSTRAT")
  missing_cols <- setdiff(required_fieldorder_cols, names(fieldorder_complete))
  if (length(missing_cols) > 0) {
    stop(paste("Field order data missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Prepare field order data for merging
  fieldorder_for_merge <- fieldorder_complete %>%
    dplyr::select(LSPEC, YEAR, CSTRAT, ISTRAT, INT) %>%
    dplyr::filter(!is.na(LSPEC))
  
  # Merge datasets
  merged_data <- morph_final %>%
    dplyr::left_join(fieldorder_for_merge, by = "LSPEC")
  
  # Identify specimens without depth data
  specimens_without_depth <- merged_data %>%
    dplyr::filter(is.na(CSTRAT)) %>%
    dplyr::mutate(
      flag_reason = "Missing CSTRAT depth data after fieldorder merge",
      flag_category = "Missing stratigraphic data",
      action_needed = "Find CSTRAT data or exclude from stratigraphic analysis"
    )
  
  # Keep only specimens with CSTRAT data
  complete_data <- merged_data %>%
    dplyr::filter(!is.na(CSTRAT)) %>%
    dplyr::arrange(CSTRAT, LSPEC)  # Sort by depth (CSTRAT) as primary
  
  # Save flagged specimens if any exist
  if (nrow(specimens_without_depth) > 0) {
    if (!dir.exists("data/flagged")) {
      dir.create("data/flagged", recursive = TRUE)
    }
    write.csv(specimens_without_depth, "data/flagged/morph_missing_depths.csv", row.names = FALSE)
  }
  
  # Check for multiple field order records per LSPEC
  duplicate_check <- complete_data %>%
    dplyr::group_by(LSPEC) %>%
    dplyr::summarise(
      n_year_values = length(unique(YEAR[!is.na(YEAR)])),
      n_cstrat_values = length(unique(CSTRAT[!is.na(CSTRAT)])),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_year_values > 1 | n_cstrat_values > 1)
  
  if (nrow(duplicate_check) > 0) {
    warning(paste("Found", nrow(duplicate_check), "LSPECs with multiple age/depth values"))
  }
  
  return(as.data.frame(complete_data))
}