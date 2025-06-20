#' Prepare Depth/Age Data for Rioja Stratigraphic Analysis
#'
#' Creates age vector in the same order as rioja species matrix for stratigraphic plotting.
#' Uses YEAR as primary axis with CSTRAT as backup.
#'
#' @param integrated_paleo_metadata Merged paleo and field order data  
#' @return Named numeric vector with LSPEC names and age/depth values
#'
#' @examples
#' depth_data <- prepare_rioja_depth_data(integrated_data)
prepare_rioja_depth_data <- function(integrated_paleo_metadata) {
  if (!is.data.frame(integrated_paleo_metadata)) {
    stop("Input must be a data frame")
  }
  
  required_cols <- c("LSPEC", "YEAR", "CSTRAT")
  missing_cols <- setdiff(required_cols, names(integrated_paleo_metadata))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Get unique LSPEC with age/depth data
  depth_data <- integrated_paleo_metadata %>%
    select(LSPEC, YEAR, CSTRAT) %>%
    distinct() %>%
    mutate(
      # Use YEAR as primary, CSTRAT as backup
      depth_value = coalesce(YEAR, CSTRAT)
    ) %>%
    filter(!is.na(depth_value)) %>%
    arrange(depth_value)
  
  # Check for duplicates
  if (anyDuplicated(depth_data$LSPEC)) {
    warning("Duplicate LSPECs found - using first occurrence")
    depth_data <- depth_data %>%
      group_by(LSPEC) %>%
      slice_head(n = 1) %>%
      ungroup()
  }
  
  # Create named vector
  depth_vector <- depth_data$depth_value
  names(depth_vector) <- depth_data$LSPEC
  
  # Report characteristics
  age_used <- sum(!is.na(depth_data$YEAR))
  depth_used <- sum(is.na(depth_data$YEAR) & !is.na(depth_data$CSTRAT))
  
  cat("Rioja Depth Data:\n")
  cat("Samples with depth/age data:", length(depth_vector), "\n")
  cat("Using YEAR (age):", age_used, "\n")
  cat("Using CSTRAT (depth):", depth_used, "\n")
  cat("Age/depth range:", min(depth_vector), "-", max(depth_vector), "\n\n")
  
  return(depth_vector)
}