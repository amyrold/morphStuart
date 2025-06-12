#' Integrate Age and Depth Data from Morphology Dataset
#'
#' Links paleo samples to actual stratigraphic depths (CSTRAT) and ages (YEAR)
#' using Mike Bell's estimates via LSPEC matching. Adds age/depth information
#' to existing sample metadata without creating additional ordering columns.
#'
#' @param paleo_metadata Output from extract_paleo_metadata()
#' @param morph_with_age Morphology dataset containing age/depth estimates
#' @return List with age-integrated data and integration summary
#'
#' @examples
#' age_result <- integrate_age_data(metadata, morph_with_age)
#' samples_with_ages <- age_result$data
integrate_age_data <- function(paleo_metadata, morph_with_age) {
  # Validate inputs
  if (!is.data.frame(paleo_metadata) || !is.data.frame(morph_with_age)) {
    stop("Both inputs must be data frames")
  }
  if (!"LSPEC" %in% names(paleo_metadata)) {
    stop("paleo_metadata must contain 'LSPEC' column from extract_paleo_metadata()")
  }
  if (!any(c("CSTRAT", "YEAR") %in% names(morph_with_age))) {
    stop("morph_with_age must contain 'CSTRAT' and/or 'YEAR' columns")
  }
  
  cat("=== INTEGRATING AGE/DEPTH DATA ===\n")
  cat("Using Mike Bell's age estimates from morphology dataset\n")
  
  # Extract age/depth reference data by LSPEC
  # Average multiple specimens from same stratigraphic level
  age_reference <- morph_with_age %>%
    filter(!is.na(LSPEC), !is.na(CSTRAT) | !is.na(YEAR)) %>%
    group_by(LSPEC) %>%
    summarise(
      CSTRAT = mean(CSTRAT, na.rm = TRUE),
      YEAR = mean(YEAR, na.rm = TRUE),
      n_morph_specimens = n(),
      age_range_years = if_else(n() > 1, max(YEAR, na.rm = TRUE) - min(YEAR, na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    # Clean infinite values from averaging
    mutate(
      CSTRAT = if_else(is.infinite(CSTRAT), NA_real_, CSTRAT),
      YEAR = if_else(is.infinite(YEAR), NA_real_, YEAR)
    )
  
  # Join paleo metadata with age data
  paleo_with_ages <- paleo_metadata %>%
    left_join(age_reference, by = "LSPEC", suffix = c("", "_morph"))
  
  # Calculate integration statistics
  integration_summary <- paleo_with_ages %>%
    summarise(
      total_samples = n(),
      samples_with_cstrat = sum(!is.na(CSTRAT), na.rm = TRUE),
      samples_with_year = sum(!is.na(YEAR), na.rm = TRUE),
      killifish_samples = sum(sample_type == "Killifish", na.rm = TRUE),
      stickleback_samples = sum(sample_type == "Stickleback", na.rm = TRUE),
      percent_with_cstrat = round(100 * sum(!is.na(CSTRAT), na.rm = TRUE) / n(), 1)
    )
  
  cat("Age integration results:\n")
  cat("Total paleo samples:", integration_summary$total_samples, "\n")
  cat("Samples with CSTRAT depth:", integration_summary$samples_with_cstrat, 
      "(", integration_summary$percent_with_cstrat, "%)\n")
  cat("Samples with YEAR age:", integration_summary$samples_with_year, "\n")
  
  # Check for potential linking issues
  missing_age_LSPECs <- paleo_with_ages %>%
    filter(is.na(CSTRAT), sample_type == "Stickleback") %>%
    select(LSPEC) %>%
    distinct()
  
  if (nrow(missing_age_LSPECs) > 0) {
    cat("WARNING:", nrow(missing_age_LSPECs), "stickleback LSPECs without age data\n")
    cat("Missing LSPECs:", paste(missing_age_LSPECs$LSPEC, collapse = ", "), "\n")
  }
  
  return(list(
    data = paleo_with_ages,
    age_reference = age_reference,
    integration_summary = integration_summary,
    has_age_data = integration_summary$samples_with_cstrat > 0
  ))
}