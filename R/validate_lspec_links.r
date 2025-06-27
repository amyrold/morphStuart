#' Validate LSPEC Links Between Paleo and Field Order Data
#'
#' Identifies which LSPECs from paleo data are missing from field order data,
#' and reports linkage statistics for quality control before rioja analysis.
#'
#' @param paleo_stickleback Stickleback-only paleo data with LSPEC column
#' @param fieldorder_complete Complete field order data with LSPEC column
#' @return List with linkage analysis results and flagged missing LSPECs
#'
#' @examples
#' link_check <- validate_lspec_links(paleo_stickleback, fieldorder_clean)
validate_lspec_links <- function(paleo_stickleback, fieldorder_complete) {
  if (!is.data.frame(paleo_stickleback) || !is.data.frame(fieldorder_complete)) {
    stop("Both inputs must be data frames")
  }
  if (!"LSPEC" %in% names(paleo_stickleback) || !"LSPEC" %in% names(fieldorder_complete)) {
    stop("Both datasets must contain 'LSPEC' column")
  }
  
  # Get unique LSPECs from each dataset
  paleo_lspecs <- unique(paleo_stickleback$LSPEC[!is.na(paleo_stickleback$LSPEC)])
  fieldorder_lspecs <- unique(fieldorder_complete$LSPEC[!is.na(fieldorder_complete$LSPEC)])
  
  # Identify missing links
  missing_from_fieldorder <- setdiff(paleo_lspecs, fieldorder_lspecs)
  linkable_lspecs <- intersect(paleo_lspecs, fieldorder_lspecs)
  
  # Create summary
  linkage_summary <- list(
    paleo_lspecs_total = length(paleo_lspecs),
    fieldorder_lspecs_total = length(fieldorder_lspecs),
    linkable_lspecs = length(linkable_lspecs),
    missing_from_fieldorder = length(missing_from_fieldorder),
    linkage_rate = round(100 * length(linkable_lspecs) / length(paleo_lspecs), 1)
  )
  
  # Flag missing LSPECs with their paleo data
  flagged_missing <- NULL
  if (length(missing_from_fieldorder) > 0) {
    flagged_missing <- paleo_stickleback %>%
      dplyr::filter(LSPEC %in% missing_from_fieldorder) %>%
      dplyr::mutate(
        flag_reason = "LSPEC missing from field order data",
        flag_category = "Missing stratigraphic data"
      )
    
    # Save flagged data
    if (!dir.exists("data/flagged")) {
      dir.create("data/flagged", recursive = TRUE)
    }
    write.csv(flagged_missing, "data/flagged/paleo_lspecs_missing_fieldorder.csv", row.names = FALSE)
  }
  
  # Print summary
  cat("LSPEC Linkage Analysis:\n")
  cat("Paleo LSPECs:", linkage_summary$paleo_lspecs_total, "\n")
  cat("Field Order LSPECs:", linkage_summary$fieldorder_lspecs_total, "\n")
  cat("Linkable LSPECs:", linkage_summary$linkable_lspecs, "\n")
  cat("Missing from field order:", linkage_summary$missing_from_fieldorder, "\n")
  cat("Linkage rate:", linkage_summary$linkage_rate, "%\n\n")
  
  return(list(
    summary = linkage_summary,
    linkable_lspecs = linkable_lspecs,
    missing_lspecs = missing_from_fieldorder,
    flagged_data = flagged_missing
  ))
}