#' Merge Multiple Microscopy Line Counts per Sample
#'
#' Handles requirement to merge "four to five separate counts" per geological 
#' sample. Each row in raw data represents one microscopy line; this function 
#' aggregates them into actual geological sample counts.
#'
#' @param paleo Raw paleo data frame with individual microscopy line counts
#' @param microfossil_type Type of microfossil to process (default NULL = all types)
#' @return Data frame with merged counts per sample + taxonomic identity
#'
#' @examples
#' merged_counts <- merge_microscopy_counts(raw_paleo)
#' merged_diatoms <- merge_microscopy_counts(raw_paleo, "Diatom")
merge_microscopy_counts <- function(paleo, microfossil_type = NULL) {
  if (!is.data.frame(paleo)) {
    stop("Input must be a data frame")
  }
  required_cols <- c("Sample_ID", "Microfossil_Type", "Count")
  missing_cols <- setdiff(required_cols, names(paleo))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  available_types <- unique(paleo$Microfossil_Type)
  
  if (is.null(microfossil_type)) {
    types_to_process <- available_types
  } else {
    if (!all(microfossil_type %in% available_types)) {
      missing_types <- setdiff(microfossil_type, available_types)
      stop(paste("Microfossil type(s) not found in data:", paste(missing_types, collapse = ", ")))
    }
    types_to_process <- microfossil_type
  }
  
  merged_counts <- paleo %>%
    filter(Microfossil_Type %in% types_to_process) %>%
    group_by(Sample_ID, Microfossil_Type, Morphotype, Genus_Type, Species, Variety) %>%
    summarise(
      Count = sum(Count, na.rm = TRUE),
      n_lines_merged = n(),
      .groups = "drop"
    ) %>%
    filter(Count > 0)
  
  return(merged_counts)
}