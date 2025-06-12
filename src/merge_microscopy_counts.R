#' Merge Multiple Microscopy Line Counts per Sample
#'
#' CRITICAL FUNCTION: Handles Jacopo's requirement to merge "four to five separate 
#' counts" per geological sample. Each row in raw data represents one microscopy 
#' line; this function aggregates them into actual geological sample counts.
#' This step is essential before any downstream analysis.
#'
#' @param paleo Raw paleo data frame with individual microscopy line counts
#' @param microfossil_type Type of microfossil to process (default NULL = all types)
#' @return Data frame with merged counts per sample + taxonomic identity
#'
#' @examples
#' # Process all microfossil types (default)
#' merged_counts <- merge_microscopy_counts(raw_paleo)
#' 
#' # Process only diatoms
#' merged_diatoms <- merge_microscopy_counts(raw_paleo, "Diatom")
#' 
#' # Process only phytoliths  
#' merged_phytoliths <- merge_microscopy_counts(raw_paleo, "Phytolith")
merge_microscopy_counts <- function(paleo, microfossil_type = NULL) {
  # Validate inputs
  if (!is.data.frame(paleo)) {
    stop("Input must be a data frame")
  }
  required_cols <- c("Sample_ID", "Microfossil_Type", "Count")
  missing_cols <- setdiff(required_cols, names(paleo))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Determine which microfossil types to process
  available_types <- unique(paleo$Microfossil_Type)
  
  if (is.null(microfossil_type)) {
    # Process all types
    types_to_process <- available_types
    cat("=== MERGING MICROSCOPY LINE COUNTS ===\n")
    cat("Processing all microfossil types:", paste(types_to_process, collapse = ", "), "\n")
  } else {
    # Process specified type(s)
    if (!all(microfossil_type %in% available_types)) {
      missing_types <- setdiff(microfossil_type, available_types)
      stop(paste("Microfossil type(s) not found in data:", paste(missing_types, collapse = ", ")))
    }
    types_to_process <- microfossil_type
    cat("=== MERGING MICROSCOPY LINE COUNTS ===\n")
    cat("Processing specified microfossil type(s):", paste(types_to_process, collapse = ", "), "\n")
  }
  
  # Filter and merge counts by sample + complete taxonomic identity
  # This is the critical step that was missing from original analysis
  merged_counts <- paleo %>%
    filter(Microfossil_Type %in% types_to_process) %>%
    group_by(Sample_ID, Microfossil_Type, Morphotype, Genus_Type, Species, Variety) %>%
    summarise(
      Count = sum(Count, na.rm = TRUE),
      n_lines_merged = n(),  # Track how many lines were merged
      .groups = "drop"
    ) %>%
    filter(Count > 0)  # Remove zero counts after merging
  
  # Calculate merging statistics
  original_rows <- nrow(paleo %>% filter(Microfossil_Type %in% types_to_process))
  final_rows <- nrow(merged_counts)
  rows_merged <- original_rows - final_rows
  
  cat("Original microscopy lines:", original_rows, "\n")
  cat("After merging geological samples:", final_rows, "\n")
  cat("Lines merged:", rows_merged, "(", round(100 * rows_merged / original_rows, 1), "%)\n")
  
  # Show examples of successful merging by microfossil type
  merge_examples <- merged_counts %>%
    filter(n_lines_merged > 1) %>%
    arrange(Microfossil_Type, desc(n_lines_merged)) %>%
    group_by(Microfossil_Type) %>%
    slice_head(n = 3) %>%
    ungroup()
  
  if (nrow(merge_examples) > 0) {
    cat("\nExamples of merged counts by type:\n")
    print(merge_examples %>% 
            select(Sample_ID, Microfossil_Type, Morphotype, Count, n_lines_merged) %>%
            mutate(Sample_ID = str_trunc(Sample_ID, 20)))
  }
  
  # Check for potential issues
  max_lines_merged <- max(merged_counts$n_lines_merged, na.rm = TRUE)
  if (max_lines_merged > 10) {
    cat("WARNING: Some samples had >10 microscopy lines. Check for data issues.\n")
  }
  
  # Summary by microfossil type
  type_summary <- merged_counts %>%
    group_by(Microfossil_Type) %>%
    summarise(
      samples = n_distinct(Sample_ID),
      taxa = n_distinct(paste(Morphotype, Genus_Type, Species)),
      total_specimens = sum(Count),
      .groups = "drop"
    )
  
  cat("\nSummary by microfossil type:\n")
  print(type_summary)
  
  return(merged_counts)
}