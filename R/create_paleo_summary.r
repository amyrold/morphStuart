#' Create Paleo Summary Table with Individual Taxa Columns
#'
#' Creates a wide-format summary table with LSPEC as rows and individual taxa as columns,
#' preserving full taxonomic resolution from the integrated paleo-fieldorder dataset.
#'
#' @param integrated_paleo_metadata Merged paleo and field order data
#' @return Data frame with LSPEC rows and taxa count columns plus metadata
#'
#' @examples
#' summary_table <- create_paleo_summary_table(integrated_data)
create_paleo_summary_table <- function(integrated_paleo_metadata) {
  if (!is.data.frame(integrated_paleo_metadata)) {
    stop("Input must be a data frame")
  }
  
  required_cols <- c("LSPEC", "Microfossil_Type", "Count", "YEAR", "CSTRAT")
  missing_cols <- setdiff(required_cols, names(integrated_paleo_metadata))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create species labels (reuse logic from create_species_matrix.R)
  data_with_labels <- integrated_paleo_metadata %>%
    mutate(
      species_label = create_species_label(Microfossil_Type, Morphotype, Genus_Type, Species, Variety)
    ) %>%
    filter(!is.na(species_label), species_label != "", Count > 0)
  
  # Aggregate counts by LSPEC and species (in case of duplicates)
  aggregated_counts <- data_with_labels %>%
    group_by(LSPEC, species_label) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
  
  # Create wide format with species as columns
  species_wide <- aggregated_counts %>%
    pivot_wider(
      names_from = species_label,
      values_from = Count,
      values_fill = 0
    )
  
  # Get metadata for each LSPEC (take first record per LSPEC)
  lspec_metadata <- data_with_labels %>%
    group_by(LSPEC) %>%
    summarise(
      YEAR = first(YEAR),
      CSTRAT = first(CSTRAT),
      ISTRAT = first(ISTRAT),
      INT = first(INT),
      Sample_ID = first(Sample_ID),
      V_number = first(V_number),
      total_count = sum(Count, na.rm = TRUE),
      n_taxa = n_distinct(species_label),
      .groups = "drop"
    )
  
  # Merge metadata with species counts
  summary_table <- lspec_metadata %>%
    left_join(species_wide, by = "LSPEC") %>%
    arrange(YEAR, CSTRAT)
  
  # Replace NA with 0 for count columns
  species_cols <- setdiff(names(summary_table), names(lspec_metadata))
  summary_table[species_cols][is.na(summary_table[species_cols])] <- 0
  
  # Report summary
  cat("Paleo Summary Table Created:\n")
  cat("LSPECs included:", nrow(summary_table), "\n")
  cat("Taxa columns:", length(species_cols), "\n")
  cat("Age range:", min(summary_table$YEAR, na.rm = TRUE), "-", max(summary_table$YEAR, na.rm = TRUE), "years\n")
  cat("Depth range:", min(summary_table$CSTRAT, na.rm = TRUE), "-", max(summary_table$CSTRAT, na.rm = TRUE), "cm\n\n")
  
  return(as.data.frame(summary_table))
}

#' Create Species Label from Taxonomic Components
#'
#' Helper function to create consistent species labels following the same logic
#' as in create_species_matrix.R
#'
#' @param microfossil_type Microfossil type
#' @param morphotype Morphotype designation  
#' @param genus_type Genus designation
#' @param species Species designation
#' @param variety Variety designation
#' @return Character vector of species labels
create_species_label <- function(microfossil_type, morphotype, genus_type, species, variety) {
  species_label <- paste(
    microfossil_type,
    ifelse(is.na(morphotype) | morphotype == "", "Unknown_Morphotype", morphotype),
    ifelse(is.na(genus_type) | genus_type == "", "spp", genus_type),
    ifelse(is.na(species) | species == "", "sp", species),
    ifelse(!is.na(variety) & variety != "", paste("var", variety), ""),
    sep = "_"
  )
  
  # Clean up multiple underscores and trailing underscores
  species_label <- str_replace_all(species_label, "_+", "_")
  species_label <- str_remove(species_label, "_$")
  
  return(species_label)
}