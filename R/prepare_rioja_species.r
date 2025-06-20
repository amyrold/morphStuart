#' Prepare Species Data for Rioja Stratigraphic Analysis
#'
#' Converts integrated paleo data into rioja-compatible species matrix format.
#' Supports multiple taxonomic grouping levels for flexible analysis.
#'
#' @param integrated_paleo_metadata Merged paleo and field order data
#' @param grouping_level Taxonomic grouping level: "full_taxonomy", "genus", "morphotype", or "microfossil_type"
#' @return Matrix with samples as rows and taxa as columns, suitable for rioja
#'
#' @examples
#' species_matrix <- prepare_rioja_species_data(integrated_data, "full_taxonomy")
#' genus_matrix <- prepare_rioja_species_data(integrated_data, "genus")
prepare_rioja_species_data <- function(integrated_paleo_metadata, 
                                     grouping_level = c("full_taxonomy", "genus", "morphotype", "microfossil_type")) {
  if (!is.data.frame(integrated_paleo_metadata)) {
    stop("Input must be a data frame")
  }
  
  grouping_level <- match.arg(grouping_level)
  
  required_cols <- c("LSPEC", "Microfossil_Type", "Count")
  missing_cols <- setdiff(required_cols, names(integrated_paleo_metadata))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Create taxonomic labels based on grouping level
  data_with_labels <- integrated_paleo_metadata %>%
    mutate(
      taxon_label = create_taxon_label(Microfossil_Type, Morphotype, Genus_Type, Species, Variety, grouping_level)
    ) %>%
    filter(!is.na(taxon_label), taxon_label != "", Count > 0)
  
  # Aggregate counts by LSPEC and taxon label
  aggregated_data <- data_with_labels %>%
    group_by(LSPEC, taxon_label) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
  
  # Create wide matrix for rioja
  species_matrix <- aggregated_data %>%
    pivot_wider(
      names_from = taxon_label,
      values_from = Count,
      values_fill = 0
    ) %>%
    column_to_rownames("LSPEC") %>%
    as.matrix()
  
  # Report matrix characteristics
  cat("Rioja Species Matrix (", grouping_level, "):\n", sep = "")
  cat("Samples (rows):", nrow(species_matrix), "\n")
  cat("Taxa (columns):", ncol(species_matrix), "\n")
  cat("Total counts:", sum(species_matrix), "\n")
  cat("Non-zero entries:", sum(species_matrix > 0), "\n")
  cat("Matrix sparsity:", round(100 * sum(species_matrix == 0) / length(species_matrix), 1), "%\n\n")
  
  return(species_matrix)
}

#' Create Taxon Labels Based on Grouping Level
#'
#' Helper function to create taxonomic labels at different hierarchical levels.
#'
#' @param microfossil_type Microfossil type
#' @param morphotype Morphotype designation
#' @param genus_type Genus designation
#' @param species Species designation
#' @param variety Variety designation
#' @param grouping_level Level of taxonomic grouping
#' @return Character vector of taxon labels
create_taxon_label <- function(microfossil_type, morphotype, genus_type, species, variety, grouping_level) {
  
  taxon_label <- switch(grouping_level,
    "full_taxonomy" = paste(
      microfossil_type,
      ifelse(is.na(morphotype) | morphotype == "", "Unknown_Morphotype", morphotype),
      ifelse(is.na(genus_type) | genus_type == "", "spp", genus_type),
      ifelse(is.na(species) | species == "", "sp", species),
      ifelse(!is.na(variety) & variety != "", paste("var", variety), ""),
      sep = "_"
    ),
    
    "genus" = paste(
      microfossil_type,
      ifelse(is.na(genus_type) | genus_type == "", "spp", genus_type),
      sep = "_"
    ),
    
    "morphotype" = paste(
      microfossil_type,
      ifelse(is.na(morphotype) | morphotype == "", "Unknown_Morphotype", morphotype),
      sep = "_"
    ),
    
    "microfossil_type" = microfossil_type
  )
  
  # Clean up multiple underscores and trailing underscores
  taxon_label <- str_replace_all(taxon_label, "_+", "_")
  taxon_label <- str_remove(taxon_label, "_$")
  
  return(taxon_label)
}