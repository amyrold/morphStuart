#' Prepare Species Data for Rioja Stratigraphic Analysis
#'
#' Converts integrated paleo data into rioja-compatible species matrix format.
#' Supports full taxonomy or functional groupings for flexible analysis.
#'
#' @param integrated_paleo_metadata Merged paleo and field order data
#' @param grouping_level Taxonomic grouping level: "full_taxonomy" or "functional_groups"
#' @return Matrix with samples as rows and taxa as columns, suitable for rioja
#'
#' @examples
#' species_matrix <- prepare_rioja_species_data(integrated_data, "full_taxonomy")
#' functional_matrix <- prepare_rioja_species_data(integrated_data, "functional_groups")
prepare_rioja_species_data <- function(integrated_paleo_metadata, 
                                     grouping_level = c("full_taxonomy", "functional_groups")) {
  if (!is.data.frame(integrated_paleo_metadata)) {
    stop("Input must be a data frame")
  }
  
  grouping_level <- match.arg(grouping_level)
  
  required_cols <- c("LSPEC", "Microfossil_Type", "Count")
  missing_cols <- setdiff(required_cols, names(integrated_paleo_metadata))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Detect if data contains functional groups
  has_functional_groups <- any(integrated_paleo_metadata$Morphotype == "functional_group", na.rm = TRUE)
  
  # Validate grouping level against data type
  if (grouping_level == "functional_groups" && !has_functional_groups) {
    stop("Requested functional_groups but data does not contain functional groups. Use apply_functional_groupings() first.")
  }
  
  if (grouping_level == "full_taxonomy" && has_functional_groups) {
    warning("Data contains functional groups but full_taxonomy requested. Results may be unexpected.")
  }
  
  # Create taxonomic labels based on grouping level and data type
  data_with_labels <- integrated_paleo_metadata %>%
    dplyr::mutate(
      taxon_label = create_taxon_label(Microfossil_Type, Morphotype, Genus_Type, Species, Variety, grouping_level)
    ) %>%
    dplyr::filter(!is.na(taxon_label), taxon_label != "", Count > 0)
  
  # Aggregate counts by LSPEC and taxon label
  aggregated_data <- data_with_labels %>%
    dplyr::group_by(LSPEC, taxon_label) %>%
    dplyr::summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
  
  # Create wide matrix for rioja
  species_matrix <- aggregated_data %>%
    tidyr::pivot_wider(
      names_from = taxon_label,
      values_from = Count,
      values_fill = 0
    ) %>%
    tibble::column_to_rownames("LSPEC") %>%
    as.matrix()
  
  # Report matrix characteristics
  data_type_label <- if (has_functional_groups) "functional groups" else "individual taxa"
  cat("Rioja Species Matrix (", grouping_level, " - ", data_type_label, "):\n", sep = "")
  cat("Samples (rows):", nrow(species_matrix), "\n")
  cat("Taxa (columns):", ncol(species_matrix), "\n")
  cat("Total counts:", sum(species_matrix), "\n")
  cat("Non-zero entries:", sum(species_matrix > 0), "\n")
  cat("Matrix sparsity:", round(100 * sum(species_matrix == 0) / length(species_matrix), 1), "%\n\n")
  
  return(species_matrix)
}

#' Create Taxon Labels Based on Grouping Level
#'
#' Helper function to create taxonomic labels. Now intelligently handles
#' functional groups by using the group name directly.
#'
#' @param microfossil_type Microfossil type (or functional group name)
#' @param morphotype Morphotype designation (or "functional_group")
#' @param genus_type Genus designation
#' @param species Species designation
#' @param variety Variety designation
#' @param grouping_level Level of taxonomic grouping
#' @return Character vector of taxon labels
create_taxon_label <- function(microfossil_type, morphotype, genus_type, species, variety, grouping_level) {
  
  # Check if this is functional group data
  is_functional_group <- !is.na(morphotype) & morphotype == "functional_group"
  
  taxon_label <- switch(grouping_level,
    "functional_groups" = {
      if (any(is_functional_group)) {
        # For functional groups, just use the microfossil_type (which contains the group name)
        microfossil_type
      } else {
        stop("functional_groups requested but data does not contain functional groups")
      }
    },
    
    "full_taxonomy" = {
      if (any(is_functional_group)) {
        # If data has functional groups but full taxonomy requested, just use group name
        warning("Data contains functional groups - using group names only")
        microfossil_type
      } else {
        # Standard full taxonomy construction
        paste(
          microfossil_type,
          ifelse(is.na(morphotype) | morphotype == "", "Unknown_Morphotype", morphotype),
          ifelse(is.na(genus_type) | genus_type == "", "spp", genus_type),
          ifelse(is.na(species) | species == "", "sp", species),
          ifelse(!is.na(variety) & variety != "", paste("var", variety), ""),
          sep = "_"
        )
      }
    }
  )
  
  # Clean up multiple underscores and trailing underscores (only for full taxonomy)
  if (grouping_level == "full_taxonomy" && !any(is_functional_group)) {
    taxon_label <- stringr::str_replace_all(taxon_label, "_+", "_")
    taxon_label <- stringr::str_remove(taxon_label, "_$")
  }
  
  return(taxon_label)
}