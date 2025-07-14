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
  
  # Create taxonomic labels based on grouping level
  if (grouping_level == "functional_groups") {
    # Use the functional_group column directly
    if (!"functional_group" %in% names(integrated_paleo_metadata)) {
      stop("functional_groups requested but data does not contain 'functional_group' column. Run apply_functional_labels() first.")
    }
    
    data_with_labels <- integrated_paleo_metadata %>%
      dplyr::filter(!is.na(functional_group), Count > 0) %>%
      dplyr::mutate(taxon_label = functional_group)
      
  } else {
    # Create full taxonomy labels
    data_with_labels <- integrated_paleo_metadata %>%
      dplyr::mutate(
        taxon_label = paste(
          Microfossil_Type,
          ifelse(is.na(Morphotype) | Morphotype == "", "Unknown_Morphotype", Morphotype),
          ifelse(is.na(Genus_Type) | Genus_Type == "", "spp", Genus_Type),
          ifelse(is.na(Species) | Species == "", "sp", Species),
          ifelse(!is.na(Variety) & Variety != "", paste("var", Variety), ""),
          sep = "_"
        )
      ) %>%
      dplyr::mutate(
        taxon_label = stringr::str_replace_all(taxon_label, "_+", "_"),
        taxon_label = stringr::str_remove(taxon_label, "_$")
      ) %>%
      dplyr::filter(!is.na(taxon_label), taxon_label != "", Count > 0)
  }
  
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
  data_type_label <- if (grouping_level == "functional_groups") "functional groups" else "individual taxa"
  cat("Rioja Species Matrix (", grouping_level, " - ", data_type_label, "):\n", sep = "")
  cat("Samples (rows):", nrow(species_matrix), "\n")
  cat("Taxa (columns):", ncol(species_matrix), "\n")
  cat("Total counts:", sum(species_matrix), "\n")
  cat("Non-zero entries:", sum(species_matrix > 0), "\n")
  cat("Matrix sparsity:", round(100 * sum(species_matrix == 0) / length(species_matrix), 1), "%\n\n")
  
  return(species_matrix)
}