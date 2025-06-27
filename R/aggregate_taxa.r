#' Aggregate Taxa by Taxonomic Level or Custom Groupings
#'
#' Flexible function to aggregate taxa at different taxonomic levels or apply
#' custom grouping logic. Supports both standard taxonomic hierarchies and
#' user-defined groupings for specialized analyses.
#'
#' @param data Data frame with taxa columns and metadata (e.g., paleo_summary_table)
#' @param level Character string specifying taxonomic level: "full", "morphotype", "genus", "microfossil_type"
#' @param custom_groupings Named list of functions for custom grouping logic by microfossil type
#' @return Data frame with aggregated taxa counts and preserved metadata
#'
#' @examples
#' # Standard taxonomic aggregation
#' agg_genus <- aggregate_taxa_by_level(paleo_summary_table, level = "genus")
#' 
#' # Custom groupings
#' custom_rules <- list(
#'   "Diatom" = function(morphotype, genus_type, species) {
#'     case_when(
#'       str_detect(morphotype, "centric") ~ "centric_diatoms",
#'       str_detect(morphotype, "araphid") ~ "araphid_diatoms",
#'       TRUE ~ "other_diatoms"
#'     )
#'   },
#'   "Phytolith" = function(morphotype, genus_type, species) {
#'     case_when(
#'       str_detect(genus_type, "grass") ~ "grassy_phytoliths",
#'       TRUE ~ "woody_phytoliths"
#'     )
#'   }
#' )
#' agg_custom <- aggregate_taxa_by_level(paleo_summary_table, custom_groupings = custom_rules)
aggregate_taxa_by_level <- function(data, level = "full", custom_groupings = NULL) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Define metadata columns to preserve
  metadata_cols <- c("LSPEC", "YEAR", "CSTRAT", "ISTRAT", "INT", "Sample_ID", "V_number", "total_count", "n_taxa")
  available_metadata <- intersect(metadata_cols, names(data))
  
  # Get taxa columns (everything that's not metadata)
  taxa_cols <- setdiff(names(data), available_metadata)
  
  if (length(taxa_cols) == 0) {
    stop("No taxa columns found in data")
  }
  
  # Apply custom groupings if provided
  if (!is.null(custom_groupings)) {
    return(apply_custom_groupings(data, custom_groupings, available_metadata, taxa_cols))
  }
  
  # Apply standard taxonomic level aggregation
  return(apply_standard_aggregation(data, level, available_metadata, taxa_cols))
}

#' Apply Custom Groupings to Taxa
#'
#' @param data Input data frame
#' @param custom_groupings Named list of grouping functions
#' @param metadata_cols Metadata column names to preserve
#' @param taxa_cols Taxa column names to process
#' @return Data frame with custom aggregated taxa
apply_custom_groupings <- function(data, custom_groupings, metadata_cols, taxa_cols) {
  
  # Parse taxa column names to extract taxonomic components
  taxa_parsed <- parse_taxa_names(taxa_cols)
  
  # Create new groupings for each taxon
  new_taxa_names <- character(length(taxa_cols))
  
  for (i in seq_along(taxa_cols)) {
    taxon_info <- taxa_parsed[i, ]
    microfossil_type <- taxon_info$microfossil_type
    
    # Apply custom grouping if available for this microfossil type
    if (microfossil_type %in% names(custom_groupings)) {
      grouping_func <- custom_groupings[[microfossil_type]]
      
      # Apply the custom function
      new_group <- tryCatch({
        grouping_func(
          morphotype = taxon_info$morphotype,
          genus_type = taxon_info$genus_type, 
          species = taxon_info$species
        )
      }, error = function(e) {
        warning(paste("Error applying custom grouping for", taxa_cols[i], ":", e$message))
        taxa_cols[i]  # Keep original name if error
      })
      
      new_taxa_names[i] <- new_group
    } else {
      # Keep original name if no custom grouping provided
      new_taxa_names[i] <- taxa_cols[i]
    }
  }
  
  # Aggregate taxa with same new names
  aggregate_by_new_names(data, taxa_cols, new_taxa_names, metadata_cols)
}

#' Apply Standard Taxonomic Level Aggregation
#'
#' @param data Input data frame
#' @param level Taxonomic level to aggregate to
#' @param metadata_cols Metadata columns to preserve
#' @param taxa_cols Taxa columns to process
#' @return Data frame with standard aggregated taxa
apply_standard_aggregation <- function(data, level, metadata_cols, taxa_cols) {
  
  if (!level %in% c("full", "morphotype", "genus", "microfossil_type")) {
    stop("Level must be one of: 'full', 'morphotype', 'genus', 'microfossil_type'")
  }
  
  if (level == "full") {
    # No aggregation needed
    return(data)
  }
  
  # Parse taxa names and create new groupings
  taxa_parsed <- parse_taxa_names(taxa_cols)
  
  new_taxa_names <- switch(level,
    "microfossil_type" = taxa_parsed$microfossil_type,
    "morphotype" = paste(taxa_parsed$microfossil_type, taxa_parsed$morphotype, sep = "_"),
    "genus" = paste(taxa_parsed$microfossil_type, taxa_parsed$genus_type, sep = "_")
  )
  
  # Clean up names
  new_taxa_names <- stringr::str_replace_all(new_taxa_names, "_+", "_")
  new_taxa_names <- stringr::str_remove(new_taxa_names, "_$")
  
  # Aggregate taxa with same new names
  aggregate_by_new_names(data, taxa_cols, new_taxa_names, metadata_cols)
}

#' Parse Taxa Column Names into Components
#'
#' @param taxa_cols Vector of taxa column names
#' @return Data frame with parsed taxonomic components
parse_taxa_names <- function(taxa_cols) {
  
  # Split taxa names by underscore
  taxa_split <- stringr::str_split(taxa_cols, "_", simplify = TRUE)
  
  # Create data frame with taxonomic components
  taxa_parsed <- data.frame(
    original_name = taxa_cols,
    microfossil_type = ifelse(ncol(taxa_split) >= 1, taxa_split[, 1], NA_character_),
    morphotype = ifelse(ncol(taxa_split) >= 2, taxa_split[, 2], NA_character_),
    genus_type = ifelse(ncol(taxa_split) >= 3, taxa_split[, 3], NA_character_),
    species = ifelse(ncol(taxa_split) >= 4, taxa_split[, 4], NA_character_),
    stringsAsFactors = FALSE
  )
  
  return(taxa_parsed)
}

#' Aggregate Taxa by New Grouping Names
#'
#' @param data Input data frame
#' @param original_taxa_cols Original taxa column names
#' @param new_taxa_names New grouping names for taxa
#' @param metadata_cols Metadata columns to preserve
#' @return Data frame with aggregated taxa
aggregate_by_new_names <- function(data, original_taxa_cols, new_taxa_names, metadata_cols) {
  
  # Create mapping data frame
  taxa_mapping <- data.frame(
    original = original_taxa_cols,
    new_group = new_taxa_names,
    stringsAsFactors = FALSE
  )
  
  # Prepare data for aggregation
  data_long <- data %>%
    dplyr::select(dplyr::all_of(c(metadata_cols, original_taxa_cols))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(original_taxa_cols),
      names_to = "original_taxon",
      values_to = "count"
    ) %>%
    dplyr::left_join(taxa_mapping, by = c("original_taxon" = "original")) %>%
    dplyr::filter(count > 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(metadata_cols, "new_group")))) %>%
    dplyr::summarise(aggregated_count = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Convert back to wide format
  result <- data_long %>%
    tidyr::pivot_wider(
      names_from = new_group,
      values_from = aggregated_count,
      values_fill = 0
    )
  
  # Recalculate summary statistics
  taxa_cols_new <- setdiff(names(result), metadata_cols)
  
  if ("total_count" %in% metadata_cols) {
    result <- result %>%
      dplyr::mutate(total_count = rowSums(dplyr::select(., dplyr::all_of(taxa_cols_new))))
  }
  
  if ("n_taxa" %in% metadata_cols) {
    result <- result %>%
      dplyr::mutate(n_taxa = rowSums(dplyr::select(., dplyr::all_of(taxa_cols_new)) > 0))
  }
  
  # Report aggregation results
  cat("Taxa Aggregation Results:\n")
  cat("Original taxa:", length(original_taxa_cols), "\n")
  cat("Aggregated taxa:", length(taxa_cols_new), "\n")
  cat("Samples:", nrow(result), "\n\n")
  
  return(as.data.frame(result))
}