#' Create Species-Level Matrix (Full Taxonomic Resolution)
#'
#' Creates summary tables with each individually recorded taxon as separate columns,
#' providing maximum taxonomic resolution for detailed paleoenvironmental analysis.
#' This complements the morphotype-grouped approach by preserving species-level information.
#'
#' @param merged_counts Output from merge_microscopy_counts() with full taxonomic data
#' @param metadata_with_ages Output from integrate_age_data() (optional)
#' @return List with species-level matrices for stickleback, killifish, and combined data
#'
#' @examples
#' species_results <- create_species_matrix(paleo_merged_counts, age_data)
create_species_matrix <- function(merged_counts, metadata_with_ages = NULL) {
  if (!is.data.frame(merged_counts)) {
    stop("Input must be a data frame")
  }
  
  data <- merged_counts %>%
    dplyr::mutate(
      species_label = paste(
        Microfossil_Type,
        ifelse(is.na(Morphotype) | Morphotype == "", "Unknown_Morphotype", Morphotype),
        ifelse(is.na(Genus_Type) | Genus_Type == "", "spp", Genus_Type),
        ifelse(is.na(Species) | Species == "", "sp", Species),
        ifelse(!is.na(Variety) & Variety != "", paste("var", Variety), ""),
        sep = "_"
      ),
      species_label = stringr::str_replace_all(species_label, "_+", "_"),
      species_label = stringr::str_remove(species_label, "_$")
    ) %>%
    dplyr::filter(!is.na(species_label), species_label != "", Count > 0)
  
  # Handle metadata - either from existing columns or extract from Sample_ID
  if (!is.null(metadata_with_ages)) {
    data <- data %>%
      dplyr::left_join(metadata_with_ages$data, by = "Sample_ID")
  } else {
    # Check if metadata already exists in merged_counts, otherwise extract from Sample_ID
    if (!"sample_type" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(
          sample_type = dplyr::case_when(
            stringr::str_detect(Sample_ID, "LXXXX") ~ "Killifish",
            stringr::str_detect(Sample_ID, "L\\d+") ~ "Stickleback", 
            TRUE ~ "Unknown"
          )
        )
    }
    
    if (!"LSPEC" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(
          LSPEC_raw = stringr::str_extract(Sample_ID, "L\\w+"),
          L_digits = stringr::str_extract(Sample_ID, "(?<=L)\\d+"),
          LSPEC = dplyr::case_when(
            !is.na(L_digits) ~ paste0("L", stringr::str_pad(L_digits, 4, pad = "0")),
            stringr::str_detect(Sample_ID, "LXXXX") ~ "LXXXX",
            TRUE ~ LSPEC_raw
          )
        ) %>%
        dplyr::select(-LSPEC_raw, -L_digits)
    }
    
    if (!"V_number" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(V_number = stringr::str_extract(Sample_ID, "V\\d+"))
    }
    
    if (!all(c("CSTRAT", "YEAR") %in% names(data))) {
      data <- data %>%
        dplyr::mutate(CSTRAT = NA_real_, YEAR = NA_real_)
    }
  }
  
  stickleback <- process_species_group(
    data %>% dplyr::filter(sample_type == "Stickleback"), 
    row_id = "LSPEC", 
    include_age = !is.null(metadata_with_ages)
  )
  
  killifish <- process_species_group(
    data %>% dplyr::filter(sample_type == "Killifish"), 
    row_id = "V_number", 
    include_age = FALSE
  )
  
  all_species <- union(colnames(stickleback$counts), colnames(killifish$counts))
  
  species_summary <- data.frame(
    Species = all_species,
    Stickleback_Total = sapply(all_species, function(x) {
      if(x %in% colnames(stickleback$counts)) sum(stickleback$counts[, x]) else 0
    }),
    Killifish_Total = sapply(all_species, function(x) {
      if(x %in% colnames(killifish$counts)) sum(killifish$counts[, x]) else 0
    }),
    Stickleback_Samples = sapply(all_species, function(x) {
      if(x %in% colnames(stickleback$counts)) sum(stickleback$counts[, x] > 0) else 0
    }),
    Killifish_Samples = sapply(all_species, function(x) {
      if(x %in% colnames(killifish$counts)) sum(killifish$counts[, x] > 0) else 0
    }),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      Total = Stickleback_Total + Killifish_Total,
      Microfossil_Type = stringr::str_extract(Species, "^[^_]+"),
      Genus = stringr::str_extract(Species, "(?<=_)[^_]+(?=_)")
    ) %>%
    dplyr::arrange(dplyr::desc(Total))
  
  return(list(
    stickleback = stickleback,
    killifish = killifish, 
    species_summary = species_summary
  ))
}

#' Process Species Group (Individual Sample Type)
#'
#' @param data Filtered data for one sample type with species labels
#' @param row_id Column to use as row identifier
#' @param include_age Whether to include age/depth data
process_species_group <- function(data, row_id, include_age) {
  wide_data <- data %>%
    dplyr::select(
      dplyr::all_of(row_id), Sample_ID, V_number, sample_type,
      if(include_age) c("CSTRAT", "YEAR") else character(),
      species_label, Count
    ) %>%
    dplyr::group_by(dplyr::across(c(-species_label, -Count))) %>%
    dplyr::summarise(Count = sum(Count), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = species_label, 
      values_from = Count, 
      values_fill = 0
    )
  
  if(!include_age) {
    wide_data <- wide_data %>%
      dplyr::mutate(CSTRAT = NA_real_, YEAR = NA_real_)
  }
  
  species_cols <- stringr::str_subset(colnames(wide_data), "^(Diatom|Phytolith|Charred)_ ")
  counts <- wide_data %>%
    dplyr::select(dplyr::all_of(species_cols)) %>%
    as.data.frame()
  samples <- wide_data %>%
    dplyr::select(-dplyr::all_of(species_cols))
  
  rownames(counts) <- samples[[row_id]]
  
  return(list(counts = counts, samples = samples))
}

#' Export Species-Level Results
#'
#' @param results Output from create_species_matrix()
#' @param prefix Filename prefix
#' @param dir Output directory
#' @return List of exported file paths
export_species_results <- function(results, prefix = "species_level", dir = ".") {
  if (!is.list(results) || !all(c("stickleback", "killifish", "species_summary") %in% names(results))) {
    stop("Input must be output from create_species_matrix()")
  }
  
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  file_paths <- list()
  
  export_matrix <- function(matrix_data, suffix, row_col) {
    filename <- file.path(dir, paste0(prefix, "_", suffix, ".csv"))
    
    combined_data <- matrix_data$counts %>%
      tibble::rownames_to_column(row_col) %>%
      dplyr::left_join(matrix_data$samples, by = setNames(row_col, names(matrix_data$samples)[1]))
    
    write.csv(combined_data, filename, row.names = FALSE)
    return(filename)
  }
  
  file_paths$stickleback <- export_matrix(results$stickleback, "stickleback", "LSPEC")
  file_paths$killifish <- export_matrix(results$killifish, "killifish", "V_number")
  
  file_paths$summary <- file.path(dir, paste0(prefix, "_summary.csv"))
  write.csv(results$species_summary, file_paths$summary, row.names = FALSE)
  
  return(file_paths)
}