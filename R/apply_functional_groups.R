#' Apply Functional Groupings Using Wildcard Patterns
#'
#' Groups microfossil taxa into functional ecological groups using wildcard
#' pattern matching for full taxonomic flexibility.
#'
#' @param paleo_merged_counts_raw Output from merge_microscopy_counts()
#' @param config Project configuration list (from yaml::read_yaml())
#' @return Data frame with functional groups replacing individual taxa
apply_functional_groupings <- function(paleo_merged_counts_raw, config) {
  if (!is.data.frame(paleo_merged_counts_raw)) {
    stop("Input must be a data frame")
  }
  if (!is.list(config) || !"functional_groupings" %in% names(config)) {
    stop("Config must contain 'functional_groupings' section")
  }
  
  required_cols <- c("Sample_ID", "Microfossil_Type", "Morphotype", "Count")
  missing_cols <- setdiff(required_cols, names(paleo_merged_counts_raw))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate config structure
  validate_functional_config_patterns(config$functional_groupings)
  
  # Add missing taxonomic columns if needed
  data_with_taxonomy <- paleo_merged_counts_raw %>%
    dplyr::mutate(
      Genus_Type = ifelse(is.na(Genus_Type) | Genus_Type == "", "spp", Genus_Type),
      Species = ifelse(is.na(Species) | Species == "", "sp", Species),
      Variety = ifelse(is.na(Variety) | Variety == "", "", Variety)
    )
  
  # Step 1: Apply exclusions
  data_filtered <- apply_exclusions_patterns(data_with_taxonomy, config$functional_groupings$exclude_taxa)
  
  # Step 2: Apply functional groupings
  functional_groups_assigned <- assign_groups_patterns(
    data_filtered$Microfossil_Type, 
    data_filtered$Morphotype, 
    data_filtered$Genus_Type, 
    data_filtered$Species, 
    data_filtered$Variety,
    config$functional_groupings$groups
  )

  data_with_functional_groups <- data_filtered %>%
    dplyr::mutate(functional_group = functional_groups_assigned) %>%
    dplyr::filter(!is.na(functional_group), Count > 0)

  # Save unassigned patterns for review
  if (sum(is.na(functional_groups_assigned)) > 0) {
    unassigned_indices <- is.na(functional_groups_assigned)
    save_unassigned_patterns(
      data_filtered$Microfossil_Type[unassigned_indices],
      data_filtered$Morphotype[unassigned_indices],
      data_filtered$Genus_Type[unassigned_indices],
      data_filtered$Species[unassigned_indices],
      data_filtered$Variety[unassigned_indices],
      config$paths$unassigned_functional_group
    )
  }
  
  # Step 3: Aggregate by functional group
  functional_aggregated <- data_with_functional_groups %>%
    dplyr::group_by(
      Sample_ID, LSPEC, V_number, sample_type, functional_group
    ) %>%
    dplyr::summarise(
      Count = sum(Count, na.rm = TRUE),
      n_original_taxa = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(Microfossil_Type = functional_group) %>%
    dplyr::mutate(
      Morphotype = "functional_group",
      Genus_Type = NA_character_,
      Species = NA_character_,
      Variety = NA_character_
    )
  
  # Summary report
  cat("Functional Grouping Summary:\n")
  cat("Records processed:", nrow(paleo_merged_counts_raw), "→", nrow(functional_aggregated), "\n")
  cat("Functional groups created:", length(unique(functional_aggregated$Microfossil_Type)), "\n\n")
  
  return(as.data.frame(functional_aggregated))
}

#' Apply Exclusions Using Pattern Matching
apply_exclusions_patterns <- function(data, exclusion_config) {
  if (is.null(exclusion_config) || length(exclusion_config) == 0) {
    return(data)
  }
  
  exclude_flags <- rep(FALSE, nrow(data))
  total_excluded <- 0
  
  for (i in seq_along(exclusion_config)) {
    exclusion_def <- exclusion_config[[i]]
    
    if (is.null(exclusion_def$microfossil_type) || is.null(exclusion_def$patterns)) {
      warning(paste("Skipping malformed exclusion definition", i))
      next
    }
    
    # Check microfossil type
    target_type <- as.character(exclusion_def$microfossil_type)[1]
    type_match <- data$Microfossil_Type == target_type
    type_match[is.na(type_match)] <- FALSE
    
    if (sum(type_match) == 0) next
    
    # Apply patterns
    pattern_match <- rep(FALSE, nrow(data))
    for (pattern in exclusion_def$patterns) {
      matches <- match_taxonomic_pattern(
        data$Morphotype, data$Genus_Type, data$Species, data$Variety, pattern
      )
      pattern_match <- pattern_match | matches
    }
    
    # Apply exclusion
    exclusion_match <- type_match & pattern_match
    exclude_flags <- exclude_flags | exclusion_match
    total_excluded <- total_excluded + sum(exclusion_match)
  }
  
  if (total_excluded > 0) {
    cat("Excluded", total_excluded, "records\n")
  }
  
  return(data[!exclude_flags, ])
}

#' Assign Functional Groups Using Pattern Matching
assign_groups_patterns <- function(microfossil_type, morphotype, genus_type, species, variety, group_config) {
  
  assigned_group <- rep(NA_character_, length(microfossil_type))
  
  for (group_name in names(group_config)) {
    group_def <- group_config[[group_name]]
    
    if (is.null(group_def$microfossil_type) || is.null(group_def$patterns)) {
      warning(paste("Skipping malformed group definition:", group_name))
      next
    }
    
    # Check microfossil type
    target_type <- as.character(group_def$microfossil_type)[1]
    type_match <- microfossil_type == target_type
    type_match[is.na(type_match)] <- FALSE
    
    if (sum(type_match) == 0) next
    
    # Apply patterns
    pattern_match <- rep(FALSE, length(microfossil_type))
    for (pattern in group_def$patterns) {
      matches <- match_taxonomic_pattern(morphotype, genus_type, species, variety, pattern)
      pattern_match <- pattern_match | matches
    }
    
    # Assign group
    group_assignment <- type_match & pattern_match
    assigned_group[group_assignment] <- group_name
  }
  
  # Report unassigned patterns if any
  unassigned_count <- sum(is.na(assigned_group))
  if (unassigned_count > 0) {
    unassigned_patterns <- generate_unassigned_patterns(
      microfossil_type[is.na(assigned_group)],
      morphotype[is.na(assigned_group)], 
      genus_type[is.na(assigned_group)],
      species[is.na(assigned_group)],
      variety[is.na(assigned_group)]
    )
    
    cat("Unassigned patterns (", unassigned_count, " records):\n", sep = "")
    for (mtype in names(unassigned_patterns)) {
      patterns <- unassigned_patterns[[mtype]]
      n_show <- min(length(patterns), 5)
      cat("  ", mtype, ": ", paste(patterns[1:n_show], collapse = ", "), 
          ifelse(length(patterns) > 5, paste(" (", length(patterns) - 5, " more)", sep = ""), ""), "\n", sep = "")
    }
  }
  
  return(assigned_group)
}

#' Generate Unassigned Patterns
generate_unassigned_patterns <- function(microfossil_type, morphotype, genus_type, species, variety) {
  
  unassigned_data <- data.frame(
    microfossil_type = microfossil_type,
    morphotype = ifelse(is.na(morphotype) | morphotype == "", "*", as.character(morphotype)),
    genus_type = ifelse(is.na(genus_type) | genus_type == "", "*", as.character(genus_type)),
    species = ifelse(is.na(species) | species == "", "*", as.character(species)),
    variety = ifelse(is.na(variety) | variety == "", "*", as.character(variety)),
    stringsAsFactors = FALSE
  )
  
  unassigned_data$pattern <- paste(
    unassigned_data$morphotype, unassigned_data$genus_type,
    unassigned_data$species, unassigned_data$variety, sep = "."
  )
  
  unique_patterns <- unassigned_data %>%
    dplyr::group_by(microfossil_type) %>%
    dplyr::summarise(patterns = list(unique(pattern)), .groups = "drop")
  
  result <- list()
  for (i in 1:nrow(unique_patterns)) {
    mtype <- unique_patterns$microfossil_type[i]
    patterns <- unique_patterns$patterns[[i]]
    result[[mtype]] <- sort(patterns)
  }
  
  return(result)
}

#' Save Unassigned Patterns to File
save_unassigned_patterns <- function(microfossil_type, morphotype, genus_type, species, variety, 
                                   filename) {
  
  if (!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  
  unassigned_data <- data.frame(
    microfossil_type = microfossil_type,
    morphotype = morphotype,
    genus_type = genus_type,
    species = species,
    variety = variety,
    pattern = paste(
      ifelse(is.na(morphotype) | morphotype == "", "*", morphotype),
      ifelse(is.na(genus_type) | genus_type == "", "*", genus_type),
      ifelse(is.na(species) | species == "", "*", species),
      ifelse(is.na(variety) | variety == "", "*", variety),
      sep = "."
    ),
    stringsAsFactors = FALSE
  )
  
  unassigned_summary <- unassigned_data %>%
    dplyr::group_by(microfossil_type, pattern, morphotype, genus_type, species, variety) %>%
    dplyr::summarise(n_records = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(microfossil_type, dplyr::desc(n_records))
  
  write.csv(unassigned_summary, filename, row.names = FALSE)
  cat("Unassigned patterns saved to:", filename, "\n")
  
  return(unassigned_summary)
}

#' Match Taxonomic Pattern with Wildcards
match_taxonomic_pattern <- function(morphotype, genus_type, species, variety, pattern) {
  
  pattern_parts <- strsplit(pattern, "\\.")[[1]]
  
  if (length(pattern_parts) != 4) {
    warning(paste("Invalid pattern format:", pattern))
    return(rep(FALSE, length(morphotype)))
  }
  
  # Clean input vectors
  morphotype_clean <- ifelse(is.na(morphotype) | morphotype == "", "", as.character(morphotype))
  genus_type_clean <- ifelse(is.na(genus_type) | genus_type == "", "", as.character(genus_type))
  species_clean <- ifelse(is.na(species) | species == "", "", as.character(species))
  variety_clean <- ifelse(is.na(variety) | variety == "", "", as.character(variety))
  
  # Match components
  match_morphotype <- (pattern_parts[1] == "*") | (morphotype_clean == pattern_parts[1])
  match_genus <- (pattern_parts[2] == "*") | (genus_type_clean == pattern_parts[2])
  match_species <- (pattern_parts[3] == "*") | (species_clean == pattern_parts[3])
  match_variety <- (pattern_parts[4] == "*") | (variety_clean == pattern_parts[4])
  
  return(match_morphotype & match_genus & match_species & match_variety)
}

#' Validate Functional Groupings Configuration
validate_functional_config_patterns <- function(fg_config) {
  if (!"groups" %in% names(fg_config)) {
    stop("functional_groupings must contain 'groups' section")
  }
  
  # Validate exclusions
  if ("exclude_taxa" %in% names(fg_config) && !is.null(fg_config$exclude_taxa)) {
    for (i in seq_along(fg_config$exclude_taxa)) {
      validate_pattern_definition(fg_config$exclude_taxa[[i]], paste("exclusion", i))
    }
  }
  
  # Validate groups
  for (group_name in names(fg_config$groups)) {
    validate_pattern_definition(fg_config$groups[[group_name]], group_name)
  }
  
  return(TRUE)
}

#' Validate Pattern Definition
validate_pattern_definition <- function(def_config, def_name) {
  
  if (!"microfossil_type" %in% names(def_config)) {
    stop(paste("Definition", def_name, "must have 'microfossil_type'"))
  }
  
  if (!"patterns" %in% names(def_config)) {
    stop(paste("Definition", def_name, "must have 'patterns'"))
  }
  
  patterns <- def_config$patterns
  if (!is.character(patterns) || length(patterns) == 0) {
    stop(paste("Definition", def_name, "patterns must be non-empty character vector"))
  }
  
  for (i in seq_along(patterns)) {
    pattern_parts <- strsplit(patterns[i], "\\.")[[1]]
    if (length(pattern_parts) != 4) {
      stop(paste("Definition", def_name, "pattern", i, "must have 4 parts:", patterns[i]))
    }
  }
}