#' Calculate Community Ecology Metrics by Time Bin
#'
#' Calculates species richness, evenness, and diversity indices for each
#' stratigraphic level (CSTRAT) using vegan package functions.
#'
#' @param data Data frame with taxa columns and CSTRAT/YEAR metadata
#' @param evenness_index Character string specifying evenness index: "pielou", "simpson", "custom"
#' @param diversity_indices Vector of diversity indices to calculate (default: c("shannon", "simpson"))
#' @return Data frame with community metrics by time bin, preserving LSPEC
#'
#' @examples
#' metrics <- calculate_community_metrics(filtered_data)
#' metrics_simpson <- calculate_community_metrics(filtered_data, evenness_index = "simpson")
calculate_community_metrics <- function(data, 
                                        evenness_index = "pielou",
                                        diversity_indices = c("shannon", "simpson"),
                                        time_column = "YEAR") {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop("Package 'vegan' is required for community metrics")
  }
  
  # Define metadata columns
  metadata_cols <- c("LSPEC", "YEAR", "CSTRAT", "ISTRAT", "INT", "Sample_ID", "V_number", "total_count", "n_taxa")
  available_metadata <- intersect(metadata_cols, names(data))
  taxa_cols <- setdiff(names(data), available_metadata)
  
  if (length(taxa_cols) == 0) {
    stop("No taxa columns found in data")
  }
  if (!time_column %in% available_metadata) {
    # Try fallback options
    fallback_options <- c("YEAR", "CSTRAT")
    available_time <- intersect(fallback_options, available_metadata)
    if (length(available_time) == 0) {
      stop("No time column found. Need YEAR or CSTRAT column for time bin analysis")
    }
    time_column <- available_time[1]
    warning(paste("Requested time column not found, using", time_column))
  }
  
  # Prepare community matrix (samples x species) - no pooling needed since 1:1 relationship
  community_matrix <- data %>%
    dplyr::select(LSPEC, dplyr::all_of(time_column), dplyr::all_of(taxa_cols)) %>%
    dplyr::arrange(.data[[time_column]])
  
  # Verify 1:1 relationship between LSPEC and time_column
  duplicate_check <- community_matrix %>%
    dplyr::count(.data[[time_column]]) %>%
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_check) > 0) {
    stop(paste("Found", nrow(duplicate_check), "time values with multiple samples. Expected 1:1 relationship."))
  }
  
  # Create species matrix for vegan functions
  species_matrix <- community_matrix %>%
    dplyr::select(dplyr::all_of(taxa_cols)) %>%
    as.matrix()
  
  # Use LSPEC as row names for vegan functions
  rownames(species_matrix) <- community_matrix$LSPEC
  
  # Calculate basic richness
  richness <- vegan::specnumber(species_matrix)
  
  # Calculate diversity indices
  diversity_results <- list()
  for (index in diversity_indices) {
    diversity_results[[paste0(index, "_diversity")]] <- vegan::diversity(species_matrix, index = index)
  }
  
  # Calculate evenness based on specified index
  evenness <- calculate_evenness(species_matrix, evenness_index)
  
  # Calculate total abundance per sample
  total_abundance <- rowSums(species_matrix)
  
  # Combine results - preserve LSPEC and time information
  metrics_data <- data.frame(
    LSPEC = community_matrix$LSPEC,
    time_value = community_matrix[[time_column]],
    richness = richness,
    evenness = evenness,
    total_abundance = total_abundance,
    stringsAsFactors = FALSE
  )
  
  # Set proper column name for time
  names(metrics_data)[2] <- time_column
  
  # Add diversity indices
  for (div_name in names(diversity_results)) {
    metrics_data[[div_name]] <- diversity_results[[div_name]]
  }
  
  # Add other metadata columns if available (CSTRAT when using YEAR, or YEAR when using CSTRAT)
  # Only add if they have 1:1 relationship with time_column
  other_metadata <- setdiff(available_metadata, c("LSPEC", time_column))
  if (length(other_metadata) > 0) {
    metadata_to_add <- data %>%
      dplyr::select(LSPEC, dplyr::all_of(other_metadata)) %>%
      dplyr::distinct()
    
    metrics_data <- metrics_data %>%
      dplyr::left_join(metadata_to_add, by = "LSPEC")
  }
  
  # Sort by time order
  metrics_data <- metrics_data %>%
    dplyr::arrange(.data[[time_column]])
  
  # Report results
  cat("Community Metrics Calculated:\n")
  cat("Samples analyzed:", nrow(metrics_data), "\n")
  cat("Richness range:", round(min(richness), 1), "-", round(max(richness), 1), "\n")
  cat("Evenness range:", round(min(evenness, na.rm = TRUE), 3), "-", round(max(evenness, na.rm = TRUE), 3), "\n")
  cat("Shannon diversity range:", round(min(diversity_results$shannon_diversity, na.rm = TRUE), 3), 
      "-", round(max(diversity_results$shannon_diversity, na.rm = TRUE), 3), "\n\n")
  
  return(as.data.frame(metrics_data))
}

#' Calculate Evenness Index
#'
#' @param species_matrix Matrix with samples as rows, species as columns
#' @param evenness_index Type of evenness index to calculate
#' @return Vector of evenness values
calculate_evenness <- function(species_matrix, evenness_index) {
  
  richness <- vegan::specnumber(species_matrix)
  
  evenness_values <- switch(evenness_index,
                            "pielou" = {
                              shannon <- vegan::diversity(species_matrix, index = "shannon")
                              shannon / log(richness)
                            },
                            "simpson" = {
                              simpson <- vegan::diversity(species_matrix, index = "simpson")
                              simpson / (1 - 1/richness)
                            },
                            "custom" = {
                              # Custom evenness calculation - can be modified as needed
                              shannon <- vegan::diversity(species_matrix, index = "shannon")
                              shannon / log(richness)
                            },
                            {
                              stop(paste("Unknown evenness index:", evenness_index))
                            }
  )
  
  # Handle cases where richness = 1 (evenness is undefined)
  evenness_values[richness <= 1] <- NA
  
  return(evenness_values)
}

#' Create Community Metrics Summary
#'
#' @param metrics_data Output from calculate_community_metrics()
#' @return Summary statistics for community metrics
create_metrics_summary <- function(metrics_data) {
  if (!is.data.frame(metrics_data)) {
    stop("Input must be a data frame")
  }
  
  numeric_cols <- sapply(metrics_data, is.numeric)
  numeric_data <- metrics_data[, numeric_cols, drop = FALSE]
  
  summary_stats <- numeric_data %>%
    dplyr::summarise_all(list(
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE)
    )) %>%
    tidyr::pivot_longer(everything(), names_to = "metric_stat", values_to = "value") %>%
    tidyr::separate(metric_stat, into = c("metric", "statistic"), sep = "_(?=[^_]*$)") %>%
    tidyr::pivot_wider(names_from = statistic, values_from = value) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3)))
  
  return(summary_stats)
}