#' Filter Rare Taxa Below Occurrence Threshold
#'
#' Removes taxa that occur in fewer than a specified percentage of samples.
#' This helps focus analysis on common taxa and reduces noise from rare
#' occurrences that may be due to contamination or preservation artifacts.
#'
#' @param data Data frame or matrix with taxa columns and (optionally) metadata
#' @param threshold Numeric value between 0 and 1 representing minimum occurrence rate (default 0.10 = 10%)
#' @return Data frame or matrix (matching input type) with rare taxa removed and updated summary statistics
#'
#' @examples
#' # Remove taxa occurring in <10% of samples
#' filtered_data <- filter_rare_taxa(aggregated_taxa)
#' 
#' # Use stricter threshold (5%)
#' filtered_strict <- filter_rare_taxa(aggregated_taxa, threshold = 0.05)
filter_rare_taxa <- function(data, threshold = 0.10) {
  if (threshold < 0 || threshold > 1) {
    stop("Threshold must be between 0 and 1")
  }
  
  is_matrix_input <- is.matrix(data)
  
  if (is_matrix_input) {
    # Convert matrix to data frame for dplyr operations
    data_df <- as.data.frame(data)
    # Store row names (LSPEC) if they exist
    if (!is.null(rownames(data))) {
      data_df$LSPEC_rownames_temp <- rownames(data)
    }
  } else if (is.data.frame(data)) {
    data_df <- data
  } else {
    stop("Input must be a data frame or a matrix")
  }
  
  # Define metadata columns to preserve
  # LSPEC_rownames_temp is a temporary column for matrix input row names
  metadata_cols <- c("LSPEC", "YEAR", "CSTRAT", "ISTRAT", "INT", "Sample_ID", "V_number", "total_count", "n_taxa", "LSPEC_rownames_temp")
  available_metadata <- intersect(metadata_cols, names(data_df))
  
  # Get taxa columns (everything that's not metadata)
  taxa_cols <- setdiff(names(data_df), available_metadata)
  
  if (length(taxa_cols) == 0) {
    stop("No taxa columns found in data")
  }
  
  n_samples <- nrow(data_df)
  min_occurrences <- ceiling(n_samples * threshold)
  
  # Calculate occurrence rates for each taxon
  occurrence_rates <- data_df %>%
    select(all_of(taxa_cols)) %>%
    summarise_all(~sum(. > 0)) %>%
    pivot_longer(everything(), names_to = "taxon", values_to = "occurrences") %>%
    mutate(
      occurrence_rate = occurrences / n_samples,
      meets_threshold = occurrence_rate >= threshold
    ) %>%
    arrange(desc(occurrence_rate))
  
  # Identify taxa to keep
  taxa_to_keep <- occurrence_rates %>%
    filter(meets_threshold) %>%
    pull(taxon)
  
  taxa_to_remove <- occurrence_rates %>%
    filter(!meets_threshold) %>%
    pull(taxon)
  
  # Filter the data
  filtered_data_df <- data_df %>%
    select(all_of(c(available_metadata, taxa_to_keep)))
  
  # Recalculate summary statistics if columns exist
  if ("total_count" %in% available_metadata && length(taxa_to_keep) > 0) {
    filtered_data_df <- filtered_data_df %>%
      mutate(total_count = rowSums(select(., all_of(taxa_to_keep))))
  }
  
  if ("n_taxa" %in% available_metadata && length(taxa_to_keep) > 0) {
    filtered_data_df <- filtered_data_df %>%
      mutate(n_taxa = rowSums(select(., all_of(taxa_to_keep)) > 0))
  }
  
  # Save details about removed taxa
  if (length(taxa_to_remove) > 0) {
    if (!dir.exists("data/processed")) {
      dir.create("data/processed", recursive = TRUE)
    }
    
    removed_taxa_details <- occurrence_rates %>%
      filter(!meets_threshold) %>%
      arrange(desc(occurrence_rate))
    
    write.csv(removed_taxa_details, "data/processed/removed_rare_taxa.csv", row.names = FALSE)
  }
  
  # Report filtering results
  cat("Rare Taxa Filtering Results:\n")
  cat("Threshold:", paste0(round(threshold * 100, 1), "%"), "(minimum", min_occurrences, "occurrences)\n")
  cat("Original taxa:", length(taxa_cols), "\n")
  cat("Taxa retained:", length(taxa_to_keep), "\n")
  cat("Taxa removed:", length(taxa_to_remove), "\n")
  cat("Retention rate:", round(100 * length(taxa_to_keep) / length(taxa_cols), 1), "%\n")
  
  if (length(taxa_to_remove) > 0) {
    cat("Removed taxa details saved to: data/processed/removed_rare_taxa.csv\n")
  }
  cat("\n")
  
  # Convert back to matrix if input was matrix
  if (is_matrix_input) {
    row_names <- NULL
    if ("LSPEC_rownames_temp" %in% names(filtered_data_df)) {
      row_names <- filtered_data_df$LSPEC_rownames_temp
      filtered_data_df <- select(filtered_data_df, -LSPEC_rownames_temp)
    }
    # Ensure only numeric columns are converted to matrix
    numeric_cols <- sapply(filtered_data_df, is.numeric)
    filtered_matrix <- as.matrix(filtered_data_df[, numeric_cols, drop = FALSE])
    if (!is.null(row_names)) {
      rownames(filtered_matrix) <- row_names
    }
    return(filtered_matrix)
  } else {
    return(filtered_data_df)
  }
}

#' Get Rare Taxa Filtering Summary
#'
#' Creates a summary of taxa occurrence rates for diagnostic purposes
#'
#' @param data Data frame with taxa columns
#' @param threshold Occurrence threshold for comparison
#' @return Data frame with occurrence statistics for all taxa
#'
#' @examples
#' occurrence_summary <- get_taxa_occurrence_summary(data, threshold = 0.10)
get_taxa_occurrence_summary <- function(data, threshold = 0.10) {
  # Define metadata columns
  metadata_cols <- c("LSPEC", "YEAR", "CSTRAT", "ISTRAT", "INT", "Sample_ID", "V_number", "total_count", "n_taxa")
  available_metadata <- intersect(metadata_cols, names(data))
  taxa_cols <- setdiff(names(data), available_metadata)
  
  n_samples <- nrow(data)
  
  occurrence_summary <- data %>%
    select(all_of(taxa_cols)) %>%
    summarise_all(list(
      occurrences = ~sum(. > 0),
      total_abundance = ~sum(., na.rm = TRUE),
      max_abundance = ~max(., na.rm = TRUE),
      mean_when_present = ~mean(.[. > 0], na.rm = TRUE)
    )) %>%
    pivot_longer(everything(), names_to = "taxon_stat", values_to = "value") %>%
    separate(taxon_stat, into = c("taxon", "statistic"), sep = "_(?=[^_]*$)") %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    mutate(
      occurrence_rate = occurrences / n_samples,
      meets_threshold = occurrence_rate >= threshold,
      rarity_category = case_when(
        occurrence_rate >= 0.5 ~ "Common (≥50%)",
        occurrence_rate >= 0.25 ~ "Frequent (25-50%)",
        occurrence_rate >= 0.10 ~ "Occasional (10-25%)",
        occurrence_rate >= 0.05 ~ "Rare (5-10%)",
        TRUE ~ "Very rare (<5%)"
      )
    ) %>%
    arrange(desc(occurrence_rate))
  
  return(occurrence_summary)
}