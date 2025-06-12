#' Create rioja-Compatible Data Matrix
#'
#' Converts processed paleo data to wide format matrix suitable for rioja
#' stratigraphic analysis. Final step after all preprocessing is complete.
#'
#' @param merged_counts Data frame from merge_microscopy_counts()
#' @param metadata_with_ages Output from integrate_age_data()
#' @param min_total_count Minimum total abundance to include taxon (default 0)
#' @param min_samples Minimum number of samples to include taxon (default 1)
#' @param include_killifish Whether to include post-stickleback samples (default TRUE)
#' @return List containing rioja-ready counts matrix and sample information
#'
#' @examples
#' rioja_data <- create_rioja_matrix(merged_counts, age_result, 
#'                                   min_total_count = 5, min_samples = 2)
create_rioja_matrix <- function(merged_counts, metadata_with_ages, 
                                min_total_count = 0, min_samples = 1,
                                include_killifish = TRUE) {
  # Validate inputs
  if (!is.data.frame(merged_counts)) {
    stop("merged_counts must be output from merge_microscopy_counts()")
  }
  if (!is.list(metadata_with_ages) || !"data" %in% names(metadata_with_ages)) {
    stop("metadata_with_ages must be output from integrate_age_data()")
  }
  
  cat("=== CREATING RIOJA MATRIX ===\n")
  cat("Filtering criteria - Min total count:", min_total_count, 
      ", Min samples:", min_samples, "\n")
  
  # Filter samples based on inclusion criteria
  samples_to_include <- metadata_with_ages$data
  if (!include_killifish) {
    samples_to_include <- samples_to_include %>%
      filter(sample_type == "Stickleback")
    cat("Excluding killifish samples (post-stickleback environment)\n")
  } else {
    cat("Including all sample types\n")
  }
  
  # Create standardized taxon names inline
  counts_with_taxa <- merged_counts %>%
    mutate(
      # Simple taxonomic hierarchy: use Morphotype if available, otherwise Genus_Type
      Taxon = case_when(
        !is.na(Morphotype) & Morphotype != "" ~ Morphotype,
        !is.na(Genus_Type) & Genus_Type != "" ~ Genus_Type,
        !is.na(Species) & Species != "" ~ paste("sp.", Species),
        TRUE ~ "Unknown"
      )
    ) %>%
    filter(Taxon != "Unknown")  # Remove unidentifiable taxa
  
  # Calculate taxa filtering criteria
  taxa_summary <- counts_with_taxa %>%
    group_by(Taxon) %>%
    summarise(
      total_count = sum(Count),
      n_samples = n_distinct(Sample_ID),
      .groups = "drop"
    )
  
  # Filter taxa based on abundance criteria
  taxa_to_include <- taxa_summary %>%
    filter(total_count >= min_total_count, n_samples >= min_samples) %>%
    pull(Taxon)
  
  cat("Taxa meeting inclusion criteria:", length(taxa_to_include), "\n")
  
  if (length(taxa_to_include) == 0) {
    stop("No taxa meet the filtering criteria. Consider lowering thresholds.")
  }
  
  # Join data and convert to wide format
  rioja_wide <- counts_with_taxa %>%
    filter(Taxon %in% taxa_to_include) %>%
    inner_join(samples_to_include, by = "Sample_ID") %>%
    # Final aggregation step to handle any remaining duplicates
    group_by(Sample_ID, Taxon, V_number, LSPEC, sample_type, CSTRAT, YEAR) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    # Convert to wide format (samples as rows, taxa as columns)
    pivot_wider(
      names_from = Taxon,
      values_from = Count,
      values_fill = 0
    ) %>%
    # Sort by stratigraphic position using CSTRAT when available
    arrange(desc(CSTRAT))
  
  # Separate sample metadata from count matrix
  sample_info <- rioja_wide %>%
    select(Sample_ID, V_number, LSPEC, sample_type, CSTRAT, YEAR)
  
  count_matrix <- rioja_wide %>%
    select(all_of(taxa_to_include)) %>%
    as.data.frame()
  
  # Set row names using LSPEC as sample identifier
  rownames(count_matrix) <- sample_info$LSPEC
  
  # Calculate matrix statistics
  total_specimens <- sum(count_matrix)
  avg_richness <- mean(rowSums(count_matrix > 0))
  
  cat("Final rioja matrix created:\n")
  cat("Dimensions:", nrow(count_matrix), "samples ×", ncol(count_matrix), "taxa\n")
  cat("Total specimens:", total_specimens, "\n")
  cat("Average richness per sample:", round(avg_richness, 1), "taxa\n")
  
  return(list(
    counts = count_matrix,
    samples = sample_info,
    taxa_included = taxa_to_include,
    has_age_data = metadata_with_ages$has_age_data,
    microfossil_type = if(length(unique(merged_counts$Microfossil_Type)) == 1) {
      unique(merged_counts$Microfossil_Type)[1]
    } else {
      "Multiple"
    },
    filtering_criteria = list(
      min_total_count = min_total_count,
      min_samples = min_samples,
      include_killifish = include_killifish
    )
  ))
}