#' Calculate Ecological Diversity Metrics
#'
#' Computes standard diversity indices for stratigraphic analysis.
#' Requires vegan package for Shannon and Simpson calculations.
#'
#' @param rioja_data Output from create_rioja_matrix()
#' @return Data frame with diversity metrics per sample
#'
#' @examples
#' diversity_data <- calculate_diversity_metrics(rioja_data)
#' plot(diversity_data$CSTRAT, diversity_data$shannon)
calculate_diversity_metrics <- function(rioja_data) {
  # Check for vegan package
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop("vegan package is required for diversity calculations. Please install it.")
  }
  
  cat("=== CALCULATING DIVERSITY METRICS ===\n")
  
  # Validate input
  if (!is.list(rioja_data) || !all(c("counts", "samples") %in% names(rioja_data))) {
    stop("Input must be output from create_rioja_matrix()")
  }
  
  counts <- rioja_data$counts
  samples <- rioja_data$samples
  
  # Calculate diversity indices
  diversity_data <- data.frame(
    Sample_ID = samples$Sample_ID,
    sample_type = samples$sample_type,
    strat_level = samples$strat_level,
    CSTRAT = samples$CSTRAT,
    YEAR = samples$YEAR,
    
    # Basic richness and abundance
    richness = apply(counts > 0, 1, sum),        # Number of taxa present
    total_count = rowSums(counts),               # Total specimens counted
    
    # Diversity indices (using explicit vegan:: calls)
    shannon = vegan::diversity(counts, index = "shannon"),
    simpson = vegan::diversity(counts, index = "simpson"),
    
    # Evenness (Shannon / log(richness))
    evenness = vegan::diversity(counts, index = "shannon") / log(apply(counts > 0, 1, sum))
  ) %>%
    # Clean up infinite/NaN values from evenness calculation
    mutate(
      evenness = if_else(is.infinite(evenness) | is.nan(evenness), NA_real_, evenness)
    ) %>%
    # Sort by stratigraphic position
    arrange(desc(strat_level))
  
  # Summary statistics
  diversity_summary <- diversity_data %>%
    summarise(
      n_samples = n(),
      richness_range = paste(min(richness), "-", max(richness)),
      shannon_range = paste(round(min(shannon, na.rm = TRUE), 2), "-", 
                            round(max(shannon, na.rm = TRUE), 2)),
      avg_evenness = round(mean(evenness, na.rm = TRUE), 2)
    )
  
  cat("Diversity metrics calculated for", nrow(diversity_data), "samples\n")
  cat("Richness range:", diversity_summary$richness_range, "taxa\n")
  cat("Shannon diversity range:", diversity_summary$shannon_range, "\n")
  cat("Average evenness:", diversity_summary$avg_evenness, "\n")
  
  return(diversity_data)
}