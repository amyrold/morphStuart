#' Summarize Morphology-Field Order Integration
#'
#' Creates comprehensive summary of the morphology-fieldorder integration process
#' including linkage statistics, flagged specimens, and data quality metrics.
#'
#' @param morph_final Original morphology data before integration
#' @param morph_with_depths Integrated morphology data with depths
#' @return List with integration summary statistics
#'
#' @examples
#' summary <- summarize_morph_integration(morph_final, morph_with_depths)
summarize_morph_integration <- function(morph_final, morph_with_depths) {
  
  # Calculate basic integration statistics
  original_fish <- length(unique(morph_final$fish_id))
  original_lspecs <- length(unique(morph_final$LSPEC[!is.na(morph_final$LSPEC)]))
  
  integrated_fish <- length(unique(morph_with_depths$fish_id))
  integrated_lspecs <- length(unique(morph_with_depths$LSPEC))
  
  # Calculate flagged specimens from file if it exists
  flagged_specimens <- 0
  flagged_lspecs <- 0
  if (file.exists("data/flagged/morph_missing_depths.csv")) {
    flagged_data <- read.csv("data/flagged/morph_missing_depths.csv")
    flagged_specimens <- length(unique(flagged_data$fish_id))
    flagged_lspecs <- length(unique(flagged_data$LSPEC[!is.na(flagged_data$LSPEC)]))
  }
  
  # Calculate data availability statistics
  if (nrow(morph_with_depths) > 0) {
    age_available <- sum(!is.na(morph_with_depths$YEAR))
    depth_available <- sum(!is.na(morph_with_depths$CSTRAT))
    both_available <- sum(!is.na(morph_with_depths$YEAR) & !is.na(morph_with_depths$CSTRAT))
    
    # Calculate ranges
    age_range <- if (age_available > 0) {
      range(morph_with_depths$YEAR, na.rm = TRUE)
    } else {
      c(NA, NA)
    }
    
    depth_range <- if (depth_available > 0) {
      range(morph_with_depths$CSTRAT, na.rm = TRUE)
    } else {
      c(NA, NA)
    }
    
    completeness_pct <- list(
      age = round(100 * age_available / nrow(morph_with_depths), 1),
      depth = round(100 * depth_available / nrow(morph_with_depths), 1),
      both = round(100 * both_available / nrow(morph_with_depths), 1)
    )
  } else {
    age_available <- 0
    depth_available <- 0
    both_available <- 0
    age_range <- c(NA, NA)
    depth_range <- c(NA, NA)
    completeness_pct <- list(age = 0, depth = 0, both = 0)
  }
  
  # Create summary object
  summary <- list(
    integration_info = list(
      integration_date = Sys.time(),
      pipeline_version = "2.0_streamlined"
    ),
    specimen_counts = list(
      original_fish = original_fish,
      integrated_fish = integrated_fish,
      flagged_fish = flagged_specimens,
      integration_rate = round(100 * integrated_fish / original_fish, 1),
      loss_rate = round(100 * flagged_specimens / original_fish, 1)
    ),
    lspec_counts = list(
      original_lspecs = original_lspecs,
      integrated_lspecs = integrated_lspecs,
      flagged_lspecs = flagged_lspecs,
      lspec_linkage_rate = round(100 * integrated_lspecs / original_lspecs, 1)
    ),
    data_quality = list(
      specimens_with_age = age_available,
      specimens_with_depth = depth_available,
      specimens_with_both = both_available,
      completeness_percentages = completeness_pct
    ),
    temporal_coverage = list(
      age_range = age_range,
      depth_range = depth_range,
      n_with_age = age_available,
      n_with_depth = depth_available
    ),
    files_created = list(
      flagged_specimens = if (flagged_specimens > 0) "data/flagged/morph_missing_depths.csv" else NULL
    )
  )
  
  # Print summary to console
  cat("Morphology-Field Order Integration Summary:\n")
  cat("==========================================\n")
  cat("Fish specimens:\n")
  cat("  Original:", original_fish, "\n")
  cat("  Successfully integrated:", integrated_fish, "\n")
  cat("  Flagged (missing depths):", flagged_specimens, "\n")
  cat("  Integration rate:", summary$specimen_counts$integration_rate, "%\n\n")
  
  cat("LSPEC linkage:\n")
  cat("  Original LSPECs:", original_lspecs, "\n")
  cat("  Linked LSPECs:", integrated_lspecs, "\n")
  cat("  Linkage rate:", summary$lspec_counts$lspec_linkage_rate, "%\n\n")
  
  cat("Data availability:\n")
  cat("  Age data (YEAR):", age_available, "specimens (", completeness_pct$age, "%)\n")
  cat("  Depth data (CSTRAT):", depth_available, "specimens (", completeness_pct$depth, "%)\n")
  cat("  Both age and depth:", both_available, "specimens (", completeness_pct$both, "%)\n\n")
  
  if (!is.na(age_range[1])) {
    cat("Age range:", age_range[1], "-", age_range[2], "years\n")
  }
  if (!is.na(depth_range[1])) {
    cat("Depth range:", depth_range[1], "-", depth_range[2], "cm\n")
  }
  
  if (flagged_specimens > 0) {
    cat("\nFlagged specimens saved to: data/flagged/morph_missing_depths.csv\n")
  }
  
  cat("\n")
  
  return(summary)
}