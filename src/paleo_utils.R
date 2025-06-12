#' Print Processing Step Summary
#'
#' Consistent summary output for paleo processing steps, compatible with
#' existing logging system.
#'
#' @param step_name Name of processing step
#' @param input_info Input data description
#' @param output_info Output data description  
#' @param additional_info Additional notes
#'
#' @examples
#' print_paleo_summary("Microscopy Merging", "5000 lines", "1200 samples")
print_paleo_summary <- function(step_name, input_info = NULL, output_info = NULL, 
                                additional_info = NULL) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("PALEO STEP:", step_name, "\n")
  if (!is.null(input_info)) cat("Input: ", input_info, "\n")
  if (!is.null(output_info)) cat("Output:", output_info, "\n")
  if (!is.null(additional_info)) cat("Info:  ", additional_info, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
}

#' Validate Paleo Data Processing Pipeline
#'
#' Comprehensive validation function matching the style used in morphology pipeline
#' Checks data preservation, quality, and integration across processing steps.
#'
#' @param original_paleo Original raw paleo data
#' @param rioja_data Final rioja-format data from create_rioja_matrix()
#' @param processing_steps List containing intermediate processing outputs
#' @return List of validation results and statistics
validate_paleo_data <- function(original_paleo, rioja_data, processing_steps) {
  cat("=== VALIDATING PALEO DATA PROCESSING ===\n")
  
  validation_results <- list()
  
  # Check sample preservation through pipeline
  original_samples <- length(unique(original_paleo$Sample_ID))
  final_samples <- nrow(rioja_data$samples)
  
  validation_results$sample_preservation <- list(
    original_samples = original_samples,
    final_samples = final_samples,
    samples_retained = final_samples / original_samples,
    preservation_rate = round(100 * final_samples / original_samples, 1)
  )
  
  # Check count preservation through pipeline
  microfossil_type <- if(is.null(rioja_data$microfossil_type)) "Diatom" else rioja_data$microfossil_type
  original_total_counts <- sum(original_paleo$Count[original_paleo$Microfossil_Type == microfossil_type], na.rm = TRUE)
  final_total_counts <- sum(rioja_data$counts)
  
  validation_results$count_preservation <- list(
    original_total = original_total_counts,
    final_total = final_total_counts,
    counts_retained = final_total_counts / original_total_counts,
    preservation_rate = round(100 * final_total_counts / original_total_counts, 1)
  )
  
  # Check taxonomic coverage and diversity
  validation_results$taxonomic_coverage <- list(
    taxa_in_matrix = ncol(rioja_data$counts),
    samples_with_age_data = sum(!is.na(rioja_data$samples$CSTRAT)),
    killifish_samples = sum(rioja_data$samples$sample_type == "Killifish", na.rm = TRUE),
    stickleback_samples = sum(rioja_data$samples$sample_type == "Stickleback", na.rm = TRUE),
    age_integration_success = rioja_data$has_age_data
  )
  
  # Data quality checks
  validation_results$quality_checks <- list(
    all_samples_have_data = all(rowSums(rioja_data$counts) > 0),
    all_taxa_have_data = all(colSums(rioja_data$counts) > 0),
    no_negative_counts = all(rioja_data$counts >= 0),
    unique_sample_labels = length(unique(rioja_data$samples$sample_label)) == nrow(rioja_data$samples),
    matrix_dimensions_valid = all(dim(rioja_data$counts) > 0)
  )
  
  # Processing step validation (if intermediate data provided)
  if (!is.null(processing_steps)) {
    validation_results$processing_steps <- list()
    
    if ("metadata" %in% names(processing_steps)) {
      validation_results$processing_steps$metadata_extraction <- 
        nrow(processing_steps$metadata) == length(unique(original_paleo$Sample_ID))
    }
    
    if ("taxonomy" %in% names(processing_steps)) {
      validation_results$processing_steps$taxonomy_standardization <- 
        "Taxon" %in% names(processing_steps$taxonomy$data)
    }
    
    if ("ages" %in% names(processing_steps)) {
      validation_results$processing_steps$age_integration <- 
        processing_steps$ages$has_age_data
    }
  }
  
  # Summary output
  cat("Validation Summary:\n")
  cat("Sample preservation:", validation_results$sample_preservation$preservation_rate, "%\n")
  cat("Count preservation:", validation_results$count_preservation$preservation_rate, "%\n")
  cat("Taxa in final matrix:", validation_results$taxonomic_coverage$taxa_in_matrix, "\n")
  cat("Samples with age data:", validation_results$taxonomic_coverage$samples_with_age_data, "\n")
  
  # Quality check summary
  quality_passed <- sum(unlist(validation_results$quality_checks))
  quality_total <- length(validation_results$quality_checks)
  cat("Quality checks passed:", quality_passed, "/", quality_total, "\n")
  
  if (quality_passed < quality_total) {
    cat("WARNING: Some quality checks failed. Review validation results.\n")
  }
  
  return(validation_results)
}
