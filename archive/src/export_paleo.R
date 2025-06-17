#' Export Paleo Data for Jacopo's Ecological Review
#'
#' Creates standardized export files matching Jacopo's requirements:
#' 1. Summary table (taxa as columns, samples as rows)
#' 2. Taxa list for ecological grouping
#' 3. Sample linking information for morphology integration
#'
#' @param rioja_data Output from create_rioja_matrix()
#' @param output_prefix Prefix for output filenames
#' @param output_dir Output directory (optional)
#' @return List of created file paths
#'
#' @examples
#' files <- export_for_jacopo(rioja_data, "diatom_analysis", "output/paleo/")
export_for_jacopo <- function(rioja_data, output_prefix = "paleo_analysis", 
                              output_dir = NULL) {
  # Validate input
  if (!is.list(rioja_data) || !all(c("counts", "samples") %in% names(rioja_data))) {
    stop("Input must be output from create_rioja_matrix()")
  }
  
  cat("=== EXPORTING FOR JACOPO'S ECOLOGICAL REVIEW ===\n")
  
  # Create output directory if specified
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  file_paths <- list()
  
  # 1. Main summary table for Jacopo (taxa as columns, samples as rows)
  summary_table <- rioja_data$counts %>%
    tibble::rownames_to_column("sample_label") %>%
    left_join(
      rioja_data$samples %>% 
        select(sample_label, Sample_ID, V_number, sample_type, LSPEC, CSTRAT, YEAR, age_quality),
      by = "sample_label"
    ) %>%
    select(Sample_ID, V_number, LSPEC, sample_type, CSTRAT, YEAR, age_quality, 
           everything(), -sample_label) %>%
    arrange(desc(CSTRAT))  # Sort by stratigraphic depth
  
  file_paths$summary_table <- file.path(output_dir %||% ".", 
                                        paste0(output_prefix, "_summary_table.csv"))
  write.csv(summary_table, file_paths$summary_table, row.names = FALSE)
  
  # 2. Taxa list for Jacopo's ecological grouping
  taxa_for_review <- data.frame(
    Taxon = names(rioja_data$counts),
    total_abundance = colSums(rioja_data$counts),
    n_samples = colSums(rioja_data$counts > 0),
    max_abundance = apply(rioja_data$counts, 2, max),
    percent_of_total = round(100 * colSums(rioja_data$counts) / sum(rioja_data$counts), 2)
  ) %>%
    arrange(desc(total_abundance)) %>%
    mutate(
      rank = row_number(),
      cumulative_percent = round(cumsum(percent_of_total), 1),
      ecological_group = ""  # Empty column for Jacopo to fill
    )
  
  file_paths$taxa_list <- file.path(output_dir %||% ".", 
                                    paste0(output_prefix, "_taxa_for_grouping.csv"))
  write.csv(taxa_for_review, file_paths$taxa_list, row.names = FALSE)
  
  # 3. Sample linking information for morphology integration
  linking_info <- rioja_data$samples %>%
    select(Sample_ID, V_number, LSPEC, sample_type, CSTRAT, YEAR, age_quality, sample_label) %>%
    arrange(V_number) %>%
    mutate(
      notes = case_when(
        is.na(V_number) ~ "No V-number for morphology linking",
        age_quality == "No age data" ~ "Missing age/depth data",
        sample_type == "Killifish" ~ "Post-stickleback environment",
        TRUE ~ ""
      )
    )
  
  file_paths$sample_info <- file.path(output_dir %||% ".", 
                                      paste0(output_prefix, "_sample_linking.csv"))
  write.csv(linking_info, file_paths$sample_info, row.names = FALSE)
  
  # 4. Processing metadata for reproducibility
  processing_metadata <- list(
    microfossil_type = rioja_data$microfossil_type,
    n_samples = nrow(rioja_data$samples),
    n_taxa = ncol(rioja_data$counts),
    filtering_criteria = rioja_data$filtering_criteria,
    has_age_integration = rioja_data$has_age_data,
    export_timestamp = Sys.time(),
    r_version = R.version.string
  )
  
  file_paths$metadata <- file.path(output_dir %||% ".", 
                                   paste0(output_prefix, "_processing_metadata.txt"))
  
  # Write metadata as readable text
  cat(file = file_paths$metadata,
      "PALEO-ECOLOGICAL DATA PROCESSING METADATA\n",
      "=========================================\n\n",
      "Microfossil type:", processing_metadata$microfossil_type, "\n",
      "Final matrix dimensions:", processing_metadata$n_samples, "samples ×", 
      processing_metadata$n_taxa, "taxa\n",
      "Age integration:", if(processing_metadata$has_age_integration) "Yes" else "No", "\n",
      "Min total count threshold:", processing_metadata$filtering_criteria$min_total_count, "\n",
      "Min samples threshold:", processing_metadata$filtering_criteria$min_samples, "\n",
      "Killifish included:", processing_metadata$filtering_criteria$include_killifish, "\n",
      "Export timestamp:", format(processing_metadata$export_timestamp, "%Y-%m-%d %H:%M:%S"), "\n",
      "R version:", processing_metadata$r_version, "\n")
  
  # Summary output
  cat("Files exported for Jacopo:\n")
  cat("1. Summary table (main data):", basename(file_paths$summary_table), "\n")
  cat("2. Taxa list (for grouping):", basename(file_paths$taxa_list), "\n") 
  cat("3. Sample linking info:", basename(file_paths$sample_info), "\n")
  cat("4. Processing metadata:", basename(file_paths$metadata), "\n")
  
  return(file_paths)
}
