#' Create Morphotype Matrix Using Split-Process-Combine Approach (Streamlined)
#'
#' @param merged_counts Output from merge_microscopy_counts()
#' @param metadata_with_ages Output from integrate_age_data()
#' @return List with stickleback, killifish, and combined results
create_morphotype_matrix_split <- function(merged_counts, metadata_with_ages) {
  
  # Aggregate and add metadata
  data <- merged_counts %>%
    group_by(Sample_ID, Microfossil_Type, Morphotype) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    filter(!is.na(Morphotype), Morphotype != "") %>%
    left_join(metadata_with_ages$data, by = "Sample_ID") %>%
    mutate(morphotype_label = paste(Microfossil_Type, Morphotype, sep = "_"))
  
  # Process each group
  stickleback <- process_group(data %>% filter(sample_type == "Stickleback"), 
                               row_id = "LSPEC", include_age = TRUE)
  killifish <- process_group(data %>% filter(sample_type == "Killifish"), 
                             row_id = "V_number", include_age = FALSE)
  
  # Combined summary - using base R instead of purrr
  all_morphotypes <- union(colnames(stickleback$counts), colnames(killifish$counts))
  
  # Calculate totals using sapply instead of map_dbl
  stickleback_totals <- sapply(all_morphotypes, function(x) {
    if(x %in% colnames(stickleback$counts)) sum(stickleback$counts[, x]) else 0
  })
  
  killifish_totals <- sapply(all_morphotypes, function(x) {
    if(x %in% colnames(killifish$counts)) sum(killifish$counts[, x]) else 0
  })
  
  stickleback_samples <- sapply(all_morphotypes, function(x) {
    if(x %in% colnames(stickleback$counts)) sum(stickleback$counts[, x] > 0) else 0
  })
  
  killifish_samples <- sapply(all_morphotypes, function(x) {
    if(x %in% colnames(killifish$counts)) sum(killifish$counts[, x] > 0) else 0
  })
  
  morphotype_summary <- data.frame(
    Morphotype = all_morphotypes,
    Stickleback_Total = stickleback_totals,
    Killifish_Total = killifish_totals,
    Stickleback_Samples = stickleback_samples,
    Killifish_Samples = killifish_samples,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Total = Stickleback_Total + Killifish_Total,
      Microfossil_Type = ifelse(grepl("^Diatom", Morphotype), "Diatom", "Phytolith")
    ) %>%
    arrange(desc(Total))
  
  return(list(
    stickleback = stickleback,
    killifish = killifish,
    morphotype_summary = morphotype_summary
  ))
}

#' Process Individual Sample Group (Streamlined)
#'
#' @param data Filtered data for one sample type
#' @param row_id Column to use as row identifier 
#' @param include_age Whether to include age/depth data
process_group <- function(data, row_id, include_age) {
  
  # Create matrix
  wide_data <- data %>%
    select(all_of(row_id), Sample_ID, V_number, sample_type, 
           if(include_age) c("CSTRAT", "YEAR") else character(), 
           morphotype_label, Count) %>%
    pivot_wider(names_from = morphotype_label, values_from = Count, values_fill = 0)
  
  if(!include_age) wide_data <- wide_data %>% mutate(CSTRAT = NA_real_, YEAR = NA_real_)
  
  # Split into counts and samples
  count_cols <- str_subset(colnames(wide_data), "^(Diatom|Phytolith)_")
  counts <- wide_data %>% select(all_of(count_cols)) %>% as.data.frame()
  samples <- wide_data %>% select(-all_of(count_cols))
  
  rownames(counts) <- samples[[row_id]]
  
  return(list(counts = counts, samples = samples))
}

#' Export Results (Streamlined)
#'
#' @param results Output from create_morphotype_matrix_split()
#' @param prefix Filename prefix
#' @param dir Output directory
export_morphotype_results <- function(results, prefix = "morphotype", dir = ".") {
  
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  # Export matrices with metadata
  export_matrix <- function(matrix_data, suffix, row_col) {
    filename <- file.path(dir, paste0(prefix, "_", suffix, ".csv"))
    
    matrix_data$counts %>%
      rownames_to_column(row_col) %>%
      left_join(matrix_data$samples, by = setNames(row_col, names(matrix_data$samples)[1])) %>%
      write_csv(filename)
    
    return(filename)
  }
  
  files <- list(
    stickleback = export_matrix(results$stickleback, "stickleback", "LSPEC"),
    killifish = export_matrix(results$killifish, "killifish", "V_number"),
    summary = write_csv(results$morphotype_summary, 
                        file.path(dir, paste0(prefix, "_summary.csv")))
  )
  
  return(files)
}

#' Export Paleo Results for Analysis and Review
#'
#' Creates standardized export files for paleo-ecological analysis matching
#' the pipeline's modular approach. Exports three key files:
#' 1. Summary table with taxa as columns and samples as rows (main analysis file)
#' 2. Taxa list ranked by abundance for ecological grouping review
#' 3. Sample metadata for linking with morphology and age data
#'
#' @param rioja_data Output from create_rioja_matrix() containing counts matrix and sample info
#' @param output_prefix Character prefix for output filenames (default "paleo_analysis")
#' @param output_dir Optional output directory path (default current directory)
#' @return List of created file paths for confirmation and logging
#'
#' @examples
#' files <- export_paleo_results(rioja_data, "diatom_analysis", "output/paleo/")
#' exported_files <- export_paleo_results(rioja_diatoms, 
#'                                        output_prefix = "stickleback_diatoms",
#'                                        output_dir = p$`1.data/b.merged`)
export_paleo_results <- function(rioja_data, output_prefix = "paleo_analysis", 
                                 output_dir = NULL) {
  # Validate input
  if (!is.list(rioja_data) || !all(c("counts", "samples") %in% names(rioja_data))) {
    stop("Input must be output from create_rioja_matrix()")
  }
  
  cat("=== EXPORTING PALEO RESULTS ===\n")
  
  # Set default output directory
  if (is.null(output_dir)) {
    output_dir <- "."
  }
  
  # Create output directory if specified and doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  file_paths <- list()
  
  # 1. Main summary table for analysis (taxa as columns, samples as rows)
  # Use LSPEC as the row identifier and include all essential sample info
  summary_table <- rioja_data$counts %>%
    tibble::rownames_to_column("LSPEC") %>%
    left_join(
      rioja_data$samples %>% 
        select(LSPEC, Sample_ID, V_number, sample_type, CSTRAT, YEAR),
      by = "LSPEC"
    ) %>%
    select(LSPEC, Sample_ID, V_number, sample_type, CSTRAT, YEAR, everything()) %>%
    arrange(desc(CSTRAT))  # Sort by stratigraphic depth (deepest first)
  
  file_paths$summary_table <- file.path(output_dir, 
                                        paste0(output_prefix, "_summary_table.csv"))
  write.csv(summary_table, file_paths$summary_table, row.names = FALSE)
  
  # 2. Taxa list ranked by abundance for ecological review
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
      cumulative_percent = round(cumsum(percent_of_total), 1)
    )
  
  file_paths$taxa_list <- file.path(output_dir, 
                                    paste0(output_prefix, "_taxa_for_grouping.csv"))
  write.csv(taxa_for_review, file_paths$taxa_list, row.names = FALSE)
  
  # 3. Sample metadata for linking and reference
  sample_info <- rioja_data$samples %>%
    arrange(LSPEC) %>%
    mutate(
      notes = case_when(
        is.na(V_number) ~ "No V-number for morphology linking",
        is.na(CSTRAT) & is.na(YEAR) ~ "Missing age/depth data",
        sample_type == "Killifish" ~ "Post-stickleback environment",
        TRUE ~ ""
      )
    )
  
  file_paths$sample_info <- file.path(output_dir, 
                                      paste0(output_prefix, "_sample_info.csv"))
  write.csv(sample_info, file_paths$sample_info, row.names = FALSE)
  
  # 4. Processing metadata for reproducibility
  processing_metadata <- list(
    microfossil_type = rioja_data$microfossil_type,
    n_samples = nrow(rioja_data$samples),
    n_taxa = ncol(rioja_data$counts),
    samples_with_cstrat = sum(!is.na(rioja_data$samples$CSTRAT)),
    samples_with_year = sum(!is.na(rioja_data$samples$YEAR)),
    killifish_samples = sum(rioja_data$samples$sample_type == "Killifish", na.rm = TRUE),
    stickleback_samples = sum(rioja_data$samples$sample_type == "Stickleback", na.rm = TRUE),
    filtering_criteria = rioja_data$filtering_criteria,
    export_timestamp = Sys.time(),
    r_version = R.version.string
  )
  
  file_paths$metadata <- file.path(output_dir, 
                                   paste0(output_prefix, "_processing_metadata.txt"))
  
  # Write metadata as readable text
  cat(file = file_paths$metadata,
      "PALEO-ECOLOGICAL DATA PROCESSING METADATA\n",
      "=========================================\n\n",
      "Microfossil type:", processing_metadata$microfossil_type, "\n",
      "Final matrix dimensions:", processing_metadata$n_samples, "samples ×", 
      processing_metadata$n_taxa, "taxa\n",
      "Samples with CSTRAT depth:", processing_metadata$samples_with_cstrat, "\n",
      "Samples with YEAR age:", processing_metadata$samples_with_year, "\n",
      "Killifish samples:", processing_metadata$killifish_samples, "\n",
      "Stickleback samples:", processing_metadata$stickleback_samples, "\n",
      "Min total count threshold:", processing_metadata$filtering_criteria$min_total_count, "\n",
      "Min samples threshold:", processing_metadata$filtering_criteria$min_samples, "\n",
      "Killifish included:", processing_metadata$filtering_criteria$include_killifish, "\n",
      "Export timestamp:", format(processing_metadata$export_timestamp, "%Y-%m-%d %H:%M:%S"), "\n",
      "R version:", processing_metadata$r_version, "\n")
  
  # Summary output
  cat("Files exported:\n")
  cat("1. Summary table (main data):", basename(file_paths$summary_table), "\n")
  cat("2. Taxa list (for grouping):", basename(file_paths$taxa_list), "\n")
  cat("3. Sample metadata:", basename(file_paths$sample_info), "\n")
  cat("4. Processing metadata:", basename(file_paths$metadata), "\n")
  
  return(file_paths)
}