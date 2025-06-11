# =============================================================================
# PALEO-ECOLOGICAL DATA CLEANING FUNCTIONS
# =============================================================================
# 
# This script contains functions for processing paleo-ecological microfossil data
# (diatoms, phytoliths) for stratigraphic analysis and integration with fish 
# morphology data. The pipeline handles:
# 1. Sample metadata extraction and classification (killifish vs stickleback)
# 2. Merging multiple microscopy line counts per geological sample
# 3. Taxonomic standardization (morphotype-level per Jacopo's specifications)
# 4. Age/depth integration from morphology dataset
# 5. rioja-compatible matrix creation for stratigraphic analysis
# 6. Export functions for ecological review and analysis
#
# Author: Aaron Myrold
# Dependencies: dplyr, tidyr, stringr, vegan (for diversity calculations)
# Collaborators: Jacopo (ecological expertise), Mike Bell (age estimates)
# =============================================================================

# Required libraries
required_packages <- c("dplyr", "tidyr", "stringr", "ggplot2")

for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    warning(paste("Package", pkg, "not available. Some functions may not work."))
  }
}

# Optional packages (loaded when needed)
if (!require("vegan", quietly = TRUE)) {
  warning("vegan package not available. Diversity calculations will not work.")
}

# =============================================================================
# SAMPLE METADATA AND CLASSIFICATION
# =============================================================================

#' Extract and Classify Paleo Sample Metadata
#'
#' Extracts sample metadata from Sample_ID strings and classifies samples as
#' killifish (post-stickleback, LXXXX) or stickleback (L + digits). Creates
#' standardized LSPEC formatting to match morphology data for age integration.
#'
#' @param paleo Data frame containing paleo-ecological data with Sample_ID column
#' @return Data frame with extracted metadata columns:
#'   - V_number: Specimen identifier for linking with morphology data
#'   - LSPEC: Standardized format (L + 4-digit) for age/depth lookup
#'   - sample_type: "Killifish" or "Stickleback" classification
#'
#' @examples
#' metadata <- extract_paleo_metadata(raw_paleo_data)
#' table(metadata$sample_type)  # Check killifish vs stickleback distribution
extract_paleo_metadata <- function(paleo) {
  # Validate input
  if (!is.data.frame(paleo)) {
    stop("Input must be a data frame")
  }
  if (!"Sample_ID" %in% names(paleo)) {
    stop("Data must contain 'Sample_ID' column")
  }
  
  cat("=== EXTRACTING PALEO METADATA ===\n")
  
  paleo_with_metadata <- paleo %>%
    distinct(Sample_ID) %>%
    mutate(
      # Extract V-number for linking with morph data
      V_number = str_extract(Sample_ID, "V\\d+"),
      
      # Extract L-number (field excavation number) and format as LSPEC
      L_number_raw = str_extract(Sample_ID, "L\\w+"),
      L_digits = str_extract(Sample_ID, "(?<=L)\\d+"),
      
      # Create standardized LSPEC format to match morph data
      # This is critical for age/depth integration
      LSPEC = case_when(
        !is.na(L_digits) ~ paste0("L", str_pad(L_digits, 4, pad = "0")),
        str_detect(Sample_ID, "LXXXX") ~ "LXXXX",  # Killifish samples
        TRUE ~ L_number_raw
      ),
      
      # Classify sample type based on Jacopo's specifications
      sample_type = case_when(
        str_detect(Sample_ID, "LXXXX") ~ "Killifish",      # Post-stickleback environment
        str_detect(Sample_ID, "L\\d+") ~ "Stickleback",    # Original stickleback period
        TRUE ~ "Unknown"
      )
    ) %>%
    select(Sample_ID, V_number, LSPEC, sample_type)
  
  # Summary output
  cat("Extracted metadata for", nrow(paleo_with_metadata), "unique samples\n")
  cat("Sample type breakdown:\n")
  type_breakdown <- table(paleo_with_metadata$sample_type)
  print(type_breakdown)
  
  # Check for potential issues
  unknown_samples <- sum(paleo_with_metadata$sample_type == "Unknown")
  if (unknown_samples > 0) {
    cat("WARNING:", unknown_samples, "samples with unknown type\n")
  }
  
  missing_v_numbers <- sum(is.na(paleo_with_metadata$V_number))
  if (missing_v_numbers > 0) {
    cat("NOTE:", missing_v_numbers, "samples without V-numbers\n")
  }
  
  return(paleo_with_metadata)
}

# =============================================================================
# MICROSCOPY COUNT MERGING
# =============================================================================

#' Merge Multiple Microscopy Line Counts per Sample
#'
#' CRITICAL FUNCTION: Handles Jacopo's requirement to merge "four to five separate 
#' counts" per geological sample. Each row in raw data represents one microscopy 
#' line; this function aggregates them into actual geological sample counts.
#' This step is essential before any downstream analysis.
#'
#' @param paleo Raw paleo data frame with individual microscopy line counts
#' @param microfossil_type Type of microfossil to process (default NULL = all types)
#' @return Data frame with merged counts per sample + taxonomic identity
#'
#' @examples
#' # Process all microfossil types (default)
#' merged_counts <- merge_microscopy_counts(raw_paleo)
#' 
#' # Process only diatoms
#' merged_diatoms <- merge_microscopy_counts(raw_paleo, "Diatom")
#' 
#' # Process only phytoliths  
#' merged_phytoliths <- merge_microscopy_counts(raw_paleo, "Phytolith")
merge_microscopy_counts <- function(paleo, microfossil_type = NULL) {
  # Validate inputs
  if (!is.data.frame(paleo)) {
    stop("Input must be a data frame")
  }
  required_cols <- c("Sample_ID", "Microfossil_Type", "Count")
  missing_cols <- setdiff(required_cols, names(paleo))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Determine which microfossil types to process
  available_types <- unique(paleo$Microfossil_Type)
  
  if (is.null(microfossil_type)) {
    # Process all types
    types_to_process <- available_types
    cat("=== MERGING MICROSCOPY LINE COUNTS ===\n")
    cat("Processing all microfossil types:", paste(types_to_process, collapse = ", "), "\n")
  } else {
    # Process specified type(s)
    if (!all(microfossil_type %in% available_types)) {
      missing_types <- setdiff(microfossil_type, available_types)
      stop(paste("Microfossil type(s) not found in data:", paste(missing_types, collapse = ", ")))
    }
    types_to_process <- microfossil_type
    cat("=== MERGING MICROSCOPY LINE COUNTS ===\n")
    cat("Processing specified microfossil type(s):", paste(types_to_process, collapse = ", "), "\n")
  }
  
  # Filter and merge counts by sample + complete taxonomic identity
  # This is the critical step that was missing from original analysis
  merged_counts <- paleo %>%
    filter(Microfossil_Type %in% types_to_process) %>%
    group_by(Sample_ID, Microfossil_Type, Morphotype, Genus_Type, Species, Variety) %>%
    summarise(
      Count = sum(Count, na.rm = TRUE),
      n_lines_merged = n(),  # Track how many lines were merged
      .groups = "drop"
    ) %>%
    filter(Count > 0)  # Remove zero counts after merging
  
  # Calculate merging statistics
  original_rows <- nrow(paleo %>% filter(Microfossil_Type %in% types_to_process))
  final_rows <- nrow(merged_counts)
  rows_merged <- original_rows - final_rows
  
  cat("Original microscopy lines:", original_rows, "\n")
  cat("After merging geological samples:", final_rows, "\n")
  cat("Lines merged:", rows_merged, "(", round(100 * rows_merged / original_rows, 1), "%)\n")
  
  # Show examples of successful merging by microfossil type
  merge_examples <- merged_counts %>%
    filter(n_lines_merged > 1) %>%
    arrange(Microfossil_Type, desc(n_lines_merged)) %>%
    group_by(Microfossil_Type) %>%
    slice_head(n = 3) %>%
    ungroup()
  
  if (nrow(merge_examples) > 0) {
    cat("\nExamples of merged counts by type:\n")
    print(merge_examples %>% 
            select(Sample_ID, Microfossil_Type, Morphotype, Count, n_lines_merged) %>%
            mutate(Sample_ID = str_trunc(Sample_ID, 20)))
  }
  
  # Check for potential issues
  max_lines_merged <- max(merged_counts$n_lines_merged, na.rm = TRUE)
  if (max_lines_merged > 10) {
    cat("WARNING: Some samples had >10 microscopy lines. Check for data issues.\n")
  }
  
  # Summary by microfossil type
  type_summary <- merged_counts %>%
    group_by(Microfossil_Type) %>%
    summarise(
      samples = n_distinct(Sample_ID),
      taxa = n_distinct(paste(Morphotype, Genus_Type, Species)),
      total_specimens = sum(Count),
      .groups = "drop"
    )
  
  cat("\nSummary by microfossil type:\n")
  print(type_summary)
  
  return(merged_counts)
}


# =============================================================================
# AGE AND DEPTH INTEGRATION
# =============================================================================

#' Integrate Age and Depth Data from Morphology Dataset
#'
#' Links paleo samples to actual stratigraphic depths (CSTRAT) and ages (YEAR)
#' using Mike Bell's estimates via LSPEC matching. Adds age/depth information
#' to existing sample metadata without creating additional ordering columns.
#'
#' @param paleo_metadata Output from extract_paleo_metadata()
#' @param morph_with_age Morphology dataset containing age/depth estimates
#' @return List with age-integrated data and integration summary
#'
#' @examples
#' age_result <- integrate_age_data(metadata, morph_with_age)
#' samples_with_ages <- age_result$data
integrate_age_data <- function(paleo_metadata, morph_with_age) {
  # Validate inputs
  if (!is.data.frame(paleo_metadata) || !is.data.frame(morph_with_age)) {
    stop("Both inputs must be data frames")
  }
  if (!"LSPEC" %in% names(paleo_metadata)) {
    stop("paleo_metadata must contain 'LSPEC' column from extract_paleo_metadata()")
  }
  if (!any(c("CSTRAT", "YEAR") %in% names(morph_with_age))) {
    stop("morph_with_age must contain 'CSTRAT' and/or 'YEAR' columns")
  }
  
  cat("=== INTEGRATING AGE/DEPTH DATA ===\n")
  cat("Using Mike Bell's age estimates from morphology dataset\n")
  
  # Extract age/depth reference data by LSPEC
  # Average multiple specimens from same stratigraphic level
  age_reference <- morph_with_age %>%
    filter(!is.na(LSPEC), !is.na(CSTRAT) | !is.na(YEAR)) %>%
    group_by(LSPEC) %>%
    summarise(
      CSTRAT = mean(CSTRAT, na.rm = TRUE),
      YEAR = mean(YEAR, na.rm = TRUE),
      n_morph_specimens = n(),
      age_range_years = if_else(n() > 1, max(YEAR, na.rm = TRUE) - min(YEAR, na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    # Clean infinite values from averaging
    mutate(
      CSTRAT = if_else(is.infinite(CSTRAT), NA_real_, CSTRAT),
      YEAR = if_else(is.infinite(YEAR), NA_real_, YEAR)
    )
  
  # Join paleo metadata with age data
  paleo_with_ages <- paleo_metadata %>%
    left_join(age_reference, by = "LSPEC", suffix = c("", "_morph"))
  
  # Calculate integration statistics
  integration_summary <- paleo_with_ages %>%
    summarise(
      total_samples = n(),
      samples_with_cstrat = sum(!is.na(CSTRAT), na.rm = TRUE),
      samples_with_year = sum(!is.na(YEAR), na.rm = TRUE),
      killifish_samples = sum(sample_type == "Killifish", na.rm = TRUE),
      stickleback_samples = sum(sample_type == "Stickleback", na.rm = TRUE),
      percent_with_cstrat = round(100 * sum(!is.na(CSTRAT), na.rm = TRUE) / n(), 1)
    )
  
  cat("Age integration results:\n")
  cat("Total paleo samples:", integration_summary$total_samples, "\n")
  cat("Samples with CSTRAT depth:", integration_summary$samples_with_cstrat, 
      "(", integration_summary$percent_with_cstrat, "%)\n")
  cat("Samples with YEAR age:", integration_summary$samples_with_year, "\n")
  
  # Check for potential linking issues
  missing_age_LSPECs <- paleo_with_ages %>%
    filter(is.na(CSTRAT), sample_type == "Stickleback") %>%
    select(LSPEC) %>%
    distinct()
  
  if (nrow(missing_age_LSPECs) > 0) {
    cat("WARNING:", nrow(missing_age_LSPECs), "stickleback LSPECs without age data\n")
    cat("Missing LSPECs:", paste(missing_age_LSPECs$LSPEC, collapse = ", "), "\n")
  }
  
  return(list(
    data = paleo_with_ages,
    age_reference = age_reference,
    integration_summary = integration_summary,
    has_age_data = integration_summary$samples_with_cstrat > 0
  ))
}

# =============================================================================
# RIOJA MATRIX CREATION
# =============================================================================

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

# =============================================================================
# DIVERSITY CALCULATIONS
# =============================================================================

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

# =============================================================================
# EXPORT FUNCTIONS
# =============================================================================

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

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

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

# =============================================================================
# END OF PALEO-ECOLOGICAL CLEANING FUNCTIONS
# =============================================================================

cat("✓ Paleo-ecological cleaning functions loaded successfully\n")
cat("Available functions: extract_paleo_metadata, merge_microscopy_counts,\n")
cat("                     standardize_taxonomy, integrate_age_data,\n")
cat("                     create_rioja_matrix, calculate_diversity_metrics,\n")
cat("                     export_for_jacopo, print_paleo_summary,\n")
cat("                     validate_paleo_data, export_paleo_results\n")