# _targets.R
# ========================================================================= #
# STICKLEBACK MORPHOLOGY & PALEO-ECOLOGY ANALYSIS PIPELINE
# ========================================================================= #
# 
# This pipeline processes stickleback morphological measurements and 
# paleo-ecological microfossil data for evolutionary analysis.
#
# PIPELINE OVERVIEW:
# 1. DATA IMPORT: Load raw morphology, paleo, and field order data
# 2. MORPHOLOGY PROCESSING: Clean IDs, handle quality issues, merge P/CP data
# 3. PALEO PROCESSING: Extract metadata, merge microscopy counts, integrate ages
# 4. ANALYSIS OUTPUTS: Create rioja matrices, diversity metrics, export results
#
# USAGE:
# - Run entire pipeline: tar_make()
# - View pipeline: tar_visnetwork()
# - Check status: tar_progress()
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("dplyr", "tidyr", "stringr", "tibble", "purrr", "ggplot2", "scales",
               "vegan", "rioja", "readr", "knitr", "DT"),
  format = "rds",
  error = "continue"
)

tar_source()

list(
  
  # ========================================================================= #
  # DATA IMPORT ----
  # ========================================================================= #
  
  tar_target(
    name = morph_file,
    command = "data/raw/020625_PitLMorph.csv",
    format = "file",
    description = "Raw morphological measurements CSV file"
  ), tar_target(name = morph_raw, command = read.csv(morph_file)),
  
  tar_target(
    name = paleo_file,
    command = "data/raw/060625_paleoeco_seriesL.csv", 
    format = "file",
    description = "Raw paleo-ecological microfossil counts CSV file"
  ), tar_target(name = paleo_raw, command = read.csv(paleo_file)),
  
  tar_target(
    name = order_file,
    command = "data/raw/PitLMorph_fieldorder.csv",
    format = "file", 
    description = "Field order reference data CSV file"
  ), tar_target(name = order_raw, command = read.csv(order_file)),
  
  # ========================================================================= #
  # EXTRACT STANDARDIZED IDs ----
  # ========================================================================= #
  
  tar_target(morph_with_ids, extract_ids(morph_raw)),
  tar_target(paleo_with_ids, extract_ids(paleo_raw)),  
  tar_target(order_with_ids, extract_ids(order_raw)),  
  
  # ========================================================================= #
  # MORPHOLOGY PROCESSING PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = flagged_fish_missing_scales,
    command = flag_missing_scales(morph_with_ids),
    description = "Vector of fish_id values missing 10mm scale bar information"
  ),
  
  tar_target(
    name = updated_scales_file,
    command = "data/raw/Results_missingscales.txt",
    format = "file",
    description = "Updated scale measurements from colleague"
  ),
  
  tar_target(
    name = morph_with_scales, 
    command = handle_missing_scales(morph_with_ids, flagged_fish_missing_scales, updated_scales_file),
    description = "Morphology data with scale issues resolved (updated or filtered)"
  ),
  
  tar_target(
    name = morph_corrected,
    command = evolved_loss(morph_with_scales),
    description = "Apply biological logic to distinguish true zeros from missing data"
  ),
  
  tar_target(
    name = morph_non_overlap,
    command = flag_counterpart_conflicts(morph_corrected, threshold = 0.05),
    description = "Flag and remove fish with conflicting part/counterpart measurements"
  ),
  
  tar_target(
    name = morph_final,
    command = merge_counter_parts(morph_non_overlap),
    description = "Merge non-conflicting part/counterpart data into single records"
  ),
  
  # ========================================================================= #
  # PALEO-ECOLOGICAL PROCESSING PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = paleo_merged_counts,
    command = merge_microscopy_counts(paleo_with_ids),
    description = "Merge 4-5 microscopy line counts per geological sample"
  ),
  
  # ========================================================================= #
  # FIELD ORDER PROCESSING PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = fieldorder_formatted,
    command = format_fieldorder(order_with_ids),
    description = "Format and standardize field order data with LSPEC IDs"
  ),
  
  tar_target(
    name = fieldorder_duplicates_processed,
    command = process_fieldorder_duplicates(fieldorder_formatted),
    description = "Remove conflicting duplicates, merge clean duplicates"
  ),
  
  tar_target(
    name = fieldorder_final,
    command = handle_fieldorder_missing_data(fieldorder_duplicates_processed),
    description = "Separate complete records from those with missing data"
  ),
  
  tar_target(
    name = fieldorder_processing_summary,
    command = summarize_fieldorder_processing(
      fieldorder_formatted, 
      fieldorder_duplicates_processed, 
      fieldorder_final,
      "PitLMorph_fieldorder.csv"
    ),
    description = "Complete processing summary with statistics and quality metrics"
  ),
  
  # ========================================================================= #
  # REPORT SUMMARIES & VISUALIZATIONS ----
  # ========================================================================= #
  
  # Essential data quality summaries
  tar_target(
    name = report_completeness_data,
    command = data_completeness_summary(morph_final),
    description = "Data completeness summary by variable"
  ),
  
  # Essential visualizations for report
  tar_target(
    name = plot_completeness,
    command = completeness_plot(report_completeness_data),
    description = "Data completeness visualization"
  ),
  
  tar_target(
    name = plot_microfossil_types,
    command = microfossil_type_plot(microfossil_type_summary(paleo_merged_counts)),
    description = "Microfossil type distribution plot"
  ),
  
  # Plot file outputs for key visualizations
  tar_target(
    name = file_completeness_plot,
    command = save_plot(plot_completeness, "results/plots/completeness.png", width = 10, height = 6),
    format = "file",
    description = "Data completeness plot file"
  ),
  
  tar_target(
    name = file_microfossil_plot,
    command = save_plot(plot_microfossil_types, "results/plots/microfossil_types.png", width = 10, height = 6),
    format = "file",
    description = "Microfossil types plot file"
  ),
  
  # ========================================================================= #
  # REPORT GENERATION ----
  # ========================================================================= #
  
  tar_render(
    name = report,
    path = "results/report/report.Rmd",
    output_dir = "results/report"
  )
)