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
               "vegan", "rioja", "readr"),
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
    command = format_fieldorder(order_raw),
    description = "Format and standardize field order data with LSPEC IDs"
  ),
  
  tar_target(
    name = fieldorder_quality_summary,
    command = get_fieldorder_quality_summary(fieldorder_formatted),
    description = "Data quality metrics for formatted field order data"
  ),
  
  tar_target(
    name = fieldorder_duplicate_analysis,
    command = identify_fieldorder_duplicates(fieldorder_formatted),
    description = "Analysis of duplicate LSPECs and potential conflicts"
  ),
  
  tar_target(
    name = fieldorder_lspec_classification,
    command = classify_lspecs(fieldorder_duplicate_analysis, fieldorder_formatted),
    description = "Classification of LSPECs into problematic vs clean categories"
  ),
  
  tar_target(
    name = fieldorder_clean,
    command = clean_fieldorder(fieldorder_formatted, fieldorder_lspec_classification$problematic_lspecs),
    description = "Clean field order dataset with conflicts removed and duplicates merged"
  ),
  
  tar_target(
    name = fieldorder_missing_data,
    command = identify_missing_data_records(fieldorder_clean),
    description = "Records with missing key data (but no conflicts)"
  ),
  
  tar_target(
    name = fieldorder_flagged,
    command = flag_fieldorder(fieldorder_formatted, fieldorder_duplicate_analysis, fieldorder_missing_data),
    description = "Comprehensive flagged dataset for manual review"
  ),
  
  tar_target(
    name = fieldorder_processing_summary,
    command = generate_fieldorder_summary(fieldorder_formatted, fieldorder_clean, fieldorder_flagged, "PitLMorph_fieldorder.csv"),
    description = "Complete processing summary with statistics and quality metrics"
  ),
  
  # ========================================================================= #
  # REPORT SUMMARIES & VISUALIZATIONS ----
  # ========================================================================= #
  
  # Essential processing summaries for report
  tar_target(
    name = report_morphology_summary,
    command = morphology_processing_summary(morph_raw, morph_with_ids, morph_with_scales, 
                                            morph_non_overlap, morph_final),
    description = "Morphology processing pipeline summary table"
  ),
  
  tar_target(
    name = report_paleo_summary,
    command = paleo_processing_summary(paleo_raw, paleo_merged_counts),
    description = "Paleo-ecology processing summary table"
  ),
  
  tar_target(
    name = report_fieldorder_summary,
    command = fieldorder_summary_table(fieldorder_processing_summary),
    description = "Field order processing summary table"
  ),
  
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
  
  tar_target(
    name = plot_fieldorder_completeness,
    command = fieldorder_completeness_plot(fieldorder_completeness_table(fieldorder_processing_summary)),
    description = "Field order completeness plot"
  ),
  
  tar_target(
    name = plot_stratigraphic_distribution,
    command = stratigraphic_distribution_plot(fieldorder_clean),
    description = "Stratigraphic level distribution plot"
  ),
  
  tar_target(
    name = plot_age_distribution,
    command = age_distribution_plot(fieldorder_clean),
    description = "Age distribution plot"
  ),
  
  tar_target(
    name = plot_age_depth,
    command = age_depth_relationship_plot(fieldorder_clean),
    description = "Age-depth relationship plot"
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
  
  tar_target(
    name = file_fieldorder_completeness_plot,
    command = save_plot(plot_fieldorder_completeness, "results/plots/fieldorder_completeness.png", width = 10, height = 6),
    format = "file",
    description = "Field order completeness plot file"
  ),
  
  tar_target(
    name = file_stratigraphic_plot,
    command = save_plot(plot_stratigraphic_distribution, "results/plots/stratigraphic_distribution.png", width = 10, height = 6),
    format = "file",
    description = "Stratigraphic distribution plot file"
  ),
  
  tar_target(
    name = file_age_distribution_plot,
    command = save_plot(plot_age_distribution, "results/plots/age_distribution.png", width = 10, height = 6),
    format = "file",
    description = "Age distribution plot file"
  ),
  
  tar_target(
    name = file_age_depth_plot,
    command = save_plot(plot_age_depth, "results/plots/age_depth_relationship.png", width = 10, height = 6),
    format = "file",
    description = "Age-depth relationship plot file"
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