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
               "vegan", "rioja", "readr", "knitr", "DT", "yaml"),
  format = "rds",
  error = "continue"
)

tar_source()

# Load project configuration globally
project_config <- yaml::read_yaml("config.yml")

list(
  
  # ========================================================================= #
  # CONFIGURATION TRACKER ----
  # ========================================================================= #
  # This target exists solely to track changes in config.yml
  # It does not produce a value that is used by other targets.
  tar_target(
    name = config_file_tracker,
    command = "config.yml",
    format = "file",
    description = "Tracks changes in the project configuration file"
  ),

  
  # ========================================================================= #
  # DATA IMPORT ----
  # ========================================================================= #
  
  tar_target(
    name = morph_file,
    command = project_config$paths$raw_morph,
    format = "file",
    description = "Raw morphological measurements CSV file"
  ), tar_target(name = morph_raw, command = read.csv(morph_file)),
  
  tar_target(
    name = paleo_file,
    command = project_config$paths$raw_paleo,
    format = "file", 
    description = "Raw paleo-ecological microfossil counts CSV file"
  ), tar_target(name = paleo_raw, command = read.csv(paleo_file)),
  
  tar_target(
    name = order_file,
    command = project_config$paths$raw_order,
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
    command = project_config$paths$raw_missing_scales,
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
    command = flag_counterpart_conflicts(morph_corrected, threshold = project_config$analysis$conflict_threshold),
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
    name = decimal_lspecs_review,
    command = validate_decimal_lspecs(order_raw),
    description = "Identify all field order records with decimal LSPEC values for colleague review"
  ),
  
  tar_target(
    name = missing_morph_lspecs_review,
    command = validate_missing_morph_lspecs(morph_with_ids, fieldorder_final$complete),
    description = "Identify morph LSPECs missing from field order data"
  ),

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
  # RIOJA INTEGRATION PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = paleo_stickleback_only,
    command = filter_stickleback_samples(paleo_merged_counts),
    description = "Filter paleo data to stickleback samples only (removes killifish LXXXX samples)"
  ),
  
  tar_target(
    name = lspec_integration_check,
    command = validate_lspec_links(paleo_stickleback_only, fieldorder_final$complete),
    description = "Validate which paleo LSPECs can be linked with field order stratigraphic data"
  ),
  
  tar_target(
    name = integrated_paleo_metadata,
    command = merge_paleo_with_fieldorder(paleo_stickleback_only, fieldorder_final$complete),
    description = "Merge stickleback paleo data with field order age/depth information via LSPEC"
  ),
  
  tar_target(
    name = paleo_summary_table,
    command = create_paleo_summary_table(integrated_paleo_metadata),
    description = "Wide-format table with LSPEC rows and individual taxa columns"
  ),
  
  tar_target(
    name = rioja_species_matrix,
    command = prepare_rioja_species_data(integrated_paleo_metadata, grouping_level = project_config$analysis$rioja_grouping_level),
    description = "Species matrix for rioja stratigraphic plots (full taxonomic resolution)"
  ),
  
  tar_target(
    name = rioja_filtered_species_matrix,
    command = filter_rare_taxa(rioja_species_matrix, threshold = project_config$analysis$rioja_rare_taxa_threshold),
    description = "Filtered species matrix for rioja plots, removing rare taxa below threshold"
  ),
  
  tar_target(
    name = rioja_depth_data,
    command = prepare_rioja_depth_data(integrated_paleo_metadata),
    description = "Age/depth vector for rioja stratigraphic analysis (YEAR primary, CSTRAT backup)"
  ),
  
  tar_target(
    name = rioja_strat_plots,
    command = create_rioja_strat_plots(rioja_filtered_species_matrix, rioja_depth_data),
    description = "Rioja stratigraphic plots showing microfossil distributions through time"
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
    command = save_plot(plot_completeness, file.path(project_config$paths$plot_dir, "completeness.png"), width = project_config$plots$completeness_width, height = project_config$plots$completeness_height),
    format = "file",
    description = "Data completeness plot file"
  ),
  
  tar_target(
    name = file_microfossil_plot,
    command = save_plot(plot_microfossil_types, file.path(project_config$paths$plot_dir, "microfossil_types.png"), width = project_config$plots$microfossil_width, height = project_config$plots$microfossil_height),
    format = "file",
    description = "Microfossil types plot file"
  ),

  # ========================================================================= #
  # PALEO COMMUNITY ECOLOGY ----
  # ========================================================================= #

  tar_target(
    name = aggregated_taxa,
    command = aggregate_taxa_by_level(paleo_summary_table, level = "full"),
    description = "Aggregate taxa by taxonomic level or custom groupings"
  ),
  
  tar_target(
    name = filtered_taxa, 
    command = filter_rare_taxa(aggregated_taxa, threshold = project_config$analysis$rare_taxa_threshold),
    description = "Filter out rare taxa below occurrence threshold"
  ),
  
  tar_target(
    name = community_metrics,
    command = calculate_community_metrics(filtered_taxa, evenness_index = project_config$analysis$community_evenness_index, time_column = project_config$analysis$time_column),
    description = "Calculate richness, evenness, and beta diversity by time bin"
  ),
  
  tar_target(
    name = turnover_matrix,
    command = calculate_pairwise_turnover(filtered_taxa, method = project_config$analysis$turnover_method, time_column = project_config$analysis$time_column),
    description = "Pairwise beta diversity matrix between all time bins"
  ),
  
  tar_target(
    name = trends_plot,
    command = visualize_community_trends(community_metrics, plot_type = project_config$analysis$community_trends_plot_type, time_axis = project_config$analysis$time_column),
    description = "Time series visualization of community metrics"
  ),
  
  tar_target(
    name = turnover_heatmap,
    command = visualize_turnover_heatmap(turnover_matrix, color_palette = project_config$plots$turnover_color_palette),
    description = "Heatmap of pairwise turnover between time bins"
  ),
  
  # Community ecology plot files
  tar_target(
    name = file_community_trends,
    command = save_plot(trends_plot, file.path(project_config$paths$plot_dir, "community_trends.png"), width = project_config$plots$community_trends_width, height = project_config$plots$community_trends_height),
    format = "file",
    description = "Community trends plot file"
  ),
  
  tar_target(
    name = file_turnover_heatmap,
    command = save_plot(turnover_heatmap, file.path(project_config$paths$plot_dir, "turnover_heatmap.png"), width = project_config$plots$turnover_heatmap_width, height = project_config$plots$turnover_heatmap_height),
    format = "file",
    description = "Turnover heatmap plot file"
  ),

  # ========================================================================= #
  # RECORDER BIAS ANALYSIS ----
  # ========================================================================= #
  

  # Recorder bias
  tar_target(
    name = recorder_mapping,
    command = extract_recorder_mapping(paleo_with_ids),
    description = "Mapping of LSPEC to recorder for bias analysis"
  ),

  tar_target(
    name = recorder_bias_analysis,
    command = analyze_recorder_bias(community_metrics, recorder_mapping, paleo_summary_table),
    description = "ANOVA analysis of community metrics by recorder to detect measurement bias"
  ),

  # Recorder bias plot files
  tar_target(
    name = file_recorder_bias_richness,
    command = save_plot(recorder_bias_analysis$plots$richness, file.path(project_config$paths$plot_dir, "recorder_bias_richness.png"), width = project_config$plots$recorder_bias_width, height = project_config$plots$recorder_bias_height),
    format = "file",
    description = "Recorder bias analysis plot for species richness"
  ),

  tar_target(
    name = file_recorder_bias_evenness,
    command = save_plot(recorder_bias_analysis$plots$evenness, file.path(project_config$paths$plot_dir, "recorder_bias_evenness.png"), width = project_config$plots$recorder_bias_width, height = project_config$plots$recorder_bias_height),
    format = "file",
    description = "Recorder bias analysis plot for species evenness"
  ),

  tar_target(
    name = file_recorder_bias_shannon,
    command = save_plot(recorder_bias_analysis$plots$shannon_diversity, file.path(project_config$paths$plot_dir, "recorder_bias_shannon.png"), width = project_config$plots$recorder_bias_width, height = project_config$plots$recorder_bias_height),
    format = "file",
    description = "Recorder bias analysis plot for Shannon diversity"
  ),
  
  # ========================================================================= #
  # REPORT GENERATION ----
  # ========================================================================= #
  
  tar_render(
    name = report,
    path = project_config$paths$report_rmd,
    output_dir = project_config$paths$report_dir
  )
)