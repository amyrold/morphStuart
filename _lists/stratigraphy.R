stratigraphy <- list(
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
  )
)