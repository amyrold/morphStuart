stratigraphy <- list(
  # ========================================================================= #
  # RIOJA INTEGRATION PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = paleo_summary_table,
    command = create_paleo_summary_table(paleo_with_depths),
    description = "Wide-format table with LSPEC rows and individual taxa columns"
  ),
  
  tar_target(
    name = rioja_species_matrix,
    command = prepare_rioja_species_data(paleo_with_depths, grouping_level = project_config$analysis$rioja_grouping_level),
    description = "Species matrix for rioja stratigraphic plots (full taxonomic resolution)"
  ),
  
  tar_target(
    name = rioja_filtered_species_matrix,
    command = filter_rare_taxa(rioja_species_matrix, threshold = project_config$analysis$rioja_rare_taxa_threshold),
    description = "Filtered species matrix for rioja plots, removing rare taxa below threshold"
  ),
  
  tar_target(
    name = rioja_depth_data,
    command = prepare_rioja_depth_data(paleo_with_depths),
    description = "Age/depth vector for rioja stratigraphic analysis (YEAR primary, CSTRAT backup)"
  ),
  
  tar_target(
    name = rioja_strat_plots,
    command = create_rioja_strat_plots(rioja_species_matrix, rioja_depth_data),
    description = "Rioja stratigraphic plots showing microfossil distributions through time"
  )
)