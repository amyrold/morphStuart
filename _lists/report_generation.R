report_generation <- list(
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
  # REPORT GENERATION ----
  # ========================================================================= #

  tar_render(
    name = report,
    path = project_config$paths$report_rmd,
    output_dir = project_config$paths$report_dir
  )
)