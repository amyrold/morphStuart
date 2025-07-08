recorder_bias <- list(
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
    command = analyze_recorder_bias(community_metrics, recorder_mapping),
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
  )
)