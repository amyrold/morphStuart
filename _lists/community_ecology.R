community_ecology <- list(
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
  )
)