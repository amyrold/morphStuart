morph_processing <- list(
  # ========================================================================= #
  # MORPH PROCESSING PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = flagged_fish_missing_scales,
    command = flag_missing_scales(morph_with_ids),
    description = "Vector of fish_id values missing 10mm scale bar information"
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

  tar_target(
    name = morph_with_depths,
    command = merge_morph_with_fieldorder(morph_final, fieldorder_final),
    description = "Merge morphology data with field order age/depth information via LSPEC"
  ),

  tar_target(
    name = morph_integration_summary,
    command = summarize_morph_integration(morph_final, morph_with_depths),
    description = "Integration summary with statistics and quality metrics for morphology-fieldorder merge"
  )
)