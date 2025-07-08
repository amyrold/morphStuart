morph <- list(
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
  )
)