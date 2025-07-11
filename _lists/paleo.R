paleo_processing <- list(
  # ========================================================================= #
  # PALEO-ECOLOGICAL PROCESSING PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = paleo_merged_counts,
    command = merge_microscopy_counts(paleo_with_ids),
    description = "Merge 4-5 microscopy line counts per geological sample"
  ),

  # Apply functional groupings and exclusions (NEW STEP)
  tar_target(
    paleo_grouped_counts,  # This becomes the functional grouped data
    {
      apply_functional_groupings(paleo_merged_counts, project_config)
    }
  ),

  tar_target(
    name = paleo_stickleback_only,
    command = filter_stickleback_samples(paleo_grouped_counts),
    description = "Filter paleo data to stickleback samples only (removes killifish LXXXX samples)"
  ),

  tar_target(
    name = lspec_integration_check,
    command = validate_lspec_links(paleo_stickleback_only, fieldorder_final$complete),
    description = "Validate which paleo LSPECs can be linked with field order stratigraphic data"
  ),
  
  tar_target(
    name = paleo_with_depths,
    command = merge_paleo_with_fieldorder(paleo_stickleback_only, fieldorder_final$complete),
    description = "Merge stickleback paleo data with field order age/depth information via LSPEC"
  )
)