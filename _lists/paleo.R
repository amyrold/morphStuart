paleo_processing <- list(
  # ========================================================================= #
  # PALEO-ECOLOGICAL PROCESSING PIPELINE ----
  # ========================================================================= #

  tar_target(
    name = paleo_stickleback_only,
    command = filter_stickleback_samples(paleo_with_ids),
    description = "Filter paleo data to stickleback samples only (removes killifish LXXXX samples)"
  ),

  tar_target(
    name = paleo_merged_counts,
    command = merge_microscopy_counts(paleo_stickleback_only),
    description = "Merge 4-5 microscopy line counts per geological sample"
  ),

  tar_target(
    name = paleo_with_functional_labels,
    command = apply_functional_labels(paleo_merged_counts, project_config),
    description = "Add functional group labels and apply exclusions, preserving taxonomic resolution"
  ),

  tar_target(
    name = lspec_integration_check,
    command = validate_lspec_links(paleo_with_functional_labels, fieldorder_final),
    description = "Validate which paleo LSPECs can be linked with field order stratigraphic data"
  ),
  
  tar_target(
    name = paleo_with_depths,
    command = merge_paleo_with_fieldorder(paleo_with_functional_labels, fieldorder_final),
    description = "Merge stickleback paleo data with field order age/depth information via LSPEC"
  )
)