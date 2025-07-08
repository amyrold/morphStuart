fieldorder_integration <- list(
  # ========================================================================= #
  # MORPH DEPTH INTEGRATION ----
  # ========================================================================= #
  
  
  # ========================================================================= #
  # PALEO DEPTH INTEGRATION ----
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
  )
)