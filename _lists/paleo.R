paleo_processing <- list(
  # ========================================================================= #
  # PALEO-ECOLOGICAL PROCESSING PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = paleo_merged_counts,
    command = merge_microscopy_counts(paleo_with_ids),
    description = "Merge 4-5 microscopy line counts per geological sample"
  )
)