fieldorder_processing <- list(
  # ========================================================================= #
  # FIELD ORDER PROCESSING PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = missing_morph_lspecs_review,
    command = validate_missing_morph_lspecs(morph_with_ids, fieldorder_final),
    description = "Identify morph LSPECs missing from field order data"
  ),

  tar_target(
    name = fieldorder_formatted,
    command = format_fieldorder(order_with_ids),
    description = "Format and standardize field order data with LSPEC IDs"
  ),
  
  tar_target(
    name = fieldorder_duplicates_clean,
    command = process_fieldorder_duplicates(fieldorder_formatted),
    description = "Remove conflicting duplicates, merge clean duplicates"
  ),

  tar_target(
    name = fieldorder_updated,
    command = update_fieldorder_data(fieldorder_duplicates_clean, updated_depths_file),
    description = "Add additional data if present"
  ),
  
  tar_target(
    name = fieldorder_final,
    command = handle_fieldorder_missing_data(fieldorder_updated),
    description = "Complete records ready for analysis"
  ),
  
  tar_target(
    name = fieldorder_processing_summary,
    command = summarize_fieldorder_processing(
      fieldorder_formatted, 
      fieldorder_duplicates_clean,
      fieldorder_updated, 
      fieldorder_final,
      "PitLMorph_fieldorder.csv"
    ),
    description = "Complete processing summary with statistics and quality metrics"
  )
)