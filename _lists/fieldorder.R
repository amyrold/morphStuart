fieldorder_processing <- list(
  # ========================================================================= #
  # FIELD ORDER PROCESSING PIPELINE ----
  # ========================================================================= #
  
  # REVIEWED: No longer needed
  # tar_target(
  #   name = decimal_lspecs_review,
  #   command = validate_decimal_lspecs(order_raw),
  #   description = "Identify all field order records with decimal LSPEC values for colleague review"
  # ),
  
  tar_target(
    name = missing_morph_lspecs_review,
    command = validate_missing_morph_lspecs(morph_with_ids, fieldorder_final$complete),
    description = "Identify morph LSPECs missing from field order data"
  ),

  tar_target(
    name = fieldorder_formatted,
    command = format_fieldorder(order_with_ids),
    description = "Format and standardize field order data with LSPEC IDs"
  ),
  
  tar_target(
    name = fieldorder_duplicates_processed,
    command = process_fieldorder_duplicates(fieldorder_formatted),
    description = "Remove conflicting duplicates, merge clean duplicates"
  ),
  
  tar_target(
    name = fieldorder_final,
    command = handle_fieldorder_missing_data(fieldorder_duplicates_processed),
    description = "Separate complete records from those with missing data"
  ),
  
  tar_target(
    name = fieldorder_processing_summary,
    command = summarize_fieldorder_processing(
      fieldorder_formatted, 
      fieldorder_duplicates_processed, 
      fieldorder_final,
      "PitLMorph_fieldorder.csv"
    ),
    description = "Complete processing summary with statistics and quality metrics"
  )
)