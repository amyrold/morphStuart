#'
#'
#'
#'
#'
#'
#'

morph_cleaning <- function(morph){
  list(
    tar_target(morph_with_ids, format_sampleID(morph)), # Clean sample IDs
    tar_target(morph_with_scales, missing_scales(morph_with_ids)), # Filter/flag fish w/o 10mm_scale
    tar_target(morph_corrected, handle_special_cases(morph_with_scales)), # Handle NA logic
    tar_target(overlap_results, identify_overlaps(morph_corrected, threshold = 0.05)), # Identify conflicting P/CP overlaps
    tar_target(morph_merged, merge_non_overlap(overlap_results$non_overlap_fish)),
    tar_target(review_overlaps, create_review_list(overlap_results, use_threshold = TRUE)), # Export conflicting P/CP overlaps
    tar_target() # Create visuals of overlap results
  )
  
}