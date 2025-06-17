#'
#'
#'
#'


missing_scales <- function(morph){
  # Check for missing scale information
  scale_summary <- morph %>%
    group_by(fish_id) %>%
    summarize(
      has_part_scale = any(!is.na(Scale_10mm[part_type == "P"])),
      has_cpart_scale = any(!is.na(Scale_10mm[part_type == "C"])),
      has_any_scale = has_part_scale | has_cpart_scale,
      .groups = "drop"
    )
  
  # Get list of fish IDs missing scales
  fish_missing_scales <- scale_summary %>%
    filter(!has_any_scale) %>%
    pull(fish_id)
  
  # Create report of fish missing scale information
  missing_scales_detail <- morph %>%
    filter(fish_id %in% fish_missing_scales) %>%
    select(fish_id, ID, part_type, Scale_10mm) %>%
    arrange(fish_id, part_type)
  
  # Write the detailed report
  write.csv(missing_scales_detail, 
            file = paste0(p$data.flagged, "fish_missing_scales_detailed.csv"), 
            row.names = FALSE)
  
  # Split the dataset
  morph_with_scales <- morph %>%
    filter(!fish_id %in% fish_missing_scales)
  
  morph_without_scales <- morph %>%
    filter(fish_id %in% fish_missing_scales)
  
  # Write the datasets to separate files for reference
  write.csv(morph_with_scales, 
            file = paste0(p$data.raw, "fish_with_scales.csv"), 
            row.names = FALSE)
  
  write.csv(morph_without_scales, 
            file = paste0(p$data.flagged, "fish_without_scales.csv"), 
            row.names = FALSE)
  return(morph_with_scales)
}