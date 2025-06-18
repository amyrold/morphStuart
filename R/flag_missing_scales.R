#' Filter Fish Specimens Missing Scale Bars
#'
#' Removes fish specimens that lack 10mm scale bar measurements, which are
#' essential for normalizing morphological measurements. Specimens without
#' scale bars cannot be properly measured or compared across the dataset.
#'
#' @param morph Data frame containing morphological data with fish_id, 
#'   part_type (P/C), and Scale_10mm columns
#' @return Data frame containing only fish specimens with scale bar measurements
#'
#' @details
#' The function checks for scale bar presence in both part (P) and counterpart (C)
#' specimens. Fish are retained if they have scale bars in either part or 
#' counterpart. Detailed reports are written to:
#' - data/flagged/fish_missing_scales_detailed.csv
#' - data/flagged/fish_without_scales.csv  
#' - data/raw/fish_with_scales.csv
#'
#' @examples
#' morph_filtered <- flag_missing_scales(morph_with_ids)
flag_missing_scales <- function(morph){
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
            file = "data/flagged/fish_missing_scales_detailed.csv", 
            row.names = FALSE)
  
  # Split the dataset
  morph_with_scales <- morph %>%
    filter(!fish_id %in% fish_missing_scales)
  
  morph_without_scales <- morph %>%
    filter(fish_id %in% fish_missing_scales)
  
  # Write the datasets to separate files for reference
  write.csv(morph_with_scales, 
            file = "data/raw/fish_with_scales.csv", 
            row.names = FALSE)
  
  write.csv(morph_without_scales, 
            file = "data/flagged/fish_without_scales.csv", 
            row.names = FALSE)
  return(morph_with_scales)
}