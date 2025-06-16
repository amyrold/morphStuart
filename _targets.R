#_targets.R
library(targets)

tar_option_set(
  packages = c("dplyr", "tidyr", "stringr", "tibble", "purrr", "ggplot2", "scales",
               "vegan", "rioja")
)

tar_source()

list(
  
  # Data Import ----
  tar_target(
    morph, 
    "data/raw/020625_PitLMorph.csv",
    format = "file"
    ),
  tar_target(morph_raw, read.csv(morph)),
  tar_target(
    paleo, 
    "data/raw/060625_paleoeco_seriesL.csv",
    format = "file"
  ),
  tar_target(paleo_raw, read.csv(paleo)),
  tar_target(
    order, 
    "data/raw/PitLMorph_fieldorder.csv",
    format = "file"
  ),
  tar_target(order_raw, read.csv(order)),
  
  # Morphology ----
  # Basic preprocessing
  tar_target(morph_with_ids, extract_fish_ids(morph_raw)),
  #tar_target(morph_quality_check, basic_quality_checks(morph_with_ids)),
  tar_target(morph_scaled, flag_missing_scales(morph_with_ids)),
  
  # Special case handling
  tar_target(morph_corrected, handle_special_cases(morph_scaled)),
  
  # Overlap detection and resolution
  tar_target(overlap_metrics, identify_overlaps(morph_corrected, threshold = 0.05)),
  tar_target(non_overlap_fish, extract_non_overlap(overlap_metrics)),
  tar_target(overlap_fish, extract_overlap_fish(overlap_metrics)),
  tar_target(morph_merged, merge_non_overlap(non_overlap_fish)),
  
  # Review and flagging
  tar_target(overlap_review, create_review_list(overlap_metrics, use_threshold = TRUE)),
  tar_target(review_export, export_review_files(overlap_review, "data/flagged")),
  
  # Field Order ----
  #tar_target(fieldorder_formatted, format_fieldorder_data(order_raw)),
  #tar_target(fieldorder_duplicates, identify_fieldorder_duplicates(fieldorder_formatted)),
  #tar_target(fieldorder_clean, create_clean_fieldorder(fieldorder_formatted, fieldorder_duplicates)),
  #tar_target(fieldorder_flagged, create_flagged_fieldorder(fieldorder_duplicates)),
  
  # Paleo Eco ----
  tar_target(paleo_metadata, extract_paleo_metadata(paleo_raw)),
  tar_target(paleo_merged_counts, merge_microscopy_counts(paleo_raw))
  
)


