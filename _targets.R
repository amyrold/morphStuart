# _targets.R
# ========================================================================= #
# STICKLEBACK MORPHOLOGY & PALEO-ECOLOGY ANALYSIS PIPELINE
# ========================================================================= #
# 
# This pipeline processes stickleback morphological measurements and 
# paleo-ecological microfossil data for evolutionary analysis.
#
# PIPELINE OVERVIEW:
# 1. DATA IMPORT: Load raw morphology, paleo, and field order data
# 2. MORPHOLOGY PROCESSING: Clean IDs, handle quality issues, merge P/CP data
# 3. PALEO PROCESSING: Extract metadata, merge microscopy counts, integrate ages
# 4. ANALYSIS OUTPUTS: Create rioja matrices, diversity metrics, export results
#
# USAGE:
# - Run entire pipeline: tar_make()
# - View pipeline: tar_visnetwork()
# - Check status: tar_progress()
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("dplyr", "tidyr", "stringr", "tibble", "purrr", "ggplot2", "scales",
               "vegan", "rioja", "readr"),
  format = "rds",
  error = "continue"
)

tar_source()

list(
  # ========================================================================= #
  # CONFIGURATION & SETUP ----
  # ========================================================================= #
  
  # tar_target(
  #   name = variable_mapping,
  #   command = variable_mapping(),
  #   description = "Define variable types (continuous, count, binary) for analysis"
  # ),
  
  # ========================================================================= #
  # DATA IMPORT ----
  # ========================================================================= #
  
  tar_target(
    name = morph_file,
    command = "data/raw/020625_PitLMorph.csv",
    format = "file",
    description = "Raw morphological measurements CSV file"
  ),
  
  tar_target(
    name = morph_raw,
    command = read.csv(morph_file),
    description = "Raw morphological data (fish measurements, P/CP pairs)"
  ),
  
  tar_target(
    name = paleo_file,
    command = "data/raw/060625_paleoeco_seriesL.csv", 
    format = "file",
    description = "Raw paleo-ecological microfossil counts CSV file"
  ),
  
  tar_target(
    name = paleo_raw,
    command = read.csv(paleo_file),
    description = "Raw paleo-ecological data (microscopy line counts)"
  ),
  
  tar_target(
    name = order_file,
    command = "data/raw/PitLMorph_fieldorder.csv",
    format = "file", 
    description = "Field order reference data CSV file"
  ),
  
  tar_target(
    name = order_raw,
    command = read.csv(order_file),
    description = "Field collection order reference data"
  ),
  
  # ========================================================================= #
  # EXTRACT STANDARDIZED IDs ----
  # ========================================================================= #
  
  tar_target(morph_with_ids, extract_ids(morph_raw)),
  tar_target(paleo_with_ids, extract_ids(paleo_raw)),  
  tar_target(order_with_ids, extract_ids(order_raw)),  
  
  # ========================================================================= #
  # MORPHOLOGY PROCESSING PIPELINE ----
  # ========================================================================= #
  
  tar_target(
    name = morph_with_scales, 
    command = flag_missing_scales(morph_with_ids),
    description = "Filter out fish specimens missing 10mm scale bars"
  ),
  
  tar_target(
    name = morph_corrected,
    command = handle_special_NAs(morph_with_scales),
    description = "Apply biological logic to distinguish true zeros from missing data"
  ),
  
  tar_target(
    name = morph_non_overlap,
    command = flag_counterpart_conflicts(morph_corrected, threshold = 0.05),
    description = "Flag and remove fish with conflicting part/counterpart measurements"
  ),
  
  tar_target(
    name = morph_final,
    command = merge_counter_parts(morph_non_overlap),
    description = "Merge non-conflicting part/counterpart data into single records"
  ),
  
  # ========================================================================= #
  # PALEO-ECOLOGICAL PROCESSING PIPELINE ----
  # ========================================================================= #
  
  # tar_target(
  #   name = paleo_metadata,
  #   command = extract_paleo_metadata(paleo_raw),
  #   description = "Extract V-numbers, LSPEC codes, and sample type classification"
  # ),
  
  tar_target(
    name = paleo_merged_counts,
    command = merge_microscopy_counts(paleo_raw),
    description = "Merge 4-5 microscopy line counts per geological sample"
  ),
  
  # tar_target( #TODO update morph_final -> paleo eco and move to analysis
  #   name = paleo_with_ages,
  #   command = integrate_age_data(paleo_metadata, morph_final),
  #   description = "Link paleo samples to stratigraphic depths and ages via LSPEC"
  # )
  
  #============================================================================#
  # FIELD ORDER ----
  #tar_target(fieldorder_formatted, format_fieldorder_data(order_raw)),
  #tar_target(fieldorder_duplicates, identify_fieldorder_duplicates(fieldorder_formatted)),
  #tar_target(fieldorder_clean, create_clean_fieldorder(fieldorder_formatted, fieldorder_duplicates)),
  #tar_target(fieldorder_flagged, create_flagged_fieldorder(fieldorder_duplicates))
  
  #============================================================================#
  
  #TODO build extract_lspec() & extract_vscore() -> utils
  
  
  #TODO build format_fieldorder()
  #TODO build identify_fieldorder_duplicates()
  #TODO build clean_fieldorder()
  # TODO build flag_fieldorder()
  tar_render(report, "report.Rmd")
)


