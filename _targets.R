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
               "vegan", "rioja", "readr", "knitr", "DT", "yaml", "visNetwork"),
  format = "rds",
  error = "continue"
)

# Load project configuration globally
project_config <- yaml::read_yaml("config.yml")

tar_source()
tar_source("_lists/")

list(

  # ========================================================================= #
  # CONFIGURATION TRACKER ----
  # ========================================================================= #
  # This target exists solely to track changes in config.yml
  # It does not produce a value that is used by other targets.
  tar_target(
    name = config_file_tracker,
    command = "config.yml",
    format = "file",
    description = "Tracks changes in the project configuration file"
  ),
  
  preprocessing,  
  
  morph,
  
  paleo,
  
  fieldorder,
  
  stratigraphy,
  
  report_generation,

  community_ecology,

  recorder_bias
  )