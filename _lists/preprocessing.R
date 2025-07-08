preprocessing <- list(
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

  # ========================================================================= #
  # DATA IMPORT ----
  # ========================================================================= #
  
  tar_target(
    name = morph_file,
    command = project_config$paths$raw_morph,
    format = "file",
    description = "Raw morphological measurements CSV file"
  ), tar_target(name = morph_raw, command = read.csv(morph_file)),
  
  tar_target(
    name = paleo_file,
    command = project_config$paths$raw_paleo,
    format = "file", 
    description = "Raw paleo-ecological microfossil counts CSV file"
  ), tar_target(name = paleo_raw, command = read.csv(paleo_file)),
  
  tar_target(
    name = order_file,
    command = project_config$paths$raw_order,
    format = "file", 
    description = "Field order reference data CSV file"
  ), tar_target(name = order_raw, command = read.csv(order_file)),
  
  # ========================================================================= #
  # EXTRACT STANDARDIZED IDs ----
  # ========================================================================= #
  
  tar_target(morph_with_ids, extract_ids(morph_raw)),
  tar_target(paleo_with_ids, extract_ids(paleo_raw)),  
  tar_target(order_with_ids, extract_ids(order_raw))
  
)