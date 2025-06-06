# =============================================================================
# MORPHOLOGY DATA CLEANING FUNCTIONS
# =============================================================================
# 
# This script contains functions for cleaning fish fossil morphology data
# measured using objectJ. The pipeline handles:
# 1. Variable mapping and type definition
# 2. Special case handling for dorsal spines and pelvic girdle
# 3. Identification of overlapping measurements between part/counterpart
# 4. Merging of non-overlapping data
# 5. Flagging of conflicting measurements for manual review
# 6. Data validation
#
# Author: Aaron Myrold
# Dependencies: dplyr, tidyr, stringr
# =============================================================================

# Required libraries
if (!require("dplyr")) {
  stop("dplyr package is required but not installed. Please install it first.")
}
if (!require("tidyr")) {
  stop("tidyr package is required but not installed. Please install it first.")
}
if (!require("stringr")) {
  stop("stringr package is required but not installed. Please install it first.")
}

# =============================================================================
# VARIABLE MAPPING AND DEFINITIONS
# =============================================================================

#' Define Variable Types for Morphological Measurements
#'
#' Creates a mapping of morphological variables into their measurement types
#' (continuous, count, binary) for appropriate statistical handling.
#'
#' @return A list containing vectors of variable names grouped by type:
#'   - continuous: Length measurements (SL, CAV, DS1-3, etc.)
#'   - count: Count measurements (MDF, MAF, MCV, etc.) 
#'   - binary: Binary flags (MDS1-3, MDS1NA-3NA, etc.)
#'   - all: Combined vector of all measurement variables
#'
#' @examples
#' var_map <- variable_mapping()
#' continuous_vars <- var_map$continuous
#' all_vars <- var_map$all
variable_mapping <- function() {
  # Define measurement types based on biological meaning and statistical properties
  continuous_vars <- c("SL", "CAV", "DS1", "DS2", "DS3", "LPT", 
                       "PSP.L", "PSP.R", "TPG", "ECT", "CLE", "PMX")
  
  count_vars <- c("MDF", "MAF", "MCV", "MAV", "MPT", "MPSP")
  
  binary_vars <- c("MDS1", "MDS2", "MDS3", "MDS1NA", 
                   "MDS2NA", "MDS3NA", "MPSPNA", "PGNA")
  
  # Return as a named list for easy access
  return(list(
    continuous = continuous_vars,
    count = count_vars,
    binary = binary_vars,
    all = c(continuous_vars, count_vars, binary_vars)
  ))
}

# =============================================================================
# SPECIAL CASE HANDLING
# =============================================================================

#' Handle Special Cases in Dorsal Spine and Pelvic Girdle Data
#'
#' Applies biological logic to distinguish between true zeros (evolutionary loss)
#' and missing data due to poor preservation. This is critical for accurate
#' evolutionary analysis of fish morphology.
#'
#' Logic implemented:
#' - If MDS* = 0 & MDS*NA = 1: Set DS* to NA (lost to preservation)
#' - If MDS* = 0 & MDS*NA = 0: Set DS* to 0 (true evolutionary loss)
#' - If MPT = 0 but any dorsal spine present: Set MPT to NA (poor preservation)
#' - Similar logic for pelvic spines using MPSP and MPSPNA flags
#' - If PGNA column exists, apply similar logic to TPG
#'
#' @param data Data frame containing morphological measurements with special case columns
#' @return Data frame with special cases properly handled (zeros vs NAs)
#'
#' @examples
#' cleaned_data <- handle_special_cases(raw_morph_data)
handle_special_cases <- function(data) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Convert to tibble for dplyr operations
  cleaned <- data %>%
    as_tibble() %>%
    # Handle dorsal spine columns using case_when for readable conditional logic
    mutate(
      # Dorsal spine 1: distinguish evolutionary loss from poor preservation
      DS1 = case_when(
        MDS1 == 0 & MDS1NA == 1 ~ NA_real_,  # Lost to preservation
        MDS1 == 0 & MDS1NA == 0 & is.na(DS1) ~ 0,  # True evolutionary loss
        TRUE ~ DS1  # Keep existing values
      ),
      # Dorsal spine 2
      DS2 = case_when(
        MDS2 == 0 & MDS2NA == 1 ~ NA_real_,
        MDS2 == 0 & MDS2NA == 0 & is.na(DS2) ~ 0,
        TRUE ~ DS2
      ),
      # Dorsal spine 3
      DS3 = case_when(
        MDS3 == 0 & MDS3NA == 1 ~ NA_real_,
        MDS3 == 0 & MDS3NA == 0 & is.na(DS3) ~ 0,
        TRUE ~ DS3
      ),
      
      # Handle MPT (pre-dorsal pterygiophores)
      # If any dorsal spine is present but MPT = 0, this is likely poor preservation
      MPT = case_when(
        MPT == 0 & (MDS1 == 1 | MDS2 == 1 | MDS3 == 1) ~ NA_real_,
        TRUE ~ MPT
      ),
      
      # Handle pelvic spine columns using similar logic
      PSP.L = case_when(
        MPSP == 0 & MPSPNA == 1 ~ NA_real_,  # Lost to preservation
        TRUE ~ PSP.L
      ),
      PSP.R = case_when(
        MPSP == 0 & MPSPNA == 1 ~ NA_real_,  # Lost to preservation
        TRUE ~ PSP.R
      )
    )
  
  # Apply pelvic girdle logic if PGNA column is available
  if ("PGNA" %in% names(cleaned)) {
    cleaned <- cleaned %>%
      mutate(
        TPG = case_when(
          PGNA == 1 ~ NA_real_,  # Lost to preservation
          TRUE ~ TPG
        )
      )
  }
  
  return(as.data.frame(cleaned))
}

# =============================================================================
# OVERLAP IDENTIFICATION
# =============================================================================

#' Identify Overlapping Measurements Between Part and Counterpart
#'
#' Identifies fish specimens that have the same measurement recorded in both
#' the "part" (higher quality) and "counterpart" (lower quality) fossil sides.
#' Calculates relative differences and flags specimens exceeding threshold.
#'
#' @param data Data frame with fish_id, part_type (P/C), and measurements
#' @param threshold Numeric threshold for relative differences (default 0.05 = 5%)
#' @return List containing:
#'   - overlap_fish: Fish with measurements exceeding threshold
#'   - non_overlap_fish: Fish without concerning overlaps  
#'   - overlap_metrics: Detailed metrics for all overlapping measurements
#'
#' @examples
#' results <- identify_overlaps(morph_data, threshold = 0.05)
#' flagged_fish <- results$overlap_fish
identify_overlaps <- function(data, threshold = 0.05) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!all(c("fish_id", "part_type") %in% names(data))) {
    stop("Data must contain 'fish_id' and 'part_type' columns")
  }
  if (threshold < 0 || threshold > 1) {
    stop("Threshold must be between 0 and 1")
  }
  
  # Get variable mappings for appropriate handling by type
  var_map <- variable_mapping()
  
  # Separate part and counterpart data to avoid many-to-many joins
  part_data <- data %>% 
    filter(part_type == "P") %>%
    select(fish_id, part_type, row_id = n, Scale_10mm, all_of(var_map$all))
  
  cpart_data <- data %>% 
    filter(part_type == "C") %>%
    select(fish_id, part_type, row_id = n, Scale_10mm, all_of(var_map$all))
  
  # Aggregate values within each fish/part combination to handle duplicates
  part_agg <- part_data %>%
    pivot_longer(
      cols = all_of(var_map$all),
      names_to = "measure",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    group_by(fish_id, measure) %>%
    summarize(
      # Use max for binary variables, mean for others
      part_value = if(first(measure) %in% var_map$binary) max(value) else mean(value),
      part_scale = mean(Scale_10mm, na.rm = TRUE),
      has_multiple_values = n() > 1,
      .groups = "drop"
    )
  
  cpart_agg <- cpart_data %>%
    pivot_longer(
      cols = all_of(var_map$all),
      names_to = "measure", 
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    group_by(fish_id, measure) %>%
    summarize(
      cpart_value = if(first(measure) %in% var_map$binary) max(value) else mean(value),
      cpart_scale = mean(Scale_10mm, na.rm = TRUE),
      has_multiple_values = n() > 1,
      .groups = "drop"
    )
  
  # Join to find overlaps (now guaranteed one-to-one)
  overlaps <- part_agg %>%
    inner_join(cpart_agg, by = c("fish_id", "measure"))
  
  # Calculate differences with type-appropriate methods
  overlaps <- overlaps %>%
    mutate(
      var_type = case_when(
        measure %in% var_map$continuous ~ "continuous",
        measure %in% var_map$count ~ "count",
        measure %in% var_map$binary ~ "binary",
        TRUE ~ "other"
      ),
      # Use average scale for normalization
      scale = coalesce((part_scale + cpart_scale) / 2, part_scale, cpart_scale),
      absolute_diff = abs(part_value - cpart_value),
      
      # Calculate relative differences appropriate to variable type
      relative_diff = case_when(
        # For continuous: relative to 10mm scale
        var_type == "continuous" & scale > 0 ~ absolute_diff / scale,
        # For count: proportion of maximum value
        var_type == "count" ~ absolute_diff / max(1, pmax(part_value, cpart_value)),
        # For binary: simply different or not
        TRUE ~ as.numeric(absolute_diff > 0)
      ),
      
      # Flag based on variable type and threshold
      exceeds_threshold = case_when(
        var_type == "continuous" ~ relative_diff > threshold,
        var_type == "count" ~ absolute_diff > 0,  # Any count difference
        var_type == "binary" ~ FALSE,  # Don't flag binary (take max)
        TRUE ~ FALSE
      ),
      
      # Flag if either part had multiple measurements
      multiple_measurements = has_multiple_values.x | has_multiple_values.y
    )
  
  # Identify fish to flag based on exceeding thresholds
  fish_to_flag <- overlaps %>%
    filter(exceeds_threshold) %>%
    select(fish_id) %>%
    distinct() %>%
    pull(fish_id)
  
  # Split original data by flagged status
  overlap_fish <- data %>%
    filter(fish_id %in% fish_to_flag)
  
  non_overlap_fish <- data %>%
    filter(!fish_id %in% fish_to_flag)
  
  # Return results maintaining original data structure
  return(list(
    overlap_fish = as.data.frame(overlap_fish),
    non_overlap_fish = as.data.frame(non_overlap_fish), 
    overlap_metrics = as.data.frame(overlaps)
  ))
}

# =============================================================================
# DATA MERGING
# =============================================================================

#' Merge Non-Overlapping Part and Counterpart Data
#'
#' For fish without conflicting measurements, intelligently merge part and 
#' counterpart data prioritizing part over counterpart. Handles multiple
#' rows per fish/part combination by aggregating appropriately by variable type.
#'
#' @param data Data frame containing non-overlapping fish measurements
#' @return Data frame with one row per fish, merged measurements
#'
#' @examples
#' merged_data <- merge_non_overlap(non_overlap_fish)
merge_non_overlap <- function(data) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!"fish_id" %in% names(data)) {
    stop("Data must contain 'fish_id' column")
  }
  
  # Get variable mappings for type-appropriate aggregation
  var_map <- variable_mapping()
  
  # Define column types
  id_cols <- c("n", "ID", "fish_id", "part_type", "bin")
  measure_cols <- setdiff(names(data), id_cols)
  
  # Get unique fish for processing
  fish_ids <- unique(data$fish_id)
  result_list <- list()
  
  # Process each fish individually
  for (i in seq_along(fish_ids)) {
    id <- fish_ids[i]
    fish_data <- data %>% filter(fish_id == id)
    
    # Separate part and counterpart data
    part_data <- fish_data %>% filter(part_type == "P")
    cpart_data <- fish_data %>% filter(part_type == "C")
    
    # Initialize result row
    result_row <- data.frame(fish_id = id)
    
    # Handle cases where only one part type exists
    if (nrow(part_data) == 0) {
      # Only counterpart data - aggregate it
      if (nrow(cpart_data) > 0) {
        template_row <- cpart_data[1,]
        # Copy metadata
        for (col in id_cols) {
          if (col != "part_type") {
            result_row[[col]] <- template_row[[col]]
          }
        }
        # Aggregate measurements
        for (col in measure_cols) {
          result_row[[col]] <- aggregate_values(cpart_data[[col]], col, var_map)
        }
      }
      result_row$part_type <- "merged"
      result_row$merged <- TRUE
      result_list[[i]] <- result_row
      next
      
    } else if (nrow(cpart_data) == 0) {
      # Only part data - aggregate it
      template_row <- part_data[1,]
      # Copy metadata  
      for (col in id_cols) {
        if (col != "part_type") {
          result_row[[col]] <- template_row[[col]]
        }
      }
      # Aggregate measurements
      for (col in measure_cols) {
        result_row[[col]] <- aggregate_values(part_data[[col]], col, var_map)
      }
      result_row$part_type <- "merged"
      result_row$merged <- TRUE
      result_list[[i]] <- result_row
      next
    }
    
    # Both part and counterpart exist - merge with part priority
    template_row <- part_data[1,]
    # Copy metadata from part
    for (col in id_cols) {
      if (col != "part_type") {
        result_row[[col]] <- template_row[[col]]
      }
    }
    result_row$merged <- TRUE
    
    # For each measurement, aggregate within type then merge with priority
    for (col in measure_cols) {
      # Aggregate part values
      part_val <- aggregate_values(part_data[[col]], col, var_map)
      # Aggregate counterpart values  
      cpart_val <- aggregate_values(cpart_data[[col]], col, var_map)
      
      # Priority: part > counterpart > NA
      if (!is.na(part_val)) {
        result_row[[col]] <- part_val
      } else if (!is.na(cpart_val)) {
        result_row[[col]] <- cpart_val
      } else {
        result_row[[col]] <- NA
      }
    }
    
    result_row$part_type <- "merged"
    result_list[[i]] <- result_row
  }
  
  # Combine all results
  result <- bind_rows(result_list)
  
  # Ensure all original columns exist
  for (col in names(data)) {
    if (!(col %in% names(result))) {
      result[[col]] <- NA
    }
  }
  
  # Add merged flag if missing
  if (!("merged" %in% names(result))) {
    result$merged <- TRUE
  }
  
  return(as.data.frame(result))
}

#' Aggregate Values by Variable Type
#'
#' Helper function to aggregate multiple values appropriately by variable type.
#'
#' @param values Vector of values to aggregate
#' @param col_name Name of the column (for type lookup)
#' @param var_map Variable mapping from variable_mapping()
#' @return Single aggregated value
aggregate_values <- function(values, col_name, var_map) {
  # Remove NA values
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(NA)
  }
  
  # Determine aggregation method by variable type
  if (col_name %in% var_map$binary || col_name %in% var_map$count) {
    return(max(values))  # Take maximum for binary/count variables
  } else {
    return(mean(values))  # Take mean for continuous variables
  }
}

# =============================================================================
# REVIEW LIST CREATION
# =============================================================================

#' Create Review List for Conflicting Measurements
#'
#' Creates prioritized lists of fish specimens requiring manual review due to
#' conflicting measurements between part and counterpart. Optionally filters
#' by threshold for focused review.
#'
#' @param overlap_results List output from identify_overlaps()
#' @param use_threshold Logical, whether to filter by threshold (default TRUE)
#' @return List containing summary table, detailed metrics, and filtering info
#'
#' @examples
#' review_list <- create_review_list(overlap_results, use_threshold = TRUE)
#' priority_fish <- review_list$summary
create_review_list <- function(overlap_results, use_threshold = TRUE) {
  # Validate input
  if (!is.list(overlap_results) || !"overlap_metrics" %in% names(overlap_results)) {
    stop("Input must be the list output from identify_overlaps()")
  }
  
  metrics <- overlap_results$overlap_metrics
  var_map <- variable_mapping()
  
  # Filter metrics based on threshold preference
  filtered_metrics <- if(use_threshold) {
    metrics %>% filter(exceeds_threshold == TRUE)
  } else {
    metrics
  }
  
  # Create summary by fish with meaningful grouping
  summary <- filtered_metrics %>%
    group_by(fish_id, var_type) %>%
    summarize(
      num_overlapping = n(),
      max_relative_diff = if_else(
        first(var_type) == "continuous", 
        max(relative_diff, na.rm = TRUE), 
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    # Summarize by fish across all variable types
    group_by(fish_id) %>%
    summarize(
      continuous_diffs = sum(var_type == "continuous"),
      count_diffs = sum(var_type == "count"), 
      binary_diffs = sum(var_type == "binary"),
      total_diffs = n(),
      max_relative_diff = max(max_relative_diff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Sort by priority (most differences first)
    arrange(desc(total_diffs), desc(continuous_diffs))
  
  # Create detailed breakdown for investigation
  details <- filtered_metrics %>%
    arrange(fish_id, var_type, desc(relative_diff))
  
  return(list(
    summary = as.data.frame(summary),
    details = as.data.frame(details),
    filtered_metrics = as.data.frame(filtered_metrics),
    use_threshold = use_threshold
  ))
}

#' Export Review Lists to CSV Files
#'
#' Exports review lists to CSV files for manual review by domain experts.
#'
#' @param review_results List output from create_review_list()
#' @param file_prefix Character prefix for output files (default "fish_review")
#' @param output_dir Optional output directory path
#' @return List of file paths created
#'
#' @examples
#' file_paths <- export_review_list(review_results, "priority_review", "output/")
export_review_list <- function(review_results, file_prefix = "fish_review", 
                               output_dir = NULL) {
  # Validate input
  if (!is.list(review_results) || !all(c("summary", "details") %in% names(review_results))) {
    stop("Input must be the list output from create_review_list()")
  }
  
  # Create output directory if specified
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    summary_path <- file.path(output_dir, paste0(file_prefix, "_summary.csv"))
    details_path <- file.path(output_dir, paste0(file_prefix, "_details.csv"))
  } else {
    summary_path <- paste0(file_prefix, "_summary.csv")
    details_path <- paste0(file_prefix, "_details.csv")
  }
  
  # Export files
  if (!require("readr", quietly = TRUE)) {
    # Fallback to base R if readr not available
    write.csv(review_results$summary, summary_path, row.names = FALSE)
    write.csv(review_results$details, details_path, row.names = FALSE)
  } else {
    readr::write_csv(review_results$summary, summary_path)
    readr::write_csv(review_results$details, details_path)
  }
  
  # Return file paths for confirmation
  return(list(
    summary = summary_path,
    details = details_path
  ))
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Print Summary Statistics for Cleaning Steps
#'
#' Helper function to print consistent summary information for each cleaning step.
#' Works well with your existing logging system.
#'
#' @param data_before Data frame before processing step
#' @param data_after Data frame after processing step (optional)
#' @param step_name Name of the processing step
#' @param additional_info Additional information to display
#'
#' @examples
#' print_step_summary(raw_data, corrected_data, "Special Case Handling")
print_step_summary <- function(data_before, data_after = NULL, step_name, additional_info = NULL) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("STEP:", step_name, "\n")
  cat("Before:", nrow(data_before), "rows")
  if ("fish_id" %in% names(data_before)) {
    cat(",", length(unique(data_before$fish_id)), "unique fish")
  }
  cat("\n")
  
  if (!is.null(data_after)) {
    cat("After: ", nrow(data_after), "rows")
    if ("fish_id" %in% names(data_after)) {
      cat(",", length(unique(data_after$fish_id)), "unique fish")
    }
    cat("\n")
    
    row_change <- nrow(data_after) - nrow(data_before)
    if ("fish_id" %in% names(data_before) && "fish_id" %in% names(data_after)) {
      fish_change <- length(unique(data_after$fish_id)) - length(unique(data_before$fish_id))
      cat("Change:", sprintf("%+d rows, %+d fish\n", row_change, fish_change))
    } else {
      cat("Change:", sprintf("%+d rows\n", row_change))
    }
  }
  
  if (!is.null(additional_info)) {
    cat("Info:  ", additional_info, "\n")
  }
  
  cat(paste(rep("=", 50), collapse = ""), "\n")
}

# =============================================================================
# END OF MORPHOLOGY CLEANING FUNCTIONS
# =============================================================================

cat("✓ Morphology cleaning functions loaded successfully\n")
cat("Available functions: variable_mapping, handle_special_cases, identify_overlaps,\n")
cat("                     merge_non_overlap, create_review_list, export_review_list,\n") 
cat("                     print_step_summary\n")