# =============================================================================
# PROJECT-SPECIFIC VALIDATION CHECKS FOR FISH MORPHOLOGY PIPELINE
# =============================================================================
# File: 2.scripts/project_validation.R
# Purpose: Custom validation checks specific to the fish morphology project
# Author: [Your name]
# Created: [Date]
# =============================================================================

# Required packages
required_packages <- c("dplyr", "tidyr", "stringr")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    warning(paste("Package", pkg, "not available. Some functions may not work."))
  }
}

# =============================================================================
# PROJECT-SPECIFIC VALIDATION CHECKS
# =============================================================================

project_specific_checks <- list(
  
  #' Validate that relative difference calculations are working correctly
  relative_difference_validation = function(data) {
    if(!all(c("fish_id", "part_type", "Scale_10mm") %in% names(data))) {
      return(list(passed = TRUE, message = "Missing required columns for relative difference check"))
    }
    
    # Check fish that have both P and C with scale data
    fish_with_both <- data %>%
      group_by(fish_id) %>%
      summarise(
        has_P = any(part_type == "P" & !is.na(Scale_10mm)),
        has_C = any(part_type == "C" & !is.na(Scale_10mm)),
        .groups = "drop"
      ) %>%
      filter(has_P & has_C)
    
    if(nrow(fish_with_both) == 0) {
      return(list(passed = TRUE, message = "No fish have both P and C with scale data"))
    }
    
    # For a sample of these fish, verify that relative differences make sense
    sample_fish <- if(nrow(fish_with_both) > 10) {
      sample_n(fish_with_both, 10)$fish_id
    } else {
      fish_with_both$fish_id
    }
    
    # Check that scales are similar between P and C for the same fish
    scale_consistency <- data %>%
      filter(fish_id %in% sample_fish & !is.na(Scale_10mm)) %>%
      select(fish_id, part_type, Scale_10mm) %>%
      pivot_wider(names_from = part_type, values_from = Scale_10mm, 
                  names_prefix = "scale_") %>%
      filter(!is.na(scale_P) & !is.na(scale_C)) %>%
      mutate(
        scale_diff = abs(scale_P - scale_C),
        relative_scale_diff = scale_diff / ((scale_P + scale_C) / 2),
        scales_very_different = relative_scale_diff > 0.2  # 20% difference
      )
    
    if(nrow(scale_consistency) == 0) {
      return(list(passed = TRUE, message = "No fish available for scale consistency check"))
    }
    
    inconsistent_scales <- sum(scale_consistency$scales_very_different, na.rm = TRUE)
    
    list(
      passed = inconsistent_scales == 0,
      message = ifelse(inconsistent_scales == 0,
                      paste("Scale measurements consistent between P/C for", nrow(scale_consistency), "fish checked"),
                      paste(inconsistent_scales, "fish have very different scales between P and C"))
    )
  },
  
  #' Validate that the overlap identification worked correctly
  overlap_detection_validation = function(overlap_results) {
    # Check that fish are properly partitioned
    overlap_fish_ids <- unique(overlap_results$overlap_fish$fish_id)
    non_overlap_fish_ids <- unique(overlap_results$non_overlap_fish$fish_id)
    
    # Check for fish appearing in both groups (should be impossible)
    fish_in_both <- intersect(overlap_fish_ids, non_overlap_fish_ids)
    
    # Check that the overlap metrics make sense
    metrics_issues <- 0
    if(!is.null(overlap_results$overlap_metrics)) {
      metrics_issues <- overlap_results$overlap_metrics %>%
        filter(
          is.na(relative_diff) | 
          relative_diff < 0 | 
          (var_type == "continuous" & relative_diff > 5)  # >500% difference seems extreme
        ) %>%
        nrow()
    }
    
    list(
      passed = length(fish_in_both) == 0 && metrics_issues == 0,
      message = paste("Overlap detection results:",
                     length(overlap_fish_ids), "overlap fish,",
                     length(non_overlap_fish_ids), "non-overlap fish,",
                     length(fish_in_both), "fish in both groups (should be 0),",
                     metrics_issues, "metric calculation issues")
    )
  },
  
  #' Validate that merging preserved all measurement data appropriately
  merge_preservation_check = function(original_data, merged_data) {
    # Check that important measurements weren't lost during merging
    key_vars <- c("SL", "CAV", "DS1", "DS2", "DS3")  # Focus on key measurements
    available_vars <- intersect(key_vars, names(original_data))
    
    if(length(available_vars) == 0) {
      return(list(passed = TRUE, message = "No key variables to check"))
    }
    
    # Count non-missing values before and after merging
    original_counts <- original_data %>%
      summarise_at(available_vars, ~sum(!is.na(.))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "original_count")
    
    merged_counts <- merged_data %>%
      summarise_at(available_vars, ~sum(!is.na(.))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "merged_count")
    
    comparison <- original_counts %>%
      left_join(merged_counts, by = "variable") %>%
      mutate(
        count_change = merged_count - original_count,
        percent_change = round(100 * count_change / original_count, 1)
      )
    
    # Allow some loss due to merging conflicts, but flag major losses
    major_losses <- comparison %>%
      filter(percent_change < -20) %>%  # More than 20% loss
      nrow()
    
    list(
      passed = major_losses == 0,
      message = if(major_losses == 0) {
        paste("Measurement preservation good: avg change =", 
              round(mean(comparison$percent_change), 1), "%")
      } else {
        paste(major_losses, "variables lost >20% of measurements during merging")
      }
    )
  },
  
  #' Validate age-morphology merge quality
  age_merge_quality_check = function(data) {
    if(!all(c("bin", "CSTRAT", "YEAR") %in% names(data))) {
      return(list(passed = TRUE, message = "Missing age data columns"))
    }
    
    # Check merge success by bin pattern
    merge_summary <- data %>%
      mutate(
        has_age_data = !is.na(CSTRAT),
        bin_pattern = str_extract(bin, "L\\d{4}")  # Extract just the L#### part
      ) %>%
      group_by(bin_pattern) %>%
      summarise(
        total_fish = n(),
        fish_with_age = sum(has_age_data),
        merge_rate = fish_with_age / total_fish,
        .groups = "drop"
      ) %>%
      arrange(desc(total_fish))
    
    if(nrow(merge_summary) == 0) {
      return(list(passed = TRUE, message = "No bin data available for merge quality check"))
    }
    
    # Flag bins with very low merge rates (might indicate data issues)
    low_merge_bins <- merge_summary %>%
      filter(total_fish >= 5 & merge_rate < 0.3) %>%  # Bins with 5+ fish but <30% merge rate
      nrow()
    
    overall_merge_rate <- sum(merge_summary$fish_with_age) / sum(merge_summary$total_fish)
    
    list(
      passed = low_merge_bins <= 2 && overall_merge_rate > 0.5,  # Allow some bins to have issues
      message = paste("Age merge quality: overall rate =", 
                     round(100 * overall_merge_rate, 1), "%,",
                     low_merge_bins, "bins with poor merge rates")
    )
  },
  
  #' Check that ID parsing worked correctly
  id_parsing_validation = function(data) {
    if(!all(c("ID", "fish_id", "part_type", "bin") %in% names(data))) {
      return(list(passed = TRUE, message = "Missing required ID columns"))
    }
    
    # Check for parsing failures
    parsing_issues <- data %>%
      summarise(
        missing_fish_id = sum(is.na(fish_id)),
        missing_part_type = sum(is.na(part_type)),
        missing_bin = sum(is.na(bin)),
        invalid_part_type = sum(!part_type %in% c("P", "C", "merged"), na.rm = TRUE)
      )
    
    total_issues <- sum(parsing_issues)
    
    list(
      passed = total_issues == 0,
      message = ifelse(total_issues == 0,
                      "ID parsing successful for all specimens",
                      paste("ID parsing issues:", 
                           parsing_issues$missing_fish_id, "missing fish_id,",
                           parsing_issues$missing_part_type, "missing part_type,",
                           parsing_issues$missing_bin, "missing bin,",
                           parsing_issues$invalid_part_type, "invalid part_type"))
    )
  },
  
  #' Validate special case handling for spine and girdle logic
  special_case_logic_validation = function(data) {
    spine_vars <- c("MDS1", "MDS1NA", "DS1", "MDS2", "MDS2NA", "DS2", "MDS3", "MDS3NA", "DS3", "MPT")
    pelvic_vars <- c("MPSP", "MPSPNA", "PSP.L", "PSP.R", "TPG")
    
    available_spine <- intersect(spine_vars, names(data))
    available_pelvic <- intersect(pelvic_vars, names(data))
    
    issues <- 0
    
    # Check spine logic
    if(length(available_spine) >= 4) {  # Need at least MDS, MDSNA, DS for one spine
      spine_issues <- data %>%
        filter(
          # Cases that should have been corrected
          (!is.na(MDS1) & !is.na(MDS1NA) & !is.na(DS1) & 
           MDS1 == 0 & MDS1NA == 1 & !is.na(DS1)) |
          (!is.na(MDS2) & !is.na(MDS2NA) & !is.na(DS2) & 
           MDS2 == 0 & MDS2NA == 1 & !is.na(DS2)) |
          (!is.na(MDS3) & !is.na(MDS3NA) & !is.na(DS3) & 
           MDS3 == 0 & MDS3NA == 1 & !is.na(DS3))
        ) %>%
        nrow()
      
      issues <- issues + spine_issues
    }
    
    # Check pelvic logic
    if(length(available_pelvic) >= 4) {
      pelvic_issues <- data %>%
        filter(
          (!is.na(MPSP) & !is.na(MPSPNA) & 
           MPSP == 0 & MPSPNA == 1 & (!is.na(PSP.L) | !is.na(PSP.R)))
        ) %>%
        nrow()
      
      issues <- issues + pelvic_issues
    }
    
    list(
      passed = issues == 0,
      message = ifelse(issues == 0,
                      "Special case logic handling appears correct",
                      paste(issues, "specimens show inconsistent spine/pelvic logic"))
    )
  }
)

# =============================================================================
# MAIN PROJECT VALIDATION FUNCTION
# =============================================================================

#' Run all project-specific validations
run_project_validations <- function(original_data = NULL, corrected_data = NULL, 
                                   overlap_results = NULL, merged_data = NULL, 
                                   final_data = NULL) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("RUNNING PROJECT-SPECIFIC VALIDATIONS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  results <- list()
  
  # Test 1: ID parsing validation
  if(!is.null(corrected_data)) {
    results$id_parsing <- project_specific_checks$id_parsing_validation(corrected_data)
    print_validation_result("ID Parsing", results$id_parsing)
  }
  
  # Test 2: Special case logic validation
  if(!is.null(corrected_data)) {
    results$special_case_logic <- project_specific_checks$special_case_logic_validation(corrected_data)
    print_validation_result("Special Case Logic", results$special_case_logic)
  }
  
  # Test 3: Relative difference validation
  if(!is.null(corrected_data)) {
    results$relative_diff <- project_specific_checks$relative_difference_validation(corrected_data)
    print_validation_result("Relative Difference Calculation", results$relative_diff)
  }
  
  # Test 4: Overlap detection validation  
  if(!is.null(overlap_results)) {
    results$overlap_detection <- project_specific_checks$overlap_detection_validation(overlap_results)
    print_validation_result("Overlap Detection", results$overlap_detection)
  }
  
  # Test 5: Merge preservation check
  if(!is.null(original_data) && !is.null(merged_data)) {
    results$merge_preservation <- project_specific_checks$merge_preservation_check(original_data, merged_data)
    print_validation_result("Merge Data Preservation", results$merge_preservation)
  }
  
  # Test 6: Age merge quality
  if(!is.null(final_data)) {
    results$age_merge <- project_specific_checks$age_merge_quality_check(final_data)
    print_validation_result("Age Data Merge Quality", results$age_merge)
  }
  
  # Summary
  total_tests <- length(results)
  passed_tests <- sum(sapply(results, function(x) x$passed))
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("PROJECT VALIDATION SUMMARY:", passed_tests, "of", total_tests, "tests passed\n")
  if(passed_tests < total_tests) {
    cat("⚠ Consider reviewing failed validations before proceeding\n")
  }
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  return(results)
}

#' Helper function to print validation results nicely
print_validation_result <- function(test_name, result) {
  status <- if(result$passed) "✓ PASS" else "✗ FAIL"
  cat(sprintf("%-35s: %s - %s\n", test_name, status, result$message))
}

cat("✓ Project-specific validation functions loaded successfully\n")
cat("Available functions: project_specific_checks, run_project_validations\n")