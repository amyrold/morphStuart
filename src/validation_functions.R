# =============================================================================
# FISH MORPHOLOGY DATA VALIDATION FUNCTIONS
# =============================================================================
# File: 2.scripts/validation_functions.R
# Purpose: Standalone validation and quality control functions
# Author: [Your name]
# Created: [Date]
# =============================================================================

# Required packages (will be loaded by sourcing script)
required_packages <- c("dplyr", "tidyr", "stringr", "ggplot2")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    warning(paste("Package", pkg, "not available. Some functions may not work."))
  }
}

# =============================================================================
# CORE VALIDATION FUNCTIONS
# =============================================================================

#' Initialize Logging System
#' @param project_name Name of the project for logs
#' @return List object to store log entries
initialize_log <- function(project_name = "Fish Morphology Cleaning") {
  log_entry <- list(
    project_name = project_name,
    start_time = Sys.time(),
    r_version = R.version.string,
    session_info = capture.output(sessionInfo()),
    steps = list()
  )
  
  return(log_entry)
}

#' Create comprehensive data summary
#' @param data Data frame to summarize
#' @param step_name Name of the processing step
#' @return List with detailed data statistics
create_data_summary <- function(data, step_name) {
  # This function needs the variable_mapping function
  # Make sure it's available or provide a fallback
  if(exists("variable_mapping")) {
    var_map <- variable_mapping()
  } else {
    # Fallback variable mapping
    var_map <- list(
      continuous = c("SL", "CAV", "DS1", "DS2", "DS3", "LPT", 
                     "PSP.L", "PSP.R", "TPG", "ECT", "CLE", "PMX"),
      count = c("MDF", "MAF", "MCV", "MAV", "MPT", "MPSP"),
      binary = c("MDS1", "MDS2", "MDS3", "MDS1NA", 
                 "MDS2NA", "MDS3NA", "MPSPNA", "PGNA"),
      all = NULL
    )
    var_map$all <- c(var_map$continuous, var_map$count, var_map$binary)
  }
  
  summary <- list(
    step_name = step_name,
    timestamp = Sys.time(),
    
    # Basic counts
    total_rows = nrow(data),
    unique_fish = if("fish_id" %in% names(data)) length(unique(data$fish_id)) else NA,
    unique_ids = if("ID" %in% names(data)) length(unique(data$ID)) else NA,
    
    # Part/counterpart breakdown
    part_breakdown = if("part_type" %in% names(data)) {
      table(data$part_type, useNA = "always")
    } else { NULL },
    
    # Fish with multiple rows
    fish_with_multiple_rows = if("fish_id" %in% names(data)) {
      data %>% 
        count(fish_id) %>% 
        filter(n > 1) %>% 
        nrow()
    } else { 0 },
    
    # Missing value analysis
    missing_analysis = list(),
    
    # Data range analysis
    range_analysis = list(),
    
    # File size
    estimated_size_mb = round(object.size(data) / 1024^2, 2)
  )
  
  # Detailed missing value analysis
  if(length(var_map$all) > 0) {
    available_vars <- intersect(var_map$all, names(data))
    
    if(length(available_vars) > 0) {
      missing_by_type <- data %>%
        select(all_of(available_vars)) %>%
        summarise_all(~sum(is.na(.))) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
        mutate(
          var_type = case_when(
            variable %in% var_map$continuous ~ "continuous",
            variable %in% var_map$count ~ "count", 
            variable %in% var_map$binary ~ "binary",
            TRUE ~ "other"
          ),
          missing_percent = round(100 * missing_count / nrow(data), 2)
        ) %>%
        arrange(desc(missing_count))
      
      summary$missing_analysis <- list(
        by_variable = missing_by_type,
        summary_by_type = missing_by_type %>%
          group_by(var_type) %>%
          summarise(
            total_vars = n(),
            avg_missing_percent = round(mean(missing_percent), 2),
            vars_with_no_missing = sum(missing_count == 0),
            vars_completely_missing = sum(missing_count == nrow(data)),
            .groups = "drop"
          )
      )
    }
  }
  
  # Range analysis for continuous variables
  if(length(var_map$continuous) > 0) {
    available_continuous <- intersect(var_map$continuous, names(data))
    
    if(length(available_continuous) > 0) {
      # Simplified range analysis to avoid pivot issues
      range_list <- list()
      
      for(var in available_continuous) {
        if(var %in% names(data)) {
          values <- data[[var]]
          values <- values[!is.na(values)]
          
          if(length(values) > 0) {
            range_list[[var]] <- data.frame(
              variable = var,
              min_val = min(values),
              max_val = max(values),
              median_val = median(values),
              negative_count = sum(values < 0),
              zero_count = sum(values == 0),
              has_negatives = sum(values < 0) > 0,
              has_implausible_values = min(values) < -1000 | max(values) > 50000,
              stringsAsFactors = FALSE
            )
          }
        }
      }
      
      if(length(range_list) > 0) {
        summary$range_analysis <- do.call(rbind, range_list)
        rownames(summary$range_analysis) <- NULL
      }
    }
  }
  
  return(summary)
}

#' Calculate changes in missing values between steps
calculate_missing_changes <- function(input_missing, output_missing) {
  if(is.null(input_missing) || is.null(output_missing)) {
    return("Cannot calculate - missing analysis unavailable")
  }
  
  if(!is.null(input_missing$by_variable) && !is.null(output_missing$by_variable)) {
    joined <- input_missing$by_variable %>%
      select(variable, input_missing = missing_count) %>%
      full_join(
        output_missing$by_variable %>% select(variable, output_missing = missing_count),
        by = "variable"
      ) %>%
      mutate(
        input_missing = replace_na(input_missing, 0),
        output_missing = replace_na(output_missing, 0),
        change = output_missing - input_missing
      ) %>%
      filter(change != 0)
    
    return(joined)
  } else {
    return("Cannot calculate - data structure mismatch")
  }
}

#' Log a processing step with before/after comparison
log_step <- function(log_obj, step_name, input_data, output_data = NULL, 
                     files_created = NULL, notes = NULL, warnings = NULL) {
  
  step_entry <- list(
    step_name = step_name,
    timestamp = Sys.time(),
    input_summary = create_data_summary(input_data, paste0(step_name, "_input")),
    notes = notes,
    warnings = warnings,
    files_created = files_created
  )
  
  # Add output summary if provided
  if(!is.null(output_data)) {
    step_entry$output_summary <- create_data_summary(output_data, paste0(step_name, "_output"))
    
    # Calculate changes
    step_entry$changes <- list(
      row_change = step_entry$output_summary$total_rows - step_entry$input_summary$total_rows,
      fish_change = ifelse(
        is.na(step_entry$output_summary$unique_fish) || is.na(step_entry$input_summary$unique_fish),
        NA,
        step_entry$output_summary$unique_fish - step_entry$input_summary$unique_fish
      ),
      missing_value_changes = calculate_missing_changes(
        step_entry$input_summary$missing_analysis,
        step_entry$output_summary$missing_analysis
      )
    )
  }
  
  # Add to main log
  log_obj$steps[[length(log_obj$steps) + 1]] <- step_entry
  
  # Print summary to console
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("STEP:", step_name, "\n")
  cat("Time:", format(step_entry$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Input: ", step_entry$input_summary$total_rows, "rows")
  if(!is.na(step_entry$input_summary$unique_fish)) {
    cat(",", step_entry$input_summary$unique_fish, "unique fish")
  }
  cat("\n")
  
  if(!is.null(output_data)) {
    cat("Output:", step_entry$output_summary$total_rows, "rows")
    if(!is.na(step_entry$output_summary$unique_fish)) {
      cat(",", step_entry$output_summary$unique_fish, "unique fish")
    }
    cat("\n")
    
    if(!is.na(step_entry$changes$fish_change)) {
      cat("Change:", sprintf("%+d rows, %+d fish\n", 
                             step_entry$changes$row_change,
                             step_entry$changes$fish_change))
    } else {
      cat("Change:", sprintf("%+d rows\n", step_entry$changes$row_change))
    }
  }
  
  if(!is.null(files_created)) {
    cat("Files created:", paste(files_created, collapse = ", "), "\n")
  }
  
  if(!is.null(warnings)) {
    cat("WARNINGS:", paste(warnings, collapse = "; "), "\n")
  }
  
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  return(log_obj)
}

#' Validate data integrity at processing steps
validate_step <- function(data, step_name, expected_fish_count = NULL, 
                          required_columns = NULL, custom_checks = NULL) {
  
  validation_results <- list(
    step_name = step_name,
    timestamp = Sys.time(),
    passed = TRUE,
    issues = c(),
    warnings = c()
  )
  
  # Check 1: No duplicate fish_id within same part_type (if applicable)
  if(all(c("fish_id", "part_type") %in% names(data))) {
    duplicates <- data %>%
      group_by(fish_id, part_type) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if(nrow(duplicates) > 0) {
      validation_results$warnings <- c(validation_results$warnings,
                                       paste("Found", length(unique(duplicates$fish_id)), 
                                             "fish with duplicate part_type entries"))
    }
  }
  
  # Check 2: Expected fish count
  if(!is.null(expected_fish_count) && "fish_id" %in% names(data)) {
    actual_count <- length(unique(data$fish_id))
    if(actual_count != expected_fish_count) {
      validation_results$issues <- c(validation_results$issues,
                                     paste("Expected", expected_fish_count, "fish, found", actual_count))
      validation_results$passed <- FALSE
    }
  }
  
  # Check 3: Required columns
  if(!is.null(required_columns)) {
    missing_cols <- setdiff(required_columns, names(data))
    if(length(missing_cols) > 0) {
      validation_results$issues <- c(validation_results$issues,
                                     paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      validation_results$passed <- FALSE
    }
  }
  
  # Check 4: Custom validation checks
  if(!is.null(custom_checks)) {
    for(check_name in names(custom_checks)) {
      check_result <- custom_checks[[check_name]](data)
      if(!check_result$passed) {
        validation_results$issues <- c(validation_results$issues,
                                       paste(check_name, ":", check_result$message))
        validation_results$passed <- FALSE
      }
    }
  }
  
  # Print validation results
  if(validation_results$passed) {
    cat("✓ VALIDATION PASSED for", step_name, "\n")
  } else {
    cat("✗ VALIDATION FAILED for", step_name, "\n")
    cat("Issues:", paste(validation_results$issues, collapse = "\n        "), "\n")
  }
  
  if(length(validation_results$warnings) > 0) {
    cat("⚠ Warnings:", paste(validation_results$warnings, collapse = "\n           "), "\n")
  }
  
  return(validation_results)
}

#' Export comprehensive log to files
export_log <- function(log_obj, output_dir = "4.docs/logs") {
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Create comprehensive log file
  log_file <- file.path(output_dir, paste0("processing_log_", timestamp, ".txt"))
  
  sink(log_file)
  cat("FISH MORPHOLOGY DATA PROCESSING LOG\n")
  cat("===================================\n\n")
  cat("Project:", log_obj$project_name, "\n")
  cat("Start time:", format(log_obj$start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("R version:", log_obj$r_version, "\n\n")
  
  for(i in seq_along(log_obj$steps)) {
    step <- log_obj$steps[[i]]
    cat("\n", "STEP", i, ":", step$step_name, "\n")
    cat(rep("-", 50), "\n")
    cat("Timestamp:", format(step$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
    
    if(!is.null(step$input_summary)) {
      cat("Input: ", step$input_summary$total_rows, "rows")
      if(!is.na(step$input_summary$unique_fish)) {
        cat(",", step$input_summary$unique_fish, "unique fish")
      }
      cat("\n")
    }
    
    if(!is.null(step$output_summary)) {
      cat("Output:", step$output_summary$total_rows, "rows")
      if(!is.na(step$output_summary$unique_fish)) {
        cat(",", step$output_summary$unique_fish, "unique fish")
      }
      cat("\n")
      
      if(!is.na(step$changes$fish_change)) {
        cat("Change:", sprintf("%+d rows, %+d fish\n", 
                               step$changes$row_change, step$changes$fish_change))
      }
    }
    
    if(!is.null(step$files_created)) {
      cat("Files created:", paste(step$files_created, collapse = ", "), "\n")
    }
    
    if(!is.null(step$notes)) {
      cat("Notes:", step$notes, "\n")
    }
    
    if(!is.null(step$warnings)) {
      cat("Warnings:", paste(step$warnings, collapse = "; "), "\n")
    }
  }
  
  sink()
  
  # Create summary CSV
  if(length(log_obj$steps) > 0) {
    summary_data <- map_dfr(log_obj$steps, function(step) {
      data.frame(
        step_name = step$step_name,
        timestamp = step$timestamp,
        input_rows = ifelse(is.null(step$input_summary), NA, step$input_summary$total_rows),
        input_fish = ifelse(is.null(step$input_summary), NA, step$input_summary$unique_fish),
        output_rows = ifelse(is.null(step$output_summary), NA, step$output_summary$total_rows),
        output_fish = ifelse(is.null(step$output_summary), NA, step$output_summary$unique_fish),
        files_created = ifelse(is.null(step$files_created), "", 
                               paste(step$files_created, collapse = "; ")),
        warnings = ifelse(is.null(step$warnings), "", 
                          paste(step$warnings, collapse = "; ")),
        stringsAsFactors = FALSE
      )
    })
    
    summary_file <- file.path(output_dir, paste0("processing_summary_", timestamp, ".csv"))
    write.csv(summary_data, summary_file, row.names = FALSE)
  } else {
    summary_file <- NULL
  }
  
  cat("Log exported to:", log_file, "\n")
  if(!is.null(summary_file)) {
    cat("Summary exported to:", summary_file, "\n")
  }
  
  return(list(log_file = log_file, summary_file = summary_file))
}

cat("✓ Validation functions loaded successfully\n")
cat("Available functions: initialize_log, log_step, validate_step, export_log\n")