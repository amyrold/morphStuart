# =============================================================================
# FISH MORPHOLOGY DATA QUALITY ASSESSMENT FUNCTIONS
# =============================================================================
# File: 2.scripts/quality_assessment.R
# Purpose: Data quality control and assessment functions
# Author: [Your name]
# Created: [Date]
# =============================================================================

# Required packages
required_packages <- c("dplyr", "tidyr", "ggplot2")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    warning(paste("Package", pkg, "not available. Some functions may not work."))
  }
}

# =============================================================================
# MORPHOLOGICAL CONSISTENCY CHECKS
# =============================================================================

#' Fish-specific validation checks
fish_data_checks <- list(
  
  # Check that merged fish don't have conflicting part_types
  consistent_part_types = function(data) {
    if(!all(c("fish_id", "part_type") %in% names(data))) {
      return(list(passed = TRUE, message = "No part_type or fish_id columns to check"))
    }
    
    inconsistent <- data %>%
      group_by(fish_id) %>%
      summarise(unique_types = n_distinct(part_type), .groups = "drop") %>%
      filter(unique_types > 2)  # Should only have P, C, or merged
    
    list(
      passed = nrow(inconsistent) == 0,
      message = ifelse(nrow(inconsistent) == 0, 
                       "All fish have consistent part types",
                       paste(nrow(inconsistent), "fish have >2 part types"))
    )
  },
  
  # Check spine logic consistency
  spine_logic_check = function(data) {
    spine_vars <- c("MDS1", "MDS1NA", "DS1", "MDS2", "MDS2NA", "DS2", "MDS3", "MDS3NA", "DS3")
    available_spine_vars <- intersect(spine_vars, names(data))
    
    if(length(available_spine_vars) < 9) {
      return(list(passed = TRUE, message = "Spine variables not all present"))
    }
    
    # Check for logical inconsistencies
    inconsistent <- data %>%
      filter(
        (MDS1 == 1 & is.na(DS1)) |  # Says spine observed but no measurement
          (MDS1 == 0 & MDS1NA == 0 & !is.na(DS1) & DS1 != 0) |  # Says spine not observed but has measurement
          (MDS2 == 1 & is.na(DS2)) |
          (MDS2 == 0 & MDS2NA == 0 & !is.na(DS2) & DS2 != 0) |
          (MDS3 == 1 & is.na(DS3)) |
          (MDS3 == 0 & MDS3NA == 0 & !is.na(DS3) & DS3 != 0)
      )
    
    list(
      passed = nrow(inconsistent) == 0,
      message = ifelse(nrow(inconsistent) == 0,
                       "Spine logic is consistent",
                       paste(nrow(inconsistent), "rows have spine logic inconsistencies"))
    )
  },
  
  # Check body proportion relationships
  body_proportion_check = function(data) {
    if(!all(c("SL", "ECT", "CLE") %in% names(data))) {
      return(list(passed = TRUE, message = "Missing variables for body proportion check"))
    }
    
    proportion_data <- data %>%
      filter(!is.na(SL) & !is.na(ECT) & !is.na(CLE)) %>%
      mutate(
        ect_sl_ratio = ECT / SL,
        cle_sl_ratio = CLE / SL,
        extreme_ect = ect_sl_ratio > 1.0 | ect_sl_ratio < 0.1,
        extreme_cle = cle_sl_ratio > 1.0 | cle_sl_ratio < 0.1
      )
    
    if(nrow(proportion_data) == 0) {
      return(list(passed = TRUE, message = "No specimens with complete body measurement data"))
    }
    
    extreme_count <- sum(proportion_data$extreme_ect | proportion_data$extreme_cle)
    
    list(
      passed = extreme_count < (nrow(proportion_data) * 0.05),  # Allow 5% outliers
      message = ifelse(extreme_count == 0,
                       "All body proportions within reasonable ranges",
                       paste(extreme_count, "specimens have extreme body proportions"))
    )
  },
  
  # Check count variable logic
  count_variable_logic = function(data) {
    count_vars <- c("MDF", "MAF", "MCV", "MAV", "MPT", "MPSP")
    available_counts <- intersect(count_vars, names(data))
    
    if(length(available_counts) == 0) {
      return(list(passed = TRUE, message = "No count variables available"))
    }
    
    # Check if fish_id exists, if not use row numbers
    if("fish_id" %in% names(data)) {
      count_data <- data %>%
        select(fish_id, all_of(available_counts)) %>%
        pivot_longer(-fish_id, names_to = "variable", values_to = "count")
    } else {
      count_data <- data %>%
        mutate(row_id = row_number()) %>%
        select(row_id, all_of(available_counts)) %>%
        pivot_longer(-row_id, names_to = "variable", values_to = "count")
    }
    
    count_data <- count_data %>%
      filter(!is.na(count)) %>%
      mutate(
        negative_count = count < 0,
        extremely_high = count > 100  # Adjust threshold as needed
      )
    
    if(nrow(count_data) == 0) {
      return(list(passed = TRUE, message = "No count data available"))
    }
    
    issues <- sum(count_data$negative_count | count_data$extremely_high)
    
    list(
      passed = issues == 0,
      message = ifelse(issues == 0,
                       "All count variables have reasonable values",
                       paste(issues, "count measurements have impossible values"))
    )
  }
)

# =============================================================================
# DATA QUALITY ASSESSMENT
# =============================================================================

#' Comprehensive data quality assessment
assess_data_quality <- function(data, create_plots = TRUE) {
  # Get variable mappings (with fallback)
  if(exists("variable_mapping")) {
    var_map <- variable_mapping()
  } else {
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
  
  quality_report <- list(
    timestamp = Sys.time(),
    total_specimens = nrow(data),
    unique_fish = if("fish_id" %in% names(data)) length(unique(data$fish_id)) else NA,
    
    # Missing data summary
    missing_data_summary = NULL,
    
    # Outlier detection
    outlier_summary = NULL,
    
    # Measurement distributions
    distribution_summary = NULL,
    
    # Data flags
    quality_flags = list()
  )
  
  # Missing data analysis
  if(length(var_map$all) > 0) {
    available_vars <- intersect(var_map$all, names(data))
    
    if(length(available_vars) > 0) {
      quality_report$missing_data_summary <- data %>%
        summarise_at(available_vars, ~sum(is.na(.))) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
        mutate(
          missing_percent = round(100 * missing_count / nrow(data), 2),
          var_type = case_when(
            variable %in% var_map$continuous ~ "continuous",
            variable %in% var_map$count ~ "count",
            variable %in% var_map$binary ~ "binary",
            TRUE ~ "other"
          )
        ) %>%
        arrange(desc(missing_percent))
    }
  }
  
  # Outlier detection for continuous variables
  continuous_vars <- intersect(var_map$continuous, names(data))
  if(length(continuous_vars) > 0) {
    # Check if fish_id exists, if not use row numbers
    if("fish_id" %in% names(data)) {
      outlier_data <- data %>%
        select(fish_id, all_of(continuous_vars)) %>%
        pivot_longer(-fish_id, names_to = "variable", values_to = "value")
    } else {
      outlier_data <- data %>%
        mutate(row_id = row_number()) %>%
        select(row_id, all_of(continuous_vars)) %>%
        pivot_longer(-row_id, names_to = "variable", values_to = "value")
    }
    
    outlier_data <- outlier_data %>%
      filter(!is.na(value)) %>%
      group_by(variable) %>%
      mutate(
        q1 = quantile(value, 0.25),
        q3 = quantile(value, 0.75),
        iqr = q3 - q1,
        lower_bound = q1 - 1.5 * iqr,
        upper_bound = q3 + 1.5 * iqr,
        is_outlier = value < lower_bound | value > upper_bound
      ) %>%
      ungroup()
    
    quality_report$outlier_summary <- outlier_data %>%
      group_by(variable) %>%
      summarise(
        n_observations = n(),
        n_outliers = sum(is_outlier),
        outlier_percent = round(100 * n_outliers / n_observations, 2),
        min_value = min(value),
        max_value = max(value),
        .groups = "drop"
      ) %>%
      arrange(desc(outlier_percent))
    
    # Flag variables with excessive outliers
    if(any(quality_report$outlier_summary$outlier_percent > 10)) {
      quality_report$quality_flags$excessive_outliers <- 
        quality_report$outlier_summary %>%
        filter(outlier_percent > 10) %>%
        pull(variable)
    }
  }
  
  # Distribution analysis
  if(length(continuous_vars) > 0) {
    # Simplified distribution analysis
    dist_list <- list()
    
    for(var in continuous_vars) {
      if(var %in% names(data)) {
        values <- data[[var]]
        values <- values[!is.na(values)]
        
        if(length(values) > 0) {
          dist_list[[var]] <- data.frame(
            variable = var,
            mean = mean(values),
            median = median(values),
            sd = sd(values),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if(length(dist_list) > 0) {
      quality_report$distribution_summary <- do.call(rbind, dist_list)
      rownames(quality_report$distribution_summary) <- NULL
    }
  }
  
  # Flag completely missing variables
  if(!is.null(quality_report$missing_data_summary)) {
    completely_missing <- quality_report$missing_data_summary %>%
      filter(missing_percent == 100) %>%
      pull(variable)
    
    if(length(completely_missing) > 0) {
      quality_report$quality_flags$completely_missing_variables <- completely_missing
    }
  }
  
  # Create visualizations if requested
  if(create_plots && !is.null(quality_report$missing_data_summary)) {
    # Missing data visualization
    p1 <- quality_report$missing_data_summary %>%
      filter(var_type == "continuous") %>%
      ggplot(aes(x = reorder(variable, missing_percent), y = missing_percent)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Missing Data by Continuous Variable",
           x = "Variable", y = "Percent Missing") +
      theme_minimal()
    
    print(p1)
    
    # Outlier visualization
    if(!is.null(quality_report$outlier_summary) && nrow(quality_report$outlier_summary) > 0) {
      p2 <- quality_report$outlier_summary %>%
        ggplot(aes(x = reorder(variable, outlier_percent), y = outlier_percent)) +
        geom_col(fill = "coral") +
        coord_flip() +
        labs(title = "Outlier Percentage by Variable",
             x = "Variable", y = "Percent Outliers") +
        theme_minimal()
      
      print(p2)
    }
  }
  
  return(quality_report)
}

#' Create comprehensive data quality report
create_quality_report <- function(data, output_dir = "4.docs/reports") {
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate quality assessment
  quality_assessment <- assess_data_quality(data, create_plots = FALSE)
  
  # Create timestamp for filenames
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Export missing data summary
  if(!is.null(quality_assessment$missing_data_summary)) {
    missing_file <- file.path(output_dir, paste0("missing_data_summary_", timestamp, ".csv"))
    write.csv(quality_assessment$missing_data_summary, missing_file, row.names = FALSE)
  }
  
  # Export outlier summary  
  if(!is.null(quality_assessment$outlier_summary)) {
    outlier_file <- file.path(output_dir, paste0("outlier_summary_", timestamp, ".csv"))
    write.csv(quality_assessment$outlier_summary, outlier_file, row.names = FALSE)
  }
  
  # Create comprehensive text report
  report_file <- file.path(output_dir, paste0("data_quality_report_", timestamp, ".txt"))
  
  sink(report_file)
  cat("FISH MORPHOLOGY DATA QUALITY REPORT\n")
  cat("==================================\n\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total specimens:", quality_assessment$total_specimens, "\n")
  if(!is.na(quality_assessment$unique_fish)) {
    cat("Unique fish:", quality_assessment$unique_fish, "\n")
  }
  cat("\n")
  
  if(!is.null(quality_assessment$quality_flags) && length(quality_assessment$quality_flags) > 0) {
    cat("QUALITY FLAGS:\n")
    cat("--------------\n")
    for(flag_name in names(quality_assessment$quality_flags)) {
      cat(flag_name, ":", paste(quality_assessment$quality_flags[[flag_name]], collapse = ", "), "\n")
    }
    cat("\n")
  } else {
    cat("No quality flags raised.\n\n")
  }
  
  if(!is.null(quality_assessment$missing_data_summary)) {
    cat("MISSING DATA SUMMARY (Top 10):\n")
    cat("------------------------------\n")
    top_missing <- head(quality_assessment$missing_data_summary, 10)
    for(i in 1:nrow(top_missing)) {
      cat(sprintf("%-10s: %3.1f%% missing (%d/%d)\n", 
                  top_missing$variable[i],
                  top_missing$missing_percent[i],
                  top_missing$missing_count[i],
                  quality_assessment$total_specimens))
    }
    cat("\n")
  }
  
  if(!is.null(quality_assessment$outlier_summary)) {
    cat("OUTLIER SUMMARY (Variables with >5% outliers):\n")
    cat("----------------------------------------------\n")
    high_outlier_vars <- quality_assessment$outlier_summary %>%
      filter(outlier_percent > 5)
    
    if(nrow(high_outlier_vars) > 0) {
      for(i in 1:nrow(high_outlier_vars)) {
        cat(sprintf("%-10s: %4.1f%% outliers (range: %.1f - %.1f)\n",
                    high_outlier_vars$variable[i],
                    high_outlier_vars$outlier_percent[i],
                    high_outlier_vars$min_value[i],
                    high_outlier_vars$max_value[i]))
      }
    } else {
      cat("No variables have >5% outliers\n")
    }
  }
  
  sink()
  
  cat("Quality report saved to:", report_file, "\n")
  return(quality_assessment)
}

cat("✓ Quality assessment functions loaded successfully\n")
cat("Available functions: fish_data_checks, assess_data_quality, create_quality_report\n")