#' Report Summary Functions
#'
#' Clean, modular functions to generate summary tables and statistics
#' for the stickleback morphology and paleo-ecology analysis pipeline.

# ========================================================================= #
# PIPELINE PROCESSING SUMMARIES ----
# ========================================================================= #

#' Create Morphology Processing Pipeline Summary
#'
#' @param morph_raw Raw morphology data
#' @param morph_with_ids Morphology data with IDs extracted
#' @param morph_with_scales Morphology data with scale issues resolved
#' @param morph_non_overlap Morphology data after removing conflicts
#' @param morph_final Final merged morphology data
#' @return Data frame with processing stage statistics
morphology_processing_summary <- function(morph_raw, morph_with_ids, morph_with_scales, 
                                          morph_non_overlap, morph_final) {
  data.frame(
    Stage = c("Raw Morphology", "With IDs", "With Scales", "After QC", "Final Merged"),
    Records = c(
      nrow(morph_raw),
      nrow(morph_with_ids), 
      nrow(morph_with_scales),
      nrow(morph_non_overlap),
      nrow(morph_final)
    ),
    Fish = c(
      length(unique(morph_raw$ID)),
      length(unique(morph_with_ids$fish_id[!is.na(morph_with_ids$fish_id)])),
      length(unique(morph_with_scales$fish_id)),
      length(unique(morph_non_overlap$fish_id)),
      length(unique(morph_final$fish_id))
    )
  )
}

#' Create Paleo-Ecology Processing Summary
#'
#' @param paleo_raw Raw paleo-ecology data
#' @param paleo_merged_counts Merged microscopy counts
#' @return Data frame with paleo processing statistics
paleo_processing_summary <- function(paleo_raw, paleo_merged_counts) {
  data.frame(
    Metric = c("Original microscopy lines", "Merged geological samples", "Data reduction"),
    Value = c(
      nrow(paleo_raw),
      nrow(paleo_merged_counts),
      paste0(round((1 - nrow(paleo_merged_counts)/nrow(paleo_raw)) * 100, 1), "%")
    )
  )
}

#' Create Field Order Processing Summary Table
#'
#' @param fieldorder_summary_data Existing field order summary object
#' @return Data frame with key processing statistics
fieldorder_summary_table <- function(fieldorder_summary_data) {
  if(is.null(fieldorder_summary_data)) {
    return(data.frame(Metric = "Field order processing data not available", Value = ""))
  }
  
  data.frame(
    Metric = c("Input Records", "Clean Records", "Flagged Records", 
               "Unique LSPECs (Input)", "Unique LSPECs (Clean)", "Data Retention Rate"),
    Value = c(
      fieldorder_summary_data$record_counts$total_input_records,
      fieldorder_summary_data$record_counts$total_clean_records,
      fieldorder_summary_data$record_counts$total_flagged_records,
      fieldorder_summary_data$record_counts$unique_lspecs_input,
      fieldorder_summary_data$record_counts$unique_lspecs_clean,
      paste0(fieldorder_summary_data$record_counts$data_retention_rate, "%")
    )
  )
}

# ========================================================================= #
# DATA QUALITY SUMMARIES ----
# ========================================================================= #

#' Create Scale Bar Availability Summary
#'
#' @param flagged_fish_missing_scales Vector of fish IDs missing scales
#' @param morph_with_ids Morphology data with IDs
#' @return List with scale availability statistics
scale_availability_summary <- function(flagged_fish_missing_scales, morph_with_ids) {
  total_fish <- length(unique(morph_with_ids$fish_id[!is.na(morph_with_ids$fish_id)]))
  missing_count <- length(flagged_fish_missing_scales)
  
  list(
    fish_missing_scales = missing_count,
    total_fish = total_fish,
    percentage_missing = round(missing_count / total_fish * 100, 1)
  )
}

#' Create Part vs Counterpart Distribution Summary
#'
#' @param morph_with_ids Morphology data with part types
#' @param morph_final Final morphology data
#' @return List with part/counterpart distribution tables
part_counterpart_summary <- function(morph_with_ids, morph_final) {
  initial_distribution <- as.data.frame(table(morph_with_ids$part_type))
  names(initial_distribution) <- c("Part Type", "Count")
  
  final_distribution <- NULL
  if("part_type" %in% names(morph_final)) {
    final_distribution <- as.data.frame(table(morph_final$part_type))
    names(final_distribution) <- c("Final Part Type", "Count")
  }
  
  list(
    initial = initial_distribution,
    final = final_distribution
  )
}

#' Create Data Completeness Summary
#'
#' @param morph_final Final morphology data
#' @return Data frame with completeness percentages by variable
data_completeness_summary <- function(morph_final) {
  var_map <- variable_mapping()
  
  # Only use quantitative variables (continuous + count) since binary columns are removed
  analysis_vars <- c(var_map$continuous, var_map$count)
  
  completeness <- morph_final %>%
    select(all_of(analysis_vars)) %>%
    summarise_all(~sum(!is.na(.)) / n() * 100) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Completeness") %>%
    arrange(desc(Completeness))
  
  as.data.frame(completeness)
}

# ========================================================================= #
# MICROFOSSIL SUMMARIES ----
# ========================================================================= #

#' Create Microfossil Type Distribution Summary
#'
#' @param paleo_merged_counts Merged paleo data
#' @return Data frame with microfossil type statistics
microfossil_type_summary <- function(paleo_merged_counts) {
  type_summary <- paleo_merged_counts %>%
    count(Microfossil_Type, name = "Samples") %>%
    arrange(desc(Samples))
  
  as.data.frame(type_summary)
}

#' Create Sample Type Classification Summary
#'
#' @param paleo_with_ids Paleo data with sample types
#' @return Data frame with sample type breakdown or NULL if not available
sample_type_summary <- function(paleo_with_ids) {
  if(!"sample_type" %in% names(paleo_with_ids)) {
    return(NULL)
  }
  
  sample_types <- paleo_with_ids %>%
    distinct(Sample_ID, sample_type) %>%
    count(sample_type)
  
  as.data.frame(sample_types)
}

# ========================================================================= #
# FIELD ORDER QUALITY SUMMARIES ----
# ========================================================================= #

#' Create Field Order Data Quality Summary Table
#'
#' @param fieldorder_summary_data Field order processing summary object
#' @return Data frame with quality metrics
fieldorder_quality_table <- function(fieldorder_summary_data) {
  if(is.null(fieldorder_summary_data)) {
    return(data.frame(Metric = "Field order quality data not available", Count = ""))
  }
  
  data.frame(
    Metric = c("Complete Records", "Records with Missing Data", "Critical Issues", 
               "High Priority Issues", "Needs Manual Review"),
    Count = c(
      fieldorder_summary_data$data_quality$complete_records,
      fieldorder_summary_data$data_quality$missing_data_records,
      fieldorder_summary_data$flag_summary$critical_issues,
      fieldorder_summary_data$flag_summary$high_priority_issues,
      fieldorder_summary_data$flag_summary$needs_manual_review
    )
  )
}

#' Create Field Order Completeness Summary Table
#'
#' @param fieldorder_summary_data Field order processing summary object
#' @return Data frame with completeness percentages by column
fieldorder_completeness_table <- function(fieldorder_summary_data) {
  if(is.null(fieldorder_summary_data)) {
    return(data.frame(Column = "Data not available", Completeness = ""))
  }
  
  data.frame(
    Column = c("CSTRAT", "ISTRAT", "YEAR", "INT"),
    Completeness = c(
      fieldorder_summary_data$data_quality$completeness_percentages$cstrat,
      fieldorder_summary_data$data_quality$completeness_percentages$istrat,
      fieldorder_summary_data$data_quality$completeness_percentages$year,
      fieldorder_summary_data$data_quality$completeness_percentages$int
    )
  )
}

#' Create Duplicate Analysis Summary
#'
#' @param fieldorder_lspec_classification LSPEC classification results
#' @return Data frame with duplicate statistics
duplicate_analysis_summary <- function(fieldorder_lspec_classification) {
  if(is.null(fieldorder_lspec_classification)) {
    return(data.frame(Category = "Duplicate analysis not available", Count = "", Percentage = ""))
  }
  
  data.frame(
    Category = c("Problematic LSPECs", "Clean LSPECs", "Total LSPECs"),
    Count = c(
      fieldorder_lspec_classification$n_problematic,
      fieldorder_lspec_classification$n_clean,
      fieldorder_lspec_classification$n_total
    ),
    Percentage = c(
      round(100 * fieldorder_lspec_classification$n_problematic / fieldorder_lspec_classification$n_total, 1),
      round(100 * fieldorder_lspec_classification$n_clean / fieldorder_lspec_classification$n_total, 1),
      100
    )
  )
}

# ========================================================================= #
# INTEGRATION SUMMARIES ----
# ========================================================================= #

#' Create LSPEC Integration Summary
#'
#' @param morph_final Final morphology data
#' @param paleo_with_ids Paleo data with IDs
#' @return Data frame with integration potential statistics
lspec_integration_summary <- function(morph_final, paleo_with_ids) {
  morph_lspecs <- unique(morph_final$LSPEC[!is.na(morph_final$LSPEC)])
  paleo_lspecs <- unique(paleo_with_ids$LSPEC[!is.na(paleo_with_ids$LSPEC)])
  
  data.frame(
    Dataset = c("Morphology", "Paleo-Ecology", "Potential Links"),
    LSPEC_Count = c(
      length(morph_lspecs),
      length(paleo_lspecs), 
      length(intersect(morph_lspecs, paleo_lspecs))
    )
  )
}

#' Create Final Pipeline Status Summary
#'
#' @param morph_final Final morphology data
#' @param paleo_merged_counts Merged paleo data
#' @param morph_lspecs Morphology LSPECs
#' @param paleo_lspecs Paleo LSPECs
#' @return Data frame with pipeline completion status
pipeline_status_summary <- function(morph_final, paleo_merged_counts, morph_lspecs = NULL, paleo_lspecs = NULL) {
  if(is.null(morph_lspecs)) {
    morph_lspecs <- unique(morph_final$LSPEC[!is.na(morph_final$LSPEC)])
  }
  if(is.null(paleo_lspecs)) {
    paleo_lspecs <- unique(paleo_merged_counts$LSPEC[!is.na(paleo_merged_counts$LSPEC)])
  }
  
  data.frame(
    Component = c("Morphology Pipeline", "Paleo Pipeline", "Data Integration"),
    Status = c("Complete", "Complete", "Ready"),
    Output = c(
      paste(nrow(morph_final), "fish records"),
      paste(nrow(paleo_merged_counts), "merged samples"),
      paste(length(intersect(morph_lspecs, paleo_lspecs)), "linkable specimens")
    )
  )
}