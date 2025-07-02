#' Validate Field Order Decimal LSPECs
#'
#' Identifies all field order records with decimal LSPEC values to investigate
#' whether decimals represent fossil halves or separate specimens.
#' Uses original L..SPEC format to preserve exact decimal notation.
#'
#' @param order_raw Raw field order data frame with L..SPEC column
#' @return Data frame with decimal LSPEC analysis and grouping information
#'
#' @examples
#' decimal_review <- validate_decimal_lspecs(order_raw)
validate_decimal_lspecs <- function(order_raw) {
  if (!is.data.frame(order_raw)) {
    stop("Input must be a data frame")
  }
  if (!"L..SPEC" %in% names(order_raw)) {
    stop("Data must contain 'L..SPEC' column")
  }
  
  # Convert L..SPEC to character to preserve exact decimal notation
  decimal_analysis <- order_raw %>%
    dplyr::mutate(
      L_SPEC_char = as.character(`L..SPEC`),
      has_decimal = stringr::str_detect(L_SPEC_char, "\\.") & `L..SPEC` != floor(`L..SPEC`)
    ) %>%
    dplyr::filter(has_decimal) %>%
    dplyr::mutate(
      base_id = floor(`L..SPEC`),
      decimal_part = `L..SPEC` - base_id
    ) %>%
    dplyr::arrange(`L..SPEC`)
  
  # Create grouping summary for each base ID
  decimal_grouping <- decimal_analysis %>%
    dplyr::group_by(base_id) %>%
    dplyr::summarise(
      n_decimal_variants = n(),
      decimal_values = paste(sort(unique(`L..SPEC`)), collapse = ", "),
      decimal_parts_only = paste(sort(unique(decimal_part)), collapse = ", "),
      min_decimal = min(`L..SPEC`),
      max_decimal = max(`L..SPEC`),
      .groups = "drop"
    ) %>%
    dplyr::arrange(base_id)
  
  # Create simplified review list
  decimal_review <- decimal_analysis %>%
    dplyr::select(
      original_LSPEC = `L..SPEC`,
      base_id,
      CSTRAT, ISTRAT, YEAR, INT, STRAT..cm., PS, DSP, PT, CT
    ) %>%
    dplyr::arrange(base_id, original_LSPEC)
  
  # Save review file
  if (!dir.exists("data/review")) {
    dir.create("data/review", recursive = TRUE)
  }
  
  write.csv(decimal_review, 
            "data/review/fieldorder_decimal_lspecs_review.csv", 
            row.names = FALSE)
  
  # Report results
  multiple_variants <- sum(decimal_grouping$n_decimal_variants > 1)
  max_variants <- max(decimal_grouping$n_decimal_variants)
  
  cat("Decimal LSPEC Validation:\n")
  cat("Total decimal records found:", nrow(decimal_review), "\n")
  cat("Unique base IDs with decimals:", nrow(decimal_grouping), "\n")
  cat("Base IDs with multiple decimal variants:", multiple_variants, "\n")
  cat("Maximum variants per base ID:", max_variants, "\n")
  cat("File saved to data/review/fieldorder_decimal_lspecs_review.csv\n\n")
  
  # Show examples of multiple variants
  if (multiple_variants > 0) {
    cat("Examples of base IDs with multiple decimal variants:\n")
    examples <- decimal_grouping %>%
      dplyr::filter(n_decimal_variants > 1) %>%
      dplyr::arrange(desc(n_decimal_variants)) %>%
      dplyr::slice_head(n = 5)
    
    for (i in 1:nrow(examples)) {
      cat(sprintf("  Base %s: %s (%d variants)\n", 
                  examples$base_id[i],
                  examples$decimal_values[i],
                  examples$n_decimal_variants[i]))
    }
  }
  
  return(as.data.frame(decimal_review))
}

#' Validate Missing Morph LSPECs
#'
#' Identifies morphology LSPECs that are missing from field order data,
#' indicating fish specimens without stratigraphic context.
#' Uses standardized LSPEC format for proper cross-dataset comparison.
#'
#' @param morph_with_ids Morphology data with standardized LSPEC column
#' @param fieldorder_complete Complete field order data with standardized LSPEC column  
#' @return Data frame with missing LSPEC analysis
#'
#' @examples
#' missing_review <- validate_missing_morph_lspecs(morph_with_ids, fieldorder_final$complete)
validate_missing_morph_lspecs <- function(morph_with_ids, fieldorder_complete) {
  if (!is.data.frame(morph_with_ids) || !is.data.frame(fieldorder_complete)) {
    stop("Both inputs must be data frames")
  }
  if (!"LSPEC" %in% names(morph_with_ids) || !"LSPEC" %in% names(fieldorder_complete)) {
    stop("Both datasets must contain 'LSPEC' column")
  }
  
  # Get unique standardized LSPECs from each dataset
  morph_lspecs <- morph_with_ids %>%
    dplyr::filter(!is.na(LSPEC), LSPEC != "LNA") %>%
    dplyr::distinct(LSPEC) %>%
    dplyr::pull(LSPEC)
  
  fieldorder_lspecs <- fieldorder_complete %>%
    dplyr::filter(!is.na(LSPEC)) %>%
    dplyr::distinct(LSPEC) %>%
    dplyr::pull(LSPEC)
  
  # Find morph LSPECs missing from field order
  missing_lspecs <- setdiff(morph_lspecs, fieldorder_lspecs)
  
  if (length(missing_lspecs) == 0) {
    cat("No missing LSPECs found - all morph LSPECs have field order data.\n")
    return(data.frame())
  }
  
  # Create simplified review list for missing LSPECs
  missing_review <- morph_with_ids %>%
    dplyr::filter(LSPEC %in% missing_lspecs) %>%
    dplyr::group_by(LSPEC) %>%
    dplyr::summarise(
      fish_ids = paste(unique(fish_id), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::arrange(LSPEC)
  
  # Save review file
  if (!dir.exists("data/review")) {
    dir.create("data/review", recursive = TRUE)
  }
  
  write.csv(missing_review,
            "data/review/morph_lspecs_missing_from_fieldorder.csv",
            row.names = FALSE)
  
  # Report results
  missing_rate <- round(100 * length(missing_lspecs) / length(morph_lspecs), 1)
  
  cat("Missing Morph LSPEC Validation:\n")
  cat("Total morph LSPECs:", length(morph_lspecs), "\n")
  cat("Field order LSPECs:", length(fieldorder_lspecs), "\n")
  cat("Missing from field order:", length(missing_lspecs), "\n")
  cat("Missing rate:", missing_rate, "%\n")
  cat("File saved to data/review/morph_lspecs_missing_from_fieldorder.csv\n\n")
  
  # Show examples
  if (nrow(missing_review) > 0) {
    cat("First 5 missing LSPECs:\n")
    examples <- head(missing_review, 5)
    for (i in 1:nrow(examples)) {
      cat(sprintf("  %s: %s\n",
                  examples$LSPEC[i],
                  examples$fish_ids[i]))
    }
  }
  
  return(as.data.frame(missing_review))
}

#' Create Combined Field Order ID Validation Summary
#'
#' Combines results from both validation functions to create
#' an overall summary of field order ID issues.
#'
#' @param decimal_review Output from validate_decimal_lspecs()
#' @param missing_review Output from validate_missing_morph_lspecs()
#' @return List with combined validation summary
#'
#' @examples
#' validation_summary <- create_fieldorder_validation_summary(decimal_review, missing_review)
create_fieldorder_validation_summary <- function(decimal_review, missing_review) {
  
  # Calculate decimal statistics
  if (nrow(decimal_review) > 0) {
    decimal_stats <- list(
      total_decimal_records = nrow(decimal_review),
      unique_base_ids = length(unique(decimal_review$base_id)),
      base_ids_with_multiple = sum(decimal_review$n_decimal_variants > 1),
      max_variants_per_base = max(decimal_review$n_decimal_variants)
    )
  } else {
    decimal_stats <- list(
      total_decimal_records = 0,
      unique_base_ids = 0, 
      base_ids_with_multiple = 0,
      max_variants_per_base = 0
    )
  }
  
  # Calculate missing LSPEC statistics  
  if (nrow(missing_review) > 0) {
    missing_stats <- list(
      missing_lspecs = nrow(missing_review)
    )
  } else {
    missing_stats <- list(
      missing_lspecs = 0
    )
  }
  
  validation_summary <- list(
    decimal_lspecs = decimal_stats,
    missing_lspecs = missing_stats,
    files_created = c(
      "data/review/fieldorder_decimal_lspecs_review.csv",
      "data/review/morph_lspecs_missing_from_fieldorder.csv"
    ),
    review_questions = list(
      decimal_investigation = "Do decimal LSPECs represent fossil halves or separate specimens?",
      missing_investigation = "Why do these morph specimens lack field order data?"
    )
  )
  
  cat("Field Order ID Validation Summary:\n")
  cat("=================================\n")
  cat("Decimal LSPECs:", decimal_stats$total_decimal_records, "records\n")
  cat("Missing morph LSPECs:", missing_stats$missing_lspecs, "specimens\n")
  cat("Review files created in data/review/\n\n")
  
  return(validation_summary)
}