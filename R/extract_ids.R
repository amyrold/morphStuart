#' Extract and Standardize IDs Across All Datasets
#'
#' Unified function to extract and standardize specimen IDs across morphology,
#' paleo-ecology, and field order datasets. Automatically detects dataset type
#' and applies appropriate ID extraction logic.
#'
#' @param data Data frame containing ID information
#' @return Data frame with appropriate ID columns added:
#'   - Morph data: adds fish_id (V_number), LSPEC, part_type
#'   - Paleo data: adds V_number, LSPEC, sample_type  
#'   - Order data: adds LSPEC
#'
#' @examples
#' morph_with_ids <- extract_ids(morph_raw)
#' paleo_with_ids <- extract_ids(paleo_raw) 
#' order_with_ids <- extract_ids(order_raw)
extract_ids <- function(data) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Detect dataset type by column names
  if ("ID" %in% names(data)) {
    cat("=== EXTRACTING MORPH IDS ===\n")
    return(extract_morph_ids(data))
  } else if ("Sample_ID" %in% names(data)) {
    cat("=== EXTRACTING PALEO IDS ===\n")
    return(extract_paleo_ids(data))
  } else if ("L..SPEC" %in% names(data)) {
    cat("=== EXTRACTING ORDER IDS ===\n")
    return(extract_order_ids(data))
  } else {
    stop("Unable to detect dataset type. Expected columns: 'ID' (morph), 'Sample_ID' (paleo), or 'L..SPEC' (order)")
  }
}

#' Extract IDs from Morphology Dataset
#' 
#' Handles IDs like "V330363_L2_1_C.jpg" or "V123456_L9999A_1_P2.jpg"
#' 
#' @param data Morphology data frame with ID column
#' @return Data frame with fish_id, LSPEC, part_type columns added
extract_morph_ids <- function(data) {
  result <- data %>%
    mutate(
      # Extract V-number as fish identifier
      fish_id = str_extract(ID, "^V\\d+"),
      
      # Extract L-number with optional A/B suffix
      LSPEC_raw = str_extract(ID, "L\\d+[A-Za-z]?"),
      LSPEC_number = str_extract(LSPEC_raw, "\\d+"),
      
      # Create standardized LSPEC with zero padding, removing A/B suffixes
      # This fixes the issue where L9999A and L9999B should both become L9999
      LSPEC = case_when(
        !is.na(LSPEC_number) ~ paste0("L", str_pad(LSPEC_number, 4, pad = "0")),
        TRUE ~ NA_character_
      ),
      
      # Extract part type (P/C) before .jpg extension
      part_type_raw = str_extract(ID, "[PC]\\d*(?=\\.jpg$)"),
      part_type = case_when(
        str_starts(part_type_raw, "P") ~ "P",  # P, P2, P3, etc. → P
        part_type_raw == "C" ~ "C",            # C stays C
        TRUE ~ part_type_raw
      )
    ) %>%
    select(-LSPEC_raw, -LSPEC_number, -part_type_raw)
  
  # Validation and summary
  total_rows <- nrow(result)
  fish_extracted <- sum(!is.na(result$fish_id))
  lspec_extracted <- sum(!is.na(result$LSPEC))
  part_extracted <- sum(!is.na(result$part_type))
  
  cat("Processed", total_rows, "morphology records\n")
  cat("V-numbers extracted:", fish_extracted, "(", round(100 * fish_extracted / total_rows, 1), "%)\n")
  cat("LSPECs extracted:", lspec_extracted, "(", round(100 * lspec_extracted / total_rows, 1), "%)\n") 
  cat("Part types extracted:", part_extracted, "(", round(100 * part_extracted / total_rows, 1), "%)\n")
  
  # Check for potential issues
  unique_fish <- length(unique(result$fish_id[!is.na(result$fish_id)]))
  unique_lspec <- length(unique(result$LSPEC[!is.na(result$LSPEC)]))
  cat("Unique fish (V-numbers):", unique_fish, "\n")
  cat("Unique LSPECs:", unique_lspec, "\n")
  
  return(result)
}

#' Extract IDs from Paleo-Ecology Dataset
#' 
#' Handles IDs like "V332333_L2072" or "V123456_LXXXX"
#' Just extracts V_number and LSPEC, preserves original data structure
#' 
#' @param data Paleo data frame with Sample_ID column
#' @return Original data frame with V_number and LSPEC columns added
extract_paleo_ids <- function(data) {
  result <- data %>%
    mutate(
      # Extract V-number for linking with morph data
      V_number = str_extract(Sample_ID, "V\\d+"),
      
      # Extract L-number and handle special cases
      L_number_raw = str_extract(Sample_ID, "L\\w+"),
      L_digits = str_extract(Sample_ID, "(?<=L)\\d+"),
      
      # Create standardized LSPEC format
      LSPEC = case_when(
        !is.na(L_digits) ~ paste0("L", str_pad(L_digits, 4, pad = "0")),
        str_detect(Sample_ID, "LXXXX") ~ "LXXXX",  # Killifish samples
        TRUE ~ L_number_raw
      )
    ) %>%
    select(-L_number_raw, -L_digits)  # Remove intermediate columns
  
  # Summary statistics
  total_rows <- nrow(result)
  unique_samples <- length(unique(result$Sample_ID))
  v_numbers_extracted <- sum(!is.na(result$V_number))
  lspecs_extracted <- sum(!is.na(result$LSPEC))
  
  cat("Processed", total_rows, "paleo records (", unique_samples, "unique samples)\n")
  cat("V-numbers extracted:", v_numbers_extracted, "(", round(100 * v_numbers_extracted / total_rows, 1), "%)\n")
  cat("LSPECs extracted:", lspecs_extracted, "(", round(100 * lspecs_extracted / total_rows, 1), "%)\n")
  
  # Check for issues
  missing_v_numbers <- sum(is.na(result$V_number))
  if (missing_v_numbers > 0) {
    cat("NOTE:", missing_v_numbers, "records without V-numbers\n")
  }
  
  return(result)
}

#' Extract IDs from Field Order Dataset
#' 
#' Handles IDs like "1", "123", "123.1", "123.2"
#' Converts to standardized LSPEC format (L0001, L0123)
#' 
#' @param data Field order data frame with L..SPEC column
#' @return Data frame with LSPEC column added
extract_order_ids <- function(data) {
  result <- data %>%
    mutate(
      # Extract the integer part, handling decimals
      L_number = floor(as.numeric(`L..SPEC`)),
      
      # Create standardized LSPEC with zero padding
      LSPEC = case_when(
        !is.na(L_number) & L_number > 0 ~ paste0("L", str_pad(L_number, 4, pad = "0")),
        TRUE ~ NA_character_
      ),
      
      # Flag potential duplicates (decimal variants)
      has_decimal = str_detect(as.character(`L..SPEC`), "\\."),
      decimal_part = ifelse(has_decimal, 
                            as.numeric(`L..SPEC`) - floor(as.numeric(`L..SPEC`)), 
                            0)
    )
  
  # Summary statistics
  total_rows <- nrow(result)
  lspecs_extracted <- sum(!is.na(result$LSPEC))
  decimal_variants <- sum(result$has_decimal, na.rm = TRUE)
  unique_lspecs <- length(unique(result$LSPEC[!is.na(result$LSPEC)]))
  
  cat("Processed", total_rows, "field order records\n")
  cat("LSPECs extracted:", lspecs_extracted, "(", round(100 * lspecs_extracted / total_rows, 1), "%)\n")
  cat("Decimal variants found:", decimal_variants, "\n")
  cat("Unique LSPECs:", unique_lspecs, "\n")
  
  # Show examples of decimal handling
  if (decimal_variants > 0) {
    decimal_examples <- result %>%
      filter(has_decimal) %>%
      select(`L..SPEC`, LSPEC, decimal_part) %>%
      head(5)
    
    cat("Examples of decimal handling:\n")
    print(decimal_examples)
  }
  
  # Check for potential issues
  failed_conversions <- sum(is.na(result$LSPEC))
  if (failed_conversions > 0) {
    cat("WARNING:", failed_conversions, "records failed LSPEC conversion\n")
  }
  
  return(result)
}