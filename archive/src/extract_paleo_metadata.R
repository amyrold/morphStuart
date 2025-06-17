#' Extract and Classify Paleo Sample Metadata
#'
#' Extracts sample metadata from Sample_ID strings and classifies samples as
#' killifish (post-stickleback, LXXXX) or stickleback (L + digits). Creates
#' standardized LSPEC formatting to match morphology data for age integration.
#'
#' @param paleo Data frame containing paleo-ecological data with Sample_ID column
#' @return Data frame with extracted metadata columns:
#'   - V_number: Specimen identifier for linking with morphology data
#'   - LSPEC: Standardized format (L + 4-digit) for age/depth lookup
#'   - sample_type: "Killifish" or "Stickleback" classification
#'
#' @examples
#' metadata <- extract_paleo_metadata(raw_paleo_data)
#' table(metadata$sample_type)  # Check killifish vs stickleback distribution
extract_paleo_metadata <- function(paleo) {
  # Validate input
  if (!is.data.frame(paleo)) {
    stop("Input must be a data frame")
  }
  if (!"Sample_ID" %in% names(paleo)) {
    stop("Data must contain 'Sample_ID' column")
  }
  
  cat("=== EXTRACTING PALEO METADATA ===\n")
  
  paleo_with_metadata <- paleo %>%
    distinct(Sample_ID) %>%
    mutate(
      # Extract V-number for linking with morph data
      V_number = str_extract(Sample_ID, "V\\d+"),
      
      # Extract L-number (field excavation number) and format as LSPEC
      L_number_raw = str_extract(Sample_ID, "L\\w+"),
      L_digits = str_extract(Sample_ID, "(?<=L)\\d+"),
      
      # Create standardized LSPEC format to match morph data
      # This is critical for age/depth integration
      LSPEC = case_when(
        !is.na(L_digits) ~ paste0("L", str_pad(L_digits, 4, pad = "0")),
        str_detect(Sample_ID, "LXXXX") ~ "LXXXX",  # Killifish samples
        TRUE ~ L_number_raw
      ),
      
      # Classify sample type based on Jacopo's specifications
      sample_type = case_when(
        str_detect(Sample_ID, "LXXXX") ~ "Killifish",      # Post-stickleback environment
        str_detect(Sample_ID, "L\\d+") ~ "Stickleback",    # Original stickleback period
        TRUE ~ "Unknown"
      )
    ) %>%
    select(Sample_ID, V_number, LSPEC, sample_type)
  
  # Summary output
  cat("Extracted metadata for", nrow(paleo_with_metadata), "unique samples\n")
  cat("Sample type breakdown:\n")
  type_breakdown <- table(paleo_with_metadata$sample_type)
  print(type_breakdown)
  
  # Check for potential issues
  unknown_samples <- sum(paleo_with_metadata$sample_type == "Unknown")
  if (unknown_samples > 0) {
    cat("WARNING:", unknown_samples, "samples with unknown type\n")
  }
  
  missing_v_numbers <- sum(is.na(paleo_with_metadata$V_number))
  if (missing_v_numbers > 0) {
    cat("NOTE:", missing_v_numbers, "samples without V-numbers\n")
  }
  
  return(paleo_with_metadata)
}
