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
#' table(metadata$sample_type)
extract_paleo_metadata <- function(paleo) {
  if (!is.data.frame(paleo)) {
    stop("Input must be a data frame")
  }
  if (!"Sample_ID" %in% names(paleo)) {
    stop("Data must contain 'Sample_ID' column")
  }
  
  paleo_with_metadata <- paleo %>%
    distinct(Sample_ID) %>%
    mutate(
      V_number = str_extract(Sample_ID, "V\\d+"),
      L_number_raw = str_extract(Sample_ID, "L\\w+"),
      L_digits = str_extract(Sample_ID, "(?<=L)\\d+"),
      LSPEC = case_when(
        !is.na(L_digits) ~ paste0("L", str_pad(L_digits, 4, pad = "0")),
        str_detect(Sample_ID, "LXXXX") ~ "LXXXX",
        TRUE ~ L_number_raw
      ),
      sample_type = case_when(
        str_detect(Sample_ID, "LXXXX") ~ "Killifish",
        str_detect(Sample_ID, "L\\d+") ~ "Stickleback",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(Sample_ID, V_number, LSPEC, sample_type)
  
  return(paleo_with_metadata)
}