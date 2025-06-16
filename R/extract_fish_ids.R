#' Extract V #'s and LSPEC #'s
#'
#'
#'

extract_fish_ids <- function(morph){
  # Extract fish ID and part type
  # ID format: VXXXXXX_LXXXX(A)_1_P/C.jpg or VXXXXXX_LXXXX(A)_1_P2.jpg
  # where VXXXXXX is the specimen ID, LXXXX(A) is the LSPEC/locality, and P/C indicates part/counterpart
  morph_with_ids <- morph %>%
    mutate(
      fish_id = str_extract(ID, "^V\\d+"),                          # Extract VXXXXX part at beginning
      LSPEC_raw = str_extract(ID, "L\\d+[A-Za-z]?"),                  # Extract LXXXX with optional letter
      # Split LSPEC into letter prefix, number, and suffix
      LSPEC_letter = str_extract(LSPEC_raw, "^L"),
      LSPEC_number = str_extract(LSPEC_raw, "\\d+"),
      LSPEC_suffix = str_extract(LSPEC_raw, "[A-Za-z]$"),
      # Create standardized LSPEC with zero padding
      LSPEC = case_when(
        !is.na(LSPEC_suffix) ~ paste0(LSPEC_letter, str_pad(LSPEC_number, 4, pad = "0"), LSPEC_suffix),
        !is.na(LSPEC_number) ~ paste0(LSPEC_letter, str_pad(LSPEC_number, 4, pad = "0")),
        TRUE ~ NA_character_
      ),
      # Extract P or C (with optional numbers) before .jpg, then standardize
      part_type_raw = str_extract(ID, "[PC]\\d*(?=\\.jpg$)"),      # Extract P, P2, P3, etc. or C before .jpg
      part_type = case_when(
        str_starts(part_type_raw, "P") ~ "P",                      # Convert P, P2, P3, etc. to just "P"
        part_type_raw == "C" ~ "C",                                # Keep C as is
        TRUE ~ part_type_raw                                       # Preserve any unexpected values
      )
    ) %>%
    select(-LSPEC_raw, -LSPEC_letter, -LSPEC_number, -LSPEC_suffix, -part_type_raw)  # Remove intermediate columns
  
  return(morph_with_ids)
}