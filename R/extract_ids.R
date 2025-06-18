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
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if ("ID" %in% names(data)) {
    return(extract_morph_ids(data))
  } else if ("Sample_ID" %in% names(data)) {
    return(extract_paleo_ids(data))
  } else if ("L..SPEC" %in% names(data)) {
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
      fish_id = str_extract(ID, "^V\\d+"),
      LSPEC_raw = str_extract(ID, "L\\d+[A-Za-z]?"),
      LSPEC_number = str_extract(LSPEC_raw, "\\d+"),
      LSPEC = case_when(
        !is.na(LSPEC_number) ~ paste0("L", str_pad(LSPEC_number, 4, pad = "0")),
        TRUE ~ NA_character_
      ),
      part_type_raw = str_extract(ID, "[PC]\\d*(?=\\.jpg$)"),
      part_type = case_when(
        str_starts(part_type_raw, "P") ~ "P",
        part_type_raw == "C" ~ "C",
        TRUE ~ part_type_raw
      )
    ) %>%
    select(-LSPEC_raw, -LSPEC_number, -part_type_raw)
  
  return(result)
}

#' Extract IDs from Paleo-Ecology Dataset
#' 
#' Handles IDs like "V332333_L2072" or "V123456_LXXXX"
#' 
#' @param data Paleo data frame with Sample_ID column
#' @return Original data frame with V_number, LSPEC, sample_type columns added
extract_paleo_ids <- function(data) {
  result <- data %>%
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
    select(-L_number_raw, -L_digits)
  
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
      L_number = floor(as.numeric(`L..SPEC`)),
      LSPEC = case_when(
        !is.na(L_number) & L_number > 0 ~ paste0("L", str_pad(L_number, 4, pad = "0")),
        TRUE ~ NA_character_
      ),
      has_decimal = str_detect(as.character(`L..SPEC`), "\\."),
      decimal_part = ifelse(has_decimal, 
                            as.numeric(`L..SPEC`) - floor(as.numeric(`L..SPEC`)), 
                            0)
    )
  
  return(result)
}