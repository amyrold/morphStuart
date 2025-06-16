#' Handle Special Cases in Dorsal Spine and Pelvic Girdle Data
#'
#' Applies biological logic to distinguish between true zeros (evolutionary loss)
#' and missing data due to poor preservation. This is critical for accurate
#' evolutionary analysis of fish morphology.
#'
#' Logic implemented:
#' - If MDS* = 0 & MDS*NA = 1: Set DS* to NA (lost to preservation)
#' - If MDS* = 0 & MDS*NA = 0: Set DS* to 0 (true evolutionary loss)
#' - If MPT = 0 but any dorsal spine present: Set MPT to NA (poor preservation)
#' - Similar logic for pelvic spines using MPSP and MPSPNA flags
#' - If PGNA column exists, apply similar logic to TPG
#'
#' @param data Data frame containing morphological measurements with special case columns
#' @return Data frame with special cases properly handled (zeros vs NAs)
#'
#' @examples
#' cleaned_data <- handle_special_cases(raw_morph_data)
handle_special_cases <- function(data) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Convert to tibble for dplyr operations
  cleaned <- data %>%
    as_tibble() %>%
    # Handle dorsal spine columns using case_when for readable conditional logic
    mutate(
      # Dorsal spine 1: distinguish evolutionary loss from poor preservation
      DS1 = case_when(
        MDS1 == 0 & MDS1NA == 1 ~ NA_real_,  # Lost to preservation
        MDS1 == 0 & MDS1NA == 0 & is.na(DS1) ~ 0,  # True evolutionary loss
        TRUE ~ DS1  # Keep existing values
      ),
      # Dorsal spine 2
      DS2 = case_when(
        MDS2 == 0 & MDS2NA == 1 ~ NA_real_,
        MDS2 == 0 & MDS2NA == 0 & is.na(DS2) ~ 0,
        TRUE ~ DS2
      ),
      # Dorsal spine 3
      DS3 = case_when(
        MDS3 == 0 & MDS3NA == 1 ~ NA_real_,
        MDS3 == 0 & MDS3NA == 0 & is.na(DS3) ~ 0,
        TRUE ~ DS3
      ),
      
      # Handle MPT (pre-dorsal pterygiophores)
      # If any dorsal spine is present but MPT = 0, this is likely poor preservation
      MPT = case_when(
        MPT == 0 & (MDS1 == 1 | MDS2 == 1 | MDS3 == 1) ~ NA_real_,
        TRUE ~ MPT
      ),
      
      # Handle pelvic spine columns using similar logic
      PSP.L = case_when(
        MPSP == 0 & MPSPNA == 1 ~ NA_real_,  # Lost to preservation
        TRUE ~ PSP.L
      ),
      PSP.R = case_when(
        MPSP == 0 & MPSPNA == 1 ~ NA_real_,  # Lost to preservation
        TRUE ~ PSP.R
      )
    )
  
  # Apply pelvic girdle logic if PGNA column is available
  if ("PGNA" %in% names(cleaned)) {
    cleaned <- cleaned %>%
      mutate(
        TPG = case_when(
          PGNA == 1 ~ NA_real_,  # Lost to preservation
          TRUE ~ TPG
        )
      )
  }
  
  return(as.data.frame(cleaned))
}