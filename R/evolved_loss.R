#' Handle Special NA Cases in Dorsal Spine and Pelvic Girdle Data
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
evolved_loss <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  cleaned <- data %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      DS1 = dplyr::case_when(
        MDS1 == 0 & MDS1NA == 1 ~ NA_real_,
        MDS1 == 0 & MDS1NA == 0 & is.na(DS1) ~ 0,
        TRUE ~ DS1
      ),
      DS2 = dplyr::case_when(
        MDS2 == 0 & MDS2NA == 1 ~ NA_real_,
        MDS2 == 0 & MDS2NA == 0 & is.na(DS2) ~ 0,
        TRUE ~ DS2
      ),
      DS3 = dplyr::case_when(
        MDS3 == 0 & MDS3NA == 1 ~ NA_real_,
        MDS3 == 0 & MDS3NA == 0 & is.na(DS3) ~ 0,
        TRUE ~ DS3
      ),
      MPT = dplyr::case_when(
        MPT == 0 & (MDS1 == 1 | MDS2 == 1 | MDS3 == 1) ~ NA_real_,
        TRUE ~ MPT
      ),
      PSP.L = dplyr::case_when(
        MPSP == 0 & MPSPNA == 1 ~ NA_real_,
        TRUE ~ PSP.L
      ),
      PSP.R = dplyr::case_when(
        MPSP == 0 & MPSPNA == 1 ~ NA_real_,
        TRUE ~ PSP.R
      )
    )
  
  if ("PGNA" %in% names(cleaned)) {
    cleaned <- cleaned %>%
      dplyr::mutate(
        TPG = dplyr::case_when(
          PGNA == 1 ~ NA_real_,
          TRUE ~ TPG
        )
      )
    
  }
  
  # Remove binary logic columns after applying transformations
  var_map <- variable_mapping()
  cols_to_keep <- setdiff(names(cleaned), var_map$binary)
  cleaned <- cleaned %>% dplyr::select(dplyr::all_of(cols_to_keep))
  
  return(as.data.frame(cleaned))
}