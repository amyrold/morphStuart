#' Define Variable Types for Morphological Measurements
#'
#' Creates a mapping of morphological variables into their measurement types
#' (continuous, count, binary) for appropriate statistical handling.
#'
#' @return A list containing vectors of variable names grouped by type:
#'   - continuous: Length measurements (SL, CAV, DS1-3, etc.)
#'   - count: Count measurements (MDF, MAF, MCV, etc.) 
#'   - binary: Binary flags (MDS1-3, MDS1NA-3NA, etc.)
#'   - all: Combined vector of all measurement variables
#'
#' @examples
#' var_map <- variable_mapping()
#' continuous_vars <- var_map$continuous
variable_mapping <- function() {
  continuous_vars <- c("SL", "CAV", "DS1", "DS2", "DS3", "LPT", 
                       "PSP.L", "PSP.R", "TPG", "ECT", "CLE", "PMX")
  
  count_vars <- c("MDF", "MAF", "MCV", "MAV", "MPT", "MPSP")
  
  binary_vars <- c("MDS1", "MDS2", "MDS3", "MDS1NA", 
                   "MDS2NA", "MDS3NA", "MPSPNA", "PGNA")
  
  return(list(
    continuous = continuous_vars,
    count = count_vars,
    binary = binary_vars,
    all = c(continuous_vars, count_vars, binary_vars)
  ))
}