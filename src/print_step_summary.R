#' Print Summary Statistics for Cleaning Steps
#'
#' Helper function to print consistent summary information for each cleaning step.
#' Works well with your existing logging system.
#'
#' @param data_before Data frame before processing step
#' @param data_after Data frame after processing step (optional)
#' @param step_name Name of the processing step
#' @param additional_info Additional information to display
#'
#' @examples
#' print_step_summary(raw_data, corrected_data, "Special Case Handling")
print_step_summary <- function(data_before, data_after = NULL, step_name, additional_info = NULL) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("STEP:", step_name, "\n")
  cat("Before:", nrow(data_before), "rows")
  if ("fish_id" %in% names(data_before)) {
    cat(",", length(unique(data_before$fish_id)), "unique fish")
  }
  cat("\n")
  
  if (!is.null(data_after)) {
    cat("After: ", nrow(data_after), "rows")
    if ("fish_id" %in% names(data_after)) {
      cat(",", length(unique(data_after$fish_id)), "unique fish")
    }
    cat("\n")
    
    row_change <- nrow(data_after) - nrow(data_before)
    if ("fish_id" %in% names(data_before) && "fish_id" %in% names(data_after)) {
      fish_change <- length(unique(data_after$fish_id)) - length(unique(data_before$fish_id))
      cat("Change:", sprintf("%+d rows, %+d fish\n", row_change, fish_change))
    } else {
      cat("Change:", sprintf("%+d rows\n", row_change))
    }
  }
  
  if (!is.null(additional_info)) {
    cat("Info:  ", additional_info, "\n")
  }
  
  cat(paste(rep("=", 50), collapse = ""), "\n")
}