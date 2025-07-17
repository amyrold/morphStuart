#' Update Field Order Data with Additional Information
#'
#' Incorporates additional field order data if present, updating existing LSPEC records
#' with new CSTRAT (depth) information. If the additional data file is not present,
#' returns the original data unchanged.
#'
#' @param fieldorder_clean_data Clean field order data from duplicate processing
#' @param additional_data_file Path to additional data file (optional)
#' @return Data frame with updated field order information
#'
#' @examples
#' updated_data <- update_fieldorder_data(fieldorder_clean_data, "data/raw/170725_missing_depths.csv")
update_fieldorder_data <- function(fieldorder_clean_data, additional_data_file = NULL) {
  if (!is.data.frame(fieldorder_clean_data)) {
    stop("Input must be a data frame")
  }
  
  # Check if additional data file exists
  if (is.null(additional_data_file) || !file.exists(additional_data_file)) {
    cat("Additional fieldorder data file not found:", ifelse(is.null(additional_data_file), "NULL", additional_data_file), "\n")
    cat("Using original data without updates.\n\n")
    return(fieldorder_clean_data)
  }
  
  # Read additional data
  tryCatch({
    additional_data <- read.csv(additional_data_file, stringsAsFactors = FALSE)
    
    # Validate required columns
    required_cols <- c("LSPEC", "CSTRAT")
    missing_cols <- setdiff(required_cols, names(additional_data))
    if (length(missing_cols) > 0) {
      warning(paste("Additional data missing required columns:", paste(missing_cols, collapse = ", ")))
      return(fieldorder_clean_data)
    }
    
    # Process additional data
    processed_updates <- additional_data %>%
      dplyr::select(LSPEC, CSTRAT_new = CSTRAT) %>%
      dplyr::filter(!is.na(LSPEC), !is.na(CSTRAT_new)) %>%
      dplyr::distinct(LSPEC, .keep_all = TRUE)  # Remove any duplicates
    
    if (nrow(processed_updates) == 0) {
      cat("No valid updates found in additional data file.\n")
      return(fieldorder_clean_data)
    }
    
    # Update existing data with new CSTRAT values
    updated_data <- fieldorder_clean_data %>%
      dplyr::left_join(processed_updates, by = "LSPEC") %>%
      dplyr::mutate(
        CSTRAT = coalesce(CSTRAT_new, CSTRAT)
      ) %>%
      dplyr::select(-CSTRAT_new)
    
    # Report update results
    updates_applied <- sum(!is.na(processed_updates$CSTRAT_new))
    lspecs_updated <- processed_updates$LSPEC
    
    cat("Additional Field Order Data Processing:\n")
    cat("Additional data file:", additional_data_file, "\n")
    cat("Updates available:", nrow(processed_updates), "\n")
    cat("Updates applied:", updates_applied, "\n")
    cat("LSPECs updated:", paste(head(lspecs_updated, 10), collapse = ", "))
    if (length(lspecs_updated) > 10) {
      cat(" (and", length(lspecs_updated) - 10, "more)")
    }
    cat("\n\n")
    
    return(as.data.frame(updated_data))
    
  }, error = function(e) {
    warning(paste("Error processing additional data file:", e$message))
    cat("Using original data without updates.\n\n")
    return(fieldorder_clean_data)
  })
}