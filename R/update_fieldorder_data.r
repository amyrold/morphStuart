#' Update Field Order Data with Additional Information
#'
#' Incorporates additional field order data if present, updating existing LSPEC records
#' with new CSTRAT (depth) information and adding new LSPEC records that don't exist
#' in the original data. If the additional data file is not present, returns the 
#' original data unchanged.
#'
#' @param fieldorder_clean_data Clean field order data from duplicate processing
#' @param additional_data_file Path to additional data file (optional)
#' @return Data frame with updated field order information including any new LSPECs
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
    
    # Identify existing vs new LSPECs
    existing_lspecs <- fieldorder_clean_data$LSPEC
    new_lspecs <- processed_updates %>%
      dplyr::filter(!LSPEC %in% existing_lspecs)
    
    # Update existing data
    updated_data <- fieldorder_clean_data %>%
      dplyr::left_join(processed_updates %>% dplyr::select(LSPEC, CSTRAT_new), by = "LSPEC") %>%
      dplyr::mutate(
        CSTRAT = coalesce(CSTRAT_new, CSTRAT)
      ) %>%
      dplyr::select(-CSTRAT_new)
    
    # Add new LSPECs if any exist
    if (nrow(new_lspecs) > 0) {
      # Get column structure from existing data
      template_cols <- names(fieldorder_clean_data)
      
      new_rows <- new_lspecs %>%
        dplyr::rename(CSTRAT = CSTRAT_new) %>%
        dplyr::mutate(
          # Set all other columns to appropriate defaults/NA
          L_SPEC_original = NA_character_,
          ISTRAT = NA_real_,
          YEAR = NA_real_,
          INT = NA_real_,
          n_records_merged = 1
        ) %>%
        # Select only columns that exist in the original data
        dplyr::select(dplyr::all_of(intersect(names(.), template_cols)))
      
      # Ensure all columns match and are in the right order
      for (col in template_cols) {
        if (!col %in% names(new_rows)) {
          new_rows[[col]] <- NA
        }
      }
      new_rows <- new_rows[, template_cols]
      
      # Combine updated data with new rows
      updated_data <- dplyr::bind_rows(updated_data, new_rows)
    }
    
    # Simple reporting
    cat("Additional Field Order Data Processing:\n")
    cat("Additional data file:", additional_data_file, "\n")
    cat("Records processed:", nrow(processed_updates), "\n")
    cat("Records before:", nrow(fieldorder_clean_data), "\n")
    cat("Records after:", nrow(updated_data), "\n\n")
    
    return(as.data.frame(updated_data))
    
  }, error = function(e) {
    warning(paste("Error processing additional data file:", e$message))
    cat("Using original data without updates.\n\n")
    return(fieldorder_clean_data)
  })
}