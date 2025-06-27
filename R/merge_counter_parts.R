#' Merge Non-Overlapping Part and Counterpart Data
#'
#' For fish without conflicting measurements, intelligently merge part and 
#' counterpart data prioritizing part over counterpart. Handles multiple
#' rows per fish/part combination by aggregating appropriately by variable type.
#'
#' @param data Data frame containing non-overlapping fish measurements
#' @return Data frame with one row per fish, merged measurements
#'
#' @examples
#' merged_data <- merge_counter_parts(non_overlap_fish)
merge_counter_parts <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!"fish_id" %in% names(data)) {
    stop("Data must contain 'fish_id' column")
  }
  
  var_map <- variable_mapping()
  id_cols <- c("n", "ID", "fish_id", "part_type", "LSPEC")
  measure_cols <- setdiff(names(data), id_cols)
  fish_ids <- unique(data$fish_id)
  result_list <- list()
  
  for (i in seq_along(fish_ids)) {
    id <- fish_ids[i]
    fish_data <- data %>% dplyr::filter(fish_id == id)
    
    part_data <- fish_data %>% dplyr::filter(part_type == "P")
    cpart_data <- fish_data %>% dplyr::filter(part_type == "C")
    
    result_row <- data.frame(fish_id = id)
    
    if (nrow(part_data) == 0) {
      if (nrow(cpart_data) > 0) {
        template_row <- cpart_data[1,]
        for (col in id_cols) {
          if (col != "part_type") {
            result_row[[col]] <- template_row[[col]]
          }
        }
        for (col in measure_cols) {
          result_row[[col]] <- aggregate_values(cpart_data[[col]], col, var_map)
        }
      }
      result_row$part_type <- "merged"
      result_row$merged <- TRUE
      result_list[[i]] <- result_row
      next
      
    } else if (nrow(cpart_data) == 0) {
      template_row <- part_data[1,]
      for (col in id_cols) {
        if (col != "part_type") {
          result_row[[col]] <- template_row[[col]]
        }
      }
      for (col in measure_cols) {
        result_row[[col]] <- aggregate_values(part_data[[col]], col, var_map)
      }
      result_row$part_type <- "merged"
      result_row$merged <- TRUE
      result_list[[i]] <- result_row
      next
    }
    
    template_row <- part_data[1,]
    for (col in id_cols) {
      if (col != "part_type") {
        result_row[[col]] <- template_row[[col]]
      }
    }
    result_row$merged <- TRUE
    
    for (col in measure_cols) {
      part_val <- aggregate_values(part_data[[col]], col, var_map)
      cpart_val <- aggregate_values(cpart_data[[col]], col, var_map)
      
      if (!is.na(part_val)) {
        result_row[[col]] <- part_val
      } else if (!is.na(cpart_val)) {
        result_row[[col]] <- cpart_val
      } else {
        result_row[[col]] <- NA
      }
    }
    
    result_row$part_type <- "merged"
    result_list[[i]] <- result_row
  }
  
  result <- dplyr::bind_rows(result_list)
  
  for (col in names(data)) {
    if (!(col %in% names(result))) {
      result[[col]] <- NA
    }
  }
  
  if (!("merged" %in% names(result))) {
    result$merged <- TRUE
  }
  
  return(as.data.frame(result))
}

#' Aggregate Values by Variable Type
#'
#' Helper function to aggregate multiple values appropriately by variable type.
#'
#' @param values Vector of values to aggregate
#' @param col_name Name of the column (for type lookup)
#' @param var_map Variable mapping from variable_mapping()
#' @return Single aggregated value
aggregate_values <- function(values, col_name, var_map) {
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(NA)
  }
  
  if (col_name %in% var_map$binary || col_name %in% var_map$count) {
    return(max(values))
  } else {
    return(mean(values))
  }
}