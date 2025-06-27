#' Flag Fish Missing Scale Information
#'
#' Identifies fish specimens that lack 10mm scale bar information needed for
#' morphological measurements. Creates detailed reports but does not modify data.
#'
#' @param morph Data frame with fish_id, part_type, and Scale_10mm columns
#' @return Vector of fish_id values that are missing scale information
#'
#' @examples
#' flagged_fish <- flag_missing_scales(morph_with_ids)
flag_missing_scales <- function(morph) {
  if (!is.data.frame(morph)) {
    stop("Input must be a data frame")
  }
  if (!all(c("fish_id", "part_type", "Scale_10mm") %in% names(morph))) {
    stop("Data must contain 'fish_id', 'part_type', and 'Scale_10mm' columns")
  }
  
  scale_summary <- morph %>%
    dplyr::group_by(fish_id) %>%
    dplyr::summarize(
      has_part_scale = any(!is.na(Scale_10mm[part_type == "P"])),
      has_cpart_scale = any(!is.na(Scale_10mm[part_type == "C"])),
      has_any_scale = has_part_scale | has_cpart_scale,
      .groups = "drop"
    )
  
  fish_missing_scales <- scale_summary %>%
    dplyr::filter(!has_any_scale) %>%
    dplyr::pull(fish_id)
  
  missing_scales_detail <- morph %>%
    dplyr::filter(fish_id %in% fish_missing_scales) %>%
    dplyr::select(fish_id, ID, part_type, Scale_10mm) %>%
    dplyr::arrange(fish_id, part_type)
  
  if (!dir.exists("data/flagged")) {
    dir.create("data/flagged", recursive = TRUE)
  }
  
  write.csv(missing_scales_detail, 
            file = "data/flagged/fish_missing_scales_detailed.csv", 
            row.names = FALSE)
  
  return(fish_missing_scales)
}

#' Handle Fish Missing Scale Information
#'
#' Processes fish missing scale data by either incorporating updated scale
#' measurements from a new file or filtering out fish without scales.
#' Merges new data using the original ID column (including .jpg extension).
#'
#' @param morph Original morphology data frame
#' @param flagged_fish_ids Vector of fish_id values missing scale data
#' @param updated_scales_file Optional path to CSV file with updated scale measurements
#' @return Data frame with scale issues resolved (either updated or filtered)
#'
#' @examples
#' morph_clean <- handle_missing_scales(morph_raw, flagged_fish)
#' morph_updated <- handle_missing_scales(morph_raw, flagged_fish, "data/updates/new_scales.csv")
handle_missing_scales <- function(morph, flagged_fish_ids, updated_scales_file = NULL) {
  if (!is.data.frame(morph)) {
    stop("Input must be a data frame")
  }
  if (!is.vector(flagged_fish_ids)) {
    stop("flagged_fish_ids must be a vector")
  }
  
  if (!is.null(updated_scales_file)) {
    if (!file.exists(updated_scales_file)) {
      stop(paste("Updated scales file not found:", updated_scales_file))
    }
    
    file_ext <- tools::file_ext(updated_scales_file)
    
    if (file_ext %in% c("tsv", "txt")) {
      updated_scales <- read.table(updated_scales_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    } else if (file_ext == "csv") {
      updated_scales <- read.csv(updated_scales_file, stringsAsFactors = FALSE)
    } else {
      first_line <- readLines(updated_scales_file, n = 2)
      if (grepl("\t", first_line[2])) {
        updated_scales <- read.table(updated_scales_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      } else {
        updated_scales <- read.csv(updated_scales_file, stringsAsFactors = FALSE)
      }
    }
    
    if (!all(c("ID", "Scale_10mm") %in% names(updated_scales))) {
      stop("Updated scales file must contain 'ID' and 'Scale_10mm' columns")
    }
    
    morph_updated <- morph %>%
      dplyr::left_join(
        updated_scales %>% dplyr::select(ID, Scale_10mm_new = Scale_10mm), 
        by = "ID"
      ) %>%
      dplyr::mutate(
        Scale_10mm = coalesce(Scale_10mm_new, Scale_10mm)
      ) %>%
      dplyr::select(-Scale_10mm_new)
    
    still_missing_fish <- flag_missing_scales(morph_updated)
    
    result <- morph_updated %>%
      dplyr::filter(!fish_id %in% still_missing_fish)
    
    fish_still_missing <- morph_updated %>%
      dplyr::filter(fish_id %in% still_missing_fish)
    
    if (!dir.exists("data/processed")) {
      dir.create("data/processed", recursive = TRUE)
    }
    
    write.csv(result, 
              file = "data/processed/fish_with_scales_updated.csv", 
              row.names = FALSE)
    
    if (length(still_missing_fish) > 0) {
      write.csv(fish_still_missing, 
                file = "data/flagged/fish_still_missing_scales.csv", 
                row.names = FALSE)
    }
    
  } else {
    result <- morph %>%
      dplyr::filter(!fish_id %in% flagged_fish_ids)
    
    fish_without_scales <- morph %>%
      dplyr::filter(fish_id %in% flagged_fish_ids)
    
    if (!dir.exists("data/processed")) {
      dir.create("data/processed", recursive = TRUE)
    }
    if (!dir.exists("data/flagged")) {
      dir.create("data/flagged", recursive = TRUE)
    }
    
    write.csv(result, 
              file = "data/processed/fish_with_scales.csv", 
              row.names = FALSE)
    
    write.csv(fish_without_scales, 
              file = "data/flagged/fish_without_scales.csv", 
              row.names = FALSE)
  }
  
  return(result)
}