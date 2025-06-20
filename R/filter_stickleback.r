#' Filter Stickleback Samples from Paleo Data
#'
#' Removes killifish samples (LXXXX pattern) and retains only stickleback samples
#' for stratigraphic analysis. Killifish samples lack age/depth data needed for rioja.
#'
#' @param paleo_merged_counts Output from merge_microscopy_counts()
#' @return Data frame containing only stickleback samples
#'
#' @examples
#' stickleback_only <- filter_stickleback_samples(paleo_merged_counts)
filter_stickleback_samples <- function(paleo_merged_counts) {
  if (!is.data.frame(paleo_merged_counts)) {
    stop("Input must be a data frame")
  }
  if (!"sample_type" %in% names(paleo_merged_counts)) {
    stop("Data must contain 'sample_type' column")
  }
  
  # Filter to stickleback only
  stickleback_data <- paleo_merged_counts %>%
    filter(sample_type == "Stickleback") %>%
    filter(!is.na(LSPEC), LSPEC != "LXXXX")
  
  # Report filtering results
  original_samples <- length(unique(paleo_merged_counts$Sample_ID))
  stickleback_samples <- length(unique(stickleback_data$Sample_ID))
  
  cat("Stickleback sample filtering:\n")
  cat("Original samples:", original_samples, "\n")
  cat("Stickleback samples retained:", stickleback_samples, "\n")
  cat("Samples removed:", original_samples - stickleback_samples, "\n\n")
  
  return(as.data.frame(stickleback_data))
}