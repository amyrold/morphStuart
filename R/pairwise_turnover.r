#' Calculate Pairwise Turnover (Beta Diversity) Matrix
#'
#' Calculates pairwise beta diversity between all time bins using vegan
#' distance functions. Results in a symmetric matrix showing compositional
#' dissimilarity between each pair of stratigraphic levels.
#'
#' @param data Data frame with taxa columns and CSTRAT metadata
#' @param method Distance method for beta diversity calculation (default "bray")
#' @param binary Logical, whether to use binary (presence/absence) data (default FALSE)
#' @return List containing distance matrix, formatted matrix for visualization, and metadata
#'
#' @examples
#' turnover <- calculate_pairwise_turnover(filtered_data)
#' turnover_jaccard <- calculate_pairwise_turnover(filtered_data, method = "jaccard", binary = TRUE)
calculate_pairwise_turnover <- function(data, method = "bray", binary = FALSE, time_column = "YEAR") {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop("Package 'vegan' is required for beta diversity calculations")
  }
  
  # Define metadata columns
  metadata_cols <- c("LSPEC", "YEAR", "CSTRAT", "ISTRAT", "INT", "Sample_ID", "V_number", "total_count", "n_taxa")
  available_metadata <- intersect(metadata_cols, names(data))
  taxa_cols <- setdiff(names(data), available_metadata)
  
  if (length(taxa_cols) == 0) {
    stop("No taxa columns found in data")
  }
  if (!time_column %in% available_metadata) {
    # Try fallback options
    fallback_options <- c("YEAR", "CSTRAT")
    available_time <- intersect(fallback_options, available_metadata)
    if (length(available_time) == 0) {
      stop("No time column found. Need YEAR or CSTRAT column for time bin analysis")
    }
    time_column <- available_time[1]
    warning(paste("Requested time column not found, using", time_column))
  }
  
  # Prepare community matrix (time bins x species)
  community_matrix <- data %>%
    select(all_of(time_column), all_of(taxa_cols)) %>%
    arrange(.data[[time_column]])
  
  # Check if we need to pool samples by time_column
  time_counts <- community_matrix %>%
    count(.data[[time_column]]) %>%
    filter(n > 1)
  
  if (nrow(time_counts) > 0) {
    # Pool samples with same time value
    cat("Pooling", sum(time_counts$n), "samples with duplicate", time_column, "values for turnover analysis\n")
    community_matrix <- community_matrix %>%
      group_by(.data[[time_column]]) %>%
      summarise(
        across(all_of(taxa_cols), sum, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Create species matrix for vegan functions
  species_matrix <- community_matrix %>%
    select(all_of(taxa_cols)) %>%
    as.matrix()
  
  rownames(species_matrix) <- paste0(time_column, "_", community_matrix[[time_column]])
  
  # Convert to binary if requested
  if (binary) {
    species_matrix[species_matrix > 0] <- 1
  }
  
  # Calculate distance matrix
  distance_matrix <- vegan::vegdist(species_matrix, method = method, binary = binary)
  
  # Convert to full matrix for easier handling
  full_matrix <- as.matrix(distance_matrix)
  
  # Create formatted matrix with proper labels
  formatted_matrix <- full_matrix
  rownames(formatted_matrix) <- community_matrix[[time_column]]
  colnames(formatted_matrix) <- community_matrix[[time_column]]
  
  # Add temporal information if available
  time_info <- NULL
  other_time_cols <- intersect(c("YEAR", "CSTRAT"), names(data))
  if (length(other_time_cols) > 0) {
    time_info <- data %>%
      select(all_of(c(time_column, other_time_cols))) %>%
      distinct() %>%
      arrange(.data[[time_column]])
  }
  
  # Create summary statistics
  turnover_summary <- create_turnover_summary(distance_matrix, formatted_matrix, method, binary)
  
  # Report results
  cat("Pairwise Turnover Analysis:\n")
  cat("Method:", method, ifelse(binary, "(binary)", "(abundance)"), "\n")
  cat("Time bins:", nrow(formatted_matrix), "\n")
  cat("Pairwise comparisons:", length(distance_matrix), "\n")
  cat("Turnover range:", round(min(distance_matrix), 3), "-", round(max(distance_matrix), 3), "\n")
  cat("Mean turnover:", round(mean(distance_matrix), 3), "\n\n")
  
  return(list(
    distance_matrix = distance_matrix,
    formatted_matrix = formatted_matrix,
    time_info = time_info,
    method = method,
    binary = binary,
    summary = turnover_summary
  ))
}

#' Create Turnover Analysis Summary
#'
#' @param distance_matrix vegan distance object
#' @param formatted_matrix Full matrix format
#' @param method Distance method used
#' @param binary Whether binary data was used
#' @return List with summary statistics
create_turnover_summary <- function(distance_matrix, formatted_matrix, method, binary) {
  
  # Basic statistics
  turnover_values <- as.vector(distance_matrix)
  
  summary <- list(
    method = method,
    binary = binary,
    n_comparisons = length(turnover_values),
    n_time_bins = nrow(formatted_matrix),
    min_turnover = min(turnover_values),
    max_turnover = max(turnover_values),
    mean_turnover = mean(turnover_values),
    median_turnover = median(turnover_values),
    sd_turnover = sd(turnover_values)
  )
  
  # Find most and least similar time bin pairs
  max_index <- which(formatted_matrix == max(turnover_values), arr.ind = TRUE)[1, ]
  min_index <- which(formatted_matrix == min(turnover_values[turnover_values > 0]), arr.ind = TRUE)[1, ]
  
  summary$most_dissimilar_pair <- c(
    rownames(formatted_matrix)[max_index[1]], 
    colnames(formatted_matrix)[max_index[2]]
  )
  summary$most_similar_pair <- c(
    rownames(formatted_matrix)[min_index[1]], 
    colnames(formatted_matrix)[min_index[2]]
  )
  
  return(summary)
}

#' Extract Temporal Turnover Trends
#'
#' Analyzes how turnover changes with temporal distance between samples
#'
#' @param turnover_result Output from calculate_pairwise_turnover()
#' @return Data frame with temporal distance vs turnover relationships
extract_temporal_trends <- function(turnover_result) {
  if (is.null(turnover_result$time_info)) {
    warning("No temporal information available for trend analysis")
    return(NULL)
  }
  
  time_data <- turnover_result$time_info
  turnover_matrix <- turnover_result$formatted_matrix
  
  # Create all pairwise combinations
  n_bins <- nrow(time_data)
  temporal_trends <- data.frame()
  
  for (i in 1:(n_bins-1)) {
    for (j in (i+1):n_bins) {
      cstrat_i <- time_data$CSTRAT[i]
      cstrat_j <- time_data$CSTRAT[j]
      year_i <- time_data$YEAR[i]
      year_j <- time_data$YEAR[j]
      
      temporal_distance <- abs(year_j - year_i)
      stratigraphic_distance <- abs(cstrat_j - cstrat_i)
      turnover_value <- turnover_matrix[as.character(cstrat_i), as.character(cstrat_j)]
      
      temporal_trends <- rbind(temporal_trends, data.frame(
        cstrat_1 = cstrat_i,
        cstrat_2 = cstrat_j,
        year_1 = year_i,
        year_2 = year_j,
        temporal_distance = temporal_distance,
        stratigraphic_distance = stratigraphic_distance,
        turnover = turnover_value
      ))
    }
  }
  
  return(temporal_trends)
}