#' Create Rioja Stratigraphic Plots
#'
#' Generates stratigraphic plots using rioja package for microfossil abundance
#' data across geological time/depth. Always saves plot to file.
#'
#' @param species_matrix Matrix with samples as rows, taxa as columns (from prepare_rioja_species_data)
#' @param depth_data Named vector with sample names and corresponding ages/depths
#' @param plot_title Optional title for the plot
#' @param output_dir Directory to save plot
#' @param export_format Format for saved plot: "svg" or "png" (default "svg")
#' @return List containing the plot object and summary statistics
#'
#' @examples
#' plots <- create_rioja_strat_plots(species_matrix, depth_data)
#' plots <- create_rioja_strat_plots(species_matrix, depth_data, export_format = "png")
create_rioja_strat_plots <- function(species_matrix, depth_data, 
                                     plot_title = "Microfossil Stratigraphic Distribution",
                                     output_dir = "results/plots",
                                     export_format = "svg") {
  
  if (!requireNamespace("rioja", quietly = TRUE)) {
    stop("Package 'rioja' is required for stratigraphic plots")
  }
  
  if (!is.matrix(species_matrix)) {
    stop("species_matrix must be a matrix")
  }
  if (!is.numeric(depth_data) || is.null(names(depth_data))) {
    stop("depth_data must be a named numeric vector")
  }
  
  # Validate export format
  export_format <- match.arg(export_format, choices = c("svg", "png"))
  
  # Match samples between species matrix and depth data
  common_samples <- intersect(rownames(species_matrix), names(depth_data))
  
  if (length(common_samples) == 0) {
    stop("No common samples found between species matrix and depth data")
  }
  
  # Subset and order data consistently
  species_subset <- species_matrix[common_samples, , drop = FALSE]
  depth_subset <- depth_data[common_samples]
  
  # Order by depth/age
  depth_order <- order(depth_subset)
  species_ordered <- species_subset[depth_order, , drop = FALSE]
  depth_ordered <- depth_subset[depth_order]
  
  # Remove taxa with zero counts across all samples
  taxa_totals <- colSums(species_ordered)
  species_final <- species_ordered[, taxa_totals > 0, drop = FALSE]
  
  # Generate summary statistics
  plot_summary <- list(
    n_samples = nrow(species_final),
    n_taxa = ncol(species_final),
    age_range = range(depth_ordered),
    taxa_removed = sum(taxa_totals == 0),
    total_abundance = sum(species_final)
  )
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Set filename based on format
  file_extension <- if(export_format == "svg") ".svg" else ".png"
  plot_filename <- file.path(output_dir, paste0("rioja_stratigraphic_plot", file_extension))
  
  # Open appropriate graphics device
  if (export_format == "svg") {
    svg(plot_filename, width = 12, height = 8)
  } else {
    png(plot_filename, width = 12, height = 8, units = "in", res = 300)
  }
  
  # Create the plot
  strat_plot <- rioja::strat.plot(
    d = species_final,
    yvar = depth_ordered,
    title = plot_title,
    ylabel = if(any(grepl("YEAR", names(depth_data)))) "Age (years)" else "Depth (cm)",
    srt.xlabel = 22.5,         # Rotate labels (was 45)
    cex.xlabel = 0.7,          # Make labels smaller
    cex.title = 0.9,           # Slightly smaller title
    cex.ylabel = 0.8,          # Smaller y-axis label
    mgp = c(2.5, 0.7, 0),      # Tighter margins [axis title, axis labels, axis line]
    xSpace = 0.01,             # Add small space between taxa columns
    mar = c(8, 4, 4, 8)        # Increase bottom margin for rotated labels [bottom, left, top, right]
  )
  
  dev.off()
  cat("Plot saved to:", plot_filename, "\n")
  
  # Report results
  cat("Rioja Stratigraphic Plot Created:\n")
  cat("Samples plotted:", plot_summary$n_samples, "\n")
  cat("Taxa plotted:", plot_summary$n_taxa, "\n")
  cat("Taxa removed (zero counts):", plot_summary$taxa_removed, "\n")
  cat("Age/depth range:", plot_summary$age_range[1], "-", plot_summary$age_range[2], "\n")
  cat("Export format:", toupper(export_format), "\n\n")
  
  return(list(
    plot = strat_plot,
    summary = plot_summary,
    species_data = species_final,
    depth_data = depth_ordered
  ))
}