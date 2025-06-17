# =============================================================================
# PALEO-ECOLOGICAL DATA CLEANING FUNCTIONS
# =============================================================================
# 
# This script contains functions for processing paleo-ecological microfossil data
# (diatoms, phytoliths) for stratigraphic analysis and integration with fish 
# morphology data. The pipeline handles:
# 1. Sample metadata extraction and classification (killifish vs stickleback)
# 2. Merging multiple microscopy line counts per geological sample
# 3. Taxonomic standardization (morphotype-level per Jacopo's specifications)
# 4. Age/depth integration from morphology dataset
# 5. rioja-compatible matrix creation for stratigraphic analysis
# 6. Export functions for ecological review and analysis
#
# Author: Aaron Myrold
# Dependencies: dplyr, tidyr, stringr, vegan (for diversity calculations)
# Collaborators: Jacopo (ecological expertise), Mike Bell (age estimates)
# =============================================================================

# Required libraries
required_packages <- c("dplyr", "tidyr", "stringr", "ggplot2")

for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    warning(paste("Package", pkg, "not available. Some functions may not work."))
  }
}

# Optional packages (loaded when needed)
if (!require("vegan", quietly = TRUE)) {
  warning("vegan package not available. Diversity calculations will not work.")
}

# =============================================================================
# SAMPLE METADATA AND CLASSIFICATION
# =============================================================================


# =============================================================================
# MICROSCOPY COUNT MERGING
# =============================================================================




# =============================================================================
# AGE AND DEPTH INTEGRATION
# =============================================================================



# =============================================================================
# RIOJA MATRIX CREATION
# =============================================================================



# =============================================================================
# DIVERSITY CALCULATIONS
# =============================================================================



# =============================================================================
# EXPORT FUNCTIONS
# =============================================================================


# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================




# =============================================================================
# END OF PALEO-ECOLOGICAL CLEANING FUNCTIONS
# =============================================================================

cat("✓ Paleo-ecological cleaning functions loaded successfully\n")
cat("Available functions: extract_paleo_metadata, merge_microscopy_counts,\n")
cat("                     standardize_taxonomy, integrate_age_data,\n")
cat("                     create_rioja_matrix, calculate_diversity_metrics,\n")
cat("                     export_for_jacopo, print_paleo_summary,\n")
cat("                     validate_paleo_data, export_paleo_results\n")