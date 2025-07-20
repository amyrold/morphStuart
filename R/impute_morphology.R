#' Multiple Imputation for Morphological Data
#'
#' This function performs multiple imputation on morphological trait data using MICE
#' (Multiple Imputation by Chained Equations) for trait variables and linear models
#' for stratigraphic variables that have deterministic relationships with CSTRAT.
#'
#' @param morph_data Data frame containing morphological measurements with missing values
#' @param config Project configuration list containing imputation parameters
#'
#' @return A mids object from the mice package containing the multiply imputed datasets
#' @export
#'
#' @details
#' The function uses two different imputation strategies:
#' 1. MICE with predictive mean matching (PMM) for morphological trait columns
#' 2. Linear model imputation for stratigraphic variables (ISTRAT, AGE, INT) based on CSTRAT
#'
#' Stratigraphic relationships:
#' - ISTRAT and AGE have deterministic -1 correlations with CSTRAT
#' - INT has a custom relationship with CSTRAT
#'
#' The function saves the mids object and optionally the completed dataset to disk.
impute_morphology <- function(morph_data, config) {
  
  # Load required packages
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' is required but not installed.")
  }
  
  # Extract imputation parameters from config
  imputation_config <- config$imputation
  
  # Validate input data
  if (!is.data.frame(morph_data)) {
    stop("morph_data must be a data frame")
  }
  
  # Check that required columns exist
  required_cols <- c("CSTRAT", imputation_config$morph_trait_columns)
  missing_cols <- setdiff(required_cols, colnames(morph_data))
  if (length(missing_cols) > 0) {
    warning("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Prepare data for imputation
  # Only include columns that will be used in imputation or are needed for prediction
  imputation_cols <- c(
    "fish_id", "LSPEC",  # ID columns
    "CSTRAT",            # Predictor for stratigraphic variables
    imputation_config$morph_trait_columns,  # Morphological traits
    names(imputation_config$linear_model_vars)  # Stratigraphic variables
  )
  
  # Filter to existing columns only
  available_cols <- intersect(imputation_cols, colnames(morph_data))
  impute_data <- morph_data[, available_cols, drop = FALSE]
  
  # Set up imputation methods
  # Default to empty (no imputation) for all variables
  method_vector <- mice::make.method(impute_data)
  method_vector[] <- ""  # Start with no imputation for any variable
  
  # Set MICE method for morphological trait columns
  trait_cols_present <- intersect(imputation_config$morph_trait_columns, colnames(impute_data))
  method_vector[trait_cols_present] <- imputation_config$mice_method
  
  # Handle stratigraphic variables with linear models
  linear_vars <- names(imputation_config$linear_model_vars)
  linear_vars_present <- intersect(linear_vars, colnames(impute_data))
  
  # For variables with deterministic relationships, use custom imputation
  for (var in linear_vars_present) {
    var_config <- imputation_config$linear_model_vars[[var]]
    
    if (var_config$relationship == "negative") {
      # For ISTRAT and AGE: perfect negative correlation with CSTRAT
      method_vector[var] <- "~ I(max(CSTRAT, na.rm = TRUE) + min(CSTRAT, na.rm = TRUE) - CSTRAT)"
    } else if (var_config$relationship == "custom") {
      # For INT: custom relationship - use regression
      method_vector[var] <- "norm.predict"
    }
  }
  
  # Create predictor matrix
  # By default, use all available variables to predict each variable
  predictor_matrix <- mice::make.predictorMatrix(impute_data)
  
  # Don't use ID columns as predictors
  id_cols <- c("fish_id", "LSPEC")
  id_cols_present <- intersect(id_cols, colnames(impute_data))
  if (length(id_cols_present) > 0) {
    predictor_matrix[, id_cols_present] <- 0
    predictor_matrix[id_cols_present, ] <- 0
  }
  
  # Print imputation setup if requested
  if (imputation_config$mice_print_flag) {
    cat("Imputation methods:\n")
    print(method_vector[method_vector != ""])
    cat("\nVariables being imputed:\n")
    print(names(method_vector)[method_vector != ""])
  }
  
  # Perform multiple imputation
  set.seed(imputation_config$mice_seed)
  
  mids_object <- mice::mice(
    data = impute_data,
    m = imputation_config$mice_datasets,
    method = method_vector,
    predictorMatrix = predictor_matrix,
    maxit = imputation_config$mice_iterations,
    printFlag = imputation_config$mice_print_flag,
    seed = imputation_config$mice_seed
  )
  
  # Post-process stratigraphic variables if needed
  # Handle deterministic relationships that mice might not capture perfectly
  if ("ISTRAT" %in% linear_vars_present && any(is.na(impute_data$ISTRAT))) {
    # Apply deterministic relationship: ISTRAT = -CSTRAT + constant
    # Find the constant from existing complete cases
    complete_cases <- complete.cases(impute_data[c("CSTRAT", "ISTRAT")])
    if (sum(complete_cases) > 0) {
      constant_istrat <- mean(impute_data$ISTRAT[complete_cases] + impute_data$CSTRAT[complete_cases], na.rm = TRUE)
      
      # Apply to all imputed datasets
      for (i in 1:imputation_config$mice_datasets) {
        complete_data <- mice::complete(mids_object, i)
        missing_istrat <- is.na(impute_data$ISTRAT)
        complete_data$ISTRAT[missing_istrat] <- constant_istrat - complete_data$CSTRAT[missing_istrat]
        mids_object$imp$ISTRAT[as.character(which(missing_istrat)), i] <- complete_data$ISTRAT[missing_istrat]
      }
    }
  }
  
  if ("AGE" %in% linear_vars_present && any(is.na(impute_data$AGE))) {
    # Apply deterministic relationship: AGE = -CSTRAT + constant
    complete_cases <- complete.cases(impute_data[c("CSTRAT", "AGE")])
    if (sum(complete_cases) > 0) {
      constant_age <- mean(impute_data$AGE[complete_cases] + impute_data$CSTRAT[complete_cases], na.rm = TRUE)
      
      # Apply to all imputed datasets
      for (i in 1:imputation_config$mice_datasets) {
        complete_data <- mice::complete(mids_object, i)
        missing_age <- is.na(impute_data$AGE)
        complete_data$AGE[missing_age] <- constant_age - complete_data$CSTRAT[missing_age]
        if ("AGE" %in% names(mids_object$imp)) {
          mids_object$imp$AGE[as.character(which(missing_age)), i] <- complete_data$AGE[missing_age]
        }
      }
    }
  }
  
  # Save mids object if requested
  if (imputation_config$save_mids_object) {
    # Ensure output directory exists
    output_dir <- dirname(imputation_config$mids_object_path)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    saveRDS(mids_object, file = imputation_config$mids_object_path)
    
    if (imputation_config$mice_print_flag) {
      cat("Saved mids object to:", imputation_config$mids_object_path, "\n")
    }
  }
  
  # Optionally save completed dataset (first imputation)
  if (!is.null(imputation_config$imputed_data_path)) {
    completed_data <- mice::complete(mids_object, 1)
    
    # Merge back with original data to preserve non-imputed columns
    original_cols <- setdiff(colnames(morph_data), colnames(completed_data))
    if (length(original_cols) > 0) {
      merge_cols <- intersect(c("fish_id", "LSPEC"), colnames(morph_data))
      if (length(merge_cols) > 0) {
        completed_data <- merge(
          completed_data, 
          morph_data[, c(merge_cols, original_cols), drop = FALSE],
          by = merge_cols,
          all.x = TRUE
        )
      }
    }
    
    # Ensure output directory exists
    output_dir <- dirname(imputation_config$imputed_data_path)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    saveRDS(completed_data, file = imputation_config$imputed_data_path)
    
    if (imputation_config$mice_print_flag) {
      cat("Saved completed dataset to:", imputation_config$imputed_data_path, "\n")
    }
  }
  
  # Print summary statistics
  if (imputation_config$mice_print_flag) {
    cat("\nImputation Summary:\n")
    cat("Number of imputed datasets:", mids_object$m, "\n")
    cat("Number of iterations:", mids_object$iteration, "\n")
    
    # Count missing values before and after
    original_na <- sum(is.na(impute_data))
    completed_na <- sum(is.na(mice::complete(mids_object, 1)))
    cat("Missing values before imputation:", original_na, "\n")
    cat("Missing values after imputation:", completed_na, "\n")
  }
  
  return(mids_object)
}