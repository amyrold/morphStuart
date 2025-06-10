# =============================================================================
# GENERAL PROJECT SETUP SYSTEM
# =============================================================================
# 
# Configuration-driven project initialization system that reads settings from
# YAML files. This allows easy customization of project requirements without
# modifying setup code.
# 
# Usage: 
#   source("2.scripts/project_setup.R")
#   setup_project("project_config.yaml")
#   
# Or for reloading:
#   reload_project()
# 
# Author: Aaron Myrold
# Version: 3.0 - Configuration-driven
# =============================================================================

# Store setup state globally
.PROJECT_STATE <- new.env(parent = emptyenv())

# =============================================================================
# YAML CONFIGURATION LOADING
# =============================================================================

#' Load Project Configuration from YAML
#' 
#' Reads and validates project configuration from YAML file
#' 
#' @param config_file Path to YAML configuration file
#' @return List containing parsed configuration
load_project_config <- function(config_file = "project_config.yaml") {
  
  # Check if yaml package is available
  if (!requireNamespace("yaml", quietly = TRUE)) {
    cat("📦 Installing yaml package for configuration loading...\n")
    install.packages("yaml")
  }
  
  if (!require("yaml", quietly = TRUE)) {
    stop("❌ Cannot load yaml package. Please install manually: install.packages('yaml')")
  }
  
  # Check if config file exists
  if (!file.exists(config_file)) {
    stop(paste("❌ Configuration file not found:", config_file))
  }
  
  cat("📋 Loading project configuration from:", config_file, "\n")
  
  # Load and parse YAML
  tryCatch({
    config <- yaml::read_yaml(config_file)
    
    # Basic validation
    required_sections <- c("project", "packages", "folders")
    missing_sections <- setdiff(required_sections, names(config))
    
    if (length(missing_sections) > 0) {
      stop(paste("❌ Missing required config sections:", paste(missing_sections, collapse = ", ")))
    }
    
    cat("   ✅ Configuration loaded successfully\n")
    return(config)
    
  }, error = function(e) {
    stop(paste("❌ Error parsing YAML configuration:", e$message))
  })
}

# =============================================================================
# PACKAGE MANAGEMENT
# =============================================================================

#' Load Packages from Configuration
#' 
#' Installs and loads packages specified in config file
#' 
#' @param package_config Package configuration from YAML
#' @return List with installation results
load_packages_from_config <- function(package_config) {
  
  cat("📦 Loading packages from configuration...\n")
  
  # Flatten package list from different categories
  required_packages <- c()
  optional_packages <- c()
  
  for (category in names(package_config)) {
    if (category == "optional") {
      optional_packages <- c(optional_packages, package_config[[category]])
    } else {
      required_packages <- c(required_packages, package_config[[category]])
    }
  }
  
  # Results tracking
  results <- list(
    required_installed = c(),
    required_failed = c(),
    optional_installed = c(),
    optional_missing = c()
  )
  
  # Install and load required packages
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("   Installing required package:", pkg, "\n")
      tryCatch({
        install.packages(pkg, dependencies = TRUE)
        if (require(pkg, character.only = TRUE, quietly = TRUE)) {
          results$required_installed <- c(results$required_installed, pkg)
        } else {
          results$required_failed <- c(results$required_failed, pkg)
        }
      }, error = function(e) {
        results$required_failed <- c(results$required_failed, pkg)
      })
    }
  }
  
  # Load optional packages (warn but don't fail)
  for (pkg in optional_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("   Optional package not available:", pkg, "\n")
      results$optional_missing <- c(results$optional_missing, pkg)
    } else {
      results$optional_installed <- c(results$optional_installed, pkg)
    }
  }
  
  # Report results
  if (length(results$required_failed) > 0) {
    stop(paste("❌ Failed to load required packages:", paste(results$required_failed, collapse = ", ")))
  }
  
  cat("   ✅ Required packages loaded:", length(required_packages), "\n")
  if (length(results$optional_missing) > 0) {
    cat("   ⚠️ Optional packages missing:", length(results$optional_missing), "\n")
  }
  
  return(results)
}

# =============================================================================
# DIRECTORY MANAGEMENT
# =============================================================================

#' Create Project Folders from Configuration
#' 
#' Creates directory structure specified in config file
#' 
#' @param folder_config Folder configuration from YAML
#' @param base_path Base directory path
#' @return List of created folder paths
create_folders_from_config <- function(folder_config, base_path = getwd()) {
  
  cat("📁 Creating project directory structure...\n")
  
  folder_paths <- list()
  created_folders <- c()
  existing_folders <- c()
  
  for (folder in folder_config) {
    full_path <- file.path(base_path, folder)
    
    if (!dir.exists(full_path)) {
      dir.create(full_path, recursive = TRUE)
      created_folders <- c(created_folders, folder)
    } else {
      existing_folders <- c(existing_folders, folder)
    }
    
    # Store path with trailing slash for convenience
    # Convert folder name to valid R variable name for list access
    folder_key <- gsub("[^a-zA-Z0-9]", ".", folder)
    folder_paths[[folder_key]] <- paste0(full_path, "/")
  }
  
  # Report results
  if (length(created_folders) > 0) {
    cat("   ✅ Created folders:", length(created_folders), "\n")
  }
  if (length(existing_folders) > 0) {
    cat("   📂 Using existing folders:", length(existing_folders), "\n")
  }
  
  return(folder_paths)
}

# =============================================================================
# FUNCTION LOADING
# =============================================================================

#' Load Function Files from Configuration
#' 
#' Sources function files specified in config, handling dependencies and errors
#' 
#' @param function_config Function configuration from YAML  
#' @param scripts_path Path to scripts directory
#' @return List with loading results
load_functions_from_config <- function(function_config, scripts_path) {
  
  cat("🔗 Loading function files from configuration...\n")
  
  # Flatten function list, maintaining order within categories
  required_functions <- c()
  optional_functions <- c()
  
  for (category in names(function_config)) {
    if (category == "optional") {
      optional_functions <- c(optional_functions, function_config[[category]])
    } else {
      required_functions <- c(required_functions, function_config[[category]])
    }
  }
  
  # Results tracking
  results <- list(
    loaded = c(),
    failed = c(),
    missing = c()
  )
  
  # Load required functions
  for (func_file in required_functions) {
    file_path <- file.path(scripts_path, func_file)
    
    if (file.exists(file_path)) {
      tryCatch({
        source(file_path)
        results$loaded <- c(results$loaded, func_file)
        cat("   ✅", func_file, "\n")
      }, error = function(e) {
        results$failed <- c(results$failed, func_file)
        cat("   ❌", func_file, "- Error:", e$message, "\n")
      })
    } else {
      results$missing <- c(results$missing, func_file)
      cat("   ⚠️", func_file, "- File not found\n")
    }
  }
  
  # Load optional functions (don't fail on errors)
  for (func_file in optional_functions) {
    file_path <- file.path(scripts_path, func_file)
    
    if (file.exists(file_path)) {
      tryCatch({
        source(file_path)
        results$loaded <- c(results$loaded, func_file)
        cat("   ✅", func_file, "(optional)\n")
      }, error = function(e) {
        cat("   ⚠️", func_file, "- Optional file failed to load\n")
      })
    }
  }
  
  # Check for critical failures
  if (length(results$failed) > 0) {
    warning(paste("Some required function files failed to load:", paste(results$failed, collapse = ", ")))
  }
  
  cat("   ✅ Function files processed:", length(results$loaded), "loaded\n")
  return(results)
}

# =============================================================================
# SETTINGS CONFIGURATION
# =============================================================================

#' Apply Global Settings from Configuration
#' 
#' Sets up R options and global variables from config
#' 
#' @param settings_config Settings configuration from YAML
apply_settings_from_config <- function(settings_config) {
  
  cat("⚙️ Applying global settings...\n")
  
  # Set reproducibility seed
  if (!is.null(settings_config$seed)) {
    set.seed(settings_config$seed)
    cat("   🎲 Random seed set to:", settings_config$seed, "\n")
  }
  
  # Apply R options
  if (!is.null(settings_config$r_options)) {
    for (option_name in names(settings_config$r_options)) {
      option_value <- settings_config$r_options[[option_name]]
      do.call(options, setNames(list(option_value), option_name))
    }
    cat("   ⚙️ R options configured:", length(settings_config$r_options), "settings\n")
  }
  
  # Store other settings globally for access
  if (!is.null(settings_config$plot_defaults)) {
    assign(".PLOT_DEFAULTS", settings_config$plot_defaults, envir = .GlobalEnv)
  }
  
  cat("   ✅ Global settings applied\n")
}

# =============================================================================
# LOGGING INITIALIZATION
# =============================================================================

#' Initialize Logging from Configuration
#' 
#' Sets up logging system based on config settings
#' 
#' @param logging_config Logging configuration from YAML
#' @param project_config Project metadata
#' @return Logging object
initialize_logging_from_config <- function(logging_config, project_config) {
  
  if (is.null(logging_config) || !logging_config$enabled) {
    cat("📝 Logging disabled in configuration\n")
    return(NULL)
  }
  
  cat("📝 Initializing logging system...\n")
  
  # Check if logging functions are available
  if (!exists("initialize_log")) {
    cat("   ⚠️ Logging functions not yet loaded\n")
    return(NULL)
  }
  
  # Create project description for log
  project_description <- paste(
    project_config$name,
    "v", project_config$version,
    "- Configuration-driven setup"
  )
  
  # Initialize log
  log_obj <- initialize_log(project_description)
  
  cat("   ✅ Logging system initialized\n")
  return(log_obj)
}

# =============================================================================
# MAIN SETUP FUNCTION
# =============================================================================

#' Setup Project from Configuration File
#' 
#' Main function that orchestrates entire project setup from YAML config
#' 
#' @param config_file Path to YAML configuration file
#' @param force_reload Force reload even if already set up
#' @return List containing setup results and project objects
setup_project <- function(config_file = "project_config.yaml", force_reload = FALSE) {
  
  # Check if already set up (unless forcing reload)
  if (!force_reload && exists(".PROJECT_SETUP_COMPLETE", envir = .PROJECT_STATE)) {
    cat("⚡ Project already set up. Use reload_project() or force_reload=TRUE\n")
    return(invisible(.PROJECT_STATE$setup_results))
  }
  
  cat("🚀 Setting up project from configuration...\n")
  cat("=" , paste(rep("=", 50), collapse = ""), "\n")
  
  # Initialize results tracking
  setup_results <- list()
  
  tryCatch({
    
    # 1. Load configuration
    config <- load_project_config(config_file)
    setup_results$config <- config
    
    # 2. Load packages
    package_results <- load_packages_from_config(config$packages)
    setup_results$packages <- package_results
    
    # 3. Create directories
    folder_paths <- create_folders_from_config(config$folders)
    setup_results$folders <- folder_paths
    
    # 4. Apply global settings
    if (!is.null(config$settings)) {
      apply_settings_from_config(config$settings)
    }
    
    # 5. Load functions
    scripts_path <- folder_paths$`2.scripts`
    if (!is.null(scripts_path) && !is.null(config$functions)) {
      function_results <- load_functions_from_config(config$functions, scripts_path)
      setup_results$functions <- function_results
    }
    
    # 6. Initialize logging
    if (!is.null(config$logging)) {
      processing_log <- initialize_logging_from_config(config$logging, config$project)
      setup_results$logging <- processing_log
      
      # Make logging available globally
      if (!is.null(processing_log)) {
        assign("processing_log", processing_log, envir = .GlobalEnv)
      }
    }
    
    # 7. Make key objects available globally
    assign("p", folder_paths, envir = .GlobalEnv)
    assign("PROJECT_CONFIG", config, envir = .GlobalEnv)
    
    # 8. Store setup state
    .PROJECT_STATE$setup_results <- setup_results
    .PROJECT_STATE$config_file <- config_file
    .PROJECT_STATE$setup_time <- Sys.time()
    assign(".PROJECT_SETUP_COMPLETE", TRUE, envir = .PROJECT_STATE)
    
    cat("=" , paste(rep("=", 50), collapse = ""), "\n")
    cat("✅ Project setup complete!\n\n")
    
    # Print status
    print_project_status()
    
    return(invisible(setup_results))
    
  }, error = function(e) {
    cat("❌ Project setup failed:", e$message, "\n")
    stop(e)
  })
}

# =============================================================================
# RELOAD AND UTILITY FUNCTIONS  
# =============================================================================

#' Reload Project Setup
#' 
#' Reloads project setup to catch new files, packages, or config changes
#' 
#' @param config_file Optional new config file (uses previous if NULL)
reload_project <- function(config_file = NULL) {
  
  cat("🔄 Reloading project setup...\n")
  
  # Use previous config file if not specified
  if (is.null(config_file) && exists("config_file", envir = .PROJECT_STATE)) {
    config_file <- .PROJECT_STATE$config_file
  } else if (is.null(config_file)) {
    config_file <- "project_config.yaml"
  }
  
  # Clear setup state
  if (exists(".PROJECT_SETUP_COMPLETE", envir = .PROJECT_STATE)) {
    rm(".PROJECT_SETUP_COMPLETE", envir = .PROJECT_STATE)
  }
  
  # Run setup with force reload
  setup_project(config_file, force_reload = TRUE)
}

#' Print Project Status
#' 
#' Shows current project configuration and status
print_project_status <- function() {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  
  if (exists("PROJECT_CONFIG")) {
    config <- get("PROJECT_CONFIG", envir = .GlobalEnv)
    cat("🐟", config$project$name, "v", config$project$version, "\n")
  } else {
    cat("🔧 PROJECT STATUS\n")
  }
  
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Project structure
  if (exists("p")) {
    cat("📁 Project Structure: ✅ Initialized\n")
    folder_count <- length(get("p", envir = .GlobalEnv))
    cat("   Folders created:", folder_count, "\n")
  } else {
    cat("📁 Project Structure: ❌ Not initialized\n")
  }
  
  # Configuration
  if (exists("PROJECT_CONFIG")) {
    config <- get("PROJECT_CONFIG", envir = .GlobalEnv)
    cat("📋 Configuration: ✅ Loaded\n")
    cat("   Config file: ", .PROJECT_STATE$config_file %||% "unknown", "\n")
  } else {
    cat("📋 Configuration: ❌ Not loaded\n")
  }
  
  # Packages
  if (exists("PROJECT_CONFIG")) {
    config <- get("PROJECT_CONFIG", envir = .GlobalEnv)
    total_packages <- length(unlist(config$packages))
    cat("📦 Packages: ✅", total_packages, "configured\n")
  }
  
  # Functions
  key_functions <- c("variable_mapping", "handle_special_cases", "extract_paleo_metadata")
  functions_available <- sum(sapply(key_functions, exists))
  cat("🔗 Functions:", functions_available, "/", length(key_functions), "key functions available\n")
  
  # Logging
  if (exists("processing_log")) {
    cat("📝 Logging: ✅ Active\n")
  } else {
    cat("📝 Logging: ❌ Not initialized\n")
  }
  
  # Setup time
  if (exists("setup_time", envir = .PROJECT_STATE)) {
    setup_time <- .PROJECT_STATE$setup_time
    cat("⏰ Last setup:", format(setup_time, "%Y-%m-%d %H:%M:%S"), "\n")
  }
  
  cat("\n💡 Ready for analysis!\n")
  cat("   Use reload_project() to refresh setup\n")
  cat("   Use get_project_info() for detailed configuration\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
}

#' Get Detailed Project Information
#' 
#' Returns detailed information about current project setup
get_project_info <- function() {
  if (!exists("PROJECT_CONFIG")) {
    cat("❌ No project configuration loaded. Run setup_project() first.\n")
    return(invisible(NULL))
  }
  
  return(list(
    config = get("PROJECT_CONFIG", envir = .GlobalEnv),
    paths = if(exists("p")) get("p", envir = .GlobalEnv) else NULL,
    setup_results = .PROJECT_STATE$setup_results %||% NULL,
    setup_time = .PROJECT_STATE$setup_time %||% NULL
  ))
}

# =============================================================================
# UTILITY OPERATORS
# =============================================================================

# Null coalescing operator for cleaner code
`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# AUTO-INITIALIZATION
# =============================================================================

cat("✅ Configuration-driven project setup loaded\n")
cat("   Main functions: setup_project(), reload_project(), print_project_status()\n")
cat("   Example: setup_project('project_config.yaml')\n\n")