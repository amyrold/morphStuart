# This is a function to initialize a new project
# It creates required subfolders (see defaults) 
# It outputs an object that stores the relative paths to these sub-folders

# If calling the function within the working directory of the project, project_number = 0
# this will create necessary sub-folders in current working directory

# If creating a new root folder for the project, provide project_number >=1 and a project_name
# this will create a folder "pX.project_name/" to store all sub-folders
new_project <- function(project_number, project_name = NULL, subfolders = NULL, wk.dir = getwd()) {
  # Set default subfolders
  default_subfolders <- c("data", "scripts", "output", "docs")
  
  # Use provided sub-folders or defaults if none are provided
  if (is.null(subfolders)) {
    subfolders <- default_subfolders
  }
  
  # Initialize project paths list
  project_paths <- list()
  
  # Check if this is a "root directory" project (project_number = 0)
  is_root_project <- project_number == 0
  
  if (is_root_project) {
    # Use working directory as the base path for root projects
    base_path <- wk.dir
    project_paths$main <- wk.dir
    message("Creating folders directly in working directory: ", wk.dir)
  } else {
    # For normal projects, validate project name is provided
    if (is.null(project_name)) {
      stop("Project name is required when project_number is not 0")
    }
    
    # Create project folder name
    project_folder <- paste0("p", project_number, ".", project_name)
    
    # Full path for the project folder
    project_path <- file.path(wk.dir, project_folder)
    
    # Create the project folder if it doesn't exist
    if (!file.exists(project_path)) {
      dir.create(project_path)
      message("Created project folder: ", project_path)
    } else {
      message("Project folder already exists: ", project_path)
    }
    
    # Set base path for subfolders
    base_path <- project_path
    project_paths$main <- project_path
  }
  
  # Create sub-folders
  for (sub in subfolders) {
    # Determine subfolder name based on project type
    if (is_root_project) {
      subfolder_name <- sub
    } else {
      subfolder_name <- paste0("p", project_number, ".", sub)
    }
    
    # Full path for the subfolder
    subfolder_path <- file.path(base_path, subfolder_name)
    
    # Create sub-folder if it doesn't exist
    if (!file.exists(subfolder_path)) {
      dir.create(subfolder_path, recursive = TRUE)  # Added recursive=TRUE for nested paths
      message("Created subfolder: ", subfolder_path)
    } else {
      message("Subfolder already exists: ", subfolder_path)
    }
    
    # Add the sub-folder path to the list
    project_paths[[sub]] <- paste0(subfolder_path, "/")
  }
  
  # Return the list of paths as an object
  return(project_paths)
}
