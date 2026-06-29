#!/usr/bin/env Rscript

# Root System Alignment Tool - Launch Script
# This script checks for required packages and launches the Shiny app

# Function to check and install packages
check_and_install <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    cat("Installing required packages:\n")
    print(new_packages)
    tryCatch({
      install.packages(new_packages, repos = "https://cloud.r-project.org/")
    }, error = function(e) {
      cat("Error installing packages:", e$message, "\n")
      cat("Please try installing packages manually:\n")
      cat("install.packages(c(", paste0('"', new_packages, '"', collapse = ", "), "))\n")
    })
  }
}

# List of required packages
required_packages <- c(
  "shiny",
  "shinydashboard",
  "imager",
  "EBImage",
  "magick",
  "XML",
  "ggplot2",
  "plotly",
  "DT",
  "rmarkdown"
)

# Check for BiocManager (needed for EBImage)
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = "https://cloud.r-project.org/")
}

# Install regular packages
regular_packages <- required_packages[required_packages != "EBImage"]
check_and_install(regular_packages)

# Install EBImage from Bioconductor
if (!requireNamespace("EBImage", quietly = TRUE)) {
  cat("Installing EBImage from Bioconductor...\n")
  BiocManager::install("EBImage")
}

# Set working directory to app location
# Robust method to get script directory that works in multiple contexts
get_script_dir <- function() {
  # Method 1: Try commandArgs (works with Rscript)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_name <- sub(file_arg, "", args[grep(file_arg, args)])
  if (length(script_name) > 0) {
    return(dirname(normalizePath(script_name)))
  }
  
  # Method 2: Try sys.frame (works in some interactive contexts)
  tryCatch({
    frame_files <- lapply(sys.frames(), function(x) x$ofile)
    frame_files <- Filter(Negate(is.null), frame_files)
    if (length(frame_files) > 0) {
      return(dirname(normalizePath(frame_files[[1]])))
    }
  }, error = function(e) NULL)
  
  # Method 3: Try rstudioapi (works in RStudio)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    tryCatch({
      return(dirname(rstudioapi::getSourceEditorContext()$path))
    }, error = function(e) NULL)
  }
  
  # Method 4: Fallback to current working directory
  return(getwd())
}

app_dir <- get_script_dir()
cat("Script directory detected as:", app_dir, "\n")
setwd(app_dir)

# Launch the app
cat("\n")
cat("========================================\n")
cat("Root System Alignment Tool\n")
cat("========================================\n")
cat("\n")
cat("The app will open in your default web browser.\n")
cat("To stop the app, press Ctrl+C in this console.\n")
cat("\n")

# Verify the app file exists
if (!file.exists("root_alignment_app.R")) {
  cat("Error: root_alignment_app.R not found in directory:", getwd(), "\n")
  cat("Please ensure you're running this script from the correct directory.\n")
  quit(status = 1)
}

# Run the app
tryCatch({
  shiny::runApp("root_alignment_app.R", launch.browser = TRUE)
}, error = function(e) {
  cat("Error launching the app:\n")
  cat(e$message, "\n")
  cat("\nTroubleshooting tips:\n")
  cat("1. Ensure all required packages are installed\n")
  cat("2. Check that root_alignment_app.R and root_alignment_utils.R exist\n")
  cat("3. Verify RSML data files are in the correct location\n")
})