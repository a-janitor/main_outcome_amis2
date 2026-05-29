###############################################################################
# PROJECT: AMIS-II MAIN OUTCOME PAPER
# PURPOSE: Setup packages and local paths for data preparation and analysis
###############################################################################

### ------------------------------------------------------------------------ ###
### 1. LOAD REQUIRED PACKAGES
### ------------------------------------------------------------------------ ###

required_packages <- c(
  # Data import
  "readxl",      # Excel files
  "readr",       # Fast and reproducible data I/O
  
  # Data handling
  "dplyr",       # Data manipulation
  "tidyr",       # Data reshaping
  "purrr",       # Functional programming, e.g., map functions
  "stringr",     # String handling
  "tibble",      # Tidy data frames
  
  # Dates
  "lubridate",   # Date handling, e.g., age calculations
  
  # Statistics / modeling
  "car",
  "lavaan",      # CFA models for balanced parcel splits
  "lcmm",
  "lavaan.mi",
  
  # Visualization
  "ggplot2",
  
  # Legacy data formats
  "haven",
  "foreign"
)

# Install missing packages and load all required packages

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

### ------------------------------------------------------------------------ ###
### 1b. OPTIONAL PACKAGE: excel.link
### ------------------------------------------------------------------------ ###
# excel.link can be useful for legacy Excel workflows, but it may not install
# or run on all systems because it depends on a local Microsoft Excel setup.
# Therefore, it is loaded only if available.

if (requireNamespace("excel.link", quietly = TRUE)) {
  library(excel.link)
} else {
  message(
    "Optional package 'excel.link' is not installed or not available on this system. ",
    "The pipeline will continue without it."
  )
}

### ------------------------------------------------------------------------ ###
### 2. DEFINE LOCAL DATA PATH
### ------------------------------------------------------------------------ ###

Sys.setenv(
  AMIS_DATA_DIR = "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/MAIN OUTCOME/02_data"
)

data_dir <- Sys.getenv("AMIS_DATA_DIR")

if (data_dir == "") {
  stop(
    "AMIS_DATA_DIR is not set. Please set it to your local Seacloud data folder"  )
}

### ------------------------------------------------------------------------ ###
### 3. DEFINE STANDARD DATA SUBFOLDERS
### ------------------------------------------------------------------------ ###

raw_data_dir <- file.path(data_dir, "00_raw_final")
analysis_data_dir <- file.path(data_dir, "01_analysis_dataset")
raw_data_file <- file.path(raw_data_dir, "amis_mainoutcome_raw_final.RDS")
analysis_data_file <- file.path(analysis_data_dir, "amis_mainoutcome_analysis_v1.RDS")
raw_sources_file <- file.path(raw_data_dir, "raw_sources_imported.RDS")
clean_sources_file <- file.path(raw_data_dir, "clean_sources_for_raw_final.RDS")
raw_data_file <- file.path(raw_data_dir, "raw_final_dataset.RDS")


### ------------------------------------------------------------------------ ###
### 4. CHECK WHETHER FOLDERS EXIST
### ------------------------------------------------------------------------ ###

if (!dir.exists(raw_data_dir)) {
  warning("Raw data folder does not exist: ", raw_data_dir)
}

if (!dir.exists(analysis_data_dir)) {
  warning("Analysis data folder does not exist: ", analysis_data_dir)
}

### ------------------------------------------------------------------------ ###
### 5. PRINT PATH SUMMARY
### ------------------------------------------------------------------------ ###

message("AMIS data directory set to: ", data_dir)
message("Final raw data folder: ", raw_data_dir)
message("Analysis data folder: ", analysis_data_dir)
message("Final raw data file: ", raw_data_file)
message("Analysis data file: ", analysis_data_file)

