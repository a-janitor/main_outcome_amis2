#-----------------------------------------------------------------------
##### PROJECT SETUP: MAIN OUTCOME PAPER #####
#-----------------------------------------------------------------------

options(
  scipen = 999,
  stringsAsFactors = FALSE
)


#-----------------------------------------------------------------------
##### PACKAGE MANAGEMENT #####
#-----------------------------------------------------------------------

##### PACKAGES REQUIRED FOR ALMOST ALL SCRIPTS #####

core_packages <- c(
  "tidyverse",
  "readxl",
  "writexl"
)


##### PACKAGES USED IN SPECIFIC PROJECT SCRIPTS #####

mplus_packages <- c(
  "MplusAutomation",
  "broom"
)

reporting_packages <- c(
  "officer",
  "flextable"
)

project_utility_packages <- c(
  "here"
)

# Used only in an older alternative R-based LCS syntax.
legacy_packages <- c(
  "lavaan"
)


##### CHECK PACKAGES #####

check_packages <- function(
    packages,
    required = TRUE
) {
  
  missing_packages <- packages[
    !vapply(
      packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]
  
  if (
    required &&
    length(missing_packages) > 0
  ) {
    stop(
      "The following required packages are missing: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  }
  
  if (
    !required &&
    length(missing_packages) > 0
  ) {
    message(
      "Optional packages not installed: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  }
  
  invisible(
    missing_packages
  )
}


##### LOAD CORE PACKAGES #####

check_packages(
  core_packages,
  required = TRUE
)

invisible(
  lapply(
    core_packages,
    library,
    character.only = TRUE
  )
)


##### CHECK OPTIONAL PROJECT PACKAGES #####

check_packages(
  c(
    mplus_packages,
    reporting_packages,
    project_utility_packages
  ),
  required = FALSE
)

check_packages(
  legacy_packages,
  required = FALSE
)


# Use namespace-qualified calls in the analysis scripts:
#
# MplusAutomation::readModels()
# broom::tidy()
# officer::read_docx()
# flextable::flextable()
#
# This avoids masking functions from tidyverse/readxl.


#-----------------------------------------------------------------------
##### ROOT PATHS #####
#-----------------------------------------------------------------------

seadrive_root <- file.path(
  "C:/Users/keil/seadrive_root",
  "Jan Keil",
  "Meine Bibliotheken"
)

github_root <- file.path(
  "C:/Users/keil/Documents",
  "main_outcome_amis2"
)

main_outcome_dir <- file.path(
  seadrive_root,
  "MAIN OUTCOME"
)

github_r_dir <- file.path(
  github_root,
  "R"
)


#-----------------------------------------------------------------------
##### MAIN PROJECT DIRECTORIES #####
#-----------------------------------------------------------------------

data_dir <- file.path(
  main_outcome_dir,
  "02_data"
)

data_prep_dir <- file.path(
  data_dir,
  "02_data_Prep"
)

results_dir <- file.path(
  main_outcome_dir,
  "03_results"
)


#-----------------------------------------------------------------------
##### MASTER DATA FILES #####
#-----------------------------------------------------------------------

##### ORIGINAL MERGED ANALYSIS DATASET #####

master_excel_file <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset.xlsx"
)


##### DATASET WITH M18 CLASS ASSIGNMENTS #####

m18_excel_file <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes.xlsx"
)


##### PREPARED R DATA FILES #####

mplus_dataset_rds_file <- file.path(
  data_prep_dir,
  "AMIS_mplus_dataset.rds"
)

m18_prepared_rds_file <- file.path(
  data_prep_dir,
  "AMIS_M18_analysis_prepared.rds"
)

merged_covariate_file <- file.path(
  data_prep_dir,
  "AMIS_M18_with_covariates.rds"
)

merged_lcs_file <- file.path(
  data_prep_dir,
  "AMIS_M18_LCS_with_covariates.rds"
)


#-----------------------------------------------------------------------
##### MPLUS DATA DIRECTORY #####
#-----------------------------------------------------------------------

seadrive_mplus_data_dir <- file.path(
  data_prep_dir,
  "MPlus_Dataset"
)


##### STANDARD MPLUS DATA FILES ON SEADRIVE #####

seadrive_mplus_data_file <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_dataset.dat"
)

seadrive_mplus_names_file <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names.rds"
)


##### M18 MPLUS DATA FILES ON SEADRIVE #####

seadrive_mplus_data_file_m18 <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_dataset_m18.dat"
)

seadrive_mplus_names_file_m18 <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names_m18.rds"
)


# Retain shorter aliases used in existing scripts.
mplus_names_file <- seadrive_mplus_names_file
mplus_names_file_m18 <- seadrive_mplus_names_file_m18


#-----------------------------------------------------------------------
##### LOCAL MPLUS DIRECTORY #####
#-----------------------------------------------------------------------

mplus_input_dir <- "C:/MPLUS/Inputs"


##### STANDARD LOCAL MPLUS DATA FILES #####

mplus_data_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset.dat"
)

mplus_names_file_local <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names.rds"
)


##### LOCAL M18 MPLUS DATA FILES #####

mplus_data_file_m18 <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset_m18.dat"
)

mplus_names_file_m18_local <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18.rds"
)


##### FINAL M18 MODEL FILES #####

m18_input_file <- file.path(
  mplus_input_dir,
  "18_mt_burden_quadratic_3class.inp"
)

m18_output_file <- file.path(
  mplus_input_dir,
  "18_mt_burden_quadratic_3class.out"
)

m18_cprob_file <- file.path(
  mplus_input_dir,
  "18_mt_burden_quadratic_3class_cprob.dat"
)


#-----------------------------------------------------------------------
##### MPLUS RESULTS DIRECTORIES #####
#-----------------------------------------------------------------------

mplus_results_dir <- file.path(
  results_dir,
  "Mplus"
)

mplus_results_dir_measurement <- file.path(
  mplus_results_dir,
  "01_measurement"
)

mplus_results_dir_invariance <- file.path(
  mplus_results_dir,
  "02_invariance"
)

mplus_results_dir_lcs <- file.path(
  mplus_results_dir,
  "03_lcs"
)

mplus_results_dir_maltreatment <- file.path(
  mplus_results_dir,
  "04_maltreatment"
)

mplus_results_dir_biology <- file.path(
  mplus_results_dir,
  "05_biology"
)


# Compatibility aliases for older scripts.
mplus_results_dir_mal <- mplus_results_dir_maltreatment
mplus_results_dir_bio <- mplus_results_dir_biology
invariance_results_dir <- mplus_results_dir_invariance


#-----------------------------------------------------------------------
##### M18 CLASS AND LCS RESULTS #####
#-----------------------------------------------------------------------

m18_class_checks_dir <- file.path(
  mplus_results_dir_maltreatment,
  "01_class_description"
)

m18_lcs_results_dir <- file.path(
  mplus_results_dir_maltreatment,
  "02_classes_predicting_change"
)


##### CLASS-DESCRIPTION OUTPUT FILES #####

m18_class_checks_file <- file.path(
  m18_class_checks_dir,
  "M18_class_description.xlsx"
)

m18_word_file <- file.path(
  m18_class_checks_dir,
  "M18_class_characteristics_APA.docx"
)


#-----------------------------------------------------------------------
##### MEASUREMENT-INVARIANCE OUTPUT FILES #####
#-----------------------------------------------------------------------

invariance_fit_file <- file.path(
  mplus_results_dir_invariance,
  "SDQ_measurement_invariance_fit_indices.csv"
)

invariance_table_csv_file <- file.path(
  mplus_results_dir_invariance,
  "Table_SDQ_measurement_invariance.csv"
)

invariance_table_word_file <- file.path(
  mplus_results_dir_invariance,
  "Table_SDQ_measurement_invariance.docx"
)


#-----------------------------------------------------------------------
##### GITHUB DIRECTORIES #####
#-----------------------------------------------------------------------

github_mplus_dir <- file.path(
  github_root,
  "Mplus"
)

github_measurement_dir <- file.path(
  github_mplus_dir,
  "01_measurement"
)

github_invariance_dir <- file.path(
  github_mplus_dir,
  "02_invariance"
)

github_lcs_dir <- file.path(
  github_mplus_dir,
  "03_lcs"
)

github_maltreatment_dir <- file.path(
  github_mplus_dir,
  "04_maltreatment"
)

github_biology_dir <- file.path(
  github_mplus_dir,
  "05_biology"
)


##### SPECIFIC GITHUB ANALYSIS DIRECTORIES #####

github_m18_class_dir <- file.path(
  github_maltreatment_dir,
  "01_class_description"
)

github_m18_lcs_dir <- file.path(
  github_maltreatment_dir,
  "02_classes_predicting_change"
)


##### VARIABLE DICTIONARY #####

mplus_variable_dictionary_file <- file.path(
  github_root,
  "mplus_variable_dictionary.csv"
)


#-----------------------------------------------------------------------
##### DYNAMIC SES SOURCE FILES #####
#-----------------------------------------------------------------------

# Add the final file first once it becomes available.
ses_source_candidates <- c(
  file.path(
    data_prep_dir,
    "SES_source_data_final.xlsx"
  ),
  file.path(
    data_prep_dir,
    "SES_source_data_proxy.xlsx"
  )
)


# Preferred variable first, temporary proxy variables afterwards.
covariate_candidates <- list(
  
  caregiver_education = c(
    "FINAL_CAREGIVER_EDUCATION_VARIABLE",
    "PROXY_CAREGIVER_EDUCATION_VARIABLE"
  ),
  
  age_t2 = c(
    "mt_age_t2"
  ),
  
  sex = c(
    "sdq_sex"
  )
)


#-----------------------------------------------------------------------
##### CREATE PROJECT DIRECTORIES #####
#-----------------------------------------------------------------------

project_directories <- c(
  github_r_dir,
  
  data_prep_dir,
  seadrive_mplus_data_dir,
  
  mplus_input_dir,
  
  mplus_results_dir_measurement,
  mplus_results_dir_invariance,
  mplus_results_dir_lcs,
  mplus_results_dir_maltreatment,
  mplus_results_dir_biology,
  
  m18_class_checks_dir,
  m18_lcs_results_dir,
  
  github_mplus_dir,
  github_measurement_dir,
  github_invariance_dir,
  github_lcs_dir,
  github_maltreatment_dir,
  github_biology_dir,
  
  github_m18_class_dir,
  github_m18_lcs_dir
)

walk(
  unique(project_directories),
  ~ dir.create(
    path = .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


#-----------------------------------------------------------------------
##### GENERAL HELPER FUNCTIONS #####
#-----------------------------------------------------------------------

##### CHECK THAT A FILE EXISTS #####

assert_file_exists <- function(
    path,
    label = "File"
) {
  
  if (
    length(path) != 1 ||
    is.na(path) ||
    !file.exists(path)
  ) {
    stop(
      label,
      " not found:\n",
      path
    )
  }
  
  invisible(
    normalizePath(
      path,
      winslash = "/",
      mustWork = TRUE
    )
  )
}


##### SELECT FIRST EXISTING FILE #####

first_existing_file <- function(
    candidates,
    label = "Input file",
    required = TRUE
) {
  
  existing_files <- candidates[
    file.exists(candidates)
  ]
  
  if (length(existing_files) == 0) {
    
    if (required) {
      stop(
        label,
        " not found. Checked:\n",
        paste(
          candidates,
          collapse = "\n"
        )
      )
    }
    
    message(
      label,
      " is not available yet."
    )
    
    return(
      NA_character_
    )
  }
  
  selected_file <- existing_files[1]
  
  message(
    label,
    ": using ",
    selected_file
  )
  
  selected_file
}


##### SELECT FIRST EXISTING VARIABLE #####

first_existing_variable <- function(
    data,
    candidates,
    label = "Variable",
    required = TRUE
) {
  
  available_variables <- candidates[
    candidates %in% names(data)
  ]
  
  if (length(available_variables) == 0) {
    
    if (required) {
      stop(
        label,
        " not found. Checked: ",
        paste(
          candidates,
          collapse = ", "
        )
      )
    }
    
    message(
      label,
      " is not available yet."
    )
    
    return(
      NA_character_
    )
  }
  
  selected_variable <- available_variables[1]
  
  message(
    label,
    ": using ",
    selected_variable
  )
  
  selected_variable
}


##### CHECK UNIQUE IDENTIFIER #####

check_unique_id <- function(
    data,
    id = "sic",
    data_label = "Dataset"
) {
  
  if (!id %in% names(data)) {
    stop(
      id,
      " is missing from ",
      data_label,
      "."
    )
  }
  
  if (anyNA(data[[id]])) {
    stop(
      "Missing ",
      id,
      " values found in ",
      data_label,
      "."
    )
  }
  
  if (anyDuplicated(data[[id]]) > 0) {
    stop(
      "Duplicated ",
      id,
      " values found in ",
      data_label,
      "."
    )
  }
  
  invisible(TRUE)
}


##### CREATE MPLUS NAMES SYNTAX #####

wrap_mplus_names <- function(
    variable_names,
    max_width = 88,
    indent = "    "
) {
  
  output_lines <- character()
  current_line <- indent
  
  for (variable_name in variable_names) {
    
    proposed_line <- paste(
      current_line,
      variable_name
    )
    
    if (nchar(proposed_line) > max_width) {
      
      output_lines <- c(
        output_lines,
        current_line
      )
      
      current_line <- paste0(
        indent,
        variable_name
      )
      
    } else {
      
      current_line <- proposed_line
    }
  }
  
  c(
    output_lines,
    current_line
  )
}


##### COPY A FILE TO THE LOCAL MPLUS DIRECTORY #####

copy_to_mplus <- function(
    source_file,
    overwrite = TRUE
) {
  
  assert_file_exists(
    source_file,
    "Mplus source file"
  )
  
  target_file <- file.path(
    mplus_input_dir,
    basename(source_file)
  )
  
  copied <- file.copy(
    from = source_file,
    to = target_file,
    overwrite = overwrite
  )
  
  if (!copied) {
    stop(
      "Could not copy file to:\n",
      target_file
    )
  }
  
  target_file
}


#-----------------------------------------------------------------------
##### PROJECT INFORMATION #####
#-----------------------------------------------------------------------

cat(
  "\nMAIN OUTCOME setup loaded.",
  "\nGitHub root: ",
  github_root,
  "\nSeadrive root: ",
  main_outcome_dir,
  "\nMplus input directory: ",
  mplus_input_dir,
  "\n\n",
  sep = ""
)