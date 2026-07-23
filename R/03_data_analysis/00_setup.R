#-----------------------------------------------------------------------
##### PROJECT SETUP: MAIN OUTCOME PAPER #####
#-----------------------------------------------------------------------

options(
  scipen = 999,
  stringsAsFactors = FALSE
)


#-----------------------------------------------------------------------
##### PACKAGES #####
#-----------------------------------------------------------------------

core_packages <- c(
  "tidyverse",
  "readxl",
  "writexl",
  "openxlsx"
)

optional_packages <- c(
  "MplusAutomation",
  "officer",
  "flextable",
  "broom",
  "here",
  "ggrepel"
)

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

  if (required && length(missing_packages) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", ")
    )
  }

  if (!required && length(missing_packages) > 0) {
    message(
      "Optional packages not installed: ",
      paste(missing_packages, collapse = ", ")
    )
  }

  invisible(missing_packages)
}

check_packages(core_packages, required = TRUE)

invisible(
  lapply(
    core_packages,
    library,
    character.only = TRUE
  )
)

missing_optional_packages <- check_packages(
  optional_packages,
  required = FALSE
)

available_optional_packages <- setdiff(
  optional_packages,
  missing_optional_packages
)

invisible(
  lapply(
    available_optional_packages,
    library,
    character.only = TRUE
  )
)

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

master_excel_file <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset.xlsx"
)

m18_excel_file <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes.xlsx"
)

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
##### MPLUS DATA FILES #####
#-----------------------------------------------------------------------

seadrive_mplus_data_dir <- file.path(
  data_prep_dir,
  "MPlus_Dataset"
)

seadrive_mplus_data_file <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_dataset.dat"
)

seadrive_mplus_names_file <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names.rds"
)

seadrive_mplus_data_file_m18 <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_dataset_m18.dat"
)

seadrive_mplus_names_file_m18 <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names_m18.rds"
)

mplus_input_dir <- "C:/MPLUS/Inputs"
mplus_archive_root <- "C:/MPLUS/Archive"

mplus_data_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset.dat"
)

mplus_names_file_local <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names.rds"
)

mplus_data_file_m18 <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset_m18.dat"
)

mplus_names_file_m18_local <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18.rds"
)

mplus_names_text_file_m18_local <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18.txt"
)

mplus_rds_file_m18_local <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset_m18.rds"
)

mplus_variable_dictionary_file <- file.path(
  github_root,
  "mplus_variable_dictionary.csv"
)

#-------------------------------------------------------------------------
##### MASTER VARIABLE DICTIONARY #####
#-------------------------------------------------------------------------

master_dictionary_file <- file.path(
  github_root,
  "mplus_variable_dictionary_master.csv"
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

# Compatibility aliases used in older scripts.
mplus_results_dir_mal <- mplus_results_dir_maltreatment
mplus_results_dir_bio <- mplus_results_dir_biology
mplus_output_dir <- mplus_input_dir
invariance_results_dir <- mplus_results_dir_invariance
lcs_results_dir <- mplus_results_dir_lcs


#-----------------------------------------------------------------------
##### M18 RESULTS #####
#-----------------------------------------------------------------------

m18_class_checks_dir <- file.path(
  mplus_results_dir_maltreatment,
  "01_class_description"
)

m18_lcs_results_dir <- file.path(
  mplus_results_dir_maltreatment,
  "02_classes_predicting_change"
)

m18_class_checks_file <- file.path(
  m18_class_checks_dir,
  "M18_class_description.xlsx"
)

m18_word_file <- file.path(
  m18_class_checks_dir,
  "M18_class_characteristics_APA.docx"
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

github_m18_class_dir <- file.path(
  github_maltreatment_dir,
  "01_class_description"
)

github_m18_lcs_dir <- file.path(
  github_maltreatment_dir,
  "02_classes_predicting_change"
)


#-----------------------------------------------------------------------
##### CREATE DIRECTORIES #####
#-----------------------------------------------------------------------

project_directories <- c(
  github_r_dir,
  data_prep_dir,
  seadrive_mplus_data_dir,
  mplus_input_dir,
  mplus_archive_root,
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
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


#-----------------------------------------------------------------------
##### HELPER FUNCTIONS #####
#-----------------------------------------------------------------------

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

  invisible(path)
}

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

copy_file_checked <- function(
    source_file,
    target,
    overwrite = TRUE
) {

  assert_file_exists(
    source_file,
    "Source file"
  )

  target_dir <- if (dir.exists(target)) {
    target
  } else {
    dirname(target)
  }

  dir.create(
    target_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  copied <- file.copy(
    from = source_file,
    to = target,
    overwrite = overwrite
  )

  if (!copied) {
    stop(
      "Could not copy file to:\n",
      target
    )
  }

  invisible(target)
}

copy_to_mplus <- function(
    source_file,
    overwrite = TRUE
) {

  target_file <- file.path(
    mplus_input_dir,
    basename(source_file)
  )

  copy_file_checked(
    source_file = source_file,
    target = target_file,
    overwrite = overwrite
  )

  target_file
}

cat(
  "\nMAIN OUTCOME setup loaded.\n",
  "GitHub root: ", github_root, "\n",
  "SeaDrive root: ", main_outcome_dir, "\n",
  "Mplus input directory: ", mplus_input_dir, "\n\n",
  sep = ""
)

create_dictionary_part <- function(
    data,
    mplus_names,
    excel_file,
    mplus_file
) {
  
  stopifnot(
    length(mplus_names) <= ncol(data)
  )
  
  tibble::tibble(
    dataset_excel = basename(excel_file),
    dataset_mplus = basename(mplus_file),
    original_name = names(data)[seq_along(mplus_names)],
    mplus_name = mplus_names,
    position_excel = seq_along(mplus_names),
    position_mplus = seq_along(mplus_names),
    retained = TRUE
  )
}

m18_dictionary_file <- file.path(
  github_root,
  "mplus_variable_dictionary_m18.csv"
)

master_dictionary_file <- file.path(
  github_root,
  "mplus_variable_dictionary_master.csv"
)


##### FORMAT CONFIDENCE INTERVALS #####

format_confidence_interval <- function(
    lower,
    upper,
    digits = 3
) {
  
  result <- rep(
    "—",
    length(lower)
  )
  
  available <- !is.na(lower) &
    !is.na(upper)
  
  result[available] <- paste0(
    "[",
    formatC(
      lower[available],
      format = "f",
      digits = digits
    ),
    ", ",
    formatC(
      upper[available],
      format = "f",
      digits = digits
    ),
    "]"
  )
  
  result
}


##### FORMAT P VALUES #####

format_p_value <- function(
    p,
    digits = 3
) {
  
  result <- rep(
    "—",
    length(p)
  )
  
  available <- !is.na(p)
  
  result[
    available & p < .001
  ] <- "< .001"
  
  regular <- available &
    p >= .001
  
  result[regular] <- sub(
    "^0",
    "",
    formatC(
      p[regular],
      format = "f",
      digits = digits
    )
  )
  
  result
}
