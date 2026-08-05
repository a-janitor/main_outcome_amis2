#-----------------------------------------------------------------------
##### ACTIVATE SCIPEN #####
#-----------------------------------------------------------------------

options(scipen = 999)

#-----------------------------------------------------------------------
##### PACKAGE GROUPS #####
#-----------------------------------------------------------------------

project_packages <- list(
  
  data = c(
    "dplyr",
    "tidyr",
    "purrr",
    "readr",
    "readxl",
    "stringr",
    "tibble",
    "writexl",
    "openxlsx"
  ),
  
  preprocessing = c(
    "car",
    "lavaan"
  ),
  
  mplus = c(
    "MplusAutomation"
  ),
  
  tables = c(
    "flextable",
    "officer"
  ),
  
  figures = c(
    "ggplot2",
    "ggrepel"
  ),
  
  exploration = c(
    "semPlot"
  )
)

load_project_packages <- function(groups) {
  
  unknown_groups <- setdiff(
    groups,
    names(project_packages)
  )
  
  if (length(unknown_groups) > 0) {
    stop(
      "Unknown package group(s): ",
      paste(unknown_groups, collapse = ", "),
      call. = FALSE
    )
  }
  
  packages <- unique(
    unlist(
      project_packages[groups],
      use.names = FALSE
    )
  )
  
  missing_packages <- packages[
    !vapply(
      packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]
  
  if (length(missing_packages) > 0) {
    stop(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them before running this script.",
      call. = FALSE
    )
  }
  
  invisible(
    lapply(
      packages,
      library,
      character.only = TRUE
    )
  )
}


# Temporary compatibility with existing analysis scripts.
# Remove once every script loads its required package groups itself.
load_project_packages("data")

# Temporary compatibility with existing scripts.
check_packages <- function(packages, required = TRUE) {
  
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
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(missing_packages)
}

#-----------------------------------------------------------------------
##### ROOT PATHS #####
#-----------------------------------------------------------------------

seadrive_root     <- file.path("C:/Users/keil/seadrive_root", "Jan Keil", "Meine Bibliotheken")
github_root       <- file.path("C:/Users/keil/Documents", "main_outcome_amis2")
main_outcome_dir  <- file.path(seadrive_root, "MAIN OUTCOME")
github_r_dir      <- file.path(github_root, "R")
mplus_input_dir   <- file.path("C:/MPLUS", "Inputs")
mplus_archive_root <- file.path("C:/MPLUS", "Archive")


#-----------------------------------------------------------------------
##### MAIN PROJECT DIRECTORIES #####
#-----------------------------------------------------------------------

data_dir                 <- file.path(main_outcome_dir, "02_data")
data_prep_dir            <- file.path(data_dir, "02_data_Prep")
results_dir              <- file.path(main_outcome_dir, "03_results")
seadrive_mplus_data_dir  <- file.path(data_prep_dir, "MPlus_Dataset")


#-----------------------------------------------------------------------
##### MASTER DATA FILES #####
#-----------------------------------------------------------------------

master_excel_file      <- file.path(data_prep_dir, "AMIS_merged_analysis_dataset.xlsx")
m18_excel_file         <- file.path(data_prep_dir, "AMIS_merged_analysis_dataset_with_M18_classes.xlsx")
mplus_dataset_rds_file <- file.path(data_prep_dir, "AMIS_mplus_dataset.rds")
m18_prepared_rds_file  <- file.path(data_prep_dir, "AMIS_M18_analysis_prepared.rds")
merged_covariate_file  <- file.path(data_prep_dir, "AMIS_M18_with_covariates.rds")
merged_lcs_file        <- file.path(data_prep_dir, "AMIS_M18_LCS_with_covariates.rds")


#-----------------------------------------------------------------------
##### GITHUB MPLUS DIRECTORIES #####
#-----------------------------------------------------------------------

github_mplus_dir        <- file.path(github_root, "Mplus")
github_measurement_dir  <- file.path(github_mplus_dir, "01_measurement")
github_invariance_dir   <- file.path(github_mplus_dir, "02_invariance")
github_lcs_dir          <- file.path(github_mplus_dir, "03_lcs")
github_maltreatment_dir <- file.path(github_mplus_dir, "04_maltreatment")
github_biology_dir      <- file.path(github_mplus_dir, "05_biology")
model_registry_file     <- file.path(github_mplus_dir, "00_model_registry.csv")

#-----------------------------------------------------------------------
##### MPLUS RESULTS DIRECTORIES #####
#-----------------------------------------------------------------------

mplus_results_dir              <- file.path(results_dir, "Mplus")
mplus_results_inputs_dir       <- file.path(mplus_results_dir, "00_model_files", "inputs")
mplus_results_outputs_dir      <- file.path(mplus_results_dir, "00_model_files", "outputs")
mplus_results_dir_measurement  <- file.path(mplus_results_dir, "01_measurement")
mplus_results_dir_invariance   <- file.path(mplus_results_dir, "02_invariance")
mplus_results_dir_lcs          <- file.path(mplus_results_dir, "03_lcs")
mplus_results_dir_maltreatment <- file.path(mplus_results_dir, "04_maltreatment")
mplus_results_dir_biology      <- file.path(mplus_results_dir, "05_biology")
mplus_results_sensitivity_dir  <- file.path(mplus_results_dir, "06_sensitivity")


#-----------------------------------------------------------------------
##### TABLE AND FIGURE DIRECTORIES #####
#-----------------------------------------------------------------------

man_table_dir   <- paste0(main_outcome_dir,"/04_manuscript","/02_Tables")
man_figure_dir  <- paste0(main_outcome_dir,"/04_manuscript","/01_Figures")
supplement_dir  <- paste0(main_outcome_dir,"/05_supplement")
mplus_local_archive_dir  <- file.path(mplus_archive_root, "MAIN_OUTCOME")

#-----------------------------------------------------------------------
##### BASE MPLUS DATA FILES #####
#-----------------------------------------------------------------------

seadrive_mplus_data_file  <- file.path(seadrive_mplus_data_dir, "AMIS_mplus_dataset.dat")
seadrive_mplus_names_file <- file.path(seadrive_mplus_data_dir, "AMIS_mplus_names.rds")
mplus_data_file           <- file.path(mplus_input_dir, "AMIS_mplus_dataset.dat")
mplus_names_file_local    <- file.path(mplus_input_dir, "AMIS_mplus_names.rds")


#-----------------------------------------------------------------------
##### VARIABLE DICTIONARIES #####
#-----------------------------------------------------------------------

mplus_variable_dictionary_file <- file.path(github_root, "mplus_variable_dictionary.csv")
m18_dictionary_file            <- file.path(github_root, "mplus_variable_dictionary_m18.csv")
master_dictionary_file         <- file.path(github_root, "mplus_variable_dictionary_master.csv")


#-----------------------------------------------------------------------
##### FINAL OPTION B / MALTREATED-ONLY DATA FILES #####
#-----------------------------------------------------------------------

m18b_excel_file_mo        <- file.path(data_prep_dir, "AMIS_merged_analysis_dataset_with_M18_classes_mo.xlsx")
m18b_prepared_rds_file_mo <- file.path(data_prep_dir, "AMIS_M18_analysis_prepared_mo.rds")


#-----------------------------------------------------------------------
##### FINAL OPTION B: LOCAL MPLUS WORKING FILES #####
#-----------------------------------------------------------------------

mplus_data_file_m18_mo       <- file.path(mplus_input_dir, "AMIS_mplus_dataset_m18_mo.dat")
mplus_names_file_m18_mo      <- file.path(mplus_input_dir, "AMIS_mplus_names_m18_mo.rds")
mplus_names_text_file_m18_mo <- file.path(mplus_input_dir, "AMIS_mplus_names_m18_mo.txt")
mplus_rds_file_m18_mo        <- file.path(mplus_input_dir, "AMIS_mplus_dataset_m18_mo.rds")


#-----------------------------------------------------------------------
##### FINAL OPTION B: FILESYNC MPLUS COPIES #####
#-----------------------------------------------------------------------

seadrive_mplus_data_file_m18_mo       <- file.path(seadrive_mplus_data_dir, basename(mplus_data_file_m18_mo))
seadrive_mplus_names_file_m18_mo      <- file.path(seadrive_mplus_data_dir, basename(mplus_names_file_m18_mo))
seadrive_mplus_names_text_file_m18_mo <- file.path(seadrive_mplus_data_dir, basename(mplus_names_text_file_m18_mo))
seadrive_mplus_rds_file_m18_mo        <- file.path(seadrive_mplus_data_dir, basename(mplus_rds_file_m18_mo))


#-----------------------------------------------------------------------
##### FINAL OPTION B: RESULTS #####
#-----------------------------------------------------------------------

m18b_class_checks_dir_mo  <- file.path(mplus_results_dir_maltreatment, "01_class_description_mo")
m18b_lcs_results_dir_mo   <- file.path(mplus_results_dir_maltreatment, "02_classes_predicting_change_mo")
m18b_class_checks_file_mo <- file.path(m18b_class_checks_dir_mo, "M18b_class_description_mo.xlsx")
m18b_word_file_mo         <- file.path(m18b_class_checks_dir_mo, "M18b_class_characteristics_APA_mo.docx")
m18b_plot_file_mo         <- file.path(m18b_class_checks_dir_mo, "M18b_mt_burden_trajectories_with_nonmaltreated_mo.png")


#-----------------------------------------------------------------------
##### FINAL OPTION B: GITHUB MODEL DIRECTORIES #####
#-----------------------------------------------------------------------

github_m18b_class_description_dir_mo <- file.path(github_maltreatment_dir, "01_class_description_mo")
github_m18b_lcs_dir_mo               <- file.path(github_maltreatment_dir, "02_classes_predicting_change_mo")


#-----------------------------------------------------------------------
##### TEMPORARY LEGACY MPLUS FILES #####
#-----------------------------------------------------------------------

mplus_data_file_m18              <- file.path(mplus_input_dir, "AMIS_mplus_dataset_m18.dat")
mplus_names_file_m18_local       <- file.path(mplus_input_dir, "AMIS_mplus_names_m18.rds")
mplus_names_text_file_m18_local  <- file.path(mplus_input_dir, "AMIS_mplus_names_m18.txt")
mplus_rds_file_m18_local         <- file.path(mplus_input_dir, "AMIS_mplus_dataset_m18.rds")
seadrive_mplus_data_file_m18     <- file.path(seadrive_mplus_data_dir, basename(mplus_data_file_m18))
seadrive_mplus_names_file_m18    <- file.path(seadrive_mplus_data_dir, basename(mplus_names_file_m18_local))


#-----------------------------------------------------------------------
##### TEMPORARY LEGACY RESULTS #####
#-----------------------------------------------------------------------

m18_class_checks_dir   <- file.path(mplus_results_dir_maltreatment, "01_class_description")
m18_lcs_results_dir    <- file.path(mplus_results_dir_maltreatment, "02_classes_predicting_change")
m18_class_checks_file  <- file.path(m18_class_checks_dir, "M18_class_description.xlsx")
m18_word_file          <- file.path(m18_class_checks_dir, "M18_class_characteristics_APA.docx")
github_m18_class_dir   <- file.path(github_maltreatment_dir, "01_class_description")
github_m18_lcs_dir     <- file.path(github_maltreatment_dir, "02_classes_predicting_change")


#-----------------------------------------------------------------------
##### TEMPORARY COMPATIBILITY ALIASES #####
#-----------------------------------------------------------------------

mplus_results_dir_mal         <- mplus_results_dir_maltreatment
mplus_results_dir_bio         <- mplus_results_dir_biology
mplus_output_dir              <- mplus_input_dir
invariance_results_dir        <- mplus_results_dir_invariance
lcs_results_dir               <- mplus_results_dir_lcs
mplus_data_rds_file_m18_mo    <- seadrive_mplus_rds_file_m18_mo
mplus_names_txt_file_m18_mo   <- seadrive_mplus_names_text_file_m18_mo


#-----------------------------------------------------------------------
##### CREATE DIRECTORIES #####
#-----------------------------------------------------------------------

output_directories <- c(
  data_prep_dir,
  seadrive_mplus_data_dir,
  mplus_input_dir,
  mplus_archive_root,
  mplus_results_dir_measurement,
  mplus_results_dir_invariance,
  mplus_results_dir_lcs,
  mplus_results_dir_maltreatment,
  mplus_results_dir_biology,
  mplus_results_sensitivity_dir,
  mplus_results_inputs_dir,
  mplus_results_outputs_dir,
  man_table_dir,
  man_figure_dir,
  supplement_dir,
  mplus_local_archive_dir,
  m18b_class_checks_dir_mo,
  m18b_lcs_results_dir_mo,
  github_m18b_class_description_dir_mo,
  github_m18b_lcs_dir_mo
)

purrr::walk(unique(output_directories), dir.create, recursive = TRUE, showWarnings = FALSE)

#-----------------------------------------------------------------------
##### FILE HELPERS #####
#-----------------------------------------------------------------------

assert_file_exists <- function(path, label = "File") {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    stop(label, " not found:\n", path, call. = FALSE)
  }
  
  invisible(path)
}


wrap_mplus_names <- function(variable_names, max_width = 88, indent = "    ") {
  if (!length(variable_names)) {
    return(character())
  }
  
  output_lines <- character()
  current_line <- paste0(indent, variable_names[[1L]])
  
  for (variable_name in variable_names[-1L]) {
    proposed_line <- paste(current_line, variable_name)
    
    if (nchar(proposed_line) > max_width) {
      output_lines <- c(output_lines, current_line)
      current_line <- paste0(indent, variable_name)
    } else {
      current_line <- proposed_line
    }
  }
  
  c(output_lines, current_line)
}


copy_file_checked <- function(source_file, target, overwrite = TRUE) {
  assert_file_exists(source_file, "Source file")
  
  if (dir.exists(target)) {
    target <- file.path(target, basename(source_file))
  }
  
  dir.create(
    dirname(target),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!file.copy(source_file, target, overwrite = overwrite)) {
    stop("Could not copy file to:\n", target, call. = FALSE)
  }
  
  invisible(target)
}


copy_to_mplus <- function(source_file, overwrite = TRUE) {
  copy_file_checked(
    source_file,
    file.path(mplus_input_dir, basename(source_file)),
    overwrite
  )
}


#-----------------------------------------------------------------------
##### FORMATTING HELPERS #####
#-----------------------------------------------------------------------

format_confidence_interval <- function(lower, upper, digits = 3) {
  if (length(lower) != length(upper)) {
    stop("lower and upper must have equal lengths.", call. = FALSE)
  }
  
  result        <- rep("—", length(lower))
  available     <- !is.na(lower) & !is.na(upper)
  number_format <- paste0("%.", digits, "f")
  
  result[available] <- paste0(
    "[", sprintf(number_format, lower[available]),
    ", ", sprintf(number_format, upper[available]), "]"
  )
  
  result
}


format_p_value <- function(p, digits = 3) {
  result  <- rep("—", length(p))
  regular <- !is.na(p) & p >= .001
  
  result[!is.na(p) & p < .001] <- "< .001"
  result[regular] <- sub(
    "^0",
    "",
    sprintf(paste0("%.", digits, "f"), p[regular])
  )
  
  result
}


register_mplus_model <- function(filename, documentation) {
  required_fields <- c(
    "model_id", "model_name", "model_family", "script", "sample",
    "estimator", "specification", "residual_covariances",
    "covariates", "model_role", "notes"
  )
  
  missing_fields <- setdiff(required_fields, names(documentation))
  
  if (length(missing_fields)) {
    stop(
      "Missing documentation field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }
  
  valid_fields <- vapply(
    documentation[required_fields],
    \(x) is.character(x) && length(x) == 1L && !is.na(x),
    logical(1)
  )
  
  if (!all(valid_fields)) {
    stop("Every documentation field must contain one character value.", call. = FALSE)
  }
  
  entry <- documentation[required_fields]
  entry <- append(entry, list(input_file = filename), after = 3L)
  entry <- tibble::as_tibble(entry)
  
  if (file.exists(model_registry_file)) {
    registry <- readr::read_csv(
      model_registry_file,
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    )
    
    if (!setequal(names(registry), names(entry))) {
      stop("Model registry has unexpected columns.", call. = FALSE)
    }
    
    registry <- registry |>
      dplyr::select(dplyr::all_of(names(entry))) |>
      dplyr::filter(.data$model_id != entry$model_id)
  } else {
    registry <- entry[0, ]
  }
  
  registry <- registry |>
    dplyr::bind_rows(entry) |>
    dplyr::arrange(
      readr::parse_number(.data$model_id),
      .data$model_id
    )
  
  readr::write_csv(registry, model_registry_file, na = "")
  
  invisible(entry)
}


write_mplus_input <- function(
    syntax,
    filename,
    github_dir,
    documentation = NULL
) {
  input_file <- file.path(mplus_input_dir, filename)
  
  writeLines(syntax, input_file)
  copy_file_checked(input_file, file.path(github_dir, filename))
  
  if (!is.null(documentation)) {
    register_mplus_model(filename, documentation)
  }
  
  invisible(input_file)
}


#-----------------------------------------------------------------------
##### SETUP MESSAGE #####
#-----------------------------------------------------------------------

cat(
  "\nMAIN OUTCOME setup loaded.\n",
  "GitHub root: ", github_root, "\n",
  "SeaDrive root: ", main_outcome_dir, "\n",
  "Mplus input directory: ", mplus_input_dir, "\n\n",
  sep = ""
)