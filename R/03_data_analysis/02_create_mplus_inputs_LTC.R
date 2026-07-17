#-------------------------------------------------------------------------
##### SETUP #####
#-------------------------------------------------------------------------

##### LOAD PACKAGES ####

library(MplusAutomation)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(flextable)
library(officer)
library(readxl)
library(writexl)

##### DEFINE MPLUS DIRECTORIES ####

mplus_input_dir <- "C:/MPLUS/Inputs"

mplus_results_dir_measurement <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/03_results/Mplus/01_measurement"
)

mplus_results_dir_invariance <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/03_results/Mplus/02_invariance"
)

mplus_results_dir_lcs <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/03_results/Mplus/03_lcs"
)

mplus_results_dir_mal <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/03_results/Mplus/04_maltreatment"
)

mplus_results_dir_bio <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/03_results/Mplus/05_biology"
)

##### DEFINE GITHUB MPLUS DIRECTORIES ####

github_mplus_dir <- file.path(
  "C:/Users/keil/Documents/main_outcome_amis2",
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
##### DEFINE SEAGATE DATA FOLDER ######
seadrive_mplus_data_dir <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/02_data/02_data_Prep/MPlus_Dataset"
)

##### CREATE DIRECTORIES ####

directories <- c(
  mplus_input_dir,
  mplus_results_dir_measurement,
  mplus_results_dir_invariance,
  mplus_results_dir_lcs,
  mplus_results_dir_mal,
  mplus_results_dir_bio,
  github_measurement_dir,
  github_invariance_dir,
  github_lcs_dir,
  github_maltreatment_dir,
  github_biology_dir
)

invisible(
  sapply(
    directories,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

#-------------------------------------------------------------------------
##### LOAD AND CHECK MPLUS DATASET #####
#-------------------------------------------------------------------------

##### DEFINE MPLUS DATASET ####

mplus_data_name <- "AMIS_mplus_dataset.dat"

mplus_data_file <- file.path(
  mplus_input_dir,
  mplus_data_name
)

mplus_names_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names.rds"
)

stopifnot(
  file.exists(mplus_data_file),
  file.exists(mplus_names_file)
)

##### LOAD MPLUS VARIABLE NAMES ####

mplus_names <- readRDS(
  mplus_names_file
)

##### CHECK MPLUS VARIABLE NAMES ####

stopifnot(
  is.character(mplus_names),
  length(mplus_names) > 0,
  !anyNA(mplus_names),
  all(nzchar(mplus_names)),
  anyDuplicated(mplus_names) == 0,
  anyDuplicated(tolower(mplus_names)) == 0,
  all(nchar(mplus_names) <= 8)
)

##### CHECK NUMBER OF DATA COLUMNS ####

first_data_line <- readLines(
  mplus_data_file,
  n = 1,
  warn = FALSE
)

stopifnot(
  length(first_data_line) == 1,
  nzchar(first_data_line)
)

number_of_data_columns <- length(
  strsplit(
    first_data_line,
    split = "\t",
    fixed = TRUE
  )[[1]]
)

stopifnot(
  number_of_data_columns == length(mplus_names)
)

#-------------------------------------------------------------------------
##### CREATE MPLUS NAMES SYNTAX #####
#-------------------------------------------------------------------------

##### WRAP MPLUS VARIABLE NAMES ####

wrap_mplus_names <- function(
    variable_names,
    max_width = 88,
    indent = "    "
) {
  
  output_lines <- character()
  current_line <- indent
  
  for (variable_name in variable_names) {
    
    if (identical(current_line, indent)) {
      
      proposed_line <- paste0(
        indent,
        variable_name
      )
      
    } else {
      
      proposed_line <- paste(
        current_line,
        variable_name
      )
    }
    
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

##### CREATE MPLUS NAMES LIST ####

names_syntax <- paste(
  wrap_mplus_names(
    mplus_names,
    max_width = 88
  ),
  collapse = "\n"
)

##### CHECK MAXIMUM LINE LENGTH ####

maximum_names_line_length <- max(
  nchar(
    strsplit(
      names_syntax,
      "\n"
    )[[1]]
  )
)

stopifnot(
  maximum_names_line_length <= 88
)

#-------------------------------------------------------------------------
##### COMPARE VARIABLE NAMES WITH M8 #####
#-------------------------------------------------------------------------

##### DEFINE M8 REFERENCE INPUT ####

reference_input_file <- file.path(
  github_lcs_dir,
  "08_sdq_classical_lcs.inp"
)

stopifnot(
  file.exists(reference_input_file)
)

##### EXTRACT MPLUS STATEMENT ####

extract_mplus_statement <- function(
    input_file,
    statement
) {
  
  input_lines <- readLines(
    input_file,
    warn = FALSE
  )
  
  # Remove Mplus comments
  input_lines <- sub(
    "!.*$",
    "",
    input_lines
  )
  
  input_text <- paste(
    input_lines,
    collapse = " "
  )
  
  statement_pattern <- paste0(
    "\\b",
    statement,
    "\\s+(?:ARE|IS|=)\\s+(.*?);"
  )
  
  statement_match <- str_match(
    input_text,
    regex(
      statement_pattern,
      ignore_case = TRUE
    )
  )
  
  if (is.na(statement_match[1, 2])) {
    
    stop(
      paste0(
        "Could not find the ",
        statement,
        " statement in ",
        basename(input_file),
        "."
      )
    )
  }
  
  str_split(
    str_squish(
      statement_match[1, 2]
    ),
    "\\s+"
  )[[1]]
}

##### EXTRACT M8 VARIABLE NAMES ####

reference_names <- extract_mplus_statement(
  input_file = reference_input_file,
  statement = "NAMES"
)

##### COMPARE CURRENT NAMES WITH M8 ####

names_identical_to_m8 <- identical(
  tolower(mplus_names),
  tolower(reference_names)
)

if (!names_identical_to_m8) {
  
  maximum_length <- max(
    length(mplus_names),
    length(reference_names)
  )
  
  current_names_padded <- c(
    mplus_names,
    rep(
      NA_character_,
      maximum_length - length(mplus_names)
    )
  )
  
  reference_names_padded <- c(
    reference_names,
    rep(
      NA_character_,
      maximum_length - length(reference_names)
    )
  )
  
  names_comparison <- tibble(
    position = seq_len(maximum_length),
    current_name = current_names_padded,
    reference_name = reference_names_padded,
    names_identical = tolower(current_names_padded) ==
      tolower(reference_names_padded)
  ) |>
    filter(
      is.na(names_identical) |
        !names_identical
    )
  
  print(
    names_comparison,
    n = Inf
  )
  
  stop(
    paste0(
      "Current variable names or their ordering ",
      "differ from M8."
    )
  )
}

#-------------------------------------------------------------------------
##### COMPARE NAMES WITH VARIABLE DICTIONARY #####
#-------------------------------------------------------------------------

##### LOAD VARIABLE DICTIONARY ####

variable_dictionary_file <- file.path(
  "C:/Users/keil/Documents/main_outcome_amis2",
  "mplus_variable_dictionary.csv"
)

stopifnot(
  file.exists(variable_dictionary_file)
)

variable_dictionary <- read_csv(
  variable_dictionary_file,
  show_col_types = FALSE
)

##### CREATE ORDERED DICTIONARY NAMES ####

dictionary_names <- variable_dictionary |>
  filter(
    retained
  ) |>
  arrange(
    final_position
  ) |>
  pull(
    mplus_name
  )

##### COMPARE WITH CURRENT MPLUS NAMES ####

names_identical_to_dictionary <- identical(
  tolower(mplus_names),
  tolower(dictionary_names)
)

stopifnot(
  names_identical_to_dictionary
)

#-------------------------------------------------------------------------
##### CHECK MALTREATMENT VARIABLE MAPPING #####
#-------------------------------------------------------------------------

##### DEFINE EXPECTED MALTREATMENT MAPPING ####

expected_maltreatment_mapping <- tibble(
  mplus_name = c(
    "mal_t1",
    "mal_t12",
    "mal_all"
  ),
  expected_original_name = c(
    "mt_mal_status_t1",
    "mt_mal_status_t1t2",
    "mt_mal_status_t2all"
  )
)

##### GET OBSERVED MALTREATMENT MAPPING ####

observed_maltreatment_mapping <- variable_dictionary |>
  filter(
    mplus_name %in%
      expected_maltreatment_mapping$mplus_name
  ) |>
  select(
    mplus_name,
    original_name,
    final_position,
    retained
  ) |>
  left_join(
    expected_maltreatment_mapping,
    by = "mplus_name"
  ) |>
  mutate(
    mapping_correct = original_name ==
      expected_original_name
  ) |>
  arrange(
    match(
      mplus_name,
      expected_maltreatment_mapping$mplus_name
    )
  )

print(
  observed_maltreatment_mapping,
  n = Inf
)

stopifnot(
  nrow(observed_maltreatment_mapping) ==
    nrow(expected_maltreatment_mapping),
  all(observed_maltreatment_mapping$retained),
  all(observed_maltreatment_mapping$mapping_correct)
)

#-------------------------------------------------------------------------
##### DISPLAY FINAL VALIDATION RESULTS #####
#-------------------------------------------------------------------------

cat(
  "\n",
  "Mplus dataset: ",
  mplus_data_file,
  "\n",
  sep = ""
)

cat(
  "Number of data columns: ",
  number_of_data_columns,
  "\n",
  sep = ""
)

cat(
  "Number of Mplus names: ",
  length(mplus_names),
  "\n",
  sep = ""
)

cat(
  "Maximum NAMES line length: ",
  maximum_names_line_length,
  "\n",
  sep = ""
)

cat(
  "Names and ordering identical to M8: ",
  names_identical_to_m8,
  "\n",
  sep = ""
)

cat(
  "Names and ordering identical to variable dictionary: ",
  names_identical_to_dictionary,
  "\n",
  sep = ""
)

cat(
  "Maltreatment variable mapping correct: ",
  all(observed_maltreatment_mapping$mapping_correct),
  "\n",
  sep = ""
)

cat(
  "\nAll dataset and variable-name checks completed successfully.\n"
)
#-------------------------------------------------------------------------
##### ANALYSIS LTC #####
#-------------------------------------------------------------------------

##### DEFINE MALTREATMENT TRAJECTORY VARIABLES ####

subtype_variables <- c(
  "z5_sa",
  "z5_kk",
  "z5_vsa",
  "z5_fsz",
  "z5_ssz",
  "z5_ja",
  "z5_jea"
)

frequency_variables <- c(
  "zfq_sa",
  "zfq_kk",
  "zfq_vsa",
  "zfq_fsz",
  "zfq_ssz",
  "zfq_ja",
  "zfq_jea"
)

severity_variables <- c(
  "zsv_sa",
  "zsv_kk",
  "zsv_vsa",
  "zsv_fsz",
  "zsv_ssz",
  "zsv_ja",
  "zsv_jea"
)

trajectory_variables <- c(
  subtype_variables,
  frequency_variables,
  severity_variables
)

stopifnot(
  all(
    trajectory_variables %in%
      mplus_names
  )
)

##### FORMAT NUMERIC VALUES FOR MPLUS ####

format_mplus_number <- function(
    number,
    digits = 6
) {
  
  formatted_number <- formatC(
    number,
    format = "f",
    digits = digits,
    decimal.mark = "."
  )
  
  formatted_number <- sub(
    "0+$",
    "",
    formatted_number
  )
  
  formatted_number <- sub(
    "\\.$",
    "",
    formatted_number
  )
  
  if (formatted_number == "-0") {
    formatted_number <- "0"
  }
  
  formatted_number
}

##### DEFINE FUNCTION TO CREATE GROWTH MODEL ####

create_growth_input <- function(
    model_number,
    model_name,
    model_title,
    observed_variables,
    time_scores,
    intercept_factor,
    slope_factor,
    quadratic_factor = NULL
) {
  
  include_quadratic <- !is.null(
    quadratic_factor
  )
  
  growth_factors <- c(
    intercept_factor,
    slope_factor
  )
  
  if (include_quadratic) {
    
    growth_factors <- c(
      growth_factors,
      quadratic_factor
    )
  }
  
  stopifnot(
    length(observed_variables) == 7,
    length(time_scores) == 7,
    all(observed_variables %in% mplus_names),
    all(is.finite(time_scores)),
    all(nchar(growth_factors) <= 8)
  )
  
  usevariables_syntax <- paste(
    wrap_mplus_names(
      observed_variables,
      max_width = 88
    ),
    collapse = "\n"
  )
  
  formatted_time_scores <- vapply(
    time_scores,
    format_mplus_number,
    character(1)
  )
  
  growth_indicators_syntax <- paste0(
    "    ",
    observed_variables,
    "@",
    formatted_time_scores,
    collapse = "\n"
  )
  
  growth_factors_syntax <- paste(
    growth_factors,
    collapse = " "
  )
  
  if (include_quadratic) {
    
    covariance_syntax <- paste0(
      "  ",
      intercept_factor,
      " WITH ",
      slope_factor,
      " ",
      quadratic_factor,
      ";\n",
      "  ",
      slope_factor,
      " WITH ",
      quadratic_factor,
      ";\n"
    )
    
  } else {
    
    covariance_syntax <- paste0(
      "  ",
      intercept_factor,
      " WITH ",
      slope_factor,
      ";\n"
    )
  }
  
  input_syntax <- paste0(
    "TITLE:\n",
    "  ", model_title, ";\n\n",
    
    "DATA:\n",
    "  FILE = ", mplus_data_name, ";\n\n",
    
    "VARIABLE:\n",
    "  NAMES ARE\n",
    names_syntax, ";\n\n",
    
    "  USEVARIABLES ARE\n",
    usevariables_syntax, ";\n\n",
    
    "  MISSING ARE ALL (-999);\n",
    "  IDVARIABLE IS SIC_N;\n\n",
    
    "ANALYSIS:\n",
    "  TYPE = GENERAL;\n",
    "  ESTIMATOR = MLR;\n\n",
    
    "MODEL:\n",
    "  ", growth_factors_syntax, " |\n",
    growth_indicators_syntax, ";\n\n",
    
    "  [", growth_factors_syntax, "];\n",
    "  ", growth_factors_syntax, ";\n",
    covariance_syntax, "\n",
    
    "OUTPUT:\n",
    "  SAMPSTAT;\n",
    "  STANDARDIZED;\n",
    "  RESIDUAL;\n",
    "  CINTERVAL;\n",
    "  TECH1;\n",
    "  TECH4;\n"
  )
  
  input_file <- file.path(
    mplus_input_dir,
    paste0(
      sprintf(
        "%02d",
        model_number
      ),
      "_",
      model_name,
      ".inp"
    )
  )
  
  writeLines(
    input_syntax,
    con = input_file
  )
  
  copy_success <- file.copy(
    from = input_file,
    to = github_maltreatment_dir,
    overwrite = TRUE
  )
  
  stopifnot(
    file.exists(input_file),
    copy_success
  )
  
  cat(
    "Created: ",
    input_file,
    "\n",
    sep = ""
  )
  
  invisible(
    input_file
  )
}

#-------------------------------------------------------------------------
##### CREATE LINEAR REFERENCE MODELS #####
#-------------------------------------------------------------------------

##### DEFINE EQUALLY SPACED PERIOD SCORES ####

linear_period_scores <- 0:6

##### CREATE M9: LINEAR NUMBER OF SUBTYPES ####

input_file_m9 <- create_growth_input(
  model_number = 9,
  model_name = "mt_subtypes_linear_1class",
  model_title = paste0(
    "M9: One-class linear growth model for ",
    "maltreatment multiplicity"
  ),
  observed_variables = subtype_variables,
  time_scores = linear_period_scores,
  intercept_factor = "sub_i",
  slope_factor = "sub_s"
)

##### CREATE M10: LINEAR FREQUENCY ####

input_file_m10 <- create_growth_input(
  model_number = 10,
  model_name = "mt_frequency_linear_1class",
  model_title = paste0(
    "M10: One-class linear growth model for ",
    "maltreatment frequency"
  ),
  observed_variables = frequency_variables,
  time_scores = linear_period_scores,
  intercept_factor = "fq_i",
  slope_factor = "fq_s"
)

##### CREATE M11: LINEAR SEVERITY ####

input_file_m11 <- create_growth_input(
  model_number = 11,
  model_name = "mt_severity_linear_1class",
  model_title = paste0(
    "M11: One-class linear growth model for ",
    "maltreatment severity"
  ),
  observed_variables = severity_variables,
  time_scores = linear_period_scores,
  intercept_factor = "sev_i",
  slope_factor = "sev_s"
)

#-------------------------------------------------------------------------
##### DEFINE DEVELOPMENTAL TIME METRIC #####
#-------------------------------------------------------------------------

##### LOAD AGE AT T5 FROM MPLUS DATASET ####

required_time_variables <- c(
  "SIC_N",
  "aget5m"
)

stopifnot(
  all(
    required_time_variables %in%
      mplus_names
  )
)

mplus_age_data <- read_tsv(
  file = mplus_data_file,
  col_names = mplus_names,
  col_types = cols(
    .default = col_double()
  ),
  col_select = all_of(
    required_time_variables
  ),
  na = "-999",
  show_col_types = FALSE,
  progress = FALSE,
  name_repair = "minimal"
)

##### CALCULATE REPRESENTATIVE JEA MIDPOINT ####

jea_age_information <- mplus_age_data |>
  filter(
    !is.na(aget5m),
    aget5m > 216
  ) |>
  distinct(
    SIC_N,
    .keep_all = TRUE
  ) |>
  summarise(
    n_jea = n(),
    minimum_age_t5_months = min(
      aget5m
    ),
    mean_age_t5_months = mean(
      aget5m
    ),
    median_age_t5_months = median(
      aget5m
    ),
    maximum_age_t5_months = max(
      aget5m
    ),
    mean_jea_midpoint_months = mean(
      (
        216 + aget5m
      ) / 2
    )
  )

print(
  jea_age_information
)

jea_midpoint_years <- jea_age_information |>
  pull(
    mean_jea_midpoint_months
  ) / 12

stopifnot(
  length(jea_midpoint_years) == 1,
  is.finite(jea_midpoint_years),
  jea_midpoint_years > 18
)

##### DEFINE DEVELOPMENTAL PERIOD MIDPOINTS ####

developmental_midpoints <- c(
  sa = 0.5,
  kk = 2.0,
  vsa = 4.5,
  fsz = 7.0,
  ssz = 10.5,
  ja = 15.5,
  jea = jea_midpoint_years
)

##### CENTER AND SCALE DEVELOPMENTAL TIME ####

time_center_age <- mean(
  range(
    developmental_midpoints
  )
)

# One time unit corresponds to three years
time_scale_years <- 3

developmental_time_scores <- (
  developmental_midpoints -
    time_center_age
) / time_scale_years

##### DISPLAY DEVELOPMENTAL TIME METRIC ####

developmental_time_table <- tibble(
  period = names(
    developmental_midpoints
  ),
  midpoint_years = as.numeric(
    developmental_midpoints
  ),
  time_score = as.numeric(
    developmental_time_scores
  ),
  quadratic_time_score = as.numeric(
    developmental_time_scores^2
  )
)

print(
  developmental_time_table,
  n = Inf
)

#-------------------------------------------------------------------------
##### CREATE QUADRATIC TRAJECTORY MODELS #####
#-------------------------------------------------------------------------

##### CREATE M12: QUADRATIC NUMBER OF SUBTYPES ####

input_file_m12 <- create_growth_input(
  model_number = 12,
  model_name = "mt_subtypes_quadratic_1class",
  model_title = paste0(
    "M12: Quadratic growth model for number of ",
    "maltreatment subtypes"
  ),
  observed_variables = subtype_variables,
  time_scores = developmental_time_scores,
  intercept_factor = "sub_i",
  slope_factor = "sub_s",
  quadratic_factor = "sub_q"
)

##### CREATE M13: QUADRATIC FREQUENCY ####

input_file_m13 <- create_growth_input(
  model_number = 13,
  model_name = "mt_frequency_quadratic_1class",
  model_title = paste0(
    "M13: Quadratic growth model for ",
    "maltreatment frequency"
  ),
  observed_variables = frequency_variables,
  time_scores = developmental_time_scores,
  intercept_factor = "fq_i",
  slope_factor = "fq_s",
  quadratic_factor = "fq_q"
)

##### CREATE M14: QUADRATIC SEVERITY ####

input_file_m14 <- create_growth_input(
  model_number = 14,
  model_name = "mt_severity_quadratic_1class",
  model_title = paste0(
    "M14: Quadratic growth model for ",
    "maltreatment severity"
  ),
  observed_variables = severity_variables,
  time_scores = developmental_time_scores,
  intercept_factor = "sev_i",
  slope_factor = "sev_s",
  quadratic_factor = "sev_q"
)

# #-------------------------------------------------------------------------
# ##### CHECK CREATED LTC INPUTS #####
# #-------------------------------------------------------------------------
# 
# created_ltc_inputs <- c(
#   input_file_m9,
#   input_file_m10,
#   input_file_m11,
#   input_file_m12,
#   input_file_m13,
#   input_file_m14
# )
# 
# stopifnot(
#   all(
#     file.exists(
#       created_ltc_inputs
#     )
#   ),
#   all(
#     file.exists(
#       file.path(
#         github_maltreatment_dir,
#         basename(
#           created_ltc_inputs
#         )
#       )
#     )
#   )
# )
# 
# cat(
#   "\nAll linear and quadratic trajectory inputs were created successfully.\n"
# )

#-------------------------------------------------------------------------
##### CREATE M15: JOINT QUADRATIC TRAJECTORY MODEL #####
#-------------------------------------------------------------------------

##### DEFINE JOINT GROWTH FACTORS ####

joint_growth_factors <- c(
  "sub_i",
  "sub_s",
  "sub_q",
  "fq_i",
  "fq_s",
  "fq_q",
  "sev_i",
  "sev_s",
  "sev_q"
)

stopifnot(
  all(
    nchar(joint_growth_factors) <= 8
  ),
  anyDuplicated(joint_growth_factors) == 0,
  length(developmental_time_scores) == 7,
  all(is.finite(developmental_time_scores))
)

##### CREATE GROWTH PROCESS SYNTAX ####

create_growth_process_syntax <- function(
    growth_factors,
    observed_variables,
    time_scores
) {
  
  stopifnot(
    length(growth_factors) == 3,
    length(observed_variables) == length(time_scores)
  )
  
  formatted_time_scores <- vapply(
    time_scores,
    format_mplus_number,
    character(1)
  )
  
  indicator_syntax <- paste0(
    "    ",
    observed_variables,
    "@",
    formatted_time_scores,
    collapse = "\n"
  )
  
  paste0(
    "  ",
    paste(
      growth_factors,
      collapse = " "
    ),
    " |\n",
    indicator_syntax,
    ";"
  )
}

##### CREATE SUBTYPE GROWTH SYNTAX ####

subtype_growth_syntax <- create_growth_process_syntax(
  growth_factors = c(
    "sub_i",
    "sub_s",
    "sub_q"
  ),
  observed_variables = subtype_variables,
  time_scores = developmental_time_scores
)

##### CREATE FREQUENCY GROWTH SYNTAX ####

frequency_growth_syntax <- create_growth_process_syntax(
  growth_factors = c(
    "fq_i",
    "fq_s",
    "fq_q"
  ),
  observed_variables = frequency_variables,
  time_scores = developmental_time_scores
)

##### CREATE SEVERITY GROWTH SYNTAX ####

severity_growth_syntax <- create_growth_process_syntax(
  growth_factors = c(
    "sev_i",
    "sev_s",
    "sev_q"
  ),
  observed_variables = severity_variables,
  time_scores = developmental_time_scores
)

##### CREATE ALL GROWTH-FACTOR COVARIANCES ####

create_all_covariances_syntax <- function(
    variable_names
) {
  
  covariance_lines <- vapply(
    seq_len(
      length(variable_names) - 1
    ),
    function(position) {
      
      paste0(
        "  ",
        variable_names[position],
        " WITH ",
        paste(
          variable_names[
            (position + 1):length(variable_names)
          ],
          collapse = " "
        ),
        ";"
      )
    },
    character(1)
  )
  
  paste(
    covariance_lines,
    collapse = "\n"
  )
}

growth_covariances_syntax <- create_all_covariances_syntax(
  joint_growth_factors
)

##### CREATE WITHIN-PERIOD RESIDUAL COVARIANCES ####

create_within_period_residual_syntax <- function(
    subtype_variables,
    frequency_variables,
    severity_variables
) {
  
  stopifnot(
    length(subtype_variables) ==
      length(frequency_variables),
    length(subtype_variables) ==
      length(severity_variables)
  )
  
  period_covariance_blocks <- vapply(
    seq_along(
      subtype_variables
    ),
    function(position) {
      
      paste0(
        "  ",
        subtype_variables[position],
        " WITH ",
        frequency_variables[position],
        " ",
        severity_variables[position],
        ";\n",
        
        "  ",
        frequency_variables[position],
        " WITH ",
        severity_variables[position],
        ";"
      )
    },
    character(1)
  )
  
  paste(
    period_covariance_blocks,
    collapse = "\n"
  )
}

within_period_residual_syntax <-
  create_within_period_residual_syntax(
    subtype_variables = subtype_variables,
    frequency_variables = frequency_variables,
    severity_variables = severity_variables
  )

##### CREATE JOINT USEVARIABLES SYNTAX ####

joint_usevariables_syntax <- paste(
  wrap_mplus_names(
    trajectory_variables,
    max_width = 88
  ),
  collapse = "\n"
)

##### CREATE M15 INPUT SYNTAX ####

input_syntax <- paste0(
  "TITLE:\n",
  "  M15: Joint quadratic parallel-process growth model for ",
  "maltreatment;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  "  NAMES ARE\n",
  names_syntax, ";\n\n",
  
  "  USEVARIABLES ARE\n",
  joint_usevariables_syntax, ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = GENERAL;\n",
  "  ESTIMATOR = MLR;\n\n",
  
  "MODEL:\n",
  "  ! Quadratic growth process for number of subtypes\n",
  subtype_growth_syntax, "\n\n",
  
  "  ! Quadratic growth process for frequency\n",
  frequency_growth_syntax, "\n\n",
  
  "  ! Quadratic growth process for severity\n",
  severity_growth_syntax, "\n\n",
  
  "  ! Growth-factor means\n",
  "  [",
  paste(
    joint_growth_factors,
    collapse = " "
  ),
  "];\n\n",
  
  "  ! Growth-factor variances\n",
  "  ",
  paste(
    joint_growth_factors,
    collapse = " "
  ),
  ";\n\n",
  
  "  ! Covariances among all growth factors\n",
  growth_covariances_syntax, "\n\n",
  
  "  ! Within-period residual covariances\n",
  within_period_residual_syntax, "\n\n",
  
  "OUTPUT:\n",
  "  SAMPSTAT;\n",
  "  STANDARDIZED;\n",
  "  RESIDUAL;\n",
  "  CINTERVAL;\n",
  "  MODINDICES(10);\n",
  "  TECH1;\n",
  "  TECH4;\n"
)

##### SAVE M15 INPUT ####

input_file <- file.path(
  mplus_input_dir,
  "15_mt_parallel_quadratic_1class.inp"
)

writeLines(
  input_syntax,
  con = input_file
)

##### COPY M15 INPUT TO GITHUB ####

copy_success <- file.copy(
  from = input_file,
  to = github_maltreatment_dir,
  overwrite = TRUE
)

stopifnot(
  file.exists(input_file),
  copy_success,
  file.exists(
    file.path(
      github_maltreatment_dir,
      basename(input_file)
    )
  )
)

input_file_m15 <- input_file

cat(
  "Created: ",
  input_file_m15,
  "\n",
  sep = ""
)

#-------------------------------------------------------------------------
##### CREATE M16: QUADRATIC MALTREATMENT BURDEN MODEL #####
#-------------------------------------------------------------------------

##### DEFINE MALTREATMENT BURDEN VARIABLES ####

burden_variables <- c(
  "zind_sa",
  "zind_kk",
  "zind_vsa",
  "zind_fsz",
  "zind_ssz",
  "zind_ja",
  "zind_jea"
)

##### CHECK MALTREATMENT BURDEN VARIABLES ####

stopifnot(
  length(burden_variables) == 7,
  all(
    burden_variables %in%
      mplus_names
  ),
  anyDuplicated(burden_variables) == 0,
  length(developmental_time_scores) == 7,
  all(is.finite(developmental_time_scores))
)

##### CREATE QUADRATIC BURDEN INPUT ####

input_file_m16 <- create_growth_input(
  model_number = 16,
  model_name = "mt_burden_quadratic_1class",
  model_title = paste0(
    "M16: One-class quadratic growth model for ",
    "maltreatment burden"
  ),
  observed_variables = burden_variables,
  time_scores = developmental_time_scores,
  intercept_factor = "bur_i",
  slope_factor = "bur_s",
  quadratic_factor = "bur_q"
)

##### CHECK CREATED M16 INPUT ####

stopifnot(
  file.exists(input_file_m16),
  file.exists(
    file.path(
      github_maltreatment_dir,
      basename(input_file_m16)
    )
  )
)

cat(
  "\nM16 quadratic maltreatment-burden input created successfully.\n"
)

#-------------------------------------------------------------------------
##### CREATE M17: TWO-CLASS QUADRATIC BURDEN MODEL #####
#-------------------------------------------------------------------------

##### DEFINE TWO-CLASS MODEL SETTINGS ####

number_of_classes_m17 <- 2

burden_growth_factors <- c(
  "bur_i",
  "bur_s",
  "bur_q"
)

stopifnot(
  length(burden_variables) == 7,
  all(burden_variables %in% mplus_names),
  length(developmental_time_scores) == 7,
  all(is.finite(developmental_time_scores)),
  all(nchar(burden_growth_factors) <= 8)
)

##### CREATE BURDEN GROWTH SYNTAX ####

burden_growth_syntax <- create_growth_process_syntax(
  growth_factors = burden_growth_factors,
  observed_variables = burden_variables,
  time_scores = developmental_time_scores
)

##### CREATE BURDEN USEVARIABLES SYNTAX ####

burden_usevariables_syntax <- paste(
  wrap_mplus_names(
    burden_variables,
    max_width = 88
  ),
  collapse = "\n"
)

##### CREATE CLASS-SPECIFIC MEAN SYNTAX ####

class_specific_means_syntax <- paste(
  vapply(
    seq_len(
      number_of_classes_m17
    ),
    function(class_number) {
      
      paste0(
        "  %c#",
        class_number,
        "%\n",
        "  [",
        paste(
          burden_growth_factors,
          collapse = " "
        ),
        "];"
      )
    },
    character(1)
  ),
  collapse = "\n\n"
)

##### CREATE M17 INPUT SYNTAX ####

input_syntax <- paste0(
  "TITLE:\n",
  "  M17: Two-class quadratic growth mixture model for ",
  "maltreatment burden;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  "  NAMES ARE\n",
  names_syntax, ";\n\n",
  
  "  USEVARIABLES ARE\n",
  burden_usevariables_syntax, ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n",
  "  CLASSES = c(", number_of_classes_m17, ");\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = MIXTURE;\n",
  "  ESTIMATOR = MLR;\n",
  "  STARTS = 2000 500;\n",
  "  STITERATIONS = 20;\n",
  "  LRTSTARTS = 0 0 500 100;\n\n",
  
  "MODEL:\n",
  "  %OVERALL%\n\n",
  
  "  ! Quadratic growth model\n",
  burden_growth_syntax, "\n\n",
  
  "  ! Class-invariant growth-factor variances\n",
  "  ",
  paste(
    burden_growth_factors,
    collapse = " "
  ),
  ";\n\n",
  
  "  ! Class-invariant growth-factor covariances\n",
  "  bur_i WITH bur_s bur_q;\n",
  "  bur_s WITH bur_q;\n\n",
  
  "  ! Class-invariant residual variances\n",
  "  ",
  paste(
    burden_variables,
    collapse = " "
  ),
  ";\n\n",
  
  "  ! Class-specific growth-factor means\n",
  class_specific_means_syntax, "\n\n",
  
  "OUTPUT:\n",
  "  SAMPSTAT;\n",
  "  STANDARDIZED;\n",
  "  CINTERVAL;\n",
  "  TECH1;\n",
  "  TECH4;\n",
  "  TECH7;\n",
  "  TECH8;\n",
  "  TECH11;\n",
  "  TECH14;\n"
)

##### SAVE M17 INPUT ####

input_file <- file.path(
  mplus_input_dir,
  "17_mt_burden_quadratic_2class.inp"
)

writeLines(
  input_syntax,
  con = input_file
)

##### COPY M17 INPUT TO GITHUB ####

copy_success <- file.copy(
  from = input_file,
  to = github_maltreatment_dir,
  overwrite = TRUE
)

stopifnot(
  file.exists(input_file),
  copy_success,
  file.exists(
    file.path(
      github_maltreatment_dir,
      basename(input_file)
    )
  )
)

input_file_m17 <- input_file

cat(
  "Created: ",
  input_file_m17,
  "\n",
  sep = ""
)

#-------------------------------------------------------------------------
##### CREATE M18 EXPORT INPUT WITH CLASS PROBABILITIES #####
#-------------------------------------------------------------------------

##### DEFINE M18 MODEL ELEMENTS ####

number_of_classes_m18 <- 3

burden_growth_factors_m18 <- c(
  "bur_i",
  "bur_s",
  "bur_q"
)

stopifnot(
  exists("burden_variables"),
  exists("developmental_time_scores"),
  exists("mplus_names"),
  exists("names_syntax"),
  length(burden_variables) == 7,
  length(developmental_time_scores) == 7,
  all(burden_variables %in% mplus_names),
  all(is.finite(developmental_time_scores)),
  all(nchar(burden_growth_factors_m18) <= 8),
  all(nchar(burden_variables) <= 8)
)

##### CREATE USEVARIABLES SYNTAX ####

burden_usevariables_syntax_m18 <- paste(
  wrap_mplus_names(
    burden_variables,
    max_width = 88
  ),
  collapse = "\n"
)

##### FORMAT DEVELOPMENTAL TIME SCORES ####

developmental_time_scores_m18 <- formatC(
  as.numeric(developmental_time_scores),
  format = "f",
  digits = 6,
  decimal.mark = "."
)

##### CREATE GROWTH-INDICATOR SYNTAX ####

burden_growth_indicators_syntax_m18 <- paste0(
  "    ",
  burden_variables,
  "@",
  developmental_time_scores_m18,
  collapse = "\n"
)

##### CREATE CLASS-SPECIFIC MEAN SYNTAX ####

class_specific_means_syntax_m18 <- paste(
  vapply(
    seq_len(
      number_of_classes_m18
    ),
    function(class_number) {
      
      paste0(
        "  %c#",
        class_number,
        "%\n",
        "  [",
        paste(
          burden_growth_factors_m18,
          collapse = " "
        ),
        "];"
      )
    },
    character(1)
  ),
  collapse = "\n\n"
)

##### CREATE COVARIANCE SYNTAX ####

growth_covariance_syntax_m18 <- paste0(
  "  ",
  burden_growth_factors_m18[1],
  " WITH ",
  paste(
    burden_growth_factors_m18[-1],
    collapse = " "
  ),
  ";\n",
  "  ",
  burden_growth_factors_m18[2],
  " WITH ",
  burden_growth_factors_m18[3],
  ";"
)

##### CREATE M18 INPUT SYNTAX ####

input_syntax <- paste0(
  "TITLE:\n",
  "  M18: Three-class quadratic growth mixture model for ",
  "maltreatment burden;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  "  NAMES ARE\n",
  names_syntax, ";\n\n",
  
  "  USEVARIABLES ARE\n",
  burden_usevariables_syntax_m18, ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n",
  "  CLASSES = c(", number_of_classes_m18, ");\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = MIXTURE;\n",
  "  ESTIMATOR = MLR;\n",
  "  STARTS = 4000 1000;\n",
  "  STITERATIONS = 20;\n",
  "  LRTSTARTS = 0 0 1000 250;\n\n",
  
  "MODEL:\n",
  "  %OVERALL%\n\n",
  
  "  ! Quadratic growth model\n",
  "  ",
  paste(
    burden_growth_factors_m18,
    collapse = " "
  ),
  " |\n",
  burden_growth_indicators_syntax_m18,
  ";\n\n",
  
  "  ! Class-invariant growth-factor variances\n",
  "  ",
  paste(
    burden_growth_factors_m18,
    collapse = " "
  ),
  ";\n\n",
  
  "  ! Class-invariant growth-factor covariances\n",
  growth_covariance_syntax_m18,
  "\n\n",
  
  "  ! Class-invariant residual variances\n",
  "  ",
  paste(
    burden_variables,
    collapse = " "
  ),
  ";\n\n",
  
  "  ! Class-specific growth-factor means\n",
  class_specific_means_syntax_m18,
  "\n\n",
  
  "OUTPUT:\n",
  "  TECH11;\n\n",
  "  TECH14;\n\n",
  
  "SAVEDATA:\n",
  "  FILE = 18_mt_burden_quadratic_3class_cprob.dat;\n",
  "  SAVE = CPROBABILITIES;\n"
)

##### SAVE M18 EXPORT INPUT ####

input_file <- file.path(
  mplus_input_dir,
  "18_mt_burden_quadratic_3class.inp"
)

writeLines(
  input_syntax,
  con = input_file
)

##### CHECK GENERATED M18 INPUT ####

generated_input_m18 <- readLines(
  input_file
)

stopifnot(
  any(
    grepl(
      "bur_i bur_s bur_q \\|",
      generated_input_m18
    )
  ),
  any(
    grepl(
      "bur_i WITH bur_s bur_q;",
      generated_input_m18,
      fixed = TRUE
    )
  ),
  any(
    grepl(
      "bur_s WITH bur_q;",
      generated_input_m18,
      fixed = TRUE
    )
  ),
  any(
    grepl(
      "SAVE = CPROBABILITIES;",
      generated_input_m18,
      fixed = TRUE
    )
  )
)

##### COPY M18 EXPORT INPUT TO GITHUB ####

copy_success <- file.copy(
  from = input_file,
  to = github_maltreatment_dir,
  overwrite = TRUE
)

stopifnot(
  file.exists(input_file),
  copy_success,
  file.exists(
    file.path(
      github_maltreatment_dir,
      basename(input_file)
    )
  )
)

input_file_m18 <- input_file

cat(
  "Created: ",
  input_file_m18,
  "\n",
  sep = ""
)

#-------------------------------------------------------------------------
##### CREATE M19: FOUR-CLASS QUADRATIC BURDEN MODEL #####
#-------------------------------------------------------------------------

##### DEFINE FOUR-CLASS MODEL SETTINGS ####

number_of_classes_m19 <- 4

stopifnot(
  exists("burden_variables"),
  exists("burden_growth_factors"),
  exists("burden_growth_syntax"),
  exists("burden_usevariables_syntax"),
  length(burden_variables) == 7,
  all(burden_variables %in% mplus_names),
  length(developmental_time_scores) == 7,
  all(is.finite(developmental_time_scores))
)

##### CREATE CLASS-SPECIFIC MEAN SYNTAX ####

class_specific_means_syntax_m19 <- paste(
  vapply(
    seq_len(
      number_of_classes_m19
    ),
    function(class_number) {
      
      paste0(
        "  %c#",
        class_number,
        "%\n",
        "  [",
        paste(
          burden_growth_factors,
          collapse = " "
        ),
        "];"
      )
    },
    character(1)
  ),
  collapse = "\n\n"
)

##### CREATE M19 INPUT SYNTAX ####

input_syntax <- paste0(
  "TITLE:\n",
  "  M19: Four-class quadratic growth mixture model for ",
  "maltreatment burden;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  "  NAMES ARE\n",
  names_syntax, ";\n\n",
  
  "  USEVARIABLES ARE\n",
  burden_usevariables_syntax, ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n",
  "  CLASSES = c(", number_of_classes_m19, ");\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = MIXTURE;\n",
  "  ESTIMATOR = MLR;\n",
  "  STARTS = 2000 500;\n",
  "  STITERATIONS = 20;\n\n",
  
  "MODEL:\n",
  "  %OVERALL%\n\n",
  
  "  ! Quadratic growth model\n",
  burden_growth_syntax, "\n\n",
  
  "  ! Class-invariant growth-factor variances\n",
  "  ",
  paste(
    burden_growth_factors,
    collapse = " "
  ),
  ";\n\n",
  
  "  ! Class-invariant growth-factor covariances\n",
  "  bur_i WITH bur_s bur_q;\n",
  "  bur_s WITH bur_q;\n\n",
  
  "  ! Class-invariant residual variances\n",
  "  ",
  paste(
    burden_variables,
    collapse = " "
  ),
  ";\n\n",
  
  "  ! Class-specific growth-factor means\n",
  class_specific_means_syntax_m19, "\n\n",
  
  "OUTPUT:\n",
  "  TECH11;\n"
)

##### SAVE M19 INPUT ####

input_file <- file.path(
  mplus_input_dir,
  "19_mt_burden_quadratic_4class.inp"
)

writeLines(
  input_syntax,
  con = input_file
)

##### COPY M19 INPUT TO GITHUB ####

copy_success <- file.copy(
  from = input_file,
  to = github_maltreatment_dir,
  overwrite = TRUE
)

stopifnot(
  file.exists(input_file),
  copy_success,
  file.exists(
    file.path(
      github_maltreatment_dir,
      basename(input_file)
    )
  )
)

input_file_m19 <- input_file

cat(
  "Created: ",
  input_file_m19,
  "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### CREATE M18A: THREE-CLASS MODEL IN T2-T5 PARTICIPANTS #####
#-------------------------------------------------------------------------

##### CHECK REQUIRED VARIABLES ####

stopifnot(
  exists("mplus_names"),
  exists("names_syntax"),
  exists("burden_variables"),
  exists("developmental_time_scores"),
  "stat_t5" %in% mplus_names,
  all(burden_variables %in% mplus_names),
  length(burden_variables) == 7,
  length(developmental_time_scores) == 7
)

##### CREATE USEVARIABLES SYNTAX ####

burden_usevariables_syntax <- paste(
  wrap_mplus_names(
    burden_variables,
    max_width = 88
  ),
  collapse = "\n"
)

##### CREATE GROWTH-INDICATOR SYNTAX ####

burden_time_scores <- formatC(
  as.numeric(developmental_time_scores),
  format = "f",
  digits = 6
)

burden_growth_syntax <- paste0(
  "      ",
  burden_variables,
  "@",
  burden_time_scores,
  collapse = "\n"
)

##### CREATE MPLUS INPUT ####

input_syntax <- paste0(
  "TITLE:\n",
  "  M18a: Three-class quadratic growth mixture model for\n",
  "  maltreatment burden among T2-T5 participants;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  "  NAMES ARE\n",
  names_syntax, ";\n\n",
  
  "  USEVARIABLES ARE\n",
  burden_usevariables_syntax, ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n",
  "  USEOBSERVATIONS ARE stat_t5 EQ 2;\n",
  "  CLASSES = c(3);\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = MIXTURE;\n",
  "  ESTIMATOR = MLR;\n",
  "  STARTS = 4000 1000;\n",
  "  STITERATIONS = 20;\n",
  "  LRTSTARTS = 0 0 1000 250;\n\n",
  
  "MODEL:\n",
  "  %OVERALL%\n\n",
  
  "  ! Quadratic growth model\n",
  "  bur_i bur_s bur_q |\n",
  burden_growth_syntax, ";\n\n",
  
  "  ! Class-invariant growth-factor variances\n",
  "  bur_i bur_s bur_q;\n\n",
  
  "  ! Class-invariant growth-factor covariances\n",
  "  bur_i WITH bur_s bur_q;\n",
  "  bur_s WITH bur_q;\n\n",
  
  "  ! Class-invariant residual variances\n",
  "  zind_sa zind_kk zind_vsa zind_fsz\n",
  "  zind_ssz zind_ja zind_jea;\n\n",
  
  "  ! Class-specific growth-factor means\n",
  "  %c#1%\n",
  "  [bur_i bur_s bur_q];\n\n",
  
  "  %c#2%\n",
  "  [bur_i bur_s bur_q];\n\n",
  
  "  %c#3%\n",
  "  [bur_i bur_s bur_q];\n\n",
  
  "OUTPUT:\n",
  "  TECH11;\n",
  "  TECH14;\n"
)

##### SAVE MPLUS INPUT ####

input_file <- file.path(
  mplus_input_dir,
  "18a_mt_burden_quadratic_3class_t2_t5.inp"
)

writeLines(
  input_syntax,
  con = input_file
)

##### COPY INPUT TO GITHUB ####

copy_success <- file.copy(
  from = input_file,
  to = github_maltreatment_dir,
  overwrite = TRUE
)

stopifnot(
  file.exists(input_file),
  copy_success
)

cat(
  "Created: ",
  input_file,
  "\n",
  sep = ""
)
#-------------------------------------------------------------------------
##### EXTRACT M18 CLASS ASSIGNMENTS #####
#-------------------------------------------------------------------------

##### READ M18 EXPORT OUTPUT ####

m18_output <- file.path(
  mplus_input_dir,
  "18_mt_burden_quadratic_3class.out"
)

stopifnot(
  file.exists(m18_output)
)

m18_results <- MplusAutomation::readModels(
  m18_output,
  what = "all"
)

stopifnot(
  !is.null(m18_results$savedata)
)

m18_savedata <- as_tibble(
  m18_results$savedata
)

##### STANDARDIZE SAVED VARIABLE NAMES ####

names(m18_savedata) <- tolower(
  names(m18_savedata)
)

stopifnot(
  all(
    c(
      "sic_n",
      "c",
      "cprob1",
      "cprob2",
      "cprob3"
    ) %in% names(m18_savedata)
  )
)

##### CREATE CLASS ASSIGNMENT DATASET ####

m18_class_assignments <- m18_savedata |>
  transmute(
    SIC_N = as.integer(sic_n),
    mt_class = as.integer(c),
    mt_prob_class1 = cprob1,
    mt_prob_class2 = cprob2,
    mt_prob_class3 = cprob3,
    mt_prob_max = pmax(
      cprob1,
      cprob2,
      cprob3
    ),
    mt_class_label = case_when(
      mt_class == 1 ~ "Low and stable",
      mt_class == 2 ~ "Elevated and declining",
      mt_class == 3 ~ "High early burden with later rebound",
      TRUE ~ NA_character_
    )
  )

##### CHECK CLASS ASSIGNMENTS ####

stopifnot(
  anyDuplicated(m18_class_assignments$SIC_N) == 0,
  all(
    m18_class_assignments$mt_class %in% 1:3
  ),
  all(
    m18_class_assignments$mt_prob_max >= 0 &
      m18_class_assignments$mt_prob_max <= 1
  )
)

m18_class_assignments |>
  count(
    mt_class,
    mt_class_label
  ) |>
  mutate(
    proportion = n / sum(n)
  ) |>
  print(
    n = Inf
  )

summary(
  m18_class_assignments$mt_prob_max
)

##### LOAD MPLUS DATASET INTO R #####

dat_mplus <- read_delim(
  file = mplus_data_file,
  delim = "\t",
  col_names = mplus_names,
  na = "-999",
  trim_ws = TRUE,
  col_types = cols(
    .default = col_double()
  ),
  progress = FALSE
)

##### CHECK LOADED MPLUS DATASET ####

stopifnot(
  ncol(dat_mplus) == length(mplus_names),
  identical(names(dat_mplus), mplus_names),
  all(vapply(dat_mplus, is.numeric, logical(1))),
  "SIC_N" %in% names(dat_mplus),
  !anyNA(dat_mplus$SIC_N),
  anyDuplicated(dat_mplus$SIC_N) == 0
)

cat(
  "Loaded Mplus dataset: ",
  nrow(dat_mplus),
  " rows and ",
  ncol(dat_mplus),
  " columns.\n",
  sep = ""
)

#-------------------------------------------------------------------------
##### PREPARE M18 CLASS VARIABLES FOR MPLUS #####
#-------------------------------------------------------------------------

m18_classes_mplus <- m18_class_assignments |>
  transmute(
    SIC_N = SIC_N,
    m18_cls = as.numeric(mt_class),
    m18_p1 = mt_prob_class1,
    m18_p2 = mt_prob_class2,
    m18_p3 = mt_prob_class3,
    m18_pmx = mt_prob_max
  )

##### CHECK M18 CLASS VARIABLES ####

stopifnot(
  anyDuplicated(m18_classes_mplus$SIC_N) == 0,
  all(m18_classes_mplus$SIC_N %in% dat_mplus$SIC_N),
  all(m18_classes_mplus$m18_cls %in% 1:3),
  all(
    abs(
      m18_classes_mplus$m18_p1 +
        m18_classes_mplus$m18_p2 +
        m18_classes_mplus$m18_p3 -
        1
    ) < 0.01
  ),
  all(nchar(names(m18_classes_mplus)) <= 8)
)

dat_mplus_m18 <- dat_mplus |>
  left_join(
    m18_classes_mplus,
    by = "SIC_N"
  )
names(dat_mplus_m18)[520:527]

#-------------------------------------------------------------------------
##### SAVE MPLUS DATASET WITH M18 CLASSES #####
#-------------------------------------------------------------------------

##### REPLACE MISSING VALUES FOR MPLUS EXPORT ####

dat_mplus_m18_export <- dat_mplus_m18 |>
  mutate(
    across(
      everything(),
      ~ replace(
        .x,
        is.na(.x),
        -999
      )
    )
  )

stopifnot(
  all(vapply(dat_mplus_m18_export, is.numeric, logical(1))),
  sum(is.na(dat_mplus_m18_export)) == 0,
  all(nchar(names(dat_mplus_m18_export)) <= 8),
  anyDuplicated(names(dat_mplus_m18_export)) == 0
)

##### DEFINE OUTPUT FILES ####

mplus_data_name_m18 <- "AMIS_mplus_dataset_m18.dat"

mplus_data_file_m18 <- file.path(
  mplus_input_dir,
  mplus_data_name_m18
)

mplus_names_file_m18 <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18.rds"
)

mplus_names_text_file_m18 <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18.txt"
)

mplus_rds_file_m18 <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset_m18.rds"
)

##### SAVE MPLUS DATA FILE ####

write.table(
  dat_mplus_m18_export,
  file = mplus_data_file_m18,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  dec = "."
)

##### SAVE MPLUS VARIABLE NAMES ####

mplus_names_m18 <- names(
  dat_mplus_m18
)

saveRDS(
  mplus_names_m18,
  mplus_names_file_m18
)

writeLines(
  mplus_names_m18,
  mplus_names_text_file_m18
)

##### SAVE R VERSION WITH ORIGINAL MISSING VALUES ####

saveRDS(
  dat_mplus_m18,
  mplus_rds_file_m18
)

##### CHECK SAVED FILES ####

stopifnot(
  file.exists(mplus_data_file_m18),
  file.exists(mplus_names_file_m18),
  file.exists(mplus_names_text_file_m18),
  file.exists(mplus_rds_file_m18)
)

##### CHECK SAVED COLUMN ORDER ####

saved_names_m18 <- readRDS(
  mplus_names_file_m18
)

stopifnot(
  identical(
    saved_names_m18,
    names(dat_mplus_m18)
  ),
  length(saved_names_m18) ==
    ncol(dat_mplus_m18)
)

##### CHECK NUMBER OF EXPORTED COLUMNS ####

number_of_exported_columns_m18 <- length(
  strsplit(
    readLines(
      mplus_data_file_m18,
      n = 1
    ),
    split = "\t",
    fixed = TRUE
  )[[1]]
)

stopifnot(
  number_of_exported_columns_m18 ==
    length(mplus_names_m18)
)

cat(
  "Saved Mplus dataset: ",
  mplus_data_file_m18,
  "\n",
  "Saved names file: ",
  mplus_names_file_m18,
  "\n",
  "Rows: ",
  nrow(dat_mplus_m18),
  "\n",
  "Columns: ",
  ncol(dat_mplus_m18),
  "\n",
  sep = ""
)

##### COPY M18 MPLUS DATASET TO SEADRIVE ####

copy_success <- file.copy(
  from = mplus_data_file_m18,
  to = seadrive_mplus_data_dir,
  overwrite = TRUE
)

stopifnot(copy_success)



##### MERGE M18 CLASSES WITH ORIGINAL EXCEL DATASET ####
dat_original <- read_excel("C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/MAIN OUTCOME/02_data/02_data_Prep/AMIS_merged_analysis_dataset.xlsx")
##### RECREATE ID LOOKUP ####

id_lookup <- dat_original |>
  distinct(sic) |>
  arrange(sic) |>
  mutate(
    SIC_N = row_number()
  )

stopifnot(
  anyDuplicated(id_lookup$sic) == 0,
  anyDuplicated(id_lookup$SIC_N) == 0
)

##### ADD ORIGINAL ID TO CLASS ASSIGNMENTS ####

m18_classes_original_id <- m18_class_assignments |>
  left_join(
    id_lookup,
    by = "SIC_N"
  )

stopifnot(
  !anyNA(m18_classes_original_id$sic)
)

##### MERGE WITH ORIGINAL DATASET ####

dat_original_m18 <- dat_original |>
  left_join(
    m18_classes_original_id |>
      select(
        -SIC_N
      ),
    by = "sic"
  )

stopifnot(
  nrow(dat_original_m18) == nrow(dat_original),
  anyDuplicated(dat_original_m18$sic) == 0
)

##### SAVE NEW EXCEL DATASET ####

excel_output_dir <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/02_data/02_data_Prep"
)

dir.create(
  excel_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

excel_file_m18 <- file.path(
  excel_output_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes.xlsx"
)

writexl::write_xlsx(
  x = dat_original_m18,
  path = excel_file_m18
)

stopifnot(
  file.exists(excel_file_m18)
)

cat(
  "Saved: ",
  normalizePath(
    excel_file_m18,
    winslash = "/"
  ),
  "\n",
  sep = ""
)
  
#-------------------------------------------------------------------------
##### PLOT M18 THREE-CLASS BURDEN TRAJECTORIES #####
#-------------------------------------------------------------------------
##### LOAD GGPLOT2 ####

library(ggplot2)

##### DEFINE M18 CLASS-SPECIFIC GROWTH MEANS ####

m18_growth_means <- tibble(
  class = c(
    "Low/stable burden (83.1%)",
    "Elevated/declining burden (12.0%)",
    "Very high/rebound burden (4.8%)"
  ),
  bur_i = c(
    -0.422,
    2.681,
    2.580
  ),
  bur_s = c(
    0.114,
    -0.403,
    -0.689
  ),
  bur_q = c(
    -0.042,
    0.104,
    0.433
  )
)

##### DEFINE DEVELOPMENTAL PERIOD TABLE ####

developmental_period_table <- tibble(
  period = toupper(
    names(
      developmental_midpoints
    )
  ),
  midpoint_age = as.numeric(
    developmental_midpoints
  ),
  time_score = as.numeric(
    developmental_time_scores
  )
)

##### CREATE TRAJECTORY PLOT DATA ####

m18_plot_data <- merge(
  m18_growth_means,
  developmental_period_table,
  by = NULL
) |>
  as_tibble() |>
  mutate(
    estimated_burden = bur_i +
      bur_s * time_score +
      bur_q * time_score^2,
    class = factor(
      class,
      levels = m18_growth_means$class
    )
  ) |>
  arrange(
    class,
    midpoint_age
  )

##### INSPECT ESTIMATED TRAJECTORY VALUES ####

m18_plot_data |>
  select(
    class,
    period,
    midpoint_age,
    estimated_burden
  ) |>
  print(
    n = Inf
  )

##### CREATE TRAJECTORY PLOT ####

m18_trajectory_plot <- ggplot(
  m18_plot_data,
  aes(
    x = midpoint_age,
    y = estimated_burden,
    color = class,
    group = class
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2.8
  ) +
  scale_x_continuous(
    breaks = developmental_period_table$midpoint_age,
    labels = developmental_period_table$period
  ) +
  scale_color_manual(
    values = c(
      "#3366A3",
      "#D68C2F",
      "#A33A3A"
    )
  ) +
  labs(
    x = "Developmental period",
    y = "Estimated maltreatment burden",
    color = NULL
  ) +
  theme_classic(
    base_size = 12
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(
      size = 10
    ),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    )
  )

##### DISPLAY TRAJECTORY PLOT ####

m18_trajectory_plot

##### SAVE TRAJECTORY PLOT ####

m18_plot_file <- file.path(
  mplus_results_dir_mal,
  "18_mt_burden_quadratic_3class_trajectories.png"
)

ggsave(
  filename = m18_plot_file,
  plot = m18_trajectory_plot,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)

cat(
  "Saved: ",
  m18_plot_file,
  "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### TABLE S9: PRELIMINARY GROWTH MODELS #####
#-------------------------------------------------------------------------

##### DEFINE MODEL METADATA ####

table_s9_models <- tibble::tribble(
  ~model, ~file_name, ~construct, ~form, ~time_metric,
  
  "M9",
  "09_mt_subtypes_linear_1class.out",
  "Subtype count",
  "Linear",
  "Equally spaced",
  
  "M10",
  "10_mt_frequency_linear_1class.out",
  "Frequency",
  "Linear",
  "Equally spaced",
  
  "M11",
  "11_mt_severity_linear_1class.out",
  "Severity",
  "Linear",
  "Equally spaced",
  
  "M12",
  "12_mt_subtypes_quadratic_1class.out",
  "Subtype count",
  "Quadratic",
  "Age-based",
  
  "M13",
  "13_mt_frequency_quadratic_1class.out",
  "Frequency",
  "Quadratic",
  "Age-based",
  
  "M14",
  "14_mt_severity_quadratic_1class.out",
  "Severity",
  "Quadratic",
  "Age-based",
  
  "M15",
  "15_mt_parallel_quadratic_1class.out",
  "Parallel process",
  "Quadratic",
  "Age-based",
  
  "M16",
  "16_mt_burden_quadratic_1class.out",
  "Burden index",
  "Quadratic",
  "Age-based"
)

##### DEFINE POSSIBLE MPLUS OUTPUT DIRECTORIES ####

table_s9_output_dirs <- c(
  mplus_results_dir_mal,
  mplus_input_dir
)

stopifnot(
  all(dir.exists(table_s9_output_dirs))
)

##### DEFINE FUNCTION TO LOCATE MPLUS OUTPUT ####

locate_mplus_output <- function(
    file_name
) {
  
  candidate_files <- file.path(
    table_s9_output_dirs,
    file_name
  )
  
  existing_files <- candidate_files[
    file.exists(candidate_files)
  ]
  
  if (length(existing_files) == 0) {
    
    stop(
      paste0(
        "Mplus output not found: ",
        file_name,
        "\nSearched in:\n",
        paste(
          table_s9_output_dirs,
          collapse = "\n"
        )
      )
    )
  }
  
  normalizePath(
    existing_files[1],
    winslash = "/",
    mustWork = TRUE
  )
}

##### LOCATE ALL TABLE S9 OUTPUTS ####

table_s9_models <- table_s9_models |>
  mutate(
    output_file = purrr::map_chr(
      file_name,
      locate_mplus_output
    )
  )

##### CHECK LOCATED OUTPUTS ####

stopifnot(
  nrow(table_s9_models) == 8,
  all(file.exists(table_s9_models$output_file)),
  anyDuplicated(table_s9_models$model) == 0,
  anyDuplicated(table_s9_models$file_name) == 0
)

##### DISPLAY LOCATED OUTPUTS ####

table_s9_models |>
  select(
    model,
    file_name,
    output_file
  ) |>
  print(
    n = Inf
  )

##### DEFINE SUMMARY VALUE EXTRACTOR ####

get_summary_value <- function(
    summary_data,
    possible_names,
    value_label
) {
  
  matching_name <- possible_names[
    possible_names %in% names(summary_data)
  ]
  
  if (length(matching_name) == 0) {
    
    stop(
      paste0(
        "Could not find ",
        value_label,
        " in the Mplus summary. Available fields: ",
        paste(
          names(summary_data),
          collapse = ", "
        )
      )
    )
  }
  
  as.numeric(
    summary_data[[matching_name[1]]][1]
  )
}

##### DEFINE MODEL FIT EXTRACTION FUNCTION ####

extract_growth_model_fit <- function(
    output_file
) {
  
  model_results <- MplusAutomation::readModels(
    output_file,
    what = "all",
    quiet = TRUE
  )
  
  summary_data <- as.data.frame(
    model_results$summaries
  )
  
  tibble(
    n = get_summary_value(
      summary_data,
      c("Observations"),
      "number of observations"
    ),
    parameters = get_summary_value(
      summary_data,
      c("Parameters"),
      "number of parameters"
    ),
    chi_square = get_summary_value(
      summary_data,
      c("ChiSqM_Value"),
      "model chi-square"
    ),
    df = get_summary_value(
      summary_data,
      c("ChiSqM_DF"),
      "model degrees of freedom"
    ),
    cfi = get_summary_value(
      summary_data,
      c("CFI"),
      "CFI"
    ),
    tli = get_summary_value(
      summary_data,
      c("TLI"),
      "TLI"
    ),
    rmsea = get_summary_value(
      summary_data,
      c("RMSEA_Estimate"),
      "RMSEA"
    ),
    rmsea_low = get_summary_value(
      summary_data,
      c(
        "RMSEA_90CI_LB",
        "RMSEA_90CI_Lower"
      ),
      "RMSEA lower confidence limit"
    ),
    rmsea_high = get_summary_value(
      summary_data,
      c(
        "RMSEA_90CI_UB",
        "RMSEA_90CI_Upper"
      ),
      "RMSEA upper confidence limit"
    ),
    srmr = get_summary_value(
      summary_data,
      c("SRMR"),
      "SRMR"
    )
  )
}

##### EXTRACT MODEL FIT ####

table_s9_fit <- purrr::map_dfr(
  table_s9_models$output_file,
  extract_growth_model_fit
)

table_s9_raw <- bind_cols(
  table_s9_models |>
    select(
      model,
      construct,
      form,
      time_metric
    ),
  table_s9_fit
)

##### CHECK EXTRACTED RESULTS ####

stopifnot(
  nrow(table_s9_raw) == nrow(table_s9_models),
  !anyNA(table_s9_raw),
  all(table_s9_raw$n > 0),
  all(table_s9_raw$df > 0),
  all(table_s9_raw$cfi >= 0 & table_s9_raw$cfi <= 1),
  all(table_s9_raw$tli >= 0 & table_s9_raw$tli <= 1),
  all(table_s9_raw$rmsea >= 0),
  all(table_s9_raw$srmr >= 0)
)

print(
  table_s9_raw,
  n = Inf
)

##### DEFINE APA NUMBER FORMATTING ####

format_apa_decimal <- function(
    x,
    digits = 3
) {
  
  formatted_value <- formatC(
    x,
    format = "f",
    digits = digits,
    decimal.mark = "."
  )
  
  sub(
    "^0",
    "",
    formatted_value
  )
}

##### CREATE APA TABLE ####

table_s9_apa <- table_s9_raw |>
  transmute(
    Model = model,
    Construct = construct,
    Form = form,
    `Time metric` = time_metric,
    `N` = as.integer(n),
    `χ² (df)` = paste0(
      formatC(
        chi_square,
        format = "f",
        digits = 2,
        decimal.mark = "."
      ),
      " (",
      as.integer(df),
      ")"
    ),
    CFI = format_apa_decimal(cfi),
    TLI = format_apa_decimal(tli),
    `RMSEA [90% CI]` = paste0(
      format_apa_decimal(rmsea),
      " [",
      format_apa_decimal(rmsea_low),
      ", ",
      format_apa_decimal(rmsea_high),
      "]"
    ),
    SRMR = format_apa_decimal(srmr)
  )

print(
  table_s9_apa,
  n = Inf
)

##### SAVE TABLE S9 AS CSV ####

table_s9_raw_file <- file.path(
  mplus_results_dir_mal,
  "Table_S9_preliminary_growth_models_raw.csv"
)

table_s9_apa_file <- file.path(
  mplus_results_dir_mal,
  "Table_S9_preliminary_growth_models_APA.csv"
)

readr::write_csv(
  table_s9_raw,
  table_s9_raw_file
)

readr::write_csv(
  table_s9_apa,
  table_s9_apa_file
)

##### CREATE FLEXTABLE ####

table_s9_ft <- flextable::flextable(
  table_s9_apa
) |>
  flextable::theme_booktabs() |>
  flextable::font(
    fontname = "Times New Roman",
    part = "all"
  ) |>
  flextable::fontsize(
    size = 9,
    part = "all"
  ) |>
  flextable::bold(
    part = "header"
  ) |>
  flextable::align(
    j = c(
      "Model",
      "Construct",
      "Form",
      "Time metric"
    ),
    align = "left",
    part = "all"
  ) |>
  flextable::align(
    j = c(
      "N",
      "χ² (df)",
      "CFI",
      "TLI",
      "RMSEA [90% CI]",
      "SRMR"
    ),
    align = "center",
    part = "all"
  ) |>
  flextable::autofit() |>
  flextable::set_table_properties(
    layout = "autofit",
    width = 1
  )

##### CREATE WORD DOCUMENT ####

table_s9_word_file <- file.path(
  mplus_results_dir_mal,
  "Table_S9_preliminary_growth_models.docx"
)

table_s9_note <- paste0(
  "Note. M9–M11 used equally spaced time scores (0–6). ",
  "M12–M16 used time scores based on representative ages ",
  "of the developmental periods. M15 simultaneously modeled ",
  "subtype count, frequency, and severity. CI = confidence ",
  "interval; CFI = comparative fit index; TLI = Tucker–Lewis ",
  "index; RMSEA = root mean square error of approximation; ",
  "SRMR = standardized root mean square residual."
)

table_s9_doc <- officer::read_docx() |>
  officer::body_add_fpar(
    officer::fpar(
      officer::ftext(
        "Table S9",
        officer::fp_text(
          font.family = "Times New Roman",
          font.size = 10,
          bold = TRUE
        )
      )
    )
  ) |>
  officer::body_add_fpar(
    officer::fpar(
      officer::ftext(
        paste0(
          "Fit of Preliminary Growth Models for ",
          "Developmental Maltreatment Indicators"
        ),
        officer::fp_text(
          font.family = "Times New Roman",
          font.size = 10,
          italic = TRUE
        )
      )
    )
  )

table_s9_doc <- flextable::body_add_flextable(
  x = table_s9_doc,
  value = table_s9_ft
)

table_s9_doc <- officer::body_add_fpar(
  table_s9_doc,
  officer::fpar(
    officer::ftext(
      table_s9_note,
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 9
      )
    )
  )
)

print(
  table_s9_doc,
  target = table_s9_word_file
)

##### CHECK SAVED TABLE FILES ####

stopifnot(
  file.exists(table_s9_raw_file),
  file.exists(table_s9_apa_file),
  file.exists(table_s9_word_file)
)

cat(
  "Saved Table S9:\n",
  table_s9_word_file,
  "\n",
  sep = ""
)



#-------------------------------------------------------------------------
##### TABLE S10: TRAJECTORY CLASS ENUMERATION #####
#-------------------------------------------------------------------------

stopifnot(
  exists("locate_mplus_output"),
  exists("get_summary_value")
)

##### DEFINE MODEL METADATA ####

table_s10_models <- tibble::tribble(
  ~model, ~file_name, ~classes, ~selected,
  
  "M16",
  "16_mt_burden_quadratic_1class.out",
  1,
  FALSE,
  
  "M17",
  "17_mt_burden_quadratic_2class.out",
  2,
  FALSE,
  
  "M18",
  "18_mt_burden_quadratic_3class.out",
  3,
  TRUE,
  
  "M19",
  "19_mt_burden_quadratic_4class.out",
  4,
  FALSE
) |>
  mutate(
    output_file = purrr::map_chr(
      file_name,
      locate_mplus_output
    )
  )

stopifnot(
  nrow(table_s10_models) == 4,
  all(file.exists(table_s10_models$output_file)),
  sum(table_s10_models$selected) == 1
)

##### DEFINE OPTIONAL SUMMARY VALUE EXTRACTOR ####

get_optional_summary_value <- function(
    summary_data,
    possible_names
) {
  
  matching_name <- possible_names[
    possible_names %in% names(summary_data)
  ]
  
  if (length(matching_name) == 0) {
    return(NA_real_)
  }
  
  value <- suppressWarnings(
    as.numeric(
      summary_data[[matching_name[1]]][1]
    )
  )
  
  if (length(value) == 0 || !is.finite(value)) {
    return(NA_real_)
  }
  
  value
}

##### DEFINE LIKELIHOOD-RATIO TEST EXTRACTOR ####

extract_test_p_value <- function(
    output_lines,
    section_heading,
    p_value_pattern = "^\\s*P-Value\\s+([0-9.]+)",
    search_window = 15
) {
  
  section_position <- which(
    grepl(
      section_heading,
      output_lines,
      fixed = TRUE
    )
  )
  
  if (length(section_position) == 0) {
    return(NA_real_)
  }
  
  section_start <- section_position[1]
  
  section_end <- min(
    length(output_lines),
    section_start + search_window
  )
  
  section_lines <- output_lines[
    section_start:section_end
  ]
  
  matches <- stringr::str_match(
    section_lines,
    p_value_pattern
  )
  
  extracted_values <- matches[, 2]
  extracted_values <- extracted_values[
    !is.na(extracted_values)
  ]
  
  if (length(extracted_values) == 0) {
    return(NA_real_)
  }
  
  as.numeric(
    extracted_values[1]
  )
}

##### DEFINE BOOTSTRAP-DRAW EXTRACTOR ####

extract_bootstrap_draws <- function(
    output_lines
) {
  
  matches <- stringr::str_match(
    output_lines,
    "Successful Bootstrap Draws\\s+([0-9]+)"
  )
  
  extracted_values <- matches[, 2]
  extracted_values <- extracted_values[
    !is.na(extracted_values)
  ]
  
  if (length(extracted_values) == 0) {
    return(NA_integer_)
  }
  
  as.integer(
    extracted_values[1]
  )
}

##### DEFINE CONDITION-NUMBER EXTRACTOR ####

extract_condition_number <- function(
    output_lines
) {
  
  condition_line <- output_lines[
    grepl(
      "Condition Number for the Information Matrix",
      output_lines,
      fixed = TRUE
    )
  ]
  
  if (length(condition_line) == 0) {
    return(NA_real_)
  }
  
  extracted_value <- stringr::str_match(
    condition_line[1],
    "([0-9.]+E[-+][0-9]+)"
  )[, 2]
  
  if (is.na(extracted_value)) {
    return(NA_real_)
  }
  
  as.numeric(
    extracted_value
  )
}

##### DEFINE CLASS-COUNT EXTRACTOR ####

extract_most_likely_class_counts <- function(
    output_lines,
    number_of_classes,
    number_of_observations
) {
  
  if (number_of_classes == 1) {
    
    return(
      tibble::tibble(
        latent_class = 1L,
        count = as.integer(number_of_observations),
        proportion = 1
      )
    )
  }
  
  section_start <- which(
    grepl(
      paste0(
        "BASED ON THEIR MOST LIKELY ",
        "LATENT CLASS MEMBERSHIP"
      ),
      output_lines,
      fixed = TRUE
    )
  )
  
  if (length(section_start) == 0) {
    stop("Most-likely class counts were not found.")
  }
  
  section_start <- section_start[1]
  
  section_end <- which(
    seq_along(output_lines) > section_start &
      grepl(
        "CLASSIFICATION QUALITY",
        output_lines,
        fixed = TRUE
      )
  )
  
  if (length(section_end) == 0) {
    stop("End of the class-count section was not found.")
  }
  
  section_end <- section_end[1]
  
  section_lines <- output_lines[
    section_start:section_end
  ]
  
  matches <- stringr::str_match(
    section_lines,
    "^\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9.]+)\\s*$"
  )
  
  class_counts <- tibble::tibble(
    latent_class = suppressWarnings(
      as.integer(matches[, 2])
    ),
    count = suppressWarnings(
      as.integer(matches[, 3])
    ),
    proportion = suppressWarnings(
      as.numeric(matches[, 4])
    )
  ) |>
    filter(
      !is.na(latent_class),
      !is.na(count),
      !is.na(proportion)
    ) |>
    slice_head(
      n = number_of_classes
    )
  
  if (nrow(class_counts) != number_of_classes) {
    
    stop(
      paste0(
        "Expected ",
        number_of_classes,
        " class-count rows, but found ",
        nrow(class_counts),
        "."
      )
    )
  }
  
  class_counts
}

##### DEFINE CLASS-MODEL EXTRACTION FUNCTION ####

extract_class_model_results <- function(
    output_file,
    number_of_classes
) {
  
  model_results <- MplusAutomation::readModels(
    output_file,
    what = "all",
    quiet = TRUE
  )
  
  summary_data <- as.data.frame(
    model_results$summaries
  )
  
  output_lines <- readLines(
    output_file,
    warn = FALSE
  )
  
  number_of_observations <- get_summary_value(
    summary_data,
    c("Observations"),
    "number of observations"
  )
  
  class_counts <- extract_most_likely_class_counts(
    output_lines = output_lines,
    number_of_classes = number_of_classes,
    number_of_observations = number_of_observations
  )
  
  smallest_class <- class_counts |>
    slice_min(
      order_by = count,
      n = 1,
      with_ties = FALSE
    )
  
  tibble::tibble(
    n = number_of_observations,
    parameters = get_summary_value(
      summary_data,
      c("Parameters"),
      "number of parameters"
    ),
    loglikelihood = get_summary_value(
      summary_data,
      c("LL", "H0_Value", "Loglikelihood"),
      "loglikelihood"
    ),
    aic = get_summary_value(
      summary_data,
      c("AIC"),
      "AIC"
    ),
    bic = get_summary_value(
      summary_data,
      c("BIC"),
      "BIC"
    ),
    abic = get_summary_value(
      summary_data,
      c("aBIC", "ABIC"),
      "sample-size adjusted BIC"
    ),
    entropy = get_optional_summary_value(
      summary_data,
      c("Entropy")
    ),
    vlmr_p = extract_test_p_value(
      output_lines,
      paste0(
        "VUONG-LO-MENDELL-RUBIN ",
        "LIKELIHOOD RATIO TEST"
      )
    ),
    lmr_p = extract_test_p_value(
      output_lines,
      "LO-MENDELL-RUBIN ADJUSTED LRT TEST"
    ),
    blrt_p = extract_test_p_value(
      output_lines,
      paste0(
        "PARAMETRIC BOOTSTRAPPED ",
        "LIKELIHOOD RATIO TEST"
      ),
      p_value_pattern = paste0(
        "^\\s*Approximate P-Value\\s+",
        "([0-9.]+)"
      )
    ),
    bootstrap_draws = extract_bootstrap_draws(
      output_lines
    ),
    smallest_class_n = smallest_class$count,
    smallest_class_proportion = smallest_class$proportion,
    condition_number = extract_condition_number(
      output_lines
    ),
    best_ll_replicated = any(
      grepl(
        paste0(
          "THE BEST LOGLIKELIHOOD VALUE ",
          "HAS BEEN REPLICATED"
        ),
        output_lines,
        fixed = TRUE
      )
    )
  )
}

##### EXTRACT CLASS-ENUMERATION RESULTS ####

table_s10_fit <- purrr::map2_dfr(
  table_s10_models$output_file,
  table_s10_models$classes,
  extract_class_model_results
)

table_s10_raw <- bind_cols(
  table_s10_models |>
    select(
      model,
      classes,
      selected
    ),
  table_s10_fit
)

##### RECHECK REPLICATION OF BEST LOGLIKELIHOOD ####

best_ll_replicated_check <- purrr::map_lgl(
  table_s10_models$output_file,
  function(output_file) {
    
    output_text <- paste(
      readLines(
        output_file,
        warn = FALSE
      ),
      collapse = " "
    )
    
    grepl(
      paste0(
        "BEST\\s+LOGLIKELIHOOD\\s+VALUE\\s+",
        "HAS\\s+BEEN\\s+REPLICATED"
      ),
      output_text,
      ignore.case = TRUE,
      perl = TRUE
    )
  }
)

table_s10_raw$best_ll_replicated <-
  best_ll_replicated_check

##### DISPLAY REPLICATION CHECK ####

table_s10_raw |>
  select(
    model,
    classes,
    best_ll_replicated
  ) |>
  print(
    n = Inf
  )

stopifnot(
  all(
    table_s10_raw$best_ll_replicated[
      table_s10_raw$classes > 1
    ]
  )
)

##### CHECK EXTRACTED RESULTS ####

stopifnot(
  nrow(table_s10_raw) == 4,
  all(table_s10_raw$n == 849),
  all(table_s10_raw$parameters > 0),
  all(is.finite(table_s10_raw$loglikelihood)),
  all(is.finite(table_s10_raw$aic)),
  all(is.finite(table_s10_raw$bic)),
  all(is.finite(table_s10_raw$abic)),
  all(
    table_s10_raw$smallest_class_n > 0
  ),
  all(
    table_s10_raw$smallest_class_proportion > 0 &
      table_s10_raw$smallest_class_proportion <= 1
  ),
  all(
    table_s10_raw$best_ll_replicated[
      table_s10_raw$classes > 1
    ]
  )
)

print(
  table_s10_raw,
  n = Inf
)

##### DEFINE APA P-VALUE FORMATTING ####

format_apa_p <- function(
    x
) {
  
  ifelse(
    is.na(x),
    "—",
    ifelse(
      x < .001,
      "< .001",
      sub(
        "^0",
        "",
        formatC(
          x,
          format = "f",
          digits = 3,
          decimal.mark = "."
        )
      )
    )
  )
}

##### CREATE APA TABLE ####

table_s10_apa <- table_s10_raw |>
  transmute(
    Model = model,
    Classes = classes,
    `Free par.` = as.integer(parameters),
    LL = formatC(
      loglikelihood,
      format = "f",
      digits = 3,
      decimal.mark = "."
    ),
    AIC = formatC(
      aic,
      format = "f",
      digits = 3,
      decimal.mark = "."
    ),
    BIC = formatC(
      bic,
      format = "f",
      digits = 3,
      decimal.mark = "."
    ),
    aBIC = formatC(
      abic,
      format = "f",
      digits = 3,
      decimal.mark = "."
    ),
    Entropy = ifelse(
      is.na(entropy),
      "—",
      format_apa_decimal(entropy)
    ),
    `VLMR p` = format_apa_p(vlmr_p),
    `aLMR p` = format_apa_p(lmr_p),
    `BLRT p (draws)` = ifelse(
      is.na(blrt_p),
      "—",
      paste0(
        format_apa_p(blrt_p),
        " (",
        bootstrap_draws,
        ")"
      )
    ),
    `Smallest class, n (%)` = paste0(
      smallest_class_n,
      " (",
      formatC(
        smallest_class_proportion * 100,
        format = "f",
        digits = 1,
        decimal.mark = "."
      ),
      "%)"
    ),
    `Condition no.` = formatC(
      condition_number,
      format = "e",
      digits = 2,
      decimal.mark = "."
    )
  )

print(
  table_s10_apa,
  n = Inf
)

##### SAVE TABLE S10 AS CSV ####

table_s10_raw_file <- file.path(
  mplus_results_dir_mal,
  "Table_S10_trajectory_class_enumeration_raw.csv"
)

table_s10_apa_file <- file.path(
  mplus_results_dir_mal,
  "Table_S10_trajectory_class_enumeration_APA.csv"
)

readr::write_csv(
  table_s10_raw,
  table_s10_raw_file
)

readr::write_csv(
  table_s10_apa,
  table_s10_apa_file
)

##### CREATE FLEXTABLE ####

table_s10_ft <- flextable::flextable(
  table_s10_apa
) |>
  flextable::theme_booktabs() |>
  flextable::font(
    fontname = "Times New Roman",
    part = "all"
  ) |>
  flextable::fontsize(
    size = 8,
    part = "all"
  ) |>
  flextable::bold(
    part = "header"
  ) |>
  flextable::bold(
    i = which(table_s10_raw$selected),
    part = "body"
  ) |>
  flextable::align(
    j = c(
      "Model",
      "Classes"
    ),
    align = "left",
    part = "all"
  ) |>
  flextable::align(
    j = setdiff(
      names(table_s10_apa),
      c("Model", "Classes")
    ),
    align = "center",
    part = "all"
  ) |>
  flextable::autofit() |>
  flextable::set_table_properties(
    layout = "autofit",
    width = 1
  )

##### CREATE WORD DOCUMENT ####

table_s10_word_file <- file.path(
  mplus_results_dir_mal,
  "Table_S10_trajectory_class_enumeration.docx"
)

table_s10_note <- paste0(
  "Note. Boldface indicates the retained solution. ",
  "Likelihood-ratio tests compare the k-class model with the ",
  "corresponding k − 1 class model. BLRT results for M17 and ",
  "M18 were based on five successful bootstrap draws and should ",
  "therefore be interpreted cautiously; the BLRT was not requested ",
  "for M19. AIC = Akaike information criterion; aBIC = sample-size ",
  "adjusted Bayesian information criterion; BIC = Bayesian ",
  "information criterion; BLRT = bootstrapped likelihood-ratio test; ",
  "aLMR = adjusted Lo–Mendell–Rubin test; LL = loglikelihood; ",
  "VLMR = Vuong–Lo–Mendell–Rubin test."
)

table_s10_doc <- officer::read_docx() |>
  officer::body_add_fpar(
    officer::fpar(
      officer::ftext(
        "Table S10",
        officer::fp_text(
          font.family = "Times New Roman",
          font.size = 10,
          bold = TRUE
        )
      )
    )
  ) |>
  officer::body_add_fpar(
    officer::fpar(
      officer::ftext(
        "Class Enumeration for Maltreatment Burden Trajectories",
        officer::fp_text(
          font.family = "Times New Roman",
          font.size = 10,
          italic = TRUE
        )
      )
    )
  )

table_s10_doc <- flextable::body_add_flextable(
  x = table_s10_doc,
  value = table_s10_ft
)

table_s10_doc <- officer::body_add_fpar(
  table_s10_doc,
  officer::fpar(
    officer::ftext(
      table_s10_note,
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 8
      )
    )
  )
)

table_s10_doc <- officer::body_end_section_landscape(
  table_s10_doc
)

print(
  table_s10_doc,
  target = table_s10_word_file
)

##### CHECK SAVED TABLE FILES ####

stopifnot(
  file.exists(table_s10_raw_file),
  file.exists(table_s10_apa_file),
  file.exists(table_s10_word_file)
)

cat(
  "Saved Table S10:\n",
  table_s10_word_file,
  "\n",
  sep = ""
)

#-------------------------------------------------------------------------
##### ARCHIVE AND COPY MALTREATMENT MPLUS FILES #####
#-------------------------------------------------------------------------

##### DEFINE FILE TRANSFER SETTINGS ####

mplus_transfer_source_dir <- mplus_input_dir

mplus_archive_dir <- file.path(
  "C:/MPLUS/Archive",
  "04_maltreatment",
  format(
    Sys.Date(),
    "%Y-%m-%d"
  )
)

mplus_transfer_model_pattern <- paste0(
  "^(",
  paste(
    c(
      "09",
      "10",
      "11",
      "12",
      "13",
      "14",
      "15",
      "16",
      "17",
      "18",
      "18a",
      "19"
    ),
    collapse = "|"
  ),
  ")_.*\\.(inp|out|dat)$"
)

##### CREATE TARGET DIRECTORIES ####

invisible(
  sapply(
    c(
      mplus_archive_dir,
      github_maltreatment_dir,
      mplus_results_dir_mal
    ),
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

##### LOCATE MALTREATMENT MPLUS FILES ####

mplus_transfer_files <- list.files(
  path = mplus_transfer_source_dir,
  pattern = mplus_transfer_model_pattern,
  full.names = TRUE,
  ignore.case = TRUE
)

stopifnot(
  length(mplus_transfer_files) > 0
)

mplus_transfer_table <- tibble(
  source_file = mplus_transfer_files,
  file_name = basename(
    source_file
  ),
  file_extension = tools::file_ext(
    source_file
  )
)

print(
  mplus_transfer_table,
  n = Inf
)

##### COPY ALL FILES TO MPLUS ARCHIVE ####

archive_copy_success <- file.copy(
  from = mplus_transfer_table$source_file,
  to = mplus_archive_dir,
  overwrite = TRUE
)

stopifnot(
  all(
    archive_copy_success
  )
)

##### COPY INPUT FILES TO GITHUB ####

mplus_input_files_to_copy <- mplus_transfer_table |>
  filter(
    file_extension == "inp"
  )

input_copy_success <- file.copy(
  from = mplus_input_files_to_copy$source_file,
  to = github_maltreatment_dir,
  overwrite = TRUE
)

stopifnot(
  all(
    input_copy_success
  )
)

##### COPY OUTPUT AND SAVEDATA FILES TO SEADRIVE RESULTS ####

mplus_output_files_to_copy <- mplus_transfer_table |>
  filter(
    file_extension %in% c(
      "out",
      "dat"
    )
  )

output_copy_success <- file.copy(
  from = mplus_output_files_to_copy$source_file,
  to = mplus_results_dir_mal,
  overwrite = TRUE
)

stopifnot(
  all(
    output_copy_success
  )
)

##### CHECK COPIED FILES ####

stopifnot(
  all(
    file.exists(
      file.path(
        mplus_archive_dir,
        mplus_transfer_table$file_name
      )
    )
  ),
  all(
    file.exists(
      file.path(
        github_maltreatment_dir,
        mplus_input_files_to_copy$file_name
      )
    )
  ),
  all(
    file.exists(
      file.path(
        mplus_results_dir_mal,
        mplus_output_files_to_copy$file_name
      )
    )
  )
)

##### DISPLAY TRANSFER SUMMARY ####

cat(
  "Archived files to:\n",
  mplus_archive_dir,
  "\n\n",
  "Copied Mplus inputs to GitHub:\n",
  github_maltreatment_dir,
  "\n\n",
  "Copied Mplus outputs and savedata files to SeaDrive results:\n",
  mplus_results_dir_mal,
  "\n",
  sep = ""
)
