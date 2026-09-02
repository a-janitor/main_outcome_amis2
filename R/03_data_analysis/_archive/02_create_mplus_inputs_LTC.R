#-------------------------------------------------------------------------
##### SETUP #####
#-------------------------------------------------------------------------

source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R")

check_packages(
  c(
    "MplusAutomation",
    "officer",
    "flextable"
  ),
  required = TRUE
)

#-------------------------------------------------------------------------
##### LOAD AND CHECK MPLUS DATASET #####
#-------------------------------------------------------------------------

##### DEFINE AND CHECK MPLUS DATASET ####

mplus_data_name <- basename(
  mplus_data_file
)

mplus_names_file <- mplus_names_file_local

assert_file_exists(
  mplus_data_file,
  "Local Mplus dataset"
)

assert_file_exists(
  mplus_names_file,
  "Local Mplus names file"
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

variable_dictionary_file <- mplus_variable_dictionary_file

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
##### RUN MPLUS MODELS M9 TO M16 #####
#-------------------------------------------------------------------------

##### SWITCH FOR AUTOMATIC MPLUS EXECUTION #####

run_m9_to_m16 <- TRUE


##### COLLECT CREATED INPUT FILES #####

mplus_input_files_m9_to_m16 <- c(
  input_file_m9,
  input_file_m10,
  input_file_m11,
  input_file_m12,
  input_file_m13,
  input_file_m14,
  input_file_m15,
  input_file_m16
)


##### CHECK INPUT FILES #####

missing_input_files <- mplus_input_files_m9_to_m16[
  !file.exists(mplus_input_files_m9_to_m16)
]

if (length(missing_input_files) > 0) {
  stop(
    "The following Mplus input files are missing:\n",
    paste(
      missing_input_files,
      collapse = "\n"
    )
  )
}


##### CHECK MPLUS DATA FILE #####

assert_file_exists(
  mplus_data_file,
  "Local Mplus dataset"
)


##### CHECK WHETHER MPLUS IS AVAILABLE #####

if (
  MplusAutomation::mplusAvailable(
    silent = FALSE
  ) != 0
) {
  stop(
    "Mplus could not be detected by MplusAutomation."
  )
}


##### DISPLAY SELECTED MODELS #####

cat(
  "\nThe following Mplus models will be run:\n",
  paste(
    basename(mplus_input_files_m9_to_m16),
    collapse = "\n"
  ),
  "\n\n",
  sep = ""
)


##### RUN M9 TO M16 #####

if (run_m9_to_m16) {
  
  MplusAutomation::runModels(
    target = mplus_input_files_m9_to_m16,
    
    # Rerun all models and overwrite existing output files
    replaceOutfile = "always",
    
    # Do not print the complete Mplus output in the R console
    showOutput = FALSE,
    
    # Save run information
    logFile = file.path(
      mplus_input_dir,
      "M9_to_M16_run.log"
    ),
    
    # Display progress in the R console
    quiet = FALSE,
    
    # Terminate remaining Mplus processes if a run fails
    killOnFail = TRUE
  )
}

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

##### SAVE M18 INPUT ####

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

##### COPY M18 INPUT TO GITHUB ####

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

input_file_m18a <- input_file

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
##### RUN MPLUS MODELS M17, M18, M19, AND M18A #####
#-------------------------------------------------------------------------

##### SELECT MODELS TO RUN #####

run_m17  <- TRUE
run_m18  <- TRUE
run_m19  <- TRUE
run_m18a <- TRUE


##### COLLECT SELECTED INPUT FILES #####

mplus_input_files_m17_to_m18a <- c(
  if (run_m17)  input_file_m17,
  if (run_m18)  input_file_m18,
  if (run_m19)  input_file_m19,
  if (run_m18a) input_file_m18a
)


##### CHECK THAT AT LEAST ONE MODEL WAS SELECTED #####

if (length(mplus_input_files_m17_to_m18a) == 0) {
  stop(
    "No Mplus models were selected for execution."
  )
}


##### CHECK INPUT FILES #####

missing_input_files <- mplus_input_files_m17_to_m18a[
  !file.exists(mplus_input_files_m17_to_m18a)
]

if (length(missing_input_files) > 0) {
  stop(
    "The following Mplus input files are missing:\n",
    paste(
      missing_input_files,
      collapse = "\n"
    )
  )
}


##### CHECK MPLUS DATA FILE #####

assert_file_exists(
  mplus_data_file,
  "Local Mplus dataset"
)


##### CHECK WHETHER MPLUS IS AVAILABLE #####

if (
  MplusAutomation::mplusAvailable(
    silent = FALSE
  ) != 0
) {
  stop(
    "Mplus could not be detected by MplusAutomation."
  )
}


##### REMOVE OLD M18 CPROB FILE #####

# This prevents an old class-probability file from being
# mistaken for a newly generated file.

if (
  run_m18 &&
  file.exists(m18_cprob_file)
) {
  unlink(
    m18_cprob_file
  )
}

if (
  run_m18 &&
  file.exists(m18_cprob_file)
) {
  stop(
    "The old M18 class-probability file could not be removed:\n",
    m18_cprob_file
  )
}


##### DISPLAY SELECTED MODELS #####

cat(
  "\nThe following Mplus models will be run:\n",
  paste(
    basename(mplus_input_files_m17_to_m18a),
    collapse = "\n"
  ),
  "\n\n",
  sep = ""
)


##### RUN SELECTED MODELS #####

MplusAutomation::runModels(
  target = mplus_input_files_m17_to_m18a,
  
  # Rerun models and overwrite existing output files
  replaceOutfile = "always",
  
  # Do not print the complete Mplus output
  showOutput = FALSE,
  
  # Save run information
  logFile = file.path(
    mplus_input_dir,
    "M17_M18_M19_M18a_run.log"
  ),
  
  # Display progress
  quiet = FALSE,
  
  # Stop remaining processes if a model fails
  killOnFail = TRUE
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
  inner_join(
    m18_classes_mplus,
    by = "SIC_N"
  )

##### CHECK WHICH MPLUS CASES HAVE NO M18 CLASS #####

stopifnot(
  nrow(dat_mplus_m18) == nrow(m18_classes_mplus),
  nrow(dat_mplus_m18) == 849,
  anyDuplicated(dat_mplus_m18$SIC_N) == 0,
  !anyNA(dat_mplus_m18$m18_cls),
  all(vapply(dat_mplus_m18, is.numeric, logical(1))),
  !"mt_class_label" %in% names(dat_mplus_m18),
  all(c("m18_cls", "m18_p1", "m18_p2", "m18_p3", "m18_pmx") %in%
        names(dat_mplus_m18))
)

##### FINAL AUDIT: CHECK M18 CLASS MERGE #####

m18_merge_audit <- dat_mplus_m18 |>
  select(
    SIC_N,
    m18_cls,
    m18_p1,
    m18_p2,
    m18_p3,
    m18_pmx
  ) |>
  left_join(
    m18_classes_mplus,
    by = "SIC_N",
    suffix = c("_merged", "_source")
  )

stopifnot(
  nrow(m18_merge_audit) == nrow(m18_classes_mplus),
  anyDuplicated(m18_merge_audit$SIC_N) == 0,
  !anyNA(m18_merge_audit$m18_cls_merged),
  all(m18_merge_audit$m18_cls_merged == m18_merge_audit$m18_cls_source),
  all(abs(m18_merge_audit$m18_p1_merged - m18_merge_audit$m18_p1_source) < 1e-10),
  all(abs(m18_merge_audit$m18_p2_merged - m18_merge_audit$m18_p2_source) < 1e-10),
  all(abs(m18_merge_audit$m18_p3_merged - m18_merge_audit$m18_p3_source) < 1e-10),
  all(abs(m18_merge_audit$m18_pmx_merged - m18_merge_audit$m18_pmx_source) < 1e-10)
)

##### CHECK WHETHER ASSIGNED CLASS MATCHES HIGHEST POSTERIOR PROBABILITY #####

m18_classes_mplus |>
  mutate(
    max_prob_class = max.col(
      cbind(
        m18_p1,
        m18_p2,
        m18_p3
      ),
      ties.method = "first"
    ),
    class_matches_max_prob = m18_cls == max_prob_class
  ) |>
  count(
    class_matches_max_prob
  ) |>
  print(
    n = Inf
  )

stopifnot(
  all(
    m18_classes_mplus$m18_cls ==
      max.col(
        cbind(
          m18_classes_mplus$m18_p1,
          m18_classes_mplus$m18_p2,
          m18_classes_mplus$m18_p3
        ),
        ties.method = "first"
      )
  )
)

cat(
  "M18 class merge audit passed.\n"
)

m18_classes_mplus$m18_cls ==
  max.col(
    cbind(
      m18_classes_mplus$m18_p1,
      m18_classes_mplus$m18_p2,
      m18_classes_mplus$m18_p3
    ),
    ties.method = "first"
  )

##### CHECK MERGED MPLUS DATASET ####

stopifnot(
  nrow(dat_mplus_m18) == nrow(dat_mplus),
  anyDuplicated(dat_mplus_m18$SIC_N) == 0,
  all(vapply(dat_mplus_m18, is.numeric, logical(1))),
  !"mt_class_label" %in% names(dat_mplus_m18),
  all(c("m18_cls", "m18_p1", "m18_p2", "m18_p3", "m18_pmx") %in%
        names(dat_mplus_m18)),
  all(nchar(names(dat_mplus_m18)) <= 8)
)

tail(
  names(dat_mplus_m18),
  10
)

#-------------------------------------------------------------------------
##### M17 CLASSES MAL ONLY (ALTERNATIVE) #####
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
##### CREATE M17B FROM EXISTING M17 INPUT: MALREATED-ONLY SAMPLE #####
#-------------------------------------------------------------------------

##### DEFINE FILES #####

m17_source_file <- file.path(
  mplus_input_dir,
  "17_mt_burden_quadratic_2class.inp"
)

m17b_output_file <- file.path(
  mplus_input_dir,
  "17b_mt_burden_quadratic_2class_maltreated.inp"
)

stopifnot(
  file.exists(m17_source_file)
)


##### READ EXISTING M17 INPUT #####

m17b_syntax <- readLines(
  m17_source_file,
  warn = FALSE
)


##### CHECK THAT FILTER IS NOT ALREADY PRESENT #####

if (
  any(
    grepl(
      "USEOBSERVATIONS",
      m17b_syntax,
      ignore.case = TRUE
    )
  )
) {
  stop(
    "The source input already contains a USEOBSERVATIONS statement."
  )
}


##### LOCATE IDVARIABLE STATEMENT #####

idvariable_position <- grep(
  "^\\s*IDVARIABLE\\s+IS\\s+SIC_N\\s*;",
  m17b_syntax,
  ignore.case = TRUE
)

if (length(idvariable_position) != 1) {
  stop(
    "Could not identify exactly one 'IDVARIABLE IS SIC_N;' statement."
  )
}


##### INSERT MALREATED-ONLY FILTER #####

m17b_syntax <- append(
  m17b_syntax,
  values = "  USEOBSERVATIONS ARE mal_all EQ 1;",
  after = idvariable_position
)


##### UPDATE TITLE #####

m17b_syntax <- sub(
  "M17: Two-class quadratic growth mixture model for",
  "M17b: Two-class quadratic growth mixture model among maltreated participants for",
  m17b_syntax,
  fixed = TRUE
)


##### SAVE NEW INPUT #####

writeLines(
  m17b_syntax,
  con = m17b_output_file
)


##### FINAL CHECKS #####

saved_m17b_syntax <- readLines(
  m17b_output_file,
  warn = FALSE
)

stopifnot(
  file.exists(m17b_output_file),
  
  sum(
    grepl(
      "USEOBSERVATIONS ARE mal_all EQ 1;",
      saved_m17b_syntax,
      fixed = TRUE
    )
  ) == 1,
  
  any(
    grepl(
      "CLASSES = c\\(2\\);",
      saved_m17b_syntax,
      ignore.case = TRUE
    )
  )
)


##### OPTIONAL: COPY TO GITHUB #####

github_m17b_file <- file.path(
  github_maltreatment_dir,
  basename(m17b_output_file)
)

copy_success <- file.copy(
  from = m17b_output_file,
  to = github_m17b_file,
  overwrite = TRUE
)

stopifnot(
  copy_success,
  file.exists(github_m17b_file)
)


cat(
  "\nM17b input created successfully.",
  "\nLocal input:",
  "\n", m17b_output_file,
  "\n",
  "\nGitHub copy:",
  "\n", github_m17b_file,
  "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### CREATE M18B FROM EXISTING M18 INPUT: MALTREATED-ONLY SAMPLE #####
#-------------------------------------------------------------------------

##### DEFINE FILES #####

m18_source_file <- file.path(
  mplus_input_dir,
  "18_mt_burden_quadratic_3class.inp"
)

m18b_output_file <- file.path(
  mplus_input_dir,
  "18b_mt_burden_quadratic_3class_maltreated.inp"
)

stopifnot(
  file.exists(m18_source_file)
)


##### READ EXISTING M18 INPUT #####

m18b_syntax <- readLines(
  m18_source_file,
  warn = FALSE
)


##### CHECK THAT FILTER IS NOT ALREADY PRESENT #####

if (
  any(
    grepl(
      "USEOBSERVATIONS",
      m18b_syntax,
      ignore.case = TRUE
    )
  )
) {
  stop(
    "The source input already contains a USEOBSERVATIONS statement."
  )
}


##### LOCATE IDVARIABLE STATEMENT #####

idvariable_position <- grep(
  "^\\s*IDVARIABLE\\s+IS\\s+SIC_N\\s*;",
  m18b_syntax,
  ignore.case = TRUE
)

if (length(idvariable_position) != 1) {
  stop(
    "Could not identify exactly one 'IDVARIABLE IS SIC_N;' statement."
  )
}


##### INSERT MALTREATED-ONLY FILTER #####

m18b_syntax <- append(
  m18b_syntax,
  values = "  USEOBSERVATIONS ARE mal_all EQ 1;",
  after = idvariable_position
)


##### UPDATE TITLE #####

m18b_syntax <- sub(
  "M18: Three-class quadratic growth mixture model for",
  "M18b: Three-class quadratic growth mixture model among maltreated participants for",
  m18b_syntax,
  fixed = TRUE
)


##### UPDATE SAVEDATA FILE NAME #####

m18b_syntax <- sub(
  "FILE = 18_mt_burden_quadratic_3class_cprob.dat;",
  "FILE = 18b_mt_burden_quadratic_3class_maltreated_cprob.dat;",
  m18b_syntax,
  fixed = TRUE
)


##### SAVE NEW INPUT #####

writeLines(
  m18b_syntax,
  con = m18b_output_file
)


##### FINAL CHECKS #####

saved_m18b_syntax <- readLines(
  m18b_output_file,
  warn = FALSE
)

stopifnot(
  file.exists(m18b_output_file),
  
  sum(
    grepl(
      "USEOBSERVATIONS ARE mal_all EQ 1;",
      saved_m18b_syntax,
      fixed = TRUE
    )
  ) == 1,
  
  any(
    grepl(
      "CLASSES = c\\(3\\);",
      saved_m18b_syntax,
      ignore.case = TRUE
    )
  ),
  
  any(
    grepl(
      "FILE = 18b_mt_burden_quadratic_3class_maltreated_cprob.dat;",
      saved_m18b_syntax,
      fixed = TRUE
    )
  )
)


##### COPY TO GITHUB #####

github_m18b_file <- file.path(
  github_maltreatment_dir,
  basename(m18b_output_file)
)

copy_success <- file.copy(
  from = m18b_output_file,
  to = github_m18b_file,
  overwrite = TRUE
)

stopifnot(
  copy_success,
  file.exists(github_m18b_file)
)


cat(
  "\nM18b input created successfully.",
  "\nLocal input:",
  "\n", m18b_output_file,
  "\n",
  "\nGitHub copy:",
  "\n", github_m18b_file,
  "\n",
  sep = ""
)

#-------------------------------------------------------------------------
##### CREATE M19B FROM EXISTING M19 INPUT: MALTREATED-ONLY SAMPLE #####
#-------------------------------------------------------------------------

##### DEFINE FILES #####

m19_source_file <- file.path(
  mplus_input_dir,
  "19_mt_burden_quadratic_4class.inp"
)

m19b_output_file <- file.path(
  mplus_input_dir,
  "19b_mt_burden_quadratic_4class_maltreated.inp"
)

stopifnot(
  file.exists(m19_source_file)
)


##### READ EXISTING M19 INPUT #####

m19b_syntax <- readLines(
  m19_source_file,
  warn = FALSE
)


##### CHECK THAT FILTER IS NOT ALREADY PRESENT #####

if (
  any(
    grepl(
      "USEOBSERVATIONS",
      m19b_syntax,
      ignore.case = TRUE
    )
  )
) {
  stop(
    "The source input already contains a USEOBSERVATIONS statement."
  )
}


##### LOCATE IDVARIABLE STATEMENT #####

idvariable_position <- grep(
  "^\\s*IDVARIABLE\\s+IS\\s+SIC_N\\s*;",
  m19b_syntax,
  ignore.case = TRUE
)

if (length(idvariable_position) != 1) {
  stop(
    "Could not identify exactly one 'IDVARIABLE IS SIC_N;' statement."
  )
}


##### INSERT MALTREATED-ONLY FILTER #####

m19b_syntax <- append(
  m19b_syntax,
  values = "  USEOBSERVATIONS ARE mal_all EQ 1;",
  after = idvariable_position
)


##### UPDATE TITLE #####

m19b_syntax <- sub(
  "M19: Four-class quadratic growth mixture model for",
  "M19b: Four-class quadratic growth mixture model among maltreated participants for",
  m19b_syntax,
  fixed = TRUE
)


##### SAVE NEW INPUT #####

writeLines(
  m19b_syntax,
  con = m19b_output_file
)


##### FINAL CHECKS #####

saved_m19b_syntax <- readLines(
  m19b_output_file,
  warn = FALSE
)

stopifnot(
  file.exists(m19b_output_file),
  
  sum(
    grepl(
      "USEOBSERVATIONS ARE mal_all EQ 1;",
      saved_m19b_syntax,
      fixed = TRUE
    )
  ) == 1,
  
  any(
    grepl(
      "CLASSES = c\\(4\\);",
      saved_m19b_syntax,
      ignore.case = TRUE
    )
  )
)


##### COPY TO GITHUB #####

github_m19b_file <- file.path(
  github_maltreatment_dir,
  basename(m19b_output_file)
)

copy_success <- file.copy(
  from = m19b_output_file,
  to = github_m19b_file,
  overwrite = TRUE
)

stopifnot(
  copy_success,
  file.exists(github_m19b_file)
)


cat(
  "\nM19b input created successfully.",
  "\nLocal input:",
  "\n", m19b_output_file,
  "\n",
  "\nGitHub copy:",
  "\n", github_m19b_file,
  "\n",
  sep = ""
)

##### RUN M17b - M19b ####

MplusAutomation::runModels(
  target = c(m17b_output_file,
    m18b_output_file,
    m19b_output_file),
  replaceOutfile = "always",
  showOutput = FALSE,
  quiet = FALSE,
  killOnFail = TRUE
)

#-------------------------------------------------------------------------
##### SAVE MPLUS DATASET WITH M18 CLASSES #####
#-------------------------------------------------------------------------

##### DEFINE LABEL-FREE DATASET FOR MPLUS EXPORT ####

dat_mplus_m18_for_export <- dat_mplus_m18 |>
  select(
    -any_of("mt_class_label")
  )

stopifnot(
  !"mt_class_label" %in% names(dat_mplus_m18_for_export),
  all(vapply(dat_mplus_m18_for_export, is.numeric, logical(1))),
  all(nchar(names(dat_mplus_m18_for_export)) <= 8),
  anyDuplicated(names(dat_mplus_m18_for_export)) == 0
)

##### REPLACE MISSING VALUES FOR MPLUS EXPORT ####

dat_mplus_m18_export <- dat_mplus_m18_for_export |>
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

mplus_data_name_m18 <- basename(
  mplus_data_file_m18
)

mplus_names_file_m18 <- mplus_names_file_m18_local
mplus_names_text_file_m18 <- mplus_names_text_file_m18_local
mplus_rds_file_m18 <- mplus_rds_file_m18_local

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
  dat_mplus_m18_for_export
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
  dat_mplus_m18_for_export,
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
    names(dat_mplus_m18_for_export)
  ),
  length(saved_names_m18) ==
    ncol(dat_mplus_m18_for_export)
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
  nrow(dat_mplus_m18_for_export),
  "\n",
  "Columns: ",
  ncol(dat_mplus_m18_for_export),
  "\n",
  sep = ""
)

##### COPY M18 MPLUS DATASET TO SEADRIVE ####

copy_success <- file.copy(
  from = mplus_data_file_m18,
  to = seadrive_mplus_data_dir,
  overwrite = TRUE
)

stopifnot(
  copy_success
)

##### COPY M18 MPLUS NAMES FILES TO SEADRIVE ####

file.copy(
  from = c(
    mplus_names_file_m18,
    mplus_names_text_file_m18,
    mplus_rds_file_m18
  ),
  to = seadrive_mplus_data_dir,
  overwrite = TRUE
)

##### MERGE M18 CLASSES WITH ORIGINAL EXCEL DATASET ####

dat_original <- read_excel(
  master_excel_file
)

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

##### FINAL AUDIT: CHECK SIC TO SIC_N LOOKUP #####

stopifnot(
  nrow(id_lookup) == n_distinct(dat_original$sic),
  anyDuplicated(id_lookup$sic) == 0,
  anyDuplicated(id_lookup$SIC_N) == 0,
  min(id_lookup$SIC_N) == 1,
  max(id_lookup$SIC_N) == nrow(id_lookup)
)

##### CHECK WHETHER ALL M18 SIC_N VALUES CAN BE TRANSLATED BACK TO SIC #####

m18_lookup_audit <- m18_class_assignments |>
  left_join(
    id_lookup,
    by = "SIC_N"
  )

stopifnot(
  nrow(m18_lookup_audit) == nrow(m18_class_assignments),
  anyDuplicated(m18_lookup_audit$SIC_N) == 0,
  !anyNA(m18_lookup_audit$sic)
)

##### CHECK EXPECTED NON-M18 CASES IN ORIGINAL LOOKUP #####

original_without_m18 <- id_lookup |>
  anti_join(
    m18_class_assignments,
    by = "SIC_N"
  )

nrow(original_without_m18)

original_without_m18 |>
  select(
    sic,
    SIC_N
  ) |>
  print(
    n = Inf
  )

stopifnot(
  nrow(original_without_m18) == 15
)

##### CHECK EXCEL MERGE AGAINST M18 SOURCE AFTER SIC LOOKUP #####

dat_original_m18_audit <- dat_original |>
  left_join(
    m18_lookup_audit |>
      select(
        sic,
        mt_class,
        mt_prob_class1,
        mt_prob_class2,
        mt_prob_class3,
        mt_prob_max,
        mt_class_label
      ),
    by = "sic"
  )

m18_excel_source_check <- dat_original_m18_audit |>
  filter(
    !is.na(mt_class)
  ) |>
  left_join(
    m18_lookup_audit |>
      select(
        sic,
        mt_class_source = mt_class,
        mt_prob_class1_source = mt_prob_class1,
        mt_prob_class2_source = mt_prob_class2,
        mt_prob_class3_source = mt_prob_class3,
        mt_prob_max_source = mt_prob_max,
        mt_class_label_source = mt_class_label
      ),
    by = "sic"
  )

stopifnot(
  nrow(m18_excel_source_check) == nrow(m18_class_assignments),
  all(m18_excel_source_check$mt_class == m18_excel_source_check$mt_class_source),
  all(abs(m18_excel_source_check$mt_prob_class1 - m18_excel_source_check$mt_prob_class1_source) < 1e-10),
  all(abs(m18_excel_source_check$mt_prob_class2 - m18_excel_source_check$mt_prob_class2_source) < 1e-10),
  all(abs(m18_excel_source_check$mt_prob_class3 - m18_excel_source_check$mt_prob_class3_source) < 1e-10),
  all(abs(m18_excel_source_check$mt_prob_max - m18_excel_source_check$mt_prob_max_source) < 1e-10),
  all(m18_excel_source_check$mt_class_label == m18_excel_source_check$mt_class_label_source)
)

cat(
  "SIC_N lookup audit passed.\n"
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
  anyDuplicated(dat_original_m18$sic) == 0,
  "mt_class_label" %in% names(dat_original_m18)
)

##### SAVE NEW EXCEL DATASET ####

excel_file_m18 <- m18_excel_file

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
##### SAVE MPLUS DATASET WITH M18B CLASSES + NON-MALTREATED REFERENCE #####
#-------------------------------------------------------------------------

##### M18B MPLUS OUTPUT #####

m18b_output_file_mo <- file.path(
  mplus_input_dir,
  "18b_mt_burden_quadratic_3class_maltreated.out"
)

assert_file_exists(
  m18b_output_file_mo,
  "M18b maltreated-only Mplus output"
)


##### LOCAL MPLUS OUTPUT FILES #####

mplus_data_file_m18_mo <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset_m18_mo.dat"
)

mplus_names_file_m18_mo <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18_mo.rds"
)

mplus_names_text_file_m18_mo <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18_mo.txt"
)

mplus_rds_file_m18_mo <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset_m18_mo.rds"
)


##### SEADRIVE MPLUS OUTPUT FILES #####

seadrive_mplus_data_file_m18_mo <- file.path(
  seadrive_mplus_data_dir,
  basename(mplus_data_file_m18_mo)
)

seadrive_mplus_names_file_m18_mo <- file.path(
  seadrive_mplus_data_dir,
  basename(mplus_names_file_m18_mo)
)

seadrive_mplus_names_text_file_m18_mo <- file.path(
  seadrive_mplus_data_dir,
  basename(mplus_names_text_file_m18_mo)
)

seadrive_mplus_rds_file_m18_mo <- file.path(
  seadrive_mplus_data_dir,
  basename(mplus_rds_file_m18_mo)
)


##### EXCEL OUTPUT FILE #####

excel_file_m18_mo <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes_mo.xlsx"
)


#-------------------------------------------------------------------------
##### READ M18B CLASS-PROBABILITY OUTPUT #####
#-------------------------------------------------------------------------

m18b_results_mo <- MplusAutomation::readModels(
  target = m18b_output_file_mo,
  what = "all"
)

if (is.null(m18b_results_mo$savedata)) {
  stop(
    paste0(
      "No SAVEDATA results were found in ",
      basename(m18b_output_file_mo),
      ". Check whether SAVE = CPROBABILITIES was requested."
    )
  )
}

m18b_savedata_mo <- m18b_results_mo$savedata |>
  as_tibble()

names(m18b_savedata_mo) <- tolower(
  names(m18b_savedata_mo)
)


##### CHECK SAVED VARIABLES #####

required_m18b_saved_variables_mo <- c(
  "sic_n",
  "c",
  "cprob1",
  "cprob2",
  "cprob3"
)

missing_m18b_saved_variables_mo <- setdiff(
  required_m18b_saved_variables_mo,
  names(m18b_savedata_mo)
)

if (length(missing_m18b_saved_variables_mo) > 0) {
  stop(
    "Missing M18b SAVEDATA variables: ",
    paste(
      missing_m18b_saved_variables_mo,
      collapse = ", "
    )
  )
}


#-------------------------------------------------------------------------
##### CREATE MALTREATED-ONLY CLASS ASSIGNMENTS #####
#-------------------------------------------------------------------------

##### CLASS LABELS #####

# These labels follow the current interpretation of the M18b output.
# Check them once more against the final trajectory plot before publication.

m18b_labels_mo <- c(
  "Moderate and initially increasing burden",
  "Elevated and declining burden",
  "High burden with later rebound"
)


##### CREATE CLASS DATA #####

m18b_class_assignments_mo <- m18b_savedata_mo |>
  transmute(
    SIC_N = as.integer(sic_n),
    
    # Original M18b classes among maltreated children: 1–3
    mt_class_mo_original = as.integer(c),
    
    mt_prob_mo_original1 = as.numeric(cprob1),
    mt_prob_mo_original2 = as.numeric(cprob2),
    mt_prob_mo_original3 = as.numeric(cprob3),
    
    mt_prob_mo_original_max = pmax(
      cprob1,
      cprob2,
      cprob3
    )
  ) |>
  mutate(
    # Shift maltreated-only classes from 1–3 to 2–4
    mt_class_mo = mt_class_mo_original + 1L,
    
    mt_class_label_mo = case_when(
      mt_class_mo == 2L ~ m18b_labels_mo[1],
      mt_class_mo == 3L ~ m18b_labels_mo[2],
      mt_class_mo == 4L ~ m18b_labels_mo[3],
      TRUE ~ NA_character_
    )
  )


##### CHECK MALTREATED-ONLY ASSIGNMENTS #####

stopifnot(
  nrow(m18b_class_assignments_mo) == 303,
  anyDuplicated(m18b_class_assignments_mo$SIC_N) == 0,
  !anyNA(m18b_class_assignments_mo$SIC_N),
  !anyNA(m18b_class_assignments_mo$mt_class_mo_original),
  all(m18b_class_assignments_mo$mt_class_mo_original %in% 1:3),
  all(m18b_class_assignments_mo$mt_class_mo %in% 2:4),
  all(
    m18b_class_assignments_mo$mt_prob_mo_original_max >= 0 &
      m18b_class_assignments_mo$mt_prob_mo_original_max <= 1
  )
)


##### CHECK CLASS MATCHES MAXIMUM POSTERIOR PROBABILITY #####

posterior_matrix_m18b_mo <- m18b_class_assignments_mo |>
  select(
    mt_prob_mo_original1,
    mt_prob_mo_original2,
    mt_prob_mo_original3
  ) |>
  as.matrix()

stopifnot(
  all(
    abs(
      rowSums(posterior_matrix_m18b_mo) - 1
    ) < 0.01
  ),
  all(
    m18b_class_assignments_mo$mt_class_mo_original ==
      max.col(
        posterior_matrix_m18b_mo,
        ties.method = "first"
      )
  )
)


#-------------------------------------------------------------------------
##### LOAD ORIGINAL MPLUS DATASET #####
#-------------------------------------------------------------------------

dat_mplus <- read_delim(
  file = mplus_data_file,
  delim = "\t",
  col_names = mplus_names,
  na = "-999",
  trim_ws = TRUE,
  col_types = cols(
    .default = col_double()
  ),
  progress = FALSE,
  name_repair = "minimal"
)


##### CHECK ORIGINAL MPLUS DATASET #####

stopifnot(
  ncol(dat_mplus) == length(mplus_names),
  identical(names(dat_mplus), mplus_names),
  all(vapply(dat_mplus, is.numeric, logical(1))),
  "SIC_N" %in% names(dat_mplus),
  "mal_all" %in% names(dat_mplus),
  !anyNA(dat_mplus$SIC_N),
  anyDuplicated(dat_mplus$SIC_N) == 0
)


##### CHECK MALTREATMENT STATUS CODING #####

observed_mal_all_values_mo <- sort(
  unique(
    na.omit(
      dat_mplus$mal_all
    )
  )
)

stopifnot(
  all(
    observed_mal_all_values_mo %in% c(
      0,
      1
    )
  )
)

cat(
  "\nMaltreatment-status distribution:\n"
)

print(
  table(
    dat_mplus$mal_all,
    useNA = "ifany"
  )
)


#-------------------------------------------------------------------------
##### AUDIT M18B SAMPLE AGAINST MAL_ALL #####
#-------------------------------------------------------------------------

m18b_status_audit_mo <- m18b_class_assignments_mo |>
  left_join(
    dat_mplus |>
      select(
        SIC_N,
        mal_all
      ),
    by = "SIC_N"
  )

stopifnot(
  nrow(m18b_status_audit_mo) ==
    nrow(m18b_class_assignments_mo),
  
  anyDuplicated(m18b_status_audit_mo$SIC_N) == 0,
  
  !anyNA(m18b_status_audit_mo$mal_all),
  
  all(
    m18b_status_audit_mo$mal_all == 1
  ),
  
  sum(
    dat_mplus$mal_all == 1,
    na.rm = TRUE
  ) ==
    nrow(m18b_class_assignments_mo)
)


##### ENSURE NO NON-MALTREATED CHILD RECEIVED AN M18B CLASS #####

stopifnot(
  !any(
    m18b_class_assignments_mo$SIC_N %in%
      dat_mplus$SIC_N[
        dat_mplus$mal_all == 0
      ]
  )
)


#-------------------------------------------------------------------------
##### CREATE COMBINED FOUR-GROUP CLASSIFICATION #####
#-------------------------------------------------------------------------

combined_classes_mo <- dat_mplus |>
  select(
    SIC_N,
    mal_all
  ) |>
  left_join(
    m18b_class_assignments_mo,
    by = "SIC_N"
  ) |>
  mutate(
    # Combined class:
    # 1 = non-maltreated
    # 2–4 = M18b maltreated-only trajectory classes
    
    mt_class_mo = case_when(
      mal_all == 0 ~ 1L,
      mal_all == 1 ~ mt_class_mo,
      TRUE ~ NA_integer_
    ),
    
    mt_class_label_mo = case_when(
      mt_class_mo == 1L ~ "Non-maltreated",
      mt_class_mo == 2L ~ m18b_labels_mo[1],
      mt_class_mo == 3L ~ m18b_labels_mo[2],
      mt_class_mo == 4L ~ m18b_labels_mo[3],
      TRUE ~ NA_character_
    ),
    
# Posterior probabilities in the combined four-group solution
   
    
    mt_prob_class1_mo = case_when(
      mal_all == 0 ~ 1,
      mal_all == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    mt_prob_class2_mo = case_when(
      mal_all == 0 ~ 0,
      mal_all == 1 ~ mt_prob_mo_original1,
      TRUE ~ NA_real_
    ),
    
    mt_prob_class3_mo = case_when(
      mal_all == 0 ~ 0,
      mal_all == 1 ~ mt_prob_mo_original2,
      TRUE ~ NA_real_
    ),
    
    mt_prob_class4_mo = case_when(
      mal_all == 0 ~ 0,
      mal_all == 1 ~ mt_prob_mo_original3,
      TRUE ~ NA_real_
    ),
    
    mt_prob_max_mo = case_when(
      mal_all == 0 ~ 1,
      mal_all == 1 ~ mt_prob_mo_original_max,
      TRUE ~ NA_real_
    )
  ) |>
  select(
    SIC_N,
    mal_all,
    mt_class_mo,
    mt_prob_class1_mo,
    mt_prob_class2_mo,
    mt_prob_class3_mo,
    mt_prob_class4_mo,
    mt_prob_max_mo,
    mt_class_label_mo
  )


#-------------------------------------------------------------------------
##### CHECK COMBINED FOUR-GROUP CLASSIFICATION #####
#-------------------------------------------------------------------------

stopifnot(
  nrow(combined_classes_mo) == nrow(dat_mplus),
  anyDuplicated(combined_classes_mo$SIC_N) == 0,
  
  all(
    combined_classes_mo$mt_class_mo[
      !is.na(combined_classes_mo$mal_all) &
        combined_classes_mo$mal_all == 0
    ] == 1
  ),
  
  all(
    combined_classes_mo$mt_class_mo[
      !is.na(combined_classes_mo$mal_all) &
        combined_classes_mo$mal_all == 1
    ] %in% 2:4
  ),
  
  all(
    is.na(
      combined_classes_mo$mt_class_mo[
        is.na(combined_classes_mo$mal_all)
      ]
    )
  )
)


##### CHECK POSTERIOR PROBABILITY SUMS #####

combined_probability_matrix_mo <- combined_classes_mo |>
  filter(
    !is.na(mt_class_mo)
  ) |>
  select(
    mt_prob_class1_mo,
    mt_prob_class2_mo,
    mt_prob_class3_mo,
    mt_prob_class4_mo
  ) |>
  as.matrix()

stopifnot(
  !anyNA(combined_probability_matrix_mo),
  all(
    abs(
      rowSums(combined_probability_matrix_mo) - 1
    ) < 0.01
  )
)


##### CHECK ASSIGNED CLASS MATCHES MAXIMUM PROBABILITY #####

combined_classification_check_mo <- combined_classes_mo |>
  filter(
    !is.na(mt_class_mo)
  )

stopifnot(
  all(
    combined_classification_check_mo$mt_class_mo ==
      max.col(
        combined_probability_matrix_mo,
        ties.method = "first"
      )
  )
)


##### DISPLAY CLASS DISTRIBUTION #####

combined_class_distribution_mo <- combined_classes_mo |>
  filter(
    !is.na(mt_class_mo)
  ) |>
  count(
    mt_class_mo,
    mt_class_label_mo,
    name = "n"
  ) |>
  mutate(
    percent = 100 * n / sum(n)
  ) |>
  arrange(
    mt_class_mo
  )

print(
  combined_class_distribution_mo,
  n = Inf
)


#-------------------------------------------------------------------------
##### PREPARE SHORT MPLUS CLASS VARIABLES #####
#-------------------------------------------------------------------------

classes_mplus_mo <- combined_classes_mo |>
  transmute(
    SIC_N,
    
    # All Mplus names must contain no more than eight characters
    mo_cls = as.numeric(mt_class_mo),
    mo_p1  = mt_prob_class1_mo,
    mo_p2  = mt_prob_class2_mo,
    mo_p3  = mt_prob_class3_mo,
    mo_p4  = mt_prob_class4_mo,
    mo_pmx = mt_prob_max_mo
  )


##### CHECK MPLUS CLASS VARIABLES #####

stopifnot(
  anyDuplicated(classes_mplus_mo$SIC_N) == 0,
  all(nchar(names(classes_mplus_mo)) <= 8),
  
  all(
    na.omit(
      classes_mplus_mo$mo_cls
    ) %in% 1:4
  )
)


#-------------------------------------------------------------------------
##### MERGE COMBINED CLASSES WITH ORIGINAL MPLUS DATASET #####
#-------------------------------------------------------------------------

dat_mplus_m18_mo <- dat_mplus |>
  left_join(
    classes_mplus_mo,
    by = "SIC_N"
  )


##### CHECK MPLUS MERGE #####

stopifnot(
  nrow(dat_mplus_m18_mo) == nrow(dat_mplus),
  anyDuplicated(dat_mplus_m18_mo$SIC_N) == 0,
  identical(
    dat_mplus_m18_mo$SIC_N,
    dat_mplus$SIC_N
  ),
  all(vapply(dat_mplus_m18_mo, is.numeric, logical(1))),
  all(
    c(
      "mo_cls",
      "mo_p1",
      "mo_p2",
      "mo_p3",
      "mo_p4",
      "mo_pmx"
    ) %in% names(dat_mplus_m18_mo)
  ),
  all(nchar(names(dat_mplus_m18_mo)) <= 8),
  anyDuplicated(names(dat_mplus_m18_mo)) == 0
)


#-------------------------------------------------------------------------
##### PREPARE MPLUS EXPORT #####
#-------------------------------------------------------------------------

dat_mplus_m18_mo_for_export <- dat_mplus_m18_mo


##### REPLACE MISSING VALUES WITH -999 #####

dat_mplus_m18_mo_export <- dat_mplus_m18_mo_for_export |>
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
  all(vapply(dat_mplus_m18_mo_export, is.numeric, logical(1))),
  sum(is.na(dat_mplus_m18_mo_export)) == 0,
  all(nchar(names(dat_mplus_m18_mo_export)) <= 8),
  anyDuplicated(names(dat_mplus_m18_mo_export)) == 0
)


#-------------------------------------------------------------------------
##### SAVE LOCAL MPLUS FILES #####
#-------------------------------------------------------------------------

write.table(
  dat_mplus_m18_mo_export,
  file = mplus_data_file_m18_mo,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  dec = "."
)


##### SAVE MPLUS VARIABLE NAMES #####

mplus_names_m18_mo <- names(
  dat_mplus_m18_mo_for_export
)

saveRDS(
  mplus_names_m18_mo,
  mplus_names_file_m18_mo
)

writeLines(
  mplus_names_m18_mo,
  mplus_names_text_file_m18_mo
)


##### SAVE R DATASET WITH ORIGINAL MISSING VALUES #####

saveRDS(
  dat_mplus_m18_mo_for_export,
  mplus_rds_file_m18_mo
)


#-------------------------------------------------------------------------
##### CHECK LOCAL MPLUS FILES #####
#-------------------------------------------------------------------------

stopifnot(
  file.exists(mplus_data_file_m18_mo),
  file.exists(mplus_names_file_m18_mo),
  file.exists(mplus_names_text_file_m18_mo),
  file.exists(mplus_rds_file_m18_mo),
  
  identical(
    readRDS(mplus_names_file_m18_mo),
    names(dat_mplus_m18_mo_for_export)
  ),
  
  identical(
    names(
      readRDS(mplus_rds_file_m18_mo)
    ),
    names(dat_mplus_m18_mo_for_export)
  )
)


##### CHECK NUMBER OF EXPORTED COLUMNS #####

number_of_exported_columns_m18_mo <- length(
  strsplit(
    readLines(
      mplus_data_file_m18_mo,
      n = 1,
      warn = FALSE
    ),
    split = "\t",
    fixed = TRUE
  )[[1]]
)

stopifnot(
  number_of_exported_columns_m18_mo ==
    length(mplus_names_m18_mo)
)


#-------------------------------------------------------------------------
##### COPY MPLUS FILES TO SEADRIVE #####
#-------------------------------------------------------------------------

copy_results_m18_mo <- c(
  file.copy(
    from = mplus_data_file_m18_mo,
    to = seadrive_mplus_data_file_m18_mo,
    overwrite = TRUE
  ),
  
  file.copy(
    from = mplus_names_file_m18_mo,
    to = seadrive_mplus_names_file_m18_mo,
    overwrite = TRUE
  ),
  
  file.copy(
    from = mplus_names_text_file_m18_mo,
    to = seadrive_mplus_names_text_file_m18_mo,
    overwrite = TRUE
  ),
  
  file.copy(
    from = mplus_rds_file_m18_mo,
    to = seadrive_mplus_rds_file_m18_mo,
    overwrite = TRUE
  )
)

stopifnot(
  all(copy_results_m18_mo),
  file.exists(seadrive_mplus_data_file_m18_mo),
  file.exists(seadrive_mplus_names_file_m18_mo),
  file.exists(seadrive_mplus_names_text_file_m18_mo),
  file.exists(seadrive_mplus_rds_file_m18_mo)
)


#-------------------------------------------------------------------------
##### MERGE COMBINED CLASSES WITH ORIGINAL EXCEL DATASET #####
#-------------------------------------------------------------------------

dat_original <- read_excel(
  master_excel_file
)


##### RECREATE ESTABLISHED SIC TO SIC_N LOOKUP #####

id_lookup_mo <- dat_original |>
  distinct(
    sic
  ) |>
  arrange(
    sic
  ) |>
  mutate(
    SIC_N = row_number()
  )

stopifnot(
  nrow(id_lookup_mo) == n_distinct(dat_original$sic),
  anyDuplicated(id_lookup_mo$sic) == 0,
  anyDuplicated(id_lookup_mo$SIC_N) == 0,
  min(id_lookup_mo$SIC_N) == 1,
  max(id_lookup_mo$SIC_N) == nrow(id_lookup_mo)
)


##### ADD ORIGINAL SIC TO COMBINED CLASSIFICATION #####

combined_classes_original_id_mo <- combined_classes_mo |>
  left_join(
    id_lookup_mo,
    by = "SIC_N"
  )

stopifnot(
  nrow(combined_classes_original_id_mo) ==
    nrow(combined_classes_mo),
  
  anyDuplicated(combined_classes_original_id_mo$SIC_N) == 0,
  
  !anyNA(combined_classes_original_id_mo$sic)
)


##### PREPARE EXCEL CLASS VARIABLES #####

classes_excel_mo <- combined_classes_original_id_mo |>
  select(
    sic,
    mt_class_mo,
    mt_prob_class1_mo,
    mt_prob_class2_mo,
    mt_prob_class3_mo,
    mt_prob_class4_mo,
    mt_prob_max_mo,
    mt_class_label_mo
  )


##### REMOVE OLD _MO VARIABLES IF CODE IS RERUN #####

dat_original_m18_mo <- dat_original |>
  select(
    -any_of(
      c(
        "mt_class_mo",
        "mt_prob_class1_mo",
        "mt_prob_class2_mo",
        "mt_prob_class3_mo",
        "mt_prob_class4_mo",
        "mt_prob_max_mo",
        "mt_class_label_mo"
      )
    )
  ) |>
  left_join(
    classes_excel_mo,
    by = "sic"
  )


##### CHECK EXCEL MERGE #####

stopifnot(
  nrow(dat_original_m18_mo) == nrow(dat_original),
  identical(
    dat_original_m18_mo$sic,
    dat_original$sic
  ),
  anyDuplicated(dat_original_m18_mo$sic) == 0,
  
  all(
    c(
      "mt_class_mo",
      "mt_prob_class1_mo",
      "mt_prob_class2_mo",
      "mt_prob_class3_mo",
      "mt_prob_class4_mo",
      "mt_prob_max_mo",
      "mt_class_label_mo"
    ) %in% names(dat_original_m18_mo)
  )
)


#-------------------------------------------------------------------------
##### FINAL EXCEL-TO-SOURCE AUDIT #####
#-------------------------------------------------------------------------

excel_merge_audit_mo <- dat_original_m18_mo |>
  select(
    sic,
    mt_class_mo,
    mt_prob_class1_mo,
    mt_prob_class2_mo,
    mt_prob_class3_mo,
    mt_prob_class4_mo,
    mt_prob_max_mo,
    mt_class_label_mo
  ) |>
  left_join(
    classes_excel_mo |>
      rename(
        mt_class_mo_source = mt_class_mo,
        mt_prob_class1_mo_source = mt_prob_class1_mo,
        mt_prob_class2_mo_source = mt_prob_class2_mo,
        mt_prob_class3_mo_source = mt_prob_class3_mo,
        mt_prob_class4_mo_source = mt_prob_class4_mo,
        mt_prob_max_mo_source = mt_prob_max_mo,
        mt_class_label_mo_source = mt_class_label_mo
      ),
    by = "sic"
  )


##### CHECK NON-MISSING CLASSIFIED CASES #####

excel_merge_audit_classified_mo <- excel_merge_audit_mo |>
  filter(
    !is.na(mt_class_mo)
  )

stopifnot(
  all(
    excel_merge_audit_classified_mo$mt_class_mo ==
      excel_merge_audit_classified_mo$mt_class_mo_source
  ),
  
  all(
    abs(
      excel_merge_audit_classified_mo$mt_prob_class1_mo -
        excel_merge_audit_classified_mo$mt_prob_class1_mo_source
    ) < 1e-10
  ),
  
  all(
    abs(
      excel_merge_audit_classified_mo$mt_prob_class2_mo -
        excel_merge_audit_classified_mo$mt_prob_class2_mo_source
    ) < 1e-10
  ),
  
  all(
    abs(
      excel_merge_audit_classified_mo$mt_prob_class3_mo -
        excel_merge_audit_classified_mo$mt_prob_class3_mo_source
    ) < 1e-10
  ),
  
  all(
    abs(
      excel_merge_audit_classified_mo$mt_prob_class4_mo -
        excel_merge_audit_classified_mo$mt_prob_class4_mo_source
    ) < 1e-10
  ),
  
  all(
    abs(
      excel_merge_audit_classified_mo$mt_prob_max_mo -
        excel_merge_audit_classified_mo$mt_prob_max_mo_source
    ) < 1e-10
  ),
  
  all(
    excel_merge_audit_classified_mo$mt_class_label_mo ==
      excel_merge_audit_classified_mo$mt_class_label_mo_source
  )
)

cat(
  "\nExcel merge audit passed.\n"
)


#-------------------------------------------------------------------------
##### SAVE NEW _MO EXCEL DATASET #####
#-------------------------------------------------------------------------

writexl::write_xlsx(
  x = dat_original_m18_mo,
  path = excel_file_m18_mo
)

stopifnot(
  file.exists(excel_file_m18_mo)
)


#-------------------------------------------------------------------------
##### FINAL SUMMARY #####
#-------------------------------------------------------------------------

cat(
  "\n============================================================",
  "\nM18B MALTREATED-ONLY PLUS NON-MALTREATED MERGE COMPLETED",
  "\n============================================================",
  "\n",
  "\nCombined classes:",
  "\n"
)

print(
  combined_class_distribution_mo,
  n = Inf
)

cat(
  "\nLocal Mplus dataset:",
  "\n", mplus_data_file_m18_mo,
  "\n",
  "\nLocal Mplus names file:",
  "\n", mplus_names_file_m18_mo,
  "\n",
  "\nSeaDrive Mplus dataset:",
  "\n", seadrive_mplus_data_file_m18_mo,
  "\n",
  "\nExcel dataset:",
  "\n", excel_file_m18_mo,
  "\n",
  sep = ""
)



#-------------------------------------------------------------------------
##### PLOT M18 THREE-CLASS BURDEN TRAJECTORIES #####
#-------------------------------------------------------------------------
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
##### PLOT M18B MALTREATED-ONLY TRAJECTORIES
##### PLOT M18B INCLUDING NON-MALTREATED REFERENCE GROUP #####
#-------------------------------------------------------------------------

##### DEFINE M18B CLASS-SPECIFIC GROWTH MEANS ####

m18b_growth_means_mo <- tibble(
  class = c(
    "Moderate/increasing burden (73.6%)",
    "Elevated/declining burden (13.9%)",
    "High/rebound burden (12.5%)"
  ),
  bur_i = c(
    1.057,
    2.188,
    3.003
  ),
  bur_s = c(
    0.328,
    -0.440,
    -0.499
  ),
  bur_q = c(
    -0.132,
    0.112,
    0.379
  )
)


##### DEFINE DEVELOPMENTAL PERIOD TABLE ####

developmental_period_table_mo <- tibble(
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


##### CREATE MODEL-ESTIMATED TRAJECTORY DATA ####

m18b_plot_data_mo <- merge(
  m18b_growth_means_mo,
  developmental_period_table_mo,
  by = NULL
) |>
  as_tibble() |>
  mutate(
    estimated_burden = bur_i +
      bur_s * time_score +
      bur_q * time_score^2,
    class = factor(
      class,
      levels = m18b_growth_means_mo$class
    )
  ) |>
  arrange(
    class,
    midpoint_age
  )


##### CALCULATE OBSERVED PERIOD-SPECIFIC MEANS FOR NON-MALTREATED GROUP ####

non_maltreated_plot_data_mo <- dat_mplus |>
  filter(
    !is.na(mal_all),
    mal_all == 0
  ) |>
  select(
    all_of(burden_variables_mo)
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "burden_variable",
    values_to = "burden"
  ) |>
  group_by(
    burden_variable
  ) |>
  summarise(
    estimated_burden = mean(
      burden,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    period = toupper(
      sub(
        "^zind_",
        "",
        burden_variable
      )
    )
  ) |>
  left_join(
    developmental_period_table_mo,
    by = "period"
  ) |>
  mutate(
    class = non_maltreated_label_mo
  ) |>
  select(
    class,
    period,
    midpoint_age,
    time_score,
    estimated_burden
  ) |>
  arrange(
    midpoint_age
  )


##### COMBINE NON-MALTREATED AND M18B TRAJECTORIES ####

class_levels_mo <- c(
  non_maltreated_label_mo,
  m18b_growth_means_mo$class
)

m18b_plot_data_with_nm_mo <- bind_rows(
  non_maltreated_plot_data_mo,
  m18b_plot_data_mo |>
    select(
      class,
      period,
      midpoint_age,
      time_score,
      estimated_burden
    )
) |>
  mutate(
    class = factor(
      class,
      levels = class_levels_mo
    )
  ) |>
  arrange(
    class,
    midpoint_age
  )


##### INSPECT PLOTTED VALUES ####

m18b_plot_data_with_nm_mo |>
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

m18b_trajectory_plot_mo <- ggplot(
  m18b_plot_data_with_nm_mo,
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
    linewidth = 1.2,
    na.rm = TRUE
  ) +
  geom_point(
    size = 2.8,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    breaks = developmental_period_table_mo$midpoint_age,
    labels = developmental_period_table_mo$period
  ) +
  scale_color_manual(
    values = c(
      "#666666",
      "#3366A3",
      "#D68C2F",
      "#A33A3A"
    )
  ) +
  labs(
    x = "Developmental period",
    y = "Standardised maltreatment burden",
    color = NULL,
    caption = stringr::str_wrap(
      paste0(
        "Note. Maltreatment burden was based on period-specific standardised ",
        "indicators of subtype count, frequency, and severity. Values below zero ",
        "indicate burden below the standardisation-sample mean and do not represent ",
        "negative maltreatment exposure. Trajectories for the maltreated classes ",
        "are model-estimated. The non-maltreated group was not included in the ",
        "latent trajectory-class estimation and is displayed using observed ",
        "period-specific mean burden scores."
      ),
      width = 200
    ))+
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
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 8.5,
      lineheight = 1.1,
      margin = margin(
        t = 10
      )
    ),
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )


##### DISPLAY TRAJECTORY PLOT ####

m18b_trajectory_plot_mo


##### SAVE TRAJECTORY PLOT ####

m18b_plot_file_mo <- file.path(
  mplus_results_dir_mal,
  "18b_mt_burden_quadratic_3class_trajectories_with_nonmaltreated_mo.png"
)

ggsave(
  filename = m18b_plot_file_mo,
  plot = m18b_trajectory_plot_mo,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

cat(
  "Saved: ",
  m18b_plot_file_mo,
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
  mplus_archive_root,
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

