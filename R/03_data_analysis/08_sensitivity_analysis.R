#-------------------------------------------------------------------------
##### COMPLETE SENSITIVITY ANALYSIS: DIAGNOSIS MULTIGROUP LCS #####
#-------------------------------------------------------------------------

##### PROJECT AND STANDARD SETUP #####

project_dir <-
  "C:/Users/keil/Documents/main_outcome_amis2"

setup_file <- file.path(
  project_dir,
  "R",
  "03_data_analysis",
  "00_setup_standardized.R"
)

helper_file <- file.path(
  project_dir,
  "R",
  "03_data_analysis",
  "00_helper_functions.R"
)

stopifnot(
  file.exists(setup_file),
  file.exists(helper_file)
)

source(setup_file)
source(helper_file)

check_packages(
  c(
    "MplusAutomation",
    "officer",
    "flextable"
  )
)


##### USER SETTINGS #####

data_prep_dir <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/",
  "Meine Bibliotheken/MAIN OUTCOME/02_data/02_data_Prep"
)

mplus_input_dir <-
  "C:/MPLUS/Inputs"

# TRUE: create inputs and run them in Mplus.
# FALSE: create inputs only.
run_mplus_models <- TRUE

# Use "S20" here if only the unadjusted M20 counterpart is wanted.
diag_mg_models_to_create <- c(
  "S20",
  "S21",
  "S22"
)

dir.create(
  mplus_input_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


##### SENSITIVITY: MULTIGROUP LCS BY AMIS-I DIAGNOSIS #####

##### LOAD THE EXACT FINAL M18 + SES + MO MPLUS FILES #####

# Do not use the generic mplus_* file objects created by the setup here.
# They can point to an earlier dataset without mo_cls and sesausb.

diag_mg_data_filename <-
  "AMIS_mplus_dataset_m18_ses_mo.dat"

diag_mg_names_filename <-
  "AMIS_mplus_names_m18_ses_mo.rds"

diag_mg_local_data_file <- file.path(
  mplus_input_dir,
  diag_mg_data_filename
)

diag_mg_local_names_file <- file.path(
  mplus_input_dir,
  diag_mg_names_filename
)

# Recover the exact files from SeaDrive only if they are not already local.
if (
  !file.exists(diag_mg_local_data_file) ||
  !file.exists(diag_mg_local_names_file)
) {
  
  diag_mg_seadrive_prep_dir <- data_prep_dir
  
  diag_mg_source_files <- list.files(
    path = diag_mg_seadrive_prep_dir,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )
  
  diag_mg_source_data_file <- diag_mg_source_files[
    tolower(basename(diag_mg_source_files)) ==
      tolower(diag_mg_data_filename)
  ]
  
  diag_mg_source_names_file <- diag_mg_source_files[
    tolower(basename(diag_mg_source_files)) ==
      tolower(diag_mg_names_filename)
  ]
  
  if (
    length(diag_mg_source_data_file) != 1L ||
    length(diag_mg_source_names_file) != 1L
  ) {
    stop(
      "The exact final M18+SES+MO .dat and names files could not ",
      "be identified uniquely in SeaDrive."
    )
  }
  
  if (!file.exists(diag_mg_local_data_file)) {
    stopifnot(
      file.copy(
        from = diag_mg_source_data_file,
        to = diag_mg_local_data_file,
        overwrite = TRUE
      )
    )
  }
  
  if (!file.exists(diag_mg_local_names_file)) {
    stopifnot(
      file.copy(
        from = diag_mg_source_names_file,
        to = diag_mg_local_names_file,
        overwrite = TRUE
      )
    )
  }
}

# Override the generic setup objects with the exact matched file pair.
mplus_data_file <- diag_mg_local_data_file
mplus_names_file_local <- diag_mg_local_names_file

mplus_names <- readRDS(
  mplus_names_file_local
)

mplus_fields <- count.fields(
  mplus_data_file,
  sep = "",
  blank.lines.skip = TRUE
)

names_syntax <- paste(
  wrap_mplus_names(
    mplus_names,
    max_width = 88
  ),
  collapse = "\n"
)

stopifnot(
  is.character(mplus_names),
  length(mplus_names) > 0L,
  !anyNA(mplus_names),
  length(mplus_fields) > 0L,
  all(mplus_fields == length(mplus_names))
)

cat(
  "Diagnosis-multigroup data:",
  mplus_data_file,
  "\n"
)

cat(
  "Diagnosis-multigroup names:",
  mplus_names_file_local,
  "\n"
)


##### LOAD THE FINAL M7 MEASUREMENT-MODEL DEFINITION #####

if (!exists("sdq_standard_measurement_model", inherits = TRUE)) {
  
  diag_mg_measurement_source_file <- file.path(
    project_dir,
    "R",
    "03_data_analysis",
    "01_create_mplus_inputs_final.R"
  )
  
  if (!file.exists(diag_mg_measurement_source_file)) {
    stop(
      "The script containing sdq_standard_measurement_model ",
      "does not exist: ",
      diag_mg_measurement_source_file
    )
  }
  
  diag_mg_source_expressions <- parse(
    file = diag_mg_measurement_source_file,
    keep.source = FALSE
  )
  
  diag_mg_measurement_assignment <- vapply(
    diag_mg_source_expressions,
    function(x) {
      is.call(x) &&
        length(x) >= 3L &&
        identical(x[[1L]], as.name("<-")) &&
        identical(
          x[[2L]],
          as.name("sdq_standard_measurement_model")
        )
    },
    logical(1)
  )
  
  if (sum(diag_mg_measurement_assignment) != 1L) {
    stop(
      "The definition of sdq_standard_measurement_model ",
      "was not found exactly once in ",
      diag_mg_measurement_source_file
    )
  }
  
  diag_mg_measurement_expression <-
    diag_mg_source_expressions[[
      which(diag_mg_measurement_assignment)
    ]]
  
  # Evaluate only the right-hand side of this single assignment.
  # The remainder of 01_create_mplus_inputs_final.R is not executed.
  sdq_standard_measurement_model <- eval(
    diag_mg_measurement_expression[[3L]],
    envir = .GlobalEnv
  )
}

stopifnot(
  is.character(sdq_standard_measurement_model),
  length(sdq_standard_measurement_model) == 1L,
  nzchar(sdq_standard_measurement_model)
)


##### DEFINE AND VALIDATE VARIABLES #####

diag_mg_indicator_variables <- c(
  "hyp_b2", "hyp_k2", "hyp_p2",
  "hyp_b5", "hyp_k5", "hyp_p5",
  "con_b2", "con_k2", "con_p2",
  "con_b5", "con_k5", "con_p5",
  "emo_b2", "emo_k2", "emo_p2",
  "emo_b5", "emo_k5", "emo_p5"
)

diag_mg_class_dummies <- c(
  "c2",
  "c3",
  "c4"
)

diag_mg_required_variables <- c(
  "SIC_N",
  "stat_t5",
  "mo_cls",
  "diag_vor",
  "aget2",
  "sext5",
  "sesausb",
  diag_mg_indicator_variables
)

diag_mg_missing_variables <- diag_mg_required_variables[
  !toupper(diag_mg_required_variables) %in%
    toupper(mplus_names)
]

if (length(diag_mg_missing_variables) > 0L) {
  stop(
    "The following variables are missing from the Mplus dataset:\n",
    paste(
      diag_mg_missing_variables,
      collapse = "\n"
    )
  )
}

if (
  any(
    toupper(diag_mg_class_dummies) %in%
    toupper(mplus_names)
  )
) {
  stop(
    "The dummy names c3 and c4 must not already occur ",
    "in the Mplus NAMES list."
  )
}


##### AUDIT THE ACTUAL MULTIGROUP SAMPLE #####

diag_mg_audit_data <- utils::read.table(
  file = mplus_data_file,
  header = FALSE,
  sep = "",
  na.strings = "-999",
  col.names = mplus_names,
  check.names = FALSE,
  blank.lines.skip = TRUE,
  stringsAsFactors = FALSE
)

# Be robust to numeric spellings such as -999.000 in the .dat file.
diag_mg_audit_data[] <- lapply(
  diag_mg_audit_data,
  function(x) {
    if (is.numeric(x)) {
      x[!is.na(x) & x == -999] <- NA
    }
    x
  }
)

stopifnot(
  ncol(diag_mg_audit_data) == length(mplus_names),
  nrow(diag_mg_audit_data) == length(mplus_fields)
)

diag_mg_invalid_group_values <- setdiff(
  unique(
    stats::na.omit(
      diag_mg_audit_data$diag_vor
    )
  ),
  c(0, 1)
)

if (length(diag_mg_invalid_group_values) > 0L) {
  stop(
    "diag_vor contains values other than 0, 1, or missing: ",
    paste(
      diag_mg_invalid_group_values,
      collapse = ", "
    )
  )
}

diag_mg_analysis_rows <- with(
  diag_mg_audit_data,
  !is.na(stat_t5) &
    stat_t5 != 0 &
    !is.na(mo_cls) &
    mo_cls >= 1 &
    mo_cls <= 4 &
    !is.na(diag_vor) &
    diag_vor %in% c(0, 1)
)

diag_mg_sample <- diag_mg_audit_data[
  diag_mg_analysis_rows,
  c("SIC_N", "mo_cls", "diag_vor"),
  drop = FALSE
]

stopifnot(
  nrow(diag_mg_sample) > 0L,
  !anyNA(diag_mg_sample$SIC_N),
  anyDuplicated(diag_mg_sample$SIC_N) == 0L
)

diag_mg_cell_table <- with(
  diag_mg_sample,
  table(
    mo_cls = factor(
      mo_cls,
      levels = 1:4
    ),
    diag_vor = factor(
      diag_vor,
      levels = 0:1
    )
  )
)

cat(
  "Multigroup analysis sample after stat_t5, class, and diagnosis filters:",
  nrow(diag_mg_sample),
  "\n"
)

print(
  addmargins(
    diag_mg_cell_table
  )
)

if (any(diag_mg_cell_table < 10L)) {
  warning(
    "At least one mo_cls x diag_vor cell contains fewer than 10 cases. ",
    "Treat the corresponding interaction estimates as exploratory."
  )
}


##### DEFINE COMMON MULTIGROUP SYNTAX #####

diag_mg_class_define_syntax <- "
  c2 = 0;
  c3 = 0;
  c4 = 0;

  IF (mo_cls EQ 2) THEN c2 = 1;
  IF (mo_cls EQ 3) THEN c3 = 1;
  IF (mo_cls EQ 4) THEN c4 = 1;
"

diag_mg_group_model_syntax <- "
MODEL nodx:

  ! Class 1 is the reference class without diagnosis

  [EXT2@0];
  [EMO2@0];

  [d_ext] (mdx0);
  [d_emo] (mdm0);

  EXT2 ON c2 (e2c20);
  EXT2 ON c3 (e2c30);
  EXT2 ON c4 (e2c40);

  EMO2 ON c2 (m2c20);
  EMO2 ON c3 (m2c30);
  EMO2 ON c4 (m2c40);

  d_ext ON c2 (dxc20);
  d_ext ON c3 (dxc30);
  d_ext ON c4 (dxc40);

  d_emo ON c2 (dmc20);
  d_emo ON c3 (dmc30);
  d_emo ON c4 (dmc40);

MODEL dx:

  ! Class 1 is the reference class with diagnosis

  [EXT2] (be11);
  [EMO2] (bm11);

  [d_ext] (mdx1);
  [d_emo] (mdm1);

  EXT2 ON c2 (e2c21);
  EXT2 ON c3 (e2c31);
  EXT2 ON c4 (e2c41);

  EMO2 ON c2 (m2c21);
  EMO2 ON c3 (m2c31);
  EMO2 ON c4 (m2c41);

  d_ext ON c2 (dxc21);
  d_ext ON c3 (dxc31);
  d_ext ON c4 (dxc41);

  d_emo ON c2 (dmc21);
  d_emo ON c3 (dmc31);
  d_emo ON c4 (dmc41);
"

diag_mg_constraint_syntax <- "
MODEL CONSTRAINT:

  NEW(
    dex_1n dex_2n dex_3n dex_4n
    dex_1d dex_2d dex_3d dex_4d
    dem_1n dem_2n dem_3n dem_4n
    dem_1d dem_2d dem_3d dem_4d

    gdx_1 gdx_2 gdx_3 gdx_4
    gdm_1 gdm_2 gdm_3 gdm_4

    intx_2 intx_3 intx_4
    intm_2 intm_3 intm_4
  );

  ! Class-specific externalizing change

  dex_1n = mdx0;
  dex_2n = mdx0 + dxc20;
  dex_3n = mdx0 + dxc30;
  dex_4n = mdx0 + dxc40;

  dex_1d = mdx1;
  dex_2d = mdx1 + dxc21;
  dex_3d = mdx1 + dxc31;
  dex_4d = mdx1 + dxc41;

  ! Class-specific emotional-problems change

  dem_1n = mdm0;
  dem_2n = mdm0 + dmc20;
  dem_3n = mdm0 + dmc30;
  dem_4n = mdm0 + dmc40;

  dem_1d = mdm1;
  dem_2d = mdm1 + dmc21;
  dem_3d = mdm1 + dmc31;
  dem_4d = mdm1 + dmc41;

  ! Diagnosis-group differences within classes

  gdx_1 = dex_1d - dex_1n;
  gdx_2 = dex_2d - dex_2n;
  gdx_3 = dex_3d - dex_3n;
  gdx_4 = dex_4d - dex_4n;

  gdm_1 = dem_1d - dem_1n;
  gdm_2 = dem_2d - dem_2n;
  gdm_3 = dem_3d - dem_3n;
  gdm_4 = dem_4d - dem_4n;

  ! Class x diagnosis interactions relative to class 1

  intx_2 = dxc21 - dxc20;
  intx_3 = dxc31 - dxc30;
  intx_4 = dxc41 - dxc40;

  intm_2 = dmc21 - dmc20;
  intm_3 = dmc31 - dmc30;
  intm_4 = dmc41 - dmc40;

MODEL TEST:

  0 = dxc21 - dxc20;
  0 = dxc31 - dxc30;
  0 = dxc41 - dxc40;

  0 = dmc21 - dmc20;
  0 = dmc31 - dmc30;
  0 = dmc41 - dmc40;
"


##### DEFINE REUSABLE DIAGNOSIS-MULTIGROUP INPUT FUNCTION #####

create_diag_mg_lcs_input <- function(
    model_id,
    model_name,
    input_filename,
    additional_predictors = character(),
    center_predictors = character()
) {
  
  stopifnot(
    length(model_id) == 1L,
    nzchar(model_id),
    length(model_name) == 1L,
    nzchar(model_name),
    length(input_filename) == 1L,
    nzchar(input_filename)
  )
  
  additional_predictors <- unique(
    additional_predictors
  )
  
  center_predictors <- unique(
    center_predictors
  )
  
  predictors_not_in_model <- center_predictors[
    !toupper(center_predictors) %in%
      toupper(additional_predictors)
  ]
  
  if (length(predictors_not_in_model) > 0L) {
    stop(
      "Variables requested for centering are not included ",
      "as additional predictors:\n",
      paste(
        predictors_not_in_model,
        collapse = "\n"
      )
    )
  }
  
  missing_predictors <- additional_predictors[
    !toupper(additional_predictors) %in%
      toupper(mplus_names)
  ]
  
  if (length(missing_predictors) > 0L) {
    stop(
      "Additional predictors are missing from the Mplus dataset:\n",
      paste(
        missing_predictors,
        collapse = "\n"
      )
    )
  }
  
  title_syntax <- paste0(
    paste(
      strwrap(
        paste0(
          model_id,
          ": ",
          model_name
        ),
        width = 84,
        indent = 2,
        exdent = 2
      ),
      collapse = "\n"
    ),
    ";"
  )
  
  usevariables <- c(
    diag_mg_indicator_variables,
    additional_predictors,
    diag_mg_class_dummies
  )
  
  usevariables_syntax <- paste(
    wrap_mplus_names(
      usevariables,
      max_width = 88
    ),
    collapse = "\n"
  )
  
  center_predictor_syntax <- if (
    length(center_predictors) == 0L
  ) {
    ""
  } else {
    paste0(
      "\n  CENTER\n",
      paste(
        wrap_mplus_names(
          center_predictors,
          max_width = 88,
          indent = "    "
        ),
        collapse = "\n"
      ),
      "\n    (GRANDMEAN);\n"
    )
  }
  
  additional_predictor_syntax <- if (
    length(additional_predictors) == 0L
  ) {
    ""
  } else {
    paste(
      vapply(
        seq_along(additional_predictors),
        function(i) {
          predictor <- additional_predictors[[i]]
          
          paste0(
            "  EXT2 ON ", predictor, " (bxe", i, ");\n",
            "  EMO2 ON ", predictor, " (bme", i, ");\n",
            "  d_ext ON ", predictor, " (bdx", i, ");\n",
            "  d_emo ON ", predictor, " (bdm", i, ");"
          )
        },
        character(1)
      ),
      collapse = "\n\n"
    )
  }
  
  input_syntax <- paste0(
    "TITLE:\n",
    title_syntax,
    "\n\n",
    "DATA:\n",
    "  FILE = ",
    basename(mplus_data_file),
    ";\n\n",
    "VARIABLE:\n",
    "  NAMES =\n",
    names_syntax,
    "\n    ;\n\n",
    "  USEVARIABLES =\n",
    usevariables_syntax,
    "\n    ;\n\n",
    "  USEOBSERVATIONS =\n",
    "    (stat_t5 NE 0) AND\n",
    "    (mo_cls GE 1) AND\n",
    "    (mo_cls LE 4);\n\n",
    "  IDVARIABLE = SIC_N;\n\n",
    "  GROUPING = diag_vor\n",
    "    (0 = nodx 1 = dx);\n\n",
    "  MISSING = ALL (-999);\n\n",
    "DEFINE:\n",
    diag_mg_class_define_syntax,
    center_predictor_syntax,
    "\n",
    "ANALYSIS:\n",
    "  ESTIMATOR = MLR;\n",
    "  COVERAGE = 0.01;\n\n",
    "MODEL:\n",
    sdq_standard_measurement_model,
    "\n\n",
    "  ! Classical two-wave latent change score model\n\n",
    "  d_ext BY EXT5@1;\n",
    "  EXT5 ON EXT2@1;\n",
    "  EXT5@0;\n",
    "  [EXT5@0];\n\n",
    "  d_emo BY EMO5@1;\n",
    "  EMO5 ON EMO2@1;\n",
    "  EMO5@0;\n",
    "  [EMO5@0];\n\n",
    "  EXT2;\n",
    "  EMO2;\n",
    "  d_ext;\n",
    "  d_emo;\n\n",
    "  EXT2 WITH EMO2;\n",
    "  EXT2 WITH d_ext;\n",
    "  EXT2 WITH d_emo;\n",
    "  EMO2 WITH d_ext;\n",
    "  EMO2 WITH d_emo;\n",
    "  d_ext WITH d_emo;\n\n",
    "  ! Class effects declared for all groups\n\n",
    "  EXT2 ON c2 c3 c4;\n",
    "  EMO2 ON c2 c3 c4;\n",
    "  d_ext ON c2 c3 c4;\n",
    "  d_emo ON c2 c3 c4;\n\n",
    if (nzchar(additional_predictor_syntax)) {
      paste0(
        "  ! Covariate effects constrained equal across diagnosis groups\n\n",
        additional_predictor_syntax,
        "\n\n"
      )
    } else {
      ""
    },
    diag_mg_group_model_syntax,
    "\n",
    diag_mg_constraint_syntax,
    "\n",
    "OUTPUT:\n",
    "  SAMPSTAT\n",
    "  STANDARDIZED\n",
    "  CINTERVAL\n",
    "  TECH1\n",
    "  TECH4;\n"
  )
  
  local_input_file <- file.path(
    mplus_input_dir,
    input_filename
  )
  
  dir.create(
    dirname(local_input_file),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  writeLines(
    input_syntax,
    con = local_input_file,
    useBytes = TRUE
  )
  
  stopifnot(
    file.exists(local_input_file),
    file.info(local_input_file)$size > 0L
  )
  
  local_input_file
}


##### DEFINE SENSITIVITY-MODEL SETTINGS #####

diag_mg_model_settings <- list(
  
  S20 = list(
    model_id = "S20",
    model_name = paste(
      "Maltreatment trajectories predicting latent change",
      "by AMIS-I diagnosis group"
    ),
    input_filename =
      "S20_sdq_lcs_mg_diag_maltreated.inp",
    additional_predictors =
      character(),
    center_predictors =
      character()
  ),
  
  S21 = list(
    model_id = "S21",
    model_name = paste(
      "Maltreatment trajectories predicting latent change",
      "by AMIS-I diagnosis group adjusted for age and sex"
    ),
    input_filename =
      "S21_sdq_lcs_mg_diag_age_sex.inp",
    additional_predictors = c(
      "aget2",
      "sext5"
    ),
    center_predictors = c(
      "aget2",
      "sext5"
    )
  ),
  
  S22 = list(
    model_id = "S22",
    model_name = paste(
      "Maltreatment trajectories predicting latent change",
      "by AMIS-I diagnosis group adjusted for",
      "age, sex, and maternal education"
    ),
    input_filename =
      "S22_sdq_lcs_mg_diag_sociodemographic.inp",
    additional_predictors = c(
      "aget2",
      "sext5",
      "sesausb"
    ),
    center_predictors = c(
      "aget2",
      "sext5",
      "sesausb"
    )
  )
)

if (
  length(diag_mg_models_to_create) == 0L ||
  anyDuplicated(diag_mg_models_to_create) > 0L ||
  !all(
    diag_mg_models_to_create %in%
    names(diag_mg_model_settings)
  )
) {
  stop(
    "diag_mg_models_to_create must contain unique model IDs from: ",
    paste(
      names(diag_mg_model_settings),
      collapse = ", "
    )
  )
}

diag_mg_model_settings <- diag_mg_model_settings[
  diag_mg_models_to_create
]


##### CREATE SENSITIVITY INPUTS WITHOUT UPDATING THE MODEL CATALOG #####

diag_mg_model_files <- lapply(
  diag_mg_model_settings,
  function(settings) {
    do.call(
      create_diag_mg_lcs_input,
      settings
    )
  }
)

diag_mg_input_files <- unname(
  vapply(
    diag_mg_model_files,
    identity,
    character(1)
  )
)

stopifnot(
  all(file.exists(diag_mg_input_files))
)

cat(
  "Created multigroup sensitivity inputs:\n",
  paste(
    diag_mg_input_files,
    collapse = "\n"
  ),
  "\n"
)


##### RUN SENSITIVITY MODELS #####

if (isTRUE(run_mplus_models)) {
  
  if (
    MplusAutomation::mplusAvailable(
      silent = FALSE
    ) != 0
  ) {
    stop(
      "Mplus could not be detected by MplusAutomation."
    )
  }
  
  MplusAutomation::runModels(
    target = diag_mg_input_files,
    replaceOutfile = "always",
    showOutput = FALSE,
    logFile = NULL,
    quiet = FALSE
  )
  
} else {
  
  message(
    "The multigroup sensitivity inputs were created, ",
    "but Mplus execution was skipped."
  )
}

with(
  subset(
    diag_mg_audit_data,
    stat_t5 != 0 &
      mo_cls %in% 1:4 &
      !is.na(diag_vor)
  ),
  addmargins(
    table(mo_cls, diag_vor)
  )
)



