#-------------------------------------------------------------------------
##### SETUP #####
#-------------------------------------------------------------------------

source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R")

check_packages(c("MplusAutomation", "officer", "flextable"))

##### COPY AND LOAD MPLUS DATA INFORMATION #####

copy_to_mplus(seadrive_mplus_data_file)
copy_to_mplus(seadrive_mplus_names_file)

mplus_names  <- readRDS(mplus_names_file_local)
mplus_fields <- count.fields(mplus_data_file, sep = "", blank.lines.skip = TRUE)

stopifnot(
  is.character(mplus_names),
  length(mplus_names) > 0L,
  !anyNA(mplus_names),
  all(nchar(mplus_names) <= 8L),
  all(grepl("^[A-Za-z][A-Za-z0-9_]*$", mplus_names)),
  anyDuplicated(toupper(mplus_names)) == 0L,
  length(mplus_fields) > 0L,
  all(mplus_fields == length(mplus_names))
)

names_syntax <- paste(
  wrap_mplus_names(mplus_names, max_width = 88),
  collapse = "\n"
)

stopifnot(
  all(nchar(strsplit(names_syntax, "\n", fixed = TRUE)[[1L]]) <= 88L)
)

#-------------------------------------------------------------------------
##### MODEL SPECIFICATIONS #####
#-------------------------------------------------------------------------
##### CREATE M1_CONFIGURAL MODEL ####
configural_input <- paste0(
  "TITLE:
  M1: SDQ CONFIGURAL MEASUREMENT MODEL T2-T5;

DATA:
  FILE = AMIS_mplus_dataset.dat;

VARIABLE:
  NAMES =
", names_syntax, "
    ;

  USEVARIABLES =
    hyp_b2 hyp_k2 hyp_p2 hyp_b5 hyp_k5 hyp_p5
    con_b2 con_k2 con_p2 con_b5 con_k5 con_p5
    emo_b2 emo_k2 emo_p2 emo_b5 emo_k5 emo_p5;

  USEOBSERVATIONS = stat_t5 NE 0;
  IDVARIABLE = SIC_N;
  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Configural CFA: freely estimated loadings and no residual correlations

  EXT2 BY hyp_b2@1 hyp_k2 hyp_p2 con_b2 con_k2 con_p2;
  EXT5 BY hyp_b5@1 hyp_k5 hyp_p5 con_b5 con_k5 con_p5;
  EMO2 BY emo_b2@1 emo_k2 emo_p2;
  EMO5 BY emo_b5@1 emo_k5 emo_p5;

  EXT2 WITH EMO2 EXT5 EMO5;
  EXT5 WITH EMO2 EMO5;
  EMO2 WITH EMO5;

OUTPUT:
  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(10);
"
)

input_file <- write_mplus_input(
  syntax     = configural_input,
  filename   = "01_sdq_configural.inp",
  github_dir = github_invariance_dir,
  documentation = list(
    model_id             = "M1",
    model_name           = "Configural measurement model",
    model_family         = "Measurement invariance",
    script               = "01_create_mplus_inputs_invariance_lcs.R",
    sample               = "stat_t5 NE 0",
    estimator            = "MLR",
    specification        = paste(
      "Four correlated factors (EXT2, EXT5, EMO2, EMO5);",
      "marker-variable scaling; remaining loadings freely estimated across time"
    ),
    residual_covariances = "None",
    covariates           = "None",
    model_role           = "Configural reference model",
    notes                = ""
  )
)

##### CREATE M2_LONGITUDINAL RESIDUALS INPUT ####

longitudinal_resid_input <- paste0(
  "TITLE:
  M2: SDQ CONFIGURAL MODEL WITH LONGITUDINAL RESIDUAL CORRELATIONS;

DATA:
  FILE = AMIS_mplus_dataset.dat;

VARIABLE:
  NAMES =
", names_syntax, "
    ;

  USEVARIABLES =
    hyp_b2 hyp_k2 hyp_p2 hyp_b5 hyp_k5 hyp_p5
    con_b2 con_k2 con_p2 con_b5 con_k5 con_p5
    emo_b2 emo_k2 emo_p2 emo_b5 emo_k5 emo_p5;

  USEOBSERVATIONS = stat_t5 NE 0;
  IDVARIABLE = SIC_N;
  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Configural CFA with correlated residuals of identical indicators over time

  EXT2 BY hyp_b2@1 hyp_k2 hyp_p2 con_b2 con_k2 con_p2;
  EXT5 BY hyp_b5@1 hyp_k5 hyp_p5 con_b5 con_k5 con_p5;
  EMO2 BY emo_b2@1 emo_k2 emo_p2;
  EMO5 BY emo_b5@1 emo_k5 emo_p5;

  EXT2 WITH EMO2 EXT5 EMO5;
  EXT5 WITH EMO2 EMO5;
  EMO2 WITH EMO5;

  hyp_b2 WITH hyp_b5;
  hyp_k2 WITH hyp_k5;
  hyp_p2 WITH hyp_p5;
  con_b2 WITH con_b5;
  con_k2 WITH con_k5;
  con_p2 WITH con_p5;
  emo_b2 WITH emo_b5;
  emo_k2 WITH emo_k5;
  emo_p2 WITH emo_p5;

OUTPUT:
  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(10);
"
)

input_file <- write_mplus_input(
  syntax     = longitudinal_resid_input,
  filename   = "02_sdq_longitudinal_resid.inp",
  github_dir = github_measurement_dir,
  documentation = list(
    model_id             = "M2",
    model_name           = "Configural model with longitudinal residual correlations",
    model_family         = "Measurement model development",
    script               = "01_create_mplus_inputs_invariance_lcs.R",
    sample               = "stat_t5 NE 0",
    estimator            = "MLR",
    specification        = paste(
      "Four correlated factors (EXT2, EXT5, EMO2, EMO5);",
      "marker-variable scaling; loadings freely estimated across time"
    ),
    residual_covariances = "Identical indicators correlated between T2 and T5",
    covariates           = "None",
    model_role           = "Comparison with M1; test longitudinal residual correlations",
    notes                = ""
  )
)

##### CREATE M3 METRIC INVARIANCE MODEL ####

metric_input <- paste0(
  "TITLE:
  M3: SDQ METRIC INVARIANCE MODEL T2-T5;

DATA:
  FILE = AMIS_mplus_dataset.dat;

VARIABLE:
  NAMES =
", names_syntax, "
    ;

  USEVARIABLES =
    hyp_b2 hyp_k2 hyp_p2 hyp_b5 hyp_k5 hyp_p5
    con_b2 con_k2 con_p2 con_b5 con_k5 con_p5
    emo_b2 emo_k2 emo_p2 emo_b5 emo_k5 emo_p5;

  USEOBSERVATIONS = stat_t5 NE 0;
  IDVARIABLE = SIC_N;
  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Equal factor loadings across T2 and T5

  EXT2 BY
    hyp_b2@1 hyp_k2 (ex2) hyp_p2 (ex3)
    con_b2 (ex4) con_k2 (ex5) con_p2 (ex6);

  EXT5 BY
    hyp_b5@1 hyp_k5 (ex2) hyp_p5 (ex3)
    con_b5 (ex4) con_k5 (ex5) con_p5 (ex6);

  EMO2 BY emo_b2@1 emo_k2 (em2) emo_p2 (em3);
  EMO5 BY emo_b5@1 emo_k5 (em2) emo_p5 (em3);

  EXT2 WITH EMO2 EXT5 EMO5;
  EXT5 WITH EMO2 EMO5;
  EMO2 WITH EMO5;

  ! Correlated residuals of identical indicators over time

  hyp_b2 WITH hyp_b5;
  hyp_k2 WITH hyp_k5;
  hyp_p2 WITH hyp_p5;
  con_b2 WITH con_b5;
  con_k2 WITH con_k5;
  con_p2 WITH con_p5;
  emo_b2 WITH emo_b5;
  emo_k2 WITH emo_k5;
  emo_p2 WITH emo_p5;

OUTPUT:
  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(10);
"
)

input_file <- write_mplus_input(
  syntax     = metric_input,
  filename   = "03_sdq_metric_invariance.inp",
  github_dir = github_invariance_dir,
  documentation = list(
    model_id             = "M3",
    model_name           = "Metric invariance model",
    model_family         = "Measurement invariance",
    script               = "01_create_mplus_inputs_invariance_lcs.R",
    sample               = "stat_t5 NE 0",
    estimator            = "MLR",
    specification        = paste(
      "Four correlated factors (EXT2, EXT5, EMO2, EMO5);",
      "marker-variable scaling; corresponding factor loadings constrained",
      "equal across T2 and T5"
    ),
    residual_covariances = "Identical indicators correlated between T2 and T5",
    covariates           = "None",
    model_role           = "Test of metric invariance relative to M2",
    notes                = ""
  )
)

##### CREATE M4 WITHIN-INFORMANT CROSS-SCALE MODEL ####

within_informant_input <- paste0(
  "TITLE:
  M4: SDQ METRIC MODEL WITH WITHIN-INFORMANT RESIDUAL COVARIANCES;

DATA:
  FILE = AMIS_mplus_dataset.dat;

VARIABLE:
  NAMES =
", names_syntax, "
    ;

  USEVARIABLES =
    hyp_b2 hyp_k2 hyp_p2 hyp_b5 hyp_k5 hyp_p5
    con_b2 con_k2 con_p2 con_b5 con_k5 con_p5
    emo_b2 emo_k2 emo_p2 emo_b5 emo_k5 emo_p5;

  USEOBSERVATIONS = stat_t5 NE 0;
  IDVARIABLE = SIC_N;
  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Equal factor loadings across T2 and T5

  EXT2 BY
    hyp_b2@1 hyp_k2 (ex2) hyp_p2 (ex3)
    con_b2 (ex4) con_k2 (ex5) con_p2 (ex6);

  EXT5 BY
    hyp_b5@1 hyp_k5 (ex2) hyp_p5 (ex3)
    con_b5 (ex4) con_k5 (ex5) con_p5 (ex6);

  EMO2 BY emo_b2@1 emo_k2 (em2) emo_p2 (em3);
  EMO5 BY emo_b5@1 emo_k5 (em2) emo_p5 (em3);

  EXT2 WITH EMO2 EXT5 EMO5;
  EXT5 WITH EMO2 EMO5;
  EMO2 WITH EMO5;

  ! Residual covariances of identical indicators over time

  hyp_b2 WITH hyp_b5;
  hyp_k2 WITH hyp_k5;
  hyp_p2 WITH hyp_p5;
  con_b2 WITH con_b5;
  con_k2 WITH con_k5;
  con_p2 WITH con_p5;
  emo_b2 WITH emo_b5;
  emo_k2 WITH emo_k5;
  emo_p2 WITH emo_p5;

  ! Within-informant residual covariances,
  ! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc) emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);
  hyp_b5 WITH con_b5 (w_b_hc) emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc) emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);
  hyp_k5 WITH con_k5 (w_k_hc) emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc) emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);
  hyp_p5 WITH con_p5 (w_p_hc) emo_p5 (w_p_he);
  con_p5 WITH emo_p5 (w_p_ce);

OUTPUT:
  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(10);
"
)

input_file <- write_mplus_input(
  syntax     = within_informant_input,
  filename   = "04_sdq_within_informant_crossscale.inp",
  github_dir = github_measurement_dir,
  documentation = list(
    model_id             = "M4",
    model_name           = "Metric model with within-informant residual covariances",
    model_family         = "Measurement model development",
    script               = "01_create_mplus_inputs_invariance_lcs.R",
    sample               = "stat_t5 NE 0",
    estimator            = "MLR",
    specification        = paste(
      "Four correlated factors (EXT2, EXT5, EMO2, EMO5);",
      "corresponding factor loadings constrained equal across T2 and T5"
    ),
    residual_covariances = paste(
      "Identical indicators correlated over time;",
      "within-informant cross-scale residual covariances constrained equal",
      "across T2 and T5"
    ),
    covariates           = "None",
    model_role           = "Test of within-informant cross-scale residual covariances",
    notes                = "Equality constraints apply to unstandardized residual covariances"
  )
)
##### CREATE M5 BETWEEN-INFORMANT RESIDUAL MODEL ####

between_informant_input <- paste0(
  "TITLE:
  M5: SDQ METRIC MODEL WITH WITHIN- AND BETWEEN-INFORMANT
  RESIDUAL COVARIANCES;

DATA:
  FILE = AMIS_mplus_dataset.dat;

VARIABLE:
  NAMES =
", names_syntax, "
    ;

  USEVARIABLES =
    hyp_b2 hyp_k2 hyp_p2 hyp_b5 hyp_k5 hyp_p5
    con_b2 con_k2 con_p2 con_b5 con_k5 con_p5
    emo_b2 emo_k2 emo_p2 emo_b5 emo_k5 emo_p5;

  USEOBSERVATIONS = stat_t5 NE 0;
  IDVARIABLE = SIC_N;
  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Equal factor loadings across T2 and T5

  EXT2 BY
    hyp_b2@1 hyp_k2 (ex2) hyp_p2 (ex3)
    con_b2 (ex4) con_k2 (ex5) con_p2 (ex6);

  EXT5 BY
    hyp_b5@1 hyp_k5 (ex2) hyp_p5 (ex3)
    con_b5 (ex4) con_k5 (ex5) con_p5 (ex6);

  EMO2 BY emo_b2@1 emo_k2 (em2) emo_p2 (em3);
  EMO5 BY emo_b5@1 emo_k5 (em2) emo_p5 (em3);

  EXT2 WITH EMO2 EXT5 EMO5;
  EXT5 WITH EMO2 EMO5;
  EMO2 WITH EMO5;

  ! Residual covariances of identical indicators over time

  hyp_b2 WITH hyp_b5;
  hyp_k2 WITH hyp_k5;
  hyp_p2 WITH hyp_p5;
  con_b2 WITH con_b5;
  con_k2 WITH con_k5;
  con_p2 WITH con_p5;
  emo_b2 WITH emo_b5;
  emo_k2 WITH emo_k5;
  emo_p2 WITH emo_p5;

  ! Within-informant residual covariances,
  ! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc) emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);
  hyp_b5 WITH con_b5 (w_b_hc) emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc) emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);
  hyp_k5 WITH con_k5 (w_k_hc) emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc) emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);
  hyp_p5 WITH con_p5 (w_p_hc) emo_p5 (w_p_he);
  con_p5 WITH emo_p5 (w_p_ce);

  ! Selected between-informant residual covariances for b and p,
  ! constrained equal across T2 and T5

  hyp_b2 WITH hyp_p2 (bi_hyp) con_p2 (bi_bphc);
  hyp_p2 WITH con_b2 (bi_pbhc);
  con_b2 WITH con_p2 (bi_con);
  emo_b2 WITH emo_p2 (bi_emo);

  hyp_b5 WITH hyp_p5 (bi_hyp) con_p5 (bi_bphc);
  hyp_p5 WITH con_b5 (bi_pbhc);
  con_b5 WITH con_p5 (bi_con);
  emo_b5 WITH emo_p5 (bi_emo);

OUTPUT:
  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(10);
"
)

input_file <- write_mplus_input(
  syntax     = between_informant_input,
  filename   = "05_sdq_between_informant_residuals.inp",
  github_dir = github_measurement_dir,
  documentation = list(
    model_id             = "M5",
    model_name           = "Metric model with between-informant residual covariances",
    model_family         = "Measurement model development",
    script               = "01_create_mplus_inputs_invariance_lcs.R",
    sample               = "stat_t5 NE 0",
    estimator            = "MLR",
    specification        = paste(
      "Four correlated factors (EXT2, EXT5, EMO2, EMO5);",
      "corresponding factor loadings constrained equal across T2 and T5"
    ),
    residual_covariances = paste(
      "Identical indicators correlated over time;",
      "within-informant cross-scale residual covariances;",
      "selected same- and cross-scale residual covariances between informants b and p;",
      "within- and between-informant covariances constrained equal across time"
    ),
    covariates           = "None",
    model_role           = "Test of selected between-informant residual covariances relative to M4",
    notes                = paste(
      "Between-informant covariances include HYP, CON, and EMO same-scale",
      "associations plus reciprocal HYP-CON associations for informants b and p;",
      "equality constraints apply to unstandardized residual covariances"
    )
  )
)

##### CREATE M6 FULL SCALAR INVARIANCE MODEL ####
full_scalar_input <- paste0(
  "TITLE:
  M6: SDQ FULL SCALAR INVARIANCE MODEL T2-T5;

DATA:
  FILE = AMIS_mplus_dataset.dat;

VARIABLE:
  NAMES =
", names_syntax, "
    ;

  USEVARIABLES =
    hyp_b2 hyp_k2 hyp_p2 hyp_b5 hyp_k5 hyp_p5
    con_b2 con_k2 con_p2 con_b5 con_k5 con_p5
    emo_b2 emo_k2 emo_p2 emo_b5 emo_k5 emo_p5;

  USEOBSERVATIONS = stat_t5 NE 0;
  IDVARIABLE = SIC_N;
  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Equal factor loadings across T2 and T5

  EXT2 BY
    hyp_b2@1 hyp_k2 (ex2) hyp_p2 (ex3)
    con_b2 (ex4) con_k2 (ex5) con_p2 (ex6);

  EXT5 BY
    hyp_b5@1 hyp_k5 (ex2) hyp_p5 (ex3)
    con_b5 (ex4) con_k5 (ex5) con_p5 (ex6);

  EMO2 BY emo_b2@1 emo_k2 (em2) emo_p2 (em3);
  EMO5 BY emo_b5@1 emo_k5 (em2) emo_p5 (em3);

  EXT2 WITH EMO2 EXT5 EMO5;
  EXT5 WITH EMO2 EMO5;
  EMO2 WITH EMO5;

  ! Residual covariances of identical indicators over time

  hyp_b2 WITH hyp_b5;
  hyp_k2 WITH hyp_k5;
  hyp_p2 WITH hyp_p5;
  con_b2 WITH con_b5;
  con_k2 WITH con_k5;
  con_p2 WITH con_p5;
  emo_b2 WITH emo_b5;
  emo_k2 WITH emo_k5;
  emo_p2 WITH emo_p5;

  ! Within-informant residual covariances,
  ! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc) emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);
  hyp_b5 WITH con_b5 (w_b_hc) emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc) emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);
  hyp_k5 WITH con_k5 (w_k_hc) emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc) emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);
  hyp_p5 WITH con_p5 (w_p_hc) emo_p5 (w_p_he);
  con_p5 WITH emo_p5 (w_p_ce);

  ! Selected between-informant residual covariances for b and p,
  ! constrained equal across T2 and T5

  hyp_b2 WITH hyp_p2 (bi_hyp) con_p2 (bi_bphc);
  hyp_p2 WITH con_b2 (bi_pbhc);
  con_b2 WITH con_p2 (bi_con);
  emo_b2 WITH emo_p2 (bi_emo);

  hyp_b5 WITH hyp_p5 (bi_hyp) con_p5 (bi_bphc);
  hyp_p5 WITH con_b5 (bi_pbhc);
  con_b5 WITH con_p5 (bi_con);
  emo_b5 WITH emo_p5 (bi_emo);

  ! Equal indicator intercepts across T2 and T5

  [hyp_b2 hyp_b5] (ihb);
  [hyp_k2 hyp_k5] (ihk);
  [hyp_p2 hyp_p5] (ihp);

  [con_b2 con_b5] (icb);
  [con_k2 con_k5] (ick);
  [con_p2 con_p5] (icp);

  [emo_b2 emo_b5] (ieb);
  [emo_k2 emo_k5] (iek);
  [emo_p2 emo_p5] (iep);

  ! Latent means fixed at zero at T2 and freely estimated at T5

  [EXT2@0 EMO2@0];
  [EXT5 EMO5];

OUTPUT:
  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(4);
"
)

input_file <- write_mplus_input(
  syntax     = full_scalar_input,
  filename   = "06_sdq_full_scalar_invariance.inp",
  github_dir = github_invariance_dir,
  documentation = list(
    model_id             = "M6",
    model_name           = "Full scalar invariance model",
    model_family         = "Measurement invariance",
    script               = "01_create_mplus_inputs_invariance_lcs.R",
    sample               = "stat_t5 NE 0",
    estimator            = "MLR",
    specification        = paste(
      "Four correlated factors (EXT2, EXT5, EMO2, EMO5);",
      "corresponding factor loadings and all indicator intercepts",
      "constrained equal across T2 and T5;",
      "latent means fixed at zero at T2 and freely estimated at T5"
    ),
    residual_covariances = paste(
      "Identical indicators correlated over time;",
      "within-informant cross-scale residual covariances;",
      "selected same- and cross-scale residual covariances between informants b and p;",
      "within- and between-informant covariances constrained equal across time"
    ),
    covariates           = "None",
    model_role           = "Test of full scalar invariance relative to M5",
    notes                = paste(
      "All nine indicator intercepts constrained equal across time;",
      "equality constraints apply to unstandardized parameters"
    )
  )
)

##### CREATE M7 PARTIAL SCALAR INVARIANCE MODEL ####

partial_scalar_input <- paste0(
  "TITLE:
  M7: SDQ PARTIAL SCALAR INVARIANCE MODEL T2-T5;

DATA:
  FILE = AMIS_mplus_dataset.dat;

VARIABLE:
  NAMES =
", names_syntax, "
    ;

  USEVARIABLES =
    hyp_b2 hyp_k2 hyp_p2 hyp_b5 hyp_k5 hyp_p5
    con_b2 con_k2 con_p2 con_b5 con_k5 con_p5
    emo_b2 emo_k2 emo_p2 emo_b5 emo_k5 emo_p5;

  USEOBSERVATIONS = stat_t5 NE 0;
  IDVARIABLE = SIC_N;
  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Equal factor loadings across T2 and T5

  EXT2 BY
    hyp_b2@1 hyp_k2 (ex2) hyp_p2 (ex3)
    con_b2 (ex4) con_k2 (ex5) con_p2 (ex6);

  EXT5 BY
    hyp_b5@1 hyp_k5 (ex2) hyp_p5 (ex3)
    con_b5 (ex4) con_k5 (ex5) con_p5 (ex6);

  EMO2 BY emo_b2@1 emo_k2 (em2) emo_p2 (em3);
  EMO5 BY emo_b5@1 emo_k5 (em2) emo_p5 (em3);

  EXT2 WITH EMO2 EXT5 EMO5;
  EXT5 WITH EMO2 EMO5;
  EMO2 WITH EMO5;

  ! Residual covariances of identical indicators over time

  hyp_b2 WITH hyp_b5;
  hyp_k2 WITH hyp_k5;
  hyp_p2 WITH hyp_p5;
  con_b2 WITH con_b5;
  con_k2 WITH con_k5;
  con_p2 WITH con_p5;
  emo_b2 WITH emo_b5;
  emo_k2 WITH emo_k5;
  emo_p2 WITH emo_p5;

  ! Within-informant residual covariances,
  ! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc) emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);
  hyp_b5 WITH con_b5 (w_b_hc) emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc) emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);
  hyp_k5 WITH con_k5 (w_k_hc) emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc) emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);
  hyp_p5 WITH con_p5 (w_p_hc) emo_p5 (w_p_he);
  con_p5 WITH emo_p5 (w_p_ce);

  ! Selected between-informant residual covariances for b and p,
  ! constrained equal across T2 and T5

  hyp_b2 WITH hyp_p2 (bi_hyp) con_p2 (bi_bphc);
  hyp_p2 WITH con_b2 (bi_pbhc);
  con_b2 WITH con_p2 (bi_con);
  emo_b2 WITH emo_p2 (bi_emo);

  hyp_b5 WITH hyp_p5 (bi_hyp) con_p5 (bi_bphc);
  hyp_p5 WITH con_b5 (bi_pbhc);
  con_b5 WITH con_p5 (bi_con);
  emo_b5 WITH emo_p5 (bi_emo);

  ! Equal indicator intercepts across T2 and T5,
  ! except for emo_k

  [hyp_b2 hyp_b5] (ihb);
  [hyp_k2 hyp_k5] (ihk);
  [hyp_p2 hyp_p5] (ihp);

  [con_b2 con_b5] (icb);
  [con_k2 con_k5] (ick);
  [con_p2 con_p5] (icp);

  [emo_b2 emo_b5] (ieb);
  [emo_p2 emo_p5] (iep);

  ! Freely estimated emo_k intercepts at each time point

  [emo_k2] (iek2);
  [emo_k5] (iek5);

  ! Latent means fixed at zero at T2 and freely estimated at T5

  [EXT2@0 EMO2@0];
  [EXT5 EMO5];

OUTPUT:
  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(4);
"
)

input_file <- write_mplus_input(
  syntax     = partial_scalar_input,
  filename   = "07_sdq_partial_scalar_invariance.inp",
  github_dir = github_invariance_dir,
  documentation = list(
    model_id             = "M7",
    model_name           = "Partial scalar invariance model",
    model_family         = "Measurement invariance",
    script               = "01_create_mplus_inputs_invariance_lcs.R",
    sample               = "stat_t5 NE 0",
    estimator            = "MLR",
    specification        = paste(
      "Four correlated factors (EXT2, EXT5, EMO2, EMO5);",
      "corresponding factor loadings and eight of nine indicator intercepts",
      "constrained equal across T2 and T5;",
      "emo_k intercept freely estimated at each time point;",
      "latent means fixed at zero at T2 and freely estimated at T5"
    ),
    residual_covariances = paste(
      "Identical indicators correlated over time;",
      "within-informant cross-scale residual covariances;",
      "selected same- and cross-scale residual covariances between informants b and p;",
      "within- and between-informant covariances constrained equal across time"
    ),
    covariates           = "None",
    model_role           = "Final partial scalar invariance model following rejection of full scalar invariance",
    notes                = paste(
      "Equality constraint on the emo_k intercept released across time;",
      "all remaining indicator intercepts constrained equal;",
      "equality constraints apply to unstandardized parameters"
    )
  )
)

##### RUN INVARIANCE INPUT FILES

#-------------------------------------------------------------------------
##### RUN MEASUREMENT AND INVARIANCE MODELS #####
#-------------------------------------------------------------------------

run_mplus_models <- TRUE

invariance_input_files <- file.path(
  mplus_input_dir,
  c(
    "01_sdq_configural.inp",
    "02_sdq_longitudinal_resid.inp",
    "03_sdq_metric_invariance.inp",
    "04_sdq_within_informant_crossscale.inp",
    "05_sdq_between_informant_residuals.inp",
    "06_sdq_full_scalar_invariance.inp",
    "07_sdq_partial_scalar_invariance.inp"
  )
)

mplus_data_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset.dat"
)


##### CHECK REQUIRED FILES #####

required_mplus_files <- c(
  invariance_input_files,
  mplus_data_file
)

missing_mplus_files <- required_mplus_files[
  !file.exists(required_mplus_files)
]

if (length(missing_mplus_files) > 0) {
  stop(
    "The following required Mplus files are missing:\n",
    paste(
      missing_mplus_files,
      collapse = "\n"
    )
  )
}


##### RUN MODELS #####

if (run_mplus_models) {
  
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
    target = invariance_input_files,
    
    # Run models even if older output files exist
    replaceOutfile = "always",
    
    # Do not print the complete Mplus output in R
    showOutput = FALSE,
    
    # Save a log of the model runs
    logFile = file.path(
      mplus_input_dir,
      "SDQ_measurement_invariance_run.log"
    ),
    
    # Show progress messages
    quiet = FALSE
  )
}


# ##### CREATE M8 PARTIAL SCALAR INVARIANCE MODEL ####
# model_8_input <- paste0(
#   "TITLE:
#   SDQ PARTIAL SCALAR INVARIANCE MODEL:
#   FREE EMO_K AND HYP_P INTERCEPTS;
# 
# DATA:
#   FILE = AMIS_mplus_dataset.dat;
# 
# VARIABLE:
#   NAMES =
# ", names_syntax, "
#     ;
# 
#   USEVARIABLES =
#     hyp_b2 hyp_k2 hyp_p2
#     hyp_b5 hyp_k5 hyp_p5
#     con_b2 con_k2 con_p2
#     con_b5 con_k5 con_p5
#     emo_b2 emo_k2 emo_p2
#     emo_b5 emo_k5 emo_p5
#     ;
# 
#   USEOBSERVATIONS =
#     stat_t5 NE 0;
# 
#   IDVARIABLE = SIC_N;
# 
#   MISSING = ALL (-999);
# 
# ANALYSIS:
#   ESTIMATOR = MLR;
#   COVERAGE = 0.01;
# 
# MODEL:
# 
#   ! Metric invariance across T2 and T5
# 
#   EXT2 BY
#     hyp_b2@1
#     hyp_k2 (ex2)
#     hyp_p2 (ex3)
#     con_b2 (ex4)
#     con_k2 (ex5)
#     con_p2 (ex6);
# 
#   EXT5 BY
#     hyp_b5@1
#     hyp_k5 (ex2)
#     hyp_p5 (ex3)
#     con_b5 (ex4)
#     con_k5 (ex5)
#     con_p5 (ex6);
# 
#   EMO2 BY
#     emo_b2@1
#     emo_k2 (em2)
#     emo_p2 (em3);
# 
#   EMO5 BY
#     emo_b5@1
#     emo_k5 (em2)
#     emo_p5 (em3);
# 
#   ! Correlations among latent factors
# 
#   EXT2 WITH EMO2 EXT5 EMO5;
#   EXT5 WITH EMO2 EMO5;
#   EMO2 WITH EMO5;
# 
#   ! Correlated residuals of identical indicators over time
# 
#   hyp_b2 WITH hyp_b5 (cu_hb);
#   hyp_k2 WITH hyp_k5 (cu_hk);
#   hyp_p2 WITH hyp_p5 (cu_hp);
# 
#   con_b2 WITH con_b5 (cu_cb);
#   con_k2 WITH con_k5 (cu_ck);
#   con_p2 WITH con_p5 (cu_cp);
# 
#   emo_b2 WITH emo_b5 (cu_eb);
#   emo_k2 WITH emo_k5 (cu_ek);
#   emo_p2 WITH emo_p5 (cu_ep);
# 
#   ! Within-informant cross-scale residual correlations
#   ! constrained to equality across T2 and T5
# 
#   hyp_b2 WITH con_b2 (w_b_hc);
#   hyp_b5 WITH con_b5 (w_b_hc);
# 
#   hyp_b2 WITH emo_b2 (w_b_he);
#   hyp_b5 WITH emo_b5 (w_b_he);
# 
#   con_b2 WITH emo_b2 (w_b_ce);
#   con_b5 WITH emo_b5 (w_b_ce);
# 
#   hyp_k2 WITH con_k2 (w_k_hc);
#   hyp_k5 WITH con_k5 (w_k_hc);
# 
#   hyp_k2 WITH emo_k2 (w_k_he);
#   hyp_k5 WITH emo_k5 (w_k_he);
# 
#   con_k2 WITH emo_k2 (w_k_ce);
#   con_k5 WITH emo_k5 (w_k_ce);
# 
#   hyp_p2 WITH con_p2 (w_p_hc);
#   hyp_p5 WITH con_p5 (w_p_hc);
# 
#   hyp_p2 WITH emo_p2 (w_p_he);
#   hyp_p5 WITH emo_p5 (w_p_he);
# 
#   con_p2 WITH emo_p2 (w_p_ce);
#   con_p5 WITH emo_p5 (w_p_ce);
# 
#   ! Between-informant residual correlations
#   ! constrained to equality across T2 and T5
# 
#   hyp_b2 WITH hyp_p2 (bi_hyp);
#   hyp_b5 WITH hyp_p5 (bi_hyp);
# 
#   con_b2 WITH con_p2 (bi_con);
#   con_b5 WITH con_p5 (bi_con);
# 
#   emo_b2 WITH emo_p2 (bi_emo);
#   emo_b5 WITH emo_p5 (bi_emo);
# 
#   hyp_b2 WITH con_p2 (bi_bphc);
#   hyp_b5 WITH con_p5 (bi_bphc);
# 
#   hyp_p2 WITH con_b2 (bi_pbhc);
#   hyp_p5 WITH con_b5 (bi_pbhc);
# 
#   ! Partial scalar invariance across T2 and T5
# 
#   [hyp_b2 hyp_b5] (ihb);
#   [hyp_k2 hyp_k5] (ihk);
# 
#   ! HYP_P intercepts freely estimated across time
# 
#   [hyp_p2] (ihp2);
#   [hyp_p5] (ihp5);
# 
#   [con_b2 con_b5] (icb);
#   [con_k2 con_k5] (ick);
#   [con_p2 con_p5] (icp);
# 
#   [emo_b2 emo_b5] (ieb);
# 
#   ! EMO_K intercepts freely estimated across time
# 
#   [emo_k2] (iek2);
#   [emo_k5] (iek5);
# 
#   [emo_p2 emo_p5] (iep);
# 
# OUTPUT:
#   SAMPSTAT
#   STANDARDIZED
#   TECH1
#   TECH4
#   MODINDICES(4);
# "
# )
# 
# ##### SAVE M8 INPUT ####
# input_file <- file.path(
#   mplus_input_dir,
#   "08_sdq_partial_scalar_emo_k_hyp_p.inp"
# )
# 
# writeLines(
#   model_8_input,
#   con = input_file
# )
# 
# ##### COPY INPUT TO GITHUB PROJECT ####
# file.copy(
#   from = input_file,
#   to = paste0(
#     "C:/Users/keil/Documents/main_outcome_amis2/",
#     "Mplus/02_invariance/",
#     "08_sdq_partial_scalar_emo_k_hyp_p.inp"
#   ),
#   overwrite = TRUE
# )
# 
# ##### CREATE M9 PARTIAL SCALAR INVARIANCE MODEL ####
# model_9_input <- paste0(
#   "TITLE:
#   SDQ PARTIAL SCALAR INVARIANCE MODEL:
#   FREE EMO_K, HYP_P, AND HYP_B INTERCEPTS;
# 
# DATA:
#   FILE = AMIS_mplus_dataset.dat;
# 
# VARIABLE:
#   NAMES =
# ", names_syntax, "
#     ;
# 
#   USEVARIABLES =
#     hyp_b2 hyp_k2 hyp_p2
#     hyp_b5 hyp_k5 hyp_p5
#     con_b2 con_k2 con_p2
#     con_b5 con_k5 con_p5
#     emo_b2 emo_k2 emo_p2
#     emo_b5 emo_k5 emo_p5
#     ;
# 
#   USEOBSERVATIONS =
#     stat_t5 NE 0;
# 
#   IDVARIABLE = SIC_N;
# 
#   MISSING = ALL (-999);
# 
# ANALYSIS:
#   ESTIMATOR = MLR;
#   COVERAGE = 0.01;
# 
# MODEL:
# 
#   ! Metric invariance across T2 and T5
# 
#   EXT2 BY
#     hyp_b2@1
#     hyp_k2 (ex2)
#     hyp_p2 (ex3)
#     con_b2 (ex4)
#     con_k2 (ex5)
#     con_p2 (ex6);
# 
#   EXT5 BY
#     hyp_b5@1
#     hyp_k5 (ex2)
#     hyp_p5 (ex3)
#     con_b5 (ex4)
#     con_k5 (ex5)
#     con_p5 (ex6);
# 
#   EMO2 BY
#     emo_b2@1
#     emo_k2 (em2)
#     emo_p2 (em3);
# 
#   EMO5 BY
#     emo_b5@1
#     emo_k5 (em2)
#     emo_p5 (em3);
# 
#   ! Correlations among latent factors
# 
#   EXT2 WITH EMO2 EXT5 EMO5;
#   EXT5 WITH EMO2 EMO5;
#   EMO2 WITH EMO5;
# 
#   ! Correlated residuals of identical indicators over time
# 
#   hyp_b2 WITH hyp_b5 (cu_hb);
#   hyp_k2 WITH hyp_k5 (cu_hk);
#   hyp_p2 WITH hyp_p5 (cu_hp);
# 
#   con_b2 WITH con_b5 (cu_cb);
#   con_k2 WITH con_k5 (cu_ck);
#   con_p2 WITH con_p5 (cu_cp);
# 
#   emo_b2 WITH emo_b5 (cu_eb);
#   emo_k2 WITH emo_k5 (cu_ek);
#   emo_p2 WITH emo_p5 (cu_ep);
# 
#   ! Within-informant cross-scale residual correlations
#   ! constrained to equality across T2 and T5
# 
#   hyp_b2 WITH con_b2 (w_b_hc);
#   hyp_b5 WITH con_b5 (w_b_hc);
# 
#   hyp_b2 WITH emo_b2 (w_b_he);
#   hyp_b5 WITH emo_b5 (w_b_he);
# 
#   con_b2 WITH emo_b2 (w_b_ce);
#   con_b5 WITH emo_b5 (w_b_ce);
# 
#   hyp_k2 WITH con_k2 (w_k_hc);
#   hyp_k5 WITH con_k5 (w_k_hc);
# 
#   hyp_k2 WITH emo_k2 (w_k_he);
#   hyp_k5 WITH emo_k5 (w_k_he);
# 
#   con_k2 WITH emo_k2 (w_k_ce);
#   con_k5 WITH emo_k5 (w_k_ce);
# 
#   hyp_p2 WITH con_p2 (w_p_hc);
#   hyp_p5 WITH con_p5 (w_p_hc);
# 
#   hyp_p2 WITH emo_p2 (w_p_he);
#   hyp_p5 WITH emo_p5 (w_p_he);
# 
#   con_p2 WITH emo_p2 (w_p_ce);
#   con_p5 WITH emo_p5 (w_p_ce);
# 
#   ! Between-informant residual correlations
#   ! constrained to equality across T2 and T5
# 
#   hyp_b2 WITH hyp_p2 (bi_hyp);
#   hyp_b5 WITH hyp_p5 (bi_hyp);
# 
#   con_b2 WITH con_p2 (bi_con);
#   con_b5 WITH con_p5 (bi_con);
# 
#   emo_b2 WITH emo_p2 (bi_emo);
#   emo_b5 WITH emo_p5 (bi_emo);
# 
#   hyp_b2 WITH con_p2 (bi_bphc);
#   hyp_b5 WITH con_p5 (bi_bphc);
# 
#   hyp_p2 WITH con_b2 (bi_pbhc);
#   hyp_p5 WITH con_b5 (bi_pbhc);
# 
#   ! Partial scalar invariance across T2 and T5
# 
#   ! HYP_B intercepts freely estimated across time
# 
#   [hyp_b2] (ihb2);
#   [hyp_b5] (ihb5);
# 
#   [hyp_k2 hyp_k5] (ihk);
# 
#   ! HYP_P intercepts freely estimated across time
# 
#   [hyp_p2] (ihp2);
#   [hyp_p5] (ihp5);
# 
#   [con_b2 con_b5] (icb);
#   [con_k2 con_k5] (ick);
#   [con_p2 con_p5] (icp);
# 
#   [emo_b2 emo_b5] (ieb);
# 
#   ! EMO_K intercepts freely estimated across time
# 
#   [emo_k2] (iek2);
#   [emo_k5] (iek5);
# 
#   [emo_p2 emo_p5] (iep);
# 
# OUTPUT:
#   SAMPSTAT
#   STANDARDIZED
#   TECH1
#   TECH4
#   MODINDICES(4);
# "
# )
# 
# ##### SAVE M9 INPUT ####
# input_file <- file.path(
#   mplus_input_dir,
#   "09_sdq_partial_scalar_emo_k_hyp_p_hyp_b.inp"
# )
# 
# writeLines(
#   model_9_input,
#   con = input_file
# )
# 
# ##### COPY INPUT TO GITHUB PROJECT ####
# file.copy(
#   from = input_file,
#   to = github_invariance_dir,
#   overwrite = TRUE
# )




##### CREATE M8 CLASSICAL LATENT CHANGE SCORE MODEL ####
model_8_input <- paste0(
  "TITLE:
  SDQ CLASSICAL BIVARIATE LATENT CHANGE SCORE MODEL
  BASED ON FINAL PARTIAL SCALAR INVARIANCE MODEL;

DATA:
  FILE = AMIS_mplus_dataset.dat;

VARIABLE:
  NAMES =
", names_syntax, "
    ;

  USEVARIABLES =
    hyp_b2 hyp_k2 hyp_p2
    hyp_b5 hyp_k5 hyp_p5
    con_b2 con_k2 con_p2
    con_b5 con_k5 con_p5
    emo_b2 emo_k2 emo_p2
    emo_b5 emo_k5 emo_p5
    ;

  USEOBSERVATIONS =
    stat_t5 NE 0;

  IDVARIABLE = SIC_N;

  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Metric invariance across T2 and T5

  EXT2 BY
    hyp_b2@1
    hyp_k2 (ex2)
    hyp_p2 (ex3)
    con_b2 (ex4)
    con_k2 (ex5)
    con_p2 (ex6);

  EXT5 BY
    hyp_b5@1
    hyp_k5 (ex2)
    hyp_p5 (ex3)
    con_b5 (ex4)
    con_k5 (ex5)
    con_p5 (ex6);

  EMO2 BY
    emo_b2@1
    emo_k2 (em2)
    emo_p2 (em3);

  EMO5 BY
    emo_b5@1
    emo_k5 (em2)
    emo_p5 (em3);

  ! Correlated residuals of identical indicators over time

  hyp_b2 WITH hyp_b5 (cu_hb);
  hyp_k2 WITH hyp_k5 (cu_hk);
  hyp_p2 WITH hyp_p5 (cu_hp);

  con_b2 WITH con_b5 (cu_cb);
  con_k2 WITH con_k5 (cu_ck);
  con_p2 WITH con_p5 (cu_cp);

  emo_b2 WITH emo_b5 (cu_eb);
  emo_k2 WITH emo_k5 (cu_ek);
  emo_p2 WITH emo_p5 (cu_ep);

  ! Within-informant cross-scale residual correlations
  ! constrained to equality across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc);
  hyp_b5 WITH con_b5 (w_b_hc);

  hyp_b2 WITH emo_b2 (w_b_he);
  hyp_b5 WITH emo_b5 (w_b_he);

  con_b2 WITH emo_b2 (w_b_ce);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc);
  hyp_k5 WITH con_k5 (w_k_hc);

  hyp_k2 WITH emo_k2 (w_k_he);
  hyp_k5 WITH emo_k5 (w_k_he);

  con_k2 WITH emo_k2 (w_k_ce);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc);
  hyp_p5 WITH con_p5 (w_p_hc);

  hyp_p2 WITH emo_p2 (w_p_he);
  hyp_p5 WITH emo_p5 (w_p_he);

  con_p2 WITH emo_p2 (w_p_ce);
  con_p5 WITH emo_p5 (w_p_ce);

  ! Between-informant residual correlations
  ! constrained to equality across T2 and T5

  hyp_b2 WITH hyp_p2 (bi_hyp);
  hyp_b5 WITH hyp_p5 (bi_hyp);

  con_b2 WITH con_p2 (bi_con);
  con_b5 WITH con_p5 (bi_con);

  emo_b2 WITH emo_p2 (bi_emo);
  emo_b5 WITH emo_p5 (bi_emo);

  hyp_b2 WITH con_p2 (bi_bphc);
  hyp_b5 WITH con_p5 (bi_bphc);

  hyp_p2 WITH con_b2 (bi_pbhc);
  hyp_p5 WITH con_b5 (bi_pbhc);

  ! Final partial scalar invariance across T2 and T5
  ! Only EMO_K intercepts are freely estimated across time

  [hyp_b2 hyp_b5] (ihb);
  [hyp_k2 hyp_k5] (ihk);
  [hyp_p2 hyp_p5] (ihp);

  [con_b2 con_b5] (icb);
  [con_k2 con_k5] (ick);
  [con_p2 con_p5] (icp);

  [emo_b2 emo_b5] (ieb);

  [emo_k2] (iek2);
  [emo_k5] (iek5);

  [emo_p2 emo_p5] (iep);

  ! Classical two-wave latent change score model

  ! Externalizing change

  d_ext BY EXT5@1;

  EXT5 ON EXT2@1;

  EXT5@0;
  [EXT5@0];

  ! Emotional-problems change

  d_emo BY EMO5@1;

  EMO5 ON EMO2@1;

  EMO5@0;
  [EMO5@0];

  ! Baseline latent means fixed for identification

  [EXT2@0 EMO2@0];

  ! Latent change means freely estimated

  [d_ext d_emo];

  ! Latent change variances freely estimated

  d_ext;
  d_emo;

  ! Covariance between baseline levels

  EXT2 WITH EMO2;

  ! Covariances between baseline levels and latent changes

  EXT2 WITH d_ext d_emo;
  EMO2 WITH d_ext d_emo;

  ! Covariance between latent changes

  d_ext WITH d_emo;

OUTPUT:
  SAMPSTAT
  STANDARDIZED
  CINTERVAL
  TECH1
  TECH4
  MODINDICES(4);
"
)

##### SAVE M8 INPUT ####
input_file <- file.path(
  mplus_input_dir,
  "08_sdq_classical_lcs.inp"
)

writeLines(
  model_8_input,
  con = input_file
)

#-----------------------------------------------------------------------
##### RUN MPLUS MODEL LCS #####
#-----------------------------------------------------------------------

run_mplus_models <- TRUE

lcs_input_files <- file.path(
  mplus_input_dir,
  c(
    "08_sdq_classical_lcs.inp")
)


##### CHECK INPUT FILES #####

missing_input_files <- lcs_input_files[
  !file.exists(lcs_input_files)
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


##### RUN MODELS #####

if (run_mplus_models) {
  
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
    target = lcs_input_files,
    
    # Run the models even if an older .out file exists
    replaceOutfile = "always",
    
    # Do not print the full Mplus estimation output in R
    showOutput = FALSE,
    
    # Save a run log
    logFile = file.path(
      mplus_input_dir,
      "SDQ_measurement_lcs_run.log"
    ),
    
    # Show progress messages
    quiet = FALSE
  )
}

##### COPY INPUT TO GITHUB PROJECT ####
copy_file_checked(
  source_file = input_file,
  target = file.path(
    github_lcs_dir,
    basename(input_file)
  )
)

#-------------------------------------------------------------

##### WRITE TABLE WITH RESULTS OF MEASUREMENT INVARIANCE ####
#-------------------------------------------------------------
##### IDENTIFY INVARIANCE OUTPUT FILES ####
invariance_output_files <- list.files(
  path = mplus_output_dir,
  pattern = "^0[1-7]_sdq_.*\\.out$",
  full.names = TRUE
)

stopifnot(
  length(invariance_output_files) == 7
)

basename(invariance_output_files)

##### READ AND EXTRACT MODEL FIT INDICES ####
fit_table <- purrr::map_dfr(
  invariance_output_files,
  function(file_path) {
    
    model_output <- MplusAutomation::readModels(
      target = file_path,
      what = "summaries",
      quiet = TRUE
    )
    
    model_summary <- model_output$summaries
    
    tibble::tibble(
      file = basename(file_path),
      
      model = stringr::str_remove(
        basename(file_path),
        "\\.out$"
      ),
      
      n = model_summary$Observations,
      parameters = model_summary$Parameters,
      chisq = model_summary$ChiSqM_Value,
      df = model_summary$ChiSqM_DF,
      p = model_summary$ChiSqM_PValue,
      cfi = model_summary$CFI,
      tli = model_summary$TLI,
      rmsea = model_summary$RMSEA_Estimate,
      rmsea_lb = model_summary$RMSEA_90CI_LB,
      rmsea_ub = model_summary$RMSEA_90CI_UB,
      srmr = model_summary$SRMR,
      aic = model_summary$AIC,
      bic = model_summary$BIC
    )
  }
) |>
  dplyr::arrange(
    as.integer(
      stringr::str_extract(
        model,
        "^\\d{2}"
      )
    )
  )

##### ROUND FIT INDICES ####
fit_table <- fit_table |>
  dplyr::mutate(
    dplyr::across(
      c(
        chisq,
        cfi,
        tli,
        rmsea,
        rmsea_lb,
        rmsea_ub,
        srmr
      ),
      ~ round(.x, 3)
    ),
    
    dplyr::across(
      c(
        aic,
        bic
      ),
      ~ round(.x, 2)
    )
  )

##### DEFINE MODEL DESCRIPTIONS ####
model_descriptions <- tibble::tribble(
  ~model,
  ~model_step,
  ~model_changes,
  
  "01_sdq_configural",
  "Configural model",
  paste0(
    "Same factor structure across T2 and T5; ",
    "loadings and indicator intercepts freely estimated"
  ),
  
  "02_sdq_residuals_free",
  "Longitudinal residuals",
  paste0(
    "Correlated residuals of identical indicators ",
    "across T2 and T5"
  ),
  
  "03_sdq_metric_invariance",
  "Metric invariance",
  paste0(
    "Factor loadings constrained to equality ",
    "across T2 and T5"
  ),
  
  "04_sdq_within_informant_crossscale",
  "Within-informant residuals",
  paste0(
    "Within-informant cross-scale residual correlations ",
    "added and constrained across time"
  ),
  
  "05_sdq_between_informant_residuals",
  "Final metric model",
  paste0(
    "Selected between-informant residual correlations ",
    "added and constrained across time"
  ),
  
  "06_sdq_full_scalar_invariance",
  "Full scalar invariance",
  paste0(
    "All indicator intercepts constrained to equality; ",
    "T2 latent means fixed to zero and T5 latent means freely estimated"
  ),
  
  "07_sdq_partial_scalar_invariance",
  "Final partial scalar model",
  paste0(
    "EMO_K intercept freely estimated across time; ",
    "all remaining indicator intercepts constrained to equality"
  )
)

##### ADD MODEL DESCRIPTIONS ####
fit_table <- fit_table |>
  dplyr::left_join(
    model_descriptions,
    by = "model"
  ) |>
  dplyr::relocate(
    model_step,
    model_changes,
    .after = model
  ) |>
  dplyr::select(
    -file
  )

##### CHECK MODEL DESCRIPTIONS ####
stopifnot(
  !any(is.na(fit_table$model_step)),
  !any(is.na(fit_table$model_changes))
)

View(fit_table)

##### SAVE COMPLETE FIT TABLE ####
readr::write_csv(
  fit_table,
  file.path(
    invariance_results_dir,
    "SDQ_measurement_invariance_fit_indices.csv"
  )
)

##### DEFINE METRIC REFERENCE MODEL ####
metric_reference <- fit_table |>
  dplyr::filter(
    model == "05_sdq_between_informant_residuals"
  ) |>
  dplyr::slice(1)

stopifnot(
  nrow(metric_reference) == 1
)

##### PREPARE APA TABLE DATA ####
apa_table_data <- fit_table |>
  dplyr::mutate(
    model_number = as.integer(
      stringr::str_extract(
        model,
        "^\\d{2}"
      )
    ),
    
    Model = paste0(
      "M",
      model_number
    ),
    
    `Model step` = model_step,
    
    `Model specification` = model_changes,
    
    N = as.integer(n),
    
    `χ²` = sprintf(
      "%.2f",
      chisq
    ),
    
    df = as.integer(df),
    
    p = dplyr::case_when(
      is.na(p) ~ "",
      p < .001 ~ "< .001",
      TRUE ~ sub(
        "^0",
        "",
        sprintf("%.3f", p)
      )
    ),
    
    CFI = sprintf(
      "%.3f",
      cfi
    ),
    
    TLI = sprintf(
      "%.3f",
      tli
    ),
    
    `RMSEA [90% CI]` = sprintf(
      "%.3f [%.3f, %.3f]",
      rmsea,
      rmsea_lb,
      rmsea_ub
    ),
    
    SRMR = sprintf(
      "%.3f",
      srmr
    ),
    
    `ΔCFI` = dplyr::case_when(
      model_number < 6 ~ "—",
      TRUE ~ sprintf(
        "%+.3f",
        cfi - metric_reference$cfi
      )
    ),
    
    `ΔRMSEA` = dplyr::case_when(
      model_number < 6 ~ "—",
      TRUE ~ sprintf(
        "%+.3f",
        rmsea - metric_reference$rmsea
      )
    ),
    
    `ΔSRMR` = dplyr::case_when(
      model_number < 6 ~ "—",
      TRUE ~ sprintf(
        "%+.3f",
        srmr - metric_reference$srmr
      )
    )
  ) |>
  dplyr::arrange(
    model_number
  ) |>
  dplyr::select(
    Model,
    `Model step`,
    `Model specification`,
    N,
    `χ²`,
    df,
    p,
    CFI,
    TLI,
    `RMSEA [90% CI]`,
    SRMR,
    `ΔCFI`,
    `ΔRMSEA`,
    `ΔSRMR`
  )

##### SAVE APA TABLE VALUES AS CSV ####
readr::write_csv(
  apa_table_data,
  file.path(
    invariance_results_dir,
    "Table_SDQ_measurement_invariance.csv"
  )
)

##### RESET FLEXTABLE DEFAULTS ####
flextable::init_flextable_defaults()

##### CREATE APA-STYLE FLEXTABLE ####
apa_ft <- flextable::flextable(
  apa_table_data
)

apa_ft <- flextable::theme_booktabs(
  apa_ft
)

apa_ft <- flextable::font(
  apa_ft,
  fontname = "Times New Roman",
  part = "all"
)

apa_ft <- flextable::fontsize(
  apa_ft,
  size = 7,
  part = "all"
)

apa_ft <- flextable::bold(
  apa_ft,
  part = "header"
)

##### HIGHLIGHT FINAL PARTIAL SCALAR MODEL ####
apa_ft <- flextable::bold(
  apa_ft,
  i = apa_table_data$Model == "M7",
  part = "body"
)

apa_ft <- flextable::align(
  apa_ft,
  j = c(
    "Model",
    "Model step",
    "Model specification"
  ),
  align = "left",
  part = "all"
)

apa_ft <- flextable::align(
  apa_ft,
  j = c(
    "N",
    "χ²",
    "df",
    "p",
    "CFI",
    "TLI",
    "RMSEA [90% CI]",
    "SRMR",
    "ΔCFI",
    "ΔRMSEA",
    "ΔSRMR"
  ),
  align = "center",
  part = "all"
)

apa_ft <- flextable::valign(
  apa_ft,
  valign = "top",
  part = "body"
)

apa_ft <- flextable::padding(
  apa_ft,
  padding = 1,
  part = "all"
)

##### SET COLUMN WIDTHS ####
apa_ft <- flextable::width(
  apa_ft,
  j = "Model",
  width = 0.40
)

apa_ft <- flextable::width(
  apa_ft,
  j = "Model step",
  width = 1.25
)

apa_ft <- flextable::width(
  apa_ft,
  j = "Model specification",
  width = 2.75
)

apa_ft <- flextable::width(
  apa_ft,
  j = "N",
  width = 0.40
)

apa_ft <- flextable::width(
  apa_ft,
  j = "χ²",
  width = 0.55
)

apa_ft <- flextable::width(
  apa_ft,
  j = "df",
  width = 0.40
)

apa_ft <- flextable::width(
  apa_ft,
  j = "p",
  width = 0.45
)

apa_ft <- flextable::width(
  apa_ft,
  j = c(
    "CFI",
    "TLI"
  ),
  width = 0.45
)

apa_ft <- flextable::width(
  apa_ft,
  j = "RMSEA [90% CI]",
  width = 1.15
)

apa_ft <- flextable::width(
  apa_ft,
  j = "SRMR",
  width = 0.50
)

apa_ft <- flextable::width(
  apa_ft,
  j = "ΔCFI",
  width = 0.50
)

apa_ft <- flextable::width(
  apa_ft,
  j = "ΔRMSEA",
  width = 0.60
)

apa_ft <- flextable::width(
  apa_ft,
  j = "ΔSRMR",
  width = 0.55
)

##### CHECK FLEXTABLE OBJECT ####
stopifnot(
  inherits(
    apa_ft,
    "flextable"
  )
)

##### DEFINE LANDSCAPE PAGE ####
landscape_section <- officer::block_section(
  officer::prop_section(
    page_size = officer::page_size(
      orient = "landscape",
      width = 11.69,
      height = 8.27
    ),
    page_margins = officer::page_mar(
      top = 0.40,
      bottom = 0.40,
      left = 0.35,
      right = 0.35
    ),
    type = "continuous"
  )
)

##### CREATE WORD DOCUMENT ####
apa_document <- officer::read_docx()

##### ADD TABLE NUMBER ####
apa_document <- officer::body_add_fpar(
  apa_document,
  officer::fpar(
    officer::ftext(
      "Table S1",
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 11,
        bold = TRUE
      )
    )
  )
)

##### ADD TABLE TITLE ####
apa_document <- officer::body_add_fpar(
  apa_document,
  officer::fpar(
    officer::ftext(
      paste0(
        "Longitudinal Measurement Invariance Testing of the SDQ ",
        "Externalizing and Emotional Problems Factors"
      ),
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 11,
        italic = TRUE
      )
    )
  )
)

##### ADD TABLE ####
apa_document <- flextable::body_add_flextable(
  apa_document,
  value = apa_ft
)

##### ADD TABLE NOTE ####
apa_document <- officer::body_add_fpar(
  apa_document,
  officer::fpar(
    officer::ftext(
      paste0(
        "Note. M5 was used as the metric reference model for comparisons ",
        "with the scalar invariance models. In M6 and M7, baseline latent ",
        "means were fixed to zero and follow-up latent means were freely ",
        "estimated. M7 was retained as the final partial scalar invariance ",
        "model. CFI = comparative fit index; TLI = Tucker–Lewis index; ",
        "RMSEA = root mean square error of approximation; CI = confidence ",
        "interval; SRMR = standardized root mean square residual. Dashes ",
        "indicate that change indices were not calculated because the ",
        "respective model preceded the metric reference model."
      ),
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 8
      )
    )
  )
)

##### APPLY LANDSCAPE SECTION ####
apa_document <- officer::body_end_block_section(
  apa_document,
  value = landscape_section
)

##### SAVE APA TABLE AS WORD FILE ####
print(
  apa_document,
  target = file.path(
    invariance_results_dir,
    "Table_S1_SDQ_measurement_invariance_APA.docx"
  )
)
#-------------------------------------------------------------
##### WRITE TABLE WITH RESULTS OF LATENT CHANGE MODEL ########
#-------------------------------------------------------------
##### DEFINE LCS OUTPUT FILE ####
lcs_output_file <- file.path(
  mplus_input_dir,
  "08_sdq_classical_lcs.out"
)

stopifnot(
  file.exists(lcs_output_file)
)

##### READ LCS OUTPUT ####
lcs_model <- MplusAutomation::readModels(
  target = lcs_output_file,
  what = c(
    "summaries",
    "parameters",
    "warn_err"
  ),
  quiet = TRUE
)

##### EXTRACT MODEL SUMMARY ####
lcs_summary <- lcs_model$summaries

##### READ MPLUS OUTPUT AS TEXT ####
lcs_output_lines <- readLines(
  lcs_output_file,
  warn = FALSE
)

##### EXTRACT OUTPUT SECTION ####
extract_mplus_section <- function(
    output_lines,
    start_pattern,
    end_pattern
) {
  
  start_index <- grep(
    start_pattern,
    output_lines
  )[1]
  
  end_indices <- grep(
    end_pattern,
    output_lines
  )
  
  end_index <- end_indices[
    end_indices > start_index
  ][1]
  
  if (
    is.na(start_index) ||
    is.na(end_index)
  ) {
    stop(
      paste0(
        "Could not identify output section: ",
        start_pattern
      )
    )
  }
  
  output_lines[
    (start_index + 1):
      (end_index - 1)
  ]
}

##### PARSE MPLUS PARAMETER SECTION ####
parse_mplus_parameter_section <- function(
    section_lines,
    confidence_intervals = FALSE
) {
  
  current_header <- NA_character_
  parameter_rows <- list()
  row_number <- 1
  
  for (line in section_lines) {
    
    line_clean <- stringr::str_squish(
      line
    )
    
    if (line_clean == "") {
      next
    }
    
    relation_header <- stringr::str_match(
      line_clean,
      "^([A-Z][A-Z0-9_]*)\\s+(BY|ON|WITH)$"
    )
    
    if (!is.na(relation_header[1, 1])) {
      
      current_header <- paste(
        relation_header[1, 2],
        relation_header[1, 3]
      )
      
      next
    }
    
    section_header <- stringr::str_to_upper(
      line_clean
    )
    
    if (
      section_header %in% c(
        "MEANS",
        "INTERCEPTS",
        "VARIANCES",
        "RESIDUAL VARIANCES"
      )
    ) {
      
      current_header <- section_header
      next
    }
    
    if (is.na(current_header)) {
      next
    }
    
    line_tokens <- stringr::str_split(
      line_clean,
      "\\s+"
    )[[1]]
    
    minimum_tokens <- ifelse(
      confidence_intervals,
      8,
      5
    )
    
    if (length(line_tokens) < minimum_tokens) {
      next
    }
    
    numeric_values <- suppressWarnings(
      as.numeric(
        line_tokens[-1]
      )
    )
    
    if (any(is.na(numeric_values))) {
      next
    }
    
    if (confidence_intervals) {
      
      if (length(numeric_values) < 7) {
        next
      }
      
      parameter_rows[[row_number]] <- tibble::tibble(
        header_key = current_header,
        param_key = line_tokens[1],
        ci_lower = numeric_values[2],
        estimate_ci = numeric_values[4],
        ci_upper = numeric_values[6]
      )
      
    } else {
      
      if (length(numeric_values) < 4) {
        next
      }
      
      parameter_rows[[row_number]] <- tibble::tibble(
        header_key = current_header,
        param_key = line_tokens[1],
        estimate = numeric_values[1],
        se = numeric_values[2],
        estimate_se = numeric_values[3],
        p = numeric_values[4]
      )
    }
    
    row_number <- row_number + 1
  }
  
  dplyr::bind_rows(
    parameter_rows
  ) |>
    dplyr::mutate(
      header_key = stringr::str_to_upper(
        header_key
      ),
      param_key = stringr::str_to_upper(
        param_key
      )
    ) |>
    dplyr::distinct(
      header_key,
      param_key,
      .keep_all = TRUE
    )
}

##### EXTRACT UNSTANDARDIZED MODEL RESULTS ####
unstandardized_section <- extract_mplus_section(
  output_lines = lcs_output_lines,
  start_pattern = "^MODEL RESULTS\\s*$",
  end_pattern = "^STANDARDIZED MODEL RESULTS\\s*$"
)

unstandardized_parameters <- parse_mplus_parameter_section(
  section_lines = unstandardized_section,
  confidence_intervals = FALSE
)

##### EXTRACT STANDARDIZED MODEL RESULTS ####
standardized_section <- extract_mplus_section(
  output_lines = lcs_output_lines,
  start_pattern = "^STANDARDIZED MODEL RESULTS\\s*$",
  end_pattern = "^R-SQUARE\\s*$"
)

standardized_parameters <- parse_mplus_parameter_section(
  section_lines = standardized_section,
  confidence_intervals = FALSE
) |>
  dplyr::transmute(
    header_key,
    param_key,
    standardized_estimate = estimate
  )

##### EXTRACT UNSTANDARDIZED CONFIDENCE INTERVALS ####
confidence_interval_section <- extract_mplus_section(
  output_lines = lcs_output_lines,
  start_pattern =
    "^CONFIDENCE INTERVALS OF MODEL RESULTS\\s*$",
  end_pattern =
    "^CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS\\s*$"
)

confidence_intervals <- parse_mplus_parameter_section(
  section_lines = confidence_interval_section,
  confidence_intervals = TRUE
) |>
  dplyr::select(
    header_key,
    param_key,
    ci_lower,
    ci_upper
  )

##### DEFINE LCS PARAMETERS FOR TABLE ####
lcs_parameter_dictionary <- tibble::tribble(
  ~parameter_group,
  ~parameter,
  ~header_key,
  ~param_key,
  ~show_standardized,
  
  "Latent mean change",
  "Externalizing problems",
  "MEANS",
  "D_EXT",
  FALSE,
  
  "Latent mean change",
  "Emotional problems",
  "MEANS",
  "D_EMO",
  FALSE,
  
  "Variance in change",
  "Externalizing problems",
  "VARIANCES",
  "D_EXT",
  FALSE,
  
  "Variance in change",
  "Emotional problems",
  "VARIANCES",
  "D_EMO",
  FALSE,
  
  "Baseline covariance",
  "Externalizing ↔ emotional problems",
  "EXT2 WITH",
  "EMO2",
  TRUE,
  
  "Baseline–change covariance",
  "Baseline externalizing ↔ change in externalizing",
  "EXT2 WITH",
  "D_EXT",
  TRUE,
  
  "Baseline–change covariance",
  "Baseline externalizing ↔ change in emotional problems",
  "EXT2 WITH",
  "D_EMO",
  TRUE,
  
  "Baseline–change covariance",
  "Baseline emotional problems ↔ change in externalizing",
  "EMO2 WITH",
  "D_EXT",
  TRUE,
  
  "Baseline–change covariance",
  "Baseline emotional problems ↔ change in emotional problems",
  "EMO2 WITH",
  "D_EMO",
  TRUE,
  
  "Change covariance",
  "Change in externalizing ↔ change in emotional problems",
  "D_EXT WITH",
  "D_EMO",
  TRUE
)

##### EXTRACT SELECTED LCS PARAMETERS ####
lcs_parameter_table <- lcs_parameter_dictionary |>
  dplyr::left_join(
    unstandardized_parameters |>
      dplyr::select(
        header_key,
        param_key,
        estimate,
        se,
        p
      ),
    by = c(
      "header_key",
      "param_key"
    )
  ) |>
  dplyr::left_join(
    confidence_intervals,
    by = c(
      "header_key",
      "param_key"
    )
  ) |>
  dplyr::left_join(
    standardized_parameters,
    by = c(
      "header_key",
      "param_key"
    )
  )

##### CHECK EXTRACTED PARAMETERS ####
missing_lcs_parameters <- lcs_parameter_table |>
  dplyr::filter(
    is.na(estimate)
  )

if (nrow(missing_lcs_parameters) > 0) {
  
  print(
    missing_lcs_parameters |>
      dplyr::select(
        parameter_group,
        parameter,
        header_key,
        param_key
      )
  )
  
  stop(
    "At least one requested LCS parameter was not found."
  )
}

print(
  lcs_parameter_table |>
    dplyr::select(
      parameter_group,
      parameter,
      estimate,
      se,
      p,
      standardized_estimate
    ),
  n = Inf
)
##### CHECK FOR PARAMETERS THAT WERE NOT FOUND ####
missing_lcs_parameters <- lcs_parameter_table |>
  dplyr::filter(
    is.na(estimate)
  )

if (nrow(missing_lcs_parameters) > 0) {
  
  print(
    missing_lcs_parameters |>
      dplyr::select(
        parameter_group,
        parameter,
        header_key,
        param_key
      )
  )
  
  stop(
    "At least one requested LCS parameter was not found in the Mplus output."
  )
}

##### SAVE COMPLETE EXTRACTED PARAMETER TABLE ####
readr::write_csv(
  lcs_parameter_table,
  file.path(
    lcs_results_dir,
    "SDQ_classical_lcs_parameters.csv"
  )
)

##### PREPARE APA TABLE DATA ####
lcs_apa_table_data <- lcs_parameter_table |>
  dplyr::transmute(
    `Parameter group` = parameter_group,
    
    Parameter = parameter,
    
    "Unstandardized Estimate" = sprintf(
      "%.3f",
      estimate
    ),
    
    SE = sprintf(
      "%.3f",
      se
    ),
    
    `95% CI` = format_confidence_interval(
      ci_lower,
      ci_upper
    ),
    
    p = format_p_value(
      p
    ),
    
    `Std. estimate` = dplyr::if_else(
      show_standardized & !is.na(standardized_estimate),
      sprintf(
        "%.3f",
        standardized_estimate
      ),
      "—"
    )
  )


##### FORMAT MODEL FIT FOR TABLE NOTE ####
model_fit_p <- dplyr::case_when(
  is.na(lcs_summary$ChiSqM_PValue) ~ "",
  lcs_summary$ChiSqM_PValue < .001 ~ "p < .001",
  TRUE ~ paste0(
    "p = ",
    sub(
      "^0",
      "",
      sprintf(
        "%.3f",
        lcs_summary$ChiSqM_PValue
      )
    )
  )
)

model_fit_note <- paste0(
  "Model fit: χ²(",
  lcs_summary$ChiSqM_DF,
  ") = ",
  sprintf(
    "%.2f",
    lcs_summary$ChiSqM_Value
  ),
  ", ",
  model_fit_p,
  ", CFI = ",
  sprintf(
    "%.3f",
    lcs_summary$CFI
  ),
  ", TLI = ",
  sprintf(
    "%.3f",
    lcs_summary$TLI
  ),
  ", RMSEA = ",
  sprintf(
    "%.3f",
    lcs_summary$RMSEA_Estimate
  ),
  ", 90% CI [",
  sprintf(
    "%.3f",
    lcs_summary$RMSEA_90CI_LB
  ),
  ", ",
  sprintf(
    "%.3f",
    lcs_summary$RMSEA_90CI_UB
  ),
  "], SRMR = ",
  sprintf(
    "%.3f",
    lcs_summary$SRMR
  ),
  "."
)

##### RESET FLEXTABLE DEFAULTS ####
flextable::init_flextable_defaults()

##### CREATE APA-STYLE FLEXTABLE ####
lcs_ft <- flextable::flextable(
  lcs_apa_table_data
)

lcs_ft <- flextable::theme_booktabs(
  lcs_ft
)

lcs_ft <- flextable::merge_v(
  lcs_ft,
  j = "Parameter group"
)

lcs_ft <- flextable::font(
  lcs_ft,
  fontname = "Times New Roman",
  part = "all"
)

lcs_ft <- flextable::fontsize(
  lcs_ft,
  size = 8.5,
  part = "all"
)

lcs_ft <- flextable::bold(
  lcs_ft,
  part = "header"
)

lcs_ft <- flextable::align(
  lcs_ft,
  j = c(
    "Parameter group",
    "Parameter"
  ),
  align = "left",
  part = "all"
)

lcs_ft <- flextable::align(
  lcs_ft,
  j = c(
    "Unstandardized Estimate",
    "SE",
    "95% CI",
    "p",
    "Std. estimate"
  ),
  align = "center",
  part = "all"
)

lcs_ft <- flextable::valign(
  lcs_ft,
  j = "Parameter group",
  valign = "top",
  part = "body"
)

lcs_ft <- flextable::padding(
  lcs_ft,
  padding = 2,
  part = "all"
)

##### SET COLUMN WIDTHS ####
lcs_ft <- flextable::width(
  lcs_ft,
  j = "Parameter group",
  width = 1.25
)

lcs_ft <- flextable::width(
  lcs_ft,
  j = "Parameter",
  width = 2.20
)

lcs_ft <- flextable::width(
  lcs_ft,
  j = "Unstandardized Estimate",
  width = 0.65
)

lcs_ft <- flextable::width(
  lcs_ft,
  j = "SE",
  width = 0.50
)

lcs_ft <- flextable::width(
  lcs_ft,
  j = "95% CI",
  width = 1.10
)

lcs_ft <- flextable::width(
  lcs_ft,
  j = "p",
  width = 0.55
)

lcs_ft <- flextable::width(
  lcs_ft,
  j = "Std. estimate",
  width = 0.80
)

##### CHECK FLEXTABLE OBJECT ####
stopifnot(
  inherits(
    lcs_ft,
    "flextable"
  )
)

##### DEFINE TABLE NUMBER AND TITLE ####
lcs_table_number <- "Table S2"

lcs_table_title <- paste0(
  "Parameter Estimates From the Unconditional Bivariate ",
  "Latent Change Score Model"
)

##### DEFINE PORTRAIT PAGE ####
portrait_section <- officer::block_section(
  officer::prop_section(
    page_size = officer::page_size(
      orient = "portrait",
      width = 8.27,
      height = 11.69
    ),
    page_margins = officer::page_mar(
      top = 0.60,
      bottom = 0.60,
      left = 0.60,
      right = 0.60
    ),
    type = "continuous"
  )
)

##### CREATE WORD DOCUMENT ####
lcs_document <- officer::read_docx()

##### ADD TABLE NUMBER ####
lcs_document <- officer::body_add_fpar(
  lcs_document,
  officer::fpar(
    officer::ftext(
      lcs_table_number,
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 11,
        bold = TRUE
      )
    )
  )
)

##### ADD TABLE TITLE ####
lcs_document <- officer::body_add_fpar(
  lcs_document,
  officer::fpar(
    officer::ftext(
      lcs_table_title,
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 11,
        italic = TRUE
      )
    )
  )
)

##### ADD TABLE ####
lcs_document <- flextable::body_add_flextable(
  lcs_document,
  value = lcs_ft
)

##### ADD TABLE NOTE ####
lcs_document <- officer::body_add_fpar(
  lcs_document,
  officer::fpar(
    officer::ftext(
      paste0(
        "Note. ",
        model_fit_note,
        " Unstandardized estimates are shown in the Estimate column. ",
        "STDYX-standardized estimates are reported for covariances. ",
        "Dashes indicate that standardized estimates are not reported for ",
        "latent means or variances. CI = confidence interval; ",
        "SE = standard error."
      ),
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 8
      )
    )
  )
)

##### APPLY PORTRAIT SECTION ####
lcs_document <- officer::body_end_block_section(
  lcs_document,
  value = portrait_section
)

##### SAVE APA TABLE AS WORD FILE ####
print(
  lcs_document,
  target = file.path(
    lcs_results_dir,
    "Table_S2_SDQ_classical_lcs_APA.docx"
  )
)


#-------------------------------------------------------------------------
##### SYNCHRONIZE AND ARCHIVE COMPLETED MPLUS MODELS #########
#-------------------------------------------------------------------------
##### DEFINE MODEL ROUTING ####
model_routing <- list(
  measurement = list(
    files = c(
      "02_sdq_residuals_free",
      "04_sdq_within_informant_crossscale",
      "05_sdq_between_informant_residuals"
    ),
    github_dir = github_measurement_dir,
    results_dir = mplus_results_dir_measurement
  ),
  
  invariance = list(
    files = c(
      "01_sdq_configural",
      "03_sdq_metric_invariance",
      "06_sdq_full_scalar_invariance",
      "07_sdq_partial_scalar_invariance"
    ),
    github_dir = github_invariance_dir,
    results_dir = mplus_results_dir_invariance
  ),
  
  lcs = list(
    files = c(
      "08_sdq_classical_lcs"
    ),
    github_dir = github_lcs_dir,
    results_dir = mplus_results_dir_lcs
  )
)

##### DEFINE ARCHIVE ####
archive_root <- file.path(
  mplus_input_dir,
  "archive"
)

archive_stamp <- format(
  Sys.time(),
  "%Y-%m-%d_%H%M"
)

dir.create(
  archive_root,
  recursive = TRUE,
  showWarnings = FALSE
)

##### DEFINE CHECKED COPY FUNCTION ####
copy_files_checked <- function(
    files,
    target_dir
) {
  
  if (length(files) == 0) {
    return(invisible(TRUE))
  }
  
  dir.create(
    target_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  copy_success <- file.copy(
    from = files,
    to = file.path(
      target_dir,
      basename(files)
    ),
    overwrite = TRUE
  )
  
  if (!all(copy_success)) {
    stop(
      paste0(
        "Could not copy all files to: ",
        target_dir
      )
    )
  }
  
  invisible(TRUE)
}

##### PROCESS MODEL GROUPS ####
for (group_name in names(model_routing)) {
  
  group <- model_routing[[group_name]]
  
  input_files <- file.path(
    mplus_input_dir,
    paste0(
      group$files,
      ".inp"
    )
  )
  
  output_files <- file.path(
    mplus_input_dir,
    paste0(
      group$files,
      ".out"
    )
  )
  
  gh5_files <- file.path(
    mplus_input_dir,
    paste0(
      group$files,
      ".gh5"
    )
  )
  
  ##### CHECK REQUIRED FILES ####
  required_files <- c(
    input_files,
    output_files
  )
  
  missing_files <- required_files[
    !file.exists(required_files)
  ]
  
  if (length(missing_files) > 0) {
    
    print(
      missing_files
    )
    
    stop(
      paste0(
        "Required files are missing for group: ",
        group_name
      )
    )
  }
  
  ##### RETAIN EXISTING OPTIONAL GH5 FILES ####
  gh5_files <- gh5_files[
    file.exists(gh5_files)
  ]
  
  result_files <- c(
    output_files,
    gh5_files
  )
  
  all_model_files <- c(
    input_files,
    result_files
  )
  
  ##### COPY INPUTS TO GITHUB ####
  copy_files_checked(
    files = input_files,
    target_dir = group$github_dir
  )
  
  ##### COPY OUTPUTS TO SEAGATE RESULTS ####
  copy_files_checked(
    files = result_files,
    target_dir = group$results_dir
  )
  
  ##### CREATE DATED GROUP ARCHIVE ####
  group_archive_dir <- file.path(
    archive_root,
    paste0(
      archive_stamp,
      "_",
      group_name
    )
  )
  
  ##### COPY ALL MODEL FILES TO ARCHIVE ####
  copy_files_checked(
    files = all_model_files,
    target_dir = group_archive_dir
  )
  
  ##### REMOVE ARCHIVED FILES FROM WORKING DIRECTORY ####
  removal_success <- file.remove(
    all_model_files
  )
  
  if (!all(removal_success)) {
    stop(
      paste0(
        "Could not remove all archived files for group: ",
        group_name
      )
    )
  }
}

##### SHOW REMAINING WORKING FILES ####
message(
  "\nSynchronization and archiving completed."
)

list.files(
  mplus_input_dir
)

##### SHOW CREATED ARCHIVE DIRECTORIES ####
list.dirs(
  archive_root,
  recursive = FALSE,
  full.names = FALSE
)
#-------------------------------------------------------------------------
