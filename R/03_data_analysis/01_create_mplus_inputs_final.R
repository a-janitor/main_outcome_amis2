#-------------------------------------------------------------------------
##### SETUP #####
#-------------------------------------------------------------------------

source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R")
source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_helper_functions.R")

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

  EMO2 BY emo_b2@1 
    emo_k2 (em2) 
    emo_p2 (em3);
  
  EMO5 BY emo_b5@1 
    emo_k5 (em2) 
    emo_p5 (em3);

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

  EMO2 BY emo_b2@1 
    emo_k2 (em2) 
    emo_p2 (em3);
  
  EMO5 BY emo_b5@1 
    emo_k5 (em2) 
    emo_p5 (em3);
    
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

 ! Within-informant residual covariances
! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc);
  hyp_b2 WITH emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);

  hyp_b5 WITH con_b5 (w_b_hc);
  hyp_b5 WITH emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc);
  hyp_k2 WITH emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);

  hyp_k5 WITH con_k5 (w_k_hc);
  hyp_k5 WITH emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc);
  hyp_p2 WITH emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);

  hyp_p5 WITH con_p5 (w_p_hc);
  hyp_p5 WITH emo_p5 (w_p_he);
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

  EMO2 BY emo_b2@1 
    emo_k2 (em2) 
    emo_p2 (em3);
  
  EMO5 BY emo_b5@1 
    emo_k5 (em2) 
    emo_p5 (em3);

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

  ! Within-informant residual covariances
! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc);
  hyp_b2 WITH emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);

  hyp_b5 WITH con_b5 (w_b_hc);
  hyp_b5 WITH emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc);
  hyp_k2 WITH emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);

  hyp_k5 WITH con_k5 (w_k_hc);
  hyp_k5 WITH emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc);
  hyp_p2 WITH emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);

  hyp_p5 WITH con_p5 (w_p_hc);
  hyp_p5 WITH emo_p5 (w_p_he);
  con_p5 WITH emo_p5 (w_p_ce);
  
  ! Selected between-informant residual covariances for b and p,
  ! constrained equal across T2 and T5

  hyp_b2 WITH hyp_p2 (bi_hyp);
  hyp_b2 WITH con_p2 (bi_bphc);
  hyp_p2 WITH con_b2 (bi_pbhc);
  con_b2 WITH con_p2 (bi_con);
  emo_b2 WITH emo_p2 (bi_emo);

  hyp_b5 WITH hyp_p5 (bi_hyp);
  hyp_b5 WITH con_p5 (bi_bphc);
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

  EMO2 BY emo_b2@1 
    emo_k2 (em2) 
    emo_p2 (em3);
  
  EMO5 BY emo_b5@1 
    emo_k5 (em2) 
    emo_p5 (em3);
    
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

! Within-informant residual covariances
! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc);
  hyp_b2 WITH emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);

  hyp_b5 WITH con_b5 (w_b_hc);
  hyp_b5 WITH emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc);
  hyp_k2 WITH emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);

  hyp_k5 WITH con_k5 (w_k_hc);
  hyp_k5 WITH emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc);
  hyp_p2 WITH emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);

  hyp_p5 WITH con_p5 (w_p_hc);
  hyp_p5 WITH emo_p5 (w_p_he);
  con_p5 WITH emo_p5 (w_p_ce);
  
  ! Selected between-informant residual covariances for b and p,
  ! constrained equal across T2 and T5

  hyp_b2 WITH hyp_p2 (bi_hyp);
  hyp_b2 WITH con_p2 (bi_bphc);
  hyp_p2 WITH con_b2 (bi_pbhc);
  con_b2 WITH con_p2 (bi_con);
  emo_b2 WITH emo_p2 (bi_emo);

  hyp_b5 WITH hyp_p5 (bi_hyp);
  hyp_b5 WITH con_p5 (bi_bphc);
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

  EMO2 BY emo_b2@1 
    emo_k2 (em2) 
    emo_p2 (em3);
  
  EMO5 BY emo_b5@1 
    emo_k5 (em2) 
    emo_p5 (em3);
    
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

! Within-informant residual covariances
! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc);
  hyp_b2 WITH emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);

  hyp_b5 WITH con_b5 (w_b_hc);
  hyp_b5 WITH emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc);
  hyp_k2 WITH emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);

  hyp_k5 WITH con_k5 (w_k_hc);
  hyp_k5 WITH emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc);
  hyp_p2 WITH emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);

  hyp_p5 WITH con_p5 (w_p_hc);
  hyp_p5 WITH emo_p5 (w_p_he);
  con_p5 WITH emo_p5 (w_p_ce);
  
  ! Selected between-informant residual covariances for b and p,
  ! constrained equal across T2 and T5

  hyp_b2 WITH hyp_p2 (bi_hyp);
  hyp_b2 WITH con_p2 (bi_bphc);
  hyp_p2 WITH con_b2 (bi_pbhc);
  con_b2 WITH con_p2 (bi_con);
  emo_b2 WITH emo_p2 (bi_emo);

  hyp_b5 WITH hyp_p5 (bi_hyp);
  hyp_b5 WITH con_p5 (bi_bphc);
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


#-------------------------------------------------------------------------
##### DEFINE STANDARD MEASUREMENT MODEL BASED ON M7 #####
#-------------------------------------------------------------------------

sdq_standard_measurement_model <- "
  ! Equal factor loadings across T2 and T5

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

  ! Within-informant residual covariances
  ! constrained equal across T2 and T5

  hyp_b2 WITH con_b2 (w_b_hc);
  hyp_b2 WITH emo_b2 (w_b_he);
  con_b2 WITH emo_b2 (w_b_ce);

  hyp_b5 WITH con_b5 (w_b_hc);
  hyp_b5 WITH emo_b5 (w_b_he);
  con_b5 WITH emo_b5 (w_b_ce);

  hyp_k2 WITH con_k2 (w_k_hc);
  hyp_k2 WITH emo_k2 (w_k_he);
  con_k2 WITH emo_k2 (w_k_ce);

  hyp_k5 WITH con_k5 (w_k_hc);
  hyp_k5 WITH emo_k5 (w_k_he);
  con_k5 WITH emo_k5 (w_k_ce);

  hyp_p2 WITH con_p2 (w_p_hc);
  hyp_p2 WITH emo_p2 (w_p_he);
  con_p2 WITH emo_p2 (w_p_ce);

  hyp_p5 WITH con_p5 (w_p_hc);
  hyp_p5 WITH emo_p5 (w_p_he);
  con_p5 WITH emo_p5 (w_p_ce);

  ! Selected between-informant residual covariances for b and p,
  ! constrained equal across T2 and T5

  hyp_b2 WITH hyp_p2 (bi_hyp);
  hyp_b2 WITH con_p2 (bi_bphc);
  hyp_p2 WITH con_b2 (bi_pbhc);
  con_b2 WITH con_p2 (bi_con);
  emo_b2 WITH emo_p2 (bi_emo);

  hyp_b5 WITH hyp_p5 (bi_hyp);
  hyp_b5 WITH con_p5 (bi_bphc);
  hyp_p5 WITH con_b5 (bi_pbhc);
  con_b5 WITH con_p5 (bi_con);
  emo_b5 WITH emo_p5 (bi_emo);

  ! Partial scalar invariance:
  ! equal indicator intercepts except for emo_k

  [hyp_b2 hyp_b5] (ihb);
  [hyp_k2 hyp_k5] (ihk);
  [hyp_p2 hyp_p5] (ihp);

  [con_b2 con_b5] (icb);
  [con_k2 con_k5] (ick);
  [con_p2 con_p5] (icp);

  [emo_b2 emo_b5] (ieb);
  [emo_p2 emo_p5] (iep);

  [emo_k2] (iek2);
  [emo_k5] (iek5);
"

#-------------------------------------------------------------------------
##### RUN M8 CLASSICAL LATENT CHANGE SCORE MODEL #####
#-------------------------------------------------------------------------

model_8_input <- paste0(
  "TITLE:
  M8: SDQ CLASSICAL BIVARIATE LATENT CHANGE SCORE MODEL
  BASED ON FINAL PARTIAL SCALAR INVARIANCE MODEL M7;

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
", sdq_standard_measurement_model, "

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

  [EXT2@0];
  [EMO2@0];

  ! Latent change means freely estimated

  [d_ext];
  [d_emo];

  ! Baseline variances

  EXT2;
  EMO2;

  ! Latent change variances

  d_ext;
  d_emo;

  ! Covariance between baseline levels

  EXT2 WITH EMO2;

  ! Covariances between baseline levels and latent changes

  EXT2 WITH d_ext;
  EXT2 WITH d_emo;

  EMO2 WITH d_ext;
  EMO2 WITH d_emo;

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

##### SAVE AND DOCUMENT M8 INPUT #####

input_file <- write_mplus_input(
  syntax     = model_8_input,
  filename   = "08_sdq_classical_lcs.inp",
  github_dir = github_lcs_dir,
  documentation = list(
    model_id     = "M8",
    model_name   = "Classical bivariate latent change score model",
    model_family = "Latent change score model",
    script       = "01_create_mplus_inputs_invariance_lcs.R",
    sample       = "stat_t5 NE 0",
    estimator    = "MLR",
    
    specification = paste(
      "Classical two-wave bivariate latent change score model",
      "for externalizing and emotional problems;",
      "measurement model based on the final M7 partial scalar",
      "invariance model;",
      "T2 latent factors represent baseline levels;",
      "d_ext and d_emo represent latent change from T2 to T5"
    ),
    
    residual_covariances = paste(
      "Identical indicators correlated over time;",
      "within-informant cross-scale residual covariances;",
      "selected same- and cross-scale residual covariances",
      "between informants b and p;",
      "within- and between-informant covariances",
      "constrained equal across time"
    ),
    
    covariates = "None",
    
    model_role = paste(
      "Unconditional baseline latent change score model",
      "used as the basis for subsequent conditional models"
    ),
    
    notes = paste(
      "Factor loadings constrained equal across T2 and T5;",
      "eight of nine indicator intercepts constrained equal;",
      "emo_k intercept freely estimated at each time point;",
      "T5 factors decomposed into T2 baseline levels",
      "and latent change factors;",
      "baseline-to-change and cross-domain covariances",
      "freely estimated"
    )
  )
)

##### RUN M8 #####

run_mplus_models <- TRUE

lcs_input_files <- input_file

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

##### RUN MODEL #####

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
    replaceOutfile = "always",
    showOutput = FALSE,
    logFile = file.path(
      mplus_input_dir,
      "SDQ_classical_lcs_run.log"
    ),
    quiet = FALSE
  )
}

#-------------------------------------------------------------------------
##### CREATE LATENT TRAJECTORY CLASS SOLUTIONS #####
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

##### DEFINE FUNCTION TO CREATE GROWTH MODEL ####

create_growth_input <- function(
    model_number,
    model_name,
    model_title,
    observed_variables,
    time_scores,
    intercept_factor,
    slope_factor,
    quadratic_factor = NULL,
    useobservations = NULL,
    documentation
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
    all(nchar(growth_factors) <= 8),
    is.list(documentation),
    length(documentation) > 0
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
  
  useobservations_syntax <- if (
    is.null(useobservations) ||
    !nzchar(useobservations)
  ) {
    ""
  } else {
    paste0(
      "  USEOBSERVATIONS ARE ",
      useobservations,
      ";\n"
    )
  }
  
  input_syntax <- paste0(
    "TITLE:\n",
    "  ", model_title, ";\n\n",
    
    "DATA:\n",
    "  FILE = ", basename(mplus_data_file), ";\n\n",
    
    "VARIABLE:\n",
    "  NAMES ARE\n",
    names_syntax, ";\n\n",
    
    "  USEVARIABLES ARE\n",
    usevariables_syntax, ";\n\n",
    
    "  MISSING ARE ALL (-999);\n",
    "  IDVARIABLE IS SIC_N;\n",
    useobservations_syntax,
    "\n",
    
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
  
  filename <- paste0(
    sprintf(
      "%02d",
      model_number
    ),
    "_",
    model_name,
    ".inp"
  )
  
  input_file <- write_mplus_input(
    syntax = input_syntax,
    filename = filename,
    github_dir = github_maltreatment_dir,
    documentation = documentation
  )
  
  stopifnot(
    file.exists(input_file)
  )
  
  invisible(
    input_file
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


##### DEFINE ONE-CLASS GROWTH MODEL HELPER #####

create_oneclass_growth_model <- function(
    model_number,
    model_name,
    dimension,
    indicator_description,
    observed_variables,
    intercept_factor,
    slope_factor,
    quadratic_factor = NULL,
    comparison_model = NULL
) {
  
  growth_form <- if (
    is.null(
      quadratic_factor
    )
  ) {
    "linear"
  } else {
    "quadratic"
  }
  
  is_quadratic <- identical(
    growth_form,
    "quadratic"
  )
  
  create_growth_input(
    model_number = model_number,
    model_name = model_name,
    model_title = paste0(
      "M",
      model_number,
      ": One-class ",
      growth_form,
      " growth model for ",
      dimension
    ),
    observed_variables = observed_variables,
    time_scores = developmental_time_scores,
    intercept_factor = intercept_factor,
    slope_factor = slope_factor,
    quadratic_factor = quadratic_factor,
    useobservations = trajectory_sample_filter,
    
    documentation = list(
      model_id = paste0(
        "M",
        model_number
      ),
      
      model_name = paste(
        "One-class",
        growth_form,
        "growth model for",
        dimension
      ),
      
      model_family = "Latent growth curve model",
      
      script = "01_create_mplus_inputs_invariance_lcs.R",
      
      sample = trajectory_sample_description,
      
      estimator = "MLR",
      
      specification = paste(
        "One-class",
        growth_form,
        "latent growth model for",
        dimension,
        "; seven standardized",
        indicator_description,
        "represent developmental periods from infancy",
        "through young adulthood;",
        if (is_quadratic) {
          paste(
            "latent intercept, linear slope, and quadratic",
            "growth factors are estimated"
          )
        } else {
          paste(
            "a latent intercept and linear slope are estimated"
          )
        }
      ),
      
      time_scores = time_score_documentation,
      
      residual_covariances = "None",
      
      covariates = "None",
      
      model_role = if (is_quadratic) {
        paste(
          "Alternative to",
          comparison_model,
          "used to evaluate whether a quadratic trajectory",
          "provides a better representation than the",
          "corresponding linear one-class model before",
          "estimating joint trajectory and mixture models"
        )
      } else {
        paste(
          "Preliminary one-class growth model used to evaluate",
          "the linear functional form before estimating",
          "joint trajectory and mixture models"
        )
      },
      
      notes = paste(
        "Observed indicators are standardized;",
        "the intercept represents the expected level at",
        "the centered developmental age;",
        "the linear slope represents change at that age;",
        if (is_quadratic) {
          "the quadratic factor represents curvature;"
        },
        "one unit of developmental time corresponds to",
        time_scale_years,
        "years;",
        "growth-factor means, variances, and covariances",
        "are freely estimated"
      )
    )
  )
}

##### DEFINE TRAJECTORY SAMPLE #####

trajectory_sample_variable <- "mal_all"
trajectory_sample_value <- 1

trajectory_sample_filter <- paste(
  trajectory_sample_variable,
  "EQ",
  trajectory_sample_value
)

trajectory_sample_description <- paste(
  "Participants with",
  trajectory_sample_filter,
  "(maltreated trajectory sample)"
)

##### DEFINE DEVELOPMENTAL TIME METRIC #####

required_time_variables <- unique(
  c(
    "SIC_N",
    "aget5m",
    trajectory_sample_variable
  )
)

missing_time_variables <- setdiff(
  required_time_variables,
  mplus_names
)

if (length(missing_time_variables) > 0) {
  stop(
    "The following variables required for the developmental ",
    "time metric are missing from mplus_names:\n",
    paste(
      missing_time_variables,
      collapse = "\n"
    )
  )
}

mplus_age_data <- readr::read_tsv(
  file = mplus_data_file,
  col_names = mplus_names,
  col_types = readr::cols(
    .default = readr::col_double()
  ),
  col_select = dplyr::all_of(
    required_time_variables
  ),
  na = "-999",
  show_col_types = FALSE,
  progress = FALSE,
  name_repair = "minimal"
)

if (anyDuplicated(mplus_age_data$SIC_N) > 0) {
  stop(
    "SIC_N is not unique in the Mplus dataset."
  )
}

observed_sample_values <- unique(
  stats::na.omit(
    mplus_age_data[[trajectory_sample_variable]]
  )
)

if (
  !all(
    observed_sample_values %in% c(
      0,
      1
    )
  )
) {
  stop(
    trajectory_sample_variable,
    " contains values other than 0, 1, or missing."
  )
}


##### CALCULATE REPRESENTATIVE JEA MIDPOINT ####

jea_start_age_months <- 18 * 12

jea_age_information <- mplus_age_data |>
  dplyr::filter(
    .data[[trajectory_sample_variable]] ==
      trajectory_sample_value,
    !is.na(aget5m),
    aget5m > jea_start_age_months
  ) |>
  dplyr::summarise(
    n_jea = dplyr::n(),
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
        jea_start_age_months +
          aget5m
      ) / 2
    )
  )

print(
  jea_age_information
)

if (
  jea_age_information$n_jea == 0 ||
  !is.finite(
    jea_age_information$mean_jea_midpoint_months
  )
) {
  stop(
    "No valid T5 ages above 18 years were available ",
    "in the trajectory sample."
  )
}

jea_midpoint_years <- jea_age_information |>
  dplyr::pull(
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

stopifnot(
  length(developmental_midpoints) == 7,
  all(
    is.finite(
      developmental_midpoints
    )
  ),
  all(
    diff(
      developmental_midpoints
    ) > 0
  )
)


##### CENTER AND SCALE DEVELOPMENTAL TIME ####

time_center_age <- mean(
  range(
    developmental_midpoints
  )
)

# One unit of developmental time corresponds to three years.
time_scale_years <- 3

developmental_time_scores <- (
  developmental_midpoints -
    time_center_age
) / time_scale_years

stopifnot(
  length(developmental_time_scores) == 7,
  all(
    is.finite(
      developmental_time_scores
    )
  )
)

##### DISPLAY DEVELOPMENTAL TIME METRIC ####

developmental_time_table <- tibble::tibble(
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

time_score_documentation <- paste(
  "Fixed time scores based on developmental-period midpoints;",
  paste0(
    names(developmental_time_scores),
    " = ",
    format(
      as.numeric(
        developmental_time_scores
      ),
      digits = 6,
      trim = TRUE
    ),
    collapse = ", "
  ),
  paste0(
    "; centered at age ",
    format(
      time_center_age,
      digits = 6,
      trim = TRUE
    ),
    " years"
  ),
  paste0(
    "; one time unit corresponds to ",
    time_scale_years,
    " years"
  )
)
##### CREATE AND DOCUMENT M9 - M11: LINEAR MULTIPLICITY #####

input_file_m9 <- create_oneclass_growth_model(
  model_number = 9,
  model_name = "mt_subtypes_linear_1class",
  dimension = "maltreatment multiplicity",
  indicator_description = paste(
    "indicators of the number of maltreatment subtypes"
  ),
  observed_variables = subtype_variables,
  intercept_factor = "sub_i",
  slope_factor = "sub_s"
)

input_file_m10 <- create_oneclass_growth_model(
  model_number = 10,
  model_name = "mt_frequency_linear_1class",
  dimension = "maltreatment frequency",
  indicator_description = "maltreatment frequency indicators",
  observed_variables = frequency_variables,
  intercept_factor = "fq_i",
  slope_factor = "fq_s"
)

input_file_m11 <- create_oneclass_growth_model(
  model_number = 11,
  model_name = "mt_severity_linear_1class",
  dimension = "maltreatment severity",
  indicator_description = "maltreatment severity indicators",
  observed_variables = severity_variables,
  intercept_factor = "sev_i",
  slope_factor = "sev_s"
)

##### RUN M9-M11 #####

run_mplus_models <- TRUE

growth_input_files <- c(
  input_file_m9,
  input_file_m10,
  input_file_m11
)


##### CHECK INPUT FILES #####

missing_input_files <- growth_input_files[
  !file.exists(growth_input_files)
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
  
  for (input_file in growth_input_files) {
    
    model_name <- tools::file_path_sans_ext(
      basename(input_file)
    )
    
    MplusAutomation::runModels(
      target = input_file,
      replaceOutfile = "always",
      showOutput = FALSE,
      logFile = NULL,
      quiet = FALSE
    )
  }
}


##### CHECK OUTPUT FILES #####

growth_output_files <- sub(
  "\\.inp$",
  ".out",
  growth_input_files,
  ignore.case = TRUE
)

missing_output_files <- growth_output_files[
  !file.exists(growth_output_files)
]

if (length(missing_output_files) > 0) {
  stop(
    "No Mplus output was created for:\n",
    paste(
      missing_output_files,
      collapse = "\n"
    )
  )
}

message(
  "M9-M11 completed successfully."
)

#--------------------------------------------------------------
##### M12 - M14: QUADRATIC MULTIPLICITY ####
#--------------------------------------------------------------
input_file_m12 <- create_oneclass_growth_model(
  model_number = 12,
  model_name = "mt_subtypes_quadratic_1class",
  dimension = "maltreatment multiplicity",
  indicator_description = paste(
    "indicators of the number of maltreatment subtypes"
  ),
  observed_variables = subtype_variables,
  intercept_factor = "sub_i",
  slope_factor = "sub_s",
  quadratic_factor = "sub_q",
  comparison_model = "M9"
)

input_file_m13 <- create_oneclass_growth_model(
  model_number = 13,
  model_name = "mt_frequency_quadratic_1class",
  dimension = "maltreatment frequency",
  indicator_description = "maltreatment frequency indicators",
  observed_variables = frequency_variables,
  intercept_factor = "fq_i",
  slope_factor = "fq_s",
  quadratic_factor = "fq_q",
  comparison_model = "M10"
)

input_file_m14 <- create_oneclass_growth_model(
  model_number = 14,
  model_name = "mt_severity_quadratic_1class",
  dimension = "maltreatment severity",
  indicator_description = "maltreatment severity indicators",
  observed_variables = severity_variables,
  intercept_factor = "sev_i",
  slope_factor = "sev_s",
  quadratic_factor = "sev_q",
  comparison_model = "M11"
)

##### RUN QUADRATIC TRAJECTORY MODELS (M12 - M14) #####


quadratic_growth_input_files <- c(
  M12 = input_file_m12,
  M13 = input_file_m13,
  M14 = input_file_m14
)

missing_input_files <- quadratic_growth_input_files[
  !file.exists(
    quadratic_growth_input_files
  )
]

if (length(missing_input_files) > 0) {
  stop(
    "The following Mplus input files were not created:\n",
    paste(
      names(missing_input_files),
      missing_input_files,
      sep = ": ",
      collapse = "\n"
    )
  )
}

if (isTRUE(run_mplus_models)) {
  
  for (
    input_file in unname(
      quadratic_growth_input_files
    )
  ) {
    
    message(
      "Running Mplus model: ",
      basename(
        input_file
      )
    )
    
    MplusAutomation::runModels(
      target = input_file,
      replaceOutfile = "always",
      showOutput = FALSE,
      logFile = NULL,
      quiet = FALSE
    )
  }
  
  
  ##### VERIFY MPLUS OUTPUT FILES ####
  
  quadratic_growth_output_files <- paste0(
    tools::file_path_sans_ext(
      quadratic_growth_input_files
    ),
    ".out"
  )
  
  missing_output_files <- quadratic_growth_output_files[
    !file.exists(
      quadratic_growth_output_files
    )
  ]
  
  if (length(missing_output_files) > 0) {
    stop(
      "Mplus did not create all expected output files:\n",
      paste(
        names(missing_output_files),
        missing_output_files,
        sep = ": ",
        collapse = "\n"
      )
    )
  }
  
  message(
    "M12-M14 were run successfully and all output files exist."
  )
  
} else {
  
  message(
    "M12-M14 input files were created, but Mplus execution ",
    "was skipped because run_mplus_models is FALSE."
  )
}

#---------------------------------------------------------------------
##### M15: JOINT QUADRATIC PARALLEL-PROCESS MODEL ####
#---------------------------------------------------------------------
##### DEFINE PARALLEL GROWTH PROCESSES ####

m15_growth_processes <- list(
  multiplicity = list(
    label = "maltreatment multiplicity",
    growth_factors = c(
      "sub_i",
      "sub_s",
      "sub_q"
    ),
    observed_variables = subtype_variables
  ),
  
  frequency = list(
    label = "maltreatment frequency",
    growth_factors = c(
      "fq_i",
      "fq_s",
      "fq_q"
    ),
    observed_variables = frequency_variables
  ),
  
  severity = list(
    label = "maltreatment severity",
    growth_factors = c(
      "sev_i",
      "sev_s",
      "sev_q"
    ),
    observed_variables = severity_variables
  )
)


##### EXTRACT JOINT MODEL ELEMENTS ####

joint_growth_factors <- unlist(
  lapply(
    m15_growth_processes,
    `[[`,
    "growth_factors"
  ),
  use.names = FALSE
)

joint_observed_variables <- unlist(
  lapply(
    m15_growth_processes,
    `[[`,
    "observed_variables"
  ),
  use.names = FALSE
)


##### CHECK M15 SPECIFICATION ####

stopifnot(
  length(m15_growth_processes) == 3,
  
  all(
    vapply(
      m15_growth_processes,
      function(process) {
        length(process$growth_factors) == 3
      },
      logical(1)
    )
  ),
  
  all(
    vapply(
      m15_growth_processes,
      function(process) {
        length(process$observed_variables) ==
          length(developmental_time_scores)
      },
      logical(1)
    )
  ),
  
  length(developmental_time_scores) == 7,
  all(is.finite(developmental_time_scores)),
  
  identical(
    joint_observed_variables,
    trajectory_variables
  ),
  
  all(joint_observed_variables %in% mplus_names),
  anyDuplicated(joint_observed_variables) == 0,
  
  length(joint_growth_factors) == 9,
  all(nchar(joint_growth_factors) <= 8),
  anyDuplicated(joint_growth_factors) == 0
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

##### CREATE ALL PAIRWISE COVARIANCES ####

create_all_covariances_syntax <- function(
    variable_names
) {
  
  variable_names <- trimws(
    as.character(
      variable_names
    )
  )
  
  if (
    length(variable_names) < 2L ||
    any(is.na(variable_names)) ||
    any(variable_names == "") ||
    anyDuplicated(variable_names)
  ) {
    stop(
      paste(
        "`variable_names` must contain at least two",
        "unique, non-missing variable names."
      )
    )
  }
  
  covariance_pairs <- utils::combn(
    variable_names,
    2L
  )
  
  paste0(
    "  ",
    covariance_pairs[1, ],
    " WITH ",
    covariance_pairs[2, ],
    ";",
    collapse = "\n"
  )
}

##### CREATE PARALLEL GROWTH-PROCESS SYNTAX ####

growth_processes_syntax <- paste(
  vapply(
    m15_growth_processes,
    function(process) {
      
      paste0(
        "  ! Quadratic growth process for ",
        process$label,
        "\n",
        
        create_growth_process_syntax(
          growth_factors = process$growth_factors,
          observed_variables = process$observed_variables,
          time_scores = developmental_time_scores
        )
      )
    },
    character(1)
  ),
  collapse = "\n\n"
)


##### CREATE ALL GROWTH-FACTOR COVARIANCES ####

growth_covariances_syntax <-
  create_all_covariances_syntax(
    joint_growth_factors
  )


##### CREATE WITHIN-PERIOD INDICATOR MATRIX ####

process_indicator_matrix <- do.call(
  cbind,
  lapply(
    m15_growth_processes,
    `[[`,
    "observed_variables"
  )
)

stopifnot(
  is.matrix(process_indicator_matrix),
  nrow(process_indicator_matrix) ==
    length(developmental_time_scores),
  ncol(process_indicator_matrix) ==
    length(m15_growth_processes)
)


##### CREATE WITHIN-PERIOD RESIDUAL COVARIANCES ####

within_period_residual_syntax <- paste(
  vapply(
    seq_len(
      nrow(process_indicator_matrix)
    ),
    function(position) {
      
      create_all_covariances_syntax(
        process_indicator_matrix[
          position,
          ,
          drop = TRUE
        ]
      )
    },
    character(1)
  ),
  collapse = "\n"
)


##### CREATE USEVARIABLES SYNTAX ####

joint_usevariables_syntax <- paste(
  wrap_mplus_names(
    joint_observed_variables,
    max_width = 88
  ),
  collapse = "\n"
)


##### DEFINE MPLUS DATA FILE ####

mplus_data_name <- basename(
  mplus_data_file
)

stopifnot(
  file.exists(mplus_data_file),
  length(mplus_data_name) == 1,
  nzchar(mplus_data_name)
)


##### CREATE M15 INPUT SYNTAX ####

input_syntax_m15 <- paste0(
  "TITLE:\n",
  "  M15: Joint one-class quadratic parallel-process ",
  "growth model for maltreatment;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  "  NAMES ARE\n",
  names_syntax, ";\n\n",
  
  "  USEVARIABLES ARE\n",
  joint_usevariables_syntax, ";\n\n",
  
  "  USEOBSERVATIONS ARE ",
  trajectory_sample_filter,
  ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = GENERAL;\n",
  "  ESTIMATOR = MLR;\n\n",
  
  "MODEL:\n",
  growth_processes_syntax, "\n\n",
  
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
  
  "  ! Residual covariances within developmental periods\n",
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


##### WRITE AND DOCUMENT M15 INPUT ####

m15_filename <- "15_mt_parallel_quadratic_1class.inp"

write_mplus_input(
  syntax = input_syntax_m15,
  filename = m15_filename,
  github_dir = github_maltreatment_dir,
  
  documentation = list(
    model_id = "M15",
    
    model_name = paste(
      "Joint one-class quadratic parallel-process",
      "growth model for maltreatment"
    ),
    
    model_family = paste(
      "Parallel-process latent growth curve model"
    ),
    
    script = "02_create_mplus_inputs_LTC.R",
    
    sample = trajectory_sample_description,
    
    estimator = "MLR",
    
    specification = paste(
      "Joint one-class quadratic parallel-process growth model",
      "for maltreatment multiplicity, frequency, and severity;",
      "each process is represented by seven standardized",
      "indicators covering infancy through young adulthood;",
      "latent intercept, linear slope, and quadratic factors",
      "are estimated for each process"
    ),
    
    time_scores = time_score_documentation,
    
    residual_covariances = paste(
      "Residual covariances among multiplicity, frequency,",
      "and severity indicators are freely estimated within",
      "each developmental period"
    ),
    
    covariates = "None",
    
    model_role = paste(
      "Preliminary joint growth model used to evaluate the",
      "simultaneous developmental structure and covariation",
      "of the three maltreatment dimensions before estimating",
      "the burden-index and trajectory-class models"
    ),
    
    notes = paste(
      "All nine growth-factor means, variances, and pairwise",
      "covariances are freely estimated;",
      "the intercept represents the expected level at the",
      "centered developmental age;",
      "one unit of developmental time corresponds to",
      time_scale_years,
      "years"
    )
  )
)


##### DEFINE AND CHECK M15 INPUT FILE ####

input_file_m15 <- file.path(
  mplus_input_dir,
  m15_filename
)

github_input_file_m15 <- file.path(
  github_maltreatment_dir,
  m15_filename
)

stopifnot(
  file.exists(input_file_m15),
  file.exists(github_input_file_m15)
)

message(
  "Created: ",
  input_file_m15
)


##### DEFINE M15 OUTPUT FILE ####

m15_output_file <- paste0(
  tools::file_path_sans_ext(
    input_file_m15
  ),
  ".out"
)


##### RECORD PREVIOUS OUTPUT TIME ####

previous_output_mtime <- if (
  file.exists(m15_output_file)
) {
  
  file.info(
    m15_output_file
  )$mtime
  
} else {
  
  as.POSIXct(NA)
}

##### RUN M15 ####

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
  
  message(
    "Running M15: ",
    basename(input_file_m15)
  )
  
  MplusAutomation::runModels(
    target = input_file_m15,
    replaceOutfile = "always",
    showOutput = TRUE,
    logFile = NULL,
    quiet = FALSE
  )
  
  
  ##### CHECK WHETHER OUTPUT WAS CREATED ####
  
  if (!file.exists(m15_output_file)) {
    
    stop(
      "M15 did not create the expected output:\n",
      m15_output_file
    )
  }
  
  
  ##### CHECK WHETHER OUTPUT WAS UPDATED ####
  
  current_output_mtime <- file.info(
    m15_output_file
  )$mtime
  
  if (
    !is.na(previous_output_mtime) &&
    current_output_mtime <= previous_output_mtime
  ) {
    
    stop(
      "The existing M15 output was not updated:\n",
      m15_output_file
    )
  }
  
  
  ##### CHECK NORMAL MODEL TERMINATION ####
  
  m15_output_text <- readLines(
    m15_output_file,
    warn = FALSE
  )
  
  model_terminated_normally <- any(
    grepl(
      "THE MODEL ESTIMATION TERMINATED NORMALLY",
      m15_output_text,
      fixed = TRUE
    )
  )
  
  if (!model_terminated_normally) {
    
    stop(
      "M15 created an output file but did not terminate normally:\n",
      m15_output_file
    )
  }
  
  
  ##### REPORT SUCCESSFUL EXECUTION ####
  
  message(
    "M15 completed successfully; output updated at ",
    format(
      current_output_mtime,
      "%Y-%m-%d %H:%M:%S"
    )
  )
  
} else {
  
  message(
    "M15 input was created, but Mplus execution was skipped."
  )
}

#---------------------------------------------------------------------
##### M16: JOINT QUADRATIC PARALLEL-PROCESS MODEL (1 Class) ####
#---------------------------------------------------------------------
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

burden_growth_factors <- c(
  "bur_i",
  "bur_s",
  "bur_q"
)


##### CHECK M16 SPECIFICATION ####

stopifnot(
  length(burden_variables) == 7,
  anyDuplicated(burden_variables) == 0,
  all(burden_variables %in% mplus_names),
  
  length(burden_growth_factors) == 3,
  anyDuplicated(burden_growth_factors) == 0,
  all(nchar(burden_growth_factors) <= 8),
  
  length(developmental_time_scores) == 7,
  all(is.finite(developmental_time_scores)),
  
  length(trajectory_sample_filter) == 1,
  nzchar(trajectory_sample_filter),
  grepl(
    "^\\s*mal_all\\s+EQ\\s+1\\s*$",
    trajectory_sample_filter
  ),
  
  length(trajectory_sample_description) == 1,
  nzchar(trajectory_sample_description),
  
  length(time_score_documentation) == 1,
  nzchar(time_score_documentation)
)


##### CREATE QUADRATIC BURDEN-GROWTH SYNTAX ####

burden_growth_syntax <- create_growth_process_syntax(
  growth_factors = burden_growth_factors,
  observed_variables = burden_variables,
  time_scores = developmental_time_scores
)


##### CREATE GROWTH-FACTOR COVARIANCES ####

burden_covariances_syntax <-
  create_all_covariances_syntax(
    burden_growth_factors
  )


##### CREATE USEVARIABLES SYNTAX ####

burden_usevariables_syntax <- paste(
  wrap_mplus_names(
    burden_variables,
    max_width = 88
  ),
  collapse = "\n"
)


##### DEFINE MPLUS DATA FILE ####

mplus_data_name <- basename(
  mplus_data_file
)

stopifnot(
  file.exists(mplus_data_file),
  length(mplus_data_name) == 1,
  nzchar(mplus_data_name)
)


##### CREATE M16 INPUT SYNTAX ####

input_syntax_m16 <- paste0(
  "TITLE:\n",
  "  M16: One-class quadratic growth model for ",
  "maltreatment burden;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  "  NAMES ARE\n",
  names_syntax, ";\n\n",
  
  "  USEVARIABLES ARE\n",
  burden_usevariables_syntax, ";\n\n",
  
  "  USEOBSERVATIONS ARE ",
  trajectory_sample_filter,
  ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = GENERAL;\n",
  "  ESTIMATOR = MLR;\n\n",
  
  "MODEL:\n",
  "  ! Quadratic growth process for maltreatment burden\n",
  burden_growth_syntax, "\n\n",
  
  "  ! Growth-factor means\n",
  "  [",
  paste(
    burden_growth_factors,
    collapse = " "
  ),
  "];\n\n",
  
  "  ! Growth-factor variances\n",
  "  ",
  paste(
    burden_growth_factors,
    collapse = " "
  ),
  ";\n\n",
  
  "  ! Covariances among growth factors\n",
  burden_covariances_syntax, "\n\n",
  
  "OUTPUT:\n",
  "  SAMPSTAT;\n",
  "  STANDARDIZED;\n",
  "  RESIDUAL;\n",
  "  CINTERVAL;\n",
  "  MODINDICES(10);\n",
  "  TECH1;\n",
  "  TECH4;\n"
)


##### WRITE AND DOCUMENT M16 INPUT ####

m16_filename <- "16_mt_burden_quadratic_1class.inp"

write_mplus_input(
  syntax = input_syntax_m16,
  filename = m16_filename,
  github_dir = github_maltreatment_dir,
  
  documentation = list(
    model_id = "M16",
    
    model_name = paste(
      "One-class quadratic growth model",
      "for maltreatment burden"
    ),
    
    model_family = "Latent growth curve model",
    
    script = "02_create_mplus_inputs_LTC.R",
    
    sample = trajectory_sample_description,
    
    estimator = "MLR",
    
    specification = paste(
      "One-class quadratic latent growth model for the",
      "period-specific maltreatment burden index;",
      "seven standardized burden indicators cover",
      "infancy through young adulthood;",
      "latent intercept, linear slope, and quadratic",
      "growth factors are freely estimated"
    ),
    
    time_scores = time_score_documentation,
    
    residual_covariances = "None",
    
    covariates = "None",
    
    model_role = paste(
      "Final one-class reference model for the composite",
      "maltreatment burden trajectory before estimating",
      "the two-, three-, and four-class trajectory models"
    ),
    
    notes = paste(
      "The burden indicators integrate standardized",
      "maltreatment multiplicity, frequency, and severity",
      "within each developmental period;",
      "all three growth-factor means, variances, and",
      "pairwise covariances are freely estimated;",
      "the intercept represents the expected burden level",
      "at the centered developmental age;",
      "one unit of developmental time corresponds to",
      time_scale_years,
      "years"
    )
  )
)


##### DEFINE AND CHECK M16 INPUT FILE ####

input_file_m16 <- file.path(
  mplus_input_dir,
  m16_filename
)

github_input_file_m16 <- file.path(
  github_maltreatment_dir,
  m16_filename
)

stopifnot(
  file.exists(input_file_m16),
  file.exists(github_input_file_m16)
)

message(
  "Created: ",
  input_file_m16
)


##### DEFINE M16 OUTPUT FILE ####

m16_output_file <- paste0(
  tools::file_path_sans_ext(
    input_file_m16
  ),
  ".out"
)


##### RECORD PREVIOUS OUTPUT TIME ####

previous_output_mtime_m16 <- if (
  file.exists(m16_output_file)
) {
  
  file.info(
    m16_output_file
  )$mtime
  
} else {
  
  as.POSIXct(NA)
}


##### RUN M16 ####

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
  
  message(
    "Running M16: ",
    basename(input_file_m16)
  )
  
  MplusAutomation::runModels(
    target = input_file_m16,
    replaceOutfile = "always",
    showOutput = TRUE,
    logFile = NULL,
    quiet = FALSE
  )
  
  
  ##### CHECK WHETHER OUTPUT WAS CREATED ####
  
  if (!file.exists(m16_output_file)) {
    
    stop(
      "M16 did not create the expected output:\n",
      m16_output_file
    )
  }
  
  
  ##### CHECK WHETHER OUTPUT WAS UPDATED ####
  
  current_output_mtime_m16 <- file.info(
    m16_output_file
  )$mtime
  
  if (
    !is.na(previous_output_mtime_m16) &&
    current_output_mtime_m16 <= previous_output_mtime_m16
  ) {
    
    stop(
      "The existing M16 output was not updated:\n",
      m16_output_file
    )
  }
  
  
  ##### CHECK NORMAL MODEL TERMINATION ####
  
  m16_output_text <- readLines(
    m16_output_file,
    warn = FALSE
  )
  
  model_terminated_normally_m16 <- any(
    grepl(
      "THE MODEL ESTIMATION TERMINATED NORMALLY",
      m16_output_text,
      fixed = TRUE
    )
  )
  
  if (!model_terminated_normally_m16) {
    
    stop(
      "M16 created an output file but did not terminate normally:\n",
      m16_output_file
    )
  }
  
  
  ##### REPORT SUCCESSFUL EXECUTION ####
  
  message(
    "M16 completed successfully; output updated at ",
    format(
      current_output_mtime_m16,
      "%Y-%m-%d %H:%M:%S"
    )
  )
  
} else {
  
  message(
    "M16 input was created, but Mplus execution was skipped."
  )
}



#---------------------------------------------------------------------
##### M17-M19: JOINT QUADRATIC PARALLEL-PROCESS MODEL (2-4 Classes) ####
#---------------------------------------------------------------------
##### CHECK COMMON BURDEN-MODEL ELEMENTS ####

stopifnot(
  exists("burden_variables"),
  exists("burden_growth_factors"),
  
  length(burden_variables) == 7,
  anyDuplicated(burden_variables) == 0,
  all(burden_variables %in% mplus_names),
  all(nchar(burden_variables) <= 8),
  
  identical(
    burden_growth_factors,
    c(
      "bur_i",
      "bur_s",
      "bur_q"
    )
  ),
  anyDuplicated(burden_growth_factors) == 0,
  all(nchar(burden_growth_factors) <= 8),
  
  length(developmental_time_scores) == 7,
  all(is.finite(developmental_time_scores)),
  
  length(trajectory_sample_filter) == 1,
  nzchar(trajectory_sample_filter),
  grepl(
    "^\\s*mal_all\\s+EQ\\s+1\\s*$",
    trajectory_sample_filter
  ),
  
  length(trajectory_sample_description) == 1,
  nzchar(trajectory_sample_description),
  
  length(time_score_documentation) == 1,
  nzchar(time_score_documentation)
)


##### CREATE COMMON BURDEN-GROWTH SYNTAX ####

burden_growth_syntax <- create_growth_process_syntax(
  growth_factors = burden_growth_factors,
  observed_variables = burden_variables,
  time_scores = developmental_time_scores
)


##### CREATE COMMON GROWTH-FACTOR COVARIANCES ####

burden_covariances_syntax <-
  create_all_covariances_syntax(
    burden_growth_factors
  )


##### CREATE COMMON USEVARIABLES SYNTAX ####

burden_usevariables_syntax <- paste(
  wrap_mplus_names(
    burden_variables,
    max_width = 88
  ),
  collapse = "\n"
)


##### DEFINE MPLUS DATA FILE ####

mplus_data_name <- basename(
  mplus_data_file
)

stopifnot(
  file.exists(mplus_data_file),
  length(mplus_data_name) == 1,
  nzchar(mplus_data_name)
)


##### DEFINE MIXTURE-MODEL START SETTINGS ####

mixture_starts_initial <- 1000L
mixture_starts_final <- 250L

mixture_lrt_starts <- c(
  0L,
  0L,
  1000L,
  250L
)

mixture_stiterations <- 20L

stopifnot(
  mixture_starts_initial > 0,
  mixture_starts_final > 0,
  mixture_starts_initial > mixture_starts_final,
  length(mixture_lrt_starts) == 4,
  all(mixture_lrt_starts >= 0),
  mixture_stiterations > 0
)

##### DEFINE TRAJECTORY GRID FOR M18 CONFIDENCE BANDS ####

trajectory_grid_points_m18 <- 81L

trajectory_time_grid_m18 <- seq(
  from = min(
    developmental_time_scores
  ),
  to = max(
    developmental_time_scores
  ),
  length.out = trajectory_grid_points_m18
)

stopifnot(
  length(trajectory_time_grid_m18) ==
    trajectory_grid_points_m18,
  all(is.finite(trajectory_time_grid_m18))
)

##### DEFINE OPTSEEDS FOR TARGETED RERUNS

optseed_by_classes <- c(
  `2` = 691234L,
  `3` = 930781L,
  `4` = 85462L
)


##### DEFINE BURDEN-MIXTURE INPUT FUNCTION #####

create_burden_mixture_input <- function(
    model_number,
    number_of_classes,
    model_role
) {
  
  stopifnot(
    length(model_number) == 1,
    length(number_of_classes) == 1,
    length(model_role) == 1,
    
    model_number %in% 17:19,
    number_of_classes %in% 2:4,
    model_number == number_of_classes + 15,
    
    nzchar(model_role)
  )
  
  
  ##### DEFINE MODEL LABELS ####
  
  model_id <- paste0(
    "M",
    model_number
  )
  
  class_word <- unname(
    c(
      `2` = "Two",
      `3` = "Three",
      `4` = "Four"
    )[
      as.character(number_of_classes)
    ]
  )
  
  class_word_lower <- tolower(
    class_word
  )
  
 
  ##### CREATE LABELED CLASS-SPECIFIC MEAN SYNTAX ####
  
  class_specific_means_syntax <- paste(
    vapply(
      seq_len(
        number_of_classes
      ),
      function(class_number) {
        
        paste0(
          "  %c#",
          class_number,
          "%\n",
          
          "  [",
          burden_growth_factors[1],
          "] (bi",
          class_number,
          ");\n",
          
          "  [",
          burden_growth_factors[2],
          "] (bs",
          class_number,
          ");\n",
          
          "  [",
          burden_growth_factors[3],
          "] (bq",
          class_number,
          ");"
        )
      },
      character(1)
    ),
    collapse = "\n\n"
  )
  
  
  ##### CREATE M18 TRAJECTORY CONSTRAINTS ####
  
  if (model_number == 18L) {
    
    trajectory_constraint_names <- unlist(
      lapply(
        seq_len(
          number_of_classes
        ),
        function(class_number) {
          
          paste0(
            "c",
            class_number,
            "p",
            sprintf(
              "%03d",
              seq_along(
                trajectory_time_grid_m18
              )
            )
          )
        }
      ),
      use.names = FALSE
    )
    
    trajectory_constraint_definitions <- unlist(
      lapply(
        seq_len(
          number_of_classes
        ),
        function(class_number) {
          
          vapply(
            seq_along(
              trajectory_time_grid_m18
            ),
            function(grid_number) {
              
              time_score <- trajectory_time_grid_m18[
                grid_number
              ]
              
              paste0(
                "  c",
                class_number,
                "p",
                sprintf(
                  "%03d",
                  grid_number
                ),
                " = bi",
                class_number,
                " + bs",
                class_number,
                " * (",
                formatC(
                  time_score,
                  format = "f",
                  digits = 8
                ),
                ") + bq",
                class_number,
                " * (",
                formatC(
                  time_score^2,
                  format = "f",
                  digits = 8
                ),
                ");"
              )
            },
            character(1)
          )
        }
      ),
      use.names = FALSE
    )
    
    trajectory_model_constraint_syntax <- paste0(
      "MODEL CONSTRAINT:\n",
      
      paste0(
        "  NEW(",
        trajectory_constraint_names,
        ");",
        collapse = "\n"
      ),
      "\n\n",
      
      paste(
        trajectory_constraint_definitions,
        collapse = "\n"
      ),
      "\n\n"
    )
    
  } else {
    
    trajectory_model_constraint_syntax <- ""
  }
  
  
  ##### DEFINE FILE NAMES ####
  
  input_filename <- paste0(
    sprintf(
      "%02d",
      model_number
    ),
    "_mt_burden_quadratic_",
    number_of_classes,
    "class.inp"
  )
  
  savedata_filename <- paste0(
    sprintf(
      "%02d",
      model_number
    ),
    "_mt_burden_quadratic_",
    number_of_classes,
    "class_cprob.dat"
  )
  
  
  ##### CREATE MPLUS INPUT SYNTAX ####
  
    input_syntax <- paste0(
    "TITLE:\n",
    "  ",
    model_id,
    ": ",
    class_word,
    "-class quadratic growth mixture model for ",
    "maltreatment burden;\n\n",
    
    "DATA:\n",
    "  FILE = ",
    mplus_data_name,
    ";\n\n",
    
    "VARIABLE:\n",
    "  NAMES ARE\n",
    names_syntax,
    ";\n\n",
    
    "  USEVARIABLES ARE\n",
    burden_usevariables_syntax,
    ";\n\n",
    
    "  USEOBSERVATIONS ARE ",
    trajectory_sample_filter,
    ";\n\n",
    
    "  MISSING ARE ALL (-999);\n",
    "  IDVARIABLE IS SIC_N;\n",
    "  CLASSES = c(",
    number_of_classes,
    ");\n\n",
    
    "ANALYSIS:\n",
    "  TYPE = MIXTURE;\n",
    "  ESTIMATOR = MLR;\n",
    "  STARTS = ",
    mixture_starts_initial,
    " ",
    mixture_starts_final,
    ";\n",
    "  STITERATIONS = ",
    mixture_stiterations,
    ";\n",
    "  LRTSTARTS = ",
    paste(
      mixture_lrt_starts,
      collapse = " "
    ),
    ";\n",
    "  PROCESSORS = 4;\n\n",
    
    "MODEL:\n",
    "  %OVERALL%\n\n",
    
    "  ! Quadratic growth process for maltreatment burden\n",
    burden_growth_syntax,
    "\n\n",
    
    "  ! Growth-factor variances constrained equal across classes\n",
    "  ",
    paste(
      burden_growth_factors,
      collapse = " "
    ),
    ";\n\n",
    
    "  ! Growth-factor covariances constrained equal across classes\n",
    burden_covariances_syntax,
    "\n\n",
    
    "  ! Residual variances constrained equal across classes\n",
    "  ",
    paste(
      burden_variables,
      collapse = " "
    ),
    ";\n\n",
    
    "  ! Class-specific growth-factor means\n",
    class_specific_means_syntax,
    "\n\n",
    
    # trajectory_model_constraint_syntax,
    # "\n",
    
    "OUTPUT:\n",
    "  TECH1;\n",
    "  TECH3;\n",
    "  TECH8;\n",
    "  TECH11;\n",
    "  TECH14;\n\n",
    
    "SAVEDATA:\n",
    "  FILE = ",
    savedata_filename,
    ";\n",
    "  SAVE = CPROBABILITIES;\n"
  )
  
  
  ##### WRITE AND DOCUMENT INPUT ####
  
  write_mplus_input(
    syntax = input_syntax,
    filename = input_filename,
    github_dir = github_maltreatment_dir,
    
    documentation = list(
      model_id = model_id,
      
      model_name = paste(
        class_word,
        "-class quadratic growth mixture model",
        "for maltreatment burden"
      ),
      
      model_family = "Growth mixture model",
      
      script = "02_create_mplus_inputs_LTC.R",
      
      sample = trajectory_sample_description,
      
      estimator = "MLR",
      
      specification = paste(
        class_word,
        "-class quadratic growth mixture model for the",
        "period-specific maltreatment burden index;",
        "seven standardized burden indicators cover",
        "infancy through young adulthood;",
        "growth-factor means vary across classes, whereas",
        "growth-factor variances, growth-factor covariances,",
        "and indicator residual variances are constrained",
        "equal across classes"
      ),
      
      time_scores = time_score_documentation,
      
      residual_covariances = paste(
        "None; period-specific residual variances are",
        "freely estimated but constrained equal across classes"
      ),
      
      covariates = "None",
      
      model_role = model_role,
      
      notes = paste(
        "Estimated only among maltreated participants;",
        "class membership is defined by differences in the",
        "means of the intercept, linear slope, and quadratic",
        "growth factors;",
        "random-start settings are STARTS =",
        mixture_starts_initial,
        mixture_starts_final,
        "and STITERATIONS =",
        mixture_stiterations,
        "; posterior class probabilities and most likely",
        "class membership are saved using",
        "SAVE = CPROBABILITIES;",
        "one unit of developmental time corresponds to",
        time_scale_years,
        "years"
      )
    )
  )
  
  
  ##### DEFINE CREATED FILE PATHS ####
  
  input_file <- file.path(
    mplus_input_dir,
    input_filename
  )
  
  github_input_file <- file.path(
    github_maltreatment_dir,
    input_filename
  )
  
  output_file <- paste0(
    tools::file_path_sans_ext(
      input_file
    ),
    ".out"
  )
  
  savedata_file <- file.path(
    mplus_input_dir,
    savedata_filename
  )
  
  
  ##### CHECK CREATED INPUT ####
  
  stopifnot(
    file.exists(input_file),
    file.exists(github_input_file)
  )
  
  generated_input <- readLines(
    input_file,
    warn = FALSE
  )
  
  stopifnot(
    any(
      grepl(
        paste0(
          "CLASSES = c(",
          number_of_classes,
          ");"
        ),
        generated_input,
        fixed = TRUE
      )
    ),
    
    any(
      grepl(
        paste0(
          "USEOBSERVATIONS ARE ",
          trajectory_sample_filter,
          ";"
        ),
        generated_input,
        fixed = TRUE
      )
    ),
    
    any(
      grepl(
        "bur_i bur_s bur_q |",
        generated_input,
        fixed = TRUE
      )
    ),
    
    any(
      grepl(
        "SAVE = CPROBABILITIES;",
        generated_input,
        fixed = TRUE
      )
    )
  )
  
  message(
    "Created ",
    model_id,
    ": ",
    input_file
  )
  
  
  ##### RETURN MODEL FILES ####
  
  list(
    model_id = model_id,
    number_of_classes = number_of_classes,
    input_file = input_file,
    output_file = output_file,
    savedata_file = savedata_file
  )
}
##### DEFINE M17-M19 MODEL SETTINGS #####

burden_mixture_model_settings <- list(
  M17 = list(
    model_number = 17L,
    number_of_classes = 2L,
    
    model_role = paste(
      "Candidate two-class solution used as the first",
      "multi-class model in the burden-trajectory",
      "class-enumeration sequence"
    )
  ),
  
  M18 = list(
    model_number = 18L,
    number_of_classes = 3L,
    
    model_role = paste(
      "Primary retained three-class solution used for",
      "the substantive trajectory classification and",
      "the subsequent outcome analyses"
    )
  ),
  
  M19 = list(
    model_number = 19L,
    number_of_classes = 4L,
    
    model_role = paste(
      "Candidate four-class solution used to evaluate",
      "whether an additional trajectory class improves",
      "the three-class representation"
    )
  )
)


##### CREATE M17-M19 INPUT FILES #####


burden_mixture_model_files <- lapply(
  burden_mixture_model_settings,
  function(settings) {
    
    create_burden_mixture_input(
      model_number = settings$model_number,
      number_of_classes = settings$number_of_classes,
      model_role = settings$model_role
    )
  }
)


##### CREATE INDIVIDUAL INPUT-FILE OBJECTS ####

input_file_m17 <-
  burden_mixture_model_files$M17$input_file

input_file_m18 <-
  burden_mixture_model_files$M18$input_file

input_file_m19 <-
  burden_mixture_model_files$M19$input_file


##### CREATE INDIVIDUAL OUTPUT-FILE OBJECTS ####

output_file_m17 <-
  burden_mixture_model_files$M17$output_file

output_file_m18 <-
  burden_mixture_model_files$M18$output_file

output_file_m19 <-
  burden_mixture_model_files$M19$output_file


##### CREATE INDIVIDUAL SAVEDATA-FILE OBJECTS ####

savedata_file_m17 <-
  burden_mixture_model_files$M17$savedata_file

savedata_file_m18 <-
  burden_mixture_model_files$M18$savedata_file

savedata_file_m19 <-
  burden_mixture_model_files$M19$savedata_file


##### COLLECT MODEL FILES ####

mixture_input_files <- vapply(
  burden_mixture_model_files,
  function(model_files) {
    model_files$input_file
  },
  character(1)
)

mixture_output_files <- vapply(
  burden_mixture_model_files,
  function(model_files) {
    model_files$output_file
  },
  character(1)
)

mixture_savedata_files <- vapply(
  burden_mixture_model_files,
  function(model_files) {
    model_files$savedata_file
  },
  character(1)
)

##### CHECK INPUT FILES ####

missing_input_files <- mixture_input_files[
  !file.exists(mixture_input_files)
]

if (length(missing_input_files) > 0) {
  
  stop(
    "The following mixture-model input files are missing:\n",
    paste(
      names(missing_input_files),
      missing_input_files,
      sep = ": ",
      collapse = "\n"
    )
  )
}


##### RECORD PREVIOUS FILE TIMES ####

previous_output_mtimes <- vapply(
  mixture_output_files,
  function(output_file) {
    
    if (file.exists(output_file)) {
      
      as.numeric(
        file.info(
          output_file
        )$mtime
      )
      
    } else {
      
      NA_real_
    }
  },
  numeric(1)
)

previous_savedata_mtimes <- vapply(
  mixture_savedata_files,
  function(savedata_file) {
    
    if (file.exists(savedata_file)) {
      
      as.numeric(
        file.info(
          savedata_file
        )$mtime
      )
      
    } else {
      
      NA_real_
    }
  },
  numeric(1)
)


##### RUN MODELS ####

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
  
  for (model_id in names(mixture_input_files)) {
    
    input_file <- mixture_input_files[[model_id]]
    output_file <- mixture_output_files[[model_id]]
    savedata_file <- mixture_savedata_files[[model_id]]
    
    message(
      "Running ",
      model_id,
      ": ",
      basename(input_file)
    )
    
    MplusAutomation::runModels(
      target = input_file,
      replaceOutfile = "always",
      showOutput = TRUE,
      logFile = NULL,
      quiet = FALSE
    )
    
    
    ##### CHECK OUTPUT FILE ####
    
    if (!file.exists(output_file)) {
      
      stop(
        model_id,
        " did not create the expected output:\n",
        output_file
      )
    }
    
    current_output_mtime <- as.numeric(
      file.info(
        output_file
      )$mtime
    )
    
    if (
      !is.na(previous_output_mtimes[[model_id]]) &&
      current_output_mtime <=
      previous_output_mtimes[[model_id]]
    ) {
      
      stop(
        model_id,
        " output exists but was not updated:\n",
        output_file
      )
    }
    
    
    ##### CHECK NORMAL TERMINATION ####
    
    output_text <- readLines(
      output_file,
      warn = FALSE
    )
    
    model_terminated_normally <- any(
      grepl(
        "THE MODEL ESTIMATION TERMINATED NORMALLY",
        output_text,
        fixed = TRUE
      )
    )
    
    if (!model_terminated_normally) {
      
      stop(
        model_id,
        " created an output but did not terminate normally:\n",
        output_file
      )
    }
    
    
    ##### CHECK REPLICATION OF BEST LOGLIKELIHOOD ####
    
    best_loglikelihood_not_replicated <- any(
      grepl(
        "THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",
        output_text,
        fixed = TRUE
      )
    )
    
    if (best_loglikelihood_not_replicated) {
      
      stop(
        model_id,
        " terminated, but the best loglikelihood was not replicated:\n",
        output_file
      )
    }
    
    
    ##### CHECK SAVEDATA FILE ####
    
    if (!file.exists(savedata_file)) {
      
      stop(
        model_id,
        " did not create the expected class-probability file:\n",
        savedata_file
      )
    }
    
    current_savedata_mtime <- as.numeric(
      file.info(
        savedata_file
      )$mtime
    )
    
    if (
      !is.na(previous_savedata_mtimes[[model_id]]) &&
      current_savedata_mtime <=
      previous_savedata_mtimes[[model_id]]
    ) {
      
      stop(
        model_id,
        " SAVEDATA file exists but was not updated:\n",
        savedata_file
      )
    }
    
    
    ##### REPORT SUCCESS ####
    
    message(
      model_id,
      " completed successfully; output and ",
      "class-probability data were updated."
    )
  }
  
} else {
  
  message(
    "M17-M19 inputs were created, but Mplus execution was skipped."
  )
}





#-------------------------------------------------------------------------
##### M20 - M22: CLASS EFFECTS ON LATENT CHANGE (WITHOUT/WITH CONTROLVARS) #####
#-------------------------------------------------------------------------

##### LOAD FINAL MPLUS DATA INFORMATION #####

outcome_mplus_data_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset_m18_ses_mo.dat"
)

outcome_mplus_names_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18_ses_mo.rds"
)

assert_file_exists(
  outcome_mplus_data_file,
  "Final Mplus dataset"
)

assert_file_exists(
  outcome_mplus_names_file,
  "Final Mplus names file"
)

outcome_mplus_names <- readRDS(
  outcome_mplus_names_file
)

outcome_mplus_fields <- count.fields(
  outcome_mplus_data_file,
  sep = "",
  blank.lines.skip = TRUE
)

stopifnot(
  is.character(outcome_mplus_names),
  length(outcome_mplus_names) > 0L,
  !anyNA(outcome_mplus_names),
  all(nchar(outcome_mplus_names) <= 8L),
  all(grepl(
    "^[A-Za-z][A-Za-z0-9_]*$",
    outcome_mplus_names
  )),
  anyDuplicated(toupper(outcome_mplus_names)) == 0L,
  length(outcome_mplus_fields) > 0L,
  all(
    outcome_mplus_fields ==
      length(outcome_mplus_names)
  )
)

outcome_names_syntax <- paste(
  wrap_mplus_names(
    outcome_mplus_names,
    max_width = 88
  ),
  collapse = "\n"
)

stopifnot(
  all(
    nchar(
      strsplit(
        outcome_names_syntax,
        "\n",
        fixed = TRUE
      )[[1L]]
    ) <= 88L
  )
)

##### DEFINE COMMON OUTCOME-MODEL VARIABLES #####

outcome_indicator_variables <- c(
  "hyp_b2", "hyp_k2", "hyp_p2",
  "hyp_b5", "hyp_k5", "hyp_p5",
  "con_b2", "con_k2", "con_p2",
  "con_b5", "con_k5", "con_p5",
  "emo_b2", "emo_k2", "emo_p2",
  "emo_b5", "emo_k5", "emo_p5"
)

outcome_class_variable <- "mo_cls"

outcome_class_dummies <- c(
  "c2",
  "c3",
  "c4"
)

required_outcome_variables <- c(
  "SIC_N",
  "stat_t5",
  outcome_class_variable,
  outcome_indicator_variables
)

missing_outcome_variables <- required_outcome_variables[
  !toupper(required_outcome_variables) %in%
    toupper(outcome_mplus_names)
]

if (length(missing_outcome_variables) > 0L) {
  stop(
    "The following variables are missing from the final Mplus dataset:\n",
    paste(
      missing_outcome_variables,
      collapse = "\n"
    )
  )
}

if (
  any(
    toupper(outcome_class_dummies) %in%
    toupper(outcome_mplus_names)
  )
) {
  stop(
    "The dummy names c2, c3, and c4 must not already occur ",
    "in the Mplus NAMES list."
  )
}


##### DEFINE COMMON CLASS SYNTAX #####

outcome_class_define_syntax <- "
  c2 = 0;
  c3 = 0;
  c4 = 0;

  IF (mo_cls EQ 2) THEN c2 = 1;
  IF (mo_cls EQ 3) THEN c3 = 1;
  IF (mo_cls EQ 4) THEN c4 = 1;
"

outcome_class_effect_syntax <- "
  ! Class effects on T2 baseline levels

  EXT2 ON c2 (e2_c2);
  EXT2 ON c3 (e2_c3);
  EXT2 ON c4 (e2_c4);

  EMO2 ON c2 (m2_c2);
  EMO2 ON c3 (m2_c3);
  EMO2 ON c4 (m2_c4);

  ! Class effects on latent change

  d_ext ON c2 (dx_c2);
  d_ext ON c3 (dx_c3);
  d_ext ON c4 (dx_c4);

  d_emo ON c2 (dm_c2);
  d_emo ON c3 (dm_c3);
  d_emo ON c4 (dm_c4);
"

outcome_change_constraint_syntax <- "
MODEL CONSTRAINT:

  NEW(
    ex2_c1 ex2_c2 ex2_c3 ex2_c4
    ex5_c1 ex5_c2 ex5_c3 ex5_c4
    em2_c1 em2_c2 em2_c3 em2_c4
    em5_c1 em5_c2 em5_c3 em5_c4

    dex_c1 dex_c2 dex_c3 dex_c4
    dem_c1 dem_c2 dem_c3 dem_c4

    dx_2v1 dx_3v1 dx_4v1
    dx_3v2 dx_4v2 dx_4v3

    dm_2v1 dm_3v1 dm_4v1
    dm_3v2 dm_4v2 dm_4v3
  );

  ! Class-specific latent levels at T2

  ex2_c1 = 0;
  ex2_c2 = e2_c2;
  ex2_c3 = e2_c3;
  ex2_c4 = e2_c4;

  em2_c1 = 0;
  em2_c2 = m2_c2;
  em2_c3 = m2_c3;
  em2_c4 = m2_c4;

  ! Class-specific latent levels at T5

  ex5_c1 = m_dext;
  ex5_c2 = e2_c2 + m_dext + dx_c2;
  ex5_c3 = e2_c3 + m_dext + dx_c3;
  ex5_c4 = e2_c4 + m_dext + dx_c4;

  em5_c1 = m_demo;
  em5_c2 = m2_c2 + m_demo + dm_c2;
  em5_c3 = m2_c3 + m_demo + dm_c3;
  em5_c4 = m2_c4 + m_demo + dm_c4;

  ! Class-specific latent changes

  dex_c1 = m_dext;
  dex_c2 = m_dext + dx_c2;
  dex_c3 = m_dext + dx_c3;
  dex_c4 = m_dext + dx_c4;

  dem_c1 = m_demo;
  dem_c2 = m_demo + dm_c2;
  dem_c3 = m_demo + dm_c3;
  dem_c4 = m_demo + dm_c4;

  ! Pairwise class contrasts: externalizing change

  dx_2v1 = dx_c2;
  dx_3v1 = dx_c3;
  dx_4v1 = dx_c4;

  dx_3v2 = dx_c3 - dx_c2;
  dx_4v2 = dx_c4 - dx_c2;
  dx_4v3 = dx_c4 - dx_c3;

  ! Pairwise class contrasts: emotional-problems change

  dm_2v1 = dm_c2;
  dm_3v1 = dm_c3;
  dm_4v1 = dm_c4;

  dm_3v2 = dm_c3 - dm_c2;
  dm_4v2 = dm_c4 - dm_c2;
  dm_4v3 = dm_c4 - dm_c3;
"


##### DEFINE REUSABLE M20-M24 INPUT FUNCTION #####

create_class_lcs_input <- function(
    model_number,
    model_name,
    input_filename,
    additional_predictors = character(),
    center_predictors = character(),
    covariates = "None",
    model_role,
    notes = ""
) {
  
  stopifnot(
    length(model_number) == 1L,
     as.character(model_number) %in%
      c(
        as.character(20:24),
        "23a",
        "23b",
        "25a",
        "25b"
      ),
    length(model_name) == 1L,
    nzchar(model_name),
    length(input_filename) == 1L,
    nzchar(input_filename),
    length(covariates) == 1L,
    length(model_role) == 1L
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
      toupper(outcome_mplus_names)
  ]
  
  if (length(missing_predictors) > 0L) {
    stop(
      "Additional predictors are missing from the final Mplus dataset:\n",
      paste(
        missing_predictors,
        collapse = "\n"
      )
    )
  }
  
  usevariables <- c(
    outcome_indicator_variables,
    additional_predictors,
    outcome_class_dummies
  )
  
  usevariables_syntax <- paste(
    wrap_mplus_names(
      usevariables,
      max_width = 88
    ),
    collapse = "\n"
  )
  
  additional_predictor_syntax <- if (
    length(additional_predictors) == 0L
  ) {
    
    ""
    
  } else {
    
    paste0(
      "\n\n",
      "  ! Effects of additional predictors on baseline and change\n\n",
      "  EXT2 EMO2 d_ext d_emo ON\n",
      paste(
        wrap_mplus_names(
          additional_predictors,
          max_width = 88,
          indent = "    "
        ),
        collapse = "\n"
      ),
      ";"
    )
  }
  
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
  
  model_id <- paste0(
    "M",
    model_number
  )
  
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
  
  input_syntax <- paste0(
    "TITLE:
", title_syntax, "

DATA:
  FILE = ", basename(outcome_mplus_data_file), ";

VARIABLE:
  NAMES =
", outcome_names_syntax, "
    ;

  USEVARIABLES =
", usevariables_syntax, "
    ;

  USEOBSERVATIONS =
    (stat_t5 NE 0) AND
    (mo_cls GE 1) AND
    (mo_cls LE 4);

  IDVARIABLE = SIC_N;

  MISSING = ALL (-999);

DEFINE:
", outcome_class_define_syntax,
    center_predictor_syntax, "

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:
", sdq_standard_measurement_model, "

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

  ! Baseline latent means fixed for factor identification

  [EXT2@0];
  [EMO2@0];

  ! Latent change intercepts:
  ! expected change in the non-maltreated reference class

  [d_ext] (m_dext);
  [d_emo] (m_demo);

  ! Baseline and latent-change residual variances

  EXT2;
  EMO2;

  d_ext;
  d_emo;

  ! Covariance between baseline levels

  EXT2 WITH EMO2;

  ! Covariances between baseline levels and latent changes

  EXT2 WITH d_ext;
  EXT2 WITH d_emo;

  EMO2 WITH d_ext;
  EMO2 WITH d_emo;

  ! Covariance between latent changes

  d_ext WITH d_emo;

", outcome_class_effect_syntax,
    additional_predictor_syntax, "

", outcome_change_constraint_syntax, "

OUTPUT:
  SAMPSTAT
  STANDARDIZED
  CINTERVAL
  TECH1
  TECH4;
"
  )
  
  write_mplus_input(
    syntax = input_syntax,
    filename = input_filename,
    github_dir = github_m18_lcs_dir,
    
    documentation = list(
      model_id = model_id,
      
      model_name = model_name,
      
      model_family =
        "Conditional latent change score model",
      
      script =
        "01_create_mplus_inputs_final.R",
      
      sample = paste(
        "Participants with stat_t5 NE 0 and valid",
        "four-group maltreatment classification in mo_cls"
      ),
      
      estimator = "MLR",
      
      specification = paste(
        "Classical two-wave bivariate latent change score model",
        "for externalizing and emotional problems;",
        "measurement model fixed to the final M7 partial scalar",
        "invariance specification;",
        "class 1 is the non-maltreated reference group;",
        "classes predict T2 baseline levels and latent change"
      ),
      
      residual_covariances = paste(
        "Inherited unchanged from",
        "sdq_standard_measurement_model"
      ),
      
      covariates = covariates,
      
      model_role = model_role,
      
      notes = notes
    )
  )
}


##### DEFINE M20-M22 SETTINGS #####

outcome_model_settings <- list(
  
  M20 = list(
    model_number = 20L,
    
    model_name = paste(
      "Four-group maltreatment classes predicting",
      "latent psychopathology change"
    ),
    
    input_filename =
      "20_sdq_lcs_class_effects_mo.inp",
    
    additional_predictors =
      character(),
    
    center_predictors =
      character(),
    
    covariates =
      "None",
    
    model_role = paste(
      "Primary unadjusted model of maltreatment-class",
      "differences in latent psychopathology change"
    ),
    
    notes = paste(
      "Class 1 is the non-maltreated reference group;",
      "class effects on EXT2 and EMO2 allow baseline latent",
      "levels to differ between classes;",
      "primary parameters are the class effects on d_ext",
      "and d_emo;",
      "class-specific latent levels, latent changes, and",
      "all pairwise change contrasts are defined in",
      "MODEL CONSTRAINT"
    )
  ),
  
  M21 = list(
    model_number = 21L,
    
    model_name = paste(
      "Four-group maltreatment classes predicting",
      "latent psychopathology change adjusted for",
      "age and sex"
    ),
    
    input_filename =
      "21_sdq_lcs_class_effects_age_sex_mo.inp",
    
    additional_predictors = c(
      "aget2",
      "sext5"
    ),
    
    center_predictors = c(
      "aget2",
      "sext5"
    ),
    
    covariates = paste(
      "Baseline age (aget2) and sex (sext5);",
      "both covariates grand-mean centered"
    ),
    
    model_role = paste(
      "Model of maltreatment-class differences in latent",
      "psychopathology change adjusted for age and sex"
    ),
    
    notes = paste(
      "Class 1 is the non-maltreated reference group;",
      "age and sex predict both T2 baseline levels and",
      "latent changes;",
      "class-specific latent levels and changes refer to",
      "the average covariate values in the analytical sample"
    )
  ),
  
  M22 = list(
    model_number = 22L,
    
    model_name = paste(
      "Four-group maltreatment classes predicting",
      "latent psychopathology change adjusted for",
      "age, sex, and maternal educational attainment"
    ),
    
    input_filename =
      "22_sdq_lcs_class_effects_sociodemographic_mo.inp",
    
    additional_predictors = c(
      "aget2",
      "sext5",
      "sesausb"
    ),
    
    center_predictors = c(
      "aget2",
      "sext5",
      "sesausb"
    ),
    
    covariates = paste(
      "Baseline age (aget2), sex (sext5), and combined",
      "maternal educational attainment (sesausb);",
      "all covariates grand-mean centered"
    ),
    
    model_role = paste(
      "Fully sociodemographically adjusted model of",
      "maltreatment-class differences in latent",
      "psychopathology change"
    ),
    
    notes = paste(
      "Class 1 is the non-maltreated reference group;",
      "combined maternal educational attainment uses",
      "the source priority T2, DFG02, JA, and T5;",
      "age, sex, and maternal educational attainment",
      "predict both T2 baseline levels and latent changes;",
      "class-specific latent levels and changes refer to",
      "the average covariate values in the analytical sample"
    )
  )
)


##### CREATE M20-M22 INPUT #####

outcome_model_files <- lapply(
  outcome_model_settings,
  function(settings) {
    do.call(
      create_class_lcs_input,
      settings
    )
  }
)

outcome_input_files <- unname(
  vapply(
    outcome_model_files,
    identity,
    character(1)
  )
)

input_file_m20 <- outcome_model_files$M20
input_file_m21 <- outcome_model_files$M21
input_file_m22 <- outcome_model_files$M22


##### CHECK M20-M22 INPUTS #####

missing_input_files <- outcome_input_files[
  !file.exists(outcome_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following outcome-model input files are missing:\n",
    paste(
      missing_input_files,
      collapse = "\n"
    )
  )
}


##### RUN M20-M22 #####

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
    target = outcome_input_files,
    replaceOutfile = "always",
    showOutput = FALSE,
    logFile = FALSE,
    quiet = FALSE
  )
  
} else {
  
  message(
    "M20-M21 inputs were created, but Mplus execution was skipped."
  )
}



#-------------------------------------------------------------------------
##### M23A-M23B: CLASS EFFECTS WITH POLYGENIC RISK SCORES #####
#-------------------------------------------------------------------------

##### DEFINE PRS MODEL SETTINGS #####

outcome_prs_model_settings <- list(
  
  M23a = list(
    model_number = "23a",
    
    model_name = paste(
      "Four-group maltreatment classes predicting",
      "latent psychopathology change adjusted for",
      "age, sex, socioeconomic status, European-ancestry",
      "MDD polygenic risk, and genetic principal components"
    ),
    
    input_filename =
      "23a_sdq_lcs_class_effects_prs_eau_mo.inp",
    
    additional_predictors = c(
      "aget2",
      "sext5",
      "sesausb",
      "prs_eau",
      "PC1",
      "PC2",
      "PC3",
      "PC4"
    ),
    
    center_predictors = c(
      "aget2",
      "sext5",
      "sesausb",
      "prs_eau",
      "PC1",
      "PC2",
      "PC3",
      "PC4"
    ),
    
    covariates = paste(
      "Baseline age (aget2), sex (sext5),",
      "socioeconomic status/maternal education (sesausb),",
      "European-ancestry MDD polygenic risk score (prs_eau),",
      "and the first four genetic principal components;",
      "all predictors grand-mean centered"
    ),
    
    model_role = paste(
      "European-ancestry PRS-adjusted model of",
      "maltreatment-class differences in latent",
      "psychopathology change"
    ),
    
    notes = paste(
      "M23a extends the sociodemographically adjusted M22 model;",
      "class 1 is the non-maltreated reference group;",
      "age, sex, socioeconomic status, prs_eau, and PC1-PC4",
      "predict both T2 baseline levels and latent changes;",
      "the PRS is entered as an additive predictor and no",
      "class-by-PRS interaction is estimated;",
      "because all predictors are grand-mean centered,",
      "class-specific latent levels and changes refer to",
      "the average predictor values in the analytical sample;",
      "class-specific latent levels, latent changes, and",
      "all pairwise change contrasts are defined in",
      "MODEL CONSTRAINT"
    )
  ),
  
  M23b = list(
    model_number = "23b",
    
    model_name = paste(
      "Four-group maltreatment classes predicting",
      "latent psychopathology change adjusted for",
      "age, sex, socioeconomic status, multi-ancestry",
      "MDD polygenic risk, and genetic principal components"
    ),
    
    input_filename =
      "23b_sdq_lcs_class_effects_prs_mau_mo.inp",
    
    additional_predictors = c(
      "aget2",
      "sext5",
      "sesausb",
      "prs_mau",
      "PC1",
      "PC2",
      "PC3",
      "PC4"
    ),
    
    center_predictors = c(
      "aget2",
      "sext5",
      "sesausb",
      "prs_mau",
      "PC1",
      "PC2",
      "PC3",
      "PC4"
    ),
    
    covariates = paste(
      "Baseline age (aget2), sex (sext5),",
      "socioeconomic status/maternal education (sesausb),",
      "multi-ancestry MDD polygenic risk score (prs_mau),",
      "and the first four genetic principal components;",
      "all predictors grand-mean centered"
    ),
    
    model_role = paste(
      "Multi-ancestry PRS-adjusted model of",
      "maltreatment-class differences in latent",
      "psychopathology change"
    ),
    
    notes = paste(
      "M23b extends the sociodemographically adjusted M22 model;",
      "class 1 is the non-maltreated reference group;",
      "age, sex, socioeconomic status, prs_mau, and PC1-PC4",
      "predict both T2 baseline levels and latent changes;",
      "the PRS is entered as an additive predictor and no",
      "class-by-PRS interaction is estimated;",
      "because all predictors are grand-mean centered,",
      "class-specific latent levels and changes refer to",
      "the average predictor values in the analytical sample;",
      "class-specific latent levels, latent changes, and",
      "all pairwise change contrasts are defined in",
      "MODEL CONSTRAINT"
    )
  )
)


##### ADD M23A-M23B TO COMMON MODEL SETTINGS #####

outcome_model_settings <- c(
  outcome_model_settings,
  outcome_prs_model_settings
)


##### CREATE M23A-M23B INPUTS #####

outcome_prs_model_files <- lapply(
  outcome_prs_model_settings,
  function(settings) {
    do.call(
      create_class_lcs_input,
      settings
    )
  }
)

outcome_prs_input_files <- unname(
  vapply(
    outcome_prs_model_files,
    identity,
    character(1)
  )
)

input_file_m23a <-
  outcome_prs_model_files$M23a

input_file_m23b <-
  outcome_prs_model_files$M23b


##### CHECK M23A-M23B INPUTS #####

missing_prs_input_files <- outcome_prs_input_files[
  !file.exists(
    outcome_prs_input_files
  )
]

if (length(missing_prs_input_files) > 0L) {
  stop(
    "The following PRS-model input files are missing:\n",
    paste(
      missing_prs_input_files,
      collapse = "\n"
    )
  )
}


##### RUN M23A-M23B #####

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
    target = outcome_prs_input_files,
    replaceOutfile = "always",
    showOutput = FALSE,
    quiet = FALSE
  )
  
} else {
  
  message(
    paste(
      "M23a-M23b inputs were created,",
      "but Mplus execution was skipped."
    )
  )
}

#-------------------------------------------------------------------------
##### M24: CLASS EFFECTS WITH HAIR CORTISOL #####
#-------------------------------------------------------------------------

##### DEFINE HAIR-CORTISOL MODEL SETTINGS #####

outcome_cortisol_model_settings <- list(
  
  M24 = list(
    model_number = 24L,
    
    model_name = paste(
      "Four-group maltreatment classes predicting",
      "latent psychopathology change adjusted for",
      "age, sex, socioeconomic status, and",
      "proximal-segment hair cortisol"
    ),
    
    input_filename =
      "24_sdq_lcs_class_effects_hair_cortisol_mo.inp",
    
    additional_predictors = c(
      "aget2",
      "sext5",
      "sesausb",
      "c2p1_z"
    ),
    
    center_predictors = c(
      "aget2",
      "sext5",
      "sesausb",
      "c2p1_z"
    ),
    
    covariates = paste(
      "Baseline age (aget2), sex (sext5),",
      "socioeconomic status/maternal education (sesausb),",
      "and standardized proximal-segment hair cortisol",
      "(c2p1_z); all predictors grand-mean centered"
    ),
    
    model_role = paste(
      "Hair-cortisol-adjusted model of",
      "maltreatment-class differences in latent",
      "psychopathology change"
    ),
    
    notes = paste(
      "M24 extends the sociodemographically adjusted M22 model;",
      "class 1 is the non-maltreated reference group;",
      "age, sex, socioeconomic status, and proximal-segment",
      "hair cortisol predict both T2 baseline levels",
      "and latent changes;",
      "hair cortisol is entered as an additive predictor and",
      "no class-by-cortisol interaction is estimated;",
      "because all predictors are grand-mean centered,",
      "class-specific latent levels and changes refer to",
      "the average predictor values in the analytical sample;",
      "class-specific latent levels, latent changes, and",
      "all pairwise change contrasts are defined in",
      "MODEL CONSTRAINT"
    )
  )
)


##### ADD M24 TO COMMON MODEL SETTINGS #####

outcome_model_settings <- c(
  outcome_model_settings,
  outcome_cortisol_model_settings
)


##### CREATE M24 INPUT #####

outcome_cortisol_model_files <- lapply(
  outcome_cortisol_model_settings,
  function(settings) {
    do.call(
      create_class_lcs_input,
      settings
    )
  }
)

outcome_cortisol_input_files <- unname(
  vapply(
    outcome_cortisol_model_files,
    identity,
    character(1)
  )
)

input_file_m24 <-
  outcome_cortisol_model_files$M24


##### CHECK M24 INPUT #####

missing_cortisol_input_files <-
  outcome_cortisol_input_files[
    !file.exists(
      outcome_cortisol_input_files
    )
  ]

if (length(missing_cortisol_input_files) > 0L) {
  stop(
    "The following hair-cortisol input files are missing:\n",
    paste(
      missing_cortisol_input_files,
      collapse = "\n"
    )
  )
}


##### RUN M24 #####

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
    target = outcome_cortisol_input_files,
    replaceOutfile = "always",
    showOutput = FALSE,
    logFile = file.path(
      mplus_input_dir,
      "SDQ_class_effects_lcs_M24_run.log"
    ),
    quiet = FALSE
  )
  
} else {
  
  message(
    paste(
      "M24 input was created,",
      "but Mplus execution was skipped."
    )
  )
}



#-------------------------------------------------------------------------
##### M25A-M25B: CLASS EFFECTS WITH BOTH BIOMARKERS #####
#-------------------------------------------------------------------------

##### DEFINE COMMON BIOMARKER VARIABLES #####

outcome_combined_biomarker_covariates <- c(
  "aget2",
  "sext5",
  "sesausb",
  "c2p1_z"
)

outcome_genetic_principal_components <- c(
  "PC1",
  "PC2",
  "PC3",
  "PC4"
)


##### DEFINE COMBINED-BIOMARKER SETTINGS FUNCTION #####

create_combined_biomarker_settings <- function(
    model_number,
    prs_variable,
    prs_description,
    input_filename
) {
  
  additional_predictors <- c(
    outcome_combined_biomarker_covariates,
    prs_variable,
    outcome_genetic_principal_components
  )
  
  list(
    model_number = model_number,
    
    model_name = paste(
      "Four-group maltreatment classes predicting",
      "latent psychopathology change adjusted for",
      "age, sex, socioeconomic status,",
      "proximal-segment hair cortisol,",
      prs_description,
      "and genetic principal components"
    ),
    
    input_filename = input_filename,
    
    additional_predictors =
      additional_predictors,
    
    center_predictors =
      additional_predictors,
    
    covariates = paste(
      "Baseline age (aget2), sex (sext5),",
      "socioeconomic status/maternal education (sesausb),",
      "standardized proximal-segment hair cortisol (c2p1_z),",
      paste0(prs_description, " (", prs_variable, "),"),
      "and the first four genetic principal components;",
      "all predictors grand-mean centered"
    ),
    
    model_role = paste(
      "Combined hair-cortisol and",
      prs_description,
      "model of maltreatment-class differences",
      "in latent psychopathology change"
    ),
    
    notes = paste(
      paste0("M", model_number),
      "extends the sociodemographically adjusted M22 model;",
      "class 1 is the non-maltreated reference group;",
      "hair cortisol and polygenic risk are entered",
      "simultaneously as additive predictors;",
      "both biomarkers predict T2 baseline levels",
      "and latent changes while mutually adjusted;",
      "PC1-PC4 adjust the PRS effect for genetic ancestry;",
      "no interactions between class, cortisol, or",
      "polygenic risk are estimated;",
      "because all predictors are grand-mean centered,",
      "class-specific latent levels and changes refer to",
      "the average predictor values in the analytical sample;",
      "class-specific latent levels, latent changes, and",
      "all pairwise change contrasts are defined in",
      "MODEL CONSTRAINT"
    )
  )
}


##### DEFINE M25A-M25B SETTINGS #####

outcome_combined_biomarker_model_settings <- list(
  
  M25a = create_combined_biomarker_settings(
    model_number =
      "25a",
    
    prs_variable =
      "prs_eau",
    
    prs_description =
      "European-ancestry MDD polygenic risk",
    
    input_filename =
      "25a_sdq_lcs_class_effects_hcc_prs_eau_mo.inp"
  ),
  
  M25b = create_combined_biomarker_settings(
    model_number =
      "25b",
    
    prs_variable =
      "prs_mau",
    
    prs_description =
      "multi-ancestry MDD polygenic risk",
    
    input_filename =
      "25b_sdq_lcs_class_effects_hcc_prs_mau_mo.inp"
  )
)

##### ADD M25A-M25B TO COMMON MODEL SETTINGS #####

outcome_model_settings[
  names(
    outcome_combined_biomarker_model_settings
  )
] <- outcome_combined_biomarker_model_settings


##### CREATE M25A-M25B INPUTS #####

outcome_combined_biomarker_model_files <- lapply(
  outcome_combined_biomarker_model_settings,
  function(settings) {
    do.call(
      create_class_lcs_input,
      settings
    )
  }
)

outcome_combined_biomarker_input_files <- unname(
  vapply(
    outcome_combined_biomarker_model_files,
    identity,
    character(1)
  )
)

input_file_m25a <-
  outcome_combined_biomarker_model_files$M25a

input_file_m25b <-
  outcome_combined_biomarker_model_files$M25b


##### CHECK M25A-M25B INPUTS #####

missing_combined_biomarker_input_files <-
  outcome_combined_biomarker_input_files[
    !file.exists(
      outcome_combined_biomarker_input_files
    )
  ]

if (
  length(
    missing_combined_biomarker_input_files
  ) > 0L
) {
  stop(
    "The following combined-biomarker input files are missing:\n",
    paste(
      missing_combined_biomarker_input_files,
      collapse = "\n"
    )
  )
}


##### RUN M25A-M25B #####

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
    target =
      outcome_combined_biomarker_input_files,
    
    replaceOutfile =
      "always",
    
    showOutput =
      FALSE,
    
    quiet =
      FALSE
  )
  
} else {
  
  message(
    paste(
      "M25a-M25b inputs were created,",
      "but Mplus execution was skipped."
    )
  )
}
