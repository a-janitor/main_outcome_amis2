##### DEFINE MPLUS INPUT DIRECTORY ####
mplus_input_dir <- "C:/MPLUS/Inputs"
mplus_results_dir <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/03_results/Mplus/02_invariance"
)

dir.create(
  mplus_input_dir,
  recursive = TRUE,
  showWarnings = FALSE
)



##### COPY MPLUS DATASET TO INPUT DIRECTORY ####

file.copy(
  from = paste0(
    "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
    "MAIN OUTCOME/02_data/02_data_Prep/MPlus_Dataset/",
    "AMIS_mplus_dataset.dat"
  ),
  to = file.path(
    mplus_input_dir,
    "AMIS_mplus_dataset.dat"
  ),
  overwrite = TRUE
)

##### CREATE MPLUS NAMES LIST ####
names_syntax <- paste(
  names(dat_mplus),
  collapse = "\n    "
)

##### CREATE CONFIGURAL MODEL ####
configural_input <- paste0(
  "TITLE:
  SDQ CONFIGURAL MEASUREMENT MODEL T2-T5;

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

USEOBSERVATIONS = stat_t5 NE 0;

  IDVARIABLE = SIC_N;

  MISSING = ALL (-999);

ANALYSIS:
  ESTIMATOR = MLR;
  COVERAGE = 0.01;

MODEL:

  ! Configural CFA without invariance constraints
  ! or correlated indicator residuals

  EXT2 BY
    hyp_b2@1
    hyp_k2
    hyp_p2
    con_b2
    con_k2
    con_p2;

  EXT5 BY
    hyp_b5@1
    hyp_k5
    hyp_p5
    con_b5
    con_k5
    con_p5;

  EMO2 BY
    emo_b2@1
    emo_k2
    emo_p2;

  EMO5 BY
    emo_b5@1
    emo_k5
    emo_p5;

  EXT2 WITH EMO2 EXT5 EMO5;
  EXT5 WITH EMO2 EMO5;
  EMO2 WITH EMO5;

OUTPUT:
  SAMPSTAT
  STANDARDIZED
  TECH1
  TECH4
  MODINDICES(10);
"
)

##### SAVE MPLUS INPUT ####
writeLines(
  configural_input,
  con = file.path(
    mplus_input_dir,
    "01_sdq_configural.inp"
  )
)


###################################### SAVE OUTPUT #############

##### FIND MPLUS OUTPUT FILES ####
output_files <- list.files(
  mplus_input_dir,
  pattern = "\\.(out|gh5)$",
  full.names = TRUE,
  ignore.case = TRUE
)

##### COPY OUTPUT FILES TO RESULTS ####
file.copy(
  from = output_files,
  to = file.path(
    mplus_results_dir,
    basename(output_files)
  ),
  overwrite = TRUE
)

##### REMOVE LOCAL OUTPUT COPIES ####
file.remove(output_files)
