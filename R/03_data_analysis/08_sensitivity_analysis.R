# ============================================================
# M20+ Sensitivitätsanalysen
# ============================================================

# Projektpfad: Code und Skripte
PROJECT_DIR <- "C:/Users/keil/Documents/main_outcome_amis2"

# Datenpfad: SeaDrive
DATA_PREP_DIR <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/",
  "Meine Bibliotheken/MAIN OUTCOME/02_data/02_data_Prep"
)

source(file.path(
  PROJECT_DIR,
  "R", "03_data_analysis", "00_setup_standardized.R"
))


# Finalen M18+SES-Datensatz suchen
data_file <- list.files(
  path        = DATA_PREP_DIR,
  pattern     = "^AMIS_mplus_dataset_m18_ses_mo\\.rds$",
  recursive   = TRUE,
  full.names  = TRUE,
  ignore.case = TRUE
)

if (length(data_file) != 1L) {
  stop(
    "Es wurde nicht genau eine passende Datei gefunden.\n",
    paste(data_file, collapse = "\n")
  )
}

dat_m20 <- readRDS(data_file[[1]])
dat_m20 <- as.data.frame(dat_m20)

cat("Datei:", data_file[[1]], "\n")
cat("Dimension:", nrow(dat_m20), "x", ncol(dat_m20), "\n")

"mt_class_mo" %in% names(dat_m20)

class_candidates <- grep(
  "class|klass|m18|prob|mt_cl",
  names(dat_m20),
  value = TRUE,
  ignore.case = TRUE
)

class_candidates

# Zusätzlich die zuletzt angefügten Variablen anzeigen
tail(names(dat_m20), 40)


table(dat_m20$mo_cls, useNA = "ifany")

with(
  dat_m20,
  table(stat_t5, mo_cls, useNA = "ifany")
)

anyDuplicated(dat_m20$SIC_N)




relevant_vars <- grep(
  "sdq|emo|hyp|con|ext|diag|ksads|papa|icd|dsm",
  names(dat_m20),
  value = TRUE,
  ignore.case = TRUE
)
relevant_vars


############################# BUILD CUT OFF ###########

to_numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

# Verwendete SDQ-Skalen und untere Auffälligkeitsgrenzen
sdq_rules <- data.frame(
  variable = c(
    "emo_b2", "con_b2", "hyp_b2",
    "emo_p2", "con_p2", "hyp_p2",
    "emo_k2", "con_k2", "hyp_k2",
    "emo_t2", "con_t2", "hyp_t2"
  ),
  informant = rep(c("b", "p", "k", "t"), each = 3),
  cut_3band = c(
    4, 3, 6,   # Bezugsperson
    4, 3, 6,   # Eltern
    6, 4, 6,   # Selbstbericht
    5, 3, 6    # Lehrkraft
  ),
  cut_4band = c(
    4, 3, 6,   # Bezugsperson
    4, 3, 6,   # Eltern
    5, 4, 6,   # Selbstbericht
    4, 3, 6    # Lehrkraft
  )
)

stopifnot(all(sdq_rules$variable %in% names(dat_m20)))

# Diagnose prüfen: erwartet werden 0 = nein und 1 = ja
diag01 <- to_numeric(dat_m20$diag_vor)

bad_diag <- setdiff(unique(na.omit(diag01)), c(0, 1))
if (length(bad_diag) > 0L) {
  stop(
    "diag_vor ist nicht sauber 0/1 codiert. Weitere Werte: ",
    paste(bad_diag, collapse = ", ")
  )
}

if ("has_diag" %in% names(dat_m20)) {
  print(table(
    has_diag = dat_m20$has_diag,
    diag_vor = dat_m20$diag_vor,
    useNA = "ifany"
  ))
}

# SDQ-Werte als numerische Matrix
sdq_scores <- do.call(
  cbind,
  lapply(dat_m20[sdq_rules$variable], to_numeric)
)
colnames(sdq_scores) <- sdq_rules$variable

if (!all(is.na(sdq_scores) | (sdq_scores >= 0 & sdq_scores <= 10))) {
  stop("Mindestens ein SDQ-Wert liegt außerhalb des gültigen Bereichs 0–10.")
}

# Auffälligkeit je einzelner Skala
sdq_positive_3 <- sweep(
  sdq_scores, 2, sdq_rules$cut_3band, FUN = ">="
)

sdq_positive_4 <- sweep(
  sdq_scores, 2, sdq_rules$cut_4band, FUN = ">="
)

# Für eine negative Einstufung muss mindestens ein Informant
# alle drei verwendeten Skalen beantwortet haben
vars_by_informant <- split(
  sdq_rules$variable,
  sdq_rules$informant
)

complete_by_informant <- sapply(
  vars_by_informant,
  function(v) {
    rowSums(!is.na(sdq_scores[, v, drop = FALSE])) == length(v)
  }
)

has_complete_informant <- rowSums(complete_by_informant) > 0L

make_binary_case <- function(sdq_positive) {
  
  any_sdq_positive <- rowSums(sdq_positive, na.rm = TRUE) > 0L
  
  positive <- (!is.na(diag01) & diag01 == 1) |
    any_sdq_positive
  
  negative <- (!is.na(diag01) & diag01 == 0) |
    FALSE
  
  negative <- negative &
    !any_sdq_positive &
    has_complete_informant
  
  out <- rep(NA_integer_, length(diag01))
  out[positive] <- 1L
  out[negative] <- 0L
  out
}

dat_m20_cut <- dat_m20

dat_m20_cut$cas3_t2 <- make_binary_case(sdq_positive_3)
dat_m20_cut$cas4_t2 <- make_binary_case(sdq_positive_4)

# Verteilungen kontrollieren
table(dat_m20_cut$cas3_t2, useNA = "ifany")
table(dat_m20_cut$cas4_t2, useNA = "ifany")

table(
  cas3_t2 = dat_m20_cut$cas3_t2,
  cas4_t2 = dat_m20_cut$cas4_t2,
  useNA = "ifany"
)

############## PLAUSIBILITY ###############

# Diagnose- versus SDQ-Beitrag
diag_positive <- !is.na(diag01) & diag01 == 1
sdq_positive  <- rowSums(sdq_positive_3, na.rm = TRUE) > 0

source_3band <- ifelse(
  diag_positive & sdq_positive, "Diagnose + SDQ",
  ifelse(
    diag_positive, "nur Diagnose",
    ifelse(
      sdq_positive, "nur SDQ",
      ifelse(
        dat_m20_cut$cas3_t2 == 0,
        "unauffällig",
        "nicht klassifizierbar"
      )
    )
  )
)

table(source_3band, useNA = "ifany")

sdq_audit <- data.frame(
  variable = colnames(sdq_scores),
  n_available = colSums(!is.na(sdq_scores)),
  n_positive = colSums(sdq_positive_3, na.rm = TRUE)
)

sdq_audit$percent_positive <- round(
  100 * sdq_audit$n_positive / sdq_audit$n_available,
  1
)

sdq_audit

addmargins(table(
  mo_cls  = dat_m20_cut$mo_cls,
  cas3_t2 = dat_m20_cut$cas3_t2,
  useNA   = "ifany"
))

diag_cross_maltreated <- with(
  subset(dat_m20_cut, mo_cls %in% c(2, 3, 4)),
  addmargins(table(
    mo_cls,
    diag_vor,
    useNA = "ifany"
  ))
)

diag_cross_maltreated

# Zeilenprozente
with(
  subset(dat_m20_cut, mo_cls %in% c(2, 3, 4)),
  round(
    100 * prop.table(
      table(mo_cls, diag_vor),
      margin = 1
    ),
    1
  )
)

# Pro Informant: mindestens eine Skala borderline/auffällig
positive_by_informant <- sapply(
  vars_by_informant,
  function(v) {
    rowSums(
      sdq_positive_3[, v, drop = FALSE],
      na.rm = TRUE
    ) > 0L
  }
)

n_positive_informants <- rowSums(positive_by_informant)
n_complete_informants <- rowSums(complete_by_informant)

# Reine SDQ-Variable:
# 1 = mindestens zwei Informant:innen auffällig
# 0 = mindestens zwei vollständige Urteile, aber weniger als zwei auffällig
# NA = nicht ausreichend beurteilbar
dat_m20_cut$sdq2_t2 <- NA_integer_

dat_m20_cut$sdq2_t2[
  n_positive_informants >= 2L
] <- 1L

dat_m20_cut$sdq2_t2[
  n_positive_informants < 2L &
    n_complete_informants >= 2L
] <- 0L


# Kombiniert mit Diagnose
diag_positive <- !is.na(diag01) & diag01 == 1
diag_negative <- !is.na(diag01) & diag01 == 0

sdq2_positive <- !is.na(dat_m20_cut$sdq2_t2) &
  dat_m20_cut$sdq2_t2 == 1

sdq2_negative <- !is.na(dat_m20_cut$sdq2_t2) &
  dat_m20_cut$sdq2_t2 == 0

dat_m20_cut$cas2_t2 <- NA_integer_

dat_m20_cut$cas2_t2[
  diag_positive | sdq2_positive
] <- 1L

dat_m20_cut$cas2_t2[
  diag_negative & sdq2_negative
] <- 0L


# Kontrollen
table(n_positive_informants, useNA = "ifany")
table(dat_m20_cut$sdq2_t2, useNA = "ifany")
table(dat_m20_cut$cas2_t2, useNA = "ifany")

addmargins(table(
  mo_cls  = dat_m20_cut$mo_cls,
  cas2_t2 = dat_m20_cut$cas2_t2,
  useNA   = "ifany"
))

addmargins(table(
  mo_cls  = dat_m20_cut$mo_cls,
  sdq2_t2 = dat_m20_cut$sdq2_t2,
  useNA   = "ifany"
))


# ############## CREATE OUTPUT ##############
# OUTPUT_DIR <- "C:/MPLUS/Inputs"
# dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
# 
# output_stub <- file.path(
#   OUTPUT_DIR,
#   "AMIS_m20_multigroup_caseness_t2"
# )
# 
# # R-Version
# saveRDS(
#   dat_m20_cut,
#   paste0(output_stub, ".rds")
# )
# 
# # Mplus-Version
# mplus_dat <- dat_m20_cut
# 
# logical_cols <- vapply(mplus_dat, is.logical, logical(1))
# mplus_dat[logical_cols] <- lapply(
#   mplus_dat[logical_cols],
#   as.integer
# )
# 
# non_numeric <- names(mplus_dat)[
#   !vapply(mplus_dat, is.numeric, logical(1))
# ]
# 
# if (length(non_numeric) > 0L) {
#   stop(
#     "Nichtnumerische Mplus-Variablen: ",
#     paste(non_numeric, collapse = ", ")
#   )
# }
# 
# write.table(
#   mplus_dat,
#   file      = paste0(output_stub, ".dat"),
#   sep       = "\t",
#   row.names = FALSE,
#   col.names = FALSE,
#   quote     = FALSE,
#   na        = "-9999"
# )
# 
# # Variablennamen passend zur Spaltenreihenfolge
# variable_names <- names(mplus_dat)
# endings <- rep("", length(variable_names))
# endings[length(endings)] <- ";"
# 
# writeLines(
#   c("NAMES ARE", paste0(variable_names, endings)),
#   paste0(output_stub, "_names.inp")
# )
# 
# cat("Gespeichert:\n")
# cat(paste0(output_stub, ".rds"), "\n")
# cat(paste0(output_stub, ".dat"), "\n")
# cat(paste0(output_stub, "_names.inp"), "\n")


#-------------------------------------------------------------------------
##### SENSITIVITY: MULTIGROUP LCS BY AMIS-I DIAGNOSIS #####
#-------------------------------------------------------------------------

##### RECOVER AND VALIDATE OBJECTS CREATED IN THE SETUP HEADER #####

if (!exists("mplus_names", inherits = TRUE)) {
  
  if (!exists("mplus_names_file_local", inherits = TRUE)) {
    stop(
      "Neither mplus_names nor mplus_names_file_local exists. ",
      "Run the complete setup header before this block."
    )
  }
  
  if (!file.exists(mplus_names_file_local)) {
    stop(
      "The local Mplus names file does not exist: ",
      mplus_names_file_local
    )
  }
  
  mplus_names <- readRDS(
    mplus_names_file_local
  )
}

if (!exists("mplus_data_file", inherits = TRUE)) {
  stop(
    "mplus_data_file does not exist. ",
    "Run the complete setup header before this block."
  )
}

if (!file.exists(mplus_data_file)) {
  stop(
    "The local Mplus data file does not exist: ",
    mplus_data_file
  )
}

if (!exists("mplus_fields", inherits = TRUE)) {
  mplus_fields <- count.fields(
    mplus_data_file,
    sep = "",
    blank.lines.skip = TRUE
  )
}

if (!exists("names_syntax", inherits = TRUE)) {
  names_syntax <- paste(
    wrap_mplus_names(
      mplus_names,
      max_width = 88
    ),
    collapse = "\n"
  )
}

stopifnot(
  is.character(mplus_names),
  length(mplus_names) > 0L,
  !anyNA(mplus_names),
  length(mplus_fields) > 0L,
  all(mplus_fields == length(mplus_names))
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
    mo_cls >= 2 &
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
      levels = 2:4
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
  c3 = 0;
  c4 = 0;

  IF (mo_cls EQ 3) THEN c3 = 1;
  IF (mo_cls EQ 4) THEN c4 = 1;
"

diag_mg_group_model_syntax <- "
MODEL nodx:

  ! Class 2 is the reference trajectory in the no-diagnosis group

  [EXT2@0];
  [EMO2@0];

  [d_ext] (mdx0);
  [d_emo] (mdm0);

  EXT2 ON c3 (e2c30);
  EXT2 ON c4 (e2c40);

  EMO2 ON c3 (m2c30);
  EMO2 ON c4 (m2c40);

  d_ext ON c3 (dxc30);
  d_ext ON c4 (dxc40);

  d_emo ON c3 (dmc30);
  d_emo ON c4 (dmc40);

MODEL dx:

  ! Class 2 is the reference trajectory in the diagnosis group

  [EXT2] (be21);
  [EMO2] (bm21);

  [d_ext] (mdx1);
  [d_emo] (mdm1);

  EXT2 ON c3 (e2c31);
  EXT2 ON c4 (e2c41);

  EMO2 ON c3 (m2c31);
  EMO2 ON c4 (m2c41);

  d_ext ON c3 (dxc31);
  d_ext ON c4 (dxc41);

  d_emo ON c3 (dmc31);
  d_emo ON c4 (dmc41);
"

diag_mg_constraint_syntax <- "
MODEL CONSTRAINT:

  NEW(
    ex2_2n ex2_3n ex2_4n
    ex2_2d ex2_3d ex2_4d
    ex5_2n ex5_3n ex5_4n
    ex5_2d ex5_3d ex5_4d

    em2_2n em2_3n em2_4n
    em2_2d em2_3d em2_4d
    em5_2n em5_3n em5_4n
    em5_2d em5_3d em5_4d

    dex_2n dex_3n dex_4n
    dex_2d dex_3d dex_4d
    dem_2n dem_3n dem_4n
    dem_2d dem_3d dem_4d

    gdx_2 gdx_3 gdx_4
    gdm_2 gdm_3 gdm_4

    intx_3 intx_4 intx_43
    intm_3 intm_4 intm_43
  );

  ! Class-specific T2 levels: no diagnosis

  ex2_2n = 0;
  ex2_3n = e2c30;
  ex2_4n = e2c40;

  em2_2n = 0;
  em2_3n = m2c30;
  em2_4n = m2c40;

  ! Class-specific T2 levels: diagnosis

  ex2_2d = be21;
  ex2_3d = be21 + e2c31;
  ex2_4d = be21 + e2c41;

  em2_2d = bm21;
  em2_3d = bm21 + m2c31;
  em2_4d = bm21 + m2c41;

  ! Class-specific latent changes: no diagnosis

  dex_2n = mdx0;
  dex_3n = mdx0 + dxc30;
  dex_4n = mdx0 + dxc40;

  dem_2n = mdm0;
  dem_3n = mdm0 + dmc30;
  dem_4n = mdm0 + dmc40;

  ! Class-specific latent changes: diagnosis

  dex_2d = mdx1;
  dex_3d = mdx1 + dxc31;
  dex_4d = mdx1 + dxc41;

  dem_2d = mdm1;
  dem_3d = mdm1 + dmc31;
  dem_4d = mdm1 + dmc41;

  ! Class-specific T5 levels

  ex5_2n = ex2_2n + dex_2n;
  ex5_3n = ex2_3n + dex_3n;
  ex5_4n = ex2_4n + dex_4n;

  ex5_2d = ex2_2d + dex_2d;
  ex5_3d = ex2_3d + dex_3d;
  ex5_4d = ex2_4d + dex_4d;

  em5_2n = em2_2n + dem_2n;
  em5_3n = em2_3n + dem_3n;
  em5_4n = em2_4n + dem_4n;

  em5_2d = em2_2d + dem_2d;
  em5_3d = em2_3d + dem_3d;
  em5_4d = em2_4d + dem_4d;

  ! Diagnosis-group differences in change within each class
  ! Positive values indicate more positive change in the diagnosis group

  gdx_2 = dex_2d - dex_2n;
  gdx_3 = dex_3d - dex_3n;
  gdx_4 = dex_4d - dex_4n;

  gdm_2 = dem_2d - dem_2n;
  gdm_3 = dem_3d - dem_3n;
  gdm_4 = dem_4d - dem_4n;

  ! Class x diagnosis interactions on latent change
  ! Difference in a class contrast between diagnosis groups

  intx_3 = dxc31 - dxc30;
  intx_4 = dxc41 - dxc40;

  intm_3 = dmc31 - dmc30;
  intm_4 = dmc41 - dmc40;

  ! Difference in the class-4 versus class-3 contrast

  intx_43 = (dxc41 - dxc31) - (dxc40 - dxc30);
  intm_43 = (dmc41 - dmc31) - (dmc40 - dmc30);

MODEL TEST:

  0 = dxc31 - dxc30;
  0 = dxc41 - dxc40;
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
    "    (mo_cls GE 2) AND\n",
    "    (mo_cls LE 4) AND\n",
    "    (diag_vor GE 0) AND\n",
    "    (diag_vor LE 1);\n\n",
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
  
  github_input_file <- file.path(
    github_m18_lcs_dir,
    input_filename
  )
  
  dir.create(
    dirname(local_input_file),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    dirname(github_input_file),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  writeLines(
    input_syntax,
    con = local_input_file,
    useBytes = TRUE
  )
  
  if (
    normalizePath(
      local_input_file,
      winslash = "/",
      mustWork = FALSE
    ) !=
    normalizePath(
      github_input_file,
      winslash = "/",
      mustWork = FALSE
    )
  ) {
    writeLines(
      input_syntax,
      con = github_input_file,
      useBytes = TRUE
    )
  }
  
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

# Select c("S20", "S21", "S22") for the complete series.
# For the unadjusted counterpart of M20 only, use "S20".
diag_mg_models_to_create <- c(
  "S20",
  "S21",
  "S22"
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
    logFile = FALSE,
    quiet = FALSE
  )
  
} else {
  
  message(
    "The multigroup sensitivity inputs were created, ",
    "but Mplus execution was skipped."
  )
}
