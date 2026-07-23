##### PACKAGES #####

library(tidyverse)
library(readxl)
library(writexl)
library(gdtools)
library(officer)
library(flextable)

packageVersion("gdtools")
packageVersion("flextable")

options(scipen = 999)


#-----------------------------------------------------------------------
##### SETUP #####
#-----------------------------------------------------------------------

##### ROOT PATHS #####

seadrive_root <- "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken"
github_root   <- "C:/Users/keil/Documents/main_outcome_amis2"

main_outcome_dir <- file.path(
  seadrive_root,
  "MAIN OUTCOME"
)


##### DATA PATHS #####

data_prep_dir <- file.path(
  main_outcome_dir,
  "02_data",
  "02_data_Prep"
)

m18_excel_file <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes.xlsx"
)

m18_prepared_rds_file <- file.path(
  data_prep_dir,
  "AMIS_M18_analysis_prepared.rds"
)

seadrive_mplus_data_dir <- file.path(
  data_prep_dir,
  "MPlus_Dataset"
)

mplus_names_file_m18 <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names_m18.rds"
)


##### LOCAL MPLUS PATH #####

mplus_input_dir <- "C:/MPLUS/Inputs"


##### SEADRIVE RESULTS PATHS #####

mplus_results_dir <- file.path(
  main_outcome_dir,
  "03_results",
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


##### CLASS-ANALYSIS RESULTS PATHS #####

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

##### GITHUB PATHS #####

github_mplus_dir <- file.path(
  github_root,
  "Mplus"
)

github_lcs_dir <- file.path(
  github_mplus_dir,
  "03_lcs"
)

github_maltreatment_dir <- file.path(
  github_mplus_dir,
  "04_maltreatment"
)

github_m18_lcs_dir <- file.path(
  github_maltreatment_dir,
  "02_classes_predicting_change"
)


##### CREATE REQUIRED DIRECTORIES #####

dirs <- c(
  mplus_input_dir,
  seadrive_mplus_data_dir,
  mplus_results_dir_measurement,
  mplus_results_dir_invariance,
  mplus_results_dir_lcs,
  mplus_results_dir_maltreatment,
  mplus_results_dir_biology,
  m18_class_checks_dir,
  m18_lcs_results_dir,
  github_mplus_dir,
  github_lcs_dir,
  github_maltreatment_dir,
  github_m18_lcs_dir
)

walk(
  dirs,
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


##### CHECK INPUT FILE #####

if (!file.exists(m18_excel_file)) {
  stop(
    "Input file not found: ",
    m18_excel_file
  )
}


#-----------------------------------------------------------------------
##### DEFINE VARIABLES #####
#-----------------------------------------------------------------------

##### SDQ VARIABLES #####

informants <- c("b", "k", "p")
timepoints <- c("t2", "t5")

sdq_grid <- expand_grid(
  informant = informants,
  timepoint = timepoints
)

emotion_vars <- sdq_grid %>%
  transmute(
    variable = paste0(
      "sdq_emotion_",
      informant,
      "_",
      timepoint
    )
  ) %>%
  pull(variable)

conduct_vars <- sdq_grid %>%
  transmute(
    variable = paste0(
      "sdq_conduct_",
      informant,
      "_",
      timepoint
    )
  ) %>%
  pull(variable)

hyper_vars <- sdq_grid %>%
  transmute(
    variable = paste0(
      "sdq_hyper_",
      informant,
      "_",
      timepoint
    )
  ) %>%
  pull(variable)

ext_vars <- sdq_grid %>%
  transmute(
    variable = paste0(
      "sdq_ext_",
      informant,
      "_",
      timepoint
    )
  ) %>%
  pull(variable)


##### MALTREATMENT VARIABLES #####

mal_base <- c(
  "mt_mal_status",
  "mt_d2_abuse_mal_status",
  "mt_d2_neglect_mal_status",
  "mt_d2_emotion_mal_status",
  "mt_sub_km_mal_status",
  "mt_sub_sm_mal_status",
  "mt_sub_em_mal_status",
  "mt_sub_mv_mal_status",
  "mt_sub_mb_mal_status",
  "mt_sub_mre_bm_mal_status"
)

mal_t1_vars <- paste0(
  mal_base,
  "_t1"
)

mal_t2all_vars <- paste0(
  mal_base,
  "_t2all"
)

mal_harm_vars <- paste0(
  mal_base,
  "_harm"
)

mal_labels <- c(
  mt_mal_status_harm =
    "Any maltreatment, n (%)",
  
  mt_d2_abuse_mal_status_harm =
    "Abuse, n (%)",
  
  mt_d2_neglect_mal_status_harm =
    "Neglect, n (%)",
  
  mt_d2_emotion_mal_status_harm =
    "Emotional maltreatment dimension, n (%)",
  
  mt_sub_km_mal_status_harm =
    "Physical abuse, n (%)",
  
  mt_sub_sm_mal_status_harm =
    "Sexual abuse, n (%)",
  
  mt_sub_em_mal_status_harm =
    "Emotional maltreatment subtype, n (%)",
  
  mt_sub_mv_mal_status_harm =
    "Physical neglect, n (%)",
  
  mt_sub_mb_mal_status_harm =
    "Lack of supervision, n (%)",
  
  mt_sub_mre_bm_mal_status_harm =
    "Moral, legal, or educational maltreatment, n (%)"
)


##### CLASS LABELS #####

class_labels <- c(
  "Low and stable",
  "Elevated and declining",
  "High early burden with later rebound"
)

class_columns <- c(
  "Overall",
  "Class 1: Low and stable",
  "Class 2: Elevated and declining",
  "Class 3: High early burden with later rebound"
)


#-----------------------------------------------------------------------
##### READ AND CHECK DATA #####
#-----------------------------------------------------------------------

d <- read_excel(
  m18_excel_file,
  na = c("", "NA")
)

##### REQUIRED SOURCE VARIABLES #####

req <- unique(
  c(
    "sic",
    "sdq_sex",
    "mt_status_t5",
    "mt_age_t2",
    "mt_age_t5",
    "mt_class",
    "mt_prob_class1",
    "mt_prob_class2",
    "mt_prob_class3",
    "mt_prob_max",
    emotion_vars,
    conduct_vars,
    hyper_vars,
    mal_t1_vars,
    mal_t2all_vars
  )
)

miss <- setdiff(
  req,
  names(d)
)

if (length(miss) > 0) {
  stop(
    "Missing variables: ",
    paste(miss, collapse = ", ")
  )
}


##### CHECK ID AND CLASS VALUES #####

if (anyNA(d$sic)) {
  stop("Missing values found in sic.")
}

if (anyDuplicated(d$sic) > 0) {
  stop("Duplicated values found in sic.")
}

if (!all(na.omit(d$mt_class) %in% 1:3)) {
  stop("mt_class contains values other than 1, 2, or 3.")
}


#-----------------------------------------------------------------------
##### CREATE ANALYSIS VARIABLES #####
#-----------------------------------------------------------------------

d <- d %>%
  mutate(
    mt_class = as.integer(mt_class),
    
    mt_class_label = case_when(
      mt_class == 1 ~ class_labels[1],
      mt_class == 2 ~ class_labels[2],
      mt_class == 3 ~ class_labels[3],
      TRUE          ~ NA_character_
    ),
    
    # Class 1 is the reference category
    mt_class2 = as.integer(
      mt_class == 2
    ),
    
    mt_class3 = as.integer(
      mt_class == 3
    ),
    
    # 0 = male, 1 = female
    female = case_when(
      sdq_sex == 1 ~ 0L,
      sdq_sex == 2 ~ 1L,
      TRUE         ~ NA_integer_
    ),
    
    # Reconstruct the Mplus T5 participation variable
    stat_t5 = case_when(
      mt_status_t5 == "drop out"  ~ 0L,
      mt_status_t5 == "completed" ~ 2L,
      !is.na(mt_status_t5)        ~ 1L,
      TRUE                        ~ NA_integer_
    ),
    
    # Corresponds to:
    # USEOBSERVATIONS ARE stat_t5 NE 0
    lcs_included = as.integer(
      !is.na(stat_t5) &
        stat_t5 != 0
    ),
    
    # Indicator of the available maltreatment assessment window
    mt_info_t2all = as.integer(
      !is.na(mt_mal_status_t2all)
    )
  )


##### CREATE MANIFEST EXTERNALIZING SCORES #####

for (i in informants) {
  
  for (tp in timepoints) {
    
    conduct_var <- paste0(
      "sdq_conduct_",
      i,
      "_",
      tp
    )
    
    hyper_var <- paste0(
      "sdq_hyper_",
      i,
      "_",
      tp
    )
    
    ext_var <- paste0(
      "sdq_ext_",
      i,
      "_",
      tp
    )
    
    d[[ext_var]] <-
      d[[conduct_var]] +
      d[[hyper_var]]
  }
}


##### CREATE HARMONIZED MALTREATMENT VARIABLES #####

for (v in mal_base) {
  
  t1_var <- paste0(
    v,
    "_t1"
  )
  
  t2all_var <- paste0(
    v,
    "_t2all"
  )
  
  harm_var <- paste0(
    v,
    "_harm"
  )
  
  d[[harm_var]] <- coalesce(
    d[[t2all_var]],
    d[[t1_var]]
  )
}


#-----------------------------------------------------------------------
##### DEFINE ANALYTIC SAMPLES #####
#-----------------------------------------------------------------------

d <- d %>%
  mutate(
    mt_age_t2_years = mt_age_t2 / 12,
    mt_age_t5_years = mt_age_t5 / 12
  )

##### FULL M18 CLASS SAMPLE #####

d_cls <- d %>%
  filter(
    !is.na(mt_class)
  )


##### LCS SAMPLE WITH M18 CLASS #####

d_lcs <- d %>%
  filter(
    lcs_included == 1,
    !is.na(mt_class)
  )


#-----------------------------------------------------------------------
##### ESSENTIAL CLASSIFICATION CHECK #####
#-----------------------------------------------------------------------

if (nrow(d_cls) == 0) {
  stop("No cases with an M18 class assignment found.")
}

p <- d_cls %>%
  select(
    mt_prob_class1,
    mt_prob_class2,
    mt_prob_class3
  ) %>%
  as.matrix()

if (anyNA(p)) {
  stop("Missing posterior probabilities found in M18 class sample.")
}

if (anyNA(d_cls$mt_prob_max)) {
  stop("Missing mt_prob_max values found in M18 class sample.")
}

if (!all(p >= 0 & p <= 1)) {
  stop("Posterior probabilities outside the range 0 to 1.")
}

prob_sum_deviation <- max(
  abs(
    rowSums(p) - 1
  )
)

if (prob_sum_deviation > .0011) {
  stop(
    "Posterior probabilities deviate from 1. ",
    "Maximum deviation: ",
    round(prob_sum_deviation, 4)
  )
}

if (
  !all(
    d_cls$mt_class ==
    max.col(
      p,
      ties.method = "first"
    )
  )
) {
  stop("Class assignments do not match the largest posterior probability.")
}

prob_max_deviation <- max(
  abs(
    d_cls$mt_prob_max -
      apply(
        p,
        1,
        max
      )
  )
)

if (prob_max_deviation > .0011) {
  stop(
    "mt_prob_max does not match the largest posterior probability. ",
    "Maximum deviation: ",
    round(prob_max_deviation, 4)
  )
}


#-----------------------------------------------------------------------
##### SAMPLE AND CLASS OVERVIEW #####
#-----------------------------------------------------------------------

##### SAMPLE SIZES #####

n_samp <- tibble(
  sample = c(
    "Full Excel dataset",
    "Full M18 class sample",
    "LCS sample: stat_t5 NE 0",
    "LCS sample with M18 class"
  ),
  
  n = c(
    nrow(d),
    nrow(d_cls),
    sum(d$lcs_included),
    nrow(d_lcs)
  )
)


##### CLASS KEY #####

class_key <- d_cls %>%
  count(
    mt_class,
    mt_class_label,
    name = "n"
  ) %>%
  mutate(
    percent = 100 * n / sum(n)
  ) %>%
  arrange(mt_class)


##### CLASSIFICATION AND RETENTION OVERVIEW #####

sum_cls <- d_cls %>%
  group_by(
    mt_class,
    mt_class_label
  ) %>%
  summarise(
    n = n(),
    
    n_lcs = sum(
      lcs_included
    ),
    
    retention_pct = 100 * mean(
      lcs_included
    ),
    
    prob_mean = mean(
      mt_prob_max
    ),
    
    prob_sd = sd(
      mt_prob_max
    ),
    
    prob_min = min(
      mt_prob_max
    ),
    
    prob_ge70_pct = 100 * mean(
      mt_prob_max >= .70
    ),
    
    avepp_class1 = mean(
      mt_prob_class1
    ),
    
    avepp_class2 = mean(
      mt_prob_class2
    ),
    
    avepp_class3 = mean(
      mt_prob_class3
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    m18_pct = 100 * n / sum(n),
    lcs_pct = 100 * n_lcs / sum(n_lcs)
  ) %>%
  relocate(
    m18_pct,
    .after = n
  ) %>%
  relocate(
    lcs_pct,
    .after = n_lcs
  ) %>%
  arrange(mt_class)


#-----------------------------------------------------------------------
##### RETENTION TEST #####
#-----------------------------------------------------------------------

ret_tab <- table(
  d_cls$mt_class,
  d_cls$lcs_included
)

ret_test <- chisq.test(
  ret_tab,
  correct = FALSE
)

ret_v <- sqrt(
  unname(ret_test$statistic) /
    (
      sum(ret_tab) *
        min(
          nrow(ret_tab) - 1,
          ncol(ret_tab) - 1
        )
    )
)

ret_counts <- as.data.frame.matrix(
  ret_tab
) %>%
  rownames_to_column(
    "mt_class"
  )

sum_ret <- tibble(
  chi_square = unname(
    ret_test$statistic
  ),
  
  df = unname(
    ret_test$parameter
  ),
  
  p = ret_test$p.value,
  
  cramers_v = ret_v
)


#-----------------------------------------------------------------------
##### APA FORMATTING FUNCTIONS #####
#-----------------------------------------------------------------------

fmt_ms <- function(x) {
  
  if (sum(!is.na(x)) == 0) {
    return("—")
  }
  
  sprintf(
    "%.2f (%.2f)",
    mean(
      x,
      na.rm = TRUE
    ),
    sd(
      x,
      na.rm = TRUE
    )
  )
}


fmt_np <- function(x, positive = 1) {
  
  n_available <- sum(
    !is.na(x)
  )
  
  if (n_available == 0) {
    return("—")
  }
  
  n_positive <- sum(
    x == positive,
    na.rm = TRUE
  )
  
  sprintf(
    "%d (%.1f%%)",
    n_positive,
    100 * n_positive / n_available
  )
}


fmt_p <- function(x) {
  
  if (is.na(x)) {
    return("")
  }
  
  if (x < .001) {
    return("< .001")
  }
  
  sub(
    "^0",
    "",
    sprintf(
      "%.3f",
      x
    )
  )
}


fmt_effect <- function(symbol, x) {
  
  if (is.na(x)) {
    return("")
  }
  
  paste0(
    symbol,
    " = ",
    sub(
      "^0",
      "",
      sprintf(
        "%.3f",
        x
      )
    )
  )
}


#-----------------------------------------------------------------------
##### TABLE FUNCTIONS #####
#-----------------------------------------------------------------------

##### EXTRACT FORMATTED VALUES BY CLASS #####

get_class_values <- function(var, formatter) {
  
  values <- c(
    formatter(d_cls[[var]]),
    
    map_chr(
      1:3,
      ~ formatter(
        d_cls[[var]][d_cls$mt_class == .x]
      )
    )
  )
  
  set_names(
    values,
    class_columns
  )
}


##### EXTRACT AVAILABLE N BY CLASS #####

get_class_n <- function(var) {
  
  values <- c(
    sum(!is.na(d_cls[[var]])),
    
    map_int(
      1:3,
      ~ sum(
        !is.na(
          d_cls[[var]][d_cls$mt_class == .x]
        )
      )
    )
  )
  
  set_names(
    values,
    class_columns
  )
}


##### CONTINUOUS VARIABLE ROW #####

make_continuous_row <- function(
    var,
    label,
    section,
    test_classes = TRUE
) {
  
  values <- get_class_values(
    var,
    fmt_ms
  )
  
  available_n <- get_class_n(
    var
  )
  
  test_text <- ""
  p_value   <- NA_real_
  eta2      <- NA_real_
  
  test_dat <- tibble(
    outcome = d_cls[[var]],
    class = factor(d_cls$mt_class)
  ) %>%
    filter(
      !is.na(outcome),
      !is.na(class)
    )
  
  if (
    test_classes &&
    nrow(test_dat) > 0 &&
    n_distinct(test_dat$class) > 1 &&
    n_distinct(test_dat$outcome) > 1
  ) {
    
    fit <- aov(
      outcome ~ class,
      data = test_dat
    )
    
    aov_tab <- summary(fit)[[1]]
    
    f_value <- as.numeric(
      aov_tab[1, "F value"]
    )
    
    p_value <- as.numeric(
      aov_tab[1, "Pr(>F)"]
    )
    
    df1 <- as.numeric(
      aov_tab[1, "Df"]
    )
    
    df2 <- as.numeric(
      aov_tab[2, "Df"]
    )
    
    ss_total <- sum(
      aov_tab[, "Sum Sq"],
      na.rm = TRUE
    )
    
    if (ss_total > 0) {
      eta2 <- as.numeric(
        aov_tab[1, "Sum Sq"]
      ) / ss_total
    }
    
    if (is.finite(f_value)) {
      test_text <- sprintf(
        "F(%d, %d) = %.2f",
        df1,
        df2,
        f_value
      )
    } else {
      p_value <- NA_real_
      eta2 <- NA_real_
    }
  }
  
  list(
    apa = bind_cols(
      tibble(
        section = section,
        characteristic = label
      ),
      
      as_tibble_row(
        values
      ),
      
      tibble(
        test = test_text,
        p = fmt_p(p_value),
        effect_size = fmt_effect(
          "η²",
          eta2
        )
      )
    ),
    
    n = bind_cols(
      tibble(
        section = section,
        characteristic = label
      ),
      
      as_tibble_row(
        available_n
      )
    )
  )
}


##### BINARY VARIABLE ROW #####

make_binary_row <- function(
    var,
    label,
    section,
    test_classes = TRUE
) {
  
  values <- get_class_values(
    var,
    fmt_np
  )
  
  available_n <- get_class_n(
    var
  )
  
  test_text <- ""
  p_value   <- NA_real_
  cramers_v <- NA_real_
  
  test_dat <- tibble(
    class = d_cls$mt_class,
    value = d_cls[[var]]
  ) %>%
    filter(
      !is.na(class),
      !is.na(value)
    )
  
  if (
    test_classes &&
    nrow(test_dat) > 0 &&
    n_distinct(test_dat$class) > 1 &&
    n_distinct(test_dat$value) > 1
  ) {
    
    tab <- table(
      test_dat$class,
      test_dat$value
    )
    
    test <- suppressWarnings(
      chisq.test(
        tab,
        correct = FALSE
      )
    )
    
    chi_square <- as.numeric(
      test$statistic
    )
    
    test_df <- as.numeric(
      test$parameter
    )
    
    p_value <- test$p.value
    
    denominator <- sum(tab) *
      min(
        nrow(tab) - 1,
        ncol(tab) - 1
      )
    
    if (denominator > 0) {
      cramers_v <- sqrt(
        chi_square / denominator
      )
    }
    
    if (is.finite(chi_square)) {
      test_text <- sprintf(
        "χ²(%d) = %.2f",
        test_df,
        chi_square
      )
    } else {
      p_value <- NA_real_
      cramers_v <- NA_real_
    }
  }
  
  list(
    apa = bind_cols(
      tibble(
        section = section,
        characteristic = label
      ),
      
      as_tibble_row(
        values
      ),
      
      tibble(
        test = test_text,
        p = fmt_p(p_value),
        effect_size = if (test_classes) {
          fmt_effect(
            "V",
            cramers_v
          )
        } else {
          ""
        }
      )
    ),
    
    n = bind_cols(
      tibble(
        section = section,
        characteristic = label
      ),
      
      as_tibble_row(
        available_n
      )
    )
  )
}


#-----------------------------------------------------------------------
##### DEMOGRAPHIC CHARACTERISTICS #####
#-----------------------------------------------------------------------

age_t2_res <- make_continuous_row(
  var = "mt_age_t2_years",
  label = "Age at T2, years, M (SD)",
  section = "Demographic characteristics"
)

age_t5_res <- make_continuous_row(
  var = "mt_age_t5_years",
  label = "Age at T5, years, M (SD)",
  section = "Demographic characteristics"
)

sex_res <- make_binary_row(
  var = "female",
  label = "Female, n (%)",
  section = "Demographic characteristics"
)

tab_demo_apa <- bind_rows(
  age_t2_res$apa,
  age_t5_res$apa,
  sex_res$apa
)

tab_demo_n <- bind_rows(
  age_t2_res$n,
  age_t5_res$n,
  sex_res$n
)


#-----------------------------------------------------------------------
##### PSYCHOPATHOLOGY #####
#-----------------------------------------------------------------------

# Replace these placeholders with the full informant names later.
informant_labels <- c(
  b = "First Caregiver",
  k = "Participant (Child/Adolescent)",
  p = "Second Caregiver"
)


##### DEFINE PSYCHOPATHOLOGY VARIABLES AND LABELS #####

psy_meta <- bind_rows(
  
  sdq_grid %>%
    transmute(
      domain = "Emotional problems",
      informant,
      timepoint,
      variable = paste0(
        "sdq_emotion_",
        informant,
        "_",
        timepoint
      )
    ),
  
  sdq_grid %>%
    transmute(
      domain = "Externalizing problems",
      informant,
      timepoint,
      variable = paste0(
        "sdq_ext_",
        informant,
        "_",
        timepoint
      )
    )
) %>%
  mutate(
    domain_order = factor(
      domain,
      levels = c(
        "Emotional problems",
        "Externalizing problems"
      )
    ),
    
    timepoint_order = factor(
      timepoint,
      levels = c(
        "t2",
        "t5"
      )
    ),
    
    informant_order = factor(
      informant,
      levels = informants
    ),
    
    informant_label = unname(
      informant_labels[informant]
    ),
    
    label = paste0(
      domain,
      ", ",
      informant_label,
      ", ",
      str_to_upper(timepoint),
      ", M (SD)"
    )
  ) %>%
  arrange(
    domain_order,
    timepoint_order,
    informant_order
  )


##### CALCULATE PSYCHOPATHOLOGY TABLE #####

psy_res <- map2(
  psy_meta$variable,
  psy_meta$label,
  ~ make_continuous_row(
    var = .x,
    label = .y,
    section = "Psychopathology"
  )
)

tab_psy_apa <- bind_rows(
  map(
    psy_res,
    "apa"
  )
)

tab_psy_n <- bind_rows(
  map(
    psy_res,
    "n"
  )
)


#-----------------------------------------------------------------------
##### MALTREATMENT CHARACTERISTICS #####
#-----------------------------------------------------------------------

##### ASSESSMENT COVERAGE #####

coverage_res <- make_binary_row(
  var = "mt_info_t2all",
  label = "T1 and T2 maltreatment information available, n (%)",
  section = "Maltreatment assessment coverage",
  test_classes = TRUE
)


##### HARMONIZED MALTREATMENT CHARACTERISTICS #####

mal_res <- map2(
  names(mal_labels),
  unname(mal_labels),
  ~ make_binary_row(
    var = .x,
    label = .y,
    section = "Maltreatment characteristics",
    test_classes = FALSE
  )
)

tab_mal_apa <- bind_rows(
  coverage_res$apa,
  bind_rows(
    map(
      mal_res,
      "apa"
    )
  )
)

tab_mal_n <- bind_rows(
  coverage_res$n,
  bind_rows(
    map(
      mal_res,
      "n"
    )
  )
)


#-----------------------------------------------------------------------
##### COMBINE AVAILABLE SAMPLE SIZES #####
#-----------------------------------------------------------------------

tab_available_n <- bind_rows(
  tab_demo_n,
  tab_psy_n,
  tab_mal_n
)


#-----------------------------------------------------------------------
##### SAVE PREPARED DATA #####
#-----------------------------------------------------------------------

saveRDS(
  d,
  m18_prepared_rds_file
)


#-----------------------------------------------------------------------
##### EXPORT RESULTS #####
#-----------------------------------------------------------------------

write_xlsx(
  list(
    sample_sizes = n_samp,
    class_key = class_key,
    class_overview = sum_cls,
    retention_counts = ret_counts,
    retention_test = sum_ret,
    demographics_APA = tab_demo_apa,
    psychopathology_APA = tab_psy_apa,
    maltreatment_APA = tab_mal_apa,
    available_N = tab_available_n
  ),
  path = m18_class_checks_file
)


#-----------------------------------------------------------------------
##### DISPLAY MAIN RESULTS #####
#-----------------------------------------------------------------------

n_samp
class_key
sum_cls
sum_ret

tab_demo_apa
tab_psy_apa
tab_mal_apa


cat(
  "\nPrepared dataset saved to:\n",
  m18_prepared_rds_file,
  "\n\nResults saved to:\n",
  m18_class_checks_file,
  "\n"
)

#-----------------------------------------------------------------------
##### CREATE APA-STYLE WORD TABLE #####
#-----------------------------------------------------------------------

##### COMBINE TABLE SECTIONS #####

tab_apa <- bind_rows(
  tab_demo_apa,
  tab_psy_apa,
  tab_mal_apa
) %>%
  select(
    section,
    characteristic,
    all_of(class_columns),
    test,
    p,
    effect_size
  )


##### USE SHORT INTERNAL COLUMN NAMES #####

names(tab_apa)[
  match(
    class_columns,
    names(tab_apa)
  )
] <- c(
  "overall",
  "class1",
  "class2",
  "class3"
)


##### INSERT SECTION HEADINGS AS SEPARATE ROWS #####

section_order <- unique(
  tab_apa$section
)

tab_word <- map_dfr(
  section_order,
  function(current_section) {
    
    section_row <- tibble(
      section = current_section,
      characteristic = current_section,
      overall = "",
      class1 = "",
      class2 = "",
      class3 = "",
      test = "",
      p = "",
      effect_size = ""
    )
    
    section_data <- tab_apa %>%
      filter(
        section == current_section
      )
    
    bind_rows(
      section_row,
      section_data
    )
  }
) %>%
  select(
    -section
  )


##### IDENTIFY SECTION ROWS #####

section_rows <- which(
  tab_word$characteristic %in%
    section_order
)


##### CLASS SIZES FOR COLUMN HEADERS #####

n_class1 <- class_key %>%
  filter(mt_class == 1) %>%
  pull(n)

n_class2 <- class_key %>%
  filter(mt_class == 2) %>%
  pull(n)

n_class3 <- class_key %>%
  filter(mt_class == 3) %>%
  pull(n)

n_overall <- nrow(
  d_cls
)


##### BUILD FLEXTABLE #####

ft_apa <- flextable(
  tab_word
) %>%
  
  set_header_labels(
    characteristic = "Characteristic",
    
    overall = paste0(
      "Overall\n(N = ",
      n_overall,
      ")"
    ),
    
    class1 = paste0(
      "Class 1\nLow and stable\n(n = ",
      n_class1,
      ")"
    ),
    
    class2 = paste0(
      "Class 2\nElevated and declining\n(n = ",
      n_class2,
      ")"
    ),
    
    class3 = paste0(
      "Class 3\nHigh early burden with later rebound\n(n = ",
      n_class3,
      ")"
    ),
    
    test = "Test",
    p = "p",
    effect_size = "Effect size"
  ) %>%
  
  theme_booktabs() %>%
  
  font(
    fontname = "Times New Roman",
    part = "all"
  ) %>%
  
  fontsize(
    size = 9,
    part = "all"
  ) %>%
  
  bold(
    part = "header"
  ) %>%
  
  align(
    j = "characteristic",
    align = "left",
    part = "all"
  ) %>%
  
  align(
    j = c(
      "overall",
      "class1",
      "class2",
      "class3",
      "test",
      "p",
      "effect_size"
    ),
    align = "center",
    part = "all"
  ) %>%
  
  valign(
    valign = "center",
    part = "all"
  ) %>%
  
  padding(
    padding.top = 2,
    padding.bottom = 2,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  ) %>%
  
  set_table_properties(
    layout = "fixed",
    width = 1
  )


##### FORMAT SECTION HEADINGS #####

for (row in section_rows) {
  
  ft_apa <- ft_apa %>%
    merge_at(
      i = row,
      j = 1:8,
      part = "body"
    ) %>%
    
    bold(
      i = row,
      bold = TRUE,
      part = "body"
    ) %>%
    
    align(
      i = row,
      align = "left",
      part = "body"
    ) %>%
    
    padding(
      i = row,
      padding.top = 7,
      padding.bottom = 3,
      part = "body"
    )
}


##### SET COLUMN WIDTHS #####

ft_apa <- ft_apa %>%
  width(
    j = "characteristic",
    width = 2.70
  ) %>%
  width(
    j = "overall",
    width = 1.00
  ) %>%
  width(
    j = "class1",
    width = 1.25
  ) %>%
  width(
    j = "class2",
    width = 1.35
  ) %>%
  width(
    j = "class3",
    width = 1.65
  ) %>%
  width(
    j = "test",
    width = 1.15
  ) %>%
  width(
    j = "p",
    width = 0.55
  ) %>%
  width(
    j = "effect_size",
    width = 0.85
  ) %>%
  set_table_properties(
    layout = "fixed",
    width = 1,
    opts_word = list(
      split = FALSE,
      keep_with_next = FALSE
    )
  )

#-----------------------------------------------------------------------
##### CREATE WORD DOCUMENT #####
#-----------------------------------------------------------------------

doc <- read_docx()


##### LANDSCAPE PAGE FORMAT #####

landscape_section <- prop_section(
  page_size = page_size(
    orient = "landscape"
  ),
  
  page_margins = page_mar(
    top = 0.55,
    bottom = 0.55,
    left = 0.55,
    right = 0.55
  )
)

doc <- body_set_default_section(
  doc,
  landscape_section
)


##### TABLE NUMBER #####

doc <- body_add_fpar(
  doc,
  fpar(
    ftext(
      "Table 1",
      fp_text(
        font.family = "Times New Roman",
        font.size = 11,
        bold = TRUE
      )
    )
  )
)


##### TABLE TITLE #####

doc <- body_add_fpar(
  doc,
  fpar(
    ftext(
      "Characteristics of the Maltreatment Trajectory Classes",
      fp_text(
        font.family = "Times New Roman",
        font.size = 11,
        italic = TRUE
      )
    )
  )
)


##### ADD TABLE #####

doc <- body_add_flextable(
  doc,
  ft_apa
)


##### APA TABLE NOTE #####

doc <- body_add_fpar(
  doc,
  fpar(
    ftext(
      "Note. ",
      fp_text(
        font.family = "Times New Roman",
        font.size = 9,
        italic = TRUE
      )
    ),
    
    ftext(
      paste0(
        "Values are M (SD) for continuous variables and n (%) for ",
        "categorical variables. Percentages are based on the available ",
        "data for each variable. Externalizing problems were calculated ",
        "as the sum of the SDQ conduct problems and hyperactivity/inattention ",
        "subscales. Harmonized maltreatment indicators were based on the ",
        "cumulative T2-all variable when available and otherwise on the ",
        "corresponding T1 variable, thereby reflecting the maximum available ",
        "maltreatment history for each participant. Values were coded as missing ",
        "when both variables were missing. Inferential tests are reported for ",
        "demographic characteristics, psychopathology, and assessment coverage, ",
        "but not for maltreatment characteristics used to define the trajectory ",
        "classes. η² = eta squared; V = Cramér's V; — = not estimated."
      ),
      
      fp_text(
        font.family = "Times New Roman",
        font.size = 9
      )
    )
  )
)


##### SAVE WORD DOCUMENT #####

print(
  doc,
  target = m18_word_file
)

cat(
  "\nAPA-style Word table saved to:\n",
  m18_word_file,
  "\n"
)

