#-------------------------------------------------------------------------
##### SETUP #####
#-------------------------------------------------------------------------

source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R")

check_packages(
  c(
    "dplyr",
    "tidyr",
    "purrr",
    "stringr",
    "readxl",
    "writexl",
    "tibble"
  ),
  required = TRUE
)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readxl)
  library(writexl)
  library(tibble)
})

#-----------------------------------------------------------------------
##### DEFINE FINAL OPTION-B CLASSIFICATION #####
#-----------------------------------------------------------------------

class_var <- "mt_class_mo"
class_prob_var <- "mt_prob_max_mo"

class_prob_vars <- c(
  "mt_prob_class1_mo",
  "mt_prob_class2_mo",
  "mt_prob_class3_mo",
  "mt_prob_class4_mo"
)

# Combined four-group coding:
# 1 = externally defined non-maltreated reference group
# 2 = final M18 c#1
# 3 = final M18 c#2
# 4 = final M18 c#3
class_levels <- 1:4

class_labels <- c(
  "Non-maltreated",
  "Moderate/early-increasing burden",
  "Elevated/declining burden",
  "High/rebound burden"
)

expected_class_sizes <- c(
  `1` = 281L,
  `2` = 223L,
  `3` = 42L,
  `4` = 38L
)

expected_m18_avepp <- c(
  `2` = 0.986,
  `3` = 0.991,
  `4` = 0.941
)

table_number <- "SX"

class_input_file <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes_SES_mo.xlsx"
)

prepared_rds_file <- file.path(
  data_prep_dir,
  "AMIS_class_description_prepared_final_mo.rds"
)

class_results_file <- file.path(
  supplement_dir,
  paste0(
    "Table_",
    table_number,
    "_class_characteristics_results.xlsx"
  )
)

dir.create(
  supplement_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

class_label_lookup <- tibble(
  class_value = class_levels,
  class_label = class_labels
)


#-------------------------------------------------------------------------
##### CHECK INPUT FILE #####
#-------------------------------------------------------------------------

if (!file.exists(class_input_file)) {
  stop(
    "Input file not found: ",
    class_input_file
  )
}

#-----------------------------------------------------------------------
##### DEFINE VARIABLES #####
#-----------------------------------------------------------------------

##### SDQ VARIABLES #####

# First caregiver, participant, and second caregiver only.
# Teacher reports are deliberately excluded because they are not included
# in the final latent change models.
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


##### CENTRAL MALTREATMENT GROUPING VARIABLE #####

mal_group_var <- "mal_all"


##### MATERNAL EDUCATIONAL ATTAINMENT #####

ses_var <- "SES_AUSB1_M_COMBINED"

ses_levels <- 0:4

ses_labels <- c(
  "No school-leaving qualification",
  "Special school-leaving certificate",
  "Lower secondary school-leaving certificate (Hauptschulabschluss)",
  "Intermediate secondary school-leaving certificate (Realschulabschluss)",
  "University entrance qualification (Abitur/Fachhochschulreife)"
)


##### MALTREATMENT STATUS VARIABLES #####

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


##### VARIABLE LABELS #####

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


##### CLASS COLUMN LABELS #####

class_columns <- c(
  "Overall",
  paste0(
    "Class ",
    class_levels,
    ": ",
    class_labels
  )
)


#-----------------------------------------------------------------------
##### READ AND CHECK DATA #####
#-----------------------------------------------------------------------

d <- read_excel(
  class_input_file,
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
    ses_var,
    class_var,
    class_prob_vars,
    class_prob_var,
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

if (
  length(
    miss
  ) > 0
) {
  stop(
    paste0(
      "Missing variables:\n",
      paste0(
        "- ",
        miss,
        collapse = "\n"
      )
    )
  )
}


##### CHECK ID VALUES #####

if (
  anyNA(
    d$sic
  )
) {
  stop(
    "Missing values found in sic."
  )
}

if (
  anyDuplicated(
    d$sic
  ) > 0
) {
  stop(
    "Duplicated values found in sic."
  )
}


##### CHECK SES VALUES #####

observed_ses_values <- sort(
  unique(
    d[[ses_var]][
      !is.na(
        d[[ses_var]]
      )
    ]
  )
)

unexpected_ses_values <- setdiff(
  observed_ses_values,
  ses_levels
)

if (
  length(
    unexpected_ses_values
  ) > 0L
) {
  stop(
    paste0(
      ses_var,
      " contains unexpected values: ",
      paste(
        unexpected_ses_values,
        collapse = ", "
      )
    )
  )
}


##### CHECK CLASS VALUES #####

observed_class_values <- sort(
  unique(
    d[[class_var]][
      !is.na(
        d[[class_var]]
      )
    ]
  )
)

unexpected_class_values <- setdiff(
  observed_class_values,
  class_levels
)

if (
  length(
    unexpected_class_values
  ) > 0
) {
  stop(
    paste0(
      class_var,
      " contains unexpected values: ",
      paste(
        unexpected_class_values,
        collapse = ", "
      )
    )
  )
}



##### CHECK FINAL OPTION-B GROUP ASSIGNMENT #####

mo_class_counts <- d |>
  filter(
    !is.na(
      .data[[class_var]]
    )
  ) |>
  count(
    class = .data[[class_var]],
    name = "n"
  ) |>
  arrange(
    class
  )

print(
  mo_class_counts,
  n = Inf
)

observed_class_sizes <- stats::setNames(
  mo_class_counts$n,
  as.character(
    mo_class_counts$class
  )
)

if (
  !identical(
    names(observed_class_sizes),
    names(expected_class_sizes)
  ) ||
  !identical(
    as.integer(observed_class_sizes),
    as.integer(expected_class_sizes)
  )
) {
  stop(
    paste0(
      "The final Option-B group sizes do not match the expected ",
      "281/223/42/38 distribution. Observed sizes: ",
      paste(
        paste0(
          names(observed_class_sizes),
          " = ",
          observed_class_sizes
        ),
        collapse = ", "
      ),
      ". Check whether the current final M18 CPROBABILITIES export ",
      "and class-label mapping were used."
    )
  )
}


#-----------------------------------------------------------------------
##### CREATE ANALYSIS VARIABLES #####
#-----------------------------------------------------------------------

d <- d |>
  mutate(
    analysis_class_number = as.integer(
      .data[[class_var]]
    ),
    
    analysis_class = factor(
      analysis_class_number,
      levels = class_levels,
      labels = class_labels
    ),
    
    analysis_class_label = as.character(
      analysis_class
    ),
    
    analysis_class_probability = as.numeric(
      .data[[class_prob_var]]
    ),
    
    # Dummy variables for class comparisons;
    # class 1 is always the reference group.
    analysis_class2 = as.integer(
      analysis_class_number == 2
    ),
    
    analysis_class3 = as.integer(
      analysis_class_number == 3
    ),
    
    analysis_class4 = if (
      length(
        class_levels
      ) == 4
    ) {
      as.integer(
        analysis_class_number == 4
      )
    } else {
      NA_integer_
    },
    
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
      !is.na(
        stat_t5
      ) &
        stat_t5 != 0
    ),
    
    # Indicator of the available maltreatment assessment window
    mt_info_t2all = as.integer(
      !is.na(
        mt_mal_status_t2all
      )
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
    mt_age_t2_months = as.numeric(
      mt_age_t2
    ),
    mt_age_t5_months = as.numeric(
      mt_age_t5
    ),
    mt_age_t2_years = mt_age_t2_months / 12,
    mt_age_t5_years = mt_age_t5_months / 12
  )


##### FULL CLASS SAMPLE #####

d_cls <- d %>%
  filter(
    !is.na(
      analysis_class_number
    )
  )


##### LCS SAMPLE WITH CLASS ASSIGNMENT #####

d_lcs <- d %>%
  filter(
    lcs_included == 1,
    !is.na(
      analysis_class_number
    )
  )


##### SES AVAILABILITY IN THE CLASS SAMPLE #####

ses_availability <- bind_rows(
  d_cls %>%
    summarise(
      analysis_class_number = NA_integer_,
      analysis_class_label = "Overall",
      n = n(),
      n_available = sum(
        !is.na(
          .data[[ses_var]]
        )
      ),
      n_missing = sum(
        is.na(
          .data[[ses_var]]
        )
      ),
      missing_pct = 100 *
        mean(
          is.na(
            .data[[ses_var]]
          )
        )
    ),
  d_cls %>%
    group_by(
      analysis_class_number,
      analysis_class_label
    ) %>%
    summarise(
      n = n(),
      n_available = sum(
        !is.na(
          .data[[ses_var]]
        )
      ),
      n_missing = sum(
        is.na(
          .data[[ses_var]]
        )
      ),
      missing_pct = 100 *
        mean(
          is.na(
            .data[[ses_var]]
          )
        ),
      .groups = "drop"
    )
)


#-----------------------------------------------------------------------
##### ESSENTIAL CLASSIFICATION CHECK #####
#-----------------------------------------------------------------------

if (
  nrow(
    d_cls
  ) == 0
) {
  stop(
    "No cases with a class assignment found."
  )
}


##### EXTRACT POSTERIOR PROBABILITIES #####

p <- d_cls %>%
  select(
    all_of(
      class_prob_vars
    )
  ) %>%
  mutate(
    across(
      everything(),
      as.numeric
    )
  ) %>%
  as.matrix()


##### CHECK POSTERIOR PROBABILITY VALUES #####

if (
  anyNA(
    p
  )
) {
  stop(
    "Missing posterior probabilities found in the final Option-B class sample."
  )
}

if (
  anyNA(
    d_cls$analysis_class_probability
  )
) {
  stop(
    paste0(
      "Missing maximum posterior probabilities found in the final ",
      "Option-B class sample."
    )
  )
}

if (
  !all(
    p >= 0 &
    p <= 1
  )
) {
  stop(
    "Posterior probabilities outside the range 0 to 1."
  )
}


##### CHECK THAT POSTERIOR PROBABILITIES SUM TO ONE #####

prob_sum_deviation <- max(
  abs(
    rowSums(
      p
    ) - 1
  )
)

if (
  prob_sum_deviation > .0011
) {
  stop(
    "Posterior probabilities deviate from 1. ",
    "Maximum deviation: ",
    round(
      prob_sum_deviation,
      4
    )
  )
}


##### CHECK CLASS ASSIGNMENT AGAINST LARGEST PROBABILITY #####

assigned_class_from_probabilities <- max.col(
  p,
  ties.method = "first"
)

if (
  !all(
    d_cls$analysis_class_number ==
    assigned_class_from_probabilities
  )
) {
  
  classification_mismatches <- d_cls %>%
    mutate(
      class_from_probability =
        assigned_class_from_probabilities
    ) %>%
    filter(
      analysis_class_number !=
        class_from_probability
    ) %>%
    select(
      sic,
      analysis_class_number,
      class_from_probability,
      all_of(
        class_prob_vars
      )
    )
  
  print(
    classification_mismatches,
    n = Inf
  )
  
  stop(
    "Class assignments do not match the largest posterior probability."
  )
}


##### CHECK MAXIMUM POSTERIOR PROBABILITY #####

prob_max_deviation <- max(
  abs(
    d_cls$analysis_class_probability -
      apply(
        p,
        1,
        max
      )
  )
)

if (
  prob_max_deviation > .0011
) {
  stop(
    "Maximum posterior probability does not match the largest ",
    "class-specific posterior probability. Maximum deviation: ",
    round(
      prob_max_deviation,
      4
    )
  )
}


##### PRINT CLASSIFICATION CHECK SUMMARY #####

classification_check_summary <- tibble(
  n_classified =
    nrow(
      d_cls
    ),
  
  number_of_groups =
    length(
      class_levels
    ),
  
  maximum_probability_sum_deviation =
    prob_sum_deviation,
  
  maximum_probability_max_deviation =
    prob_max_deviation
)

print(
  classification_check_summary,
  n = Inf
)


#-----------------------------------------------------------------------
##### SAMPLE AND CLASS OVERVIEW #####
#-----------------------------------------------------------------------

##### SAMPLE SIZES #####

n_samp <- tibble(
  sample = c(
    "Full Excel dataset",
    "Full Option-B group sample",
    "LCS sample: stat_t5 NE 0",
    "LCS sample with Option-B group assignment"
  ),
  
  n = c(
    nrow(d),
    nrow(d_cls),
    sum(
      d$lcs_included,
      na.rm = TRUE
    ),
    nrow(d_lcs)
  )
)


##### CLASS KEY #####

class_key <- d_cls %>%
  count(
    analysis_class_number,
    analysis_class_label,
    name = "n"
  ) %>%
  mutate(
    percent = 100 *
      n /
      sum(
        n
      )
  ) %>%
  arrange(
    analysis_class_number
  )


##### CLASSIFICATION AND RETENTION OVERVIEW #####

sum_cls <- d_cls %>%
  group_by(
    analysis_class_number,
    analysis_class_label
  ) %>%
  summarise(
    n = n(),
    
    n_lcs = sum(
      lcs_included,
      na.rm = TRUE
    ),
    
    retention_pct = 100 *
      mean(
        lcs_included,
        na.rm = TRUE
      ),
    
    prob_mean = mean(
      analysis_class_probability,
      na.rm = TRUE
    ),
    
    prob_sd = sd(
      analysis_class_probability,
      na.rm = TRUE
    ),
    
    prob_min = min(
      analysis_class_probability,
      na.rm = TRUE
    ),
    
    prob_ge70_pct = 100 *
      mean(
        analysis_class_probability >= .70,
        na.rm = TRUE
      ),
    
    across(
      all_of(
        class_prob_vars
      ),
      ~ mean(
        .x,
        na.rm = TRUE
      ),
      .names = "avepp_{.col}"
    ),
    
    .groups = "drop"
  ) %>%
  rename_with(
    .fn = ~ stringr::str_replace(
      .x,
      "^avepp_mt_prob_class([1-4])(_mo)?$",
      "avepp_class\\1"
    ),
    .cols = starts_with(
      "avepp_mt_prob_class"
    )
  ) %>%
  mutate(
    class_pct = 100 *
      n /
      sum(
        n
      ),
    
    lcs_pct = 100 *
      n_lcs /
      sum(
        n_lcs
      ),

    # The non-maltreated reference group was externally defined and has
    # no latent-class classification uncertainty. Its synthetic one-hot
    # probabilities are therefore not reported as classification quality.
    across(
      c(
        prob_mean,
        prob_sd,
        prob_min,
        prob_ge70_pct,
        starts_with(
          "avepp_class"
        )
      ),
      ~ if_else(
        analysis_class_number == 1L,
        NA_real_,
        as.numeric(.x)
      )
    )
  ) %>%
  relocate(
    class_pct,
    .after = n
  ) %>%
  relocate(
    lcs_pct,
    .after = n_lcs
  ) %>%
  arrange(
    analysis_class_number
  )


##### VERIFY FINAL M18 CLASS MAPPING WITH AVEPP #####

observed_m18_avepp <- vapply(
  2:4,
  function(class_number) {
    sum_cls[[
      paste0(
        "avepp_class",
        class_number
      )
    ]][
      sum_cls$analysis_class_number ==
        class_number
    ]
  },
  numeric(1)
)

names(observed_m18_avepp) <- as.character(
  2:4
)

if (
  any(
    abs(
      observed_m18_avepp -
        expected_m18_avepp
    ) > 0.005
  )
) {
  stop(
    paste0(
      "The class-specific average posterior probabilities do not match ",
      "the final M18 solution (.986/.991/.941). Observed values: ",
      paste(
        formatC(
          observed_m18_avepp,
          format = "f",
          digits = 3
        ),
        collapse = "/"
      ),
      ". Check the current M18 class mapping and probability columns."
    )
  )
}


##### INSPECT SAMPLE AND CLASS OVERVIEW #####

print(
  n_samp,
  n = Inf
)

print(
  class_key,
  n = Inf
)

print(
  sum_cls,
  n = Inf
)


#-----------------------------------------------------------------------
##### RETENTION OVERVIEW AND TEST #####
#-----------------------------------------------------------------------

ret_tab <- table(
  d_cls$analysis_class_number,
  d_cls$lcs_included,
  useNA = "no"
)


##### RETENTION COUNTS BY CLASS #####

ret_counts <- as.data.frame.matrix(
  ret_tab
) %>%
  as_tibble(
    rownames = "analysis_class_number"
  ) %>%
  mutate(
    analysis_class_number = as.integer(
      analysis_class_number
    )
  ) %>%
  left_join(
    class_label_lookup %>%
      rename(
        analysis_class_number = class_value,
        analysis_class_label = class_label
      ),
    by = "analysis_class_number"
  ) %>%
  relocate(
    analysis_class_label,
    .after = analysis_class_number
  ) %>%
  arrange(
    analysis_class_number
  )


##### RUN RETENTION TEST ONLY IF BOTH RETENTION CATEGORIES EXIST #####

if (
  nrow(ret_tab) >= 2 &&
  ncol(ret_tab) >= 2
) {
  
  ret_test <- chisq.test(
    ret_tab,
    correct = FALSE
  )
  
  ret_v <- sqrt(
    unname(
      ret_test$statistic
    ) /
      (
        sum(ret_tab) *
          min(
            nrow(ret_tab) - 1,
            ncol(ret_tab) - 1
          )
      )
  )
  
  sum_ret <- tibble(
    test_performed = TRUE,
    
    reason_not_performed = NA_character_,
    
    chi_square = unname(
      ret_test$statistic
    ),
    
    df = unname(
      ret_test$parameter
    ),
    
    p = ret_test$p.value,
    
    cramers_v = ret_v,
    
    minimum_expected_count = min(
      ret_test$expected
    ),
    
    cells_expected_below_5 = sum(
      ret_test$expected < 5
    )
  )
  
} else {
  
  sum_ret <- tibble(
    test_performed = FALSE,
    
    reason_not_performed = paste0(
      "Retention did not vary in the classified sample; ",
      "all classified participants had the same lcs_included status."
    ),
    
    chi_square = NA_real_,
    df = NA_real_,
    p = NA_real_,
    cramers_v = NA_real_,
    minimum_expected_count = NA_real_,
    cells_expected_below_5 = NA_integer_
  )
  
  message(
    "Retention test not performed: lcs_included does not vary within d_cls."
  )
}


##### INSPECT RETENTION RESULTS #####

print(
  ret_counts,
  n = Inf
)

print(
  sum_ret,
  n = Inf
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

get_class_values <- function(
    var,
    formatter
) {
  
  values <- c(
    formatter(
      d_cls[[var]]
    ),
    
    map_chr(
      class_levels,
      ~ formatter(
        d_cls[[var]][
          d_cls$analysis_class_number == .x
        ]
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
    sum(
      !is.na(
        d_cls[[var]]
      )
    ),
    
    map_int(
      class_levels,
      ~ sum(
        !is.na(
          d_cls[[var]][
            d_cls$analysis_class_number == .x
          ]
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
    class = factor(
      d_cls$analysis_class_number,
      levels = class_levels,
      labels = class_labels
    )
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
    
    aov_tab <- summary(
      fit
    )[[1]]
    
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
        p = fmt_p(
          p_value
        ),
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
    class = factor(
      d_cls$analysis_class_number,
      levels = class_levels,
      labels = class_labels
    ),
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
    
    denominator <- sum(
      tab
    ) *
      min(
        nrow(tab) - 1,
        ncol(tab) - 1
      )
    
    if (denominator > 0) {
      
      cramers_v <- sqrt(
        chi_square /
          denominator
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
        p = fmt_p(
          p_value
        ),
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


##### MULTICATEGORY VARIABLE ROWS #####

make_categorical_rows <- function(
    var,
    levels,
    labels,
    heading,
    section,
    test_classes = TRUE,
    simulation_seed = 20260813L,
    simulation_replicates = 100000L
) {
  stopifnot(
    length(
      levels
    ) == length(
      labels
    )
  )

  test_text <- ""
  p_value <- NA_real_
  cramers_v <- NA_real_

  test_dat <- tibble(
    class = factor(
      d_cls$analysis_class_number,
      levels = class_levels,
      labels = class_labels
    ),
    value = factor(
      d_cls[[var]],
      levels = levels,
      labels = labels
    )
  ) %>%
    filter(
      !is.na(class),
      !is.na(value)
    ) %>%
    mutate(
      class = droplevels(
        class
      ),
      value = droplevels(
        value
      )
    )

  if (
    test_classes &&
    nrow(
      test_dat
    ) > 0L &&
    n_distinct(
      test_dat$class
    ) > 1L &&
    n_distinct(
      test_dat$value
    ) > 1L
  ) {
    tab <- table(
      test_dat$class,
      test_dat$value
    )

    pearson_test <- suppressWarnings(
      chisq.test(
        tab,
        correct = FALSE
      )
    )

    chi_square <- as.numeric(
      pearson_test$statistic
    )

    test_df <- as.numeric(
      pearson_test$parameter
    )

    if (
      any(
        pearson_test$expected < 5
      )
    ) {
      set.seed(
        simulation_seed
      )

      simulated_test <- suppressWarnings(
        chisq.test(
          tab,
          simulate.p.value = TRUE,
          B = simulation_replicates
        )
      )

      p_value <- simulated_test$p.value

      test_text <- sprintf(
        "χ²(%d) = %.2f [Monte Carlo]",
        test_df,
        chi_square
      )

    } else {
      p_value <- pearson_test$p.value

      test_text <- sprintf(
        "χ²(%d) = %.2f",
        test_df,
        chi_square
      )
    }

    denominator <- sum(
      tab
    ) *
      min(
        nrow(tab) - 1,
        ncol(tab) - 1
      )

    if (
      denominator > 0
    ) {
      cramers_v <- sqrt(
        chi_square /
          denominator
      )
    }
  }

  heading_values <- set_names(
    rep(
      "",
      length(
        class_columns
      )
    ),
    class_columns
  )

  heading_apa <- bind_cols(
    tibble(
      section = section,
      characteristic = heading
    ),
    as_tibble_row(
      heading_values
    ),
    tibble(
      test = test_text,
      p = fmt_p(
        p_value
      ),
      effect_size = fmt_effect(
        "V",
        cramers_v
      )
    )
  )

  category_apa <- map2_dfr(
    levels,
    labels,
    function(category_value, category_label) {
      values <- get_class_values(
        var,
        function(x) {
          fmt_np(
            x,
            positive = category_value
          )
        }
      )

      bind_cols(
        tibble(
          section = section,
          characteristic = paste0(
            "  ",
            category_label
          )
        ),
        as_tibble_row(
          values
        ),
        tibble(
          test = "",
          p = "",
          effect_size = ""
        )
      )
    }
  )

  available_n <- get_class_n(
    var
  )

  list(
    apa = bind_rows(
      heading_apa,
      category_apa
    ),
    n = bind_cols(
      tibble(
        section = section,
        characteristic = heading
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

ses_res <- make_categorical_rows(
  var = ses_var,
  levels = ses_levels,
  labels = ses_labels,
  heading = "Maternal educational attainment, n (%)",
  section = "Demographic characteristics"
)

tab_demo_apa <- bind_rows(
  age_t2_res$apa,
  age_t5_res$apa,
  sex_res$apa,
  ses_res$apa
)

tab_demo_n <- bind_rows(
  age_t2_res$n,
  age_t5_res$n,
  sex_res$n,
  ses_res$n
)


#-----------------------------------------------------------------------
##### PSYCHOPATHOLOGY #####
#-----------------------------------------------------------------------

informant_labels <- c(
  b = "First caregiver",
  k = "Participant",
  p = "Second caregiver"
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
      str_to_upper(
        timepoint
      ),
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
    section = "Psychopathology",
    test_classes = TRUE
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

##### HARMONISED MALTREATMENT CHARACTERISTICS #####

mal_res <- map2(
  names(
    mal_labels
  ),
  unname(
    mal_labels
  ),
  ~ make_binary_row(
    var = .x,
    label = .y,
    section = "Maltreatment characteristics",
    test_classes = FALSE
  )
)


##### COMBINE MALTREATMENT RESULTS #####

tab_mal_apa <- bind_rows(
  map(
    mal_res,
    "apa"
  )
)

tab_mal_n <- bind_rows(
  map(
    mal_res,
    "n"
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
  prepared_rds_file
)


#-----------------------------------------------------------------------
##### EXPORT RESULTS #####
#-----------------------------------------------------------------------

write_xlsx(
  list(
    sample_sizes = n_samp,
    class_key = class_key,
    classification_check = classification_check_summary,
    class_overview = sum_cls,
    SES_availability = ses_availability,
    retention_counts = ret_counts,
    retention_test = sum_ret,
    demographics_APA = tab_demo_apa,
    psychopathology_APA = tab_psy_apa,
    maltreatment_APA = tab_mal_apa,
    available_N = tab_available_n
  ),
  path = class_results_file
)


#-----------------------------------------------------------------------
##### DISPLAY MAIN RESULTS #####
#-----------------------------------------------------------------------

print(
  n_samp,
  n = Inf
)

print(
  class_key,
  n = Inf
)

print(
  sum_cls,
  n = Inf
)

print(
  ses_availability,
  n = Inf
)

print(
  tab_demo_apa,
  n = Inf
)

print(
  tab_psy_apa,
  n = Inf
)

print(
  tab_mal_apa,
  n = Inf
)


cat(
  "\nClassification strategy:\n",
  "Final Option B",
  "\n\nPrepared dataset saved to:\n",
  prepared_rds_file,
  "\n\nResults saved to:\n",
  class_results_file,
  "\n",
  sep = ""
)

