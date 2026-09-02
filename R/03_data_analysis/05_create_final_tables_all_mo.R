#-----------------------------------------------------------------------
##### CREATE FINAL TABLES: MAIN OUTCOME PAPER #####
#-----------------------------------------------------------------------

source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R")
source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_helpers_final_tables.R")
source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/04_class_description_analysis_final_mo.R")

required_packages <- c(
  "MplusAutomation", "dplyr", "tidyr", "tibble",
  "purrr", "officer", "flextable"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop("Install first: ", paste(missing_packages, collapse = ", "))
}

library(flextable)
library(officer)

#-------------------------------------------------------------------------
##### TABLE SX: FIT OF SDQ MEASUREMENT AND LATENT CHANGE SCORE MODELS #####
#-------------------------------------------------------------------------

table_number <- "SX"

table_title <- paste(
  "Fit Statistics for the SDQ Measurement, Measurement Invariance,",
  "and Latent Change Score Models"
)


##### DEFINE MODELS #####

model_catalog <- tibble::tribble(
  ~model_id, ~model_order, ~model_family,
  ~output_stem, ~model_step, ~model_specification,
  ~comparison_model, ~retained_model,
  
  "M1", 1L,
  "Measurement model",
  "01_sdq_configural",
  "Configural model",
  paste(
    "Same factor structure across T2 and T5;",
    "loadings and indicator intercepts freely estimated"
  ),
  NA_character_,
  FALSE,
  
  "M2", 2L,
  "Measurement model",
  "02_sdq_longitudinal_resid",
  "Longitudinal residuals",
  paste(
    "Residual covariances between corresponding indicators",
    "added across T2 and T5"
  ),
  "M1",
  FALSE,
  
  "M3", 3L,
  "Measurement invariance",
  "03_sdq_metric_invariance",
  "Metric invariance",
  paste(
    "Factor loadings constrained to equality",
    "across T2 and T5"
  ),
  "M2",
  FALSE,
  
  "M4", 4L,
  "Measurement model",
  "04_sdq_within_informant_crossscale",
  "Within-informant residuals",
  paste(
    "Within-informant cross-scale residual covariances",
    "added and constrained across time"
  ),
  "M3",
  FALSE,
  
  "M5", 5L,
  "Measurement model",
  "05_sdq_between_informant_residuals",
  "Final metric model",
  paste(
    "Selected between-informant residual covariances",
    "added and constrained across time"
  ),
  "M4",
  FALSE,
  
  "M6", 6L,
  "Measurement invariance",
  "06_sdq_full_scalar_invariance",
  "Full scalar invariance",
  paste(
    "All indicator intercepts constrained to equality;",
    "T2 latent means fixed to zero and",
    "T5 latent means freely estimated"
  ),
  "M5",
  FALSE,
  
  "M7", 7L,
  "Measurement invariance",
  "07_sdq_partial_scalar_invariance",
  "Final partial scalar model",
  paste(
    "Child-report emotional-problems indicator intercept",
    "freely estimated across time;",
    "all remaining indicator intercepts constrained"
  ),
  "M5",
  TRUE,
  
  "M8", 8L,
  "Latent change score model",
  "08_sdq_classical_lcs",
  "Unconditional LCS model",
  paste(
    "Bivariate latent change score model for",
    "externalizing and emotional problems based on M7"
  ),
  NA_character_,
  FALSE,
  
  "M20", 20L,
  "Conditional LCS model",
  "20_sdq_lcs_class_effects_mo",
  "Unadjusted class-effects model",
  paste(
    "Four-group maltreatment classification predicting",
    "T2 latent levels and latent changes"
  ),
  NA_character_,
  FALSE,
  
  "M21", 21L,
  "Conditional LCS model",
  "21_sdq_lcs_class_effects_age_sex_mo",
  "Age- and sex-adjusted model",
  paste(
    "Class effects adjusted for baseline age",
    "and sex"
  ),
  NA_character_,
  FALSE,
  
  "M22", 22L,
  "Conditional LCS model",
  "22_sdq_lcs_class_effects_sociodemographic_mo",
  "Sociodemographically adjusted model",
  paste(
    "Class effects adjusted for baseline age, sex,",
    "and maternal educational attainment"
  ),
  NA_character_,
  TRUE,
  
  "M23a", 23L,
  "Biomarker LCS model",
  "23a_sdq_lcs_class_effects_prs_eau_mo",
  "European-ancestry PRS model",
  paste(
    "M22 extended by European-ancestry MDD polygenic risk",
    "and four genetic principal components"
  ),
  NA_character_,
  FALSE,
  
  "M23b", 24L,
  "Biomarker LCS model",
  "23b_sdq_lcs_class_effects_prs_mau_mo",
  "Multi-ancestry PRS model",
  paste(
    "M22 extended by multi-ancestry MDD polygenic risk",
    "and four genetic principal components"
  ),
  NA_character_,
  FALSE,
  
  "M24", 25L,
  "Biomarker LCS model",
  "24_sdq_lcs_class_effects_hair_cortisol_mo",
  "Hair-cortisol model",
  paste(
    "M22 extended by standardized proximal-segment",
    "hair cortisol concentration"
  ),
  NA_character_,
  FALSE,
  
  "M25a", 26L,
  "Combined-biomarker LCS model",
  "25a_sdq_lcs_class_effects_hcc_prs_eau_mo",
  "Cortisol and European-ancestry PRS",
  paste(
    "M22 extended simultaneously by hair cortisol,",
    "European-ancestry MDD polygenic risk,",
    "and four genetic principal components"
  ),
  NA_character_,
  FALSE,
  
  "M25b", 27L,
  "Combined-biomarker LCS model",
  "25b_sdq_lcs_class_effects_hcc_prs_mau_mo",
  "Cortisol and multi-ancestry PRS",
  paste(
    "M22 extended simultaneously by hair cortisol,",
    "multi-ancestry MDD polygenic risk,",
    "and four genetic principal components"
  ),
  NA_character_,
  FALSE
)


##### LOCATE OUTPUT FILES #####

model_catalog <- model_catalog |>
  dplyr::mutate(
    output_file = file.path(
      mplus_input_dir,
      paste0(
        .data$output_stem,
        ".out"
      )
    )
  )

missing_output_files <- model_catalog |>
  dplyr::filter(
    !file.exists(
      .data$output_file
    )
  )

if (nrow(missing_output_files) > 0L) {
  stop(
    "The following Mplus output files are missing:\n",
    paste(
      paste0(
        missing_output_files$model_id,
        ": ",
        missing_output_files$output_file
      ),
      collapse = "\n"
    )
  )
}


##### EXTRACT MODEL FIT #####

fit_values <- purrr::map2_dfr(
  model_catalog$output_file,
  model_catalog$model_id,
  
  function(
    output_file,
    model_id
  ) {
    
    read_mplus_fit(
      output_file
    ) |>
      dplyr::mutate(
        model_id = model_id
      )
  }
)

fit_table <- model_catalog |>
  dplyr::select(
    -output_file
  ) |>
  dplyr::left_join(
    fit_values,
    by = "model_id"
  ) |>
  dplyr::arrange(
    .data$model_order
  )

if (
  anyNA(
    fit_table$model_step
  ) ||
  anyNA(
    fit_table$model_specification
  )
) {
  stop(
    paste(
      "At least one Mplus output could not be matched",
      "to the model catalog."
    )
  )
}


##### ADD EXPLICIT REFERENCE-MODEL FIT #####

reference_fit <- fit_table |>
  dplyr::select(
    reference_model = model_id,
    reference_cfi = cfi,
    reference_rmsea = rmsea,
    reference_srmr = srmr
  )

fit_table <- fit_table |>
  dplyr::left_join(
    reference_fit,
    by = c(
      "comparison_model" =
        "reference_model"
    )
  ) |>
  dplyr::mutate(
    delta_cfi = dplyr::if_else(
      is.na(.data$reference_cfi),
      NA_real_,
      .data$cfi -
        .data$reference_cfi
    ),
    
    delta_rmsea = dplyr::if_else(
      is.na(.data$reference_rmsea),
      NA_real_,
      .data$rmsea -
        .data$reference_rmsea
    ),
    
    delta_srmr = dplyr::if_else(
      is.na(.data$reference_srmr),
      NA_real_,
      .data$srmr -
        .data$reference_srmr
    )
  )


##### DEFINE MISSING-VALUE FORMATTERS #####

format_number_or_dash <- function(
    x,
    digits = 2L
) {
  
  output <- rep(
    "—",
    length(x)
  )
  
  available <- !is.na(x)
  
  output[available] <- sprintf(
    paste0(
      "%.",
      digits,
      "f"
    ),
    x[available]
  )
  
  output
}


format_integer_or_dash <- function(x) {
  
  output <- rep(
    "—",
    length(x)
  )
  
  available <- !is.na(x)
  
  output[available] <- format(
    as.integer(
      x[available]
    ),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
  
  output
}


format_p_or_dash <- function(x) {
  
  output <- rep(
    "—",
    length(x)
  )
  
  available <- !is.na(x)
  
  output[available] <- format_p(
    x[available]
  )
  
  output
}


format_decimal_or_dash <- function(
    x,
    signed = FALSE
) {
  
  output <- rep(
    "—",
    length(x)
  )
  
  available <- !is.na(x)
  
  output[available] <- format_decimal(
    x[available],
    signed = signed
  )
  
  output
}


format_ci_or_dash <- function(
    estimate,
    lower,
    upper
) {
  
  output <- rep(
    "—",
    length(estimate)
  )
  
  available <-
    !is.na(estimate) &
    !is.na(lower) &
    !is.na(upper)
  
  output[available] <- format_ci(
    estimate[available],
    lower[available],
    upper[available]
  )
  
  output
}


##### PREPARE APA TABLE #####

apa_table_data <- fit_table |>
  dplyr::mutate(
    Model = .data$model_id,
    
    `Model family` =
      .data$model_family,
    
    `Model step` =
      .data$model_step,
    
    `Model specification` =
      .data$model_specification,
    
    N = format_integer_or_dash(
      .data$n
    ),
    
    `χ²` = format_number_or_dash(
      .data$chisq,
      digits = 2L
    ),
    
    df = format_integer_or_dash(
      .data$df
    ),
    
    p = format_p_or_dash(
      .data$p
    ),
    
    CFI = format_decimal_or_dash(
      .data$cfi
    ),
    
    TLI = format_decimal_or_dash(
      .data$tli
    ),
    
    `RMSEA [90% CI]` = format_ci_or_dash(
      .data$rmsea,
      .data$rmsea_lb,
      .data$rmsea_ub
    ),
    
    SRMR = format_decimal_or_dash(
      .data$srmr
    ),
    
    `ΔCFI` = format_decimal_or_dash(
      .data$delta_cfi,
      signed = TRUE
    ),
    
    `ΔRMSEA` = format_decimal_or_dash(
      .data$delta_rmsea,
      signed = TRUE
    ),
    
    `ΔSRMR` = format_decimal_or_dash(
      .data$delta_srmr,
      signed = TRUE
    )
  ) |>
  dplyr::select(
    Model,
    `Model family`,
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


##### FORMAT TABLE #####

apa_ft <- format_apa_table(
  data = apa_table_data,
  
  left_columns = c(
    "Model",
    "Model family",
    "Model step",
    "Model specification"
  ),
  
  widths = c(
    "Model" = 0.48,
    "Model family" = 1.15,
    "Model step" = 1.40,
    "Model specification" = 2.60,
    "N" = 0.45,
    "χ²" = 0.60,
    "df" = 0.42,
    "p" = 0.45,
    "CFI" = 0.43,
    "TLI" = 0.43,
    "RMSEA [90% CI]" = 1.08,
    "SRMR" = 0.48,
    "ΔCFI" = 0.48,
    "ΔRMSEA" = 0.58,
    "ΔSRMR" = 0.55
  ),
  
  font_size = 7.5,
  
  bold_rows =
    fit_table$retained_model
)

apa_ft <- flextable::italic(
  apa_ft,
  j = c(
    "N",
    "df",
    "p"
  ),
  part = "header"
)


##### SAVE WORD TABLE #####

table_note <- paste(
  "Note.",
  "Chi-square values are robust maximum likelihood (MLR)",
  "model test statistics.",
  "For the measurement-model development sequence, each model",
  "was compared with the preceding model, except that M6 and M7",
  "were compared with the final metric reference model M5.",
  "Change indices were not calculated for the latent change score",
  "models because these models differ in their predictors and,",
  "from M22 onward, their analytical samples.",
  "Consequently, fit indices should not be compared directly",
  "across models estimated in different samples.",
  "M7 represents the retained partial scalar measurement model;",
  "M22 represents the primary sociodemographically adjusted",
  "class-effects model.",
  "Boldface indicates these retained primary specifications.",
  "CFI = comparative fit index;",
  "TLI = Tucker–Lewis index;",
  "RMSEA = root mean square error of approximation;",
  "CI = confidence interval;",
  "SRMR = standardized root mean square residual.",
  "Dashes indicate that a statistic was not applicable",
  "or was not calculated."
)

table_output_file <- file.path(
  supplement_dir,
  paste0(
    "Table_",
    table_number,
    "_Panel_A_Fit_Statistics_SEMs.docx"
  )
)

save_apa_table(
  ft = apa_ft,
  number = table_number,
  title = table_title,
  note = table_note,
  target = table_output_file,
  landscape = TRUE
)


##### DISPLAY TABLE DATA #####

if (interactive()) {
  View(
    apa_table_data
  )
}
#-------------------------------------------------------------------------
##### TABLE SX: MALTREATMENT BURDEN CLASS ENUMERATION ####
#-------------------------------------------------------------------------

table_title <- paste(
  "Model Fit Statistics for Maltreatment Burden",
  "Trajectory Class Enumeration"
)


##### DEFINE FINAL MODEL OUTPUTS ####

enumeration_model_lookup <- tibble::tibble(
  model_number = 16:19,
  number_of_classes = 1:4,
  output_file = file.path(
    mplus_input_dir,
    c(
      "16_mt_burden_quadratic_1class.out",
      "17_mt_burden_quadratic_2class.out",
      "18_mt_burden_quadratic_3class.out",
      "19_mt_burden_quadratic_4class.out"
    )
  )
)

if (any(!file.exists(enumeration_model_lookup$output_file))) {
  stop(
    paste0(
      "The following class-enumeration output files are missing:\n",
      paste(
        enumeration_model_lookup$output_file[
          !file.exists(
            enumeration_model_lookup$output_file
          )
        ],
        collapse = "\n"
      )
    )
  )
}


##### DEFINE MPLUS ENUMERATION EXTRACTION HELPERS ####

extract_mplus_scalar <- function(
    output_lines,
    pattern,
    label,
    required = TRUE
) {
  matching_lines <- grep(
    pattern,
    output_lines,
    value = TRUE,
    perl = TRUE
  )
  
  if (length(matching_lines) == 0L) {
    if (!required) {
      return(
        NA_real_
      )
    }
    
    stop(
      label,
      " could not be found in the Mplus output."
    )
  }
  
  if (length(matching_lines) > 1L) {
    stop(
      label,
      " could not be identified uniquely in the Mplus output."
    )
  }
  
  value_text <- sub(
    pattern,
    "",
    matching_lines,
    perl = TRUE
  )
  
  numeric_match <- regexpr(
    "-?[0-9]+(?:\\.[0-9]+)?(?:[dDeE][+-]?[0-9]+)?",
    value_text,
    perl = TRUE
  )
  
  if (numeric_match[1] < 0L) {
    stop(
      label,
      " was found, but its numeric value could not be read."
    )
  }
  
  numeric_value <- regmatches(
    value_text,
    numeric_match
  )
  
  as.numeric(
    gsub(
      "[dD]",
      "E",
      numeric_value
    )
  )
}


extract_mplus_section_scalar <- function(
    output_lines,
    section_pattern,
    value_pattern,
    label,
    maximum_section_lines = 20L,
    required = FALSE
) {
  section_start <- grep(
    section_pattern,
    output_lines,
    perl = TRUE
  )
  
  if (length(section_start) == 0L) {
    if (!required) {
      return(
        NA_real_
      )
    }
    
    stop(
      label,
      " section could not be found in the Mplus output."
    )
  }
  
  if (length(section_start) > 1L) {
    stop(
      label,
      " section could not be identified uniquely."
    )
  }
  
  section_end <- min(
    length(output_lines),
    section_start + maximum_section_lines
  )
  
  section_lines <- output_lines[
    section_start:section_end
  ]
  
  extract_mplus_scalar(
    output_lines = section_lines,
    pattern = value_pattern,
    label = label,
    required = required
  )
}


extract_most_likely_class_sizes <- function(
    output_lines,
    number_of_classes,
    n
) {
  if (number_of_classes == 1L) {
    return(
      tibble::tibble(
        class_number = 1L,
        class_n = as.integer(n),
        class_proportion = 1
      )
    )
  }
  
  section_start <- grep(
    paste(
      "BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP"
    ),
    output_lines,
    fixed = TRUE
  )
  
  if (length(section_start) != 1L) {
    stop(
      paste(
        "The most-likely-class-membership section",
        "could not be identified uniquely."
      )
    )
  }
  
  later_classification_section <- grep(
    "^\\s*CLASSIFICATION QUALITY\\s*$",
    output_lines,
    perl = TRUE
  )
  
  later_classification_section <-
    later_classification_section[
      later_classification_section > section_start
    ]
  
  if (length(later_classification_section) < 1L) {
    stop(
      paste(
        "The end of the most-likely-class-membership",
        "section could not be identified."
      )
    )
  }
  
  class_size_lines <- output_lines[
    section_start:
      (later_classification_section[1] - 1L)
  ]
  
  class_size_lines <- grep(
    paste0(
      "^\\s*[0-9]+\\s+",
      "[0-9]+\\s+",
      "[01](?:\\.[0-9]+)?\\s*$"
    ),
    class_size_lines,
    value = TRUE,
    perl = TRUE
  )
  
  if (length(class_size_lines) != number_of_classes) {
    stop(
      paste0(
        "Expected ",
        number_of_classes,
        " class-size rows, but found ",
        length(class_size_lines),
        "."
      )
    )
  }
  
  class_size_values <- lapply(
    class_size_lines,
    function(line) {
      strsplit(
        trimws(line),
        "\\s+"
      )[[1]]
    }
  )
  
  class_size_data <- tibble::tibble(
    class_number = as.integer(
      vapply(
        class_size_values,
        `[[`,
        character(1),
        1L
      )
    ),
    class_n = as.integer(
      vapply(
        class_size_values,
        `[[`,
        character(1),
        2L
      )
    ),
    class_proportion = as.numeric(
      vapply(
        class_size_values,
        `[[`,
        character(1),
        3L
      )
    )
  )
  
  if (
    sum(class_size_data$class_n) != n ||
    abs(
      sum(class_size_data$class_proportion) - 1
    ) > 0.001
  ) {
    stop(
      "The extracted class sizes are inconsistent with the model sample size."
    )
  }
  
  class_size_data
}


format_class_sizes <- function(
    class_size_data
) {
  class_size_data |>
    dplyr::arrange(
      dplyr::desc(class_n)
    ) |>
    dplyr::transmute(
      class_size = paste0(
        class_n,
        " (",
        formatC(
          100 * class_proportion,
          format = "f",
          digits = 1
        ),
        "%)"
      )
    ) |>
    dplyr::pull(
      class_size
    ) |>
    paste(
      collapse = "; "
    )
}


extract_enumeration_model <- function(
    model_number,
    number_of_classes,
    output_file
) {
  output_lines <- readLines(
    output_file,
    warn = FALSE
  )
  
  n <- extract_mplus_scalar(
    output_lines,
    "^\\s*Number of observations\\s+",
    "Number of observations"
  )
  
  class_size_data <- extract_most_likely_class_sizes(
    output_lines = output_lines,
    number_of_classes = number_of_classes,
    n = n
  )
  
  tibble::tibble(
    model_number = model_number,
    number_of_classes = number_of_classes,
    n = n,
    free_parameters = extract_mplus_scalar(
      output_lines,
      "^\\s*Number of Free Parameters\\s+",
      "Number of free parameters"
    ),
    loglikelihood = extract_mplus_scalar(
      output_lines,
      "^\\s*H0 Value\\s+",
      "H0 loglikelihood"
    ),
    aic = extract_mplus_scalar(
      output_lines,
      "^\\s*Akaike \\(AIC\\)\\s+",
      "AIC"
    ),
    bic = extract_mplus_scalar(
      output_lines,
      "^\\s*Bayesian \\(BIC\\)\\s+",
      "BIC"
    ),
    abic = extract_mplus_scalar(
      output_lines,
      "^\\s*Sample-Size Adjusted BIC\\s+",
      "Sample-size adjusted BIC"
    ),
    entropy = extract_mplus_scalar(
      output_lines,
      "^\\s*Entropy\\s+",
      "Entropy",
      required = FALSE
    ),
    almr_p = extract_mplus_section_scalar(
      output_lines = output_lines,
      section_pattern = paste(
        "^\\s*LO-MENDELL-RUBIN ADJUSTED LRT TEST\\s*$"
      ),
      value_pattern = "^\\s*P-Value\\s+",
      label = "Adjusted LMR-LRT p value",
      maximum_section_lines = 5L,
      required = number_of_classes > 1L
    ),
    blrt_p = extract_mplus_section_scalar(
      output_lines = output_lines,
      section_pattern = paste(
        "^\\s*PARAMETRIC BOOTSTRAPPED LIKELIHOOD RATIO TEST"
      ),
      value_pattern = "^\\s*Approximate P-Value\\s+",
      label = "BLRT p value",
      maximum_section_lines = 10L,
      required = number_of_classes > 1L
    ),
    class_sizes = format_class_sizes(
      class_size_data
    )
  )
}


format_optional_decimal <- function(
    values
) {
  vapply(
    values,
    function(value) {
      if (is.na(value)) {
        "—"
      } else {
        format_decimal(value)
      }
    },
    character(1)
  )
}


format_optional_p <- function(
    values
) {
  vapply(
    values,
    function(value) {
      if (is.na(value)) {
        "—"
      } else {
        format_p(value)
      }
    },
    character(1)
  )
}


##### EXTRACT ENUMERATION STATISTICS ####

fit_table <- dplyr::bind_rows(
  lapply(
    seq_len(
      nrow(enumeration_model_lookup)
    ),
    function(row_number) {
      extract_enumeration_model(
        model_number =
          enumeration_model_lookup$model_number[
            row_number
          ],
        number_of_classes =
          enumeration_model_lookup$number_of_classes[
            row_number
          ],
        output_file =
          enumeration_model_lookup$output_file[
            row_number
          ]
      )
    }
  )
)


##### VERIFY EXTRACTED STATISTICS ####

stopifnot(
  identical(
    fit_table$model_number,
    16:19
  ),
  identical(
    fit_table$number_of_classes,
    1:4
  ),
  all(
    fit_table$n == 303
  ),
  all(
    is.finite(
      fit_table$loglikelihood
    )
  ),
  all(
    is.finite(
      fit_table$aic
    )
  ),
  all(
    is.finite(
      fit_table$bic
    )
  ),
  all(
    is.finite(
      fit_table$abic
    )
  ),
  is.na(
    fit_table$entropy[
      fit_table$number_of_classes == 1L
    ]
  ),
  all(
    is.finite(
      fit_table$entropy[
        fit_table$number_of_classes > 1L
      ]
    )
  )
)


##### PREPARE APA TABLE ####

apa_table_data <- fit_table |>
  dplyr::mutate(
    Model = paste0(
      "M",
      model_number
    ),
    Classes = as.integer(
      number_of_classes
    ),
    N = as.integer(n),
    `Free parameters` = as.integer(
      free_parameters
    ),
    LL = sprintf(
      "%.3f",
      loglikelihood
    ),
    AIC = sprintf(
      "%.3f",
      aic
    ),
    BIC = sprintf(
      "%.3f",
      bic
    ),
    aBIC = sprintf(
      "%.3f",
      abic
    ),
    Entropy = format_optional_decimal(
      entropy
    ),
    `aLMR-LRT, p` = format_optional_p(
      almr_p
    ),
    `BLRT, p` = format_optional_p(
      blrt_p
    ),
    `Class sizes, n (%)` = class_sizes
  ) |>
  dplyr::select(
    Model,
    Classes,
    N,
    `Free parameters`,
    LL,
    AIC,
    BIC,
    aBIC,
    Entropy,
    `aLMR-LRT, p`,
    `BLRT, p`,
    `Class sizes, n (%)`
  )


##### FORMAT TABLE ####

apa_ft <- format_apa_table(
  data = apa_table_data,
  left_columns = c(
    "Model",
    "Class sizes, n (%)"
  ),
  widths = c(
    "Model" = 0.45,
    "Classes" = 0.50,
    "N" = 0.45,
    "Free parameters" = 0.75,
    "LL" = 0.80,
    "AIC" = 0.70,
    "BIC" = 0.70,
    "aBIC" = 0.70,
    "Entropy" = 0.60,
    "aLMR-LRT, p" = 0.80,
    "BLRT, p" = 0.65,
    "Class sizes, n (%)" = 2.55
  ),
  font_size = 8,
  bold_rows = apa_table_data$Model == "M18"
)

apa_ft <- flextable::italic(
  apa_ft,
  j = "N",
  part = "header"
)

apa_ft <- flextable::compose(
  apa_ft,
  j = "aLMR-LRT, p",
  value = flextable::as_paragraph(
    "aLMR-LRT, ",
    flextable::as_i("p")
  ),
  part = "header"
)

apa_ft <- flextable::compose(
  apa_ft,
  j = "BLRT, p",
  value = flextable::as_paragraph(
    "BLRT, ",
    flextable::as_i("p")
  ),
  part = "header"
)

apa_ft <- flextable::compose(
  apa_ft,
  j = "Class sizes, n (%)",
  value = flextable::as_paragraph(
    "Class sizes, ",
    flextable::as_i("n"),
    " (%)"
  ),
  part = "header"
)


##### SAVE WORD TABLE ####

table_note <- paste(
  "All models were estimated using robust maximum likelihood (MLR) in",
  "the maltreated subsample. Growth-factor variances and covariances and",
  "indicator residual variances were constrained equal across classes;",
  "growth-factor means were estimated separately within each class.",
  "AIC = Akaike information criterion; BIC = Bayesian information",
  "criterion; aBIC = sample-size adjusted BIC; aLMR-LRT = adjusted",
  "Lo-Mendell-Rubin likelihood ratio test; BLRT = bootstrap likelihood",
  "ratio test. The aLMR-LRT and BLRT for a k-class model compare that",
  "model with the corresponding k - 1 class model. Class sizes are based",
  "on most likely latent class membership and are listed in descending",
  "order. All mixture models used 1,000 initial-stage random starts and",
  "250 final-stage optimizations, and the best loglikelihood value was",
  "replicated for each mixture model.",
  "Boldface indicates the retained three-class solution. Dashes indicate",
  "statistics that are not applicable to the one-class model."
)

table_output_file <- file.path(
  supplement_dir,
  paste0(
    "Table_",
    table_number,
    "_maltreatment_burden_class_enumeration_APA.docx"
  )
)

save_apa_table(
  ft = apa_ft,
  number = table_number,
  title = table_title,
  note = table_note,
  target = table_output_file,
  landscape = TRUE
)

if (interactive()) {
  View(
    apa_table_data
  )
}


#-----------------------------------------------------------------------
##### TABLE SX: CLASS CHARACTERISTICS #####
#-----------------------------------------------------------------------

##### TABLE SETTINGS #####

table_number <- "SX"

class_levels <- 1:4

class_labels <- c(
  "Non-maltreated",
  "Moderate/early-increasing burden",
  "Elevated/declining burden",
  "High/rebound burden"
)

class_columns <- c(
  "Overall",
  paste0(
    "Class ",
    class_levels,
    ": ",
    class_labels
  )
)

word_output_file <- file.path(
  supplement_dir,
  paste0(
    "Table_",
    table_number,
    "_class_characteristics_APA.docx"
  )
)

##### COMBINE TABLE SECTIONS #####

tab_apa <- bind_rows(
  tab_demo_apa,
  tab_psy_apa,
  tab_mal_apa
) %>%
  select(
    section,
    characteristic,
    all_of(
      class_columns
    ),
    test,
    p,
    effect_size
  )


##### DEFINE INTERNAL CLASS COLUMN NAMES #####

class_internal_names <- paste0(
  "class",
  seq_along(
    class_levels
  )
)

internal_column_names <- c(
  "overall",
  class_internal_names
)


##### REPLACE DISPLAY NAMES WITH SHORT INTERNAL NAMES #####

names(tab_apa)[
  match(
    class_columns,
    names(tab_apa)
  )
] <- internal_column_names


##### INSERT SECTION HEADINGS AS SEPARATE ROWS #####

section_order <- unique(
  tab_apa$section
)

tab_word <- map_dfr(
  section_order,
  function(current_section) {
    
    section_row <- tibble(
      section = current_section,
      characteristic = current_section
    )
    
    for (
      current_column in c(
        internal_column_names,
        "test",
        "p",
        "effect_size"
      )
    ) {
      section_row[[current_column]] <- ""
    }
    
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

n_overall <- sum(
  class_key$n
)

class_sizes <- class_key %>%
  complete(
    analysis_class_number = class_levels,
    fill = list(
      n = 0
    )
  ) %>%
  arrange(
    analysis_class_number
  ) %>%
  pull(
    n
  )


##### CREATE DYNAMIC HEADER LABELS #####

header_labels <- c(
  characteristic = "Characteristic",
  
  overall = paste0(
    "Overall\n(N = ",
    n_overall,
    ")"
  )
)

for (
  class_index in seq_along(
    class_levels
  )
) {
  
  current_internal_name <- class_internal_names[
    class_index
  ]
  
  header_labels[
    current_internal_name
  ] <- paste0(
    "Class ",
    class_levels[class_index],
    "\n",
    class_labels[class_index],
    "\n(n = ",
    class_sizes[class_index],
    ")"
  )
}

header_labels <- c(
  header_labels,
  test = "Test",
  p = "p",
  effect_size = "Effect size"
)


##### BUILD FLEXTABLE #####

ft_apa <- flextable(
  tab_word
)

ft_apa <- do.call(
  set_header_labels,
  c(
    list(
      x = ft_apa
    ),
    as.list(
      header_labels
    )
  )
)



ft_apa <- ft_apa %>%
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
  
  italic(
    j = "p",
    italic = TRUE,
    part = "header"
  ) %>%
  
  align(
    j = "characteristic",
    align = "left",
    part = "all"
  ) %>%
  
  align(
    j = c(
      internal_column_names,
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

##### ITALICIZE N AND n IN COLUMN HEADERS #####

ft_apa <- flextable::compose(
  ft_apa,
  j = "overall",
  part = "header",
  value = flextable::as_paragraph(
    flextable::as_chunk("Overall\n("),
    flextable::as_i("N"),
    flextable::as_chunk(
      paste0(
        " = ",
        n_overall,
        ")"
      )
    )
  )
)

for (
  class_index in seq_along(
    class_levels
  )
) {
  current_internal_name <- class_internal_names[
    class_index
  ]
  
  ft_apa <- flextable::compose(
    ft_apa,
    j = current_internal_name,
    part = "header",
    value = flextable::as_paragraph(
      flextable::as_chunk(
        paste0(
          "Class ",
          class_levels[class_index],
          "\n",
          class_labels[class_index],
          "\n("
        )
      ),
      flextable::as_i("n"),
      flextable::as_chunk(
        paste0(
          " = ",
          class_sizes[class_index],
          ")"
        )
      )
    )
  )
}


##### FORMAT SECTION HEADINGS #####

for (row in section_rows) {
  
  ft_apa <- ft_apa %>%
    merge_at(
      i = row,
      j = seq_len(
        ncol(
          tab_word
        )
      ),
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
    width = 2.40
  ) %>%
  
  width(
    j = "overall",
    width = 0.95
  ) %>%
  
  width(
    j = class_internal_names,
    width = 1.20
  ) %>%
  
  width(
    j = "test",
    width = 1.10
  ) %>%
  
  width(
    j = "p",
    width = 0.50
  ) %>%
  
  width(
    j = "effect_size",
    width = 0.80
  ) %>%
  
  set_table_properties(
    layout = "fixed",
    width = 1,
    opts_word = list(
      split = FALSE,
      keep_with_next = FALSE
    )
  )


##### DEFINE TABLE TITLE AND NOTE #####

table_title <- paste0(
  "Characteristics of the Non-Maltreated Reference Group and ",
  "Maltreatment Burden Trajectory Classes"
)

table_note_text <- paste0(
  "Values are M (SD) for continuous variables and n (%) for categorical ",
  "variables. Percentages are based on the available data for each variable. ",
  "Psychopathology scores are shown for the first caregiver, participant, ",
  "and second caregiver; teacher reports were excluded to align the ",
  "descriptive analyses with the final latent change models. Externalizing ",
  "problems were calculated as the sum of the SDQ conduct problems and ",
  "hyperactivity/inattention subscales. Harmonized maltreatment indicators ",
  "were based on the cumulative T2-all variable when available and otherwise ",
  "on the corresponding T1 variable, thereby reflecting the maximum available ",
  "maltreatment history for each participant. Values were coded as missing ",
  "when both variables were missing. Class 1 is the externally defined ",
  "non-maltreated reference group. Classes 2–4 correspond to the final M18 ",
  "latent trajectory classes estimated among maltreated participants: ",
  "moderate/early-increasing burden, elevated/declining burden, and ",
  "high/rebound burden, respectively. The non-maltreated reference group was ",
  "not included in the latent trajectory-class estimation. Inferential tests ",
  "compare the four displayed groups for demographic characteristics and ",
  "psychopathology. No inferential tests are reported for maltreatment ",
  "characteristics because maltreatment status defined the non-maltreated ",
  "reference group and the maltreatment indicators describe the trajectory ",
  "classes. η² = eta squared; V = Cramér's V; — = not estimated."
)


##### CREATE WORD DOCUMENT #####

doc <- read_docx()

##### LANDSCAPE PAGE FORMAT #####

landscape_section <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.27,
    height = 11.69
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
      paste0(
        "Table ",
        table_number
      ),
      fp_text(
        font.family = "Times New Roman",
        font.size = 12,
        bold = TRUE
      )
    ),
    fp_p = heading_paragraph_properties
    )
  )



##### TABLE TITLE #####

doc <- body_add_fpar(
  doc,
  fpar(
    ftext(
      table_title,
      fp_text(
        font.family = "Times New Roman",
        font.size = 11,
        italic = TRUE
      )
    ),
    fp_p = heading_paragraph_properties
  )
)


##### ADD TABLE #####

doc <- body_add_flextable(
  x = doc,
  value = ft_apa,
  align = "left"
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
      table_note_text,
      fp_text(
        font.family = "Times New Roman",
        font.size = 9
      )
    ),
    
    fp_p = note_paragraph_properties
  )
)


##### SAVE WORD DOCUMENT #####

print(
  doc,
  target = word_output_file
)

cat("\n\nAPA-style Word table saved to:\n",
  word_output_file,
  "\n",
  sep = ""
)



#-------------------------------------------------------------------------
##### TABLE SX, PANEL B: MALTREATMENT TRAJECTORY MODELS #####
#-------------------------------------------------------------------------

table_number <- "SX"

table_title <- paste(
  "Panel B. Fit and Classification Statistics for the",
  "Maltreatment Trajectory Models"
)


##### DEFINE TRAJECTORY MODEL CATALOG #####

trajectory_model_catalog <- tibble::tribble(
  ~model_id, ~model_order, ~output_stem,
  ~trajectory_dimension, ~functional_form,
  ~number_of_classes, ~model_specification,
  ~retained_model,
  
  "M9", 9L,
  "09_mt_subtypes_linear_1class",
  "Multiplicity",
  "Linear",
  1L,
  paste(
    "One-class linear growth model for the",
    "number of maltreatment subtypes"
  ),
  FALSE,
  
  "M10", 10L,
  "10_mt_frequency_linear_1class",
  "Frequency",
  "Linear",
  1L,
  paste(
    "One-class linear growth model for",
    "maltreatment frequency"
  ),
  FALSE,
  
  "M11", 11L,
  "11_mt_severity_linear_1class",
  "Severity",
  "Linear",
  1L,
  paste(
    "One-class linear growth model for",
    "maltreatment severity"
  ),
  FALSE,
  
  "M12", 12L,
  "12_mt_subtypes_quadratic_1class",
  "Multiplicity",
  "Quadratic",
  1L,
  paste(
    "One-class quadratic growth model for the",
    "number of maltreatment subtypes"
  ),
  FALSE,
  
  "M13", 13L,
  "13_mt_frequency_quadratic_1class",
  "Frequency",
  "Quadratic",
  1L,
  paste(
    "One-class quadratic growth model for",
    "maltreatment frequency"
  ),
  FALSE,
  
  "M14", 14L,
  "14_mt_severity_quadratic_1class",
  "Severity",
  "Quadratic",
  1L,
  paste(
    "One-class quadratic growth model for",
    "maltreatment severity"
  ),
  FALSE,
  
  "M15", 15L,
  "15_mt_parallel_quadratic_1class",
  "Parallel processes",
  "Quadratic",
  1L,
  paste(
    "Joint one-class quadratic parallel-process model",
    "of multiplicity, frequency, and severity"
  ),
  FALSE,
  
  "M16", 16L,
  "16_mt_burden_quadratic_1class",
  "Composite burden",
  "Quadratic",
  1L,
  paste(
    "One-class quadratic growth model of the",
    "standardized maltreatment burden index"
  ),
  FALSE,
  
  "M17", 17L,
  "17_mt_burden_quadratic_2class",
  "Composite burden",
  "Quadratic",
  2L,
  paste(
    "Two-class quadratic growth mixture model",
    "of maltreatment burden"
  ),
  FALSE,
  
  "M18", 18L,
  "18_mt_burden_quadratic_3class",
  "Composite burden",
  "Quadratic",
  3L,
  paste(
    "Three-class quadratic growth mixture model",
    "of maltreatment burden"
  ),
  TRUE,
  
  "M19", 19L,
  "19_mt_burden_quadratic_4class",
  "Composite burden",
  "Quadratic",
  4L,
  paste(
    "Four-class quadratic growth mixture model",
    "of maltreatment burden"
  ),
  FALSE
)


##### LOCATE OUTPUT FILES #####

trajectory_model_catalog <- trajectory_model_catalog |>
  dplyr::mutate(
    output_file = file.path(
      mplus_input_dir,
      paste0(
        .data$output_stem,
        ".out"
      )
    )
  )

missing_trajectory_output_files <-
  trajectory_model_catalog |>
  dplyr::filter(
    !file.exists(
      .data$output_file
    )
  )

if (nrow(missing_trajectory_output_files) > 0L) {
  stop(
    "The following trajectory-model outputs are missing:\n",
    paste(
      paste0(
        missing_trajectory_output_files$model_id,
        ": ",
        missing_trajectory_output_files$output_file
      ),
      collapse = "\n"
    )
  )
}


##### DEFINE TRAJECTORY-FIT EXTRACTION #####

read_mplus_trajectory_fit <- function(
    output_file,
    expected_classes
) {
  
  if (!file.exists(output_file)) {
    stop(
      "Mplus output file is missing:\n",
      output_file
    )
  }
  
  model <- MplusAutomation::readModels(
    output_file,
    quiet = TRUE
  )
  
  output_text <- readLines(
    output_file,
    warn = FALSE
  )
  
  terminated_normally <- any(
    grepl(
      "THE MODEL ESTIMATION TERMINATED NORMALLY",
      output_text,
      fixed = TRUE
    )
  )
  
  if (!terminated_normally) {
    stop(
      "The following model did not terminate normally:\n",
      output_file
    )
  }
  
  if (length(model$errors) > 0L) {
    stop(
      "Mplus errors were found in:\n",
      output_file,
      "\n",
      paste(
        unlist(model$errors),
        collapse = "\n"
      )
    )
  }
  
  model_summary <- model$summaries
  
  if (
    is.null(model_summary) ||
    length(model_summary) == 0L
  ) {
    stop(
      "No model summary could be extracted from:\n",
      output_file
    )
  }
  
  
  ##### EXTRACT VALUE FROM MPLUSAUTOMATION SUMMARY #####
  
  extract_summary_value <- function(variable) {
    
    value <- model_summary[[variable]]
    
    if (
      is.null(value) ||
      length(value) == 0L
    ) {
      return(
        NA_real_
      )
    }
    
    suppressWarnings(
      as.numeric(
        value[[1L]]
      )
    )
  }
  
  
  ##### EXTRACT FINAL NUMBER FROM OUTPUT LINE #####
  
  extract_final_number <- function(line) {
    
    if (
      length(line) == 0L ||
      is.na(line)
    ) {
      return(
        NA_real_
      )
    }
    
    number_text <- sub(
      paste0(
        "^.*?(",
        "[-+]?[0-9]*\\.?[0-9]+",
        "(?:[DEde][-+]?[0-9]+)?",
        ")\\s*$"
      ),
      "\\1",
      line,
      perl = TRUE
    )
    
    if (identical(number_text, line)) {
      return(
        NA_real_
      )
    }
    
    number_text <- gsub(
      "[Dd]",
      "E",
      number_text
    )
    
    suppressWarnings(
      as.numeric(
        number_text
      )
    )
  }
  
  
  ##### EXTRACT P VALUE FROM A SPECIFIC OUTPUT SECTION #####
  
  extract_section_p <- function(
    section_pattern,
    search_lines = 20L
  ) {
    
    section_index <- grep(
      section_pattern,
      output_text,
      ignore.case = TRUE,
      perl = TRUE
    )
    
    if (length(section_index) == 0L) {
      return(
        NA_real_
      )
    }
    
    section_index <- section_index[[1L]]
    
    section_end <- min(
      length(output_text),
      section_index + search_lines
    )
    
    section_text <- output_text[
      seq.int(
        section_index,
        section_end
      )
    ]
    
    p_line <- grep(
      "P-Value",
      section_text,
      ignore.case = TRUE,
      value = TRUE
    )
    
    if (length(p_line) == 0L) {
      return(
        NA_real_
      )
    }
    
    extract_final_number(
      p_line[[1L]]
    )
  }
  
  
  ##### EXTRACT ENTROPY #####
  
  entropy_line <- grep(
    "^\\s*Entropy\\s+",
    output_text,
    value = TRUE,
    perl = TRUE
  )
  
  entropy <- if (
    length(entropy_line) == 0L
  ) {
    
    extract_summary_value(
      "Entropy"
    )
    
  } else {
    
    extract_final_number(
      entropy_line[[1L]]
    )
  }
  
  
  ##### EXTRACT SMALLEST MOST-LIKELY CLASS #####
  
  smallest_class_n <- NA_real_
  smallest_class_proportion <- NA_real_
  
  class_section_start <- grep(
    paste(
      "BASED ON THEIR MOST LIKELY",
      "LATENT CLASS MEMBERSHIP"
    ),
    output_text,
    fixed = TRUE
  )
  
  if (
    expected_classes > 1L &&
    length(class_section_start) > 0L
  ) {
    
    class_section_start <-
      class_section_start[[1L]]
    
    following_lines <- seq.int(
      class_section_start,
      length(output_text)
    )
    
    class_section_end_candidates <-
      following_lines[
        grepl(
          "CLASSIFICATION QUALITY",
          output_text[following_lines],
          fixed = TRUE
        )
      ]
    
    class_section_end <- if (
      length(class_section_end_candidates) > 0L
    ) {
      
      class_section_end_candidates[[1L]]
      
    } else {
      
      min(
        length(output_text),
        class_section_start + 50L
      )
    }
    
    class_section <- output_text[
      seq.int(
        class_section_start,
        class_section_end
      )
    ]
    
    class_rows <- lapply(
      class_section,
      function(line) {
        
        fields <- strsplit(
          trimws(line),
          "\\s+"
        )[[1L]]
        
        valid_row <-
          length(fields) == 3L &&
          grepl(
            "^[0-9]+$",
            fields[[1L]]
          ) &&
          grepl(
            "^[0-9]+(?:\\.[0-9]+)?$",
            fields[[2L]]
          ) &&
          grepl(
            "^0?\\.[0-9]+$",
            fields[[3L]]
          )
        
        if (!valid_row) {
          return(
            NULL
          )
        }
        
        c(
          class = as.numeric(fields[[1L]]),
          count = as.numeric(fields[[2L]]),
          proportion = as.numeric(fields[[3L]])
        )
      }
    )
    
    class_rows <- Filter(
      Negate(is.null),
      class_rows
    )
    
    if (length(class_rows) > 0L) {
      
      class_count_table <- as.data.frame(
        do.call(
          rbind,
          class_rows
        )
      )
      
      class_count_table <- class_count_table |>
        dplyr::filter(
          .data$class %in%
            seq_len(expected_classes)
        ) |>
        dplyr::distinct(
          .data$class,
          .keep_all = TRUE
        )
      
      if (
        nrow(class_count_table) ==
        expected_classes
      ) {
        
        smallest_class_row <-
          class_count_table |>
          dplyr::slice_min(
            order_by = .data$count,
            n = 1L,
            with_ties = FALSE
          )
        
        smallest_class_n <-
          smallest_class_row$count[[1L]]
        
        smallest_class_proportion <-
          smallest_class_row$proportion[[1L]]
      }
    }
  }
  
  
  ##### RETURN EXTRACTED FIT #####
  
  tibble::tibble(
    n = extract_summary_value(
      "Observations"
    ),
    
    parameters = extract_summary_value(
      "Parameters"
    ),
    
    loglikelihood = extract_summary_value(
      "LL"
    ),
    
    aic = extract_summary_value(
      "AIC"
    ),
    
    bic = extract_summary_value(
      "BIC"
    ),
    
    abic = extract_summary_value(
      "aBIC"
    ),
    
    entropy = entropy,
    
    lmr_p = extract_section_p(
      "LO-MENDELL-RUBIN ADJUSTED LRT TEST"
    ),
    
    blrt_p = extract_section_p(
      paste(
        "PARAMETRIC BOOTSTRAPPED",
        "LIKELIHOOD RATIO TEST"
      )
    ),
    
    smallest_class_n =
      smallest_class_n,
    
    smallest_class_proportion =
      smallest_class_proportion,
    
    status =
      "Normal termination"
  )
}


##### EXTRACT TRAJECTORY-MODEL FIT #####

trajectory_fit_values <- purrr::map2_dfr(
  trajectory_model_catalog$output_file,
  trajectory_model_catalog$number_of_classes,
  
  function(
    output_file,
    number_of_classes
  ) {
    
    read_mplus_trajectory_fit(
      output_file = output_file,
      expected_classes = number_of_classes
    )
  },
  
  .id = "catalog_row"
)

trajectory_fit_values <- trajectory_fit_values |>
  dplyr::mutate(
    catalog_row = as.integer(
      .data$catalog_row
    )
  )

trajectory_fit_table <- trajectory_model_catalog |>
  dplyr::mutate(
    catalog_row = dplyr::row_number()
  ) |>
  dplyr::left_join(
    trajectory_fit_values,
    by = "catalog_row"
  ) |>
  dplyr::arrange(
    .data$model_order
  )


##### FORMAT FIT NUMBERS #####

format_fit_number_or_dash <- function(
    x,
    digits = 2L
) {
  
  output <- rep(
    "—",
    length(x)
  )
  
  available <- !is.na(x)
  
  output[available] <- format(
    round(
      x[available],
      digits = digits
    ),
    nsmall = digits,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
  
  output
}


format_smallest_class <- function(
    class_n,
    class_proportion
) {
  
  output <- rep(
    "—",
    length(class_n)
  )
  
  available <-
    !is.na(class_n) &
    !is.na(class_proportion)
  
  output[available] <- sprintf(
    "%d (%.1f%%)",
    as.integer(
      round(
        class_n[available]
      )
    ),
    100 *
      class_proportion[available]
  )
  
  output
}


##### PREPARE APA TABLE #####

trajectory_apa_table_data <- trajectory_fit_table |>
  dplyr::transmute(
    Model =
      .data$model_id,
    
    Dimension =
      .data$trajectory_dimension,
    
    Form =
      .data$functional_form,
    
    `No. of\nclasses` =
      as.character(
        .data$number_of_classes
      ),
    
    `Model specification` =
      .data$model_specification,
    
    N =
      format_integer_or_dash(
        .data$n
      ),
    
    `Free\nparameters` =
      format_integer_or_dash(
        .data$parameters
      ),
    
    `Log\nlikelihood` =
      format_fit_number_or_dash(
        .data$loglikelihood,
        digits = 2L
      ),
    
    AIC =
      format_fit_number_or_dash(
        .data$aic,
        digits = 2L
      ),
    
    BIC =
      format_fit_number_or_dash(
        .data$bic,
        digits = 2L
      ),
    
    `Adjusted\nBIC` =
      format_fit_number_or_dash(
        .data$abic,
        digits = 2L
      ),
    
    Entropy =
      format_decimal_or_dash(
        .data$entropy
      ),
    
    `LMR p` =
      format_p_or_dash(
        .data$lmr_p
      ),
    
    `BLRT p` =
      format_p_or_dash(
        .data$blrt_p
      ),
    
    `Smallest class,\nn (%)` =
      format_smallest_class(
        .data$smallest_class_n,
        .data$smallest_class_proportion
      )
  )


##### DEFINE AND SCALE COLUMN WIDTHS #####

trajectory_table_widths <- c(
  "Model" = 0.45,
  "Dimension" = 1.00,
  "Form" = 0.65,
  "No. of\nclasses" = 0.55,
  "Model specification" = 2.35,
  "N" = 0.45,
  "Free\nparameters" = 0.65,
  "Log\nlikelihood" = 0.78,
  "AIC" = 0.67,
  "BIC" = 0.67,
  "Adjusted\nBIC" = 0.78,
  "Entropy" = 0.58,
  "LMR p" = 0.52,
  "BLRT p" = 0.52,
  "Smallest class,\nn (%)" = 0.95
)

trajectory_table_widths <- scale_table_widths(
  widths = trajectory_table_widths,
  maximum_width = 10.40
)

if (
  !identical(
    names(
      trajectory_table_widths
    ),
    names(
      trajectory_apa_table_data
    )
  )
) {
  stop(
    paste(
      "The column names in trajectory_table_widths do not match",
      "the columns of trajectory_apa_table_data."
    )
  )
}


##### FORMAT TABLE #####

trajectory_apa_ft <- format_apa_table(
  data = trajectory_apa_table_data,
  
  left_columns = c(
    "Model",
    "Dimension",
    "Form",
    "Model specification"
  ),
  
  widths = trajectory_table_widths,
  
  font_size = 7.5,
  
  bold_rows =
    trajectory_fit_table$retained_model
)

trajectory_numeric_columns <- setdiff(
  names(
    trajectory_apa_table_data
  ),
  c(
    "Model",
    "Dimension",
    "Form",
    "Model specification"
  )
)

trajectory_apa_ft <- trajectory_apa_ft |>
  flextable::set_table_properties(
    layout = "fixed",
    align = "left"
  ) |>
  flextable::align(
    j = c(
      "Model",
      "Dimension",
      "Form",
      "Model specification"
    ),
    align = "left",
    part = "all"
  ) |>
  flextable::align(
    j = trajectory_numeric_columns,
    align = "center",
    part = "all"
  ) |>
  flextable::valign(
    valign = "center",
    part = "all"
  ) |>
  flextable::padding(
    padding.top = 2,
    padding.bottom = 2,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  )

trajectory_apa_ft <- flextable::italic(
  trajectory_apa_ft,
  j = c(
    "N",
    "LMR p",
    "BLRT p"
  ),
  part = "header"
)


##### DEFINE TABLE NOTE #####

trajectory_table_note <- paste(
  "All models were estimated using robust maximum likelihood.",
  "M9–M14 represent separate growth models for maltreatment",
  "multiplicity, frequency, and severity.",
  "M15 represents their joint quadratic parallel-process model.",
  "M16–M19 model the standardized composite maltreatment-burden",
  "index.",
  "The Lo–Mendell–Rubin adjusted likelihood-ratio test and the",
  "parametric bootstrapped likelihood-ratio test compare a",
  "k-class model with the corresponding k − 1 class model.",
  "Entropy and the smallest most-likely class are applicable only",
  "to models containing more than one latent class.",
  "M18 was retained based on statistical fit, classification",
  "quality, class sizes, stability, and substantive interpretability.",
  "The four-class M19 solution contained a very small additional",
  "class and was therefore not retained despite lower information",
  "criteria.",
  "Boldface indicates the retained model.",
  "AIC = Akaike information criterion;",
  "BIC = Bayesian information criterion;",
  "LMR = Lo–Mendell–Rubin adjusted likelihood-ratio test;",
  "BLRT = parametric bootstrapped likelihood-ratio test.",
  "Dashes indicate that a statistic was not applicable",
  "or was not available in the Mplus output."
)


##### DEFINE OUTPUT FILE #####

trajectory_table_output_file <- file.path(
  supplement_dir,
  paste0(
    "Table_",
    table_number,
    "_Panel_B_maltreatment_trajectory_models_APA.docx"
  )
)


##### CREATE WORD DOCUMENT #####

trajectory_doc <- officer::read_docx()

common_paragraph_properties <- officer::fp_par(
  text.align = "left",
  line_spacing = 1,
  padding = 0
)

heading_paragraph_properties <- officer::fp_par(
  text.align = "left",
  line_spacing = 2,
  padding = 0
)

note_paragraph_properties <- officer::fp_par(
  text.align = "left",
  line_spacing = 1,
  padding = 0
)


##### ADD TABLE NUMBER #####

trajectory_doc <- officer::body_add_fpar(
  x = trajectory_doc,
  value = officer::fpar(
    officer::ftext(
      paste0(
        "Table ",
        table_number
      ),
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 12,
        bold = TRUE
      )
    ),
    fp_p = heading_paragraph_properties
  )
)


##### ADD TABLE TITLE #####

trajectory_doc <- officer::body_add_fpar(
  x = trajectory_doc,
  value = officer::fpar(
    officer::ftext(
      table_title,
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 12,
        italic = TRUE
      )
    ),
    fp_p = heading_paragraph_properties
  )
)


##### ADD TABLE #####

trajectory_doc <- flextable::body_add_flextable(
  x = trajectory_doc,
  value = trajectory_apa_ft,
  align = "left"
)


##### ADD TABLE NOTE #####

trajectory_doc <- officer::body_add_fpar(
  x = trajectory_doc,
  value = officer::fpar(
    officer::ftext(
      "Note. ",
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 8,
        italic = TRUE
      )
    ),
    officer::ftext(
      trajectory_table_note,
      officer::fp_text(
        font.family = "Times New Roman",
        font.size = 8
      )
    ),
    fp_p = note_paragraph_properties
  )
)


##### SET A4 LANDSCAPE FORMAT #####

trajectory_landscape_section <- officer::prop_section(
  page_size = officer::page_size(
    orient = "landscape",
    width = 8.27,
    height = 11.69
  ),
  page_margins = officer::page_mar(
    top = 0.50,
    bottom = 0.50,
    left = 0.50,
    right = 0.50,
    header = 0.25,
    footer = 0.25
  ),
  type = "continuous"
)

trajectory_doc <- officer::body_end_block_section(
  x = trajectory_doc,
  value = officer::block_section(
    trajectory_landscape_section
  )
)


##### SAVE WORD TABLE #####

print(
  trajectory_doc,
  target = trajectory_table_output_file
)

message(
  "Saved trajectory-model fit table:\n",
  trajectory_table_output_file
)


##### DISPLAY TABLE DATA #####

if (interactive()) {
  View(
    trajectory_apa_table_data
  )
}


