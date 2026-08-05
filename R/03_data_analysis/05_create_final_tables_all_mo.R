#-----------------------------------------------------------------------
##### CREATE FINAL TABLES: MAIN OUTCOME PAPER #####
#-----------------------------------------------------------------------

source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R")
source("C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_helpers_final_tables.R")

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


#-------------------------------------------------------------------------
##### TABLE X: SDQ MEASUREMENT INVARIANCE SDQ T2 - T5 #####
#-------------------------------------------------------------------------

table_number <- "SX"

table_title <- paste(
  "Measurement Model Development and Longitudinal Measurement",
  "Invariance Testing of the SDQ Externalizing and Emotional",
  "Problems Factors"
)


##### DEFINE MODELS #####

model_catalog <- tibble::tribble(
  ~model, ~model_step, ~model_specification,
  
  "01_sdq_configural",
  "Configural model",
  paste(
    "Same factor structure across T2 and T5;",
    "loadings and indicator intercepts freely estimated"
  ),
  
  "02_sdq_longitudinal_resid",
  "Longitudinal residuals",
  paste(
    "Longitudinal residuals",
    "added across T2 and T5"
  ),
  
  "03_sdq_metric_invariance",
  "Metric invariance",
  "Factor loadings constrained to equality across T2 and T5",
  
  "04_sdq_within_informant_crossscale",
  "Within-informant residuals",
  paste(
    "Within-informant cross-scale residual covariances",
    "added and constrained across time"
  ),
  
  "05_sdq_between_informant_residuals",
  "Final metric model",
  paste(
    "Selected between-informant residual covariances",
    "added and constrained across time"
  ),
  
  "06_sdq_full_scalar_invariance",
  "Full scalar invariance",
  paste(
    "All indicator intercepts constrained to equality;",
    "T2 latent means fixed to zero and T5 latent means freely estimated"
  ),
  
  "07_sdq_partial_scalar_invariance",
  "Final partial scalar model",
  paste(
    "Child-report emotional-problems indicator intercept",
    "freely estimated across time; all remaining",
    "indicator intercepts constrained to equality"
  )
)


##### LOCATE OUTPUT FILES #####

# runModels() created the outputs beside the Mplus input files
invariance_output_files <- file.path(
  mplus_input_dir,
  paste0(
    model_catalog$model,
    ".out"
  )
)

missing_output_files <- invariance_output_files[
  !file.exists(invariance_output_files)
]

if (length(missing_output_files) > 0) {
  stop(
    "The following Mplus output files are missing:\n",
    paste(missing_output_files, collapse = "\n")
  )
}


##### EXTRACT MODEL FIT #####

fit_table <- purrr::map_dfr(
  invariance_output_files,
  read_mplus_fit
) |>
  dplyr::left_join(
    model_catalog,
    by = "model"
  ) |>
  dplyr::mutate(
    model_number = match(
      model,
      model_catalog$model
    )
  ) |>
  dplyr::arrange(model_number)

if (
  anyNA(fit_table$model_step) ||
  anyNA(fit_table$model_specification)
) {
  stop("At least one Mplus output could not be matched to the model catalog.")
}


##### DEFINE METRIC REFERENCE MODEL #####

metric_reference <- fit_table |>
  dplyr::filter(
    model == "05_sdq_between_informant_residuals"
  )

if (nrow(metric_reference) != 1) {
  stop("The metric reference model M5 could not be identified uniquely.")
}


##### PREPARE APA TABLE #####

apa_table_data <- fit_table |>
  dplyr::mutate(
    Model = paste0("M", model_number),
    `Model step` = model_step,
    `Model specification` = model_specification,
    N = as.integer(n),
    `χ²` = sprintf("%.2f", chisq),
    df = as.integer(df),
    p = format_p(p),
    CFI = format_decimal(cfi),
    TLI = format_decimal(tli),
    `RMSEA [90% CI]` = format_ci(
      rmsea,
      rmsea_lb,
      rmsea_ub
    ),
    SRMR = format_decimal(srmr),
    cfi_reference = dplyr::case_when(
      model_number == 1 ~ NA_real_,
      model_number == 7 ~ metric_reference$cfi[[1]],
      TRUE ~ dplyr::lag(cfi)
    ),
    
    rmsea_reference = dplyr::case_when(
      model_number == 1 ~ NA_real_,
      model_number == 7 ~ metric_reference$rmsea[[1]],
      TRUE ~ dplyr::lag(rmsea)
    ),
    
    srmr_reference = dplyr::case_when(
      model_number == 1 ~ NA_real_,
      model_number == 7 ~ metric_reference$srmr[[1]],
      TRUE ~ dplyr::lag(srmr)
    ),
    
    `ΔCFI` = dplyr::if_else(
      is.na(cfi_reference),
      "—",
      format_decimal(
        cfi - cfi_reference,
        signed = TRUE
      )
    ),
    
    `ΔRMSEA` = dplyr::if_else(
      is.na(rmsea_reference),
      "—",
      format_decimal(
        rmsea - rmsea_reference,
        signed = TRUE
      )
    ),
    
    `ΔSRMR` = dplyr::if_else(
      is.na(srmr_reference),
      "—",
      format_decimal(
        srmr - srmr_reference,
        signed = TRUE
      )
    )
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


##### FORMAT TABLE #####

apa_ft <- format_apa_table(
  data = apa_table_data,
  left_columns = c(
    "Model",
    "Model step",
    "Model specification"
  ),
  widths = c(
    "Model" = 0.40,
    "Model step" = 1.15,
    "Model specification" = 2.65,
    "N" = 0.45,
    "χ²" = 0.60,
    "df" = 0.40,
    "p" = 0.45,
    "CFI" = 0.45,
    "TLI" = 0.45,
    "RMSEA [90% CI]" = 1.10,
    "SRMR" = 0.50,
    "ΔCFI" = 0.50,
    "ΔRMSEA" = 0.60,
    "ΔSRMR" = 0.55
  ),
  font_size = 8,
  bold_rows = apa_table_data$Model == "M7"
)

apa_ft <- flextable::italic(
  apa_ft,
  j = c("N", "df", "p"),
  part = "header"
)


##### SAVE WORD TABLE #####

table_note <- paste(
  "Chi-square values are robust maximum likelihood (MLR) model test",
  "statistics. M5 served as the metric reference model for comparisons",
  "with M6 and M7. In M6 and M7, T2 latent means were fixed to zero and",
  "T5 latent means were freely estimated. Boldface indicates the retained",
  "final model. CFI = comparative fit index; TLI = Tucker–Lewis index;",
  "RMSEA = root mean square error of approximation; CI = confidence",
  "interval; SRMR = standardized root mean square residual. Dashes",
  "indicate that change indices were not calculated for the",
  "model-development steps preceding M5."
)

table_output_file <- file.path(
  supplement_dir,
  paste0(
    "Table_",
    table_number,
    "_SDQ_measurement_invariance_APA.docx"
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
  View(apa_table_data)
}


#-----------------------------------------------------------------------
##### SELECT CLASSIFICATION VARIANT #####
#-----------------------------------------------------------------------

# Use "original" for the original three-class solution or "mo" for the
# non-maltreated reference group plus the three maltreated LTC classes.
classification_variant <- "mo"

if (classification_variant == "original") {

  variant_suffix <- ""

  class_labels <- c(
    "Low and stable",
    "Elevated and declining",
    "High early burden with later rebound"
  )

  model_files <- list(
    m21 = c(
      "m21_lcs_with_ltc_classes.out"
    ),
    m22 = c(
      "m22_lcs_with_ltc_classes_aget2_sex.out",
      "m22_lcs_with_ltc_classes_age_sex.out"
    ),
    m24a = c(
      "m24a_lcs_classes_prs_eau.out"
    ),
    m25 = c(
      "m25_lcs_classes_hair_cortisol.out"
    ),
    m26a = c(
      "m26a_lcs_classes_hcc_prs_eau.out"
    )
  )

} else if (classification_variant == "mo") {

  variant_suffix <- "_mo"

  class_labels <- c(
    "Non-maltreated",
    "Moderate/early-increasing burden",
    "Elevated/declining burden",
    "High/rebound burden"
  )

  model_files <- list(
    m21 = c(
      "m21_lcs_with_ltc_classes_mo.out"
    ),
    m22 = c(
      "m22_lcs_with_ltc_classes_age_sex_mo.out",
      "m22_lcs_with_ltc_classes_aget2_sex_mo.out"
    ),
    m24a = c(
      "m24a_lcs_classes_prs_eau_mo.out"
    ),
    m25 = c(
      "m25_lcs_classes_hair_cortisol_mo.out"
    ),
    m26a = c(
      "m26a_lcs_classes_hcc_prs_eau_mo.out"
    )
  )

} else {

  stop(
    "classification_variant must be either 'original' or 'mo'."
  )
}

# Compatibility fallback while the standardized setup is being installed.
if (!exists("tables_manuscript_dir")) {
  tables_manuscript_dir <- file.path(mplus_results_dir, "07_tables", "manuscript")
}
if (!exists("tables_appendix_dir")) {
  tables_appendix_dir <- file.path(mplus_results_dir, "07_tables", "appendix")
}

dir.create(tables_manuscript_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_appendix_dir, recursive = TRUE, showWarnings = FALSE)


#-----------------------------------------------------------------------
##### READ FINAL MODELS #####
#-----------------------------------------------------------------------

m21 <- read_model(
  model_files$m21
)

m22 <- read_model(
  model_files$m22
)

m24a <- read_model(
  model_files$m24a
)

m25 <- read_model(
  model_files$m25
)

m26a <- read_model(
  model_files$m26a
)


#-----------------------------------------------------------------------
##### REPORT FILES USED #####
#-----------------------------------------------------------------------

model_file_overview <- tibble::tibble(
  Model = c(
    "M21",
    "M22",
    "M24a",
    "M25",
    "M26a"
  ),
  File = c(
    attr(m21, "source_file"),
    attr(m22, "source_file"),
    attr(m24a, "source_file"),
    attr(m25, "source_file"),
    attr(m26a, "source_file")
  )
)

print(
  model_file_overview,
  n = Inf
)


#-----------------------------------------------------------------------
##### TABLE 1: LTC CLASS EFFECTS #####
#-----------------------------------------------------------------------

outcome_specs <- tibble::tribble(
  ~Outcome, ~prefix_reference, ~prefix_pairwise, ~outcome_order,
  "Baseline externalizing problems", "EX2", "EX", 1,
  "Baseline emotional problems", "EM2", "EM", 2,
  "Change in externalizing problems", "DEX", "DX", 3,
  "Change in emotional problems", "DEM", "DM", 4
)


create_class_key <- function(
    labels,
    variant
) {

  n_classes <- length(
    labels
  )

  reference_rows <- purrr::map_dfr(
    2:n_classes,
    function(class_number) {

      outcome_specs |>
        dplyr::transmute(
          parameter = paste0(
            .data$prefix_reference,
            "_C",
            class_number
          ),
          Outcome = .data$Outcome,
          Contrast = paste0(
            labels[class_number],
            " vs ",
            labels[1]
          ),
          outcome_order = .data$outcome_order,
          contrast_order = class_number - 1
        )
    }
  )

  pairwise_rows <- tibble::tibble()

  if (n_classes >= 3) {

    class_pairs <- utils::combn(
      2:n_classes,
      2
    )

    pairwise_rows <- purrr::map_dfr(
      seq_len(
        ncol(class_pairs)
      ),
      function(pair_index) {

        lower_class <- class_pairs[
          1,
          pair_index
        ]

        higher_class <- class_pairs[
          2,
          pair_index
        ]

        outcome_specs |>
          dplyr::transmute(
            parameter = paste0(
              .data$prefix_pairwise,
              "_",
              higher_class,
              "V",
              lower_class
            ),
            Outcome = .data$Outcome,
            Contrast = paste0(
              labels[higher_class],
              " vs ",
              labels[lower_class]
            ),
            outcome_order = .data$outcome_order,
            contrast_order =
              n_classes - 1 + pair_index
          )
      }
    )
  }

  class_key <- dplyr::bind_rows(
    reference_rows,
    pairwise_rows
  ) |>
    dplyr::arrange(
      .data$outcome_order,
      .data$contrast_order
    ) |>
    dplyr::mutate(
      order = dplyr::row_number()
    ) |>
    dplyr::select(
      .data$parameter,
      .data$Outcome,
      .data$Contrast,
      .data$order
    )

  if (variant == "original") {

    expected_parameters <- c(
      "EX2_C2", "EX2_C3", "EX_3V2",
      "EM2_C2", "EM2_C3", "EM_3V2",
      "DEX_C2", "DEX_C3", "DX_3V2",
      "DEM_C2", "DEM_C3", "DM_3V2"
    )

    class_key <- class_key |>
      dplyr::filter(
        .data$parameter %in%
          expected_parameters
      ) |>
      dplyr::mutate(
        order = match(
          .data$parameter,
          expected_parameters
        )
      ) |>
      dplyr::arrange(
        .data$order
      )
  }

  class_key
}


class_key <- create_class_key(
  labels = class_labels,
  variant = classification_variant
)


extract_class_model <- function(model, label) {
  purrr::map_dfr(class_key$parameter, function(parameter) {
    result <- extract_new(model, parameter)
    tibble::tibble(
      parameter = parameter,
      Model = label,
      estimate = format_estimate(result$estimate, result$se),
      p = format_p(result$p)
    )
  })
}

class_results <- dplyr::bind_rows(
  extract_class_model(m21, "M21: Unadjusted"),
  extract_class_model(m22, "M22: Adjusted")
) |>
  dplyr::left_join(class_key, by = "parameter") |>
  tidyr::pivot_wider(
    id_cols = c("Outcome", "Contrast", "order"),
    names_from = "Model",
    values_from = c("estimate", "p")
  ) |>
  dplyr::arrange(.data$order) |>
  dplyr::transmute(
    Outcome,
    Contrast,
    `M21, b [95% CI]` = `estimate_M21: Unadjusted`,
    `p` = `p_M21: Unadjusted`,
    `M22, b [95% CI]` = `estimate_M22: Adjusted`,
    `p ` = `p_M22: Adjusted`
  )

table_1_ft <- format_apa_table(
  class_results,
  left_columns = c("Outcome", "Contrast")
) |>
  flextable::merge_v(j = "Outcome") |>
  flextable::valign(j = "Outcome", valign = "top")

utils::write.csv(
  class_results,
  file.path(tables_manuscript_dir, paste0("Table_1_LTC_class_effects", variant_suffix, ".csv")),
  row.names = FALSE
)

#-----------------------------------------------------------------------
##### TABLE 2: BIOLOGICAL PREDICTORS #####
#-----------------------------------------------------------------------

bio_key <- tibble::tribble(
  ~Predictor, ~Outcome, ~Header,
  "Depression PRI", "Baseline externalizing problems", "EXT2 ON",
  "Depression PRI", "Baseline emotional problems", "EMO2 ON",
  "Depression PRI", "Change in externalizing problems", "DEXT ON",
  "Depression PRI", "Change in emotional problems", "DEMO ON",
  "Hair cortisol", "Baseline externalizing problems", "EXT2 ON",
  "Hair cortisol", "Baseline emotional problems", "EMO2 ON",
  "Hair cortisol", "Change in externalizing problems", "DEXT ON",
  "Hair cortisol", "Change in emotional problems", "DEMO ON"
)

extract_bio_row <- function(Predictor, Outcome, Header) {
  if (Predictor == "Depression PRI") {
    separate <- extract_regression(m24a, Header, "PRS_EAU")
    joint <- extract_regression(m26a, Header, "PRS_EAU")
  } else {
    separate <- extract_regression(m25, Header, "C2P1_Z")
    joint <- extract_regression(m26a, Header, "C2P1_Z")
  }
  tibble::tibble(
    Predictor = Predictor,
    Outcome = Outcome,
    `Separate model, b [95% CI]` = format_estimate(separate$estimate, separate$se),
    `p` = format_p(separate$p),
    `Joint model, b [95% CI]` = format_estimate(joint$estimate, joint$se),
    `p ` = format_p(joint$p)
  )
}

bio_results <- purrr::pmap_dfr(bio_key, extract_bio_row)

table_2_ft <- format_apa_table(
  bio_results,
  left_columns = c("Predictor", "Outcome")
) |>
  flextable::merge_v(j = "Predictor") |>
  flextable::valign(j = "Predictor", valign = "top")

utils::write.csv(
  bio_results,
  file.path(tables_manuscript_dir, paste0("Table_2_biological_predictors", variant_suffix, ".csv")),
  row.names = FALSE
)

#-----------------------------------------------------------------------
##### TABLE A1: MODEL FIT #####
#-----------------------------------------------------------------------

summary_row <- function(model, label) {
  s <- as.data.frame(model$summaries)
  get_value <- function(candidates) {
    hit <- intersect(candidates, names(s))
    if (length(hit) == 0) return(NA_real_)
    as.numeric(s[[hit[1]]][1])
  }
  n <- get_value(c("Observations", "N", "nobs"))
  chisq <- get_value(c("ChiSqM_Value", "ChiSq", "chisq"))
  df <- get_value(c("ChiSqM_DF", "DF", "df"))
  cfi <- get_value(c("CFI", "cfi"))
  tli <- get_value(c("TLI", "tli"))
  rmsea <- get_value(c("RMSEA_Estimate", "RMSEA", "rmsea"))
  srmr <- get_value(c("SRMR", "srmr"))
  tibble::tibble(
    Model = label,
    N = n,
    `chi-square (df)` = sprintf("%.2f (%.0f)", chisq, df),
    CFI = sprintf("%.3f", cfi),
    TLI = sprintf("%.3f", tli),
    RMSEA = sprintf("%.3f", rmsea),
    SRMR = sprintf("%.3f", srmr)
  )
}

fit_results <- dplyr::bind_rows(
  summary_row(m21, "M21: Classes, unadjusted"),
  summary_row(m22, "M22: Classes, age and gender adjusted"),
  summary_row(m24a, "M24a: European-ancestry depression PRI"),
  summary_row(m25, "M25: Hair cortisol"),
  summary_row(m26a, "M26a: Joint PRI and hair cortisol")
)

table_a1_ft <- format_apa_table(fit_results, left_columns = "Model")

utils::write.csv(
  fit_results,
  file.path(tables_appendix_dir, paste0("Table_A1_model_fit", variant_suffix, ".csv")),
  row.names = FALSE
)

#-----------------------------------------------------------------------
##### WRITE WORD DOCUMENTS #####
#-----------------------------------------------------------------------

main_doc <- officer::read_docx()
main_doc <- add_table_to_doc(
  main_doc,
  "Table 1",
  if (classification_variant == "mo") {
    "Associations Between the Non-Maltreated Reference Group, Maltreatment Burden Trajectory Classes, and Psychopathology"
  } else {
    "Associations Between Maltreatment Burden Trajectory Classes and Psychopathology"
  },
  table_1_ft,
  paste0(
    "Note. M21 is unadjusted; M22 is adjusted for baseline age and gender. ",
    if (classification_variant == "mo") {
      paste0(
        "Class 1 is the externally defined non-maltreated reference group. ",
        "Classes 2-4 are the latent maltreatment burden trajectory classes ",
        "estimated among maltreated participants. "
      )
    } else {
      paste0(
        "Class 1 (low and stable burden) is the reference class. "
      )
    },
    "Estimates are unstandardized coefficients with 95% confidence intervals."
  )
)
main_doc <- add_table_to_doc(
  main_doc,
  "Table 2",
  "Associations of Biological Predictors With Baseline Psychopathology and Latent Change",
  table_2_ft,
  paste0(
    "Note. Separate models refer to M24a for the European-ancestry depression PRI ",
    "and M25 for hair cortisol. The joint model is M26a. All models include the ",
    if (classification_variant == "mo") {
      "non-maltreated reference group and maltreatment burden classes, baseline age, and gender; "
    } else {
      "maltreatment burden classes, baseline age, and gender; "
    },
    "PRI models additionally include PC1-PC4."
  ),
  page_break = TRUE
)
print(
  main_doc,
  target = file.path(tables_manuscript_dir, paste0("Manuscript_Tables_1_2", variant_suffix, ".docx"))
)

appendix_doc <- officer::read_docx()
appendix_doc <- add_table_to_doc(
  appendix_doc,
  "Table A1",
  "Fit Indices and Sample Sizes of the Reported Latent Change Score Models",
  table_a1_ft,
  "Note. All fit indices were extracted directly from the final Mplus output files."
)
print(
  appendix_doc,
  target = file.path(tables_appendix_dir, paste0("Appendix_Table_A1_model_fit", variant_suffix, ".docx"))
)

utils::write.csv(
  model_file_overview,
  file.path(
    tables_appendix_dir,
    paste0(
      "Model_files_used",
      variant_suffix,
      ".csv"
    )
  ),
  row.names = FALSE
)

cat(
  "\nTables created successfully.\n",
  "Classification variant: ", classification_variant, "\n",
  "Manuscript: ", tables_manuscript_dir, "\n",
  "Appendix: ", tables_appendix_dir, "\n",
  sep = ""
)
