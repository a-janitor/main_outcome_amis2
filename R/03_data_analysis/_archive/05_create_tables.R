#-----------------------------------------------------------------------
##### CREATE FINAL TABLES: MAIN OUTCOME PAPER #####
#-----------------------------------------------------------------------

# This script can be run independently after the Mplus models have run.
# It creates the manuscript and appendix tables for M21-M26.

source(
  "C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R"
)

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
##### HELPERS #####
#-----------------------------------------------------------------------

clean_text <- function(x) {
  toupper(gsub("[^A-Za-z0-9]+", "", trimws(as.character(x))))
}

read_model <- function(filename) {
  path <- file.path(mplus_input_dir, filename)
  if (!file.exists(path)) stop("Mplus output not found: ", path)
  MplusAutomation::readModels(
    path,
    what = c("summaries", "parameters", "warn_err"),
    quiet = TRUE
  )
}

parameter_table <- function(model) {
  x <- tibble::as_tibble(model$parameters$unstandardized)
  names(x) <- tolower(names(x))
  x |>
    dplyr::mutate(
      header_clean = clean_text(.data$paramheader),
      param_clean = clean_text(.data$param)
    )
}

extract_new <- function(model, parameter_name) {
  pars <- parameter_table(model)
  match <- pars |>
    dplyr::filter(
      grepl("NEW|ADDITIONAL", .data$header_clean),
      .data$param_clean == clean_text(parameter_name)
    )
  if (nrow(match) != 1) {
    stop("Expected one new parameter ", parameter_name, "; found ", nrow(match))
  }
  list(
    estimate = as.numeric(match$est[1]),
    se = as.numeric(match$se[1]),
    p = as.numeric(match$pval[1])
  )
}

extract_regression <- function(model, header, predictor) {
  pars <- parameter_table(model)
  match <- pars |>
    dplyr::filter(
      .data$header_clean == clean_text(header),
      .data$param_clean == clean_text(predictor)
    )
  if (nrow(match) != 1) {
    stop(
      "Expected one regression for ", header, " / ", predictor,
      "; found ", nrow(match)
    )
  }
  list(
    estimate = as.numeric(match$est[1]),
    se = as.numeric(match$se[1]),
    p = as.numeric(match$pval[1])
  )
}

format_p <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "",
    x < .001 ~ "< .001",
    TRUE ~ sub("^0", "", sprintf("%.3f", x))
  )
}

format_estimate <- function(b, se) {
  sprintf("%.2f [%.2f, %.2f]", b, b - 1.96 * se, b + 1.96 * se)
}

apa_rule <- officer::fp_border(
  color = "#000000",
  width = 2.75,
  style = "single"
)

format_apa_table <- function(data, left_columns, widths = NULL) {
  ft <- flextable::flextable(data) |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::bold(part = "header") |>
    flextable::align(j = left_columns, align = "left", part = "all") |>
    flextable::align(
      j = setdiff(names(data), left_columns),
      align = "center",
      part = "all"
    ) |>
    flextable::valign(valign = "center", part = "all") |>
    flextable::padding(
      padding.top = 3, padding.bottom = 3,
      padding.left = 3, padding.right = 3,
      part = "all"
    ) |>
    flextable::border_remove() |>
    flextable::set_table_properties(layout = "fixed", width = 1, align = "left")

  if (!is.null(widths)) {
    for (column in names(widths)) {
      ft <- flextable::width(ft, j = column, width = widths[[column]])
    }
  }

  ft <- flextable::fix_border_issues(ft)
  ft <- flextable::hline_top(ft, part = "header", border = apa_rule)
  ft <- flextable::hline_bottom(ft, part = "header", border = apa_rule)
  ft <- flextable::hline_bottom(ft, part = "body", border = apa_rule)
  ft
}

add_table_to_doc <- function(doc, number, title, ft, note = NULL, page_break = FALSE) {
  if (page_break) doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, number, style = "Normal")
  doc <- officer::body_add_par(doc, title, style = "Normal")
  doc <- flextable::body_add_flextable(doc, value = ft)
  if (!is.null(note)) doc <- officer::body_add_par(doc, note, style = "Normal")
  doc
}

#-----------------------------------------------------------------------
##### READ FINAL MODELS #####
#-----------------------------------------------------------------------

m21 <- read_model("m21_lcs_with_ltc_classes.out")
m22 <- read_model("m22_lcs_with_ltc_classes_aget2_sex.out")
m24a <- read_model("m24a_lcs_classes_prs_eau.out")
m25 <- read_model("m25_lcs_classes_hair_cortisol.out")
m26a <- read_model("m26a_lcs_classes_hcc_prs_eau.out")

#-----------------------------------------------------------------------
##### TABLE 1: LTC CLASS EFFECTS #####
#-----------------------------------------------------------------------

class_key <- tibble::tribble(
  ~parameter, ~Outcome, ~Contrast, ~order,
  "EX2_C2", "Baseline externalizing problems", "Elevated and declining vs low and stable", 1,
  "EX2_C3", "Baseline externalizing problems", "High early burden with later rebound vs low and stable", 2,
  "EX_3V2", "Baseline externalizing problems", "High early burden with later rebound vs elevated and declining", 3,
  "EM2_C2", "Baseline emotional problems", "Elevated and declining vs low and stable", 4,
  "EM2_C3", "Baseline emotional problems", "High early burden with later rebound vs low and stable", 5,
  "EM_3V2", "Baseline emotional problems", "High early burden with later rebound vs elevated and declining", 6,
  "DEX_C2", "Change in externalizing problems", "Elevated and declining vs low and stable", 7,
  "DEX_C3", "Change in externalizing problems", "High early burden with later rebound vs low and stable", 8,
  "DX_3V2", "Change in externalizing problems", "High early burden with later rebound vs elevated and declining", 9,
  "DEM_C2", "Change in emotional problems", "Elevated and declining vs low and stable", 10,
  "DEM_C3", "Change in emotional problems", "High early burden with later rebound vs low and stable", 11,
  "DM_3V2", "Change in emotional problems", "High early burden with later rebound vs elevated and declining", 12
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
  file.path(tables_manuscript_dir, "Table_1_LTC_class_effects.csv"),
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
  file.path(tables_manuscript_dir, "Table_2_biological_predictors.csv"),
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
  file.path(tables_appendix_dir, "Table_A1_model_fit.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------------------
##### WRITE WORD DOCUMENTS #####
#-----------------------------------------------------------------------

main_doc <- officer::read_docx()
main_doc <- add_table_to_doc(
  main_doc,
  "Table 1",
  "Associations Between Maltreatment Trajectory Classes and Psychopathology",
  table_1_ft,
  paste0(
    "Note. M21 is unadjusted; M22 is adjusted for baseline age and gender. ",
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
    "maltreatment classes, baseline age, and gender; PRI models additionally include PC1-PC4."
  ),
  page_break = TRUE
)
print(
  main_doc,
  target = file.path(tables_manuscript_dir, "Manuscript_Tables_1_2.docx")
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
  target = file.path(tables_appendix_dir, "Appendix_Table_A1_model_fit.docx")
)

cat(
  "\nTables created successfully.\n",
  "Manuscript: ", tables_manuscript_dir, "\n",
  "Appendix: ", tables_appendix_dir, "\n",
  sep = ""
)
