###############################################################################
# PROJECT: AMIS-II MAIN OUTCOME PAPER
# SCRIPT: 01_sdq_parcel_invariance_fs.R
# PURPOSE: Test longitudinal measurement invariance of Fateme's SDQ parcel model
###############################################################################

### ------------------------------------------------------------------------ ###
### 0. SETUP
### ------------------------------------------------------------------------ ###

source("R/01_data_preprocessing/00_setup_paths_packages.R")
source("R/01_data_preprocessing/01_create_functions.R")


### ------------------------------------------------------------------------ ###
### 1. LOAD / CREATE SDQ PARCEL DATA
### ------------------------------------------------------------------------ ###

if (!exists("df_parcels_fs")) {
  source("R/02_data_exploration/01_CFG_systematic_2Parcel.R")
}

df <- df_parcels_fs


### ------------------------------------------------------------------------ ###
### 2. SETTINGS
### ------------------------------------------------------------------------ ###

subs_all <- c("emo", "con", "hyp", "peer", "pros")
waves <- c("t2", "t5")

use_wlsmv <- FALSE


### ------------------------------------------------------------------------ ###
### 3. PREPARE PARCEL VARIABLES
### ------------------------------------------------------------------------ ###

to_num <- function(z) {
  if (is.factor(z)) z <- as.character(z)
  suppressWarnings(as.numeric(z))
}

parcel_cols <- unlist(lapply(subs_all, function(s) {
  c(
    paste0(s, "_parA_t2"),
    paste0(s, "_parB_t2"),
    paste0(s, "_parA_t5"),
    paste0(s, "_parB_t5")
  )
}))

parcel_cols <- intersect(parcel_cols, names(df))

if (length(parcel_cols) == 0) {
  stop("No parcel columns found. Please check df_parcels_fs / column names.")
}

df[parcel_cols] <- lapply(df[parcel_cols], to_num)


### ------------------------------------------------------------------------ ###
### 4. MODEL-BUILDING FUNCTIONS
### ------------------------------------------------------------------------ ###

build_trait_lines <- function(subs, inv = c("configural", "metric", "scalar")) {
  
  inv <- match.arg(inv)
  lines <- c()
  
  for (s in subs) {
    
    f2 <- paste0(s, "_t2")
    f5 <- paste0(s, "_t5")
    
    pa2 <- paste0(s, "_parA_t2")
    pb2 <- paste0(s, "_parB_t2")
    pa5 <- paste0(s, "_parA_t5")
    pb5 <- paste0(s, "_parB_t5")
    
    if (inv == "configural") {
      lines <- c(
        lines,
        sprintf("%s =~ %s + %s", f2, pa2, pb2),
        sprintf("%s =~ %s + %s", f5, pa5, pb5)
      )
    }
    
    if (inv %in% c("metric", "scalar")) {
      
      l1 <- paste0("l1_", s)
      l2 <- paste0("l2_", s)
      
      lines <- c(
        lines,
        sprintf("%s =~ %s*%s + %s*%s", f2, l1, pa2, l2, pb2),
        sprintf("%s =~ %s*%s + %s*%s", f5, l1, pa5, l2, pb5)
      )
    }
    
    if (inv == "scalar") {
      
      i1 <- paste0("i1_", s)
      i2 <- paste0("i2_", s)
      
      lines <- c(
        lines,
        sprintf("%s ~ %s*1", pa2, i1),
        sprintf("%s ~ %s*1", pa5, i1),
        sprintf("%s ~ %s*1", pb2, i2),
        sprintf("%s ~ %s*1", pb5, i2)
      )
    }
  }
  
  return(lines)
}


build_xwave_resid <- function(subs) {
  
  unlist(lapply(subs, function(s) {
    c(
      paste0(s, "_parA_t2 ~~ ", s, "_parA_t5"),
      paste0(s, "_parB_t2 ~~ ", s, "_parB_t5")
    )
  }))
}


build_withinwave_factor_corr <- function(subs, wave) {
  
  facs <- paste0(subs, "_", wave)
  
  if (length(facs) < 2) return(character(0))
  
  combn(
    facs,
    2,
    FUN = function(x) paste0(x[1], " ~~ ", x[2])
  )
}


build_stability <- function(subs) {
  
  paste0(subs, "_t2 ~~ ", subs, "_t5")
}


build_model <- function(inv = c("configural", "metric", "scalar")) {
  
  inv <- match.arg(inv)
  
  trait_lines <- build_trait_lines(subs_all, inv)
  xwave_resid <- build_xwave_resid(subs_all)
  corr_t2 <- build_withinwave_factor_corr(subs_all, "t2")
  corr_t5 <- build_withinwave_factor_corr(subs_all, "t5")
  stab <- build_stability(subs_all)
  
  paste(
    c(trait_lines, xwave_resid, corr_t2, corr_t5, stab),
    collapse = "\n"
  )
}


safe_fit <- function(model_syntax, estimator = "MLR") {
  
  args <- list(
    model = model_syntax,
    data = df,
    missing = "fiml",
    estimator = estimator,
    std.lv = TRUE,
    meanstructure = TRUE
  )
  
  if (estimator == "WLSMV") {
    args$ordered <- parcel_cols
  }
  
  fit <- try(do.call(lavaan::cfa, args), silent = TRUE)
  
  if (inherits(fit, "try-error") || !lavaan::lavInspect(fit, "converged")) {
    return(NULL)
  }
  
  return(fit)
}


fit_and_report <- function(inv) {
  
  syn <- build_model(inv = inv)
  
  fit <- safe_fit(
    syn,
    estimator = if (use_wlsmv) "WLSMV" else "MLR"
  )
  
  if (is.null(fit)) {
    return(NULL)
  }
  
  fit_summary <- lavaan::fitMeasures(
    fit,
    c("cfi", "tli", "rmsea", "srmr", "chisq", "df", "pvalue")
  )
  
  return(
    list(
      syntax = syn,
      fit = fit,
      fit_summary = fit_summary
    )
  )
}


### ------------------------------------------------------------------------ ###
### 5. RUN INVARIANCE MODELS
### ------------------------------------------------------------------------ ###

fit_cfg <- fit_and_report("configural")
fit_met <- fit_and_report("metric")
fit_sca <- fit_and_report("scalar")


### ------------------------------------------------------------------------ ###
### 6. COLLECT FIT INDICES
### ------------------------------------------------------------------------ ###

fit_indices <- dplyr::bind_rows(
  if (!is.null(fit_cfg)) {
    tibble::tibble(
      model = "configural",
      fit = list(fit_cfg$fit),
      cfi = fit_cfg$fit_summary["cfi"],
      tli = fit_cfg$fit_summary["tli"],
      rmsea = fit_cfg$fit_summary["rmsea"],
      srmr = fit_cfg$fit_summary["srmr"],
      chisq = fit_cfg$fit_summary["chisq"],
      df = fit_cfg$fit_summary["df"],
      pvalue = fit_cfg$fit_summary["pvalue"]
    )
  },
  if (!is.null(fit_met)) {
    tibble::tibble(
      model = "metric",
      fit = list(fit_met$fit),
      cfi = fit_met$fit_summary["cfi"],
      tli = fit_met$fit_summary["tli"],
      rmsea = fit_met$fit_summary["rmsea"],
      srmr = fit_met$fit_summary["srmr"],
      chisq = fit_met$fit_summary["chisq"],
      df = fit_met$fit_summary["df"],
      pvalue = fit_met$fit_summary["pvalue"]
    )
  },
  if (!is.null(fit_sca)) {
    tibble::tibble(
      model = "scalar",
      fit = list(fit_sca$fit),
      cfi = fit_sca$fit_summary["cfi"],
      tli = fit_sca$fit_summary["tli"],
      rmsea = fit_sca$fit_summary["rmsea"],
      srmr = fit_sca$fit_summary["srmr"],
      chisq = fit_sca$fit_summary["chisq"],
      df = fit_sca$fit_summary["df"],
      pvalue = fit_sca$fit_summary["pvalue"]
    )
  }
)

print(fit_indices)


### ------------------------------------------------------------------------ ###
### 7. MODEL COMPARISON USING FIT-INDEX CHANGES (ADAPTED BY JAN)
### ------------------------------------------------------------------------ ###

extract_fit <- function(fit_object, model_name) {
  
  if (is.null(fit_object) || is.null(fit_object$fit)) {
    return(NULL)
  }
  
  fit <- fit_object$fit
  
  tibble::tibble(
    model = model_name,
    cfi = lavaan::fitMeasures(fit, "cfi"),
    tli = lavaan::fitMeasures(fit, "tli"),
    rmsea = lavaan::fitMeasures(fit, "rmsea"),
    srmr = lavaan::fitMeasures(fit, "srmr"),
    chisq = lavaan::fitMeasures(fit, "chisq"),
    df = lavaan::fitMeasures(fit, "df"),
    pvalue = lavaan::fitMeasures(fit, "pvalue")
  )
}

fit_compare <- dplyr::bind_rows(
  extract_fit(fit_cfg, "configural"),
  extract_fit(fit_met, "metric"),
  extract_fit(fit_sca, "scalar")
) %>%
  dplyr::mutate(
    delta_cfi = c(NA, diff(cfi)),
    delta_tli = c(NA, diff(tli)),
    delta_rmsea = c(NA, diff(rmsea)),
    delta_srmr = c(NA, diff(srmr))
  )

print(fit_compare)

### ------------------------------------------------------------------------ ###
### 8. SAVE CHECK OUTPUTS
### ------------------------------------------------------------------------ ###

sdq_measurement_dir <- file.path(data_dir, "../03a_outputs/sdq_measurement_models")

if (!dir.exists(sdq_measurement_dir)) {
  dir.create(sdq_measurement_dir, recursive = TRUE)
}

readr::write_csv(
  fit_indices %>% dplyr::select(-fit),
  file.path(sdq_measurement_dir, "sdq_parcel_invariance_fit_indices_fs.csv")
)

if (!is.null(fit_cfg)) {
  writeLines(
    fit_cfg$syntax,
    file.path(sdq_measurement_dir, "sdq_parcel_invariance_configural_syntax_fs.txt")
  )
}

if (!is.null(fit_met)) {
  writeLines(
    fit_met$syntax,
    file.path(sdq_measurement_dir, "sdq_parcel_invariance_metric_syntax_fs.txt")
  )
}

if (!is.null(fit_sca)) {
  writeLines(
    fit_sca$syntax,
    file.path(sdq_measurement_dir, "sdq_parcel_invariance_scalar_syntax_fs.txt")
  )
}


### ------------------------------------------------------------------------ ###
### 9. OPTIONAL PLOT
### ------------------------------------------------------------------------ ###

if (!is.null(fit_cfg) && requireNamespace("semPlot", quietly = TRUE)) {
  
  png(
    filename = file.path(sdq_measurement_dir, "sdq_parcel_configural_plot_fs.png"),
    width = 1800,
    height = 1200,
    res = 200
  )
  
  semPlot::semPaths(
    fit_cfg$fit,
    "std",
    layout = "tree",
    whatLabels = "std",
    edge.label.cex = .8,
    sizeMan = 4,
    sizeLat = 6
  )
  
  dev.off()
}


### ------------------------------------------------------------------------ ###
### 10. SESSION INFO
### ------------------------------------------------------------------------ ###

sessionInfo()

###############################################################################
# END OF SCRIPT
###############################################################################