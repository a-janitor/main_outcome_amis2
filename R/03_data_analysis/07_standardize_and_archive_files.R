#-----------------------------------------------------------------------
##### STANDARDIZE AND ARCHIVE MAIN OUTCOME FILES #####
#-----------------------------------------------------------------------

# Safety first: DRY_RUN is TRUE by default. Review the printed migration
# plan, then set DRY_RUN <- FALSE to create copies. This script never deletes
# source files.

source(
  "C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R"
)

DRY_RUN <- FALSE
CREATE_LOCAL_SNAPSHOT <- FALSE
snapshot_stamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
local_snapshot_dir <- file.path(mplus_local_archive_dir, snapshot_stamp)

if (!exists("mplus_results_inputs_dir")) {
  stop("Install the standardized 00_setup.R before running this script.")
}

models <- tibble::tribble(
  ~model, ~role, ~input_file, ~output_file,
  "M20", "main", "M20_sdq_classical_lcs_final_dataset.inp", "m20_sdq_classical_lcs_final_dataset.out",
  "M21", "main", "M21_lcs_with_LTC_classes.inp", "m21_lcs_with_ltc_classes.out",
  "M22", "main", "M22_lcs_with_LTC_classes_ageT2_sex.inp", "m22_lcs_with_ltc_classes_aget2_sex.out",
  "M24a", "main", "M24a_lcs_classes_PRS_EAU.inp", "m24a_lcs_classes_prs_eau.out",
  "M25", "main", "M25_lcs_classes_hair_cortisol.inp", "m25_lcs_classes_hair_cortisol.out",
  "M26a", "main", "M26a_lcs_classes_HCC_PRS_EAU.inp", "m26a_lcs_classes_hcc_prs_eau.out",
  "M21K", "sensitivity", "M21_alternative_lcs_known_classes.inp", "m21_alternative_lcs_known_classes.out",
  "M21CC", "sensitivity", "M21cc_lcs_with_LTC_classes_T5age_complete_cases.inp", "m21cc_lcs_with_ltc_classes_t5age_complete_cases.out",
  "M22-T5-age", "sensitivity", "M22_lcs_with_LTC_classes_age_sex.inp", "m22_lcs_with_ltc_classes_age_sex.out",
  "M22-follow-up", "sensitivity", "M22b_lcs_classes_ageT2_sex_followup.inp", "m22b_lcs_classes_aget2_sex_followup.out",
  "M24b", "exploratory", "M24b_lcs_classes_PRS_MAU.inp", "m24b_lcs_classes_prs_mau.out",
  "M26b", "exploratory", "M26b_lcs_classes_HCC_PRS_MAU.inp", "m26b_lcs_classes_hcc_prs_mau.out"
)

files_identical <- function(
    source,
    target
) {
  
  if (
    !file.exists(source) ||
    !file.exists(target)
  ) {
    return(FALSE)
  }
  
  source_size <- file.info(source)$size
  target_size <- file.info(target)$size
  
  if (
    is.na(source_size) ||
    is.na(target_size) ||
    source_size != target_size
  ) {
    return(FALSE)
  }
  
  identical(
    unname(
      tools::md5sum(source)
    ),
    unname(
      tools::md5sum(target)
    )
  )
}

copy_checked <- function(
    source,
    target_dir,
    dry_run = TRUE
) {
  
  target <- file.path(
    target_dir,
    basename(source)
  )
  
  if (!file.exists(source)) {
    
    status <- "MISSING"
    
  } else if (files_identical(source, target)) {
    
    status <- "SKIPPED_IDENTICAL"
    
  } else if (dry_run) {
    
    status <- "WOULD_COPY"
    
  } else {
    
    dir.create(
      target_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    copied <- file.copy(
      from = source,
      to = target,
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
    
    status <- if (copied) {
      "COPIED"
    } else {
      "FAILED"
    }
  }
  
  tibble::tibble(
    source = source,
    target = target,
    status = status
  )
}

migration <- purrr::pmap_dfr(
  models,
  function(model, role, input_file, output_file) {
    input_source <- file.path(mplus_input_dir, input_file)
    output_source <- file.path(mplus_input_dir, output_file)
    permanent_input_dir <- if (role == "main") {
      mplus_results_inputs_dir
    } else {
      file.path(mplus_results_sensitivity_dir, role, "inputs")
    }
    permanent_output_dir <- if (role == "main") {
      mplus_results_outputs_dir
    } else {
      file.path(mplus_results_sensitivity_dir, role, "outputs")
    }

    rows <- dplyr::bind_rows(
      copy_checked(
        input_source,
        permanent_input_dir,
        DRY_RUN
      ),
      copy_checked(
        output_source,
        permanent_output_dir,
        DRY_RUN
      )
    )
    
    if (CREATE_LOCAL_SNAPSHOT) {
      rows <- dplyr::bind_rows(
        rows,
        copy_checked(
          input_source,
          file.path(local_snapshot_dir, role),
          DRY_RUN
        ),
        copy_checked(
          output_source,
          file.path(local_snapshot_dir, role),
          DRY_RUN
        )
      )
    }
    
    rows |>
      dplyr::mutate(
        model = model,
        role = role,
        .before = 1
      )
  }
)

print(migration, n = Inf)

if (!DRY_RUN && CREATE_LOCAL_SNAPSHOT) {
  
  dir.create(
    local_snapshot_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  utils::write.csv(
    migration,
    file.path(
      local_snapshot_dir,
      "migration_log.csv"
    ),
    row.names = FALSE
  )
}

cat(
  "\nDRY_RUN = ", DRY_RUN, "\n",
  "No source files are deleted by this script.\n",
  sep = ""
)


