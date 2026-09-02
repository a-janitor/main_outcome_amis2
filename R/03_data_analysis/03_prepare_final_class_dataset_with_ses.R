#-------------------------------------------------------------------------
##### PREPARE FINAL EXCEL AND MPLUS DATASETS: M18 CLASSES + SES ####
#-------------------------------------------------------------------------

source(
  paste0(
    "C:/Users/keil/Documents/main_outcome_amis2/",
    "R/03_data_analysis/00_setup_standardized.R"
  )
)

check_packages(
  c(
    "dplyr",
    "readr",
    "readxl",
    "writexl",
    "tibble",
    "MplusAutomation",
    "openxlsx"
  ),
  required = TRUE
)


#-------------------------------------------------------------------------
##### DEFINE SOURCE FILES ####
#-------------------------------------------------------------------------

# Original Excel dataset. This file is read only and never overwritten.
original_excel_file_mo <- master_excel_file

# Original Mplus dataset and its established ordered variable names.
original_mplus_data_file_mo <- seadrive_mplus_data_file
original_mplus_names_file_mo <- seadrive_mplus_names_file

# Final maltreated-only three-class solution.
m18_output_file_mo <- file.path(
  mplus_input_dir,
  "18_mt_burden_quadratic_3class.out"
)


##### DEFINE COMPLEMENTARY SES SOURCES #####

# AMIS 1 SES sources.
ses_source_directory_amis1_mo <- file.path(
  main_outcome_dir,
  "02_data",
  "00_raw_final",
  "AMIS 1",
  "data"
)

# AMIS 2 SES source.
ses_source_directory_amis2_mo <- file.path(
  main_outcome_dir,
  "02_data",
  "00_raw_final",
  "AMIS 2",
  "data"
)

ses_source_files_mo <- c(
  T2 = file.path(
    ses_source_directory_amis1_mo,
    "PV0880_T00631_NODUP.xlsx"
  ),
  
  T5 = file.path(
    ses_source_directory_amis2_mo,
    "PV0880_T01368_NODUP.xlsx"
  ),
  
  DFG02 = file.path(
    ses_source_directory_amis1_mo,
    "PV0880_T01640_NODUP.xlsx"
  ),
  
  JA = file.path(
    ses_source_directory_amis1_mo,
    "PV0880_T01656_NODUP.xlsx"
  )
)

ses_id_variables_mo <- c(
  T2 = "SES_B_T2_SIC",
  T5 = "SIC",
  DFG02 = "SIC",
  JA = "SIC"
)

ses_value_variables_mo <- c(
  T2 = "SES_B_T2_AUSB1_M",
  T5 = "SES_B_T5_AUSB1_M",
  DFG02 = "SES_BP_DFG02_AUSB1_M",
  JA = "SES_BP_JA_AUSB1_M"
)

# All four source variables measure the same construct using the same
# 0-4 coding. Missing values are completed according to the order:
# T2, T5, DFG02, and JA.
ses_source_priority_mo <- c(
  "T2",
  "DFG02",
  "JA",
  "T5"
)

# Name of the combined SES variable in the final Excel dataset.
ses_final_variable_mo <- "SES_AUSB1_M_COMBINED"

# Additional variable documenting the source of the selected SES value.
ses_source_indicator_mo <- "SES_AUSB1_M_SOURCE"

ses_excel_variables_mo <- c(
  ses_final_variable_mo,
  ses_source_indicator_mo
)

# Existing original Excel-to-Mplus variable mapping.
original_dictionary_file_mo <- mplus_variable_dictionary_file


#-------------------------------------------------------------------------
##### DEFINE FINAL OUTPUT FILES ####
#-------------------------------------------------------------------------

final_excel_file_mo <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes_SES_mo.xlsx"
)

final_mplus_basename_mo <- "AMIS_mplus_dataset_m18_ses_mo"

final_mplus_data_file_mo <- file.path(
  seadrive_mplus_data_dir,
  paste0(
    final_mplus_basename_mo,
    ".dat"
  )
)

final_mplus_names_file_mo <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names_m18_ses_mo.rds"
)

final_mplus_names_text_file_mo <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names_m18_ses_mo.txt"
)

final_mplus_rds_file_mo <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_dataset_m18_ses_mo.rds"
)

# M20-M24 read the same final files from the local Mplus directory.
local_final_mplus_data_file_mo <- file.path(
  mplus_input_dir,
  basename(
    final_mplus_data_file_mo
  )
)

local_final_mplus_names_file_mo <- file.path(
  mplus_input_dir,
  basename(
    final_mplus_names_file_mo
  )
)

local_final_mplus_names_text_file_mo <- file.path(
  mplus_input_dir,
  basename(
    final_mplus_names_text_file_mo
  )
)

local_final_mplus_rds_file_mo <- file.path(
  mplus_input_dir,
  basename(
    final_mplus_rds_file_mo
  )
)

final_dictionary_csv_file_mo <- file.path(
  data_prep_dir,
  "mplus_variable_dictionary_final_mo.csv"
)

final_dictionary_xlsx_file_mo <- file.path(
  data_prep_dir,
  "mplus_variable_dictionary_final_mo.xlsx"
)


#-------------------------------------------------------------------------
##### CHECK SOURCE FILES ####
#-------------------------------------------------------------------------

source_files_mo <- c(
  original_excel_file_mo,
  original_mplus_data_file_mo,
  original_mplus_names_file_mo,
  m18_output_file_mo,
  unname(
    ses_source_files_mo
  ),
  original_dictionary_file_mo
)

missing_source_files_mo <- source_files_mo[
  !file.exists(
    source_files_mo
  )
]

if (
  length(
    missing_source_files_mo
  ) > 0L
) {
  stop(
    paste0(
      "Missing source files:\n",
      paste0(
        "- ",
        missing_source_files_mo,
        collapse = "\n"
      )
    )
  )
}

dir.create(
  data_prep_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  seadrive_mplus_data_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


#-------------------------------------------------------------------------
##### DEFINE FINAL CLASS INFORMATION ####
#-------------------------------------------------------------------------

class_levels_mo <- 1:4

class_labels_mo <- c(
  "Non-maltreated",
  "Moderate/early-increasing burden",
  "Elevated/declining burden",
  "High/rebound burden"
)

expected_class_sizes_mo <- c(
  `1` = 281L,
  `2` = 223L,
  `3` = 42L,
  `4` = 38L
)

expected_m18_avepp_mo <- c(
  `2` = 0.986,
  `3` = 0.991,
  `4` = 0.941
)

class_excel_variables_mo <- c(
  "mt_class_mo",
  "mt_prob_class1_mo",
  "mt_prob_class2_mo",
  "mt_prob_class3_mo",
  "mt_prob_class4_mo",
  "mt_prob_max_mo",
  "mt_class_label_mo"
)

# Text labels remain in Excel only. All other new variables are appended to
# the original Mplus dataset in this exact order.
new_mplus_mapping_mo <- tibble::tribble(
  ~original_name,       ~mplus_name,
  "mt_class_mo",        "mo_cls",
  "mt_prob_class1_mo",  "mo_p1",
  "mt_prob_class2_mo",  "mo_p2",
  "mt_prob_class3_mo",  "mo_p3",
  "mt_prob_class4_mo",  "mo_p4",
  "mt_prob_max_mo",     "mo_pmx",
  "SES_AUSB1_M_COMBINED", "sesausb"
)

stopifnot(
  all(
    nchar(
      new_mplus_mapping_mo$mplus_name
    ) <= 8L
  ),
  anyDuplicated(
    tolower(
      new_mplus_mapping_mo$mplus_name
    )
  ) == 0L
)


#-------------------------------------------------------------------------
##### READ ORIGINAL EXCEL DATASET ####
#-------------------------------------------------------------------------

original_excel_data_mo <- readxl::read_excel(
  original_excel_file_mo,
  na = c(
    "",
    "NA"
  )
)

stopifnot(
  nrow(
    original_excel_data_mo
  ) > 0L,
  "sic" %in% names(
    original_excel_data_mo
  ),
  !anyNA(
    original_excel_data_mo$sic
  ),
  anyDuplicated(
    original_excel_data_mo$sic
  ) == 0L
)

standardize_sic_mo <- function(x) {
  x <- toupper(
    trimws(
      as.character(
        x
      )
    )
  )
  
  x[
    x == ""
  ] <- NA_character_
  
  x
}

sic_lookup_mo <- original_excel_data_mo |>
  dplyr::transmute(
    sic = standardize_sic_mo(
      sic
    )
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    sic
  ) |>
  dplyr::mutate(
    SIC_N = dplyr::row_number()
  )

stopifnot(
  !anyNA(
    sic_lookup_mo$sic
  ),
  anyDuplicated(
    sic_lookup_mo$sic
  ) == 0L,
  anyDuplicated(
    sic_lookup_mo$SIC_N
  ) == 0L
)


#-------------------------------------------------------------------------
##### READ ORIGINAL MPLUS DATASET WITHOUT CHANGING IT ####
#-------------------------------------------------------------------------

original_mplus_names_mo <- readRDS(
  original_mplus_names_file_mo
)

stopifnot(
  is.character(
    original_mplus_names_mo
  ),
  length(
    original_mplus_names_mo
  ) > 0L,
  all(
    nchar(
      original_mplus_names_mo
    ) <= 8L
  ),
  anyDuplicated(
    tolower(
      original_mplus_names_mo
    )
  ) == 0L
)

original_mplus_data_mo <- readr::read_delim(
  file = original_mplus_data_file_mo,
  delim = "\t",
  col_names = original_mplus_names_mo,
  na = "-999",
  trim_ws = TRUE,
  col_types = readr::cols(
    .default = readr::col_double()
  ),
  progress = FALSE,
  name_repair = "minimal"
)

stopifnot(
  identical(
    names(
      original_mplus_data_mo
    ),
    original_mplus_names_mo
  ),
  "SIC_N" %in% names(
    original_mplus_data_mo
  ),
  "mal_all" %in% names(
    original_mplus_data_mo
  ),
  !anyNA(
    original_mplus_data_mo$SIC_N
  ),
  anyDuplicated(
    original_mplus_data_mo$SIC_N
  ) == 0L,
  all(
    vapply(
      original_mplus_data_mo,
      is.numeric,
      logical(1)
    )
  )
)


#-------------------------------------------------------------------------
##### READ FINAL M18 CLASS-PROBABILITY OUTPUT ####
#-------------------------------------------------------------------------

m18_results_mo <- MplusAutomation::readModels(
  target = m18_output_file_mo,
  what = "all"
)

if (
  is.null(
    m18_results_mo$savedata
  )
) {
  stop(
    paste0(
      "No SAVEDATA results were found in ",
      basename(
        m18_output_file_mo
      ),
      "."
    )
  )
}

m18_savedata_mo <- tibble::as_tibble(
  m18_results_mo$savedata
)

names(
  m18_savedata_mo
) <- tolower(
  names(
    m18_savedata_mo
  )
)

required_m18_variables_mo <- c(
  "sic_n",
  "c",
  "cprob1",
  "cprob2",
  "cprob3"
)

missing_m18_variables_mo <- setdiff(
  required_m18_variables_mo,
  names(
    m18_savedata_mo
  )
)

if (
  length(
    missing_m18_variables_mo
  ) > 0L
) {
  stop(
    "Missing M18 SAVEDATA variables: ",
    paste(
      missing_m18_variables_mo,
      collapse = ", "
    )
  )
}

m18_assignments_mo <- m18_savedata_mo |>
  dplyr::transmute(
    SIC_N = as.integer(
      sic_n
    ),
    mt_class_original_mo = as.integer(
      c
    ),
    mt_prob_original1_mo = as.numeric(
      cprob1
    ),
    mt_prob_original2_mo = as.numeric(
      cprob2
    ),
    mt_prob_original3_mo = as.numeric(
      cprob3
    ),
    mt_prob_original_max_mo = pmax(
      cprob1,
      cprob2,
      cprob3
    )
  )

posterior_matrix_m18_mo <- m18_assignments_mo |>
  dplyr::select(
    mt_prob_original1_mo,
    mt_prob_original2_mo,
    mt_prob_original3_mo
  ) |>
  as.matrix()

stopifnot(
  nrow(
    m18_assignments_mo
  ) == 303L,
  !anyNA(
    m18_assignments_mo$SIC_N
  ),
  anyDuplicated(
    m18_assignments_mo$SIC_N
  ) == 0L,
  all(
    m18_assignments_mo$mt_class_original_mo %in% 1:3
  ),
  all(
    abs(
      rowSums(
        posterior_matrix_m18_mo
      ) - 1
    ) < 0.01
  ),
  all(
    m18_assignments_mo$mt_class_original_mo ==
      max.col(
        posterior_matrix_m18_mo,
        ties.method = "first"
      )
  )
)


#-------------------------------------------------------------------------
##### CREATE FINAL FOUR-GROUP CLASSIFICATION ####
#-------------------------------------------------------------------------

combined_classes_mo <- original_mplus_data_mo |>
  dplyr::select(
    SIC_N,
    mal_all
  ) |>
  dplyr::left_join(
    m18_assignments_mo,
    by = "SIC_N"
  ) |>
  dplyr::mutate(
    mt_class_mo = dplyr::case_when(
      mal_all == 0 ~ 1L,
      mal_all == 1 ~ mt_class_original_mo + 1L,
      TRUE ~ NA_integer_
    ),
    mt_prob_class1_mo = dplyr::case_when(
      mal_all == 0 ~ 1,
      mal_all == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    mt_prob_class2_mo = dplyr::case_when(
      mal_all == 0 ~ 0,
      mal_all == 1 ~ mt_prob_original1_mo,
      TRUE ~ NA_real_
    ),
    mt_prob_class3_mo = dplyr::case_when(
      mal_all == 0 ~ 0,
      mal_all == 1 ~ mt_prob_original2_mo,
      TRUE ~ NA_real_
    ),
    mt_prob_class4_mo = dplyr::case_when(
      mal_all == 0 ~ 0,
      mal_all == 1 ~ mt_prob_original3_mo,
      TRUE ~ NA_real_
    ),
    mt_prob_max_mo = dplyr::case_when(
      mal_all == 0 ~ 1,
      mal_all == 1 ~ mt_prob_original_max_mo,
      TRUE ~ NA_real_
    ),
    mt_class_label_mo = dplyr::case_when(
      mt_class_mo == 1L ~ class_labels_mo[1],
      mt_class_mo == 2L ~ class_labels_mo[2],
      mt_class_mo == 3L ~ class_labels_mo[3],
      mt_class_mo == 4L ~ class_labels_mo[4],
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(
    SIC_N,
    dplyr::all_of(
      class_excel_variables_mo
    )
  )

observed_class_sizes_mo <- combined_classes_mo |>
  dplyr::filter(
    !is.na(
      mt_class_mo
    )
  ) |>
  dplyr::count(
    mt_class_mo,
    name = "n"
  ) |>
  dplyr::arrange(
    mt_class_mo
  ) |>
  dplyr::pull(
    n,
    name = mt_class_mo
  )

stopifnot(
  identical(
    observed_class_sizes_mo,
    expected_class_sizes_mo
  )
)

observed_m18_avepp_mo <- m18_assignments_mo |>
  dplyr::mutate(
    mt_class_mo = mt_class_original_mo + 1L,
    assigned_probability_mo = dplyr::case_when(
      mt_class_original_mo == 1L ~ mt_prob_original1_mo,
      mt_class_original_mo == 2L ~ mt_prob_original2_mo,
      mt_class_original_mo == 3L ~ mt_prob_original3_mo,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::group_by(
    mt_class_mo
  ) |>
  dplyr::summarise(
    avepp = mean(
      assigned_probability_mo
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    mt_class_mo
  ) |>
  dplyr::pull(
    avepp,
    name = mt_class_mo
  )

stopifnot(
  all(
    abs(
      observed_m18_avepp_mo -
        expected_m18_avepp_mo
    ) < 0.002
  )
)


#-------------------------------------------------------------------------
##### READ AND PREPARE SES ####
#-------------------------------------------------------------------------

read_ses_source_mo <- function(source_name) {
  
  source_file <- ses_source_files_mo[[source_name]]
  id_variable <- ses_id_variables_mo[[source_name]]
  value_variable <- ses_value_variables_mo[[source_name]]
  
  source_data <- readxl::read_excel(
    source_file,
    na = c(
      "",
      "NA"
    )
  )
  
  missing_variables <- setdiff(
    c(
      id_variable,
      value_variable
    ),
    names(
      source_data
    )
  )
  
  if (
    length(
      missing_variables
    ) > 0L
  ) {
    stop(
      paste0(
        "Missing variables in ",
        basename(
          source_file
        ),
        ": ",
        paste(
          missing_variables,
          collapse = ", "
        )
      )
    )
  }
  
  raw_values <- as.character(
    source_data[[
      value_variable
    ]]
  )
  
  numeric_values <- suppressWarnings(
    as.numeric(
      raw_values
    )
  )
  
  invalid_values <- unique(
    raw_values[
      !is.na(
        raw_values
      ) &
        nzchar(
          trimws(
            raw_values
          )
        ) &
        is.na(
          numeric_values
        )
    ]
  )
  
  if (
    length(
      invalid_values
    ) > 0L
  ) {
    stop(
      paste0(
        "SES contains non-numeric values in ",
        basename(
          source_file
        ),
        ": ",
        paste(
          invalid_values,
          collapse = ", "
        )
      )
    )
  }
  
  prepared_data <- tibble::tibble(
    sic = standardize_sic_mo(
      source_data[[
        id_variable
      ]]
    ),
    
    ses_value = numeric_values
  ) |>
    dplyr::filter(
      !is.na(
        .data$sic
      )
    )
  
  stopifnot(
    anyDuplicated(
      prepared_data$sic
    ) == 0L,
    
    all(
      stats::na.omit(
        prepared_data$ses_value
      ) %in% 0:4
    )
  )
  
  names(
    prepared_data
  )[2L] <- paste0(
    ".ses_",
    source_name
  )
  
  prepared_data
}


##### READ ALL SES SOURCES #####

ses_source_data_list_mo <- lapply(
  names(
    ses_source_files_mo
  ),
  read_ses_source_mo
)

names(
  ses_source_data_list_mo
) <- names(
  ses_source_files_mo
)


##### MERGE SES SOURCES #####

ses_wide_data_mo <- Reduce(
  function(x, y) {
    dplyr::full_join(
      x,
      y,
      by = "sic"
    )
  },
  ses_source_data_list_mo
)


##### CREATE COMBINED SES VARIABLE #####

ses_merge_data_mo <- ses_wide_data_mo |>
  dplyr::transmute(
    sic,
    
    !!ses_final_variable_mo := dplyr::coalesce(
      .data$.ses_T2,
      .data$.ses_DFG02,
      .data$.ses_JA,
      .data$.ses_T5
    ),
    
    !!ses_source_indicator_mo := dplyr::case_when(
      !is.na(
        .data$.ses_T2
      ) ~ "T2",
      
      !is.na(
        .data$.ses_DFG02
      ) ~ "DFG02",
      
      !is.na(
        .data$.ses_JA
      ) ~ "JA",
      
      !is.na(
        .data$.ses_T5
      ) ~ "T5",
      
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(
    !is.na(
      .data$sic
    )
  )


##### CHECK ID MATCHES #####

n_ses_id_matches_by_source_mo <- vapply(
  ses_source_data_list_mo,
  
  function(source_data) {
    sum(
      sic_lookup_mo$sic %in%
        source_data$sic[
          !is.na(
            source_data[[2L]]
          )
        ]
    )
  },
  
  integer(1)
)

n_ses_id_matches_mo <- sum(
  sic_lookup_mo$sic %in%
    ses_merge_data_mo$sic[
      !is.na(
        ses_merge_data_mo[[
          ses_final_variable_mo
        ]]
      )
    ]
)

if (
  n_ses_id_matches_mo == 0L
) {
  stop(
    paste0(
      "All four SES merges produced zero matching IDs with ",
      "non-missing SES values. Check the SES ID variables ",
      "against sic before creating the final datasets."
    )
  )
}


##### VALIDATE COMBINED SES DATA #####

stopifnot(
  anyDuplicated(
    ses_merge_data_mo$sic
  ) == 0L,
  
  all(
    stats::na.omit(
      ses_merge_data_mo[[
        ses_final_variable_mo
      ]]
    ) %in% 0:4
  ),
  
  all(
    stats::na.omit(
      ses_merge_data_mo[[
        ses_source_indicator_mo
      ]]
    ) %in% ses_source_priority_mo
  )
)


##### REPORT SES COMPLETION #####

ses_source_counts_mo <- ses_merge_data_mo |>
  dplyr::count(
    .data[[
      ses_source_indicator_mo
    ]],
    name = "N",
    .drop = FALSE
  )

print(
  ses_source_counts_mo,
  n = Inf
)

message(
  "Number of IDs with a usable combined SES value: ",
  n_ses_id_matches_mo
)

#-------------------------------------------------------------------------
##### MERGE CLASSES AND SES WITH ORIGINAL EXCEL DATASET ####
#-------------------------------------------------------------------------

class_merge_data_mo <- combined_classes_mo |>
  dplyr::left_join(
    sic_lookup_mo,
    by = "SIC_N"
  ) |>
  dplyr::select(
    sic,
    dplyr::all_of(
      class_excel_variables_mo
    )
  )

stopifnot(
  !anyNA(
    class_merge_data_mo$sic
  ),
  anyDuplicated(
    class_merge_data_mo$sic
  ) == 0L
)

final_excel_data_mo <- original_excel_data_mo |>
  dplyr::mutate(
    .original_order_mo = dplyr::row_number(),
    .merge_sic_mo = standardize_sic_mo(
      sic
    )
  ) |>
  dplyr::select(
    -dplyr::any_of(
      c(
        class_excel_variables_mo,
        ses_excel_variables_mo
      )
    )
  ) |>
  dplyr::left_join(
    class_merge_data_mo |>
      dplyr::rename(
        .merge_sic_mo = sic
      ),
    by = ".merge_sic_mo"
  ) |>
  dplyr::left_join(
    ses_merge_data_mo |>
      dplyr::rename(
        .merge_sic_mo = sic
      ),
    by = ".merge_sic_mo"
  ) |>
  dplyr::arrange(
    .original_order_mo
  ) |>
  dplyr::select(
    -.original_order_mo,
    -.merge_sic_mo
  )

stopifnot(
  nrow(
    final_excel_data_mo
  ) == nrow(
    original_excel_data_mo
  ),
  identical(
    final_excel_data_mo$sic,
    original_excel_data_mo$sic
  ),
  identical(
    names(
      final_excel_data_mo
    )[seq_along(
      names(
        original_excel_data_mo
      )
    )],
    names(
      original_excel_data_mo
    )
  ),
  identical(
    tail(
      names(
        final_excel_data_mo
      ),
      length(
        class_excel_variables_mo
      ) +
        length(
          ses_excel_variables_mo
        )
    ),
    c(
      class_excel_variables_mo,
      ses_excel_variables_mo
    )
  ),
  anyDuplicated(
    names(
      final_excel_data_mo
    )
  ) == 0L
)

writexl::write_xlsx(
  x = final_excel_data_mo,
  path = final_excel_file_mo
)

stopifnot(
  file.exists(
    final_excel_file_mo
  )
)


#-------------------------------------------------------------------------
##### APPEND NEW VARIABLES TO ORIGINAL MPLUS DATASET ####
#-------------------------------------------------------------------------

ses_mplus_merge_mo <- ses_merge_data_mo |>
  dplyr::left_join(
    sic_lookup_mo,
    by = "sic"
  ) |>
  dplyr::select(
    SIC_N,
    dplyr::all_of(
      ses_final_variable_mo
    )
  )

new_mplus_variables_mo <- combined_classes_mo |>
  dplyr::select(
    SIC_N,
    mt_class_mo,
    mt_prob_class1_mo,
    mt_prob_class2_mo,
    mt_prob_class3_mo,
    mt_prob_class4_mo,
    mt_prob_max_mo
  ) |>
  dplyr::left_join(
    ses_mplus_merge_mo,
    by = "SIC_N"
  )

names(
  new_mplus_variables_mo
)[
  match(
    new_mplus_mapping_mo$original_name,
    names(
      new_mplus_variables_mo
    )
  )
] <- new_mplus_mapping_mo$mplus_name

final_mplus_data_mo <- original_mplus_data_mo |>
  dplyr::select(
    -dplyr::any_of(
      new_mplus_mapping_mo$mplus_name
    )
  ) |>
  dplyr::left_join(
    new_mplus_variables_mo,
    by = "SIC_N"
  )

final_mplus_names_mo <- c(
  original_mplus_names_mo,
  new_mplus_mapping_mo$mplus_name
)

changed_original_mplus_variables_mo <-
  original_mplus_names_mo[
    !vapply(
      original_mplus_names_mo,
      function(variable) {
        identical(
          final_mplus_data_mo[[
            variable
          ]],
          original_mplus_data_mo[[
            variable
          ]]
        )
      },
      logical(1)
    )
  ]

if (
  length(
    changed_original_mplus_variables_mo
  ) > 0L
) {
  stop(
    paste0(
      "The following original Mplus variables changed during the merge:\n",
      paste0(
        "- ",
        changed_original_mplus_variables_mo,
        collapse = "\n"
      )
    )
  )
}

stopifnot(
  nrow(
    final_mplus_data_mo
  ) == nrow(
    original_mplus_data_mo
  ),
  identical(
    final_mplus_data_mo$SIC_N,
    original_mplus_data_mo$SIC_N
  ),
  identical(
    names(
      final_mplus_data_mo
    ),
    final_mplus_names_mo
  ),
  all(
    vapply(
      final_mplus_data_mo,
      is.numeric,
      logical(1)
    )
  ),
  all(
    nchar(
      names(
        final_mplus_data_mo
      )
    ) <= 8L
  ),
  anyDuplicated(
    tolower(
      names(
        final_mplus_data_mo
      )
    )
  ) == 0L
)

final_mplus_export_mo <- final_mplus_data_mo |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ replace(
        .x,
        is.na(
          .x
        ),
        -999
      )
    )
  )

write.table(
  final_mplus_export_mo,
  file = final_mplus_data_file_mo,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  dec = "."
)

saveRDS(
  final_mplus_names_mo,
  final_mplus_names_file_mo
)

writeLines(
  final_mplus_names_mo,
  final_mplus_names_text_file_mo
)

saveRDS(
  final_mplus_data_mo,
  final_mplus_rds_file_mo
)


#-------------------------------------------------------------------------
##### COPY FINAL MPLUS FILES TO LOCAL MPLUS DIRECTORY ####
#-------------------------------------------------------------------------

copy_results_mo <- c(
  file.copy(
    final_mplus_data_file_mo,
    local_final_mplus_data_file_mo,
    overwrite = TRUE
  ),
  file.copy(
    final_mplus_names_file_mo,
    local_final_mplus_names_file_mo,
    overwrite = TRUE
  ),
  file.copy(
    final_mplus_names_text_file_mo,
    local_final_mplus_names_text_file_mo,
    overwrite = TRUE
  ),
  file.copy(
    final_mplus_rds_file_mo,
    local_final_mplus_rds_file_mo,
    overwrite = TRUE
  )
)

stopifnot(
  all(
    copy_results_mo
  )
)


#-------------------------------------------------------------------------
##### CREATE FINAL TWO-VERSION VARIABLE DICTIONARY ####
#-------------------------------------------------------------------------

original_dictionary_mo <- readr::read_csv(
  original_dictionary_file_mo,
  show_col_types = FALSE
)

required_dictionary_columns_mo <- c(
  "original_name",
  "mplus_name",
  "retained",
  "final_position"
)

stopifnot(
  all(
    required_dictionary_columns_mo %in%
      names(
        original_dictionary_mo
      )
  )
)

observed_original_mapping_mo <- original_dictionary_mo |>
  dplyr::filter(
    retained %in% TRUE
  ) |>
  dplyr::arrange(
    final_position
  ) |>
  dplyr::pull(
    mplus_name
  )

stopifnot(
  identical(
    tolower(
      observed_original_mapping_mo
    ),
    tolower(
      original_mplus_names_mo
    )
  )
)

original_excel_names_mo <- names(
  original_excel_data_mo
)

final_excel_names_mo <- names(
  final_excel_data_mo
)

all_excel_names_mo <- unique(
  c(
    original_excel_names_mo,
    final_excel_names_mo
  )
)

dictionary_mapping_mo <- tibble::tibble(
  variable = all_excel_names_mo
) |>
  dplyr::left_join(
    original_dictionary_mo |>
      dplyr::select(
        original_name,
        mplus_name,
        retained
      ) |>
      dplyr::rename(
        variable = original_name,
        mplus_name_original = mplus_name,
        retained_original = retained
      ),
    by = "variable"
  ) |>
  dplyr::left_join(
    new_mplus_mapping_mo |>
      dplyr::rename(
        variable = original_name,
        mplus_name_new = mplus_name
      ),
    by = "variable"
  ) |>
  dplyr::mutate(
    excel_name_original = dplyr::if_else(
      variable %in% original_excel_names_mo,
      variable,
      NA_character_
    ),
    excel_position_original = match(
      variable,
      original_excel_names_mo
    ),
    mplus_position_original = match(
      tolower(
        mplus_name_original
      ),
      tolower(
        original_mplus_names_mo
      )
    ),
    mplus_name_original = original_mplus_names_mo[
      mplus_position_original
    ],
    excel_name_final = dplyr::if_else(
      variable %in% final_excel_names_mo,
      variable,
      NA_character_
    ),
    excel_position_final = match(
      variable,
      final_excel_names_mo
    ),
    mplus_name_final = dplyr::coalesce(
      mplus_name_new,
      mplus_name_original
    ),
    mplus_name_final = dplyr::if_else(
      mplus_name_final %in% final_mplus_names_mo,
      mplus_name_final,
      NA_character_
    ),
    mplus_position_final = match(
      mplus_name_final,
      final_mplus_names_mo
    ),
    added_in_final = !variable %in% original_excel_names_mo
  ) |>
  dplyr::select(
    variable,
    added_in_final,
    excel_name_original,
    excel_position_original,
    mplus_name_original,
    mplus_position_original,
    excel_name_final,
    excel_position_final,
    mplus_name_final,
    mplus_position_final
  ) |>
  dplyr::arrange(
    excel_position_final
  )

stopifnot(
  anyDuplicated(
    dictionary_mapping_mo$variable
  ) == 0L,
  sum(
    !is.na(
      dictionary_mapping_mo$excel_name_original
    )
  ) == length(
    original_excel_names_mo
  ),
  sum(
    !is.na(
      dictionary_mapping_mo$excel_name_final
    )
  ) == length(
    final_excel_names_mo
  ),
  sum(
    !is.na(
      dictionary_mapping_mo$mplus_name_original
    )
  ) == length(
    original_mplus_names_mo
  ),
  sum(
    !is.na(
      dictionary_mapping_mo$mplus_name_final
    )
  ) == length(
    final_mplus_names_mo
  )
)

dataset_information_mo <- tibble::tibble(
  version = c(
    "Original",
    "Final: M18 classes + SES"
  ),
  excel_file = c(
    basename(
      original_excel_file_mo
    ),
    basename(
      final_excel_file_mo
    )
  ),
  mplus_dat_file = c(
    basename(
      original_mplus_data_file_mo
    ),
    basename(
      final_mplus_data_file_mo
    )
  ),
  mplus_names_file = c(
    basename(
      original_mplus_names_file_mo
    ),
    basename(
      final_mplus_names_file_mo
    )
  ),
  n_excel_cases = c(
    nrow(
      original_excel_data_mo
    ),
    nrow(
      final_excel_data_mo
    )
  ),
  n_excel_variables = c(
    ncol(
      original_excel_data_mo
    ),
    ncol(
      final_excel_data_mo
    )
  ),
  n_mplus_cases = c(
    nrow(
      original_mplus_data_mo
    ),
    nrow(
      final_mplus_data_mo
    )
  ),
  n_mplus_variables = c(
    ncol(
      original_mplus_data_mo
    ),
    ncol(
      final_mplus_data_mo
    )
  )
)

ses_codebook_mo <- tibble::tribble(
  ~code, ~english_label,
  0L, "No school-leaving qualification",
  1L, "Special school-leaving certificate",
  2L, "Lower secondary school-leaving certificate (Hauptschulabschluss)",
  3L, "Intermediate secondary school-leaving certificate (Realschulabschluss)",
  4L, "University entrance qualification (Abitur/Fachhochschulreife)"
)

class_codebook_mo <- tibble::tibble(
  code = class_levels_mo,
  english_label = class_labels_mo,
  expected_n = as.integer(
    expected_class_sizes_mo
  )
)

readr::write_csv(
  dictionary_mapping_mo,
  final_dictionary_csv_file_mo,
  na = ""
)

dictionary_workbook_mo <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  dictionary_workbook_mo,
  "Variable Mapping"
)

openxlsx::writeData(
  dictionary_workbook_mo,
  "Variable Mapping",
  dictionary_mapping_mo,
  withFilter = TRUE
)

openxlsx::freezePane(
  dictionary_workbook_mo,
  "Variable Mapping",
  firstRow = TRUE,
  firstCol = TRUE
)

openxlsx::setColWidths(
  dictionary_workbook_mo,
  "Variable Mapping",
  cols = seq_len(
    ncol(
      dictionary_mapping_mo
    )
  ),
  widths = "auto"
)

openxlsx::addWorksheet(
  dictionary_workbook_mo,
  "Dataset Information"
)

openxlsx::writeData(
  dictionary_workbook_mo,
  "Dataset Information",
  dataset_information_mo
)

openxlsx::setColWidths(
  dictionary_workbook_mo,
  "Dataset Information",
  cols = seq_len(
    ncol(
      dataset_information_mo
    )
  ),
  widths = "auto"
)

openxlsx::addWorksheet(
  dictionary_workbook_mo,
  "SES Coding"
)

openxlsx::writeData(
  dictionary_workbook_mo,
  "SES Coding",
  ses_codebook_mo
)

openxlsx::setColWidths(
  dictionary_workbook_mo,
  "SES Coding",
  cols = seq_len(
    ncol(
      ses_codebook_mo
    )
  ),
  widths = "auto"
)

openxlsx::addWorksheet(
  dictionary_workbook_mo,
  "Class Coding"
)

openxlsx::writeData(
  dictionary_workbook_mo,
  "Class Coding",
  class_codebook_mo
)

openxlsx::setColWidths(
  dictionary_workbook_mo,
  "Class Coding",
  cols = seq_len(
    ncol(
      class_codebook_mo
    )
  ),
  widths = "auto"
)

openxlsx::saveWorkbook(
  dictionary_workbook_mo,
  final_dictionary_xlsx_file_mo,
  overwrite = TRUE
)


#-------------------------------------------------------------------------
##### FINAL AUDIT AND SUMMARY ####
#-------------------------------------------------------------------------

stopifnot(
  file.exists(
    final_excel_file_mo
  ),
  file.exists(
    final_mplus_data_file_mo
  ),
  file.exists(
    final_mplus_names_file_mo
  ),
  file.exists(
    final_mplus_names_text_file_mo
  ),
  file.exists(
    final_mplus_rds_file_mo
  ),
  file.exists(
    final_dictionary_csv_file_mo
  ),
  file.exists(
    final_dictionary_xlsx_file_mo
  ),
  identical(
    readRDS(
      final_mplus_names_file_mo
    ),
    names(
      readRDS(
        final_mplus_rds_file_mo
      )
    )
  )
)

cat(
  "\n============================================================",
  "\nFINAL M18 + SES DATA PREPARATION COMPLETED",
  "\n============================================================",
  "\n\nOriginal Excel dataset retained unchanged:",
  "\n", original_excel_file_mo,
  "\n\nFinal Excel dataset:",
  "\n", final_excel_file_mo,
  "\n\nFinal Mplus dataset:",
  "\n", final_mplus_data_file_mo,
  "\n\nFinal variable dictionary:",
  "\n", final_dictionary_xlsx_file_mo,
  "\n\nSES IDs matched: ",
  n_ses_id_matches_mo,
  "\nSES IDs matched by source:",
  "\n  T2: ", n_ses_id_matches_by_source_mo[["T2"]],
  "\n  DFG02: ", n_ses_id_matches_by_source_mo[["DFG02"]],
  "\n  JA: ", n_ses_id_matches_by_source_mo[["JA"]],
  "\n  T5: ", n_ses_id_matches_by_source_mo[["T5"]],
  "\n\nClass distribution:\n",
  sep = ""
)

print(
  observed_class_sizes_mo
)

cat(
  "\nSES distribution:\n"
)

print(
  table(
    final_excel_data_mo[[
      ses_final_variable_mo
    ]],
    useNA = "ifany"
  )
)

cat(
  "\nSES values actually selected by source:\n"
)

print(
  table(
    final_excel_data_mo[[
      ses_source_indicator_mo
    ]],
    useNA = "ifany"
  )
)