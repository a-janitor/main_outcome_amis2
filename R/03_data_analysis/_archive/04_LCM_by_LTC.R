source(
  "C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup.R"
)

#-------------------------------------------------------------------------
##### FILE PATHS #####
#-------------------------------------------------------------------------

final_mplus_data_name <- "AMIS_mplus_dataset_m18_ses.dat"

seadrive_final_mplus_data_file <- file.path(
  seadrive_mplus_data_dir,
  final_mplus_data_name
)

seadrive_final_mplus_names_file <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names_m18_ses.rds"
)

seadrive_final_mplus_names_text_file <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_names_m18_ses.txt"
)

seadrive_final_mplus_rds_file <- file.path(
  seadrive_mplus_data_dir,
  "AMIS_mplus_dataset_m18_ses.rds"
)

local_final_mplus_data_file <- file.path(
  mplus_input_dir,
  final_mplus_data_name
)

local_final_mplus_names_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18_ses.rds"
)

local_final_mplus_names_text_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names_m18_ses.txt"
)

local_final_mplus_rds_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset_m18_ses.rds"
)

ses_source_file <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/02_data/00_raw_final/AMIS 2/data/",
  "PV0880_T01368_NODUP.xlsx"
)

m18_input_file <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes.xlsx"
)

m18_ses_output_file <- file.path(
  data_prep_dir,
  "AMIS_merged_analysis_dataset_with_M18_classes_SES.xlsx"
)

# Bestehender Excel-Basisdatensatz
if (!exists("master_excel_file")) {
  master_excel_file <- file.path(
    data_prep_dir,
    "AMIS_merged_analysis_dataset.xlsx"
  )
}

# Bestehender Excel-M18-Datensatz
if (!exists("m18_excel_file")) {
  m18_excel_file <- m18_input_file
}

# Ursprüngliches Dictionary
if (!exists("mplus_variable_dictionary_file")) {
  mplus_variable_dictionary_file <- file.path(
    github_root,
    "mplus_variable_dictionary.csv"
  )
}

# M18-Dictionary
if (!exists("m18_dictionary_file")) {
  m18_dictionary_file <- file.path(
    github_root,
    "mplus_variable_dictionary_m18.csv"
  )
}

# Gemeinsames Master-Dictionary
if (!exists("master_dictionary_file")) {
  master_dictionary_file <- file.path(
    github_root,
    "mplus_variable_dictionary_master.csv"
  )
}

ses_variable <- toupper(
  "ses_b_t5_ausb1_m"
)


#-------------------------------------------------------------------------
##### CHECK FILES AND DIRECTORIES #####
#-------------------------------------------------------------------------

stopifnot(
  file.exists(ses_source_file),
  file.exists(m18_input_file),
  file.exists(master_excel_file),
  file.exists(m18_excel_file),
  file.exists(mplus_variable_dictionary_file),
  file.exists(m18_dictionary_file)
)

dir.create(
  dirname(m18_ses_output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  dirname(master_dictionary_file),
  recursive = TRUE,
  showWarnings = FALSE
)


#-------------------------------------------------------------------------
##### LOAD DATA #####
#-------------------------------------------------------------------------

m18_data <- readxl::read_excel(
  m18_input_file
)

ses_data <- readxl::read_excel(
  ses_source_file
)


#-------------------------------------------------------------------------
##### CHECK VARIABLES #####
#-------------------------------------------------------------------------

stopifnot(
  "sic" %in% names(m18_data),
  "SIC" %in% names(ses_data),
  ses_variable %in% names(ses_data)
)

# Schlüsselvariablen vereinheitlichen
m18_data <- m18_data |>
  dplyr::mutate(
    sic = trimws(as.character(sic))
  )

ses_data <- ses_data |>
  dplyr::mutate(
    SIC = trimws(as.character(SIC))
  )

# Leere Schlüssel als fehlend behandeln
m18_data <- m18_data |>
  dplyr::mutate(
    sic = dplyr::na_if(sic, "")
  )

ses_data <- ses_data |>
  dplyr::mutate(
    SIC = dplyr::na_if(SIC, "")
  )

# Doppelte gültige Schlüssel prüfen
m18_duplicate_keys <- m18_data |>
  dplyr::filter(
    !is.na(sic)
  ) |>
  dplyr::count(
    sic,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  )

ses_duplicate_keys <- ses_data |>
  dplyr::filter(
    !is.na(SIC)
  ) |>
  dplyr::count(
    SIC,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  )

stopifnot(
  nrow(m18_duplicate_keys) == 0,
  nrow(ses_duplicate_keys) == 0
)


#-------------------------------------------------------------------------
##### PREPARE SES VARIABLE #####
#-------------------------------------------------------------------------

ses_merge_data <- ses_data |>
  dplyr::transmute(
    sic = SIC,
    SES_B_T5_AUSB1_M = suppressWarnings(
      as.numeric(
        as.character(.data[[ses_variable]])
      )
    )
  )

m18_data <- m18_data |>
  dplyr::mutate(
    original_order = dplyr::row_number()
  )


#-------------------------------------------------------------------------
##### MERGE SES VARIABLE #####
#-------------------------------------------------------------------------

m18_ses_data <- m18_data |>
  dplyr::select(
    -dplyr::any_of(ses_variable)
  ) |>
  dplyr::left_join(
    ses_merge_data,
    by = "sic"
  ) |>
  dplyr::arrange(
    original_order
  ) |>
  dplyr::select(
    -original_order
  )


#-------------------------------------------------------------------------
##### CHECK MERGE #####
#-------------------------------------------------------------------------

stopifnot(
  nrow(m18_ses_data) == nrow(m18_data),
  identical(
    m18_ses_data$sic,
    m18_data$sic
  ),
  ses_variable %in% names(m18_ses_data)
)

n_matches <- sum(
  m18_data$sic %in% ses_merge_data$sic,
  na.rm = TRUE
)

n_nonmissing_ses <- sum(
  !is.na(m18_ses_data[[ses_variable]])
)

n_missing_ses <- sum(
  is.na(m18_ses_data[[ses_variable]])
)

cat(
  "\nAnzahl Fälle im M18-Datensatz: ",
  nrow(m18_ses_data),
  "\nÜber SIC im SES-Datensatz gefunden: ",
  n_matches,
  "\nSES-Werte vorhanden: ",
  n_nonmissing_ses,
  "\nSES-Werte fehlend: ",
  n_missing_ses,
  "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### SAVE NEW DATASET #####
#-------------------------------------------------------------------------

writexl::write_xlsx(
  m18_ses_data,
  m18_ses_output_file
)

stopifnot(
  file.exists(m18_ses_output_file)
)

cat(
  "\nNeuer Datensatz gespeichert unter:\n",
  m18_ses_output_file,
  "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### CREATE MASTER DICTIONARY #####
#-------------------------------------------------------------------------

base_dictionary <- readr::read_csv(
  mplus_variable_dictionary_file,
  show_col_types = FALSE
)

m18_dictionary <- readr::read_csv(
  m18_dictionary_file,
  show_col_types = FALSE
)

required_dictionary_columns <- c(
  "original_name",
  "mplus_name",
  "retained",
  "final_position"
)

stopifnot(
  all(
    required_dictionary_columns %in%
      names(base_dictionary)
  ),
  all(
    required_dictionary_columns %in%
      names(m18_dictionary)
  )
)


#-------------------------------------------------------------------------
##### READ DATASET VARIABLE NAMES #####
#-------------------------------------------------------------------------

base_excel_names <- names(
  readxl::read_excel(
    master_excel_file,
    n_max = 0
  )
)

m18_excel_names <- names(
  readxl::read_excel(
    m18_excel_file,
    n_max = 0
  )
)

m18_ses_excel_names <- names(
  readxl::read_excel(
    m18_ses_output_file,
    n_max = 0
  )
)


#-------------------------------------------------------------------------
##### BASE DATASET DICTIONARY #####
#-------------------------------------------------------------------------

dictionary_base <- base_dictionary |>
  dplyr::mutate(
    dataset_version = "base",
    
    dataset_excel =
      basename(master_excel_file),
    
    dataset_mplus =
      "AMIS_mplus_dataset.dat",
    
    position_excel =
      match(
        original_name,
        base_excel_names
      ),
    
    position_mplus =
      dplyr::if_else(
        retained %in% TRUE,
        as.integer(final_position),
        NA_integer_
      )
  )


#-------------------------------------------------------------------------
##### M18 DATASET DICTIONARY #####
#-------------------------------------------------------------------------

dictionary_m18 <- m18_dictionary |>
  dplyr::mutate(
    dataset_version = "m18",
    
    dataset_excel =
      basename(m18_excel_file),
    
    dataset_mplus =
      "AMIS_mplus_dataset_m18.dat",
    
    position_excel =
      match(
        original_name,
        m18_excel_names
      ),
    
    position_mplus =
      dplyr::if_else(
        retained %in% TRUE,
        as.integer(final_position),
        NA_integer_
      )
  )


#-------------------------------------------------------------------------
##### M18 + SES DICTIONARY #####
#-------------------------------------------------------------------------

ses_variable_original <- ses_variable
ses_variable_mplus <- "sesausb"

stopifnot(
  nchar(ses_variable_mplus) <= 8,
  ses_variable_original %in% m18_ses_excel_names
)

# Falls die SES-Variable schon im M18-Dictionary enthalten sein sollte,
# wird sie vor dem erneuten Hinzufügen entfernt.
m18_ses_dictionary <- m18_dictionary |>
  dplyr::filter(
    original_name != ses_variable_original
  )

# Höchste bislang verwendete Mplus-Position
last_mplus_position <- m18_ses_dictionary |>
  dplyr::filter(
    retained %in% TRUE,
    !is.na(final_position)
  ) |>
  dplyr::summarise(
    last_position = max(
      final_position,
      na.rm = TRUE
    )
  ) |>
  dplyr::pull(
    last_position
  )

stopifnot(
  length(last_mplus_position) == 1,
  is.finite(last_mplus_position)
)

# Neue Dictionary-Zeile mit derselben Spaltenstruktur erzeugen
new_ses_row <- m18_ses_dictionary[1, , drop = FALSE]

new_ses_row[,] <- NA

new_ses_row$original_name <- ses_variable_original
new_ses_row$mplus_name <- ses_variable_mplus
new_ses_row$retained <- TRUE
new_ses_row$final_position <- last_mplus_position + 1

m18_ses_dictionary <- m18_ses_dictionary |>
  dplyr::bind_rows(
    new_ses_row
  ) |>
  dplyr::arrange(
    final_position
  )

dictionary_m18_ses <- m18_ses_dictionary |>
  dplyr::mutate(
    dataset_version = "m18_ses",
    
    dataset_excel =
      basename(m18_ses_output_file),
    
    dataset_mplus =
      "AMIS_mplus_dataset_m18_ses.dat",
    
    position_excel =
      match(
        original_name,
        m18_ses_excel_names
      ),
    
    position_mplus =
      dplyr::if_else(
        retained %in% TRUE,
        as.integer(final_position),
        NA_integer_
      )
  )


#-------------------------------------------------------------------------
##### CREATE WIDE MASTER DICTIONARY #####
#-------------------------------------------------------------------------

# Zunächst nur die für das Mapping relevanten Spalten behalten
dictionary_base_wide <- dictionary_base |>
  dplyr::transmute(
    original_name,
    base_mplus_name = mplus_name,
    base_excel_position = position_excel,
    base_mplus_position = position_mplus,
    base_retained = retained
  )

dictionary_m18_wide <- dictionary_m18 |>
  dplyr::transmute(
    original_name,
    m18_mplus_name = mplus_name,
    m18_excel_position = position_excel,
    m18_mplus_position = position_mplus,
    m18_retained = retained
  )

dictionary_m18_ses_wide <- dictionary_m18_ses |>
  dplyr::transmute(
    original_name,
    m18_ses_mplus_name = mplus_name,
    m18_ses_excel_position = position_excel,
    m18_ses_mplus_position = position_mplus,
    m18_ses_retained = retained
  )


#-------------------------------------------------------------------------
##### JOIN DATASET VERSIONS BY VARIABLE #####
#-------------------------------------------------------------------------

master_dictionary_wide <- dictionary_base_wide |>
  dplyr::full_join(
    dictionary_m18_wide,
    by = "original_name"
  ) |>
  dplyr::full_join(
    dictionary_m18_ses_wide,
    by = "original_name"
  )


#-------------------------------------------------------------------------
##### CREATE CONSOLIDATED MPLUS NAME #####
#-------------------------------------------------------------------------

master_dictionary_wide <- master_dictionary_wide |>
  dplyr::mutate(
    mplus_name = dplyr::coalesce(
      m18_ses_mplus_name,
      m18_mplus_name,
      base_mplus_name
    )
  ) |>
  dplyr::relocate(
    original_name,
    mplus_name
  )


#-------------------------------------------------------------------------
##### CHECK MPLUS NAME CONSISTENCY #####
#-------------------------------------------------------------------------

mplus_name_conflicts <- master_dictionary_wide |>
  dplyr::filter(
    (
      !is.na(base_mplus_name) &
        !is.na(m18_mplus_name) &
        base_mplus_name != m18_mplus_name
    ) |
      (
        !is.na(base_mplus_name) &
          !is.na(m18_ses_mplus_name) &
          base_mplus_name != m18_ses_mplus_name
      ) |
      (
        !is.na(m18_mplus_name) &
          !is.na(m18_ses_mplus_name) &
          m18_mplus_name != m18_ses_mplus_name
      )
  )

if (nrow(mplus_name_conflicts) > 0) {
  warning(
    "Für einige Variablen unterscheiden sich die Mplus-Namen ",
    "zwischen den Datensatzversionen."
  )
  
  print(
    mplus_name_conflicts
  )
}


#-------------------------------------------------------------------------
##### ADD PRESENCE INDICATORS #####
#-------------------------------------------------------------------------

master_dictionary_wide <- master_dictionary_wide |>
  dplyr::mutate(
    present_base =
      !is.na(base_excel_position),
    
    present_m18 =
      !is.na(m18_excel_position),
    
    present_m18_ses =
      !is.na(m18_ses_excel_position)
  )


#-------------------------------------------------------------------------
##### ORDER VARIABLES #####
#-------------------------------------------------------------------------

master_dictionary_wide <- master_dictionary_wide |>
  dplyr::arrange(
    dplyr::coalesce(
      m18_ses_excel_position,
      m18_excel_position,
      base_excel_position
    ),
    original_name
  ) |>
  dplyr::select(
    original_name,
    mplus_name,
    
    present_base,
    base_excel_position,
    base_mplus_position,
    base_retained,
    
    present_m18,
    m18_excel_position,
    m18_mplus_position,
    m18_retained,
    
    present_m18_ses,
    m18_ses_excel_position,
    m18_ses_mplus_position,
    m18_ses_retained,
    
    base_mplus_name,
    m18_mplus_name,
    m18_ses_mplus_name
  )


#-------------------------------------------------------------------------
##### CHECK SES VARIABLE #####
#-------------------------------------------------------------------------

ses_dictionary_row <- master_dictionary_wide |>
  dplyr::filter(
    original_name == ses_variable_original
  )

stopifnot(
  nrow(ses_dictionary_row) == 1,
  ses_dictionary_row$present_m18_ses,
  !ses_dictionary_row$present_base,
  !ses_dictionary_row$present_m18,
  !is.na(ses_dictionary_row$m18_ses_excel_position),
  !is.na(ses_dictionary_row$m18_ses_mplus_position),
  ses_dictionary_row$mplus_name == ses_variable_mplus
)


#-------------------------------------------------------------------------
##### OUTPUT FILE PATH #####
#-------------------------------------------------------------------------

master_dictionary_xlsx_file <- file.path(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/MAIN OUTCOME/02_data/02_data_Prep",
  "mplus_variable_dictionary_master.xlsx"
)


#-------------------------------------------------------------------------
##### SAVE FORMATTED EXCEL VERSION #####
#-------------------------------------------------------------------------

dictionary_workbook <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  dictionary_workbook,
  sheetName = "Variable Mapping"
)

openxlsx::writeData(
  dictionary_workbook,
  sheet = "Variable Mapping",
  x = master_dictionary_wide,
  withFilter = TRUE
)

openxlsx::freezePane(
  dictionary_workbook,
  sheet = "Variable Mapping",
  firstRow = TRUE,
  firstCol = TRUE
)

openxlsx::setColWidths(
  dictionary_workbook,
  sheet = "Variable Mapping",
  cols = seq_len(ncol(master_dictionary_wide)),
  widths = "auto"
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom"
)

openxlsx::addStyle(
  dictionary_workbook,
  sheet = "Variable Mapping",
  style = header_style,
  rows = 1,
  cols = seq_len(ncol(master_dictionary_wide)),
  gridExpand = TRUE
)

position_columns <- grep(
  "position$",
  names(master_dictionary_wide)
)

openxlsx::addStyle(
  dictionary_workbook,
  sheet = "Variable Mapping",
  style = openxlsx::createStyle(
    numFmt = "0"
  ),
  rows = 2:(nrow(master_dictionary_wide) + 1),
  cols = position_columns,
  gridExpand = TRUE
)

openxlsx::addWorksheet(
  dictionary_workbook,
  sheetName = "Dataset Information"
)

dataset_information <- tibble::tibble(
  dataset_version = c(
    "base",
    "m18",
    "m18_ses"
  ),
  excel_file = c(
    basename(master_excel_file),
    basename(m18_excel_file),
    basename(m18_ses_output_file)
  ),
  mplus_file = c(
    "AMIS_mplus_dataset.dat",
    "AMIS_mplus_dataset_m18.dat",
    "AMIS_mplus_dataset_m18_ses.dat"
  ),
  description = c(
    "Ursprünglicher Analyse-Datensatz",
    "Analyse-Datensatz mit M18-Klassenzuweisung",
    "Analyse-Datensatz mit M18-Klassenzuweisung und SES-Variable"
  )
)

openxlsx::writeData(
  dictionary_workbook,
  sheet = "Dataset Information",
  x = dataset_information,
  withFilter = TRUE
)

openxlsx::setColWidths(
  dictionary_workbook,
  sheet = "Dataset Information",
  cols = seq_len(ncol(dataset_information)),
  widths = "auto"
)

openxlsx::freezePane(
  dictionary_workbook,
  sheet = "Dataset Information",
  firstRow = TRUE
)

openxlsx::saveWorkbook(
  dictionary_workbook,
  file = master_dictionary_xlsx_file,
  overwrite = TRUE
)


#-------------------------------------------------------------------------
##### FINAL CHECKS #####
#-------------------------------------------------------------------------

stopifnot(
  file.exists(master_dictionary_xlsx_file),
  anyDuplicated(master_dictionary_wide$original_name) == 0
)

cat(
  "\nMaster-Dictionary erstellt.",
  "\nAnzahl eindeutiger Variablen: ",
  nrow(master_dictionary_wide),
  "\n",
  "\nDirekt lesbare Excel-Datei:",
  "\n",
  master_dictionary_xlsx_file,
  "\n",
  sep = ""
)

View(
  master_dictionary_wide
)

#-------------------------------------------------------------------------
##### CREATE FINAL MPLUS DATASET FOR 04_LCM_by_LTC #####
#-------------------------------------------------------------------------

final_excel_data <- readxl::read_excel(
  m18_ses_output_file,
  na = c("", "NA")
)

stopifnot(
  nrow(final_excel_data) > 0,
  ncol(final_excel_data) > 0,
  "sic" %in% names(final_excel_data),
  "mt_status_t5" %in% names(final_excel_data),
  !anyNA(final_excel_data$sic),
  anyDuplicated(final_excel_data$sic) == 0
)


#-------------------------------------------------------------------------
##### RECREATE ESTABLISHED NUMERIC SIC_N MAPPING #####
#-------------------------------------------------------------------------

# Dieselbe Zuordnung wie im ursprünglichen Mplus-Datensatz:
# eindeutige SIC sortieren und fortlaufend nummerieren.

sic_n_lookup <- readxl::read_excel(
  master_excel_file,
  n_max = Inf
) |>
  dplyr::transmute(
    sic = trimws(
      as.character(sic)
    )
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(
    sic
  ) |>
  dplyr::mutate(
    SIC_N = dplyr::row_number()
  )

final_sic <- trimws(
  as.character(final_excel_data$sic)
)

sic_match <- match(
  final_sic,
  sic_n_lookup$sic
)

if (anyNA(sic_match)) {
  stop(
    "Folgende SIC-Werte fehlen im Basisdatensatz:\n",
    paste(
      final_sic[is.na(sic_match)],
      collapse = ", "
    )
  )
}

stopifnot(
  !anyNA(sic_n_lookup$sic),
  !anyNA(sic_n_lookup$SIC_N),
  anyDuplicated(sic_n_lookup$sic) == 0,
  anyDuplicated(sic_n_lookup$SIC_N) == 0
)


#-------------------------------------------------------------------------
##### PREPARE SPECIAL VARIABLES #####
#-------------------------------------------------------------------------

final_excel_data <- final_excel_data |>
  dplyr::mutate(
    # Überschreibt sic direkt – die Spaltenposition bleibt erhalten
    sic = sic_n_lookup$SIC_N[sic_match],
    
    status_t5_character =
      stringr::str_to_lower(
        stringr::str_squish(
          as.character(mt_status_t5)
        )
      ),
    
    mt_status_t5 =
      dplyr::case_when(
        status_t5_character == "drop out" ~ 0,
        status_t5_character == "completed" ~ 2,
        status_t5_character == "0" ~ 0,
        status_t5_character == "1" ~ 1,
        status_t5_character == "2" ~ 2,
        !is.na(status_t5_character) &
          nzchar(status_t5_character) ~ 1,
        TRUE ~ NA_real_
      )
  ) |>
  dplyr::select(
    -status_t5_character
  )

table(
  final_excel_data$mt_status_t5,
  useNA = "ifany"
)

stopifnot(
  !anyNA(final_excel_data$sic),
  anyDuplicated(final_excel_data$sic) == 0,
  all(
    stats::na.omit(
      final_excel_data$mt_status_t5
    ) %in% 0:2
  )
)

#-------------------------------------------------------------------------
##### DEFINE EXACT MPLUS VARIABLE MAPPING #####
#-------------------------------------------------------------------------

final_mplus_mapping <- master_dictionary_wide |>
  dplyr::filter(
    present_m18_ses,
    m18_ses_retained %in% TRUE
  ) |>
  dplyr::transmute(
    original_name,
    mplus_name = m18_ses_mplus_name,
    excel_position =
      as.integer(m18_ses_excel_position),
    mplus_position =
      as.integer(m18_ses_mplus_position)
  ) |>
  dplyr::arrange(
    mplus_position
  )

stopifnot(
  nrow(final_mplus_mapping) > 0,
  !anyNA(final_mplus_mapping$original_name),
  !anyNA(final_mplus_mapping$mplus_name),
  !anyNA(final_mplus_mapping$excel_position),
  !anyNA(final_mplus_mapping$mplus_position),
  anyDuplicated(final_mplus_mapping$original_name) == 0,
  anyDuplicated(final_mplus_mapping$mplus_name) == 0,
  anyDuplicated(
    tolower(final_mplus_mapping$mplus_name)
  ) == 0,
  all(
    nchar(final_mplus_mapping$mplus_name) <= 8
  ),
  all(
    grepl(
      "^[A-Za-z][A-Za-z0-9_]*$",
      final_mplus_mapping$mplus_name
    )
  ),
  identical(
    final_mplus_mapping$mplus_position,
    seq_len(nrow(final_mplus_mapping))
  )
)


#-------------------------------------------------------------------------
##### CHECK EXCEL POSITIONS #####
#-------------------------------------------------------------------------

observed_excel_positions <- match(
  final_mplus_mapping$original_name,
  names(final_excel_data)
)

position_mismatches <- final_mplus_mapping |>
  dplyr::mutate(
    observed_excel_position =
      observed_excel_positions
  ) |>
  dplyr::filter(
    is.na(observed_excel_position) |
      observed_excel_position != excel_position
  )

if (nrow(position_mismatches) > 0) {
  print(
    position_mismatches,
    n = Inf
  )
  
  stop(
    "Die Variablenpositionen in der finalen Excel-Datei ",
    "stimmen nicht mit dem Master-Dictionary überein."
  )
}


#-------------------------------------------------------------------------
##### CREATE FINAL MPLUS DATASET IN EXACT ORDER #####
#-------------------------------------------------------------------------

dat_mplus_final <- final_excel_data |>
  dplyr::select(
    dplyr::all_of(
      final_mplus_mapping$original_name
    )
  )

names(dat_mplus_final) <-
  final_mplus_mapping$mplus_name

non_numeric_variables <- names(dat_mplus_final)[
  !vapply(
    dat_mplus_final,
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0) {
  stop(
    "Folgende beibehaltene Mplus-Variablen sind nicht numerisch:\n",
    paste(
      non_numeric_variables,
      collapse = ", "
    )
  )
}

stopifnot(
  nrow(dat_mplus_final) ==
    nrow(final_excel_data),
  identical(
    names(dat_mplus_final),
    final_mplus_mapping$mplus_name
  ),
  all(
    nchar(names(dat_mplus_final)) <= 8
  ),
  anyDuplicated(names(dat_mplus_final)) == 0,
  anyDuplicated(
    tolower(names(dat_mplus_final))
  ) == 0,
  "SIC_N" %in% names(dat_mplus_final),
  "stat_t5" %in% names(dat_mplus_final),
  "sesausb" %in% names(dat_mplus_final),
  !anyNA(dat_mplus_final$SIC_N),
  anyDuplicated(dat_mplus_final$SIC_N) == 0
)


#-------------------------------------------------------------------------
##### PREPARE MPLUS EXPORT #####
#-------------------------------------------------------------------------

dat_mplus_final_export <- dat_mplus_final |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ replace(
        .x,
        is.na(.x),
        -999
      )
    )
  )

stopifnot(
  all(
    vapply(
      dat_mplus_final_export,
      is.numeric,
      logical(1)
    )
  ),
  sum(is.na(dat_mplus_final_export)) == 0
)


#-------------------------------------------------------------------------
##### CREATE MPLUS NAMES LIST #####
#-------------------------------------------------------------------------

mplus_names_final <- names(
  dat_mplus_final
)

mplus_names_lines <- strwrap(
  paste(
    mplus_names_final,
    collapse = " "
  ),
  width = 84
)

mplus_names_syntax <- c(
  "NAMES ARE",
  paste0(
    "    ",
    mplus_names_lines
  ),
  ";"
)

stopifnot(
  max(nchar(mplus_names_syntax)) <= 88
)


#-------------------------------------------------------------------------
##### SAVE FINAL FILES #####
#-------------------------------------------------------------------------

dir.create(
  seadrive_mplus_data_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  mplus_input_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write.table(
  dat_mplus_final_export,
  file = seadrive_final_mplus_data_file,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  dec = "."
)

saveRDS(
  mplus_names_final,
  seadrive_final_mplus_names_file
)

writeLines(
  mplus_names_syntax,
  seadrive_final_mplus_names_text_file
)

saveRDS(
  dat_mplus_final,
  seadrive_final_mplus_rds_file
)


#-------------------------------------------------------------------------
##### COPY FINAL FILES TO C:/MPLUS/INPUTS #####
#-------------------------------------------------------------------------

files_to_copy <- c(
  seadrive_final_mplus_data_file,
  seadrive_final_mplus_names_file,
  seadrive_final_mplus_names_text_file,
  seadrive_final_mplus_rds_file
)

copy_targets <- c(
  local_final_mplus_data_file,
  local_final_mplus_names_file,
  local_final_mplus_names_text_file,
  local_final_mplus_rds_file
)

copy_success <- mapply(
  FUN = function(source_file, target_file) {
    file.copy(
      from = source_file,
      to = target_file,
      overwrite = TRUE
    )
  },
  source_file = files_to_copy,
  target_file = copy_targets
)

stopifnot(
  all(copy_success)
)


#-------------------------------------------------------------------------
##### FINAL EXPORT CHECKS #####
#-------------------------------------------------------------------------

count_mplus_columns <- function(
    data_file
) {
  
  first_line <- readLines(
    data_file,
    n = 1,
    warn = FALSE
  )
  
  if (
    length(first_line) != 1 ||
    !nzchar(first_line)
  ) {
    stop(
      "Die erste Datenzeile konnte nicht gelesen werden:\n",
      data_file
    )
  }
  
  length(
    strsplit(
      first_line,
      split = "\t",
      fixed = TRUE
    )[[1]]
  )
}

stopifnot(
  file.exists(seadrive_final_mplus_data_file),
  file.exists(seadrive_final_mplus_names_file),
  file.exists(seadrive_final_mplus_names_text_file),
  file.exists(seadrive_final_mplus_rds_file),
  file.exists(local_final_mplus_data_file),
  file.exists(local_final_mplus_names_file),
  file.exists(local_final_mplus_names_text_file),
  file.exists(local_final_mplus_rds_file),
  
  identical(
    readRDS(seadrive_final_mplus_names_file),
    mplus_names_final
  ),
  
  identical(
    readRDS(local_final_mplus_names_file),
    mplus_names_final
  ),
  
  identical(
    names(
      readRDS(local_final_mplus_rds_file)
    ),
    mplus_names_final
  ),
  
  count_mplus_columns(
    seadrive_final_mplus_data_file
  ) == length(mplus_names_final),
  
  count_mplus_columns(
    local_final_mplus_data_file
  ) == length(mplus_names_final),
  
  identical(
    unname(
      tools::md5sum(
        seadrive_final_mplus_data_file
      )
    ),
    unname(
      tools::md5sum(
        local_final_mplus_data_file
      )
    )
  )
)


#-------------------------------------------------------------------------
##### OBJECTS FOR SUBSEQUENT MPLUS MODELS #####
#-------------------------------------------------------------------------

mplus_data_name <- basename(
  local_final_mplus_data_file
)

mplus_names <- mplus_names_final

names_syntax <- paste(
  mplus_names_syntax,
  collapse = "\n"
)


#-------------------------------------------------------------------------
##### DISPLAY FINAL FILE INFORMATION #####
#-------------------------------------------------------------------------

cat(
  "\nFinaler Mplus-Datensatz erfolgreich erstellt.",
  "\nQuelle:",
  "\n", m18_ses_output_file,
  "\n",
  "\nFälle: ", nrow(dat_mplus_final),
  "\nVariablen: ", ncol(dat_mplus_final),
  "\n",
  "\nLokaler Mplus-Datensatz:",
  "\n", local_final_mplus_data_file,
  "\n",
  "\nLokale Namensliste:",
  "\n", local_final_mplus_names_text_file,
  "\n",
  sep = ""
)







#-------------------------------------------------------------------------
##### MODEL M20: REPLICATE FINAL LCS WITH NEW MPLUS DATASET #####
#-------------------------------------------------------------------------

##### CHECK REQUIRED OBJECTS #####

required_objects_M20 <- c(
  "mplus_data_name",
  "mplus_names",
  "names_syntax",
  "local_final_mplus_data_file",
  "mplus_input_dir",
  "github_m18_lcs_dir"
)

missing_objects_M20 <- required_objects_M20[
  !vapply(
    required_objects_M20,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects_M20) > 0) {
  stop(
    "Missing required objects: ",
    paste(
      missing_objects_M20,
      collapse = ", "
    )
  )
}

assert_file_exists(
  local_final_mplus_data_file,
  "Final local Mplus dataset"
)


#-------------------------------------------------------------------------
##### DEFINE LCS VARIABLES #####
#-------------------------------------------------------------------------

lcs_indicator_variables <- c(
  "hyp_b2",
  "hyp_k2",
  "hyp_p2",
  "hyp_b5",
  "hyp_k5",
  "hyp_p5",
  
  "con_b2",
  "con_k2",
  "con_p2",
  "con_b5",
  "con_k5",
  "con_p5",
  
  "emo_b2",
  "emo_k2",
  "emo_p2",
  "emo_b5",
  "emo_k5",
  "emo_p5"
)

required_lcs_variables <- c(
  "SIC_N",
  "stat_t5",
  lcs_indicator_variables
)

missing_lcs_variables <- setdiff(
  required_lcs_variables,
  mplus_names
)

if (length(missing_lcs_variables) > 0) {
  stop(
    "The following variables are missing from the final Mplus dataset: ",
    paste(
      missing_lcs_variables,
      collapse = ", "
    )
  )
}

stopifnot(
  all(nchar(required_lcs_variables) <= 8),
  anyDuplicated(required_lcs_variables) == 0
)

lcs_usevariables_syntax <- paste(
  wrap_mplus_names(
    lcs_indicator_variables,
    max_width = 88
  ),
  collapse = "\n"
)


#-------------------------------------------------------------------------
##### CREATE MPLUS INPUT #####
#-------------------------------------------------------------------------

model_M20_name <- "M20_sdq_classical_lcs_final_dataset"

input_syntax_M20 <- paste0(
  "TITLE:\n",
  "  M20: SDQ classical bivariate latent change score model\n",
  "  based on the final partial scalar invariance model\n",
  "  using the final M18 plus SES dataset;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  names_syntax, "\n\n",
  
  "  USEVARIABLES ARE\n",
  lcs_usevariables_syntax, ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n",
  "  USEOBSERVATIONS ARE stat_t5 NE 0;\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = GENERAL;\n",
  "  ESTIMATOR = MLR;\n",
  "  COVERAGE = 0.01;\n\n",
  
  "MODEL:\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Measurement model: externalizing and emotional problems\n",
  "  ! Three informants: primary caregiver, child, second caregiver\n",
  "  ! Equal factor loadings across T2 and T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  EXT2 BY\n",
  "    hyp_b2@1\n",
  "    hyp_k2 (ex2)\n",
  "    hyp_p2 (ex3)\n",
  "    con_b2 (ex4)\n",
  "    con_k2 (ex5)\n",
  "    con_p2 (ex6);\n\n",
  
  "  EXT5 BY\n",
  "    hyp_b5@1\n",
  "    hyp_k5 (ex2)\n",
  "    hyp_p5 (ex3)\n",
  "    con_b5 (ex4)\n",
  "    con_k5 (ex5)\n",
  "    con_p5 (ex6);\n\n",
  
  "  EMO2 BY\n",
  "    emo_b2@1\n",
  "    emo_k2 (le1)\n",
  "    emo_p2 (le2);\n\n",
  
  "  EMO5 BY\n",
  "    emo_b5@1\n",
  "    emo_k5 (le1)\n",
  "    emo_p5 (le2);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Final partial scalar invariance across T2 and T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  [hyp_b2 hyp_b5] (ihb);\n",
  "  [hyp_k2 hyp_k5] (ihk);\n",
  "  [hyp_p2 hyp_p5] (ihp);\n\n",
  
  "  [con_b2 con_b5] (icb);\n",
  "  [con_k2 con_k5] (ick);\n",
  "  [con_p2 con_p5] (icp);\n\n",
  
  "  [emo_b2 emo_b5] (ieb);\n",
  "  [emo_k2] (iek2);\n",
  "  [emo_k5] (iek5);\n",
  "  [emo_p2 emo_p5] (iep);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Correlated uniqueness across time\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH hyp_b5 (cu_hyp_b);\n",
  "  hyp_k2 WITH hyp_k5 (cu_hyp_k);\n",
  "  hyp_p2 WITH hyp_p5 (cu_hyp_p);\n\n",
  
  "  con_b2 WITH con_b5 (cu_con_b);\n",
  "  con_k2 WITH con_k5 (cu_con_k);\n",
  "  con_p2 WITH con_p5 (cu_con_p);\n\n",
  
  "  emo_b2 WITH emo_b5 (cu_emo_b);\n",
  "  emo_k2 WITH emo_k5 (cu_emo_k);\n",
  "  emo_p2 WITH emo_p5 (cu_emo_p);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Within-informant residual correlations at T2\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH con_b2 (w_b_hc);\n",
  "  hyp_k2 WITH con_k2 (w_k_hc);\n",
  "  hyp_p2 WITH con_p2 (w_p_hc);\n\n",
  
  "  emo_b2 WITH hyp_b2 (w_b_eh);\n",
  "  emo_b2 WITH con_b2 (w_b_ec);\n",
  "  emo_k2 WITH hyp_k2 (w_k_eh);\n",
  "  emo_k2 WITH con_k2 (w_k_ec);\n",
  "  emo_p2 WITH hyp_p2 (w_p_eh);\n",
  "  emo_p2 WITH con_p2 (w_p_ec);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Within-informant residual correlations at T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b5 WITH con_b5 (w_b_hc);\n",
  "  hyp_k5 WITH con_k5 (w_k_hc);\n",
  "  hyp_p5 WITH con_p5 (w_p_hc);\n\n",
  
  "  emo_b5 WITH hyp_b5 (w_b_eh);\n",
  "  emo_b5 WITH con_b5 (w_b_ec);\n",
  "  emo_k5 WITH hyp_k5 (w_k_eh);\n",
  "  emo_k5 WITH con_k5 (w_k_ec);\n",
  "  emo_p5 WITH hyp_p5 (w_p_eh);\n",
  "  emo_p5 WITH con_p5 (w_p_ec);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Selected between-informant residual correlations\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH hyp_p2 (bh_hyp);\n",
  "  hyp_b5 WITH hyp_p5 (bh_hyp);\n\n",
  
  "  con_b2 WITH con_p2 (bh_con);\n",
  "  con_b5 WITH con_p5 (bh_con);\n\n",
  
  "  hyp_b2 WITH con_p2 (bp_cross);\n",
  "  hyp_b5 WITH con_p5 (bp_cross);\n\n",
  
  "  hyp_p2 WITH con_b2 (pb_cross);\n",
  "  hyp_p5 WITH con_b5 (pb_cross);\n\n",
  
  "  emo_b2 WITH emo_p2 (bh_emo);\n",
  "  emo_b5 WITH emo_p5 (bh_emo);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Classical two-wave latent change score model\n",
  "  ! -------------------------------------------------\n\n",
  
  "  ! Externalizing change\n\n",
  "  d_ext BY EXT5@1;\n",
  "  EXT5 ON EXT2@1;\n",
  "  EXT5@0;\n",
  "  [EXT5@0];\n\n",
  
  "  ! Emotional-problems change\n\n",
  "  d_emo BY EMO5@1;\n",
  "  EMO5 ON EMO2@1;\n",
  "  EMO5@0;\n",
  "  [EMO5@0];\n\n",
  
  "  ! Baseline latent means fixed for identification\n\n",
  "  [EXT2@0 EMO2@0];\n\n",
  
  "  ! Latent change means freely estimated\n\n",
  "  [d_ext d_emo];\n\n",
  
  "  ! Latent change variances freely estimated\n\n",
  "  d_ext;\n",
  "  d_emo;\n\n",
  
  "  ! Covariance between baseline levels\n\n",
  "  EXT2 WITH EMO2;\n\n",
  
  "  ! Covariances between baseline levels and latent changes\n\n",
  "  EXT2 WITH d_ext d_emo;\n",
  "  EMO2 WITH d_ext d_emo;\n\n",
  
  "  ! Covariance between latent changes\n\n",
  "  d_ext WITH d_emo;\n\n",
  
  "OUTPUT:\n",
  "  SAMPSTAT;\n",
  "  STANDARDIZED;\n",
  "  CINTERVAL;\n",
  "  TECH1;\n",
  "  TECH4;\n",
  "  MODINDICES(4.0);\n"
)


#-------------------------------------------------------------------------
##### SAVE AND COPY INPUT #####
#-------------------------------------------------------------------------

input_file_M20 <- file.path(
  mplus_input_dir,
  paste0(
    model_M20_name,
    ".inp"
  )
)

github_input_file_M20 <- file.path(
  github_m18_lcs_dir,
  basename(input_file_M20)
)

writeLines(
  input_syntax_M20,
  con = input_file_M20
)

copy_file_checked(
  source_file = input_file_M20,
  target = github_input_file_M20
)

stopifnot(
  file.exists(input_file_M20),
  file.exists(github_input_file_M20),
  identical(
    readLines(
      input_file_M20,
      warn = FALSE
    ),
    readLines(
      github_input_file_M20,
      warn = FALSE
    )
  )
)

cat(
  "\nModel M20 input created.",
  "\nLocal Mplus input:",
  "\n", input_file_M20,
  "\n",
  "\nGitHub copy:",
  "\n", github_input_file_M20,
  "\n",
  sep = ""
)
#-------------------------------------------------------------------------
##### RUN MODEL M20 AND EXTRACT CENTRAL RESULTS #####
#-------------------------------------------------------------------------

stopifnot(
  exists("input_file_M20"),
  exists("model_M20_name"),
  exists("github_m18_lcs_dir"),
  file.exists(input_file_M20)
)


#-------------------------------------------------------------------------
##### LOCATE MPLUS #####
#-------------------------------------------------------------------------

mplus_command_M20 <- MplusAutomation::detectMplus()

if (MplusAutomation::mplusAvailable(silent = FALSE) != 0) {
  stop(
    "MplusAutomation konnte Mplus nicht finden. ",
    "Prüfe die Mplus-Installation bzw. den Windows-PATH."
  )
}


#-------------------------------------------------------------------------
##### DEFINE OUTPUT PATHS #####
#-------------------------------------------------------------------------

output_file_M20 <- paste0(
  tools::file_path_sans_ext(input_file_M20),
  ".out"
)

run_log_file_M20 <- file.path(
  dirname(input_file_M20),
  paste0(
    model_M20_name,
    "_run.log"
  )
)

github_output_file_M20 <- file.path(
  github_m18_lcs_dir,
  basename(output_file_M20)
)

results_excel_file_M20 <- file.path(
  github_m18_lcs_dir,
  paste0(
    model_M20_name,
    "_key_results.xlsx"
  )
)

results_rds_file_M20 <- file.path(
  github_m18_lcs_dir,
  paste0(
    model_M20_name,
    "_full_results.rds"
  )
)


#-------------------------------------------------------------------------
##### RUN MPLUS MODEL #####
#-------------------------------------------------------------------------

MplusAutomation::runModels(
  target = input_file_M20,
  recursive = FALSE,
  showOutput = TRUE,
  replaceOutfile = "always",
  logFile = run_log_file_M20,
  Mplus_command = mplus_command_M20,
  killOnFail = TRUE,
  quiet = FALSE
)

if (!file.exists(output_file_M20)) {
  stop(
    "Mplus wurde aufgerufen, aber keine Output-Datei wurde erzeugt:\n",
    output_file_M20
  )
}


#-------------------------------------------------------------------------
##### READ MPLUS OUTPUT #####
#-------------------------------------------------------------------------

model_results_M20 <- MplusAutomation::readModels(
  target = output_file_M20,
  what = c(
    "warn_err",
    "summaries",
    "parameters",
    "tech4",
    "output"
  ),
  quiet = FALSE
)


#-------------------------------------------------------------------------
##### CHECK MODEL TERMINATION #####
#-------------------------------------------------------------------------

flatten_messages <- function(x) {
  
  if (
    is.null(x) ||
    length(x) == 0
  ) {
    return(
      character(0)
    )
  }
  
  messages <- trimws(
    as.character(
      unlist(
        x,
        recursive = TRUE,
        use.names = FALSE
      )
    )
  )
  
  messages[
    nzchar(messages)
  ]
}

model_errors_M20 <- flatten_messages(
  model_results_M20$errors
)

model_warnings_M20 <- flatten_messages(
  model_results_M20$warnings
)

model_output_text_M20 <- flatten_messages(
  model_results_M20$output
)

terminated_normally_M20 <- any(
  grepl(
    "THE MODEL ESTIMATION TERMINATED NORMALLY",
    model_output_text_M20,
    fixed = TRUE
  )
)

if (
  length(model_errors_M20) > 0 ||
  !terminated_normally_M20
) {
  
  cat(
    "\nMplus errors:\n",
    paste(
      model_errors_M20,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "\nMplus warnings:\n",
    paste(
      model_warnings_M20,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )
  
  stop(
    "Modell M20 wurde nicht regulär beendet. ",
    "Bitte den Mplus-Output prüfen:\n",
    output_file_M20
  )
}


#-------------------------------------------------------------------------
##### EXTRACT MODEL FIT #####
#-------------------------------------------------------------------------

summary_M20 <- tibble::as_tibble(
  model_results_M20$summaries
)

fit_columns_M20 <- c(
  "Title",
  "Estimator",
  "Observations",
  "Parameters",
  "ChiSqM_Value",
  "ChiSqM_DF",
  "ChiSqM_PValue",
  "CFI",
  "TLI",
  "RMSEA_Estimate",
  "RMSEA_90CI_LB",
  "RMSEA_90CI_UB",
  "RMSEA_pLT05",
  "SRMR",
  "LL",
  "AIC",
  "BIC",
  "aBIC"
)

model_fit_M20 <- summary_M20 |>
  dplyr::select(
    dplyr::any_of(
      fit_columns_M20
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.numeric),
      ~ round(.x, 4)
    )
  )


#-------------------------------------------------------------------------
##### HELPER: GET PARAMETER SECTION #####
#-------------------------------------------------------------------------

get_parameter_section_M20 <- function(
    parameter_list,
    section_pattern
) {
  
  if (
    is.null(parameter_list) ||
    length(parameter_list) == 0
  ) {
    return(
      tibble::tibble()
    )
  }
  
  section_names <- names(parameter_list)
  
  selected_section <- section_names[
    grepl(
      section_pattern,
      section_names,
      ignore.case = TRUE
    )
  ]
  
  if (length(selected_section) == 0) {
    return(
      tibble::tibble()
    )
  }
  
  tibble::as_tibble(
    parameter_list[[
      selected_section[1]
    ]]
  )
}


#-------------------------------------------------------------------------
##### EXTRACT CENTRAL UNSTANDARDIZED PARAMETERS #####
#-------------------------------------------------------------------------

parameters_unstandardized_M20 <-
  get_parameter_section_M20(
    model_results_M20$parameters,
    "^unstandardized$"
  )

if (nrow(parameters_unstandardized_M20) == 0) {
  stop(
    "Die unstandardisierten Modellparameter konnten ",
    "nicht aus dem Mplus-Output gelesen werden."
  )
}

parameters_unstandardized_M20 <-
  parameters_unstandardized_M20 |>
  dplyr::mutate(
    header_upper =
      toupper(
        trimws(
          as.character(paramHeader)
        )
      ),
    
    parameter_upper =
      toupper(
        trimws(
          as.character(param)
        )
      )
  )

central_parameters_M20 <-
  parameters_unstandardized_M20 |>
  dplyr::filter(
    (
      header_upper == "MEANS" &
        parameter_upper %in%
        c(
          "D_EXT",
          "D_EMO"
        )
    ) |
      (
        header_upper == "VARIANCES" &
          parameter_upper %in%
          c(
            "D_EXT",
            "D_EMO"
          )
      ) |
      (
        header_upper == "D_EXT WITH" &
          parameter_upper == "EXT2"
      ) |
      (
        header_upper == "D_EMO WITH" &
          parameter_upper == "EMO2"
      ) |
      (
        header_upper == "EXT2 WITH" &
          parameter_upper == "EMO2"
      )
  ) |>
  dplyr::mutate(
    result = dplyr::case_when(
      header_upper == "MEANS" &
        parameter_upper == "D_EXT" ~
        "Mean latent change: externalizing",
      
      header_upper == "MEANS" &
        parameter_upper == "D_EMO" ~
        "Mean latent change: emotional problems",
      
      header_upper == "VARIANCES" &
        parameter_upper == "D_EXT" ~
        "Variance latent change: externalizing",
      
      header_upper == "VARIANCES" &
        parameter_upper == "D_EMO" ~
        "Variance latent change: emotional problems",
      
      header_upper == "D_EXT WITH" &
        parameter_upper == "EXT2" ~
        "Baseline-change covariance: externalizing",
      
      header_upper == "D_EMO WITH" &
        parameter_upper == "EMO2" ~
        "Baseline-change covariance: emotional problems",
      
      header_upper == "EXT2 WITH" &
        parameter_upper == "EMO2" ~
        "Baseline covariance: EXT with EMO",
      
      TRUE ~
        paste(
          paramHeader,
          param
        )
    )
  ) |>
  dplyr::select(
    result,
    paramHeader,
    param,
    dplyr::any_of(
      c(
        "est",
        "se",
        "est_se",
        "pval"
      )
    )
  )


#-------------------------------------------------------------------------
##### ADD 95% CONFIDENCE INTERVALS IF AVAILABLE #####
#-------------------------------------------------------------------------

parameters_ci_M20 <-
  get_parameter_section_M20(
    model_results_M20$parameters,
    "^ci\\.unstandardized$"
  )

if (nrow(parameters_ci_M20) > 0) {
  
  parameters_ci_M20 <-
    parameters_ci_M20 |>
    dplyr::mutate(
      header_upper =
        toupper(
          trimws(
            as.character(paramHeader)
          )
        ),
      
      parameter_upper =
        toupper(
          trimws(
            as.character(param)
          )
        )
    ) |>
    dplyr::select(
      header_upper,
      parameter_upper,
      dplyr::any_of(
        c(
          "low2.5",
          "up2.5"
        )
      )
    )
  
  central_parameters_M20 <-
    central_parameters_M20 |>
    dplyr::mutate(
      header_upper =
        toupper(
          trimws(
            as.character(paramHeader)
          )
        ),
      
      parameter_upper =
        toupper(
          trimws(
            as.character(param)
          )
        )
    ) |>
    dplyr::left_join(
      parameters_ci_M20,
      by = c(
        "header_upper",
        "parameter_upper"
      )
    ) |>
    dplyr::select(
      -header_upper,
      -parameter_upper
    )
}

central_parameters_M20 <-
  central_parameters_M20 |>
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.numeric),
      ~ round(.x, 4)
    )
  )


#-------------------------------------------------------------------------
##### EXTRACT STANDARDIZED ASSOCIATIONS #####
#-------------------------------------------------------------------------

parameters_stdyx_M20 <-
  get_parameter_section_M20(
    model_results_M20$parameters,
    "^stdyx\\.standardized$"
  )

standardized_associations_M20 <- tibble::tibble()

if (nrow(parameters_stdyx_M20) > 0) {
  
  standardized_associations_M20 <-
    parameters_stdyx_M20 |>
    dplyr::mutate(
      header_upper =
        toupper(
          trimws(
            as.character(paramHeader)
          )
        ),
      
      parameter_upper =
        toupper(
          trimws(
            as.character(param)
          )
        )
    ) |>
    dplyr::filter(
      (
        header_upper == "D_EXT WITH" &
          parameter_upper == "EXT2"
      ) |
        (
          header_upper == "D_EMO WITH" &
            parameter_upper == "EMO2"
        ) |
        (
          header_upper == "EXT2 WITH" &
            parameter_upper == "EMO2"
        )
    ) |>
    dplyr::mutate(
      result = dplyr::case_when(
        header_upper == "D_EXT WITH" ~
          "Baseline-change association: externalizing",
        
        header_upper == "D_EMO WITH" ~
          "Baseline-change association: emotional problems",
        
        header_upper == "EXT2 WITH" ~
          "Baseline association: EXT with EMO",
        
        TRUE ~
          paste(
            paramHeader,
            param
          )
      )
    ) |>
    dplyr::select(
      result,
      paramHeader,
      param,
      dplyr::any_of(
        c(
          "est",
          "se",
          "est_se",
          "pval"
        )
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ round(.x, 4)
      )
    )
}


#-------------------------------------------------------------------------
##### CREATE DIAGNOSTICS TABLE #####
#-------------------------------------------------------------------------

diagnostics_M20 <- tibble::tibble(
  item = c(
    "Normal termination",
    "Number of Mplus errors",
    "Number of Mplus warnings",
    "Input file",
    "Output file",
    "Run log"
  ),
  
  value = c(
    as.character(terminated_normally_M20),
    as.character(length(model_errors_M20)),
    as.character(length(model_warnings_M20)),
    input_file_M20,
    output_file_M20,
    run_log_file_M20
  )
)

warning_table_M20 <- tibble::tibble(
  warning = if (
    length(model_warnings_M20) == 0
  ) {
    "No Mplus warnings."
  } else {
    model_warnings_M20
  }
)


#-------------------------------------------------------------------------
##### SAVE OUTPUT AND EXTRACTED RESULTS #####
#-------------------------------------------------------------------------

copy_success_M20 <- file.copy(
  from = output_file_M20,
  to = github_output_file_M20,
  overwrite = TRUE
)

stopifnot(
  copy_success_M20,
  file.exists(github_output_file_M20)
)

saveRDS(
  model_results_M20,
  results_rds_file_M20
)

writexl::write_xlsx(
  x = list(
    Model_fit =
      model_fit_M20,
    
    Central_parameters =
      central_parameters_M20,
    
    Standardized =
      standardized_associations_M20,
    
    Diagnostics =
      diagnostics_M20,
    
    Warnings =
      warning_table_M20
  ),
  path = results_excel_file_M20
)

stopifnot(
  file.exists(results_excel_file_M20),
  file.exists(results_rds_file_M20)
)


#-------------------------------------------------------------------------
##### DISPLAY CENTRAL RESULTS #####
#-------------------------------------------------------------------------

cat(
  "\n============================================================",
  "\nMODEL M20 COMPLETED SUCCESSFULLY",
  "\n============================================================\n",
  sep = ""
)

cat(
  "\nMODEL FIT\n"
)

print(
  model_fit_M20,
  width = Inf
)

cat(
  "\nCENTRAL UNSTANDARDIZED LCS PARAMETERS\n"
)

print(
  central_parameters_M20,
  n = Inf,
  width = Inf
)

if (nrow(standardized_associations_M20) > 0) {
  
  cat(
    "\nSTANDARDIZED ASSOCIATIONS\n"
  )
  
  print(
    standardized_associations_M20,
    n = Inf,
    width = Inf
  )
}

if (length(model_warnings_M20) > 0) {
  
  cat(
    "\nMPLUS WARNINGS\n",
    paste(
      model_warnings_M20,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )
}

cat(
  "\nExtracted Excel results:",
  "\n", results_excel_file_M20,
  "\n",
  "\nFull parsed Mplus results:",
  "\n", results_rds_file_M20,
  "\n",
  "\nCopied Mplus output:",
  "\n", github_output_file_M20,
  "\n",
  sep = ""
)
#-------------------------------------------------------------------------
##### MODEL M21: LCS WITH LTC CLASS PREDICTORS #####
#-------------------------------------------------------------------------

##### CHECK REQUIRED OBJECTS #####

required_objects_M21 <- c(
  "mplus_data_name",
  "mplus_names",
  "names_syntax",
  "local_final_mplus_data_file",
  "mplus_input_dir",
  "github_m18_lcs_dir"
)

missing_objects_M21 <- required_objects_M21[
  !vapply(
    required_objects_M21,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects_M21) > 0) {
  stop(
    "Missing required objects: ",
    paste(
      missing_objects_M21,
      collapse = ", "
    )
  )
}

assert_file_exists(
  local_final_mplus_data_file,
  "Final local Mplus dataset"
)


#-------------------------------------------------------------------------
##### DEFINE LCS VARIABLES #####
#-------------------------------------------------------------------------

lcs_indicator_variables <- c(
  "hyp_b2",
  "hyp_k2",
  "hyp_p2",
  "hyp_b5",
  "hyp_k5",
  "hyp_p5",
  
  "con_b2",
  "con_k2",
  "con_p2",
  "con_b5",
  "con_k5",
  "con_p5",
  
  "emo_b2",
  "emo_k2",
  "emo_p2",
  "emo_b5",
  "emo_k5",
  "emo_p5"
)

# Resolve the existing LTC class variable from the final Mplus dataset.
# According to the master dictionary, its Mplus name is m18_cls.
class_variable_M21 <- "m18_cls"

if (!(tolower(class_variable_M21) %in% tolower(mplus_names))) {
  stop(
    "The LTC class variable 'm18_cls' is not present in the final Mplus dataset. ",
    "Class-like names currently available are: ",
    paste(
      mplus_names[
        grepl("m18|class|cls|^c$", mplus_names, ignore.case = TRUE)
      ],
      collapse = ", "
    )
  )
}

# Preserve the exact spelling/capitalization used in the Mplus NAMES list.
class_variable_M21 <- mplus_names[
  match(tolower(class_variable_M21), tolower(mplus_names))
]

required_lcs_variables <- c(
  "SIC_N",
  "stat_t5",
  class_variable_M21,
  lcs_indicator_variables
)

missing_lcs_variables <- required_lcs_variables[
  !(tolower(required_lcs_variables) %in% tolower(mplus_names))
]

if (length(missing_lcs_variables) > 0) {
  stop(
    "The following variables are missing from the final Mplus dataset: ",
    paste(
      missing_lcs_variables,
      collapse = ", "
    )
  )
}

stopifnot(
  all(nchar(required_lcs_variables) <= 8),
  anyDuplicated(required_lcs_variables) == 0
)

lcs_usevariables_syntax <- paste(
  wrap_mplus_names(
    lcs_indicator_variables,
    max_width = 88
  ),
  collapse = "\n"
)


#-------------------------------------------------------------------------
##### CREATE MPLUS INPUT #####
#-------------------------------------------------------------------------

model_M21_name <- "M21_lcs_with_LTC_classes"

input_syntax_M21 <- paste0(
  "TITLE:\n",
  "  M21: Bivariate latent change score model with LTC classes\n",
  "  Class 1 (low and stable) is the reference group\n",
  "  Classes predict baseline levels and latent changes;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  names_syntax, "\n\n",
  
  "  USEVARIABLES ARE\n",
  lcs_usevariables_syntax, "\n",
  "    c2 c3;\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n",
  "  USEOBSERVATIONS ARE (stat_t5 NE 0) AND (", class_variable_M21, " GT 0);\n\n",
  
  "DEFINE:\n",
  "  c2 = 0;\n",
  "  c3 = 0;\n",
  "  IF (", class_variable_M21, " EQ 2) THEN c2 = 1;\n",
  "  IF (", class_variable_M21, " EQ 3) THEN c3 = 1;\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = GENERAL;\n",
  "  ESTIMATOR = MLR;\n",
  "  COVERAGE = 0.01;\n\n",
  
  "MODEL:\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Measurement model: externalizing and emotional problems\n",
  "  ! Three informants: primary caregiver, child, second caregiver\n",
  "  ! Equal factor loadings across T2 and T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  EXT2 BY\n",
  "    hyp_b2@1\n",
  "    hyp_k2 (ex2)\n",
  "    hyp_p2 (ex3)\n",
  "    con_b2 (ex4)\n",
  "    con_k2 (ex5)\n",
  "    con_p2 (ex6);\n\n",
  
  "  EXT5 BY\n",
  "    hyp_b5@1\n",
  "    hyp_k5 (ex2)\n",
  "    hyp_p5 (ex3)\n",
  "    con_b5 (ex4)\n",
  "    con_k5 (ex5)\n",
  "    con_p5 (ex6);\n\n",
  
  "  EMO2 BY\n",
  "    emo_b2@1\n",
  "    emo_k2 (le1)\n",
  "    emo_p2 (le2);\n\n",
  
  "  EMO5 BY\n",
  "    emo_b5@1\n",
  "    emo_k5 (le1)\n",
  "    emo_p5 (le2);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Final partial scalar invariance across T2 and T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  [hyp_b2 hyp_b5] (ihb);\n",
  "  [hyp_k2 hyp_k5] (ihk);\n",
  "  [hyp_p2 hyp_p5] (ihp);\n\n",
  
  "  [con_b2 con_b5] (icb);\n",
  "  [con_k2 con_k5] (ick);\n",
  "  [con_p2 con_p5] (icp);\n\n",
  
  "  [emo_b2 emo_b5] (ieb);\n",
  "  [emo_k2] (iek2);\n",
  "  [emo_k5] (iek5);\n",
  "  [emo_p2 emo_p5] (iep);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Correlated uniqueness across time\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH hyp_b5 (cu_hyp_b);\n",
  "  hyp_k2 WITH hyp_k5 (cu_hyp_k);\n",
  "  hyp_p2 WITH hyp_p5 (cu_hyp_p);\n\n",
  
  "  con_b2 WITH con_b5 (cu_con_b);\n",
  "  con_k2 WITH con_k5 (cu_con_k);\n",
  "  con_p2 WITH con_p5 (cu_con_p);\n\n",
  
  "  emo_b2 WITH emo_b5 (cu_emo_b);\n",
  "  emo_k2 WITH emo_k5 (cu_emo_k);\n",
  "  emo_p2 WITH emo_p5 (cu_emo_p);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Within-informant residual correlations at T2\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH con_b2 (w_b_hc);\n",
  "  hyp_k2 WITH con_k2 (w_k_hc);\n",
  "  hyp_p2 WITH con_p2 (w_p_hc);\n\n",
  
  "  emo_b2 WITH hyp_b2 (w_b_eh);\n",
  "  emo_b2 WITH con_b2 (w_b_ec);\n",
  "  emo_k2 WITH hyp_k2 (w_k_eh);\n",
  "  emo_k2 WITH con_k2 (w_k_ec);\n",
  "  emo_p2 WITH hyp_p2 (w_p_eh);\n",
  "  emo_p2 WITH con_p2 (w_p_ec);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Within-informant residual correlations at T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b5 WITH con_b5 (w_b_hc);\n",
  "  hyp_k5 WITH con_k5 (w_k_hc);\n",
  "  hyp_p5 WITH con_p5 (w_p_hc);\n\n",
  
  "  emo_b5 WITH hyp_b5 (w_b_eh);\n",
  "  emo_b5 WITH con_b5 (w_b_ec);\n",
  "  emo_k5 WITH hyp_k5 (w_k_eh);\n",
  "  emo_k5 WITH con_k5 (w_k_ec);\n",
  "  emo_p5 WITH hyp_p5 (w_p_eh);\n",
  "  emo_p5 WITH con_p5 (w_p_ec);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Selected between-informant residual correlations\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH hyp_p2 (bh_hyp);\n",
  "  hyp_b5 WITH hyp_p5 (bh_hyp);\n\n",
  
  "  con_b2 WITH con_p2 (bh_con);\n",
  "  con_b5 WITH con_p5 (bh_con);\n\n",
  
  "  hyp_b2 WITH con_p2 (bp_cross);\n",
  "  hyp_b5 WITH con_p5 (bp_cross);\n\n",
  
  "  hyp_p2 WITH con_b2 (pb_cross);\n",
  "  hyp_p5 WITH con_b5 (pb_cross);\n\n",
  
  "  emo_b2 WITH emo_p2 (bh_emo);\n",
  "  emo_b5 WITH emo_p5 (bh_emo);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Classical two-wave latent change score model\n",
  "  ! -------------------------------------------------\n\n",
  
  "  ! Externalizing change\n\n",
  "  d_ext BY EXT5@1;\n",
  "  EXT5 ON EXT2@1;\n",
  "  EXT5@0;\n",
  "  [EXT5@0];\n\n",
  
  "  ! Emotional-problems change\n\n",
  "  d_emo BY EMO5@1;\n",
  "  EMO5 ON EMO2@1;\n",
  "  EMO5@0;\n",
  "  [EMO5@0];\n\n",
  
  "  ! Baseline latent intercepts fixed for identification\n\n",
  "  [EXT2@0 EMO2@0];\n\n",
  
  "  ! Latent change intercepts: expected change in reference class 1\n\n",
  "  [d_ext] (m_dext);\n",
  "  [d_emo] (m_demo);\n\n",
  
  "  ! LTC class effects on baseline levels\n",
  "  ! Class 1 = low and stable (reference)\n\n",
  "  EXT2 ON c2 (e2_c2);\n",
  "  EXT2 ON c3 (e2_c3);\n",
  "  EMO2 ON c2 (m2_c2);\n",
  "  EMO2 ON c3 (m2_c3);\n\n",
  
  "  ! LTC class effects on latent changes\n\n",
  "  d_ext ON c2 (dx_c2);\n",
  "  d_ext ON c3 (dx_c3);\n",
  "  d_emo ON c2 (dm_c2);\n",
  "  d_emo ON c3 (dm_c3);\n\n",
  
  "  ! Latent change variances freely estimated\n\n",
  "  d_ext;\n",
  "  d_emo;\n\n",
  
  "  ! Covariance between baseline levels\n\n",
  "  EXT2 WITH EMO2;\n\n",
  
  "  ! Covariances between baseline levels and latent changes\n\n",
  "  EXT2 WITH d_ext d_emo;\n",
  "  EMO2 WITH d_ext d_emo;\n\n",
  
  "  ! Covariance between latent changes\n\n",
  "  d_ext WITH d_emo;\n\n",
  
  "MODEL CONSTRAINT:\n",
  "  NEW(\n",
  "    ex2_c1 ex2_c2 ex2_c3\n",
  "    em2_c1 em2_c2 em2_c3\n",
  "    dex_c1 dex_c2 dex_c3\n",
  "    dem_c1 dem_c2 dem_c3\n",
  "    ex_3v2 em_3v2 dx_3v2 dm_3v2\n",
  "  );\n\n",
  "  ex2_c1 = 0;\n",
  "  ex2_c2 = e2_c2;\n",
  "  ex2_c3 = e2_c3;\n\n",
  "  em2_c1 = 0;\n",
  "  em2_c2 = m2_c2;\n",
  "  em2_c3 = m2_c3;\n\n",
  "  dex_c1 = m_dext;\n",
  "  dex_c2 = m_dext + dx_c2;\n",
  "  dex_c3 = m_dext + dx_c3;\n\n",
  "  dem_c1 = m_demo;\n",
  "  dem_c2 = m_demo + dm_c2;\n",
  "  dem_c3 = m_demo + dm_c3;\n\n",
  "  ex_3v2 = e2_c3 - e2_c2;\n",
  "  em_3v2 = m2_c3 - m2_c2;\n",
  "  dx_3v2 = dx_c3 - dx_c2;\n",
  "  dm_3v2 = dm_c3 - dm_c2;\n\n",
  
  "OUTPUT:\n",
  "  SAMPSTAT;\n",
  "  STANDARDIZED;\n",
  "  CINTERVAL;\n",
  "  TECH1;\n",
  "  TECH4;\n",
  "  MODINDICES(4.0);\n"
)


#-------------------------------------------------------------------------
##### SAVE AND COPY INPUT #####
#-------------------------------------------------------------------------

input_file_M21 <- file.path(
  mplus_input_dir,
  paste0(
    model_M21_name,
    ".inp"
  )
)

github_input_file_M21 <- file.path(
  github_m18_lcs_dir,
  basename(input_file_M21)
)

if (
  grepl(
    "ON c2 \\(.*\\) c3 \\(",
    input_syntax_M21
  )
) {
  stop(
    "Internal syntax check failed: combined labelled ON statements remain."
  )
}

writeLines(
  input_syntax_M21,
  con = input_file_M21
)

copy_file_checked(
  source_file = input_file_M21,
  target = github_input_file_M21
)

stopifnot(
  file.exists(input_file_M21),
  file.exists(github_input_file_M21),
  identical(
    readLines(
      input_file_M21,
      warn = FALSE
    ),
    readLines(
      github_input_file_M21,
      warn = FALSE
    )
  )
)

cat(
  "\nModel M21 input created.",
  "\nLocal Mplus input:",
  "\n", input_file_M21,
  "\n",
  "\nGitHub copy:",
  "\n", github_input_file_M21,
  "\n",
  sep = ""
)

cat("\nClass variable used to create M21 dummies: m18_cls\n")

#-------------------------------------------------------------------------
##### RUN MODEL M21 AND EXTRACT LTC CLASS RESULTS #####
#-------------------------------------------------------------------------


if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
  stop(
    "Das Paket 'MplusAutomation' fehlt. Bitte einmal ausführen:\n",
    "install.packages('MplusAutomation')"
  )
}

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop(
    "Das Paket 'writexl' fehlt. Bitte einmal ausführen:\n",
    "install.packages('writexl')"
  )
}

stopifnot(
  exists("input_file_M21"),
  exists("model_M21_name"),
  exists("github_m18_lcs_dir"),
  file.exists(input_file_M21)
)


#-------------------------------------------------------------------------
##### LOCATE MPLUS #####
#-------------------------------------------------------------------------

mplus_command_M21 <- MplusAutomation::detectMplus()

if (MplusAutomation::mplusAvailable(silent = FALSE) != 0) {
  stop(
    "MplusAutomation konnte Mplus nicht finden. ",
    "Prüfe die Mplus-Installation bzw. den Windows-PATH."
  )
}


#-------------------------------------------------------------------------
##### DEFINE OUTPUT PATHS #####
#-------------------------------------------------------------------------

output_file_M21 <- paste0(
  tools::file_path_sans_ext(input_file_M21),
  ".out"
)

run_log_file_M21 <- file.path(
  dirname(input_file_M21),
  paste0(
    model_M21_name,
    "_run.log"
  )
)

github_output_file_M21 <- file.path(
  github_m18_lcs_dir,
  basename(output_file_M21)
)

results_excel_file_M21 <- file.path(
  github_m18_lcs_dir,
  paste0(
    model_M21_name,
    "_key_results.xlsx"
  )
)

results_rds_file_M21 <- file.path(
  github_m18_lcs_dir,
  paste0(
    model_M21_name,
    "_full_results.rds"
  )
)


#-------------------------------------------------------------------------
##### RUN MPLUS MODEL #####
#-------------------------------------------------------------------------

MplusAutomation::runModels(
  target = input_file_M21,
  recursive = FALSE,
  showOutput = TRUE,
  replaceOutfile = "always",
  logFile = run_log_file_M21,
  Mplus_command = mplus_command_M21,
  killOnFail = TRUE,
  quiet = FALSE
)

if (!file.exists(output_file_M21)) {
  stop(
    "Mplus wurde aufgerufen, aber keine Output-Datei wurde erzeugt:\n",
    output_file_M21
  )
}


#-------------------------------------------------------------------------
##### READ MPLUS OUTPUT #####
#-------------------------------------------------------------------------

model_results_M21 <- MplusAutomation::readModels(
  target = output_file_M21,
  what = c(
    "warn_err",
    "summaries",
    "parameters",
    "tech4",
    "output"
  ),
  quiet = FALSE
)


#-------------------------------------------------------------------------
##### CHECK MODEL TERMINATION #####
#-------------------------------------------------------------------------

flatten_messages <- function(x) {
  
  if (
    is.null(x) ||
    length(x) == 0
  ) {
    return(
      character(0)
    )
  }
  
  messages <- trimws(
    as.character(
      unlist(
        x,
        recursive = TRUE,
        use.names = FALSE
      )
    )
  )
  
  messages[
    nzchar(messages)
  ]
}

model_errors_M21 <- flatten_messages(
  model_results_M21$errors
)

model_warnings_M21 <- flatten_messages(
  model_results_M21$warnings
)

model_output_text_M21 <- flatten_messages(
  model_results_M21$output
)

terminated_normally_M21 <- any(
  grepl(
    "THE MODEL ESTIMATION TERMINATED NORMALLY",
    model_output_text_M21,
    fixed = TRUE
  )
)

if (
  length(model_errors_M21) > 0 ||
  !terminated_normally_M21
) {
  
  cat(
    "\nMplus errors:\n",
    paste(
      model_errors_M21,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "\nMplus warnings:\n",
    paste(
      model_warnings_M21,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )
  
  stop(
    "Modell M21 wurde nicht regulär beendet. ",
    "Bitte den Mplus-Output prüfen:\n",
    output_file_M21
  )
}


#-------------------------------------------------------------------------
##### EXTRACT MODEL FIT #####
#-------------------------------------------------------------------------

summary_M21 <- tibble::as_tibble(
  model_results_M21$summaries
)

fit_columns_M21 <- c(
  "Title",
  "Estimator",
  "Observations",
  "Parameters",
  "ChiSqM_Value",
  "ChiSqM_DF",
  "ChiSqM_PValue",
  "CFI",
  "TLI",
  "RMSEA_Estimate",
  "RMSEA_90CI_LB",
  "RMSEA_90CI_UB",
  "RMSEA_pLT05",
  "SRMR",
  "LL",
  "AIC",
  "BIC",
  "aBIC"
)

model_fit_M21 <- summary_M21 |>
  dplyr::select(
    dplyr::any_of(
      fit_columns_M21
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.numeric),
      ~ round(.x, 4)
    )
  )


#-------------------------------------------------------------------------
##### HELPER: GET PARAMETER SECTION #####
#-------------------------------------------------------------------------

get_parameter_section_M21 <- function(
    parameter_list,
    section_pattern
) {
  
  if (
    is.null(parameter_list) ||
    length(parameter_list) == 0
  ) {
    return(
      tibble::tibble()
    )
  }
  
  section_names <- names(parameter_list)
  
  selected_section <- section_names[
    grepl(
      section_pattern,
      section_names,
      ignore.case = TRUE
    )
  ]
  
  if (length(selected_section) == 0) {
    return(
      tibble::tibble()
    )
  }
  
  tibble::as_tibble(
    parameter_list[[
      selected_section[1]
    ]]
  )
}


#-------------------------------------------------------------------------
##### EXTRACT UNSTANDARDIZED CLASS EFFECTS #####
#-------------------------------------------------------------------------

parameters_unstandardized_M21 <- get_parameter_section_M21(
  model_results_M21$parameters, "^unstandardized$"
)
if (nrow(parameters_unstandardized_M21) == 0) {
  stop("Die unstandardisierten Modellparameter konnten nicht gelesen werden.")
}
parameters_unstandardized_M21 <- parameters_unstandardized_M21 |>
  dplyr::mutate(
    header_upper = toupper(trimws(as.character(paramHeader))),
    parameter_upper = toupper(trimws(as.character(param)))
  )

class_effects_M21 <- parameters_unstandardized_M21 |>
  dplyr::filter(
    header_upper %in% c("EXT2 ON", "EMO2 ON", "D_EXT ON", "D_EMO ON"),
    parameter_upper %in% c("C2", "C3")
  ) |>
  dplyr::mutate(
    outcome = dplyr::case_when(
      header_upper == "EXT2 ON" ~ "Baseline externalizing",
      header_upper == "EMO2 ON" ~ "Baseline emotional problems",
      header_upper == "D_EXT ON" ~ "Change in externalizing",
      header_upper == "D_EMO ON" ~ "Change in emotional problems"
    ),
    comparison = dplyr::case_when(
      parameter_upper == "C2" ~ "Class 2 vs Class 1",
      parameter_upper == "C3" ~ "Class 3 vs Class 1"
    )
  ) |>
  dplyr::select(outcome, comparison, paramHeader, param,
                dplyr::any_of(c("est", "se", "est_se", "pval")))

parameters_ci_M21 <- get_parameter_section_M21(
  model_results_M21$parameters, "^ci\\.unstandardized$"
)
if (nrow(parameters_ci_M21) > 0) {
  parameters_ci_M21 <- parameters_ci_M21 |>
    dplyr::mutate(
      header_upper = toupper(trimws(as.character(paramHeader))),
      parameter_upper = toupper(trimws(as.character(param)))
    ) |>
    dplyr::select(header_upper, parameter_upper,
                  dplyr::any_of(c("low2.5", "up2.5")))
  class_effects_M21 <- class_effects_M21 |>
    dplyr::mutate(
      header_upper = toupper(trimws(as.character(paramHeader))),
      parameter_upper = toupper(trimws(as.character(param)))
    ) |>
    dplyr::left_join(parameters_ci_M21,
                     by = c("header_upper", "parameter_upper")) |>
    dplyr::select(-header_upper, -parameter_upper)
}
class_effects_M21 <- class_effects_M21 |>
  dplyr::mutate(
    significance = dplyr::case_when(
      is.na(pval) ~ NA_character_, pval < .001 ~ "p < .001",
      pval < .05 ~ "p < .05", TRUE ~ "not significant"
    ),
    dplyr::across(dplyr::where(is.numeric), ~ round(.x, 4))
  )

# Model-constraint parameters: estimated class values and class 3 vs 2
additional_parameters_M21 <- get_parameter_section_M21(
  model_results_M21$parameters, "new/additional|additional|new parameters"
)
class_estimates_M21 <- tibble::tibble()
class_contrasts_3v2_M21 <- tibble::tibble()
if (nrow(additional_parameters_M21) > 0) {
  additional_parameters_M21 <- additional_parameters_M21 |>
    dplyr::mutate(parameter_upper = toupper(trimws(as.character(param))))
  class_estimates_M21 <- additional_parameters_M21 |>
    dplyr::filter(parameter_upper %in% c(
      "EX2_C1","EX2_C2","EX2_C3","EM2_C1","EM2_C2","EM2_C3",
      "DEX_C1","DEX_C2","DEX_C3","DEM_C1","DEM_C2","DEM_C3")) |>
    dplyr::mutate(
      outcome = dplyr::case_when(
        grepl("^EX2", parameter_upper) ~ "Baseline externalizing",
        grepl("^EM2", parameter_upper) ~ "Baseline emotional problems",
        grepl("^DEX", parameter_upper) ~ "Latent change: externalizing",
        grepl("^DEM", parameter_upper) ~ "Latent change: emotional problems"),
      class = dplyr::case_when(
        grepl("C1$", parameter_upper) ~ "Class 1: Low and stable",
        grepl("C2$", parameter_upper) ~ "Class 2: Elevated and declining",
        grepl("C3$", parameter_upper) ~ "Class 3: High early burden with later rebound")
    ) |>
    dplyr::select(outcome, class, param,
                  dplyr::any_of(c("est","se","est_se","pval"))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x,4)))
  class_contrasts_3v2_M21 <- additional_parameters_M21 |>
    dplyr::filter(parameter_upper %in% c("EX_3V2","EM_3V2","DX_3V2","DM_3V2")) |>
    dplyr::mutate(
      outcome = dplyr::case_when(
        parameter_upper == "EX_3V2" ~ "Baseline externalizing",
        parameter_upper == "EM_3V2" ~ "Baseline emotional problems",
        parameter_upper == "DX_3V2" ~ "Change in externalizing",
        parameter_upper == "DM_3V2" ~ "Change in emotional problems"),
      comparison = "Class 3 vs Class 2"
    ) |>
    dplyr::select(outcome, comparison, param,
                  dplyr::any_of(c("est","se","est_se","pval"))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x,4)))
}

parameters_stdyx_M21 <- get_parameter_section_M21(
  model_results_M21$parameters, "^stdyx\\.standardized$"
)
standardized_class_effects_M21 <- tibble::tibble()
if (nrow(parameters_stdyx_M21) > 0) {
  standardized_class_effects_M21 <- parameters_stdyx_M21 |>
    dplyr::mutate(
      header_upper = toupper(trimws(as.character(paramHeader))),
      parameter_upper = toupper(trimws(as.character(param)))
    ) |>
    dplyr::filter(
      header_upper %in% c("EXT2 ON","EMO2 ON","D_EXT ON","D_EMO ON"),
      parameter_upper %in% c("C2","C3")
    ) |>
    dplyr::mutate(
      outcome = dplyr::case_when(
        header_upper == "EXT2 ON" ~ "Baseline externalizing",
        header_upper == "EMO2 ON" ~ "Baseline emotional problems",
        header_upper == "D_EXT ON" ~ "Change in externalizing",
        header_upper == "D_EMO ON" ~ "Change in emotional problems"),
      comparison = dplyr::case_when(
        parameter_upper == "C2" ~ "Class 2 vs Class 1",
        parameter_upper == "C3" ~ "Class 3 vs Class 1")
    ) |>
    dplyr::select(outcome, comparison, paramHeader, param,
                  dplyr::any_of(c("est","se","est_se","pval"))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x,4)))
}

class_counts_M21 <- tibble::tibble()
if (exists("local_final_mplus_data_file") && exists("mplus_names") &&
    file.exists(local_final_mplus_data_file)) {
  
  # Resolve independently in case this extraction script is run separately.
  # The master dictionary identifies the LTC class variable as m18_cls.
  class_variable_M21 <- "m18_cls"
  
  if (!(tolower(class_variable_M21) %in% tolower(mplus_names))) {
    stop(
      "The LTC class variable 'm18_cls' is not present in the final Mplus dataset. ",
      "Class-like names currently available are: ",
      paste(
        mplus_names[
          grepl("m18|class|cls|^c$", mplus_names, ignore.case = TRUE)
        ],
        collapse = ", "
      )
    )
  }
  
  # Preserve the exact spelling/capitalization used in the NAMES list.
  class_variable_M21 <- mplus_names[
    match(tolower(class_variable_M21), tolower(mplus_names))
  ]
  
  raw_data_M21 <- utils::read.table(
    local_final_mplus_data_file, header = FALSE, na.strings = "-999",
    col.names = mplus_names, check.names = FALSE
  )
  
  class_counts_M21 <- raw_data_M21 |>
    dplyr::transmute(
      stat_t5 = .data[["stat_t5"]],
      mt_class = .data[[class_variable_M21]]
    ) |>
    dplyr::filter(stat_t5 != 0, mt_class %in% 1:3) |>
    dplyr::count(mt_class, name = "n") |>
    dplyr::mutate(
      class_label = dplyr::case_when(
        mt_class == 1 ~ "Low and stable",
        mt_class == 2 ~ "Elevated and declining",
        mt_class == 3 ~ "High early burden with later rebound"),
      percent = round(100 * n / sum(n), 1)
    ) |>
    dplyr::select(mt_class, class_label, n, percent)
}

#-------------------------------------------------------------------------
##### CREATE AUTOMATIC INTERPRETATION SUMMARY #####
#-------------------------------------------------------------------------

interpret_effect_M21 <- function(outcome, comparison, est, pval) {
  direction <- ifelse(est > 0, "higher / more positive", "lower / more negative")
  significance <- dplyr::case_when(
    is.na(pval) ~ "p-value unavailable",
    pval < .001 ~ "statistically significant (p < .001)",
    pval < .05 ~ paste0("statistically significant (p = ", format(round(pval, 3), nsmall = 3), ")"),
    TRUE ~ paste0("not statistically significant (p = ", format(round(pval, 3), nsmall = 3), ")")
  )
  paste0(comparison, " shows a ", direction, " value for ", tolower(outcome),
         " relative to Class 1; the difference is ", significance, ".")
}

interpretation_summary_M21 <- class_effects_M21 |>
  dplyr::mutate(
    interpretation = mapply(
      interpret_effect_M21,
      outcome, comparison, est, pval,
      USE.NAMES = FALSE
    )
  ) |>
  dplyr::select(outcome, comparison, est, pval, interpretation)

#-------------------------------------------------------------------------
##### CREATE DIAGNOSTICS TABLE #####
#-------------------------------------------------------------------------

diagnostics_M21 <- tibble::tibble(
  item = c(
    "Normal termination",
    "Number of Mplus errors",
    "Number of Mplus warnings",
    "Input file",
    "Output file",
    "Run log"
  ),
  
  value = c(
    as.character(terminated_normally_M21),
    as.character(length(model_errors_M21)),
    as.character(length(model_warnings_M21)),
    input_file_M21,
    output_file_M21,
    run_log_file_M21
  )
)

warning_table_M21 <- tibble::tibble(
  warning = if (
    length(model_warnings_M21) == 0
  ) {
    "No Mplus warnings."
  } else {
    model_warnings_M21
  }
)


#-------------------------------------------------------------------------
##### SAVE OUTPUT AND EXTRACTED RESULTS #####
#-------------------------------------------------------------------------

copy_success_M21 <- file.copy(
  from = output_file_M21,
  to = github_output_file_M21,
  overwrite = TRUE
)

stopifnot(
  copy_success_M21,
  file.exists(github_output_file_M21)
)

saveRDS(
  model_results_M21,
  results_rds_file_M21
)

writexl::write_xlsx(
  x = list(
    Model_fit =
      model_fit_M21,
    
    Class_counts = class_counts_M21,
    Class_effects = class_effects_M21,
    Class_estimates = class_estimates_M21,
    Class3_vs_Class2 = class_contrasts_3v2_M21,
    Interpretation = interpretation_summary_M21,
    Standardized = standardized_class_effects_M21,
    
    Diagnostics =
      diagnostics_M21,
    
    Warnings =
      warning_table_M21
  ),
  path = results_excel_file_M21
)

stopifnot(
  file.exists(results_excel_file_M21),
  file.exists(results_rds_file_M21)
)


#-------------------------------------------------------------------------
##### DISPLAY CENTRAL RESULTS #####
#-------------------------------------------------------------------------

cat(
  "\n============================================================",
  "\nMODEL M21 COMPLETED SUCCESSFULLY",
  "\n============================================================\n",
  sep = ""
)

cat(
  "\nMODEL FIT\n"
)

print(
  model_fit_M21,
  width = Inf
)

cat("\nLTC CLASS COUNTS\n")
print(class_counts_M21, n = Inf, width = Inf)

cat("\nUNSTANDARDIZED CLASS EFFECTS: CLASS 2/3 VS CLASS 1\n")
print(class_effects_M21, n = Inf, width = Inf)

cat("\nAUTOMATIC INTERPRETATION SUMMARY\n")
print(interpretation_summary_M21, n = Inf, width = Inf)

if (nrow(class_estimates_M21) > 0) {
  cat("\nESTIMATED BASELINE LEVELS AND LATENT CHANGES BY CLASS\n")
  print(class_estimates_M21, n = Inf, width = Inf)
}
if (nrow(class_contrasts_3v2_M21) > 0) {
  cat("\nDIRECT CONTRAST: CLASS 3 VS CLASS 2\n")
  print(class_contrasts_3v2_M21, n = Inf, width = Inf)
}
if (nrow(standardized_class_effects_M21) > 0) {
  cat("\nSTANDARDIZED CLASS EFFECTS\n")
  print(standardized_class_effects_M21, n = Inf, width = Inf)
}

if (length(model_warnings_M21) > 0) {
  
  cat(
    "\nMPLUS WARNINGS\n",
    paste(
      model_warnings_M21,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )
}

cat(
  "\nExtracted Excel results:",
  "\n", results_excel_file_M21,
  "\n",
  "\nFull parsed Mplus results:",
  "\n", results_rds_file_M21,
  "\n",
  "\nCopied Mplus output:",
  "\n", github_output_file_M21,
  "\n",
  sep = ""
)

#-------------------------------------------------------------------------
##### MODEL M21_ALTERNATIVE: LCS WITH KNOWN LTC CLASSES #####
#-------------------------------------------------------------------------

##### CHECK REQUIRED OBJECTS #####

required_objects_M21_alt <- c(
  "mplus_data_name",
  "mplus_names",
  "names_syntax",
  "local_final_mplus_data_file",
  "mplus_input_dir",
  "github_m18_lcs_dir",
  "mplus_command_M21"
)

missing_objects_M21_alt <- required_objects_M21_alt[
  !vapply(
    required_objects_M21_alt,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects_M21_alt) > 0) {
  stop(
    "Missing required objects: ",
    paste(
      missing_objects_M21_alt,
      collapse = ", "
    )
  )
}

assert_file_exists(
  local_final_mplus_data_file,
  "Final local Mplus dataset"
)


#-------------------------------------------------------------------------
##### DEFINE LCS VARIABLES #####
#-------------------------------------------------------------------------

lcs_indicator_variables_alt <- c(
  "hyp_b2",
  "hyp_k2",
  "hyp_p2",
  "hyp_b5",
  "hyp_k5",
  "hyp_p5",
  
  "con_b2",
  "con_k2",
  "con_p2",
  "con_b5",
  "con_k5",
  "con_p5",
  
  "emo_b2",
  "emo_k2",
  "emo_p2",
  "emo_b5",
  "emo_k5",
  "emo_p5"
)

# Resolve the existing LTC class variable from the final Mplus dataset.
# According to the master dictionary, its Mplus name is m18_cls.
class_variable_M21_alt <- "m18_cls"

if (!(tolower(class_variable_M21_alt) %in% tolower(mplus_names))) {
  stop(
    "The LTC class variable 'm18_cls' is not present in the final Mplus dataset. ",
    "Class-like names currently available are: ",
    paste(
      mplus_names[
        grepl(
          "m18|class|cls|^c$",
          mplus_names,
          ignore.case = TRUE
        )
      ],
      collapse = ", "
    )
  )
}

# Preserve the exact spelling/capitalization used in the Mplus NAMES list.
class_variable_M21_alt <- mplus_names[
  match(
    tolower(class_variable_M21_alt),
    tolower(mplus_names)
  )
]

required_lcs_variables_alt <- c(
  "SIC_N",
  "stat_t5",
  class_variable_M21_alt,
  lcs_indicator_variables_alt
)

missing_lcs_variables_alt <- required_lcs_variables_alt[
  !(tolower(required_lcs_variables_alt) %in% tolower(mplus_names))
]

if (length(missing_lcs_variables_alt) > 0) {
  stop(
    "The following variables are missing from the final Mplus dataset: ",
    paste(
      missing_lcs_variables_alt,
      collapse = ", "
    )
  )
}

stopifnot(
  all(nchar(required_lcs_variables_alt) <= 8),
  anyDuplicated(required_lcs_variables_alt) == 0
)

lcs_usevariables_syntax_alt <- paste(
  wrap_mplus_names(
    lcs_indicator_variables_alt,
    max_width = 88
  ),
  collapse = "\n"
)


#-------------------------------------------------------------------------
##### CREATE MPLUS INPUT #####
#-------------------------------------------------------------------------

model_M21_alt_name <- "M21_alternative_lcs_known_classes"

input_syntax_M21_alt <- paste0(
  "TITLE:\n",
  "  M21 alternative: Bivariate latent change score model\n",
  "  LTC classes are included as known classes\n",
  "  Same measurement and LCS specifications as M21;\n\n",
  
  "DATA:\n",
  "  FILE = ", mplus_data_name, ";\n\n",
  
  "VARIABLE:\n",
  names_syntax, "\n\n",
  
  "  USEVARIABLES ARE\n",
  lcs_usevariables_syntax_alt, ";\n\n",
  
  "  MISSING ARE ALL (-999);\n",
  "  IDVARIABLE IS SIC_N;\n",
  "  USEOBSERVATIONS ARE (stat_t5 NE 0) AND (",
  class_variable_M21_alt,
  " GT 0);\n",
  "  CLASSES = kc(3);\n",
  "  KNOWNCLASS = kc(",
  class_variable_M21_alt, " = 1 ",
  class_variable_M21_alt, " = 2 ",
  class_variable_M21_alt, " = 3);\n\n",
  
  "ANALYSIS:\n",
  "  TYPE = MIXTURE;\n",
  "  ESTIMATOR = MLR;\n",
  "  COVERAGE = 0.01;\n",
  "  STARTS = 0;\n\n",
  
  "MODEL:\n\n",
  
  "  %OVERALL%\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Measurement model: externalizing and emotional problems\n",
  "  ! Three informants: primary caregiver, child, second caregiver\n",
  "  ! Equal factor loadings across T2 and T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  EXT2 BY\n",
  "    hyp_b2@1\n",
  "    hyp_k2 (ex2)\n",
  "    hyp_p2 (ex3)\n",
  "    con_b2 (ex4)\n",
  "    con_k2 (ex5)\n",
  "    con_p2 (ex6);\n\n",
  
  "  EXT5 BY\n",
  "    hyp_b5@1\n",
  "    hyp_k5 (ex2)\n",
  "    hyp_p5 (ex3)\n",
  "    con_b5 (ex4)\n",
  "    con_k5 (ex5)\n",
  "    con_p5 (ex6);\n\n",
  
  "  EMO2 BY\n",
  "    emo_b2@1\n",
  "    emo_k2 (le1)\n",
  "    emo_p2 (le2);\n\n",
  
  "  EMO5 BY\n",
  "    emo_b5@1\n",
  "    emo_k5 (le1)\n",
  "    emo_p5 (le2);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Final partial scalar invariance across T2 and T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  [hyp_b2 hyp_b5] (ihb);\n",
  "  [hyp_k2 hyp_k5] (ihk);\n",
  "  [hyp_p2 hyp_p5] (ihp);\n\n",
  
  "  [con_b2 con_b5] (icb);\n",
  "  [con_k2 con_k5] (ick);\n",
  "  [con_p2 con_p5] (icp);\n\n",
  
  "  [emo_b2 emo_b5] (ieb);\n",
  "  [emo_k2] (iek2);\n",
  "  [emo_k5] (iek5);\n",
  "  [emo_p2 emo_p5] (iep);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Correlated uniqueness across time\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH hyp_b5 (cu_hyp_b);\n",
  "  hyp_k2 WITH hyp_k5 (cu_hyp_k);\n",
  "  hyp_p2 WITH hyp_p5 (cu_hyp_p);\n\n",
  
  "  con_b2 WITH con_b5 (cu_con_b);\n",
  "  con_k2 WITH con_k5 (cu_con_k);\n",
  "  con_p2 WITH con_p5 (cu_con_p);\n\n",
  
  "  emo_b2 WITH emo_b5 (cu_emo_b);\n",
  "  emo_k2 WITH emo_k5 (cu_emo_k);\n",
  "  emo_p2 WITH emo_p5 (cu_emo_p);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Within-informant residual correlations at T2\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH con_b2 (w_b_hc);\n",
  "  hyp_k2 WITH con_k2 (w_k_hc);\n",
  "  hyp_p2 WITH con_p2 (w_p_hc);\n\n",
  
  "  emo_b2 WITH hyp_b2 (w_b_eh);\n",
  "  emo_b2 WITH con_b2 (w_b_ec);\n",
  "  emo_k2 WITH hyp_k2 (w_k_eh);\n",
  "  emo_k2 WITH con_k2 (w_k_ec);\n",
  "  emo_p2 WITH hyp_p2 (w_p_eh);\n",
  "  emo_p2 WITH con_p2 (w_p_ec);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Within-informant residual correlations at T5\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b5 WITH con_b5 (w_b_hc);\n",
  "  hyp_k5 WITH con_k5 (w_k_hc);\n",
  "  hyp_p5 WITH con_p5 (w_p_hc);\n\n",
  
  "  emo_b5 WITH hyp_b5 (w_b_eh);\n",
  "  emo_b5 WITH con_b5 (w_b_ec);\n",
  "  emo_k5 WITH hyp_k5 (w_k_eh);\n",
  "  emo_k5 WITH con_k5 (w_k_ec);\n",
  "  emo_p5 WITH hyp_p5 (w_p_eh);\n",
  "  emo_p5 WITH con_p5 (w_p_ec);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Selected between-informant residual correlations\n",
  "  ! -------------------------------------------------\n\n",
  
  "  hyp_b2 WITH hyp_p2 (bh_hyp);\n",
  "  hyp_b5 WITH hyp_p5 (bh_hyp);\n\n",
  
  "  con_b2 WITH con_p2 (bh_con);\n",
  "  con_b5 WITH con_p5 (bh_con);\n\n",
  
  "  hyp_b2 WITH con_p2 (bp_cross);\n",
  "  hyp_b5 WITH con_p5 (bp_cross);\n\n",
  
  "  hyp_p2 WITH con_b2 (pb_cross);\n",
  "  hyp_p5 WITH con_b5 (pb_cross);\n\n",
  
  "  emo_b2 WITH emo_p2 (bh_emo);\n",
  "  emo_b5 WITH emo_p5 (bh_emo);\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Classical two-wave latent change score model\n",
  "  ! -------------------------------------------------\n\n",
  
  "  ! Externalizing change\n\n",
  "  d_ext BY EXT5@1;\n",
  "  EXT5 ON EXT2@1;\n",
  "  EXT5@0;\n",
  "  [EXT5@0];\n\n",
  
  "  ! Emotional-problems change\n\n",
  "  d_emo BY EMO5@1;\n",
  "  EMO5 ON EMO2@1;\n",
  "  EMO5@0;\n",
  "  [EMO5@0];\n\n",
  
  "  ! Latent change variances freely estimated\n",
  "  ! and constrained equal across known classes\n\n",
  "  d_ext;\n",
  "  d_emo;\n\n",
  
  "  ! Covariance between baseline levels\n",
  "  ! constrained equal across known classes\n\n",
  "  EXT2 WITH EMO2;\n\n",
  
  "  ! Covariances between baseline levels and latent changes\n",
  "  ! constrained equal across known classes\n\n",
  "  EXT2 WITH d_ext d_emo;\n",
  "  EMO2 WITH d_ext d_emo;\n\n",
  
  "  ! Covariance between latent changes\n",
  "  ! constrained equal across known classes\n\n",
  "  d_ext WITH d_emo;\n\n",
  
  "  ! -------------------------------------------------\n",
  "  ! Class-specific latent means\n",
  "  ! -------------------------------------------------\n\n",
  
  "  %kc#1%\n",
  "  ! Class 1: Low and stable\n",
  "  ! Baseline factor means define the reference scale\n",
  "  [EXT2@0 EMO2@0];\n",
  "  [d_ext] (dex_c1);\n",
  "  [d_emo] (dem_c1);\n\n",
  
  "  %kc#2%\n",
  "  ! Class 2: Elevated and declining\n",
  "  [EXT2] (ex2_c2);\n",
  "  [EMO2] (em2_c2);\n",
  "  [d_ext] (dex_c2);\n",
  "  [d_emo] (dem_c2);\n\n",
  
  "  %kc#3%\n",
  "  ! Class 3: High early burden with later rebound\n",
  "  [EXT2] (ex2_c3);\n",
  "  [EMO2] (em2_c3);\n",
  "  [d_ext] (dex_c3);\n",
  "  [d_emo] (dem_c3);\n\n",
  
  "MODEL CONSTRAINT:\n",
  "  NEW(\n",
  "    ex2_c1 em2_c1\n",
  "    ex_2v1 ex_3v1 ex_3v2\n",
  "    em_2v1 em_3v1 em_3v2\n",
  "    dx_2v1 dx_3v1 dx_3v2\n",
  "    dm_2v1 dm_3v1 dm_3v2\n",
  "  );\n\n",
  
  "  ex2_c1 = 0;\n",
  "  em2_c1 = 0;\n\n",
  
  "  ex_2v1 = ex2_c2 - ex2_c1;\n",
  "  ex_3v1 = ex2_c3 - ex2_c1;\n",
  "  ex_3v2 = ex2_c3 - ex2_c2;\n\n",
  
  "  em_2v1 = em2_c2 - em2_c1;\n",
  "  em_3v1 = em2_c3 - em2_c1;\n",
  "  em_3v2 = em2_c3 - em2_c2;\n\n",
  
  "  dx_2v1 = dex_c2 - dex_c1;\n",
  "  dx_3v1 = dex_c3 - dex_c1;\n",
  "  dx_3v2 = dex_c3 - dex_c2;\n\n",
  
  "  dm_2v1 = dem_c2 - dem_c1;\n",
  "  dm_3v1 = dem_c3 - dem_c1;\n",
  "  dm_3v2 = dem_c3 - dem_c2;\n\n",
  
  "OUTPUT:\n",
  "  SAMPSTAT;\n",
  "  STANDARDIZED;\n",
  "  CINTERVAL;\n",
  "  TECH1;\n",
  "  TECH4;\n",
  "  TECH8;\n"
)


#-------------------------------------------------------------------------
##### SAVE AND COPY INPUT #####
#-------------------------------------------------------------------------

input_file_M21_alt <- file.path(
  mplus_input_dir,
  paste0(
    model_M21_alt_name,
    ".inp"
  )
)

github_input_file_M21_alt <- file.path(
  github_m18_lcs_dir,
  basename(input_file_M21_alt)
)

# Internal checks: ensure that this really is a known-class model
# and that no dummy-predictor syntax from M21 remains.
stopifnot(
  grepl(
    "TYPE = MIXTURE;",
    input_syntax_M21_alt,
    fixed = TRUE
  ),
  grepl(
    "KNOWNCLASS = kc(",
    input_syntax_M21_alt,
    fixed = TRUE
  ),
  !grepl(
    "EXT2 ON c2",
    input_syntax_M21_alt,
    fixed = TRUE
  ),
  !grepl(
    "d_ext ON c2",
    input_syntax_M21_alt,
    fixed = TRUE
  )
)

writeLines(
  input_syntax_M21_alt,
  con = input_file_M21_alt
)

copy_file_checked(
  source_file = input_file_M21_alt,
  target = github_input_file_M21_alt
)

stopifnot(
  file.exists(input_file_M21_alt),
  file.exists(github_input_file_M21_alt),
  identical(
    readLines(
      input_file_M21_alt,
      warn = FALSE
    ),
    readLines(
      github_input_file_M21_alt,
      warn = FALSE
    )
  )
)

cat(
  "\nModel M21 alternative input created.",
  "\nLocal Mplus input:",
  "\n", input_file_M21_alt,
  "\n",
  "\nGitHub copy:",
  "\n", github_input_file_M21_alt,
  "\n",
  sep = ""
)

cat(
  "\nKnown-class variable used: ",
  class_variable_M21_alt,
  "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### RUN MODEL M21_ALTERNATIVE #####
#-------------------------------------------------------------------------

if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
  stop(
    "Das Paket 'MplusAutomation' fehlt. Bitte einmal ausführen:\n",
    "install.packages('MplusAutomation')"
  )
}

stopifnot(
  exists("input_file_M21_alt"),
  exists("model_M21_alt_name"),
  exists("github_m18_lcs_dir"),
  exists("mplus_command_M21"),
  file.exists(input_file_M21_alt)
)


#-------------------------------------------------------------------------
##### DEFINE OUTPUT PATHS #####
#-------------------------------------------------------------------------

output_file_M21_alt <- paste0(
  tools::file_path_sans_ext(input_file_M21_alt),
  ".out"
)

run_log_file_M21_alt <- file.path(
  dirname(input_file_M21_alt),
  paste0(
    model_M21_alt_name,
    "_run.log"
  )
)

github_output_file_M21_alt <- file.path(
  github_m18_lcs_dir,
  basename(output_file_M21_alt)
)

results_excel_file_M21_alt <- file.path(
  github_m18_lcs_dir,
  paste0(
    model_M21_alt_name,
    "_key_results.xlsx"
  )
)

results_rds_file_M21_alt <- file.path(
  github_m18_lcs_dir,
  paste0(
    model_M21_alt_name,
    "_full_results.rds"
  )
)


#-------------------------------------------------------------------------
##### RUN MPLUS MODEL #####
#-------------------------------------------------------------------------

MplusAutomation::runModels(
  target = input_file_M21_alt,
  recursive = FALSE,
  showOutput = TRUE,
  replaceOutfile = "always",
  logFile = run_log_file_M21_alt,
  Mplus_command = mplus_command_M21,
  killOnFail = TRUE,
  quiet = FALSE
)

if (!file.exists(output_file_M21_alt)) {
  stop(
    "Mplus wurde aufgerufen, aber keine Output-Datei wurde erzeugt:\n",
    output_file_M21_alt
  )
}

copy_file_checked(
  source_file = output_file_M21_alt,
  target = github_output_file_M21_alt
)

stopifnot(
  file.exists(output_file_M21_alt),
  file.exists(github_output_file_M21_alt)
)

cat(
  "\nModel M21 alternative completed.",
  "\nLocal Mplus output:",
  "\n", output_file_M21_alt,
  "\n",
  "\nGitHub copy:",
  "\n", github_output_file_M21_alt,
  "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### MODEL M22: M21 + AGE_T5 AND SEX #####
#-------------------------------------------------------------------------

required_M22 <- c(
  "input_syntax_M21",
  "mplus_names",
  "mplus_input_dir",
  "github_m18_lcs_dir",
  "mplus_command_M21",
  "copy_file_checked"
)

missing_M22 <- required_M22[
  !vapply(required_M22, exists, logical(1), inherits = TRUE)
]

if (length(missing_M22) > 0) {
  stop(
    "Missing required objects: ",
    paste(missing_M22, collapse = ", ")
  )
}

covariates_M22 <- c("aget5m", "sext5")

if (!all(tolower(covariates_M22) %in% tolower(mplus_names))) {
  stop("M22 covariates aget5m and/or sext5 are missing.")
}

covariates_M22 <- mplus_names[
  match(tolower(covariates_M22), tolower(mplus_names))
]

age_M22 <- covariates_M22[1]
sex_M22 <- covariates_M22[2]

model_M22_name <- "M22_lcs_with_LTC_classes_age_sex"

input_syntax_M22 <- input_syntax_M21

input_syntax_M22 <- sub(
  "M21: Bivariate latent change score model with LTC classes",
  "M22: M21 adjusted for age and sex",
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  "Classes predict baseline levels and latent changes;",
  "Classes, age, and sex predict baseline levels and latent changes;",
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  "    c2 c3;\n\n",
  paste0(
    "    ",
    age_M22, " ",
    sex_M22,
    " c2 c3;\n\n"
  ),
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  paste0(
    "  IF (", class_variable_M21,
    " EQ 3) THEN c3 = 1;\n\n"
  ),
  paste0(
    "  IF (", class_variable_M21,
    " EQ 3) THEN c3 = 1;\n",
    "  CENTER ", age_M22, " ", sex_M22,
    " (GRANDMEAN);\n\n"
  ),
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  "  EMO2 ON c3 (m2_c3);\n\n",
  paste0(
    "  EMO2 ON c3 (m2_c3);\n",
    "  EXT2 ON ", age_M22, " ", sex_M22, ";\n",
    "  EMO2 ON ", age_M22, " ", sex_M22, ";\n\n"
  ),
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  "  d_emo ON c3 (dm_c3);\n\n",
  paste0(
    "  d_emo ON c3 (dm_c3);\n",
    "  d_ext ON ", age_M22, " ", sex_M22, ";\n",
    "  d_emo ON ", age_M22, " ", sex_M22, ";\n\n"
  ),
  input_syntax_M22,
  fixed = TRUE
)

stopifnot(
  grepl(
    paste0("CENTER ", age_M22, " ", sex_M22, " (GRANDMEAN);"),
    input_syntax_M22,
    fixed = TRUE
  ),
  grepl(
    paste0("d_ext ON ", age_M22, " ", sex_M22, ";"),
    input_syntax_M22,
    fixed = TRUE
  )
)

input_file_M22 <- file.path(
  mplus_input_dir,
  paste0(model_M22_name, ".inp")
)

github_input_file_M22 <- file.path(
  github_m18_lcs_dir,
  basename(input_file_M22)
)

writeLines(input_syntax_M22, input_file_M22)

copy_file_checked(
  source_file = input_file_M22,
  target = github_input_file_M22
)


#-------------------------------------------------------------------------
##### RUN AND READ MODEL #####
#-------------------------------------------------------------------------

output_file_M22 <- paste0(
  tools::file_path_sans_ext(input_file_M22),
  ".out"
)

github_output_file_M22 <- file.path(
  github_m18_lcs_dir,
  basename(output_file_M22)
)

run_log_file_M22 <- file.path(
  dirname(input_file_M22),
  paste0(model_M22_name, "_run.log")
)

MplusAutomation::runModels(
  target = input_file_M22,
  recursive = FALSE,
  showOutput = TRUE,
  replaceOutfile = "always",
  logFile = run_log_file_M22,
  Mplus_command = mplus_command_M21,
  killOnFail = TRUE,
  quiet = FALSE
)

if (!file.exists(output_file_M22)) {
  stop("No M22 output was created: ", output_file_M22)
}

results_M22 <- MplusAutomation::readModels(
  output_file_M22,
  what = c("summaries", "parameters", "warn_err"),
  quiet = TRUE
)

if (length(results_M22$errors) > 0) {
  stop(
    "M22 contains Mplus errors:\n",
    paste(unlist(results_M22$errors), collapse = "\n")
  )
}

copy_file_checked(
  source_file = output_file_M22,
  target = github_output_file_M22
)

saveRDS(
  results_M22,
  file.path(
    github_m18_lcs_dir,
    paste0(model_M22_name, "_full_results.rds")
  )
)


#-------------------------------------------------------------------------
##### DISPLAY CENTRAL RESULTS #####
#-------------------------------------------------------------------------

cat("\nM22 completed successfully.\n\nMODEL FIT\n")
print(results_M22$summaries, width = Inf)

parameters_M22 <- tibble::as_tibble(
  results_M22$parameters$unstandardized
)

effects_M22 <- parameters_M22 |>
  dplyr::filter(
    toupper(.data$paramHeader) %in% c(
      "EXT2 ON",
      "EMO2 ON",
      "D_EXT ON",
      "D_EMO ON"
    ),
    toupper(.data$param) %in% toupper(
      c("c2", "c3", age_M22, sex_M22)
    )
  )

cat("\nCLASS AND COVARIATE EFFECTS\n")
print(effects_M22, n = Inf, width = Inf)

cat(
  "\nSaved:\n",
  input_file_M22, "\n",
  output_file_M22, "\n",
  github_input_file_M22, "\n",
  github_output_file_M22, "\n",
  sep = ""
)
#-------------------------------------------------------------------------
##### MODEL M22: M21 + AGE_T2 AND SEX #####
#-------------------------------------------------------------------------

required_M22 <- c(
  "input_syntax_M21",
  "mplus_names",
  "mplus_input_dir",
  "github_m18_lcs_dir",
  "mplus_command_M21",
  "copy_file_checked"
)

missing_M22 <- required_M22[
  !vapply(required_M22, exists, logical(1), inherits = TRUE)
]

if (length(missing_M22) > 0) {
  stop(
    "Missing required objects: ",
    paste(missing_M22, collapse = ", ")
  )
}

covariates_M22 <- c("aget2", "sext5")

if (!all(tolower(covariates_M22) %in% tolower(mplus_names))) {
  stop("M22 covariates aget2 and/or sext5 are missing.")
}

covariates_M22 <- mplus_names[
  match(tolower(covariates_M22), tolower(mplus_names))
]

age_M22 <- covariates_M22[1]
sex_M22 <- covariates_M22[2]

model_M22_name <- "M22_lcs_with_LTC_classes_ageT2_sex"

input_syntax_M22 <- input_syntax_M21

input_syntax_M22 <- sub(
  "M21: Bivariate latent change score model with LTC classes",
  "M22: M21 adjusted for age at T2 and sex",
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  "Classes predict baseline levels and latent changes;",
  "Classes, age at T2, and sex predict baseline levels and latent changes;",
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  "    c2 c3;\n\n",
  paste0(
    "    ",
    age_M22, " ",
    sex_M22,
    " c2 c3;\n\n"
  ),
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  paste0(
    "  IF (", class_variable_M21,
    " EQ 3) THEN c3 = 1;\n\n"
  ),
  paste0(
    "  IF (", class_variable_M21,
    " EQ 3) THEN c3 = 1;\n",
    "  CENTER ", age_M22, " ", sex_M22,
    " (GRANDMEAN);\n\n"
  ),
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  "  EMO2 ON c3 (m2_c3);\n\n",
  paste0(
    "  EMO2 ON c3 (m2_c3);\n",
    "  EXT2 ON ", age_M22, " ", sex_M22, ";\n",
    "  EMO2 ON ", age_M22, " ", sex_M22, ";\n\n"
  ),
  input_syntax_M22,
  fixed = TRUE
)

input_syntax_M22 <- sub(
  "  d_emo ON c3 (dm_c3);\n\n",
  paste0(
    "  d_emo ON c3 (dm_c3);\n",
    "  d_ext ON ", age_M22, " ", sex_M22, ";\n",
    "  d_emo ON ", age_M22, " ", sex_M22, ";\n\n"
  ),
  input_syntax_M22,
  fixed = TRUE
)

stopifnot(
  grepl(
    paste0("CENTER ", age_M22, " ", sex_M22, " (GRANDMEAN);"),
    input_syntax_M22,
    fixed = TRUE
  ),
  grepl(
    paste0("d_ext ON ", age_M22, " ", sex_M22, ";"),
    input_syntax_M22,
    fixed = TRUE
  )
)

input_file_M22 <- file.path(
  mplus_input_dir,
  paste0(model_M22_name, ".inp")
)

github_input_file_M22 <- file.path(
  github_m18_lcs_dir,
  basename(input_file_M22)
)

writeLines(input_syntax_M22, input_file_M22)

copy_file_checked(
  source_file = input_file_M22,
  target = github_input_file_M22
)


#-------------------------------------------------------------------------
##### RUN AND READ MODEL #####
#-------------------------------------------------------------------------

output_file_M22 <- paste0(
  tools::file_path_sans_ext(input_file_M22),
  ".out"
)

github_output_file_M22 <- file.path(
  github_m18_lcs_dir,
  basename(output_file_M22)
)

run_log_file_M22 <- file.path(
  dirname(input_file_M22),
  paste0(model_M22_name, "_run.log")
)

MplusAutomation::runModels(
  target = input_file_M22,
  recursive = FALSE,
  showOutput = TRUE,
  replaceOutfile = "always",
  logFile = run_log_file_M22,
  Mplus_command = mplus_command_M21,
  killOnFail = TRUE,
  quiet = FALSE
)

if (!file.exists(output_file_M22)) {
  stop("No M22 output was created: ", output_file_M22)
}

results_M22 <- MplusAutomation::readModels(
  output_file_M22,
  what = c("summaries", "parameters", "warn_err"),
  quiet = TRUE
)

if (length(results_M22$errors) > 0) {
  stop(
    "M22 contains Mplus errors:\n",
    paste(unlist(results_M22$errors), collapse = "\n")
  )
}

copy_file_checked(
  source_file = output_file_M22,
  target = github_output_file_M22
)

saveRDS(
  results_M22,
  file.path(
    github_m18_lcs_dir,
    paste0(model_M22_name, "_full_results.rds")
  )
)



#-------------------------------------------------------------------------
##### MODEL M21CC: M21 ON THE M22-T5 COMPLETE-CASE SAMPLE #####
#-------------------------------------------------------------------------

required_M21cc <- c(
  "input_syntax_M21",
  "mplus_names",
  "mplus_input_dir",
  "github_m18_lcs_dir",
  "mplus_command_M21",
  "copy_file_checked",
  "class_variable_M21"
)

missing_M21cc <- required_M21cc[
  !vapply(required_M21cc, exists, logical(1), inherits = TRUE)
]

if (length(missing_M21cc) > 0) {
  stop(
    "Missing required objects: ",
    paste(missing_M21cc, collapse = ", ")
  )
}

cc_variables_M21cc <- c("aget5m", "sext5")

if (!all(tolower(cc_variables_M21cc) %in% tolower(mplus_names))) {
  stop("M21cc variables aget5m and/or sext5 are missing.")
}

cc_variables_M21cc <- mplus_names[
  match(tolower(cc_variables_M21cc), tolower(mplus_names))
]

age_t5_M21cc <- cc_variables_M21cc[1]
sex_M21cc <- cc_variables_M21cc[2]

model_M21cc_name <- "M21cc_lcs_with_LTC_classes_T5age_complete_cases"

input_syntax_M21cc <- input_syntax_M21

input_syntax_M21cc <- sub(
  "M21: Bivariate latent change score model with LTC classes",
  "M21cc: M21 restricted to complete cases for T5 age and sex",
  input_syntax_M21cc,
  fixed = TRUE
)

old_useobs_M21cc <- paste0(
  "  USEOBSERVATIONS ARE (stat_t5 NE 0) AND (",
  class_variable_M21,
  " GT 0);"
)

new_useobs_M21cc <- paste0(
  "  USEOBSERVATIONS ARE\n",
  "    (stat_t5 NE 0) AND\n",
  "    (", class_variable_M21, " GT 0) AND\n",
  "    (", age_t5_M21cc, " NE -999) AND\n",
  "    (", sex_M21cc, " NE -999);"
)

if (!grepl(old_useobs_M21cc, input_syntax_M21cc, fixed = TRUE)) {
  stop("The expected M21 USEOBSERVATIONS statement was not found.")
}

input_syntax_M21cc <- sub(
  old_useobs_M21cc,
  new_useobs_M21cc,
  input_syntax_M21cc,
  fixed = TRUE
)

stopifnot(
  grepl(
    paste0("(", age_t5_M21cc, " NE -999)"),
    input_syntax_M21cc,
    fixed = TRUE
  ),
  grepl(
    paste0("(", sex_M21cc, " NE -999)"),
    input_syntax_M21cc,
    fixed = TRUE
  ),
  !grepl(
    paste0("EXT2 ON ", age_t5_M21cc),
    input_syntax_M21cc,
    fixed = TRUE
  ),
  !grepl(
    paste0("d_ext ON ", age_t5_M21cc),
    input_syntax_M21cc,
    fixed = TRUE
  )
)

input_file_M21cc <- file.path(
  mplus_input_dir,
  paste0(model_M21cc_name, ".inp")
)

github_input_file_M21cc <- file.path(
  github_m18_lcs_dir,
  basename(input_file_M21cc)
)

writeLines(
  input_syntax_M21cc,
  input_file_M21cc
)

copy_file_checked(
  source_file = input_file_M21cc,
  target = github_input_file_M21cc
)


#-------------------------------------------------------------------------
##### RUN AND READ MODEL #####
#-------------------------------------------------------------------------

output_file_M21cc <- paste0(
  tools::file_path_sans_ext(input_file_M21cc),
  ".out"
)

github_output_file_M21cc <- file.path(
  github_m18_lcs_dir,
  basename(output_file_M21cc)
)

run_log_file_M21cc <- file.path(
  dirname(input_file_M21cc),
  paste0(model_M21cc_name, "_run.log")
)

MplusAutomation::runModels(
  target = input_file_M21cc,
  recursive = FALSE,
  showOutput = TRUE,
  replaceOutfile = "always",
  logFile = run_log_file_M21cc,
  Mplus_command = mplus_command_M21,
  killOnFail = TRUE,
  quiet = FALSE
)

if (!file.exists(output_file_M21cc)) {
  stop("No M21cc output was created: ", output_file_M21cc)
}

results_M21cc <- MplusAutomation::readModels(
  output_file_M21cc,
  what = c("summaries", "parameters", "warn_err"),
  quiet = TRUE
)

if (length(results_M21cc$errors) > 0) {
  stop(
    "M21cc contains Mplus errors:\n",
    paste(unlist(results_M21cc$errors), collapse = "\n")
  )
}

copy_file_checked(
  source_file = output_file_M21cc,
  target = github_output_file_M21cc
)

saveRDS(
  results_M21cc,
  file.path(
    github_m18_lcs_dir,
    paste0(model_M21cc_name, "_full_results.rds")
  )
)


#-------------------------------------------------------------------------
##### DISPLAY CENTRAL RESULTS #####
#-------------------------------------------------------------------------

cat("\nM21cc completed successfully.\n\nMODEL FIT\n")
print(results_M21cc$summaries, width = Inf)

parameters_M21cc <- tibble::as_tibble(
  results_M21cc$parameters$unstandardized
)

class_effects_M21cc <- parameters_M21cc |>
  dplyr::filter(
    toupper(.data$paramHeader) %in% c(
      "EXT2 ON",
      "EMO2 ON",
      "D_EXT ON",
      "D_EMO ON"
    ),
    toupper(.data$param) %in% c("C2", "C3")
  )

cat("\nCLASS EFFECTS\n")
print(class_effects_M21cc, n = Inf, width = Inf)

cat(
  "\nSaved:\n",
  input_file_M21cc, "\n",
  output_file_M21cc, "\n",
  github_input_file_M21cc, "\n",
  github_output_file_M21cc, "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### MODEL M22B: AGE T2 + SEX + FOLLOW-UP DURATION #####
#-------------------------------------------------------------------------

# followup = months between T2 and T5
dat_mplus_final <- dat_mplus_final |>
  dplyr::mutate(
    followup = aget5m - aget2
  )

if (any(dat_mplus_final$followup < 0, na.rm = TRUE)) {
  stop("Negative follow-up durations detected.")
}

cat("\nFollow-up duration in months:\n")
print(summary(dat_mplus_final$followup))


#-------------------------------------------------------------------------
##### UPDATE FINAL MPLUS DATASET AND NAMES LIST #####
#-------------------------------------------------------------------------

mplus_names <- names(dat_mplus_final)

stopifnot(
  all(nchar(mplus_names) <= 8),
  anyDuplicated(mplus_names) == 0,
  "followup" %in% mplus_names
)

names_syntax <- paste0(
  "  NAMES ARE\n",
  paste(
    wrap_mplus_names(
      mplus_names,
      max_width = 88
    ),
    collapse = "\n"
  ),
  ";"
)

utils::write.table(
  dat_mplus_final,
  file = local_final_mplus_data_file,
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE,
  na = "-999"
)

assert_file_exists(
  local_final_mplus_data_file,
  "Updated final local Mplus dataset"
)


#-------------------------------------------------------------------------
##### CREATE M22B INPUT FROM M21 #####
#-------------------------------------------------------------------------

required_M22b <- c(
  "input_syntax_M21",
  "class_variable_M21",
  "mplus_data_name",
  "mplus_input_dir",
  "github_m18_lcs_dir",
  "mplus_command_M21",
  "copy_file_checked"
)

missing_M22b <- required_M22b[
  !vapply(required_M22b, exists, logical(1), inherits = TRUE)
]

if (length(missing_M22b) > 0) {
  stop(
    "Missing required objects: ",
    paste(missing_M22b, collapse = ", ")
  )
}

model_M22b_name <- "M22b_lcs_classes_ageT2_sex_followup"

input_syntax_M22b <- input_syntax_M21

# Replace the old NAMES list with the updated list containing followup.
input_syntax_M22b <- sub(
  "(?s)\\s*NAMES ARE.*?;\\s*USEVARIABLES ARE",
  paste0(
    "\n",
    names_syntax,
    "\n\n  USEVARIABLES ARE"
  ),
  input_syntax_M22b,
  perl = TRUE
)

input_syntax_M22b <- sub(
  "M21: Bivariate latent change score model with LTC classes",
  "M22b: M21 adjusted for age at T2, sex, and follow-up duration",
  input_syntax_M22b,
  fixed = TRUE
)

input_syntax_M22b <- sub(
  "Classes predict baseline levels and latent changes;",
  "Classes and covariates predict baseline levels and latent changes;",
  input_syntax_M22b,
  fixed = TRUE
)

input_syntax_M22b <- sub(
  "    c2 c3;\n\n",
  "    aget2 sext5 followup c2 c3;\n\n",
  input_syntax_M22b,
  fixed = TRUE
)

input_syntax_M22b <- sub(
  paste0(
    "  IF (", class_variable_M21,
    " EQ 3) THEN c3 = 1;\n\n"
  ),
  paste0(
    "  IF (", class_variable_M21,
    " EQ 3) THEN c3 = 1;\n",
    "  CENTER aget2 sext5 followup (GRANDMEAN);\n\n"
  ),
  input_syntax_M22b,
  fixed = TRUE
)

input_syntax_M22b <- sub(
  "  EMO2 ON c3 (m2_c3);\n\n",
  paste0(
    "  EMO2 ON c3 (m2_c3);\n",
    "  EXT2 ON aget2 sext5;\n",
    "  EMO2 ON aget2 sext5;\n\n"
  ),
  input_syntax_M22b,
  fixed = TRUE
)

input_syntax_M22b <- sub(
  "  d_emo ON c3 (dm_c3);\n\n",
  paste0(
    "  d_emo ON c3 (dm_c3);\n",
    "  d_ext ON aget2 sext5 followup;\n",
    "  d_emo ON aget2 sext5 followup;\n\n"
  ),
  input_syntax_M22b,
  fixed = TRUE
)

stopifnot(
  grepl(
    "followup",
    sub(
      "(?s).*?NAMES ARE(.*?)USEVARIABLES ARE.*",
      "\\1",
      input_syntax_M22b,
      perl = TRUE
    ),
    fixed = TRUE
  ),
  grepl(
    "CENTER aget2 sext5 followup (GRANDMEAN);",
    input_syntax_M22b,
    fixed = TRUE
  ),
  grepl(
    "d_ext ON aget2 sext5 followup;",
    input_syntax_M22b,
    fixed = TRUE
  )
)


#-------------------------------------------------------------------------
##### SAVE, RUN, AND COPY MODEL #####
#-------------------------------------------------------------------------

input_file_M22b <- file.path(
  mplus_input_dir,
  paste0(model_M22b_name, ".inp")
)

output_file_M22b <- paste0(
  tools::file_path_sans_ext(input_file_M22b),
  ".out"
)

github_input_file_M22b <- file.path(
  github_m18_lcs_dir,
  basename(input_file_M22b)
)

github_output_file_M22b <- file.path(
  github_m18_lcs_dir,
  basename(output_file_M22b)
)

run_log_file_M22b <- file.path(
  dirname(input_file_M22b),
  paste0(model_M22b_name, "_run.log")
)

writeLines(
  input_syntax_M22b,
  input_file_M22b
)

copy_file_checked(
  source_file = input_file_M22b,
  target = github_input_file_M22b
)

MplusAutomation::runModels(
  target = input_file_M22b,
  recursive = FALSE,
  showOutput = TRUE,
  replaceOutfile = "always",
  logFile = run_log_file_M22b,
  Mplus_command = mplus_command_M21,
  killOnFail = TRUE,
  quiet = FALSE
)

if (!file.exists(output_file_M22b)) {
  stop("No M22b output was created: ", output_file_M22b)
}

copy_file_checked(
  source_file = output_file_M22b,
  target = github_output_file_M22b
)

results_M22b <- MplusAutomation::readModels(
  output_file_M22b,
  what = c("summaries", "parameters", "warn_err"),
  quiet = TRUE
)

saveRDS(
  results_M22b,
  file.path(
    github_m18_lcs_dir,
    paste0(model_M22b_name, "_full_results.rds")
  )
)

cat(
  "\nM22b completed.\n",
  "Input:  ", input_file_M22b, "\n",
  "Output: ", output_file_M22b, "\n",
  sep = ""
)


#-------------------------------------------------------------------------
##### EXPLORE AGE #######
#-------------------------------------------------------------------------

summary(dat_mplus_final$followup)
sd(dat_mplus_final$followup, na.rm = TRUE)
quantile(
  dat_mplus_final$followup,
  probs = c(.05, .25, .50, .75, .95),
  na.rm = TRUE
)

dat_mplus_final |>
  dplyr::filter(followup > 115) |>
  dplyr::select(
    SIC_N,
    followup,
    aget2,
    aget5m,
    m18_cls
  ) |>
  dplyr::arrange(
    dplyr::desc(followup)
  )
#-------------------------------------------------------------------------
##### EXPLORE SES #######
#-------------------------------------------------------------------------
summary(dat_mplus_final$sesausb)
table(dat_mplus_final$sesausb, useNA = "ifany")

dat_mplus_final |>
  dplyr::filter(
    stat_t5 != 0,
    m18_cls %in% 1:3
  ) |>
  dplyr::summarise(
    n_total = dplyr::n(),
    n_ses = sum(!is.na(sesausb)),
    n_missing_ses = sum(is.na(sesausb)),
    percent_missing = mean(is.na(sesausb)) * 100
  )

dat_mplus_final |>
  dplyr::filter(
    stat_t5 != 0,
    m18_cls %in% 1:3
  ) |>
  dplyr::group_by(m18_cls) |>
  dplyr::summarise(
    n = dplyr::n(),
    n_ses = sum(!is.na(sesausb)),
    n_missing = sum(is.na(sesausb)),
    percent_missing = mean(is.na(sesausb)) * 100,
    mean_ses = mean(sesausb, na.rm = TRUE),
    .groups = "drop"
  )

#-------------------------------------------------------------------------
##### EXPLORE BIO NAMES #####
#-------------------------------------------------------------------------
grep(
  "prs|pri|poly|score|pc[0-9]|hcc|cort|hair|h1_|h2_",
  names(dat_mplus_final),
  value = TRUE,
  ignore.case = TRUE
)
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
##### MODELS M24-M26: PRS AND HAIR CORTISOL #####
#-------------------------------------------------------------------------

required_bio_models <- c(
  "input_syntax_M21",
  "class_variable_M21",
  "mplus_names",
  "mplus_input_dir",
  "github_m18_lcs_dir",
  "mplus_command_M21",
  "copy_file_checked"
)

missing_bio_models <- required_bio_models[
  !vapply(
    required_bio_models,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_bio_models) > 0) {
  stop(
    "Missing required objects: ",
    paste(missing_bio_models, collapse = ", ")
  )
}


#-------------------------------------------------------------------------
##### MODEL SPECIFICATIONS #####
#-------------------------------------------------------------------------

model_specs_bio <- list(
  
  M24a = list(
    name = "M24a_lcs_classes_PRS_EAU",
    title = "M24a: LTC classes plus European-ancestry MDD PRS",
    predictors = c(
      "aget2", "sext5",
      "prs_eau",
      "PC1", "PC2", "PC3", "PC4"
    )
  ),
  
  M24b = list(
    name = "M24b_lcs_classes_PRS_MAU",
    title = "M24b: LTC classes plus multi-ancestry MDD PRS",
    predictors = c(
      "aget2", "sext5",
      "prs_mau",
      "PC1", "PC2", "PC3", "PC4"
    )
  ),
  
  M25 = list(
    name = "M25_lcs_classes_hair_cortisol",
    title = "M25: LTC classes plus proximal-segment hair cortisol",
    predictors = c(
      "aget2", "sext5",
      "c2p1_z"
    )
  ),
  
  M26a = list(
    name = "M26a_lcs_classes_HCC_PRS_EAU",
    title = "M26a: LTC classes plus hair cortisol and European-ancestry MDD PRS",
    predictors = c(
      "aget2", "sext5",
      "c2p1_z",
      "prs_eau",
      "PC1", "PC2", "PC3", "PC4"
    )
  ),
  
  M26b = list(
    name = "M26b_lcs_classes_HCC_PRS_MAU",
    title = "M26b: LTC classes plus hair cortisol and multi-ancestry MDD PRS",
    predictors = c(
      "aget2", "sext5",
      "c2p1_z",
      "prs_mau",
      "PC1", "PC2", "PC3", "PC4"
    )
  )
)


#-------------------------------------------------------------------------
##### CHECK VARIABLE NAMES #####
#-------------------------------------------------------------------------

all_bio_predictors <- unique(
  unlist(
    lapply(
      model_specs_bio,
      `[[`,
      "predictors"
    )
  )
)

missing_bio_predictors <- all_bio_predictors[
  !(tolower(all_bio_predictors) %in% tolower(mplus_names))
]

if (length(missing_bio_predictors) > 0) {
  stop(
    "Missing biological/covariate variables: ",
    paste(missing_bio_predictors, collapse = ", ")
  )
}

# Preserve exact capitalization from the Mplus NAMES list.
resolve_mplus_names <- function(x) {
  mplus_names[
    match(
      tolower(x),
      tolower(mplus_names)
    )
  ]
}

model_specs_bio <- lapply(
  model_specs_bio,
  function(spec) {
    spec$predictors <- resolve_mplus_names(
      spec$predictors
    )
    spec
  }
)


#-------------------------------------------------------------------------
##### FUNCTION: CREATE INPUT #####
#-------------------------------------------------------------------------

create_bio_input <- function(spec) {
  
  predictors <- spec$predictors
  predictors_string <- paste(
    predictors,
    collapse = " "
  )
  
  syntax <- input_syntax_M21
  
  syntax <- sub(
    "M21: Bivariate latent change score model with LTC classes",
    spec$title,
    syntax,
    fixed = TRUE
  )
  
  syntax <- sub(
    "Classes predict baseline levels and latent changes;",
    "Classes and biological predictors predict baseline levels and latent changes;",
    syntax,
    fixed = TRUE
  )
  
  syntax <- sub(
    "    c2 c3;\n\n",
    paste0(
      "    ",
      predictors_string,
      " c2 c3;\n\n"
    ),
    syntax,
    fixed = TRUE
  )
  
  syntax <- sub(
    paste0(
      "  IF (",
      class_variable_M21,
      " EQ 3) THEN c3 = 1;\n\n"
    ),
    paste0(
      "  IF (",
      class_variable_M21,
      " EQ 3) THEN c3 = 1;\n",
      "  CENTER ",
      predictors_string,
      " (GRANDMEAN);\n\n"
    ),
    syntax,
    fixed = TRUE
  )
  
  syntax <- sub(
    "  EMO2 ON c3 (m2_c3);\n\n",
    paste0(
      "  EMO2 ON c3 (m2_c3);\n",
      "  EXT2 ON ",
      predictors_string,
      ";\n",
      "  EMO2 ON ",
      predictors_string,
      ";\n\n"
    ),
    syntax,
    fixed = TRUE
  )
  
  syntax <- sub(
    "  d_emo ON c3 (dm_c3);\n\n",
    paste0(
      "  d_emo ON c3 (dm_c3);\n",
      "  d_ext ON ",
      predictors_string,
      ";\n",
      "  d_emo ON ",
      predictors_string,
      ";\n\n"
    ),
    syntax,
    fixed = TRUE
  )
  
  stopifnot(
    grepl(
      paste0(
        "CENTER ",
        predictors_string,
        " (GRANDMEAN);"
      ),
      syntax,
      fixed = TRUE
    ),
    grepl(
      paste0(
        "d_ext ON ",
        predictors_string,
        ";"
      ),
      syntax,
      fixed = TRUE
    )
  )
  
  syntax
}


#-------------------------------------------------------------------------
##### FUNCTION: RUN AND EXTRACT MODEL #####
#-------------------------------------------------------------------------

run_bio_model <- function(spec) {
  
  input_syntax <- create_bio_input(spec)
  
  input_file <- file.path(
    mplus_input_dir,
    paste0(spec$name, ".inp")
  )
  
  output_file <- paste0(
    tools::file_path_sans_ext(input_file),
    ".out"
  )
  
  github_input_file <- file.path(
    github_m18_lcs_dir,
    basename(input_file)
  )
  
  github_output_file <- file.path(
    github_m18_lcs_dir,
    basename(output_file)
  )
  
  run_log_file <- file.path(
    dirname(input_file),
    paste0(spec$name, "_run.log")
  )
  
  results_rds_file <- file.path(
    github_m18_lcs_dir,
    paste0(spec$name, "_full_results.rds")
  )
  
  writeLines(
    input_syntax,
    input_file
  )
  
  copy_file_checked(
    source_file = input_file,
    target = github_input_file
  )
  
  MplusAutomation::runModels(
    target = input_file,
    recursive = FALSE,
    showOutput = TRUE,
    replaceOutfile = "always",
    logFile = run_log_file,
    Mplus_command = mplus_command_M21,
    killOnFail = TRUE,
    quiet = FALSE
  )
  
  if (!file.exists(output_file)) {
    stop(
      "No output was created for ",
      spec$name,
      ": ",
      output_file
    )
  }
  
  results <- MplusAutomation::readModels(
    output_file,
    what = c(
      "summaries",
      "parameters",
      "warn_err"
    ),
    quiet = TRUE
  )
  
  if (length(results$errors) > 0) {
    stop(
      spec$name,
      " contains Mplus errors:\n",
      paste(
        unlist(results$errors),
        collapse = "\n"
      )
    )
  }
  
  copy_file_checked(
    source_file = output_file,
    target = github_output_file
  )
  
  saveRDS(
    results,
    results_rds_file
  )
  
  parameters <- tibble::as_tibble(
    results$parameters$unstandardized
  )
  
  central_effects <- parameters |>
    dplyr::filter(
      toupper(.data$paramHeader) %in% c(
        "EXT2 ON",
        "EMO2 ON",
        "D_EXT ON",
        "D_EMO ON"
      ),
      toupper(.data$param) %in% toupper(
        c(
          "c2",
          "c3",
          spec$predictors
        )
      )
    )
  
  cat(
    "\n============================================================",
    "\n", spec$name,
    "\n============================================================\n",
    sep = ""
  )
  
  print(
    tibble::as_tibble(results$summaries),
    n = Inf,
    width = Inf
  )
  
  cat("\nCENTRAL EFFECTS\n")
  
  print(
    central_effects,
    n = Inf,
    width = Inf
  )
  
  list(
    specification = spec,
    input_file = input_file,
    output_file = output_file,
    github_input_file = github_input_file,
    github_output_file = github_output_file,
    results_rds_file = results_rds_file,
    results = results,
    central_effects = central_effects
  )
}


#-------------------------------------------------------------------------
##### RUN ALL MODELS #####
#-------------------------------------------------------------------------

results_bio_models <- lapply(
  model_specs_bio,
  run_bio_model
)

saveRDS(
  results_bio_models,
  file.path(
    github_m18_lcs_dir,
    "M24_M26_biological_models_all_results.rds"
  )
)

cat(
  "\nAll M24-M26 biological models completed successfully.\n"
)

#-------------------------------------------------------------------------
##### TABLE: LTC CLASSES AND PSYCHOPATHOLOGY #####
#-------------------------------------------------------------------------

word_output <- file.path(
  mplus_results_dir_maltreatment,
  "Table_LTC_classes_psychopathology_M21_M22.docx"
)

#-------------------------------------------------------------------------
##### FIND FINAL OUTPUTS #####
#-------------------------------------------------------------------------

m21_out <- file.path(
  mplus_input_dir,
  "m21_lcs_with_ltc_classes.out"
)

m22_out <- file.path(
  mplus_input_dir,
  "m22_lcs_with_ltc_classes_aget2_sex.out"
)


#-------------------------------------------------------------------------
##### READ MPLUS OUTPUTS #####
#-------------------------------------------------------------------------

m21 <- MplusAutomation::readModels(
  m21_out,
  what = c("summaries", "parameters", "warn_err"),
  quiet = TRUE
)

m22 <- MplusAutomation::readModels(
  m22_out,
  what = c("summaries", "parameters", "warn_err"),
  quiet = TRUE
)

if (length(m21$errors) > 0) {
  stop("M21 contains Mplus errors.")
}

if (length(m22$errors) > 0) {
  stop("M22 contains Mplus errors.")
}


#-------------------------------------------------------------------------
##### EXTRACT MODEL-CONSTRAINT PARAMETERS #####
#-------------------------------------------------------------------------

parameter_key <- tribble(
  ~parameter, ~Outcome, ~Contrast, ~outcome_order, ~contrast_order,
  "EX2_C2", "Baseline externalizing problems",
  "Elevated and declining vs low and stable", 1, 1,
  "EX2_C3", "Baseline externalizing problems",
  "High early burden with later rebound vs low and stable", 1, 2,
  "EX_3V2", "Baseline externalizing problems",
  "High early burden with later rebound vs elevated and declining", 1, 3,
  
  "EM2_C2", "Baseline emotional problems",
  "Elevated and declining vs low and stable", 2, 1,
  "EM2_C3", "Baseline emotional problems",
  "High early burden with later rebound vs low and stable", 2, 2,
  "EM_3V2", "Baseline emotional problems",
  "High early burden with later rebound vs elevated and declining", 2, 3,
  
  "DEX_C2", "Change in externalizing problems",
  "Elevated and declining vs low and stable", 3, 1,
  "DEX_C3", "Change in externalizing problems",
  "High early burden with later rebound vs low and stable", 3, 2,
  "DX_3V2", "Change in externalizing problems",
  "High early burden with later rebound vs elevated and declining", 3, 3,
  
  "DEM_C2", "Change in emotional problems",
  "Elevated and declining vs low and stable", 4, 1,
  "DEM_C3", "Change in emotional problems",
  "High early burden with later rebound vs low and stable", 4, 2,
  "DM_3V2", "Change in emotional problems",
  "High early burden with later rebound vs elevated and declining", 4, 3
)

extract_contrasts <- function(model, model_label) {
  
  pars <- as_tibble(
    model$parameters$unstandardized
  )
  
  names(pars) <- tolower(names(pars))
  
  estimate_column <- intersect(
    c("est", "estimate"),
    names(pars)
  )[1]
  
  se_column <- intersect(
    c("se", "std.error", "stderr"),
    names(pars)
  )[1]
  
  p_column <- intersect(
    c("pval", "pvalue", "p"),
    names(pars)
  )[1]
  
  if (any(is.na(c(
    estimate_column,
    se_column,
    p_column
  )))) {
    stop(
      "Could not identify estimate, SE, or p-value columns in ",
      model_label,
      ". Available columns: ",
      paste(names(pars), collapse = ", ")
    )
  }
  
  pars |>
    mutate(
      param_upper = toupper(.data$param)
    ) |>
    filter(
      param_upper %in% parameter_key$parameter
    ) |>
    transmute(
      parameter = param_upper,
      Model = model_label,
      b = .data[[estimate_column]],
      SE = .data[[se_column]],
      p = .data[[p_column]],
      CI_low = b - 1.96 * SE,
      CI_high = b + 1.96 * SE
    ) |>
    left_join(
      parameter_key,
      by = "parameter"
    )
}

results_long <- bind_rows(
  extract_contrasts(m21, "M21: Unadjusted"),
  extract_contrasts(m22, "M22: Adjusted")
)

missing_parameters <- setdiff(
  parameter_key$parameter,
  unique(results_long$parameter)
)

if (length(missing_parameters) > 0) {
  stop(
    "These expected parameters were not found: ",
    paste(missing_parameters, collapse = ", ")
  )
}


#-------------------------------------------------------------------------
##### FORMAT VALUES #####
#-------------------------------------------------------------------------

format_p <- function(x) {
  case_when(
    is.na(x) ~ "",
    x < .001 ~ "< .001",
    TRUE ~ sub(
      "^0",
      "",
      sprintf("%.3f", x)
    )
  )
}

format_b_ci <- function(b, low, high) {
  sprintf(
    "%.2f [%.2f, %.2f]",
    b,
    low,
    high
  )
}

table_data <- results_long |>
  mutate(
    estimate = format_b_ci(
      b,
      CI_low,
      CI_high
    ),
    p_formatted = format_p(p)
  ) |>
  select(
    Outcome,
    Contrast,
    outcome_order,
    contrast_order,
    Model,
    estimate,
    p_formatted
  ) |>
  tidyr::pivot_wider(
    names_from = Model,
    values_from = c(
      estimate,
      p_formatted
    )
  ) |>
  arrange(
    outcome_order,
    contrast_order
  ) |>
  select(
    Outcome,
    Contrast,
    `M21, b [95% CI]` =
      `estimate_M21: Unadjusted`,
    `p` =
      `p_formatted_M21: Unadjusted`,
    `M22, b [95% CI]` =
      `estimate_M22: Adjusted`,
    `p_adjusted` =
      `p_formatted_M22: Adjusted`
  )

names(table_data)[
  names(table_data) == "p_adjusted"
] <- "p "


#-------------------------------------------------------------------------
##### APA-STYLE FLEXTABLE #####
#-------------------------------------------------------------------------

ft <- flextable(table_data)

ft <- ft |>
  set_header_labels(
    Outcome = "Outcome",
    Contrast = "Class contrast",
    `M21, b [95% CI]` = "M21, b [95% CI]",
    p = "p",
    `M22, b [95% CI]` = "M22, b [95% CI]",
    `p ` = "p"
  ) |>
  merge_v(j = "Outcome") |>
  valign(
    j = "Outcome",
    valign = "top"
  ) |>
  font(
    fontname = "Times New Roman",
    part = "all"
  ) |>
  fontsize(
    size = 10,
    part = "all"
  ) |>
  bold(
    part = "header"
  ) |>
  align(
    j = c(
      "Outcome",
      "Contrast"
    ),
    align = "left",
    part = "all"
  ) |>
  align(
    j = 3:6,
    align = "center",
    part = "all"
  ) |>
  padding(
    padding.top = 3,
    padding.bottom = 3,
    padding.left = 3,
    padding.right = 3,
    part = "all"
  ) |>
  border_remove()

apa_line <- officer::fp_border(
  color = "000000",
  width = 1
)

thin_line <- officer::fp_border(
  color = "000000",
  width = 0.5
)

ft <- ft |>
  hline_top(
    part = "header",
    border = apa_line
  ) |>
  hline(
    i = 1,
    part = "header",
    border = thin_line
  ) |>
  hline_bottom(
    part = "body",
    border = apa_line
  ) |>
  width(
    j = "Outcome",
    width = 1.55
  ) |>
  width(
    j = "Contrast",
    width = 2.85
  ) |>
  width(
    j = c(
      "M21, b [95% CI]",
      "M22, b [95% CI]"
    ),
    width = 1.35
  ) |>
  width(
    j = c(
      "p",
      "p "
    ),
    width = 0.55
  ) |>
  set_table_properties(
    layout = "fixed",
    width = 1
  )


#-------------------------------------------------------------------------
##### MODEL FIT FOR TABLE NOTE #####
#-------------------------------------------------------------------------

get_summary_value <- function(summary_object, candidates) {
  
  summary_df <- as.data.frame(summary_object)
  names_lower <- tolower(names(summary_df))
  
  match_index <- match(
    tolower(candidates),
    names_lower
  )
  
  match_index <- match_index[
    !is.na(match_index)
  ][1]
  
  if (is.na(match_index)) {
    return(NA_real_)
  }
  
  as.numeric(summary_df[[match_index]][1])
}

m21_n <- get_summary_value(
  m21$summaries,
  c("Observations", "NObservations")
)

m22_n <- get_summary_value(
  m22$summaries,
  c("Observations", "NObservations")
)

note_text <- paste0(
  "Note. Unstandardized coefficients are reported. ",
  "Confidence intervals are Wald 95% confidence intervals calculated as b ± 1.96 SE. ",
  "M21 was unadjusted (N = ",
  m21_n,
  "); M22 was adjusted for age at baseline and sex (N = ",
  m22_n,
  "). ",
  "Low and stable was the reference class unless the contrast explicitly compared ",
  "high early burden with later rebound with elevated and declining."
)


#-------------------------------------------------------------------------
##### WRITE WORD DOCUMENT #####
#-------------------------------------------------------------------------

doc <- read_docx() |>
  body_add_par(
    "Table 11",
    style = "Normal"
  ) |>
  body_add_par(
    "Associations of Maltreatment Trajectory Classes With Baseline Psychopathology and Latent Change",
    style = "Normal"
  ) |>
  body_add_flextable(ft) |>
  body_add_par(
    note_text,
    style = "Normal"
  )

doc <- doc |>
  body_set_default_section(
    prop_section(
      page_size = page_size(
        orient = "landscape"
      ),
      page_margins = page_mar(
        top = 0.7,
        bottom = 0.7,
        left = 0.6,
        right = 0.6
      )
    )
  )

print(
  doc,
  target = word_output
)

message(
  "Word table saved to:\n",
  word_output
)

#-------------------------------------------------------------------------
##### TABLE 12: BIOLOGICAL PREDICTORS #####
#-------------------------------------------------------------------------

required_objects <- c(
  "mplus_input_dir",
  "mplus_results_dir_biology"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  stop(
    "Run the project setup first. Missing objects: ",
    paste(missing_objects, collapse = ", ")
  )
}


#-------------------------------------------------------------------------
##### FILE PATHS #####
#-------------------------------------------------------------------------

m24a_out <- file.path(
  mplus_input_dir,
  "m24a_lcs_classes_prs_eau.out"
)

m25_out <- file.path(
  mplus_input_dir,
  "m25_lcs_classes_hair_cortisol.out"
)

m26a_out <- file.path(
  mplus_input_dir,
  "m26a_lcs_classes_hcc_prs_eau.out"
)

output_dir <- file.path(
  mplus_results_dir_biology,
  "tables"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

word_output <- file.path(
  output_dir,
  "Table_2_biological_predictors.docx"
)

csv_output <- file.path(
  output_dir,
  "Table_2_biological_predictors.csv"
)

for (file in c(
  m24a_out,
  m25_out,
  m26a_out
)) {
  if (!file.exists(file)) {
    stop(
      "Mplus output not found:\n",
      file
    )
  }
}


#-------------------------------------------------------------------------
##### READ MPLUS OUTPUTS #####
#-------------------------------------------------------------------------

read_mplus_output <- function(path) {
  
  model <- MplusAutomation::readModels(
    path,
    what = c(
      "summaries",
      "parameters",
      "warn_err"
    ),
    quiet = TRUE
  )
  
  if (length(model$errors) > 0) {
    stop(
      "Mplus errors found in:\n",
      path,
      "\n",
      paste(
        unlist(model$errors),
        collapse = "\n"
      )
    )
  }
  
  model
}

m24a <- read_mplus_output(m24a_out)
m25 <- read_mplus_output(m25_out)
m26a <- read_mplus_output(m26a_out)


#-------------------------------------------------------------------------
##### HELPERS #####
#-------------------------------------------------------------------------

get_model_n <- function(model) {
  
  summary_data <- as.data.frame(
    model$summaries
  )
  
  possible_names <- c(
    "Observations",
    "NObservations"
  )
  
  selected_name <- possible_names[
    possible_names %in% names(summary_data)
  ][1]
  
  if (is.na(selected_name)) {
    return(NA_integer_)
  }
  
  as.integer(
    summary_data[[selected_name]][1]
  )
}

extract_effects <- function(
    model,
    predictor,
    model_name
) {
  
  parameters <- tibble::as_tibble(
    model$parameters$unstandardized
  )
  
  names(parameters) <- tolower(
    names(parameters)
  )
  
  estimate_column <- intersect(
    c("est", "estimate"),
    names(parameters)
  )[1]
  
  se_column <- intersect(
    c("se", "stderr", "std.error"),
    names(parameters)
  )[1]
  
  p_column <- intersect(
    c("pval", "pvalue", "p"),
    names(parameters)
  )[1]
  
  if (any(is.na(c(
    estimate_column,
    se_column,
    p_column
  )))) {
    stop(
      "Could not identify estimate, SE, or p-value columns in ",
      model_name,
      "."
    )
  }
  
  outcome_key <- tibble::tribble(
    ~header_clean, ~Outcome,                           ~outcome_order,
    "EXT2ON",      "Baseline externalizing problems", 1,
    "EMO2ON",      "Baseline emotional problems",     2,
    "DEXTON",      "Change in externalizing problems", 3,
    "DEMOON",      "Change in emotional problems",    4
  )
  
  extracted <- parameters |>
    dplyr::mutate(
      header_clean = toupper(
        gsub(
          "[^A-Za-z0-9]+",
          "",
          trimws(as.character(.data$paramheader))
        )
      ),
      parameter_clean = toupper(
        trimws(as.character(.data$param))
      )
    ) |>
    dplyr::filter(
      .data$parameter_clean == toupper(predictor),
      .data$header_clean %in% c(
        "EXT2ON",
        "EMO2ON",
        "DEXTON",
        "DEMOON"
      )
    ) |>
    dplyr::transmute(
      header_clean = .data$header_clean,
      Model = model_name,
      Predictor = predictor,
      b = .data[[estimate_column]],
      SE = .data[[se_column]],
      p = .data[[p_column]],
      CI_low = .data$b - 1.96 * .data$SE,
      CI_high = .data$b + 1.96 * .data$SE
    ) |>
    dplyr::left_join(
      outcome_key,
      by = "header_clean"
    )
  
  if (nrow(extracted) != 4) {
    stop(
      "Expected four effects for ",
      predictor,
      " in ",
      model_name,
      " but found ",
      nrow(extracted),
      "."
    )
  }
  
  extracted
}

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "—",
    p < .001 ~ "< .001",
    TRUE ~ sub(
      "^0",
      "",
      sprintf(
        "%.3f",
        p
      )
    )
  )
}

format_estimate_ci <- function(
    b,
    lower,
    upper
) {
  sprintf(
    "%.2f [%.2f, %.2f]",
    b,
    lower,
    upper
  )
}


#-------------------------------------------------------------------------
##### EXTRACT EFFECTS #####
#-------------------------------------------------------------------------

n_m24a <- get_model_n(m24a)
n_m25 <- get_model_n(m25)
n_m26a <- get_model_n(m26a)

prs_separate <- extract_effects(
  model = m24a,
  predictor = "prs_eau",
  model_name = "Separate"
) |>
  dplyr::mutate(
    Biological_predictor =
      "Depression-associated European-ancestry PRI",
    predictor_order = 1
  )

hcc_separate <- extract_effects(
  model = m25,
  predictor = "c2p1_z",
  model_name = "Separate"
) |>
  dplyr::mutate(
    Biological_predictor =
      "Hair cortisol",
    predictor_order = 2
  )

prs_joint <- extract_effects(
  model = m26a,
  predictor = "prs_eau",
  model_name = "Joint"
) |>
  dplyr::mutate(
    Biological_predictor =
      "Depression-associated European-ancestry PRI",
    predictor_order = 1
  )

hcc_joint <- extract_effects(
  model = m26a,
  predictor = "c2p1_z",
  model_name = "Joint"
) |>
  dplyr::mutate(
    Biological_predictor =
      "Hair cortisol",
    predictor_order = 2
  )

results_long <- dplyr::bind_rows(
  prs_separate,
  hcc_separate,
  prs_joint,
  hcc_joint
)


#-------------------------------------------------------------------------
##### BUILD TABLE DATA #####
#-------------------------------------------------------------------------

table_data <- results_long |>
  dplyr::mutate(
    estimate_ci = format_estimate_ci(
      .data$b,
      .data$CI_low,
      .data$CI_high
    ),
    p_formatted = format_p(
      .data$p
    )
  ) |>
  dplyr::select(
    .data$Biological_predictor,
    .data$Outcome,
    .data$predictor_order,
    .data$outcome_order,
    .data$Model,
    .data$estimate_ci,
    .data$p_formatted
  ) |>
  tidyr::pivot_wider(
    names_from = .data$Model,
    values_from = c(
      .data$estimate_ci,
      .data$p_formatted
    )
  ) |>
  dplyr::arrange(
    .data$predictor_order,
    .data$outcome_order
  ) |>
  dplyr::transmute(
    `Biological predictor` =
      .data$Biological_predictor,
    Outcome =
      .data$Outcome,
    `Separate model, b [95% CI]` =
      .data$estimate_ci_Separate,
    `p` =
      .data$p_formatted_Separate,
    `Joint model, b [95% CI]` =
      .data$estimate_ci_Joint,
    `p ` =
      .data$p_formatted_Joint
  )

utils::write.csv(
  table_data,
  csv_output,
  row.names = FALSE,
  na = ""
)


#-------------------------------------------------------------------------
##### FORMAT APA TABLE #####
#-------------------------------------------------------------------------

ft <- flextable::flextable(
  table_data
) |>
  flextable::merge_v(
    j = "Biological predictor"
  ) |>
  flextable::valign(
    j = "Biological predictor",
    valign = "top"
  ) |>
  flextable::font(
    fontname = "Times New Roman",
    part = "all"
  ) |>
  flextable::fontsize(
    size = 10,
    part = "all"
  ) |>
  flextable::bold(
    part = "header"
  ) |>
  flextable::align(
    j = c(
      "Biological predictor",
      "Outcome"
    ),
    align = "left",
    part = "all"
  ) |>
  flextable::align(
    j = 3:6,
    align = "center",
    part = "all"
  ) |>
  flextable::padding(
    padding.top = 3,
    padding.bottom = 3,
    padding.left = 3,
    padding.right = 3,
    part = "all"
  ) |>
  flextable::border_remove()

apa_border <- officer::fp_border(
  color = "000000",
  width = 1
)

thin_border <- officer::fp_border(
  color = "000000",
  width = 0.5
)

ft <- ft |>
  flextable::hline_top(
    part = "header",
    border = apa_border
  ) |>
  flextable::hline(
    i = 1,
    part = "header",
    border = thin_border
  ) |>
  flextable::hline_bottom(
    part = "body",
    border = apa_border
  ) |>
  flextable::width(
    j = "Biological predictor",
    width = 2.25
  ) |>
  flextable::width(
    j = "Outcome",
    width = 2.05
  ) |>
  flextable::width(
    j = c(
      "Separate model, b [95% CI]",
      "Joint model, b [95% CI]"
    ),
    width = 1.45
  ) |>
  flextable::width(
    j = c(
      "p",
      "p "
    ),
    width = 0.55
  ) |>
  flextable::set_table_properties(
    layout = "fixed",
    width = 1
  )


#-------------------------------------------------------------------------
##### TABLE NOTE #####
#-------------------------------------------------------------------------

note_text <- paste0(
  "Note. Unstandardized coefficients are reported. ",
  "Confidence intervals are Wald 95% confidence intervals. ",
  "Separate models included either the depression-associated ",
  "European-ancestry polygenic risk index (PRI; N = ",
  n_m24a,
  ") or proximal-segment hair cortisol (N = ",
  n_m25,
  "). The joint model included both biological predictors (N = ",
  n_m26a,
  "). All models were adjusted for maltreatment trajectory class, ",
  "age at baseline, and sex. Models containing the PRI were additionally ",
  "adjusted for the first four genetic principal components. ",
  "Hair cortisol was assessed from the proximal 3-cm hair segment."
)


#-------------------------------------------------------------------------
##### WRITE WORD DOCUMENT #####
#-------------------------------------------------------------------------

doc <- officer::read_docx() |>
  officer::body_add_par(
    "Table 2",
    style = "Normal"
  ) |>
  officer::body_add_par(
    paste0(
      "Associations of the Depression-Associated Polygenic Risk Index ",
      "and Hair Cortisol With Baseline Psychopathology and Latent Change"
    ),
    style = "Normal"
  ) |>
  flextable::body_add_flextable(
    value = ft
  ) |>
  officer::body_add_par(
    note_text,
    style = "Normal"
  ) |>
  officer::body_set_default_section(
    officer::prop_section(
      page_size = officer::page_size(
        orient = "landscape"
      ),
      page_margins = officer::page_mar(
        top = 0.7,
        bottom = 0.7,
        left = 0.6,
        right = 0.6
      )
    )
  )

print(
  doc,
  target = word_output
)

cat(
  "\nTable created successfully.\n",
  "Word: ", word_output, "\n",
  "CSV:  ", csv_output, "\n",
  sep = ""
)
#-------------------------------------------------------------------------
##### APPENDIX TABLES: MODEL FIT AND DESCRIPTIVE STATISTICS #####
#-------------------------------------------------------------------------

required_objects <- c(
  "mplus_input_dir",
  "mplus_results_dir",
  "dat_mplus_final"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  stop(
    "Run the project setup and load dat_mplus_final first. Missing: ",
    paste(missing_objects, collapse = ", ")
  )
}


#-------------------------------------------------------------------------
##### OUTPUT DIRECTORY #####
#-------------------------------------------------------------------------

appendix_dir <- file.path(
  mplus_results_dir,
  "06_Appendix"
)

dir.create(
  appendix_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

word_output <- file.path(
  appendix_dir,
  "Appendix_model_fit_and_descriptives.docx"
)

fit_csv_output <- file.path(
  appendix_dir,
  "Table_A1_model_fit.csv"
)

descriptive_csv_output <- file.path(
  appendix_dir,
  "Table_A2_descriptive_statistics.csv"
)


#-------------------------------------------------------------------------
##### MPLUS OUTPUT FILES #####
#-------------------------------------------------------------------------

model_files <- tibble::tribble(
  ~Model, ~Description, ~File,
  "M21",
  "LTC classes, unadjusted",
  file.path(
    mplus_input_dir,
    "m21_lcs_with_ltc_classes.out"
  ),
  "M22",
  "LTC classes, adjusted for baseline age and sex",
  file.path(
    mplus_input_dir,
    "m22_lcs_with_ltc_classes_aget2_sex.out"
  ),
  "M24a",
  "Depression-associated European-ancestry PRI",
  file.path(
    mplus_input_dir,
    "m24a_lcs_classes_prs_eau.out"
  ),
  "M25",
  "Proximal-segment hair cortisol",
  file.path(
    mplus_input_dir,
    "m25_lcs_classes_hair_cortisol.out"
  ),
  "M26a",
  "Depression-associated PRI and hair cortisol",
  file.path(
    mplus_input_dir,
    "m26a_lcs_classes_hcc_prs_eau.out"
  )
)

missing_model_files <- model_files |>
  dplyr::filter(
    !file.exists(.data$File)
  )

if (nrow(missing_model_files) > 0) {
  stop(
    "Mplus output files not found:\n",
    paste(
      missing_model_files$File,
      collapse = "\n"
    )
  )
}


#-------------------------------------------------------------------------
##### HELPERS #####
#-------------------------------------------------------------------------

read_mplus_output <- function(path) {
  MplusAutomation::readModels(
    path,
    what = c(
      "summaries",
      "warn_err"
    ),
    quiet = TRUE
  )
}

get_first_value <- function(
    summary_data,
    candidates
) {
  
  summary_data <- as.data.frame(
    summary_data
  )
  
  available <- candidates[
    candidates %in% names(summary_data)
  ]
  
  if (length(available) == 0) {
    return(NA_real_)
  }
  
  as.numeric(
    summary_data[[available[1]]][1]
  )
}

format_p_value_local <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "—",
    p < .001 ~ "< .001",
    TRUE ~ sub(
      "^0",
      "",
      sprintf("%.3f", p)
    )
  )
}

format_number <- function(
    x,
    digits = 3
) {
  ifelse(
    is.na(x),
    "—",
    formatC(
      x,
      format = "f",
      digits = digits
    )
  )
}

format_rmsea <- function(
    rmsea,
    lower,
    upper
) {
  
  if (
    any(is.na(c(
      rmsea,
      lower,
      upper
    )))
  ) {
    return("—")
  }
  
  paste0(
    format_number(
      rmsea,
      3
    ),
    " [",
    format_number(
      lower,
      3
    ),
    ", ",
    format_number(
      upper,
      3
    ),
    "]"
  )
}


#-------------------------------------------------------------------------
##### TABLE A1: MODEL FIT #####
#-------------------------------------------------------------------------

fit_data <- purrr::pmap_dfr(
  model_files,
  function(
    Model,
    Description,
    File
  ) {
    
    model <- read_mplus_output(
      File
    )
    
    summaries <- model$summaries
    
    n <- get_first_value(
      summaries,
      c(
        "Observations",
        "NObservations"
      )
    )
    
    chisq <- get_first_value(
      summaries,
      c(
        "ChiSqM_Value",
        "ChiSqMValue"
      )
    )
    
    df <- get_first_value(
      summaries,
      c(
        "ChiSqM_DF",
        "ChiSqMDF"
      )
    )
    
    p_chisq <- get_first_value(
      summaries,
      c(
        "ChiSqM_PValue",
        "ChiSqMPValue"
      )
    )
    
    cfi <- get_first_value(
      summaries,
      "CFI"
    )
    
    tli <- get_first_value(
      summaries,
      "TLI"
    )
    
    rmsea <- get_first_value(
      summaries,
      "RMSEA_Estimate"
    )
    
    rmsea_low <- get_first_value(
      summaries,
      c(
        "RMSEA_90CI_LB",
        "RMSEA_90CI_Lower"
      )
    )
    
    rmsea_high <- get_first_value(
      summaries,
      c(
        "RMSEA_90CI_UB",
        "RMSEA_90CI_Upper"
      )
    )
    
    srmr <- get_first_value(
      summaries,
      "SRMR"
    )
    
    tibble::tibble(
      Model = Model,
      Description = Description,
      N = as.integer(n),
      `χ² (df)` = ifelse(
        is.na(chisq) | is.na(df),
        "—",
        sprintf(
          "%.2f (%d)",
          chisq,
          as.integer(df)
        )
      ),
      `p` = format_p_value_local(
        p_chisq
      ),
      CFI = format_number(
        cfi,
        3
      ),
      TLI = format_number(
        tli,
        3
      ),
      `RMSEA [90% CI]` = format_rmsea(
        rmsea,
        rmsea_low,
        rmsea_high
      ),
      SRMR = format_number(
        srmr,
        3
      )
    )
  }
)

utils::write.csv(
  fit_data,
  fit_csv_output,
  row.names = FALSE,
  na = ""
)


#-------------------------------------------------------------------------
##### TABLE A2: DESCRIPTIVE STATISTICS #####
#-------------------------------------------------------------------------

analysis_data <- dat_mplus_final |>
  dplyr::filter(
    .data$stat_t5 != 0,
    .data$m18_cls %in% 1:3
  ) |>
  dplyr::mutate(
    age_t2_years = .data$aget2 / 12
  )

continuous_variables <- tibble::tribble(
  ~variable, ~Label,
  "age_t2_years", "Age at baseline, years",
  "prs_eau", "Depression-associated European-ancestry PRI",
  "c2p1_z", "Hair cortisol, proximal 3-cm segment"
)

continuous_descriptives <- purrr::pmap_dfr(
  continuous_variables,
  function(
    variable,
    Label
  ) {
    
    x <- analysis_data[[variable]]
    
    tibble::tibble(
      Variable = Label,
      Category = "",
      N = sum(
        !is.na(x)
      ),
      Missing = sum(
        is.na(x)
      ),
      `M (SD)` = ifelse(
        all(is.na(x)),
        "—",
        sprintf(
          "%.2f (%.2f)",
          mean(
            x,
            na.rm = TRUE
          ),
          stats::sd(
            x,
            na.rm = TRUE
          )
        )
      ),
      Range = ifelse(
        all(is.na(x)),
        "—",
        sprintf(
          "%.2f–%.2f",
          min(
            x,
            na.rm = TRUE
          ),
          max(
            x,
            na.rm = TRUE
          )
        )
      ),
      `n (%)` = ""
    )
  }
)

categorical_descriptives <- dplyr::bind_rows(
  
  analysis_data |>
    dplyr::count(
      .data$sext5,
      .drop = FALSE
    ) |>
    dplyr::mutate(
      Variable = "Sex",
      Category = dplyr::case_when(
        .data$sext5 == 0 ~ "Male",
        .data$sext5 == 1 ~ "Female",
        is.na(.data$sext5) ~ "Missing",
        TRUE ~ as.character(
          .data$sext5
        )
      ),
      N = nrow(
        analysis_data
      ) - sum(
        is.na(
          analysis_data$sext5
        )
      ),
      Missing = sum(
        is.na(
          analysis_data$sext5
        )
      ),
      `M (SD)` = "",
      Range = "",
      `n (%)` = sprintf(
        "%d (%.1f)",
        .data$n,
        100 * .data$n /
          nrow(
            analysis_data
          )
      )
    ) |>
    dplyr::select(
      Variable,
      Category,
      N,
      Missing,
      `M (SD)`,
      Range,
      `n (%)`
    ),
  
  analysis_data |>
    dplyr::count(
      .data$m18_cls,
      .drop = FALSE
    ) |>
    dplyr::mutate(
      Variable = "Maltreatment trajectory class",
      Category = dplyr::case_when(
        .data$m18_cls == 1 ~ "Low and stable",
        .data$m18_cls == 2 ~ "Elevated and declining",
        .data$m18_cls == 3 ~ "High early burden with later rebound",
        TRUE ~ "Missing"
      ),
      N = nrow(
        analysis_data
      ),
      Missing = 0L,
      `M (SD)` = "",
      Range = "",
      `n (%)` = sprintf(
        "%d (%.1f)",
        .data$n,
        100 * .data$n /
          nrow(
            analysis_data
          )
      )
    ) |>
    dplyr::select(
      .data$Variable,
      .data$Category,
      .data$N,
      .data$Missing,
      `M (SD)`,
      .data$Range,
      `n (%)`
    )
)

descriptive_data <- dplyr::bind_rows(
  continuous_descriptives,
  categorical_descriptives
)

utils::write.csv(
  descriptive_data,
  descriptive_csv_output,
  row.names = FALSE,
  na = ""
)


#-------------------------------------------------------------------------
##### APA 7 TABLE A2 FORMATTING #####
#-------------------------------------------------------------------------

apa_rule <- officer::fp_border(
  color = "#000000",
  width = 1.5,
  style = "single"
)


#-------------------------------------------------------------------------
##### BASIC TABLE FORMATTING #####
#-------------------------------------------------------------------------

format_apa_table <- function(
    data,
    left_columns,
    widths = NULL
) {
  
  ft <- flextable::flextable(
    data
  ) |>
    flextable::font(
      fontname = "Times New Roman",
      part = "all"
    ) |>
    flextable::fontsize(
      size = 10,
      part = "all"
    ) |>
    flextable::bold(
      part = "header"
    ) |>
    flextable::align(
      j = left_columns,
      align = "left",
      part = "all"
    ) |>
    flextable::align(
      j = setdiff(
        names(data),
        left_columns
      ),
      align = "center",
      part = "all"
    ) |>
    flextable::valign(
      valign = "center",
      part = "all"
    ) |>
    flextable::padding(
      padding.top = 3,
      padding.bottom = 3,
      padding.left = 3,
      padding.right = 3,
      part = "all"
    ) |>
    flextable::line_spacing(
      space = 1,
      part = "all"
    ) |>
    flextable::border_remove() |>
    flextable::set_table_properties(
      layout = "fixed",
      width = 1,
      align = "left"
    )
  
  if (!is.null(widths)) {
    
    for (column in names(widths)) {
      
      ft <- flextable::width(
        ft,
        j = column,
        width = widths[[column]]
      )
    }
  }
  
  ft
}


#-------------------------------------------------------------------------
##### CREATE FLEXTABLES #####
#-------------------------------------------------------------------------

fit_ft <- format_apa_table(
  fit_data,
  left_columns = c(
    "Model",
    "Description"
  ),
  widths = c(
    Model = 0.55,
    Description = 2.65,
    N = 0.55,
    `χ² (df)` = 1.05,
    p = 0.55,
    CFI = 0.55,
    TLI = 0.55,
    `RMSEA [90% CI]` = 1.35,
    SRMR = 0.60
  )
)

descriptive_ft <- format_apa_table(
  descriptive_data,
  left_columns = c(
    "Variable",
    "Category"
  ),
  widths = c(
    Variable = 2.65,
    Category = 2.30,
    N = 0.55,
    Missing = 0.70,
    `M (SD)` = 1.05,
    Range = 1.05,
    `n (%)` = 0.90
  )
) |>
  flextable::merge_v(
    j = "Variable"
  ) |>
  flextable::valign(
    j = "Variable",
    valign = "top"
  )


#-------------------------------------------------------------------------
##### FINAL APA TABLE LINES #####
#-------------------------------------------------------------------------

apply_final_apa_rules <- function(ft) {
  
  # Remove all existing borders first.
  ft <- flextable::border_remove(
    ft
  )
  
  # Repair borders created by vertically merged cells.
  ft <- flextable::fix_border_issues(
    ft
  )
  
  # Thick solid line above the header.
  ft <- flextable::hline_top(
    ft,
    part = "header",
    border = apa_rule
  )
  
  # Thick solid line below the header.
  ft <- flextable::hline_bottom(
    ft,
    part = "header",
    border = apa_rule
  )
  
  # Thick solid line below the final body row.
  ft <- flextable::hline_bottom(
    ft,
    part = "body",
    border = apa_rule
  )
  
  # Keep the complete table flush left.
  ft <- flextable::set_table_properties(
    ft,
    layout = "fixed",
    width = 1,
    align = "left"
  )
  
  ft
}

fit_ft <- apply_final_apa_rules(
  fit_ft
)

descriptive_ft <- apply_final_apa_rules(
  descriptive_ft
)


#-------------------------------------------------------------------------
##### WORD TITLE AND NOTE FORMATTING #####
#-------------------------------------------------------------------------

table_number_style <- officer::fp_text(
  font.family = "Times New Roman",
  font.size = 12,
  bold = TRUE
)

table_title_style <- officer::fp_text(
  font.family = "Times New Roman",
  font.size = 12,
  italic = TRUE
)

table_note_style <- officer::fp_text(
  font.family = "Times New Roman",
  font.size = 10
)

table_number_paragraph <- function(text) {
  
  officer::fpar(
    officer::ftext(
      text,
      table_number_style
    ),
    fp_p = officer::fp_par(
      text.align = "left",
      line_spacing = 2,
      padding = 0
    )
  )
}

table_title_paragraph <- function(text) {
  
  officer::fpar(
    officer::ftext(
      text,
      table_title_style
    ),
    fp_p = officer::fp_par(
      text.align = "left",
      line_spacing = 2,
      padding = 0
    )
  )
}

table_note_paragraph <- function(text) {
  
  officer::fpar(
    officer::ftext(
      text,
      table_note_style
    ),
    fp_p = officer::fp_par(
      text.align = "left",
      line_spacing = 1,
      padding = 0
    )
  )
}


#-------------------------------------------------------------------------
##### TABLE NOTES #####
#-------------------------------------------------------------------------

fit_note <- paste0(
  "Note. M21 was unadjusted. M22 was adjusted for baseline age and gender. ",
  "M24a additionally included the depression-associated European-ancestry ",
  "polygenic risk index and the first four genetic principal components. ",
  "M25 additionally included proximal-segment hair cortisol. M26a jointly ",
  "included the polygenic risk index, genetic principal components, and hair cortisol. ",
  "RMSEA confidence intervals are 90% confidence intervals."
)

descriptive_note <- paste0(
  "Note. Descriptive statistics are based on the primary LTC analysis sample ",
  "with participation at T2 and T5 and valid class assignment (N = ",
  nrow(
    analysis_data
  ),
  "). Percentages use the full primary analysis sample as denominator. ",
  "The PRI and hair cortisol variables were entered in the models using the ",
  "scales shown here."
)


#-------------------------------------------------------------------------
##### WRITE WORD DOCUMENT #####
#-------------------------------------------------------------------------

doc <- officer::read_docx() |>
  officer::body_add_fpar(
    table_number_paragraph(
      "Table A1"
    )
  ) |>
  officer::body_add_fpar(
    table_title_paragraph(
      paste0(
        "Fit Indices and Sample Sizes of the Reported ",
        "Latent Change Score Models"
      )
    )
  ) |>
  flextable::body_add_flextable(
    value = fit_ft
  ) |>
  officer::body_add_fpar(
    table_note_paragraph(
      fit_note
    )
  ) |>
  officer::body_add_break() |>
  officer::body_add_fpar(
    table_number_paragraph(
      "Table A2"
    )
  ) |>
  officer::body_add_fpar(
    table_title_paragraph(
      paste0(
        "Descriptive Statistics for Biological Predictors ",
        "and Model Covariates"
      )
    )
  ) |>
  flextable::body_add_flextable(
    value = descriptive_ft
  ) |>
  officer::body_add_fpar(
    table_note_paragraph(
      descriptive_note
    )
  ) |>
  officer::body_set_default_section(
    officer::prop_section(
      page_size = officer::page_size(
        orient = "landscape"
      ),
      page_margins = officer::page_mar(
        top = 0.7,
        bottom = 0.7,
        left = 0.65,
        right = 0.65
      )
    )
  )

print(
  doc,
  target = word_output
)

cat(
  "\nAppendix tables created successfully.\n",
  "Word: ", word_output, "\n",
  "Table A1 CSV: ", fit_csv_output, "\n",
  "Table A2 CSV: ", descriptive_csv_output, "\n",
  sep = ""
)
#-------------------------------------------------------------------------
##### FIGURES: ADJUSTED PSYCHOPATHOLOGY FROM T2 TO T5 BY LTC CLASS #####
#-------------------------------------------------------------------------

# Run the project setup before this script.
#
# Creates two separate manuscript-ready figures:
#   1. Externalizing problems from T2 to T5
#   2. Emotional problems from T2 to T5
#
# Each figure contains one line per maltreatment trajectory class.
# Estimates come from the age- and gender-adjusted M22 model.
#
# Figure numbers and italicized titles should be added in the manuscript,
# not embedded inside the graph image.

required_objects <- c(
  "mplus_input_dir",
  "mplus_results_dir_maltreatment"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  stop(
    "Run the project setup first. Missing objects: ",
    paste(missing_objects, collapse = ", ")
  )
}


#-------------------------------------------------------------------------
##### PATHS #####
#-------------------------------------------------------------------------

m22_out <- file.path(
  mplus_input_dir,
  "m22_lcs_with_ltc_classes_aget2_sex.out"
)

if (!file.exists(m22_out)) {
  stop(
    "M22 output not found:\n",
    m22_out
  )
}

figure_dir <- file.path(
  mplus_results_dir_maltreatment,
  "figures"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

externalizing_png <- file.path(
  figure_dir,
  "Figure_M22_externalizing_T2_T5_by_class.png"
)

externalizing_pdf <- file.path(
  figure_dir,
  "Figure_M22_externalizing_T2_T5_by_class.pdf"
)

emotional_png <- file.path(
  figure_dir,
  "Figure_M22_emotional_T2_T5_by_class.png"
)

emotional_pdf <- file.path(
  figure_dir,
  "Figure_M22_emotional_T2_T5_by_class.pdf"
)


#-------------------------------------------------------------------------
##### READ MPLUS OUTPUT #####
#-------------------------------------------------------------------------

m22 <- MplusAutomation::readModels(
  m22_out,
  what = c(
    "parameters",
    "summaries",
    "warn_err"
  ),
  quiet = TRUE
)

parameters <- tibble::as_tibble(
  m22$parameters$unstandardized
)

names(parameters) <- tolower(
  names(parameters)
)

parameters <- parameters |>
  dplyr::mutate(
    header_clean = toupper(
      gsub(
        "[^A-Za-z0-9]+",
        "",
        trimws(
          as.character(
            .data$paramheader
          )
        )
      )
    ),
    param_clean = toupper(
      gsub(
        "[^A-Za-z0-9]+",
        "",
        trimws(
          as.character(
            .data$param
          )
        )
      )
    )
  )


#-------------------------------------------------------------------------
##### EXTRACT CLASS-SPECIFIC BASELINE AND CHANGE ESTIMATES #####
#-------------------------------------------------------------------------

parameter_key <- tibble::tribble(
  ~Outcome, ~Class, ~Baseline_parameter, ~Change_parameter,
  
  "Externalizing problems",
  "Low and stable",
  "EX2_C1",
  "DEX_C1",
  
  "Externalizing problems",
  "Elevated and declining",
  "EX2_C2",
  "DEX_C2",
  
  "Externalizing problems",
  "High early burden with later rebound",
  "EX2_C3",
  "DEX_C3",
  
  "Emotional problems",
  "Low and stable",
  "EM2_C1",
  "DEM_C1",
  
  "Emotional problems",
  "Elevated and declining",
  "EM2_C2",
  "DEM_C2",
  
  "Emotional problems",
  "High early burden with later rebound",
  "EM2_C3",
  "DEM_C3"
)

new_parameters <- parameters |>
  dplyr::filter(
    grepl(
      "NEW|ADDITIONAL",
      .data$header_clean
    )
  )

clean_parameter_name <- function(x) {
  toupper(
    gsub(
      "[^A-Za-z0-9]+",
      "",
      x
    )
  )
}

extract_one_estimate <- function(parameter_name) {
  
  parameter_name_clean <- clean_parameter_name(
    parameter_name
  )
  
  matched <- new_parameters |>
    dplyr::filter(
      .data$param_clean == parameter_name_clean
    )
  
  if (nrow(matched) != 1) {
    stop(
      "Expected exactly one parameter named ",
      parameter_name,
      " but found ",
      nrow(matched),
      "."
    )
  }
  
  matched$est[1]
}

class_estimates <- parameter_key |>
  dplyr::rowwise() |>
  dplyr::mutate(
    Baseline = extract_one_estimate(
      .data$Baseline_parameter
    ),
    Change = extract_one_estimate(
      .data$Change_parameter
    ),
    Follow_up = .data$Baseline + .data$Change
  ) |>
  dplyr::ungroup()


#-------------------------------------------------------------------------
##### LONG FORMAT #####
#-------------------------------------------------------------------------

plot_data <- class_estimates |>
  dplyr::select(
    .data$Outcome,
    .data$Class,
    T2 = .data$Baseline,
    T5 = .data$Follow_up
  ) |>
  tidyr::pivot_longer(
    cols = c(
      "T2",
      "T5"
    ),
    names_to = "Time",
    values_to = "Estimate"
  ) |>
  dplyr::mutate(
    Time = factor(
      .data$Time,
      levels = c(
        "T2",
        "T5"
      )
    ),
    Class = factor(
      .data$Class,
      levels = c(
        "Low and stable",
        "Elevated and declining",
        "High early burden with later rebound"
      )
    )
  )


#-------------------------------------------------------------------------
##### VISUAL DESIGN #####
#-------------------------------------------------------------------------

# A qualitative palette is used because the lines represent discrete
# trajectory classes rather than a continuous numeric scale.
class_colours <- c(
  "Low and stable" = "#1B9E77",
  "Elevated and declining" = "#D95F02",
  "High early burden with later rebound" = "#355C9A"
)

class_linetypes <- c(
  "Low and stable" = "solid",
  "Elevated and declining" = "longdash",
  "High early burden with later rebound" = "dotdash"
)

class_shapes <- c(
  "Low and stable" = 16,
  "Elevated and declining" = 17,
  "High early burden with later rebound" = 15
)


#-------------------------------------------------------------------------
##### CREATE TWO-PANEL FIGURE #####
#-------------------------------------------------------------------------


y_min <- floor(
  min(
    plot_data$Estimate,
    na.rm = TRUE
  ) * 2
) / 2

y_max <- ceiling(
  max(
    plot_data$Estimate,
    na.rm = TRUE
  ) * 2
) / 2

combined_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = .data$Time,
    y = .data$Estimate,
    group = interaction(
      .data$Outcome,
      .data$Class
    ),
    colour = .data$Class,
    linetype = .data$Class,
    shape = .data$Class
  )
) +
  
  # Three class-specific lines in each panel.
  ggplot2::geom_line(
    linewidth = 1.75,
    lineend = "round"
  ) +
  
  ggplot2::geom_point(
    size = 4.2,
    stroke = 0.95
  ) +
  
  # Two panels next to each other.
  ggplot2::facet_wrap(
    facets = ggplot2::vars(
      Outcome
    ),
    nrow = 1,
    scales = "fixed",
    labeller = ggplot2::labeller(
      Outcome = c(
        "Externalizing problems" = "EXTERNALIZING PROBLEMS",
        "Emotional problems" = "EMOTIONAL PROBLEMS"
      )
    )
  ) +
  
  ggplot2::scale_colour_manual(
    values = class_colours
  ) +
  
  ggplot2::scale_linetype_manual(
    values = class_linetypes
  ) +
  
  ggplot2::scale_shape_manual(
    values = class_shapes
  ) +
  
  ggplot2::scale_x_discrete(
    labels = c(
      "T2" = "BASELINE (T2)",
      "T5" = "FOLLOW-UP (T5)"
    ),
    expand = ggplot2::expansion(
      mult = c(
        0.12,
        0.12
      )
    )
  ) +
  
  ggplot2::scale_y_continuous(
    limits = c(
      y_min,
      y_max
    ),
    breaks = seq(
      y_min,
      y_max,
      by = 0.5
    ),
    expand = ggplot2::expansion(
      mult = c(
        0.04,
        0.06
      )
    )
  ) +
  
  ggplot2::labs(
    x = "ASSESSMENT",
    y = "ADJUSTED LATENT SCORE",
    colour = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      nrow = 1,
      byrow = TRUE
    ),
    linetype = ggplot2::guide_legend(
      nrow = 1,
      byrow = TRUE
    ),
    shape = ggplot2::guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  ) +
  
  ggplot2::theme_classic(
    base_family = "Arial",
    base_size = 12
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank(),
    
    # Larger uppercase axis titles.
    axis.title.x = ggplot2::element_text(
      size = 15,
      face = "bold",
      colour = "black",
      margin = ggplot2::margin(
        t = 12
      )
    ),
    
    axis.title.y = ggplot2::element_text(
      size = 15,
      face = "bold",
      colour = "black",
      margin = ggplot2::margin(
        r = 12
      )
    ),
    
    # Larger axis labels.
    axis.text.x = ggplot2::element_text(
      size = 12,
      face = "bold",
      colour = "black"
    ),
    
    axis.text.y = ggplot2::element_text(
      size = 13,
      colour = "black"
    ),
    
    axis.line = ggplot2::element_line(
      colour = "black",
      linewidth = 0.75
    ),
    
    axis.ticks = ggplot2::element_line(
      colour = "black",
      linewidth = 0.65
    ),
    
    axis.ticks.length = grid::unit(
      2.5,
      "mm"
    ),
    
    # Panel headings.
    strip.background = ggplot2::element_blank(),
    
    strip.text = ggplot2::element_text(
      size = 14,
      face = "bold",
      colour = "black",
      margin = ggplot2::margin(
        b = 10
      )
    ),
    
    # Space between the two panels.
    panel.spacing = grid::unit(
      1.5,
      "cm"
    ),
    
    # Subtle horizontal grid lines.
    panel.grid.major.y = ggplot2::element_line(
      colour = "grey88",
      linewidth = 0.40
    ),
    
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    
    # Shared legend above both panels.
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_blank(),
    
    legend.text = ggplot2::element_text(
      size = 10.5
    ),
    
    legend.key.width = grid::unit(
      1.8,
      "cm"
    ),
    
    legend.key.height = grid::unit(
      0.55,
      "cm"
    ),
    
    plot.margin = ggplot2::margin(
      t = 10,
      r = 14,
      b = 10,
      l = 10
    )
  )


#-------------------------------------------------------------------------
##### SAVE TWO-PANEL FIGURE #####
#-------------------------------------------------------------------------

combined_png <- file.path(
  figure_dir,
  "Figure_M22_T2_T5_by_class_two_panel.png"
)

combined_pdf <- file.path(
  figure_dir,
  "Figure_M22_T2_T5_by_class_two_panel.pdf"
)

ggplot2::ggsave(
  filename = combined_png,
  plot = combined_plot,
  width = 12,
  height = 5.8,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = combined_pdf,
  plot = combined_plot,
  width = 12,
  height = 5.8,
  units = "in",
  device = grDevices::cairo_pdf
)

print(
  combined_plot
)

cat(
  "\nCombined two-panel figure created successfully.\n",
  "PNG: ", combined_png, "\n",
  "PDF: ", combined_pdf, "\n",
  sep = ""
)
#-------------------------------------------------------------------------
##### Combined figure workflow for the MAIN OUTCOME PAPER #####
#-------------------------------------------------------------------------
# Run the project setup first.

required_packages <- c(
  "MplusAutomation", "dplyr", "tidyr", "tibble",
  "purrr", "ggplot2", "ggrepel"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop("Install first: ", paste(missing_packages, collapse = ", "))
}

required_objects <- c("mplus_input_dir", "mplus_results_dir_maltreatment")
missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1), inherits = TRUE)
]

if (length(missing_objects) > 0) {
  stop("Run the project setup first. Missing: ", paste(missing_objects, collapse = ", "))
}

m22_out <- file.path(mplus_input_dir, "m22_lcs_with_ltc_classes_aget2_sex.out")
m24a_out <- file.path(mplus_input_dir, "m24a_lcs_classes_prs_eau.out")
m25_out <- file.path(mplus_input_dir, "m25_lcs_classes_hair_cortisol.out")

required_files <- c(m22_out, m24a_out, m25_out)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing Mplus outputs:\n", paste(missing_files, collapse = "\n"))
}

figure_dir <- file.path(mplus_results_dir_maltreatment, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

clean_text <- function(x) {
  toupper(gsub("[^A-Za-z0-9]+", "", trimws(as.character(x))))
}

read_unstandardized_parameters <- function(path) {
  model <- MplusAutomation::readModels(
    path,
    what = c("parameters", "summaries", "warn_err"),
    quiet = TRUE
  )
  parameters <- tibble::as_tibble(model$parameters$unstandardized)
  names(parameters) <- tolower(names(parameters))
  parameters |>
    dplyr::mutate(
      header_clean = clean_text(.data$paramheader),
      param_clean = clean_text(.data$param)
    )
}

extract_new_parameter <- function(parameters, parameter_name) {
  matched <- parameters |>
    dplyr::filter(
      grepl("NEW|ADDITIONAL", .data$header_clean),
      .data$param_clean == clean_text(parameter_name)
    )
  if (nrow(matched) != 1) {
    stop("Expected exactly one new parameter named ", parameter_name,
         " but found ", nrow(matched), ".")
  }
  matched |>
    dplyr::slice(1) |>
    dplyr::transmute(estimate = .data$est, se = .data$se, p = .data$pval)
}

extract_regression <- function(
    parameters,
    header,
    predictor
) {
  
  matched <- parameters |>
    dplyr::filter(
      .data$header_clean == clean_text(header),
      .data$param_clean == clean_text(predictor)
    )
  
  if (nrow(matched) != 1) {
    stop(
      "Expected exactly one regression for ",
      header,
      " / ",
      predictor,
      " but found ",
      nrow(matched),
      ".\nAvailable matching headers: ",
      paste(
        unique(parameters$paramheader),
        collapse = ", "
      )
    )
  }
  
  list(
    estimate = as.numeric(
      matched$est[1]
    ),
    se = as.numeric(
      matched$se[1]
    ),
    p = as.numeric(
      matched$pval[1]
    )
  )
}

save_figure <- function(plot, stem, width, height) {
  png_path <- file.path(figure_dir, paste0(stem, ".png"))
  pdf_path <- file.path(figure_dir, paste0(stem, ".pdf"))
  ggplot2::ggsave(png_path, plot, width = width, height = height,
                  units = "in", dpi = 600, bg = "white")
  ggplot2::ggsave(pdf_path, plot, width = width, height = height,
                  units = "in", device = grDevices::cairo_pdf)
  c(PNG = png_path, PDF = pdf_path)
}

class_colours <- c(
  "Low and stable" = "#1B9E77",
  "Elevated and declining" = "#D95F02",
  "High early burden with later rebound" = "#355C9A"
)

class_linetypes <- c(
  "Low and stable" = "solid",
  "Elevated and declining" = "longdash",
  "High early burden with later rebound" = "dotdash"
)

class_shapes <- c(
  "Low and stable" = 16,
  "Elevated and declining" = 17,
  "High early burden with later rebound" = 15
)

base_theme <- ggplot2::theme_classic(base_family = "Arial", base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = ggplot2::element_text(size = 12, face = "bold", colour = "black"),
    axis.text.y = ggplot2::element_text(size = 13, colour = "black"),
    axis.line = ggplot2::element_line(colour = "black", linewidth = 0.75),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.65),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 14, face = "bold", colour = "black",
                                       margin = ggplot2::margin(b = 10)),
    panel.grid.major.y = ggplot2::element_line(colour = "grey88", linewidth = 0.40),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10)
  )

m22_parameters <- read_unstandardized_parameters(m22_out)
m24a_parameters <- read_unstandardized_parameters(m24a_out)
m25_parameters <- read_unstandardized_parameters(m25_out)

# Figure 1: direct-labelled class trajectories
trajectory_key <- tibble::tribble(
  ~Outcome, ~Class, ~Baseline_parameter, ~Change_parameter,
  "Externalizing problems", "Low and stable", "EX2_C1", "DEX_C1",
  "Externalizing problems", "Elevated and declining", "EX2_C2", "DEX_C2",
  "Externalizing problems", "High early burden with later rebound", "EX2_C3", "DEX_C3",
  "Emotional problems", "Low and stable", "EM2_C1", "DEM_C1",
  "Emotional problems", "Elevated and declining", "EM2_C2", "DEM_C2",
  "Emotional problems", "High early burden with later rebound", "EM2_C3", "DEM_C3"
)

trajectory_estimates <- purrr::pmap_dfr(
  trajectory_key,
  function(Outcome, Class, Baseline_parameter, Change_parameter) {
    baseline <- extract_new_parameter(m22_parameters, Baseline_parameter)
    change <- extract_new_parameter(m22_parameters, Change_parameter)
    tibble::tibble(
      Outcome = Outcome,
      Class = Class,
      T2 = baseline$estimate,
      T5 = baseline$estimate + change$estimate
    )
  }
)

trajectory_data <- trajectory_estimates |>
  tidyr::pivot_longer(c("T2", "T5"), names_to = "Time", values_to = "Estimate") |>
  dplyr::mutate(
    Time = factor(.data$Time, levels = c("T2", "T5")),
    Class = factor(.data$Class, levels = names(class_colours)),
    Outcome = factor(.data$Outcome,
                     levels = c("Externalizing problems", "Emotional problems"))
  )

trajectory_labels <- trajectory_data |>
  dplyr::filter(.data$Time == "T5") |>
  dplyr::mutate(
    label = dplyr::recode(
      as.character(.data$Class),
      "High early burden with later rebound" = "High early burden\nwith later rebound"
    )
  )

trajectory_y_min <- floor(min(trajectory_data$Estimate, na.rm = TRUE) * 2) / 2
trajectory_y_max <- ceiling(max(trajectory_data$Estimate, na.rm = TRUE) * 2) / 2

figure_1 <- ggplot2::ggplot(
  trajectory_data,
  ggplot2::aes(
    x = .data$Time,
    y = .data$Estimate,
    group = interaction(.data$Outcome, .data$Class),
    colour = .data$Class,
    linetype = .data$Class,
    shape = .data$Class
  )
) +
  ggplot2::geom_line(linewidth = 1.70, lineend = "round") +
  ggplot2::geom_point(size = 4.0, stroke = 0.90) +
  ggrepel::geom_text_repel(
    data = trajectory_labels,
    ggplot2::aes(label = .data$label),
    direction = "y",
    hjust = 0,
    nudge_x = 0.18,
    size = 3.5,
    fontface = "bold",
    family = "Arial",
    segment.colour = "grey55",
    segment.size = 0.35,
    box.padding = 0.25,
    point.padding = 0.15,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(Outcome),
    nrow = 1,
    scales = "fixed",
    labeller = ggplot2::labeller(
      Outcome = c(
        "Externalizing problems" = "EXTERNALIZING",
        "Emotional problems" = "EMOTIONAL PROBLEMS"
      )
    )
  ) +
  ggplot2::scale_colour_manual(values = class_colours) +
  ggplot2::scale_linetype_manual(values = class_linetypes) +
  ggplot2::scale_shape_manual(values = class_shapes) +
  ggplot2::scale_x_discrete(
    labels = c("T2" = "BASELINE (T2)", "T5" = "FOLLOW-UP (T5)"),
    expand = ggplot2::expansion(add = c(0.10, 0.55))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(trajectory_y_min, trajectory_y_max),
    breaks = seq(trajectory_y_min, trajectory_y_max, by = 0.5),
    expand = ggplot2::expansion(mult = c(0.04, 0.06))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(x = "ASSESSMENT", y = "ADJUSTED LATENT SCORE") +
  base_theme +
  ggplot2::theme(
    legend.position = "none",
    panel.spacing = grid::unit(2.2, "cm")
  )

figure_1_paths <- save_figure(
  figure_1,
  "Figure_1_M22_direct_labelled_trajectories",
  width = 12.5,
  height = 5.8
)

# Figure 2: forest plot of adjusted class contrasts
contrast_key <- tibble::tribble(
  ~Outcome, ~Contrast, ~Header, ~Predictor,
  "Baseline externalizing", "Elevated and declining vs low and stable", "EXT2 ON", "C2",
  "Baseline externalizing", "High early burden with later rebound vs low and stable", "EXT2 ON", "C3",
  "Baseline emotional problems", "Elevated and declining vs low and stable", "EMO2 ON", "C2",
  "Baseline emotional problems", "High early burden with later rebound vs low and stable", "EMO2 ON", "C3",
  "Change in externalizing", "Elevated and declining vs low and stable", "DEXT ON", "C2",
  "Change in externalizing", "High early burden with later rebound vs low and stable", "DEXT ON", "C3",
  "Change in emotional problems", "Elevated and declining vs low and stable", "DEMO ON", "C2",
  "Change in emotional problems", "High early burden with later rebound vs low and stable", "DEMO ON", "C3"
)

forest_data <- purrr::pmap_dfr(
  contrast_key,
  function(
    Outcome,
    Contrast,
    Header,
    Predictor
  ) {
    
    matched <- m22_parameters |>
      dplyr::filter(
        .data$header_clean == clean_text(Header),
        .data$param_clean == clean_text(Predictor)
      )
    
    if (nrow(matched) != 1) {
      stop(
        "Expected exactly one parameter for ",
        Header,
        " / ",
        Predictor,
        " but found ",
        nrow(matched),
        "."
      )
    }
    
    estimate_value <- as.numeric(
      matched$est[1]
    )
    
    se_value <- as.numeric(
      matched$se[1]
    )
    
    p_value <- as.numeric(
      matched$pval[1]
    )
    
    tibble::tibble(
      Outcome = Outcome,
      Contrast = Contrast,
      estimate = estimate_value,
      se = se_value,
      p = p_value,
      lower = estimate_value - 1.96 * se_value,
      upper = estimate_value + 1.96 * se_value
    )
  }
) |>
  dplyr::mutate(
    Outcome = factor(
      .data$Outcome,
      levels = c(
        "Baseline externalizing",
        "Baseline emotional problems",
        "Change in externalizing",
        "Change in emotional problems"
      )
    ),
    Contrast = factor(
      .data$Contrast,
      levels = c(
        "Elevated and declining vs low and stable",
        "High early burden with later rebound vs low and stable"
      )
    )
  )

contrast_colours <- c(
  "Elevated and declining vs low and stable" = "#D95F02",
  "High early burden with later rebound vs low and stable" = "#355C9A"
)

figure_2 <- ggplot2::ggplot(
  forest_data,
  ggplot2::aes(x = .data$estimate, y = .data$Contrast, colour = .data$Contrast)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      linewidth = 0.65, colour = "grey45") +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
    height = 0.16,
    linewidth = 0.85
  ) +
  ggplot2::geom_point(size = 3.6) +
  ggplot2::facet_wrap(ggplot2::vars(Outcome), ncol = 2, scales = "free_x") +
  ggplot2::scale_colour_manual(values = contrast_colours) +
  ggplot2::labs(x = "UNSTANDARDIZED ESTIMATE WITH 95% CI", y = NULL) +
  base_theme +
  ggplot2::theme(
    legend.position = "none",
    axis.text.y = ggplot2::element_text(size = 10.5, colour = "black"),
    panel.spacing = grid::unit(1.3, "cm")
  )

figure_2_paths <- save_figure(
  figure_2,
  "Figure_2_M22_class_contrasts_forest",
  width = 11,
  height = 7.2
)

# Figure A1: continuous PRS associations with baseline outcomes
prs_key <- tibble::tribble(
  ~Outcome, ~Header,
  "Baseline externalizing", "EXT2 ON",
  "Baseline emotional problems", "EMO2 ON"
)

prs_coefficients <- purrr::pmap_dfr(
  prs_key,
  function(Outcome, Header) {
    estimate <- extract_regression(m24a_parameters, Header, "PRS_EAU")
    tibble::tibble(Outcome = Outcome, beta = estimate$estimate,
                   se = estimate$se, p = estimate$p)
  }
)

prs_plot_data <- tidyr::crossing(
  prs_coefficients,
  PRS = seq(-2, 2, by = 0.05)
) |>
  dplyr::mutate(
    Predicted_difference = .data$beta * .data$PRS,
    lower_raw = (.data$beta - 1.96 * .data$se) * .data$PRS,
    upper_raw = (.data$beta + 1.96 * .data$se) * .data$PRS,
    lower = pmin(.data$lower_raw, .data$upper_raw),
    upper = pmax(.data$lower_raw, .data$upper_raw)
  )

figure_a1 <- ggplot2::ggplot(
  prs_plot_data,
  ggplot2::aes(x = .data$PRS, y = .data$Predicted_difference)
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.55, colour = "grey55") +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
    alpha = 0.18,
    fill = "#355C9A"
  ) +
  ggplot2::geom_line(linewidth = 1.50, colour = "#355C9A") +
  ggplot2::facet_wrap(ggplot2::vars(Outcome), nrow = 1, scales = "fixed") +
  ggplot2::scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("-2 SD", "-1 SD", "MEAN", "+1 SD", "+2 SD")
  ) +
  ggplot2::labs(
    x = "EUROPEAN-ANCESTRY DEPRESSION PRI",
    y = "EXPECTED DIFFERENCE IN LATENT SCORE"
  ) +
  base_theme +
  ggplot2::theme(panel.spacing = grid::unit(1.5, "cm"))

figure_a1_paths <- save_figure(
  figure_a1,
  "Figure_A1_PRS_baseline_associations",
  width = 10.5,
  height = 5.4
)

# Figure A2: continuous HCC associations with latent change
hcc_key <- tibble::tribble(
  ~Outcome, ~Header,
  "Change in externalizing", "DEXT ON",
  "Change in emotional problems", "DEMO ON"
)

hcc_coefficients <- purrr::pmap_dfr(
  hcc_key,
  function(Outcome, Header) {
    estimate <- extract_regression(m25_parameters, Header, "C2P1_Z")
    tibble::tibble(Outcome = Outcome, beta = estimate$estimate,
                   se = estimate$se, p = estimate$p)
  }
)

hcc_plot_data <- tidyr::crossing(
  hcc_coefficients,
  HCC = seq(-2, 2, by = 0.05)
) |>
  dplyr::mutate(
    Predicted_difference = .data$beta * .data$HCC,
    lower_raw = (.data$beta - 1.96 * .data$se) * .data$HCC,
    upper_raw = (.data$beta + 1.96 * .data$se) * .data$HCC,
    lower = pmin(.data$lower_raw, .data$upper_raw),
    upper = pmax(.data$lower_raw, .data$upper_raw)
  )

figure_a2 <- ggplot2::ggplot(
  hcc_plot_data,
  ggplot2::aes(x = .data$HCC, y = .data$Predicted_difference)
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.55, colour = "grey55") +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
    alpha = 0.18,
    fill = "#8C510A"
  ) +
  ggplot2::geom_line(linewidth = 1.50, colour = "#8C510A") +
  ggplot2::facet_wrap(ggplot2::vars(Outcome), nrow = 1, scales = "fixed") +
  ggplot2::scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("-2 SD", "-1 SD", "MEAN", "+1 SD", "+2 SD")
  ) +
  ggplot2::labs(
    x = "PROXIMAL-SEGMENT HAIR CORTISOL",
    y = "EXPECTED DIFFERENCE IN LATENT CHANGE"
  ) +
  base_theme +
  ggplot2::theme(panel.spacing = grid::unit(1.5, "cm"))

figure_a2_paths <- save_figure(
  figure_a2,
  "Figure_A2_HCC_change_associations",
  width = 10.5,
  height = 5.4
)

print(figure_1)
print(figure_2)
print(figure_a1)
print(figure_a2)

cat(
  "\nFigures created successfully.\n\n",
  "Figure 1:\n", paste(figure_1_paths, collapse = "\n"),
  "\n\nFigure 2:\n", paste(figure_2_paths, collapse = "\n"),
  "\n\nFigure A1:\n", paste(figure_a1_paths, collapse = "\n"),
  "\n\nFigure A2:\n", paste(figure_a2_paths, collapse = "\n"),
  "\n",
  sep = ""
)
