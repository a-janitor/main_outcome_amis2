###############################################################################
# PROJECT: AMIS-II MAIN OUTCOME PAPER
# SCRIPT: 01_create_raw_final_dataset.R
# PURPOSE: Create or document the final raw dataset for the main outcome paper
###############################################################################

### ------------------------------------------------------------------------ ###
### 0. SETUP
### ------------------------------------------------------------------------ ###

# This script assumes that 00_setup_paths_packages.R defines:
# - data_dir
# - raw_data_dir
# - analysis_data_dir
# - raw_data_file
# - analysis_data_file
# - mplus_data_file

source("R/01_data_preprocessing/00_setup_paths_packages.R")
source("R/01_data_preprocessing/01_create_functions.R")

### ------------------------------------------------------------------------ ###
### 1. PURPOSE OF THIS SCRIPT
### ------------------------------------------------------------------------ ###

# The goal of the AMIS main outcome preprocessing workflow is to work from:
#
#   several raw dataset
#   ->
#   one final analysis dataset

### ------------------------------------------------------------------------ ###
### 2. CHECK WHETHER FINAL RAW DATA FOLDER EXISTS
### ------------------------------------------------------------------------ ###

if (!dir.exists(raw_data_dir)) {
  dir.create(raw_data_dir, recursive = TRUE)
  message("Created final raw data folder: ", raw_data_dir)
} else {
  message("Final raw data folder exists: ", raw_data_dir)
}

### ------------------------------------------------------------------------ ###
### 3. CHECK WHETHER FINAL RAW DATASET ALREADY EXISTS
### ------------------------------------------------------------------------ ###

if (file.exists(raw_data_file)) {
  
  message("Final raw dataset found:")
  message(raw_data_file)
  
  raw_final <- readRDS(raw_data_file)
  
  message("Number of rows: ", nrow(raw_final))
  message("Number of columns: ", ncol(raw_final))
  
  message("Variable names:")
  print(names(raw_final))
  
} else {
  
  message("Final raw dataset not found yet:")
  message(raw_data_file)
  
  message("Next step: create this file from the source datasets or place the final raw dataset in the folder.")
  
}

### ------------------------------------------------------------------------ ###
### 4. IMPORT SOURCE DATASETS
### ------------------------------------------------------------------------ ###

### ------------------------------------------------------------------------ ###
### 4a. LIFE T1
### ------------------------------------------------------------------------ ###

t1 <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Child Depression T01/data/PV0880_datajoin.xlsx"
  )
))


### ------------------------------------------------------------------------ ###
### 4b. AMIS I
### ------------------------------------------------------------------------ ###

a1 <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "AMIS 1/data/PV0880_datajoin.xlsx"
  )
))

a1t <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "AMIS 1/data/PV0880_T00579_NODUP.xlsx"
  )
))

diag <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Klinische Diagnosen/klin_diag.xlsx"
  )
))


### ------------------------------------------------------------------------ ###
### 4c. AMIS II
### ------------------------------------------------------------------------ ###

a2 <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "AMIS 2/data/PV0880_datajoin.xlsx"
  )
))


### ------------------------------------------------------------------------ ###
### 4d. UEBERBRUECKUNGSSTUDIE / T3
### ------------------------------------------------------------------------ ###

# LIFE YOUTH
uj <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Child Depression T03/data/PV0880_T01095_NODUP.xlsx"
  )
))

# LIFE CAREGIVER
ue <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Child Depression T03/data/PV0880_T01094.xlsx"
  )
))

# JA & DFG KG
uja <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Child Depression T03/data/PV0880_T01096_2025-09-30.xlsx"
  )
)) # CAREGIVER Child Protective Services ("Jugendamt")

ujakg <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Child Depression T03/data/PV0880_T01097_2025-09-30.xlsx"
  )
)) # CAREGIVER CPS control group

udfg <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Child Depression T03/data/PV0880_T01098_2025-09-30.xlsx"
  )
)) # CAREGIVER DFG


### ------------------------------------------------------------------------ ###
### 4e. CORTISOL
### ------------------------------------------------------------------------ ###

hp1 <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Cortisol/PV0880_T00558_NODUP.xlsx"
  )
))

hp2 <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Cortisol/PV0880_T01376_NODUP.xlsx"
  )
))

c1 <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Cortisol/PV0880_T01708.xlsx"
  )
))

c2 <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Cortisol/PV0880_T01709_NODUP.xlsx"
  )
))

### ------------------------------------------------------------------------ ###
### 4f. POLYGENIC RISK SCORES
### ------------------------------------------------------------------------ ###

prs <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "PRIs/PV0880_polygenic_risk_indices_2026-03-20.xlsx"
  )
))


### ------------------------------------------------------------------------ ###
### 4g. MALTREATMENT DATA
### ------------------------------------------------------------------------ ###

micm <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Maltreatment Daten/PV0880_Maltreatment_Derivat_MICM_AMIS_2_(T05).xlsx"
  )
))

mcs <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Maltreatment Daten/PV0880_Maltreatment_Derivat_JA_MCS_AMIS_2_(T05).xlsx"
  )
))

cicm <- as.data.frame(read_excel(
  file.path(
    raw_data_dir,
    "Maltreatment Daten/PV0880_Maltreatment_Derivat_CICM_AMIS_2_(T05).xlsx"
  )
))


### ------------------------------------------------------------------------ ###
### 4h. CREATE LIST OF RAW SOURCE DATASETS
### ------------------------------------------------------------------------ ###

raw_sources <- list(
  t1 = t1,
  a1 = a1,
  a1t = a1t,
  diag = diag,
  a2 = a2,
  uj = uj,
  ue = ue,
  uja = uja,
  ujakg = ujakg,
  udfg = udfg,
  hp1 = hp1,
  hp2 = hp2,
  c1 = c1,
  c2 = c2,
  prs = prs,
  micm = micm,
  mcs = mcs,
  cicm = cicm
)


### ------------------------------------------------------------------------ ###
### 4i. BASIC IMPORT CHECKS
### ------------------------------------------------------------------------ ###

raw_source_overview <- tibble::tibble(
  dataset = names(raw_sources),
  n_rows = purrr::map_int(raw_sources, nrow),
  n_cols = purrr::map_int(raw_sources, ncol)
)

print(raw_source_overview)

### ------------------------------------------------------------------------ ###
### 6. CLEAN LIFE T1: BASELINE DATA
### ------------------------------------------------------------------------ ###

# Reduce variables and harmonize variable names.

t1 <- t1 %>%
  dplyr::select(
    -matches(
      "CTS|CBCL|START|END|CURATED|CHECKED|DQP|UNTERSUCHER|SGROUP|TEILNEHMER_VERSION|FB|RAHMEN"
    )
  )

# Replace the prefix "SDQ" with "SD", add the suffix "T1" to informant-specific
# variables, and label core participant variables as T1 measures.

names(t1) <- names(t1) |>
  gsub("^SDQ", "SD", x = _) |>
  gsub("(_E_|_K_)", "\\1T1_", x = _) |>
  gsub(
    "^(TEILNEHMER_GESCHLECHT|TEILNEHMER_GEB_JJJJMM|age_K)$",
    "\\1_T1",
    x = _
  )

# Rename SDQ item variables.
# IMPORTANT: rename_sdq_items() must be defined before running this script.

t1 <- rename_sdq_items(t1, prefix = "K", suffix = "T1", type = "k")
t1 <- rename_sdq_items(t1, prefix = "E", suffix = "T1", type = "b")

# Final cleanup T1

names(t1)[names(t1) == "TEILNEHMER_SIC"] <- "SIC"

t1 <- t1 %>%
  dplyr::select(
    SIC,
    dplyr::matches("^(consid|restles|somatic|shares|tantrum|loner|obeys|worries|caring|fidgety|friend|fights|unhappy|popular|distrac|clingy|kind|lies|bullied|helpout|reflect|steals|oldbest|afraid|attends)_[bk]_t1$")
  )

names(t1)

### ------------------------------------------------------------------------ ###
### 7. CLEAN AMIS I / T2
### ------------------------------------------------------------------------ ###

# Harmonize ID variable in AMIS-I main data.

names(a1)[names(a1) == "TEILNEHMER_SIC"] <- "SIC"

# Merge teacher data to AMIS-I main data.

a1 <- merge(a1, a1t, by = "SIC", all.x = TRUE)

# Remove non-relevant variables.

a1 <- a1[, !grepl(
  "MNBS|CTSPC|CBCL|CTS|START|END|CURATED|CHECKED|DQP|UNTERSUCHER|SGROUP|TEILNEHMER_VERSION",
  names(a1)
)]

# Harmonize variable names.

names(a1) <- names(a1) |>
  gsub("^SDQ_E", "SD_B", x = _) |>   # caregiver
  gsub("^SDQ_K", "SD_K", x = _) |>   # child
  gsub(
    "^(TEILNEHMER_GESCHLECHT|TEILNEHMER_GEB_JJJJMM)$",
    "\\1_T2",
    x = _
  )

names(a1)[names(a1) == "TEILNEHMER_GESCHLECHT_T2"] <- "gender_t2"

# Keep only variables needed for SDQ harmonization and core participant data.

a1 <- a1[
  ,
  grepl(
    "SIC|SD_B_T2_DATUM|SD_|SOSD_T_|gender_t2|TEILNEHMER_GEB_JJJJMM",
    names(a1)
  )
]

# Rename SDQ item variables.
# Note:
# - K = child report
# - B = caregiver report
# - P = parent report
# - T = teacher report
# Teacher items use SOSD_T variables and are handled inside rename_sdq_items().

a1 <- rename_sdq_items(a1, prefix = "K", suffix = "T2", type = "k")
a1 <- rename_sdq_items(a1, prefix = "B", suffix = "T2", type = "b")
a1 <- rename_sdq_items(a1, prefix = "P", suffix = "T2", type = "p")
a1 <- rename_sdq_items(a1, prefix = "T", suffix = "T2", type = "t")

# Drop remaining raw teacher variables and unnecessary SDQ screening variables.

a1 <- a1[, !grepl("SOSD_T|SD_P_|SD_K_T2_SP|SD_B_T2_SP", names(a1))]

# Keep only harmonized variables needed for the final raw dataset.

a1 <- a1 %>%
  dplyr::select(
    SIC,
    dplyr::matches("^gender_t2$"),
    dplyr::matches("^TEILNEHMER_GEB_JJJJMM_T2$"),
    dplyr::matches("^SD_B_T2_DATUM$"),
    dplyr::matches(
      "^(consid|restles|somatic|shares|tantrum|loner|obeys|worries|caring|fidgety|friend|fights|unhappy|popular|distrac|clingy|kind|lies|bullied|helpout|reflect|steals|oldbest|afraid|attends)_[kbpt]_t2$"
    )
  )
names(a1)

### ------------------------------------------------------------------------ ###
### 8. CLEAN DIAGNOSIS DATA
### ------------------------------------------------------------------------ ###

names(diag)[names(diag) == "PSEUDONYM"] <- "SIC"

### ------------------------------------------------------------------------ ###
### 9. CLEAN AMIS UEBERBRUECKUNGSSTUDIE / T3
### ------------------------------------------------------------------------ ###

# Remove file-specific prefixes in column names.

names(uja)   <- sub("^t01096_", "", names(uja))
names(ujakg) <- sub("^t01097_", "", names(ujakg))
names(udfg)  <- sub("^t01098_", "", names(udfg))

# Remove redundant columns.

uja   <- uja[,   names(uja)   != "f0005"]
ujakg <- ujakg[, names(ujakg) != "f0005"]
udfg  <- udfg[,  names(udfg)  != "f0008"]

# Combine caregiver datasets vertically.

combined <- rbind(uja, ujakg, udfg)

combined <- combined %>%
  rename(
    SD_B_T3_1  = f0147, SD_B_T3_2  = f0148, SD_B_T3_3  = f0149,
    SD_B_T3_4  = f0150, SD_B_T3_5  = f0151, SD_B_T3_6  = f0152,
    SD_B_T3_7  = f0153, SD_B_T3_8  = f0154, SD_B_T3_9  = f0155,
    SD_B_T3_10 = f0156, SD_B_T3_11 = f0157, SD_B_T3_12 = f0158,
    SD_B_T3_13 = f0159, SD_B_T3_14 = f0160, SD_B_T3_15 = f0161,
    SD_B_T3_16 = f0162, SD_B_T3_17 = f0163, SD_B_T3_18 = f0164,
    SD_B_T3_19 = f0165, SD_B_T3_20 = f0166, SD_B_T3_21 = f0167,
    SD_B_T3_22 = f0168, SD_B_T3_23 = f0169, SD_B_T3_24 = f0170,
    SD_B_T3_25 = f0171
  )

names(combined)[names(combined) == "edat"] <- "SD_B_T3_DATUM"
names(combined)[names(combined) == "sic"]  <- "SIC"

combined <- combined[, grepl("SIC|SD_B", names(combined))]

# Clean LIFE caregiver T3 data.

ue <- ue %>%
  rename(
    SD_B_T3_1  = PSYEW_E_T3_SD_1,  SD_B_T3_2  = PSYEW_E_T3_SD_2,
    SD_B_T3_3  = PSYEW_E_T3_SD_3,  SD_B_T3_4  = PSYEW_E_T3_SD_4,
    SD_B_T3_5  = PSYEW_E_T3_SD_5,  SD_B_T3_6  = PSYEW_E_T3_SD_6,
    SD_B_T3_7  = PSYEW_E_T3_SD_7,  SD_B_T3_8  = PSYEW_E_T3_SD_8,
    SD_B_T3_9  = PSYEW_E_T3_SD_9,  SD_B_T3_10 = PSYEW_E_T3_SD_10,
    SD_B_T3_11 = PSYEW_E_T3_SD_11, SD_B_T3_12 = PSYEW_E_T3_SD_12,
    SD_B_T3_13 = PSYEW_E_T3_SD_13, SD_B_T3_14 = PSYEW_E_T3_SD_14,
    SD_B_T3_15 = PSYEW_E_T3_SD_15, SD_B_T3_16 = PSYEW_E_T3_SD_16,
    SD_B_T3_17 = PSYEW_E_T3_SD_17, SD_B_T3_18 = PSYEW_E_T3_SD_18,
    SD_B_T3_19 = PSYEW_E_T3_SD_19, SD_B_T3_20 = PSYEW_E_T3_SD_20,
    SD_B_T3_21 = PSYEW_E_T3_SD_21, SD_B_T3_22 = PSYEW_E_T3_SD_22,
    SD_B_T3_23 = PSYEW_E_T3_SD_23, SD_B_T3_24 = PSYEW_E_T3_SD_24,
    SD_B_T3_25 = PSYEW_E_T3_SD_25
  )

names(ue)[names(ue) == "PSYEW_E_T3_DATUM"] <- "SD_B_T3_DATUM"
names(ue)[names(ue) == "PSYEW_E_T3_SIC"]   <- "SIC"

ue <- ue[, grepl("SIC|SD_B_", names(ue))]

# Combine caregiver T3 data.

au <- bind_rows(combined, ue)

# Clean youth T3 data.

uj <- uj %>%
  rename(
    SD_K_T3_1  = PSYEW_J_T3_SD_1,  SD_K_T3_2  = PSYEW_J_T3_SD_2,
    SD_K_T3_3  = PSYEW_J_T3_SD_3,  SD_K_T3_4  = PSYEW_J_T3_SD_4,
    SD_K_T3_5  = PSYEW_J_T3_SD_5,  SD_K_T3_6  = PSYEW_J_T3_SD_6,
    SD_K_T3_7  = PSYEW_J_T3_SD_7,  SD_K_T3_8  = PSYEW_J_T3_SD_8,
    SD_K_T3_9  = PSYEW_J_T3_SD_9,  SD_K_T3_10 = PSYEW_J_T3_SD_10,
    SD_K_T3_11 = PSYEW_J_T3_SD_11, SD_K_T3_12 = PSYEW_J_T3_SD_12,
    SD_K_T3_13 = PSYEW_J_T3_SD_13, SD_K_T3_14 = PSYEW_J_T3_SD_14,
    SD_K_T3_15 = PSYEW_J_T3_SD_15, SD_K_T3_16 = PSYEW_J_T3_SD_16,
    SD_K_T3_17 = PSYEW_J_T3_SD_17, SD_K_T3_18 = PSYEW_J_T3_SD_18,
    SD_K_T3_19 = PSYEW_J_T3_SD_19, SD_K_T3_20 = PSYEW_J_T3_SD_20,
    SD_K_T3_21 = PSYEW_J_T3_SD_21, SD_K_T3_22 = PSYEW_J_T3_SD_22,
    SD_K_T3_23 = PSYEW_J_T3_SD_23, SD_K_T3_24 = PSYEW_J_T3_SD_24,
    SD_K_T3_25 = PSYEW_J_T3_SD_25
  )

names(uj)[names(uj) == "PSYEW_J_T3_EDAT"] <- "SD_K_T3_DATUM"
names(uj)[names(uj) == "PSYEW_J_T3_SIC"]  <- "SIC"

uj <- uj[, grepl("SIC|SD_K_", names(uj))]

# Merge caregiver and youth T3 data.

au <- merge(au, uj, by = "SIC", all.x = TRUE)

# Rename SDQ item variables.

au <- rename_sdq_items(au, prefix = "K", suffix = "T3", type = "k")
au <- rename_sdq_items(au, prefix = "B", suffix = "T3", type = "b")

### ------------------------------------------------------------------------ ###
### 10. CLEAN AMIS II / T5
### ------------------------------------------------------------------------ ###

# Drop unused variables.

a2 <- a2[, !grepl(
  "MNBS|CTSPC|CBCL|CTS|START|END|CURATED|CHECKED|DQP|UNTERSUCHER|SGROUP|TEILNEHMER_VERSION|FA",
  names(a2)
)]

# Harmonize ID variable.

names(a2)[names(a2) == "TEILNEHMER_SIC"] <- "SIC"

# Harmonize variable names.

names(a2) <- names(a2) |>
  gsub(
    "^(TEILNEHMER_GESCHLECHT|TEILNEHMER_GEB_JJJJMM)$",
    "\\1_T5",
    x = _
  )

# Rename SDQ item variables.
# Note:
# - K = child report
# - B = caregiver report
# - P = parent report
# - T = teacher report
# Teacher items use SOSD_T variables and are handled inside rename_sdq_items().

a2 <- rename_sdq_items(a2, prefix = "K", suffix = "T5", type = "k")
a2 <- rename_sdq_items(a2, prefix = "B", suffix = "T5", type = "b")
a2 <- rename_sdq_items(a2, prefix = "P", suffix = "T5", type = "p")
a2 <- rename_sdq_items(a2, prefix = "T", suffix = "T5", type = "t")

# Drop remaining raw teacher variables and unnecessary SDQ screening variables.

a2 <- a2[, !grepl(
  "SOSD_T|SD_P_T5_[SP]|SD_K_T5_SP|SD_B_T5_SP",
  names(a2)
)]

# Keep only harmonized variables needed for the final raw dataset.

a2 <- a2 %>%
  dplyr::select(
    SIC,
    dplyr::matches("^TEILNEHMER_GESCHLECHT_T5$"),
    dplyr::matches("^TEILNEHMER_GEB_JJJJMM_T5$"),
    dplyr::matches("^SD_[BKTP]_T5_DATUM$"),
    dplyr::matches(
      "^(consid|restles|somatic|shares|tantrum|loner|obeys|worries|caring|fidgety|friend|fights|unhappy|popular|distrac|clingy|kind|lies|bullied|helpout|reflect|steals|oldbest|afraid|attends)_[kbpt]_t5$"
    )
  )

### ------------------------------------------------------------------------ ###
### 11. CLEAN CORTISOL AND HAIR PROTOCOL DATA
### ------------------------------------------------------------------------ ###

# Harmonize ID variables.

names(hp1)[names(hp1) == "HP_K_T2_SIC"]  <- "SIC"
names(hp2)[names(hp2) == "HP_K_T5_SIC"]  <- "SIC"
names(c1)[names(c1)   == "CORT_H_SIC"]   <- "SIC"
names(c2)[names(c2)   == "CORT_H_T5_SIC"] <- "SIC"

# Select cortisol variables.

c1 <- c1[, c("SIC", "CORT_H_IA_COR_PG_1", "CORT_H_IA_COR_PG_2", "CORT_H_IA_COR_PG_3")]
c2 <- c2[, c("SIC", "CORT_H_T5_LCMS_COR_PG")]

cf <- merge(c1, c2, by = "SIC", all = TRUE)

# Hair protocol T2.

hp1 <- subset(
  hp1,
  select = c(
    SIC, HP_K_T2_4, HP_K_T2_6,
    HP_K_T2_7B, HP_K_T2_7, HP_K_T2_7A,
    HP_K_T2_8, HP_K_T2_8A, HP_K_T2_8B,
    HP_K_T2_10, HP_K_T2_12, HP_K_T2_13,
    HP_K_T2_14, HP_K_T2_15, HP_K_T2_16
  )
)

names(hp1)[names(hp1) == "HP_K_T2_4"]  <- "hp2_nat_col"
names(hp1)[names(hp1) == "HP_K_T2_6"]  <- "hp2_num_wash"
names(hp1)[names(hp1) == "HP_K_T2_7"]  <- "hp2_dye"
names(hp1)[names(hp1) == "HP_K_T2_8"]  <- "hp2_col_status"
names(hp1)[names(hp1) == "HP_K_T2_10"] <- "hp2_treat_oth"
names(hp1)[names(hp1) == "HP_K_T2_12"] <- "hp2_sport"
names(hp1)[names(hp1) == "HP_K_T2_14"] <- "hp2_act"
names(hp1)[names(hp1) == "HP_K_T2_15"] <- "hp2_sick"
names(hp1)[names(hp1) == "HP_K_T2_16"] <- "hp2_stress"

hp1 <- hp1[, c(grep("SIC|hp2", names(hp1), value = TRUE))]

# Hair protocol T5.

hp2 <- subset(
  hp2,
  select = c(
    SIC, HP_K_T5_7, HP_K_T5_8,
    HP_K_T5_10, HP_K_T5_11, HP_K_T5_12,
    HP_K_T5_13, HP_K_T5_15, HP_K_T5_16,
    HP_K_T5_36, HP_K_T5_38, HP_K_T5_39, HP_K_T5_41
  )
)

names(hp2)[names(hp2) == "HP_K_T5_7"]  <- "hp5_nat_col"
names(hp2)[names(hp2) == "HP_K_T5_8"]  <- "hp5_num_wash"
names(hp2)[names(hp2) == "HP_K_T5_10"] <- "hp5_dye"
names(hp2)[names(hp2) == "HP_K_T5_12"] <- "hp5_col_status"
names(hp2)[names(hp2) == "HP_K_T5_16"] <- "hp5_treat_oth"
names(hp2)[names(hp2) == "HP_K_T5_36"] <- "hp5_sport"
names(hp2)[names(hp2) == "HP_K_T5_38"] <- "hp5_act"
names(hp2)[names(hp2) == "HP_K_T5_39"] <- "hp5_sick"
names(hp2)[names(hp2) == "HP_K_T5_41"] <- "hp5_stress"

hp2 <- hp2[, c(grep("SIC|hp5", names(hp2), value = TRUE))]

# Merge hair protocol data.

hpf <- merge(hp1, hp2, by = "SIC", all = TRUE)

# Reorder hair protocol variables by base names.

base_names <- gsub("^hp[0-9]+_", "", names(hpf))
new_order <- order(base_names, names(hpf))

hpf <- hpf[, c("SIC", names(hpf)[new_order][names(hpf)[new_order] != "SIC"])]

# Merge cortisol and hair protocol data.

cf <- merge(cf, hpf, by = "SIC", all = TRUE)

### ------------------------------------------------------------------------ ###
### 12. CLEAN POLYGENIC RISK SCORES
### ------------------------------------------------------------------------ ###

names(prs)[names(prs) == "PSEUDONYM"] <- "SIC"

### ------------------------------------------------------------------------ ###
### 13. CLEAN MALTREATMENT VARIABLES
### ------------------------------------------------------------------------ ###

# Harmonize ID variable.

micm <- micm %>% rename(TEILNEHMER_SIC = MICM_T5_SIC)
mcs  <- mcs  %>% rename(TEILNEHMER_SIC = JA_MCS_T5_SIC)
cicm <- cicm %>% rename(TEILNEHMER_SIC = CICM_T5_SIC)

# Merge maltreatment datasets, base = MICM.

mal <- micm %>%
  left_join(mcs,  by = "TEILNEHMER_SIC") %>%
  left_join(cicm, by = "TEILNEHMER_SIC")

names(mal)[names(mal) == "TEILNEHMER_SIC"] <- "SIC"
### ------------------------------------------------------------------------ ###
### 14. MERGE CLEANED DATASETS INTO FINAL RAW DATASET
### ------------------------------------------------------------------------ ###

# The final raw dataset is built in wide format with one row per participant.
# AMIS-I / T2 is used as the anchor dataset.

raw_final <- merge(a1, t1, by = "SIC", all.x = TRUE)
raw_final <- merge(raw_final, diag, by = "SIC", all.x = TRUE)
raw_final <- merge(raw_final, au,   by = "SIC", all.x = TRUE)
raw_final <- merge(raw_final, a2,   by = "SIC", all.x = TRUE)
raw_final <- merge(raw_final, cf,   by = "SIC", all.x = TRUE)
raw_final <- merge(raw_final, prs,  by = "SIC", all.x = TRUE)
raw_final <- merge(raw_final, mal,  by = "SIC", all.x = TRUE)

message("Rows: ", nrow(raw_final))
message("Columns: ", ncol(raw_final))
saveRDS(raw_final, raw_data_file)

### ------------------------------------------------------------------------ ###
### 15. CHECK AND SAVE FINAL RAW DATASET
### ------------------------------------------------------------------------ ###

# Check whether SIC is unique.

sic_duplicates <- raw_final %>%
  count(SIC) %>%
  filter(n > 1)

if (nrow(sic_duplicates) > 0) {
  warning("Final raw dataset contains duplicate SIC values. Please inspect sic_duplicates.")
} else {
  message("SIC is unique in final raw dataset.")
}

# Check missing SIC.

n_missing_sic <- sum(is.na(raw_final$SIC) | raw_final$SIC == "")

if (n_missing_sic > 0) {
  warning("Final raw dataset contains missing SIC values: ", n_missing_sic)
} else {
  message("No missing SIC values in final raw dataset.")
}

# Save final raw dataset.

if (!dir.exists(raw_data_dir)) {
  dir.create(raw_data_dir, recursive = TRUE)
}

saveRDS(raw_final, raw_data_file)

message("Final raw dataset saved to: ", raw_data_file)

### ------------------------------------------------------------------------ ###
### 16. SAVE CLEANED SOURCE DATASETS
### ------------------------------------------------------------------------ ###

clean_sources <- list(
  t1   = t1,
  a1   = a1,
  diag = diag,
  au   = au,
  a2   = a2,
  cf   = cf,
  prs  = prs,
  mal  = mal
)

clean_sources_file <- file.path(
  raw_data_dir,
  "clean_sources_for_raw_final.RDS"
)

saveRDS(clean_sources, clean_sources_file)

message("Cleaned source datasets saved to: ", clean_sources_file)
### ------------------------------------------------------------------------ ###
### 17. CLEAN GLOBAL ENVIRONMENT
### ------------------------------------------------------------------------ ###

objects_to_remove <- c(
  "t1", "a1", "a1t", "diag", "a2",
  "uj", "ue", "uja", "ujakg", "udfg",
  "hp1", "hp2", "c1", "c2", "cf",
  "prs", "micm", "mcs", "cicm", "mal",
  "au", "combined", "hpf",
  "base_names", "new_order",
  "sic_duplicates","clean_sources",
  "raw_source_overview","raw_sources",
  "n_missing_sic","source_base_dir","objects_to_remove"
)

rm(list = intersect(objects_to_remove, ls()))

