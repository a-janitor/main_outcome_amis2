###############################################################################
# PROJECT: AMIS-II MAIN OUTCOME PAPER
# SCRIPT: 03a_create_sdq_subscales_fs.R
# PURPOSE: Create cross-informant SDQ mini-items and parcels based on Fateme's syntax
###############################################################################

### ------------------------------------------------------------------------ ###
### 0. SETUP
### ------------------------------------------------------------------------ ###

source("R/01_data_preprocessing/00_setup_paths_packages.R")
source("R/01_data_preprocessing/01_create_functions.R")


### ------------------------------------------------------------------------ ###
### 1. LOAD FINAL RAW DATASET
### ------------------------------------------------------------------------ ###

raw_final <- readRDS(raw_data_file)


### ------------------------------------------------------------------------ ###
### 2. CREATE WORKING DATASET
### ------------------------------------------------------------------------ ###

sdq_data_fs <- raw_final


### ------------------------------------------------------------------------ ###
### 3. DEFINE SDQ ITEM MAP
### ------------------------------------------------------------------------ ###

item_map <- list(
  emo  = c("somatic", "worries", "unhappy", "clingy", "afraid"),
  con  = c("tantrum", "obeys", "fights", "lies", "steals"),
  hyp  = c("restles", "fidgety", "distrac", "reflect", "attends"),
  peer = c("loner", "friend", "popular", "bullied", "oldbest"),
  pros = c("consid", "shares", "caring", "kind", "helpout")
)


### ------------------------------------------------------------------------ ###
### 4. DEFINE REPORTERS AND WAVES
### ------------------------------------------------------------------------ ###

# These are adapted to Jan's harmonized variable names:
# e.g., somatic_b_t2, worries_k_t5, attends_t_t5

selected_reporters <- c("b", "k", "p", "t")
selected_waves <- c("t2", "t5")

min_reporters_per_item <- 2


### ------------------------------------------------------------------------ ###
### 5. CHECK AVAILABLE SDQ ITEM VARIABLES
### ------------------------------------------------------------------------ ###

all_stems <- unique(unlist(item_map))

expected_sdq_items <- expand.grid(
  stem = all_stems,
  reporter = selected_reporters,
  wave = selected_waves,
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(
    variable = paste0(stem, "_", reporter, "_", wave),
    available = variable %in% names(sdq_data_fs)
  )

sdq_item_availability <- expected_sdq_items %>%
  dplyr::count(wave, reporter, available)

print(sdq_item_availability)

missing_sdq_items <- expected_sdq_items %>%
  dplyr::filter(!available)

print(missing_sdq_items)

### ------------------------------------------------------------------------ ###
### 6. CREATE CROSS-INFORMANT MINI-ITEMS
### ------------------------------------------------------------------------ ###

sdq_data_fs <- create_crossinformant_mini_items(
  data = sdq_data_fs,
  item_map = item_map,
  reporters = selected_reporters,
  waves = selected_waves,
  min_reporters_per_item = min_reporters_per_item
)


### ------------------------------------------------------------------------ ###
### 7. CHECK CREATED MINI-ITEMS
### ------------------------------------------------------------------------ ###

mini_vars <- names(sdq_data_fs)[
  grepl("^mini_.*_t[25]$", names(sdq_data_fs))
]

mini_overview <- tibble::tibble(
  variable = mini_vars
) %>%
  tidyr::extract(
    variable,
    into = c("stem", "wave"),
    regex = "^mini_(.*)_(t[25])$",
    remove = FALSE
  ) %>%
  dplyr::arrange(wave, stem)

print(mini_overview)

mini_count <- mini_overview %>%
  dplyr::count(wave, name = "n_mini_items")

print(mini_count)


### ------------------------------------------------------------------------ ###
### 8. CREATE BALANCED PARCEL PLAN USING CFA LOADINGS AT T2
### ------------------------------------------------------------------------ ###

parcel_plan <- lapply(
  item_map,
  function(stems) {
    balanced_split_cfa(
      data = sdq_data_fs,
      stems = stems,
      wave_for_cfa = "t2"
    )
  }
)

print(parcel_plan)


### ------------------------------------------------------------------------ ###
### 9. CREATE SDQ PARCELS FOR T2 AND T5
### ------------------------------------------------------------------------ ###

sdq_data_fs <- create_sdq_parcels(
  data = sdq_data_fs,
  item_map = item_map,
  parcel_plan = parcel_plan,
  waves = selected_waves
)

### ------------------------------------------------------------------------ ###
### 10. CHECK CREATED PARCELS
### ------------------------------------------------------------------------ ###

parcel_check <- check_sdq_parcels(
  data = sdq_data_fs,
  waves = selected_waves
)

print(parcel_check$parcel_overview)
print(parcel_check$parcel_count)


### ------------------------------------------------------------------------ ###
### 11. CREATE PARCEL DATASET FOR INSPECTION
### ------------------------------------------------------------------------ ###

id_cols <- names(sdq_data_fs)[
  grepl("SIC|TEILNEHMER", names(sdq_data_fs))
]

parcel_cols <- names(sdq_data_fs)[
  grepl("_(parA|parB)_t[25]$", names(sdq_data_fs))
]

df_parcels_fs <- sdq_data_fs %>%
  dplyr::select(
    dplyr::any_of(id_cols),
    dplyr::all_of(parcel_cols)
  )

print(names(df_parcels_fs))


### ------------------------------------------------------------------------ ###
### 12. OPTIONAL: SAVE CHECK TABLES, BUT NOT THE DATASET
### ------------------------------------------------------------------------ ###

sdq_check_dir <- file.path(data_dir, "../03a_outputs/sdq_checks")

if (!dir.exists(sdq_check_dir)) {
  dir.create(sdq_check_dir, recursive = TRUE)
}

readr::write_csv(
  sdq_item_availability,
  file.path(sdq_check_dir, "sdq_fs_item_availability.csv")
)

readr::write_csv(
  missing_sdq_items,
  file.path(sdq_check_dir, "sdq_fs_missing_items.csv")
)

readr::write_csv(
  mini_overview,
  file.path(sdq_check_dir, "sdq_fs_mini_item_overview.csv")
)

readr::write_csv(
  mini_count,
  file.path(sdq_check_dir, "sdq_fs_mini_item_count.csv")
)

readr::write_csv(
  parcel_check$parcel_overview,
  file.path(sdq_check_dir, "sdq_fs_parcel_overview.csv")
)

readr::write_csv(
  parcel_check$parcel_count,
  file.path(sdq_check_dir, "sdq_fs_parcel_count.csv")
)


### ------------------------------------------------------------------------ ###
### 13. CLEAN GLOBAL ENVIRONMENT
### ------------------------------------------------------------------------ ###

objects_to_remove <- c(
  "all_stems",
  "expected_sdq_items",
  "id_cols",
  "parcel_cols"
)

rm(list = intersect(objects_to_remove, ls()))


### ------------------------------------------------------------------------ ###
### 14. SESSION INFO
### ------------------------------------------------------------------------ ###

sessionInfo()

###############################################################################
# END OF SCRIPT
###############################################################################