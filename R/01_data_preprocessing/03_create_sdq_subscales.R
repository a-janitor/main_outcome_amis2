###############################################################################
# PROJECT: AMIS-II MAIN OUTCOME PAPER
# SCRIPT: 03_create_sdq_subscales.R
# PURPOSE: Create SDQ subscale scores from the final raw dataset
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
### 2. CREATE WORKING DATASET FOR SDQ SUBSCALES
### ------------------------------------------------------------------------ ###

sdq_data <- raw_final


### ------------------------------------------------------------------------ ###
### 3. DEFINE AVAILABLE INFORMANT-TIMEPOINT COMBINATIONS
### ------------------------------------------------------------------------ ###

sdq_combinations <- tibble::tribble(
  ~informant, ~timepoint,
  "k",        "t1",
  "b",        "t1",
  "b",        "t2",
  "k",        "t2",
  "p",        "t2",
  "t",        "t2",
  "b",        "t3",
  "k",        "t3",
  "b",        "t5",
  "k",        "t5",
  "p",        "t5",
  "t",        "t5"
)


### ------------------------------------------------------------------------ ###
### 4. REVERSE INVERTED ITEMS
### ------------------------------------------------------------------------ ###

for (i in seq_len(nrow(sdq_combinations))) {
  
  current_informant <- sdq_combinations$informant[i]
  current_timepoint <- sdq_combinations$timepoint[i]
  
  sdq_data <- reverse_items(
    data = sdq_data,
    informant = current_informant,
    timepoint = current_timepoint
  )
}


### ------------------------------------------------------------------------ ###
### 5. CALCULATE SDQ SUBSCALES
### ------------------------------------------------------------------------ ###

for (i in seq_len(nrow(sdq_combinations))) {
  
  current_informant <- sdq_combinations$informant[i]
  current_timepoint <- sdq_combinations$timepoint[i]
  
     message(
         "Scoring SDQ scales: informant = ",
         current_informant,
         ", timepoint = ",
         current_timepoint
       )
  
  sdq_data <- score_sdq_scales(
    data = sdq_data,
    informant = current_informant,
    timepoint = current_timepoint,
    show_summary = TRUE
  )
}


### ------------------------------------------------------------------------ ###
### 6. CHECK CREATED SDQ SCALE VARIABLES
### ------------------------------------------------------------------------ ###

sdq_scale_vars <- names(sdq_data)[
  grepl(
    "^(emotion|conduct|hyper|peer|prosoc|tot)_[kbpt]_t[1235]$",
    names(sdq_data)
  )
]

print(sort(sdq_scale_vars))


### ------------------------------------------------------------------------ ###
### 7. CREATE SDQ SCALE OVERVIEW
### ------------------------------------------------------------------------ ###

sdq_scale_overview <- tibble::tibble(
  variable = sdq_scale_vars
) %>%
  tidyr::extract(
    variable,
    into = c("scale", "informant", "timepoint"),
    regex = "^(emotion|conduct|hyper|peer|prosoc|tot)_([kbpt])_(t[1235])$",
    remove = FALSE
  ) %>%
  dplyr::arrange(timepoint, informant, scale)

print(sdq_scale_overview)

sdq_scale_count <- sdq_scale_overview %>%
  dplyr::count(timepoint, informant, name = "n_scales")

print(sdq_scale_count)


### ------------------------------------------------------------------------ ###
### 8. CHECK EXPECTED NUMBER OF SDQ SCALES
### ------------------------------------------------------------------------ ###

expected_n_scales <- nrow(sdq_combinations) * 6

if (length(sdq_scale_vars) != expected_n_scales) {
  warning(
    "Unexpected number of SDQ scale variables. Expected ",
    expected_n_scales,
    " but found ",
    length(sdq_scale_vars),
    ". Please inspect sdq_scale_overview and sdq_scale_count."
  )
}


### ------------------------------------------------------------------------ ###
### 10. CLEAN GLOBAL ENVIRONMENT
### ------------------------------------------------------------------------ ###

objects_to_remove <- c(
  "i",
  "current_informant",
  "current_timepoint",
  "expected_n_scales",
  "sdq_scale_count",
  "sdq_scale_overview"
)

rm(list = intersect(objects_to_remove, ls()))

