###############################################################################
# FUNCTION: rename_sdq_items
# PROJECT: AMIS-II MAIN OUTCOME PAPER
# PURPOSE: Rename SDQ item variables into harmonized item names
###############################################################################

rename_sdq_items <- function(data, prefix, suffix, type) {
  
  # data   = data frame
  # prefix = original informant prefix, e.g., "K", "B", "E", "P", "T"
  # suffix = time point suffix, e.g., "T1", "T2", "T3", "T5"
  # type   = harmonized informant type, e.g., "k", "b", "p", "t"
  
  item_names <- c(
    "consid",
    "restles",
    "somatic",
    "shares",
    "tantrum",
    "loner",
    "obeys",
    "worries",
    "caring",
    "fidgety",
    "friend",
    "fights",
    "unhappy",
    "popular",
    "distrac",
    "clingy",
    "kind",
    "lies",
    "bullied",
    "helpout",
    "reflect",
    "steals",
    "oldbest",
    "afraid",
    "attends"
  )
  
  new_names <- paste0(
    item_names,
    "_",
    type,
    "_",
    tolower(suffix)
  )
  
  # Case 1: regular SDQ item names for child/caregiver/parent formats
  # Example: SD_K_T2_1, SD_B_T2_1, SD_P_T5_1
  if (type != "t") {
    
    old_names <- paste0("SD_", prefix, "_", suffix, "_", 1:25)
    
  }
  
  # Case 2: teacher SDQ item names from SOSD_T variables
  # Example: SOSD_T_T2_14, SOSD_T_T5_14
  if (type == "t") {
    
    teacher_item_numbers <- c(
      14, 12, 13, 27, 10,
      11, 48, 26, 39, 25,
      24, 63, 38, 36, 37,
      51, 52, 23, 49, 65,
      61, 35, 62, 64, 50
    )
    
    old_names <- paste0("SOSD_T_", suffix, "_", teacher_item_numbers)
    
  }
  
  existing_old_names <- old_names[old_names %in% names(data)]
  corresponding_new_names <- new_names[old_names %in% names(data)]
  
  names(data)[match(existing_old_names, names(data))] <- corresponding_new_names
  
  return(data)
}

### ------------------------------------------------------------------------ ###
### 2. REVERSE CODING FUNCTION
### ------------------------------------------------------------------------ ###

rev_fun <- function(x) {
  dplyr::recode(
    x,
    `0` = 2,
    `1` = 1,
    `2` = 0,
    .default = NA_real_
  )
}


### ------------------------------------------------------------------------ ###
### 2. REVERSE SDQ ITEMS
### ------------------------------------------------------------------------ ###

reverse_items <- function(data, informant, timepoint) {
  
  items <- c("obeys", "reflect", "attends", "friend", "popular")
  
  old_names <- paste0(items, "_", informant, "_", timepoint)
  
  missing_vars <- old_names[!old_names %in% names(data)]
  
  if (length(missing_vars) > 0) {
    stop(
      "These SDQ variables are missing and cannot be reverse-coded: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(old_names),
        rev_fun,
        .names = "r{.col}"
      )
    )
}


### ------------------------------------------------------------------------ ###
### 3. SCORE SDQ SCALES
### ------------------------------------------------------------------------ ###

score_sdq_scales <- function(data, informant, timepoint, max_na = 2, show_summary = FALSE) {
  
  suffix <- paste0("_", informant, "_", timepoint)
  
  scales <- list(
    emotion = c("somatic", "worries", "unhappy", "clingy", "afraid"),
    conduct = c("tantrum", "robeys", "fights", "lies", "steals"),
    hyper   = c("restles", "fidgety", "distrac", "rreflect", "rattends"),
    peer    = c("loner", "rfriend", "rpopular", "bullied", "oldbest"),
    prosoc  = c("consid", "shares", "caring", "kind", "helpout")
  )
  
  make_score <- function(items) {
    
    vars <- paste0(items, suffix)
    
    if (!all(vars %in% names(data))) {
      missing_vars <- vars[!vars %in% names(data)]
      stop(
        "These variables are missing from the data: ",
        paste(missing_vars, collapse = ", ")
      )
    }
    
    x <- data[vars]
    
    non_numeric <- vars[!sapply(x, is.numeric)]
    
    if (length(non_numeric) > 0) {
      stop(
        "These variables are not numeric: ",
        paste(non_numeric, collapse = ", ")
      )
    }
    
    n_na <- rowSums(is.na(x))
    score <- rowMeans(x, na.rm = TRUE)
    
    score[n_na > max_na] <- NA
    score[n_na == length(vars)] <- NA
    
    round(score * length(vars))
  }
  
  for (scale_name in names(scales)) {
    data[[paste0(scale_name, suffix)]] <- make_score(scales[[scale_name]])
  }
  
  total_vars <- paste0(c("emotion", "conduct", "hyper", "peer"), suffix)
  data[[paste0("tot", suffix)]] <- rowSums(data[total_vars], na.rm = FALSE)
  
  if (show_summary) {
    summary_vars <- c(
      paste0("emotion", suffix),
      paste0("conduct", suffix),
      paste0("hyper", suffix),
      paste0("peer", suffix),
      paste0("prosoc", suffix),
      paste0("tot", suffix)
    )
    
    print(summary(data[summary_vars]))
  }
  
  data
}

### ------------------------------------------------------------------------ ###
### 4. SDQ CHECK FUNCTION
### ------------------------------------------------------------------------ ###

check_sdq_combination <- function(data, informant, timepoint) {
  
  item_names <- c(
    "consid", "restles", "somatic", "shares", "tantrum",
    "loner", "obeys", "worries", "caring", "fidgety",
    "friend", "fights", "unhappy", "popular", "distrac",
    "clingy", "kind", "lies", "bullied", "helpout",
    "reflect", "steals", "oldbest", "afraid", "attends"
  )
  
  reverse_item_names <- c(
    "robeys", "rreflect", "rattends", "rfriend", "rpopular"
  )
  
  scale_names <- c(
    "emotion", "conduct", "hyper", "peer", "prosoc", "tot"
  )
  
  suffix <- paste0("_", informant, "_", timepoint)
  
  expected_items <- paste0(item_names, suffix)
  expected_reversed <- paste0(reverse_item_names, suffix)
  expected_scales <- paste0(scale_names, suffix)
  
  tibble::tibble(
    informant = informant,
    timepoint = timepoint,
    n_expected_items = length(expected_items),
    n_present_items = sum(expected_items %in% names(data)),
    missing_items = paste(expected_items[!expected_items %in% names(data)], collapse = ", "),
    n_expected_reversed = length(expected_reversed),
    n_present_reversed = sum(expected_reversed %in% names(data)),
    missing_reversed = paste(expected_reversed[!expected_reversed %in% names(data)], collapse = ", "),
    n_expected_scales = length(expected_scales),
    n_present_scales = sum(expected_scales %in% names(data)),
    missing_scales = paste(expected_scales[!expected_scales %in% names(data)], collapse = ", ")
  )
}



