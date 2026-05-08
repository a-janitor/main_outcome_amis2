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
