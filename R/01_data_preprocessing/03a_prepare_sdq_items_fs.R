library(readxl)
library(dplyr)
library(stringr)
library(car)


# ========= functions ============
process_sdq <- function(data, reporter = "K", wave = "T2") {
  

  
  # 1) Rename SDQ items
  old_names <- paste0("SD_", reporter, "_", wave, "_", 1:25)
  
  new_base <- c(
    "consid","restles","somatic","shares","tantrum",
    "loner","obeys","worries","caring","fidgety",
    "friend","fights","unhappy","popular","distrac",
    "clingy","kind","lies","bullied","helpout",
    "reflect","steals","oldbest","afraid","attends"
  )
  
  new_names <- paste0(new_base, "_", reporter, "_", wave)
  
  matched <- old_names %in% names(data)
  names(data)[match(old_names[matched], names(data))] <- new_names[matched]
  
  # 2) Convert responses to numeric (0–2)
  pattern_items <- paste0("_", reporter, "_", wave, "$")
  item_vars <- names(data)[grepl(pattern_items, names(data), ignore.case = TRUE)]
  
  data <- data %>%
    mutate(across(
      all_of(item_vars),
      ~ {
        x <- as.character(.)
        x[x == ""] <- NA
        
        if (all(x %in% c("0","1","2", NA))) {
          as.numeric(x)
        } else {
          as.numeric(factor(x, levels = unique(na.omit(x)))) - 1
        }
      }
    ))
  
  # 3) Reverse-code positive items
  positive <- c("obeys","reflect","attends","friend","popular")
  
  for (p in positive) {
    raw_col <- paste0(p, "_", reporter, "_", wave)
    rec_col <- paste0(raw_col, "_rec")
    
    if (raw_col %in% names(data)) {
      data[[raw_col]] <- car::recode(
        data[[raw_col]],
        "0=2; 1=1; 2=0; else=NA"
      )
    }
  }
  
  return(data)
}

clean_sdq_teacher <- function(df, wave = "T2") {
  

  prefix <- paste0("SD_T_", wave, "_") # e.g. "SD_T_T2_"
  
  # old numeric item columns in your data
  old_names <- paste0(prefix, c(
    14,12,13,27,10,
    11,48,26,39,25,
    24,63,38,36,37,
    51,52,23,49,65,
    61,35,62,64,50
  ))
  
  # new attribute names you want
  new_bases <- c(
    "consid","restles","somatic","shares","tantrum",
    "loner","obeys","worries","caring","fidgety",
    "friend","fights","unhappy","popular","distrac",
    "clingy","kind","lies","bullied","helpout",
    "reflect","steals","oldbest","afraid","attends"
  )
  new_names <- paste0(new_bases, "_T_", wave)  # e.g. "consid_T_T2"
  
  # mapping: new = old
  rename_map <- stats::setNames(old_names, new_names)
  
  df_out <- df |>
    dplyr::rename(!!!rename_map) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(new_names),      # only convert those that exist
        ~ as.numeric(as.factor(.x)) - 1
      )
    )
  
  return(df_out)
}


# ===== AMIS-I SDQ =====
# Read file
df <- read_excel("PV0880_datajoin_AMIS-I.xlsx")

# Define demographic variables
demo_vars <- c(
  "TEILNEHMER_SIC",
  "TEILNEHMER_VERSION",
  "TEILNEHMER_GESCHLECHT",
  "TEILNEHMER_GEB_JJJJMM"
)

# Columns starting with SD
sd_vars <- grep("^SD", names(df), value = TRUE, ignore.case = TRUE)

# Columns starting with PERSON
#person_vars <- grep("^PERSON", names(df), value = TRUE, ignore.case = TRUE)

# Select combined variables
df_SD_T2 <- df %>% 
  select(all_of(demo_vars), all_of(sd_vars))

#clean up and rename variable names
names(df_SD_T2) <- names(df_SD_T2) |>
  gsub("^SDQ_E", "SD_B", x = _) |>                     # 1. replaces SDQ_E with SD_B (stands for first caregiver/parent)
  gsub("^SDQ_K", "SD_K", x = _) |>                     # 2. replaces SDQ_K with SD_K (stands for child)
  gsub("^(TEILNEHMER_GESCHLECHT|TEILNEHMER_GEB_JJJJMM)$", "\\1_T2", x = _)  # 3. _T2 anhängen


df_SD_T2 <- df_SD_T2[, !grepl("SDQ_L|DATUM|START|END|CURATED|CHECKED|DQP|UNTERSUCHER|SGROUP|TEILNEHMER_VERSION|SP",names(df_SD_T2))]
names (df_SD_T2)

df_SD_T2 <- process_sdq(df_SD_T2, reporter = "K", wave = "T2")
df_SD_T2 <- process_sdq(df_SD_T2, reporter = "B", wave = "T2") # first caregiver/parent
df_SD_T2 <-process_sdq(df_SD_T2, reporter = "P", wave = "T2") # second caregiver/parent
names (df_SD_T2)


#load teacher data
df_t <-as.data.frame(read_excel("PV0880_T00579_NODUP_AMIS-I.xlsx"))
names(df_t) <- names(df_t) |> gsub("^SOSD", "SD", x = _)
names(df_t)[names(df_t)=="SIC"] <- "TEILNEHMER_SIC"

df_t_T2 <- clean_sdq_teacher(df_t, wave = "T2")
names(df_t_T2)
      
#rename date variable + SIC variable in preparation for merge
df_t_T2 <- df_t_T2[, !(grepl("^SD_T_T2|GRUPPE", names(df_t_T2)))]
names(df_t_T2)

###merge teacher data with the main dataset based on participant ID
table(df_t_T2$TEILNEHMER_SIC %in% df_SD_T2$TEILNEHMER_SIC)
df_SD_T2 <-merge(df_SD_T2,df_t_T2,by="TEILNEHMER_SIC",all.x=T)
names(df_SD_T2)



# ===== AMIS-I SDQ =====
# Read file
df <- read_excel("PV0880_datajoin_AMIS-II.xlsx")


names(df) <- names(df) |> gsub("^SOSD", "SD", x = _)
names(df)

# Define demographic variables
demo_vars <- c(
  "TEILNEHMER_SIC",
  "TEILNEHMER_VERSION",
  "TEILNEHMER_GESCHLECHT",
  "TEILNEHMER_GEB_JJJJMM"
)

# Columns starting with SD
sd_vars <- grep("^SD", names(df), value = TRUE, ignore.case = TRUE)

# Columns starting with PERSON
#person_vars <- grep("^PERSON", names(df), value = TRUE, ignore.case = TRUE)

# Select combined variables
df_SD_T5 <- df %>% 
  select(all_of(demo_vars), all_of(sd_vars))

#clean up and rename variable names
names(df_SD_T5) <- names(df_SD_T5) |>
  gsub("^(TEILNEHMER_GESCHLECHT|TEILNEHMER_GEB_JJJJMM)$", "\\1_T5", x = _)  # 3. _T5 anhängen


df_SD_T5 <- df_SD_T5[, !grepl("DATUM|START|END|CURATED|CHECKED|DQP|UNTERSUCHER|SGROUP|TEILNEHMER_VERSION|SP",names(df_SD_T5))]
names (df_SD_T5)

df_SD_T5 <- process_sdq(df_SD_T5, reporter = "K", wave = "T5")
df_SD_T5 <- process_sdq(df_SD_T5, reporter = "B", wave = "T5") # first caregiver/parent
df_SD_T5 <-process_sdq(df_SD_T5, reporter = "P", wave = "T5") # second caregiver/parent
df_SD_T5 <- df_SD_T5[, !grepl("SD_K",names(df_SD_T5))]

names(df_SD_T5)
df_SD_T5 <- clean_sdq_teacher(df_SD_T5, wave = "T5")

df_SD_T5 <- df_SD_T5[, !(grepl("^SD_T_T5|GRUPPE", names(df_SD_T5)))]
names(df_SD_T5)


# merge T2 and T5
table(df_SD_T2$TEILNEHMER_SIC %in% df_SD_T5$TEILNEHMER_SIC)
df_SD <-merge(df_SD_T2,df_SD_T5,by="TEILNEHMER_SIC",all.x=T)
names(df_SD)


# Save df_SD
library(openxlsx)
write.xlsx(df_SD, file = "AMIS_sdq_t2-t5.xlsx", overwrite = TRUE)
