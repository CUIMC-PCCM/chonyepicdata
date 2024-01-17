# Create basic dataset examples for Liza

library(chonyepicdata)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)

# Load configuration files. You may need to edit the file (located in a config folder) with your own filepath.
# Alternately you can just send in the correct filename.
load_config(useglobal = TRUE)

# load all encounters
df_encounters <- load_encounters(paste0(data_path_chony, fname_encounter))

# Get all ICD codes
df_icd <- load_icd_dx(paste0(data_path_chony, fname_icd_dx))

# Find all patients with a code for Trisomy 21
t21_mrn <- df_icd %>% filter(str_detect(icd10_code, '^Q90')) %>%
     distinct(mrn) %>% pull()

# Limit the list of encounters to those with a T21 diagnosis
df_encounters <- df_encounters %>% filter(mrn %in% t21_mrn)

# Get a list of just the encounter IDs
t21_enc_id <- df_encounters %>% distinct(enc_id) %>% pull()

# Now get a set of all PICU start/stop datetimes for these encounters
df_picu_startstop <- get_picu_intervals(paste0(data_path_chony, fname_adt)) %>%
     filter(enc_id %in% t21_enc_id) %>%
     arrange(mrn, enc_id, icu_start_date)

# Get RASS scores for all these patients
df_rass <- load_rass(paste0(data_path_chony, fname_sedation_delirium)) %>%
     filter(enc_id %in% t21_enc_id) %>%
     arrange(mrn, enc_id, rass_date)

# Get CAPD scores for all these patients
df_capd <- load_capd(paste0(data_path_chony, fname_sedation_delirium)) %>%
     filter(enc_id %in% t21_enc_id) %>%
     arrange(mrn, enc_id, capd_date)

# Get all of the ICD codes across time for these patients.
df_icd_t21only <- df_icd %>% filter(enc_id %in% t21_enc_id) %>%
     select(mrn, enc_id, icd10_code, dx_name, dx_date)

# Example of how to save:
# write_xlsx(df_rass, 'file_location.xlsx')
