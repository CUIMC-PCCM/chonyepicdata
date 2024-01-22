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
     arrange(mrn, enc_id, rass_time)

# Get CAPD scores for all these patients
df_capd <- load_capd(paste0(data_path_chony, fname_sedation_delirium)) %>%
     filter(enc_id %in% t21_enc_id) %>%
     arrange(mrn, enc_id, capd_time)

# Get all of the ICD codes across time for these patients.
df_icd_t21only <- df_icd %>% filter(enc_id %in% t21_enc_id) %>%
     select(mrn, enc_id, icd10_code, dx_name, dx_date)

# Load medication data, just for the T21 group
df_meds <- load_meds(paste0(data_path_chony, fname_ip_meds))
df_meds <- df_meds %>% filter(mrn %in% t21_mrn)

# Adjust the start/stop PICU time table to match the format needed for clean_meds()
time_limits <- df_picu_startstop %>%
     mutate(picu_intervals = interval(icu_start_date, icu_stop_date)) %>%
     select(-mrn, -icu_start_date, -icu_stop_date)

# Get medication exposures, just for the T21 cohort, only during valid PICU times, and only for fentanyl,
# midazolam, and dexmedetomidine. Individual PICU stays will have the encounter ID, with #1, #2, #3, etc
# appended to the end to denote which PICU stay it was
med_exposure <- clean_meds(df_meds, medlist = c('fentanyl', 'midazolam', 'dexmedetomidine'), time_limits = time_limits)

# Separate out the number of the PICU stay, and bind the medication exposure back to the encounter and MRN
med_exposure <- med_exposure %>%
     separate_wider_delim(cols = enc_id, delim = '#', names = c('enc_id', 'picu_stay_num')) %>%
     mutate(picu_stay_num = as.integer(picu_stay_num)) %>%
     left_join(df_encounters)

# Get the PICU start/stop dates and add in
df_picu_startstop <- df_picu_startstop %>%
     group_by(enc_id) %>% mutate(picu_stay_num = row_number())
med_exposure <- med_exposure %>%
     left_join(df_picu_startstop) %>%
     relocate(mrn, enc_id, hospital_admission_date, hospital_discharge_date, icu_start_date, icu_stop_date, picu_stay_num) %>%
     select(-sex, dob)

# Example of how to save:
writexl::write_xlsx(med_exposure, paste0(data_path_chony, '../output/T21_med_exposure-', today(), '.xlsx'))
