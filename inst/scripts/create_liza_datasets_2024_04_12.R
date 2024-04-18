# Create basic dataset examples for Liza

# *****************************************************************************
# Update log ------------------------------------------------------------------
# *****************************************************************************
#' 4/12/2024
#'   Added section for patients without trisomy 21


# *****************************************************************************
# Basic loading ---------------------------------------------------------------
# *****************************************************************************

library(chonyepicdata)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)

options(scipen = 3)

# Load configuration files. You may need to edit the file (located in a config folder) with your own filepath.
# Alternately you can just send in the correct filename.
load_config(useglobal = TRUE)
data_path <- data_path_chony

# *****************************************************************************
# Loading new data ------------------------------------------------------------
# *****************************************************************************

# load all encounters
df_encounters <- load_encounters(paste0(data_path, fname_encounter))

# Get all ICD codes
df_icd <- load_icd_dx(paste0(data_path, fname_icd_dx))

# Find all patients with a code for Trisomy 21
t21_mrn <- df_icd %>% filter(str_detect(icd10_code, '^Q90')) %>%
     distinct(mrn) %>% pull()

# Limit the list of encounters to those with a T21 diagnosis
df_encounters <- df_encounters %>% filter(mrn %in% t21_mrn)

# Get a list of just the encounter IDs
t21_enc_id <- df_encounters %>% distinct(enc_id) %>% pull()

# Now get a set of all PICU start/stop datetimes for these encounters
df_picu_startstop <- get_picu_intervals(paste0(data_path, fname_adt)) %>%
     filter(enc_id %in% t21_enc_id) %>%
     arrange(mrn, enc_id, icu_start_date)

# Get RASS scores for all these patients
df_rass <- load_rass(paste0(data_path, fname_sedation_delirium)) %>%
     filter(enc_id %in% t21_enc_id) %>%
     arrange(mrn, enc_id, rass_time)

# Get CAPD scores for all these patients
df_capd <- load_capd(paste0(data_path, fname_sedation_delirium)) %>%
     filter(enc_id %in% t21_enc_id) %>%
     arrange(mrn, enc_id, capd_time)

# Get all of the ICD codes across time for these patients.
df_icd_t21only <- df_icd %>% filter(enc_id %in% t21_enc_id)

# Load medication data, just for the T21 group
df_meds <- load_meds(paste0(data_path, fname_ip_meds))
df_meds <- df_meds %>% filter(mrn %in% t21_mrn)

# Get ventilator data and just limit to patient encounters we are interested in
df_vent <- load_vent(paste0(data_path, fname_imv))
df_vent <- df_vent %>% filter(enc_id %in% t21_enc_id)
df_vent_wide <- clean_vent(df_vent)
df_vent_episodes <- get_imv_startstop(df_vent_wide)

# Join based on encounter ID, and whether the intervals for (vent start, vent stop) and (icu start, icu stop)
# have any overlap. This ensures we include patients intubated in the ED or a procedural area
by <- join_by(enc_id, overlaps(x$vent_time_start, x$vent_time_stop, y$icu_start_date, y$icu_stop_date))
df_vent_episodes <- left_join(df_vent_episodes, df_picu_startstop, by) %>%
     inner_join(df_encounters, by = c('mrn', 'enc_id')) %>%
     select(mrn, enc_id, hospital_admission_date, hospital_discharge_date, icu_start_date, icu_stop_date,
            vent_episode, vent_time_start, vent_time_stop, timediff, first_trach_datetime)
df_vent_episodes <- df_vent_episodes %>% select(-first_trach_datetime)

# We are now going to look at drug exposure. The start/stop times will be defined by start/stop
# of ventilation. We later may use pre/post drug exposure.
time_limits <- df_vent_episodes %>%
     mutate(picu_intervals = interval(vent_time_start, vent_time_stop)) %>%
     select(enc_id, picu_intervals)

# # Adjust the start/stop PICU time table to match the format needed for clean_meds()
# time_limits <- df_picu_startstop %>%
#      mutate(picu_intervals = interval(icu_start_date, icu_stop_date)) %>%
#      select(-mrn, -icu_start_date, -icu_stop_date)

# Get weights, which are required for adjusting to weight-based dosing.
# For some reason weight is recorded in ounces, so divide by 35.274 to get kg
df_weights <- load_vitals(paste0(data_path, fname_vitals), vitals_to_load = 'R NYC DRY (DOSING) WEIGHT') %>%
     group_by(enc_id) %>% arrange(enc_id, vital_time) %>%
     slice_head(n=1) %>% ungroup() %>%
     mutate(dosing_weight = as.numeric(meas_value)/35.274) %>%
     select(enc_id, dosing_weight)

# Get medication exposures, just for the T21 cohort, only during valid PICU times, and only for fentanyl,
# midazolam, and dexmedetomidine. Individual PICU stays will have the encounter ID, with #1, #2, #3, etc
# appended to the end to denote which PICU stay it was
med_exposure <- clean_meds(df_meds, medlist = c('midazolam',
                                                'lorazepam',
                                                'diazepam',
                                                'morphine',
                                                'fentanyl',
                                                'hydromorphone',
                                                'oxycodone',
                                                'methadone',
                                                'clonidine',
                                                'ketamine',
                                                'pentobarbital',
                                                'quetiapine',
                                                'haloperidol',
                                                'risperidone',
                                                'olanzapine',
                                                'chlorpromazine',
                                                'aripiprazole',
                                                'diphenhydramine',
                                                'hydroyxyzine',
                                                'dexmedetomidine'),
                           time_limits = time_limits, patient_weights = df_weights)

# Separate out the number of the episode of ventilation
med_exposure <- med_exposure %>%
     separate_wider_delim(cols = enc_id, delim = '#', names = c('enc_id', 'vent_ep_num')) %>%
     mutate(vent_ep_num = as.integer(vent_ep_num)) %>%
     left_join(df_encounters)

# Get the ventilation episode number
df_vent_episodes_num <- df_vent_episodes %>%
     group_by(enc_id) %>% mutate(vent_ep_num = row_number()) %>%
     ungroup() %>%
     select(mrn, enc_id, vent_ep_num, vent_time_start, vent_time_stop)

# Join the vent episode data to the med exposure data
med_exposure <- med_exposure %>%
     left_join(df_vent_episodes_num) %>%
     select(-sex, -dob)

# Now join the PICU stay data
picu_stay_num <- df_picu_startstop %>%
     group_by(enc_id) %>%
     mutate(picu_stay_num = row_number()) %>%
     ungroup()

med_exposure <- med_exposure %>%
     left_join(picu_stay_num, by = join_by(mrn, enc_id,
                                           overlaps(x$vent_time_start, x$vent_time_stop, y$icu_start_date, y$icu_stop_date))) %>%
     relocate(mrn, enc_id, hospital_admission_date, hospital_discharge_date,
              picu_stay_num, icu_start_date, icu_stop_date,
              vent_ep_num, vent_time_start, vent_time_stop)

# Load laboratory data

# Save the data
saveRDS(df_vent_episodes, paste0(data_path, '../output/t21_vent_med_exposures_', today(), '.rds'))
writexl::write_xlsx(df_vent_episodes, paste0(data_path, '../output/t21_vent_med_exposures_', today(), '.xlsx'))

# Example of how to save:
writexl::write_xlsx(med_exposure, paste0(data_path, '../output/T21_med_exposure-', today(), '.xlsx'))

# *****************************************************************************
# Previously loaded data ------------------------------------------------------
# *****************************************************************************


# *****************************************************************************
## Load prior T21 patient data -------------------------------------------------
# *****************************************************************************
list2env(get_rds(file_path = data_path), envir = .GlobalEnv)
df_icd <- df_icd_dx
rm(df_icd_dx)

# Find all patients with a code for Trisomy 21
t21_mrn <- df_icd %>% filter(str_detect(icd10_code, '^Q90')) %>%
     distinct(mrn) %>% pull()

# Limit the list of encounters to those with a T21 diagnosis
df_encounters <- df_encounters %>% filter(mrn %in% t21_mrn)

# Get a list of just the encounter IDs
t21_enc_id <- df_encounters %>% distinct(enc_id) %>% pull()

# Now get a set of all PICU start/stop datetimes for these encounters
df_picu_startstop <- df_picu_startstop %>%
     filter(enc_id %in% t21_enc_id) %>%
     arrange(mrn, enc_id, icu_start_date)

# Get vent for T21 group
df_vent_episodes <- df_vent_episodes %>% filter(enc_id %in% t21_enc_id)

# Join based on encounter ID, and whether the intervals for (vent start, vent stop) and (icu start, icu stop)
# have any overlap. This ensures we include patients intubated in the ED or a procedural area
by <- join_by(enc_id, overlaps(x$vent_time_start, x$vent_time_stop, y$icu_start_date, y$icu_stop_date))
df_vent_episodes <- left_join(df_vent_episodes, df_picu_startstop, by) %>%
     inner_join(df_encounters, by = c('mrn', 'enc_id')) %>%
     select(mrn, enc_id, hospital_admission_date, hospital_discharge_date, icu_start_date, icu_stop_date,
            vent_episode, vent_time_start, vent_time_stop, timediff, first_trach_datetime)
df_vent_episodes <- df_vent_episodes %>% select(-first_trach_datetime)

# We are now going to look at drug exposure. The start/stop times will be defined by start/stop
# of ventilation. We later may use pre/post drug exposure.
time_limits <- df_vent_episodes %>%
     mutate(picu_intervals = interval(vent_time_start, vent_time_stop)) %>%
     select(enc_id, picu_intervals)

# Get weights, which are required for adjusting to weight-based dosing.
# For some reason weight is recorded in ounces, so divide by 35.274 to get kg
df_weight <- df_weight %>%
     filter(weight_type == 'R NYC DRY (DOSING) WEIGHT') %>%
     group_by(enc_id) %>% arrange(enc_id, weight_time) %>%
     slice_head(n=1) %>% ungroup() %>%
     mutate(dosing_weight = as.numeric(weight)/35.274) %>%
     select(enc_id, dosing_weight)

# Load medication data, just for the T21 group
df_meds <- df_meds %>% filter(mrn %in% t21_mrn)

# Get medication exposures, just for the T21 cohort, only during valid PICU times, and only for fentanyl,
# midazolam, and dexmedetomidine. Individual PICU stays will have the encounter ID, with #1, #2, #3, etc
# appended to the end to denote which PICU stay it was
med_exposure <- clean_meds(df_meds, medlist = c('midazolam',
                                                'lorazepam',
                                                'diazepam',
                                                'morphine',
                                                'fentanyl',
                                                'hydromorphone',
                                                'oxycodone',
                                                'methadone',
                                                'clonidine',
                                                'ketamine',
                                                'pentobarbital',
                                                'quetiapine',
                                                'haloperidol',
                                                'risperidone',
                                                'olanzapine',
                                                'chlorpromazine',
                                                'aripiprazole',
                                                'diphenhydramine',
                                                'hydroyxyzine',
                                                'dexmedetomidine'),
                           time_limits = time_limits, patient_weights = df_weight)


# Separate out the number of the episode of ventilation
med_exposure <- med_exposure %>%
     separate_wider_delim(cols = enc_id, delim = '#', names = c('enc_id', 'vent_ep_num')) %>%
     mutate(vent_ep_num = as.integer(vent_ep_num)) %>%
     left_join(df_encounters)

# Get the ventilation episode number
df_vent_episodes_num <- df_vent_episodes %>%
     group_by(enc_id) %>% mutate(vent_ep_num = row_number()) %>%
     ungroup() %>%
     select(mrn, enc_id, vent_ep_num, vent_time_start, vent_time_stop)

# Join the vent episode data to the med exposure data
med_exposure <- med_exposure %>%
     left_join(df_vent_episodes_num) %>%
     select(-sex, -dob)

# Now join the PICU stay data
picu_stay_num <- df_picu_startstop %>%
     group_by(enc_id) %>%
     mutate(picu_stay_num = row_number()) %>%
     ungroup()

med_exposure <- med_exposure %>%
     left_join(picu_stay_num, by = join_by(mrn, enc_id,
                                           overlaps(x$vent_time_start, x$vent_time_stop, y$icu_start_date, y$icu_stop_date))) %>%
     relocate(mrn, enc_id, hospital_admission_date, hospital_discharge_date,
              picu_stay_num, icu_start_date, icu_stop_date,
              vent_ep_num, vent_time_start, vent_time_stop)

# Save the data
saveRDS(med_exposure, paste0(data_path, '../output/t21_vent_med_exposures_', today(), '.rds'))
writexl::write_xlsx(med_exposure, paste0(data_path, '../output/t21_vent_med_exposures_', today(), '.xlsx'))

# *****************************************************************************
## Load prior non-T21 patients -------------------------------------------------
# *****************************************************************************
list2env(get_rds(file_path = data_path), envir = .GlobalEnv)
df_icd <- df_icd_dx
rm(df_icd_dx)

# Find all patients with a code for Trisomy 21
not_t21_mrn <- df_icd %>% filter(str_detect(icd10_code, '^Q90', negate = TRUE)) %>%
     distinct(mrn)

# Remove MRNs that had T21 on a different encounter
not_t21_mrn <- not_t21_mrn %>% filter(!(mrn %in% t21_mrn)) %>% pull()

# Limit the list of encounters to those with a T21 diagnosis
df_encounters_nont21 <- df_encounters %>% filter(mrn %in% not_t21_mrn)

# Get a list of just the encounter IDs
not_t21_enc_id <- df_encounters_nont21 %>% distinct(enc_id) %>% pull()

# Now get a set of all PICU start/stop datetimes for these encounters
df_picu_startstop_nont21 <- df_picu_startstop %>%
     filter(enc_id %in% not_t21_enc_id) %>%
     arrange(mrn, enc_id, icu_start_date)

# Get vent for non-T21 group
df_vent_episodes <- df_vent_episodes %>% filter(enc_id %in% not_t21_enc_id)

# Join based on encounter ID, and whether the intervals for (vent start, vent stop) and (icu start, icu stop)
# have any overlap. This ensures we include patients intubated in the ED or a procedural area
by <- join_by(enc_id, overlaps(x$vent_time_start, x$vent_time_stop, y$icu_start_date, y$icu_stop_date))
df_vent_episodes <- left_join(df_vent_episodes, df_picu_startstop_nont21, by) %>%
     inner_join(df_encounters, by = c('mrn', 'enc_id')) %>%
     select(mrn, enc_id, hospital_admission_date, hospital_discharge_date, icu_start_date, icu_stop_date,
            vent_episode, vent_time_start, vent_time_stop, timediff, first_trach_datetime)
df_vent_episodes <- df_vent_episodes %>% select(-first_trach_datetime)

# We are now going to look at drug exposure. The start/stop times will be defined by start/stop
# of ventilation. We later may use pre/post drug exposure.
time_limits <- df_vent_episodes %>%
     mutate(picu_intervals = interval(vent_time_start, vent_time_stop)) %>%
     select(enc_id, picu_intervals)

# Get weights, which are required for adjusting to weight-based dosing.
# For some reason weight is recorded in ounces, so divide by 35.274 to get kg
df_weight <- df_weight %>%
     filter(weight_type == 'R NYC DRY (DOSING) WEIGHT') %>%
     group_by(enc_id) %>% arrange(enc_id, weight_time) %>%
     slice_head(n=1) %>% ungroup() %>%
     mutate(dosing_weight = as.numeric(weight)/35.274) %>%
     select(enc_id, dosing_weight)

# Load medication data, just for the T21 group
df_meds <- df_meds %>% filter(mrn %in% not_t21_mrn)

# Get medication exposures, just for the T21 cohort, only during valid PICU times, and only for fentanyl,
# midazolam, and dexmedetomidine. Individual PICU stays will have the encounter ID, with #1, #2, #3, etc
# appended to the end to denote which PICU stay it was
med_exposure <- clean_meds(df_meds, medlist = c('midazolam',
                                                'lorazepam',
                                                'diazepam',
                                                'morphine',
                                                'fentanyl',
                                                'hydromorphone',
                                                'oxycodone',
                                                'methadone',
                                                'clonidine',
                                                'ketamine',
                                                'pentobarbital',
                                                'quetiapine',
                                                'haloperidol',
                                                'risperidone',
                                                'olanzapine',
                                                'chlorpromazine',
                                                'aripiprazole',
                                                'diphenhydramine',
                                                'hydroyxyzine',
                                                'dexmedetomidine'),
                           time_limits = time_limits, patient_weights = df_weight)


# Separate out the number of the episode of ventilation
med_exposure <- med_exposure %>%
     separate_wider_delim(cols = enc_id, delim = '#', names = c('enc_id', 'vent_ep_num')) %>%
     mutate(vent_ep_num = as.integer(vent_ep_num)) %>%
     left_join(df_encounters)

# Get the ventilation episode number
df_vent_episodes_num <- df_vent_episodes %>%
     group_by(enc_id) %>% mutate(vent_ep_num = row_number()) %>%
     ungroup() %>%
     select(mrn, enc_id, vent_ep_num, vent_time_start, vent_time_stop)

# Join the vent episode data to the med exposure data
med_exposure <- med_exposure %>%
     left_join(df_vent_episodes_num) %>%
     select(-sex, -dob)

# Now join the PICU stay data
picu_stay_num <- df_picu_startstop_nont21 %>%
     group_by(enc_id) %>%
     mutate(picu_stay_num = row_number()) %>%
     ungroup()

med_exposure <- med_exposure %>%
     left_join(picu_stay_num, by = join_by(mrn, enc_id,
                                           overlaps(x$vent_time_start, x$vent_time_stop, y$icu_start_date, y$icu_stop_date))) %>%
     relocate(mrn, enc_id, hospital_admission_date, hospital_discharge_date,
              picu_stay_num, icu_start_date, icu_stop_date,
              vent_ep_num, vent_time_start, vent_time_stop)

# Get RASS scores for all these patients
df_rass_nont21 <- df_rass %>%
     filter(enc_id %in% not_t21_enc_id) %>%
     arrange(mrn, enc_id, rass_time)

# Get CAPD scores for all these patients
df_capd_nont21 <- df_capd %>%
     filter(enc_id %in% not_t21_enc_id) %>%
     arrange(mrn, enc_id, capd_time)

# Get all of the ICD codes across time for these patients.
df_icd_nont21 <- df_icd %>% filter(enc_id %in% not_t21_enc_id)

# Save the data
saveRDS(med_exposure, paste0(data_path, '../output/non_t21_vent_med_exposures_', today(), '.rds'))
writexl::write_xlsx(med_exposure, paste0(data_path, '../output/non_t21_vent_med_exposures_', today(), '.xlsx'))

saveRDS(df_icd_nont21, paste0(data_path, '../output/non_t21_icd_', today(), '.rds'))
writexl::write_xlsx(df_icd_nont21, paste0(data_path, '../output/non_t21_icd_', today(), '.xlsx'))

saveRDS(df_icd_nont21, paste0(data_path, '../output/non_t21_icd_', today(), '.rds'))
writexl::write_xlsx(df_icd_nont21, paste0(data_path, '../output/non_t21_icd_', today(), '.xlsx'))

# *****************************************************************************
## Get data necessary for pSOFA calculation -----------------------------------
# *****************************************************************************

# Load laboratory data
df_labs <- load_labs(paste0(data_path, fname_labs))

# Get the necessary labs for scoring pSOFA
psofa_labnames <- c('platelet count, auto',
                    'po2 (arterial)',
                    'bilirubin, total',
                    'bilirubin, plasma',
                    'creatinine')

psofa_lab_renames <- c('platelets',
                       'pao2',
                       'tbili1',
                       'tbili2',
                       'creatinine')

# For some reason there are two total bilirubin labs. Need to combine them. Take the average
# in the extremely rare case when they coexist.
# Some data have the format "<300" or ">10" if values are far outside normal ranges.
# Remove the non-numeric characters, and then convert to numeric.
df_psofa_labs <- get_labs_by_type(df_labs, labnames = psofa_labnames, labvarnames = psofa_lab_renames) %>%
     mutate(across(all_of(psofa_lab_renames), ~ str_remove_all(.x, '[^0-9.]'))) %>%
     mutate(across(all_of(psofa_lab_renames), ~ str_replace_all(.x, '^$', NA_character_))) %>%
     mutate(across(all_of(psofa_lab_renames), as.numeric)) %>%
     mutate(tbili = rowMeans(select(., tbili1, tbili2), na.rm = TRUE),
            tbili = replace_na(tbili, NA)) %>%
     select(-tbili1, -tbili2)

saveRDS(df_psofa_labs, paste0(data_path, '../output/psofa_labs_', today(), '.rds'))
writexl::write_xlsx(df_psofa_labs, paste0(data_path, '../output/psofa_labs_', today(), '.xlsx'))

# Load GCS
df_gcs <-load_gcs(paste0(data_path, fname_advanced_monitoring))

saveRDS(df_gcs, paste0(data_path, '../output/gcs_', today(), '.rds'))
writexl::write_xlsx(df_psofa_labs, paste0(data_path, '../output/gcs_', today(), '.xlsx'))

