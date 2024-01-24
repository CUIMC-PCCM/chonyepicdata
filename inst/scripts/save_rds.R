# Create basic dataset examples for Liza

library(chonyepicdata)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)

options(scipen = 3)

# Load configuration files. You may need to edit the file (located in a config folder) with your own filepath.
# Alternately you can just send in the correct filename.
load_config(useglobal = TRUE)

# load all encounters
df_encounters <- load_encounters(paste0(data_path, fname_encounter))
saveRDS(df_encounters, paste0(data_path, 'encounters_', today(), '.rds'))

# Get all ICD codes
df_icd <- load_icd_dx(paste0(data_path, fname_icd_dx))
saveRDS(df_icd, paste0(data_path, 'icd_dx_', today(), '.rds'))

# Now get a set of all PICU start/stop datetimes for these encounters
df_picu_startstop <- get_picu_intervals(paste0(data_path, fname_adt)) %>%
     arrange(mrn, enc_id, icu_start_date)
saveRDS(df_picu_startstop, paste0(data_path, 'picu_start_stop', today(), '.rds'))

# Get RASS scores for all these patients
df_rass <- load_rass(paste0(data_path, fname_sedation_delirium)) %>%
     arrange(mrn, enc_id, rass_time)
saveRDS(df_rass, paste0(data_path, 'rass_', today(), '.rds'))

# Get CAPD scores for all these patients
df_capd <- load_capd(paste0(data_path, fname_sedation_delirium)) %>%
     arrange(mrn, enc_id, capd_time)
saveRDS(df_capd, paste0(data_path, 'capd_', today(), '.rds'))

# Load medication data
df_meds <- load_meds(paste0(data_path, fname_ip_meds))
saveRDS(df_meds, paste0(data_path, 'meds_', today(), '.rds'))

# Get vital signs
df_vitals <- load_vitals(paste0(data_path, fname_vitals))

df_weight <- df_vitals %>%
     filter(flowsheet_name %in% c('r nyc dry (dosing) weight', 'weight/scale')) %>%
     select(mrn, enc_id, weight_type = flowsheet_name, weight = meas_value, weight_time = vital_time)

df_height <- df_vitals %>%
     filter(flowsheet_name == 'height') %>%
     select(mrn, enc_id, height = meas_value, height_time = vital_time)

df_bsa <- df_vitals %>%
     filter(flowsheet_name == 'r bsa') %>%
     select(mrn, enc_id, bsa = meas_value, bsa_time = vital_time)

df_temp <- df_vitals %>%
     filter(flowsheet_name %in% c('temperature', 'temp source')) %>%
     pivot_wider(id_cols = c('enc_id', 'mrn', 'vital_time'),
                  names_from = flowsheet_name,
                  values_from = meas_value) %>%
     clean_names() %>%
     rename(temp_time = vital_time)

df_vitals_wide <- df_vitals %>%
     select(-common_name, -units, -cust_list_map_value) %>%
     filter(flowsheet_name %in% c('pulse',
                                  'blood pressure',
                                  'respirations',
                                  'pulse oximetry',
                                  'r fs arterial line blood pressure',
                                  'r fs map',
                                  'r fs map a-line',
                                  'r fs device cvp mean')) %>%
     filter(!is.na(meas_value) & !is.null(meas_value) & !(meas_value == 'null')) %>%
     pivot_wider(id_cols = c('enc_id', 'mrn', 'vital_time'),
                 names_from = flowsheet_name,
                 values_from = meas_value) %>%
     clean_names() %>%
     separate_wider_delim(col = blood_pressure, names = c('sbp_ni', 'dbp_ni'), delim = '/', too_few = 'align_start') %>%
     separate_wider_delim(col = r_fs_arterial_line_blood_pressure, names = c('sbp_art', 'ndbp_art'), delim = '/', too_few = 'align_start') %>%
     rename(map_ni = r_fs_map, map_art=r_fs_map_a_line, cvp = r_fs_device_cvp_mean)

saveRDS(df_weight, paste0(data_path, 'weight_', today(), '.rds'))
saveRDS(df_height, paste0(data_path, 'height_', today(), '.rds'))
saveRDS(df_bsa, paste0(data_path, 'bsa_', today(), '.rds'))
saveRDS(df_temp, paste0(data_path, 'temp_', today(), '.rds'))
saveRDS(df_vitals_wide, paste0(data_path, 'vitals_', today(), '.rds'))


# Get medication exposures, just for the T21 cohort, only during valid PICU times, and only for fentanyl,
# midazolam, and dexmedetomidine. Individual PICU stays will have the encounter ID, with #1, #2, #3, etc
# appended to the end to denote which PICU stay it was
med_exposure <- clean_meds(df_meds, medlist = c('fentanyl', 'midazolam', 'dexmedetomidine'),
                           time_limits = time_limits, patient_weights = df_weights)

# Separate out the number of the PICU stay
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
     select(-sex, -dob)

# Example of how to save:
writexl::write_xlsx(med_exposure, paste0(data_path, '../output/T21_med_exposure-', today(), '.xlsx'))
