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
saveRDS(df_encounters, paste0(data_path_chony, 'encounters_', today(), '.rds'))

# Get all ICD codes
df_icd <- load_icd_dx(paste0(data_path_chony, fname_icd_dx))
saveRDS(df_icd, paste0(data_path_chony, 'icd_dx_', today(), '.rds'))

# Now get a set of all PICU start/stop datetimes for these encounters
df_picu_startstop <- get_picu_intervals(paste0(data_path_chony, fname_adt)) %>%
     arrange(mrn, enc_id, icu_start_date)
saveRDS(df_picu_startstop, paste0(data_path_chony, 'picu_start_stop_', today(), '.rds'))

# Get RASS scores for all these patients
df_rass <- load_rass(paste0(data_path_chony, fname_sedation_delirium)) %>%
     arrange(mrn, enc_id, rass_time)
saveRDS(df_rass, paste0(data_path_chony, 'rass_', today(), '.rds'))

# Get CAPD scores for all these patients
df_capd <- load_capd(paste0(data_path_chony, fname_sedation_delirium)) %>%
     arrange(mrn, enc_id, capd_time)
saveRDS(df_capd, paste0(data_path_chony, 'capd_', today(), '.rds'))

# Load medication data
df_meds <- load_meds(paste0(data_path_chony, fname_ip_meds))
saveRDS(df_meds, paste0(data_path_chony, 'meds_', today(), '.rds'))

# Get vital signs (takes some processing so multiple steps)
df_vitals <- load_vitals(paste0(data_path_chony, fname_vitals))

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

df_vitals_wide <- clean_vitals(df_vitals)

saveRDS(df_weight, paste0(data_path_chony, 'weight_', today(), '.rds'))
saveRDS(df_height, paste0(data_path_chony, 'height_', today(), '.rds'))
saveRDS(df_bsa, paste0(data_path_chony, 'bsa_', today(), '.rds'))
saveRDS(df_temp, paste0(data_path_chony, 'temp_', today(), '.rds'))
saveRDS(df_vitals_wide, paste0(data_path_chony, 'vitals_', today(), '.rds'))

# Get ventilator support
df_vent <- load_vent(paste0(data_path_chony, fname_imv))
df_vent_wide <- clean_vent(df_vent)
df_vent_episodes <- get_imv_startstop(df_vent_wide)

saveRDS(df_vent_wide, paste0(data_path_chony, 'vent_wide_', today(), '.rds'))
saveRDS(df_vent_episodes, paste0(data_path, 'vent_episodes_', today(), '.rds'))

df_vent_wide <- readRDS(paste0(data_path, 'vent_wide_2024-01-26.rds'))

list2env(get_rds(file_path = data_path), envir = .GlobalEnv)


