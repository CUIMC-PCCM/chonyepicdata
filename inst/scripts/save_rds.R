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
saveRDS(df_picu_startstop, paste0(data_path, 'picu_start_stop_', today(), '.rds'))

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

# Get vital signs (takes some processing so multiple steps)
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

df_vitals_wide <- clean_vitals(df_vitals)

saveRDS(df_weight, paste0(data_path, 'weight_', today(), '.rds'))
saveRDS(df_height, paste0(data_path, 'height_', today(), '.rds'))
saveRDS(df_bsa, paste0(data_path, 'bsa_', today(), '.rds'))
saveRDS(df_temp, paste0(data_path, 'temp_', today(), '.rds'))
saveRDS(df_vitals_wide, paste0(data_path, 'vitals_', today(), '.rds'))

# Get ventilator support
df_vent <- load_vent(paste0(data_path, fname_imv))

# Process to make into a wide-format dataset for simultaneously recorded data
df_vent_wide <- df_vent %>%
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

get_rds <- function(file_path = getwd()) {
     # Get all file paths
     all_files <- list.files(path = file_path, pattern = "\\.rds", full.names = TRUE)

     # Read all your data into a list
     data_list <- lapply(file_paths, readRDS)

     # Assign file names to list elements
     names(data_list) <- file_names

     return(data_list)
}

list2env(get_rds(data_path), envir = .GlobalEnv)
