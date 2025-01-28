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
# data_path <- data_path_chony

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

# Get unique IDs based on PICU encounter (not hospital encounter)
df_picu_id <- df_picu_startstop %>%
     group_by(mrn, enc_id) %>%
     arrange(mrn, enc_id, icu_start_date) %>%
     reframe(n = row_number(),
             icu_start_date = icu_start_date,
             icu_stop_date = icu_stop_date) %>%
     unite(picu_enc, enc_id, n)

df_picu_blank <- create_blank_series(df_picu_id$picu_enc, df_picu_id$icu_start_date, df_picu_id$icu_stop_date, increment = 'hours')

# Get RASS scores for all these patients
df_rass <- load_rass(paste0(data_path, fname_sedation_delirium)) %>%
     arrange(mrn, enc_id, rass_time)
rass_intervals <- get_rass_intervals(df_rass$enc_id, df_rass$rass, df_rass$rass_time, max_inter_ep_duration = 4)
df_coma_intervals <- get_coma_intervals(rass_intervals)
saveRDS(df_rass, paste0(data_path, 'rass_', today(), '.rds'))

# Get CAPD scores for all these patients
df_capd <- load_capd(paste0(data_path, fname_sedation_delirium)) %>%
     arrange(mrn, enc_id, capd_time)
# capd_intervals <- get_capd_intervals(df_capd$mrn, df_capd$capd, df_capd$capd_time)
capd_intervals <- get_capd_intervals(df_capd$mrn, df_capd$capd, df_capd$capd_time, df_coma_intervals)

saveRDS(df_capd, paste0(data_path, 'capd_', today(), '.rds'))

# Load medication data
df_meds <- load_meds(paste0(data_path, fname_ip_meds))
df_meds_cumulative <- clean_meds(df_meds)
saveRDS(df_meds, paste0(data_path, 'meds_', today(), '.rds'))

# Get vital signs (takes some processing so multiple steps)
df_vitals <- load_vitals(paste0(data_path, fname_vitals))
df_vitals_wide <- clean_vitals(df_vitals)
saveRDS(df_vitals_wide, paste0(data_path, 'vitals_', today(), '.rds'))

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


saveRDS(df_weight, paste0(data_path, 'weight_', today(), '.rds'))
saveRDS(df_height, paste0(data_path, 'height_', today(), '.rds'))
saveRDS(df_bsa, paste0(data_path, 'bsa_', today(), '.rds'))
saveRDS(df_temp, paste0(data_path, 'temp_', today(), '.rds'))

# Get respiratory support
df_resp <- load_resp_support(paste0(data_path, fname_imv))
df_resp_wide <- clean_resp_support(df_resp)
df_resp_support_episodes <- classify_resp_support(df_resp_wide)

# Specifically get FiO2 and SpO2
df_fio2_spo2 <- load_generic_flowsheet_rows(paste0(data_path, fname_imv),
                                       key_name = 'PAT_ENC_CSN_ID',
                                       time_col = 'RECORDED_TIME',
                                       var_col = 'FLOWSHEET_MEASURE_NAME',
                                       measure_col = 'MEASURE_VALUE',
                                       varnames = c('R FIO2', 'PULSE OXIMETRY'),
                                       rename_vars = c('fio2', 'spo2'),
                                       max_load = Inf)

df_fio2_spo2 <- df_fio2_spo2 %>%
     mutate(fio2 = as.numeric(fio2),
            spo2 = as.numeric(spo2)) %>%
     filter(!is.na(fio2) & !is.na(spo2))

saveRDS(df_fio2_spo2, paste0(data_path, '../output/fio2_spo2_', today(), '.rds'))

df_encounters_with_resp_support <- inner_join(df_encounters, df_resp_support_episodes, multiple = 'all')
writexl::write_xlsx(df_encounters_with_resp_support, paste0(data_path, '../output/resp_support_', today(), '.xlsx'))

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



# saveRDS(df_vent_wide, paste0(data_path, 'vent_wide_', today(), '.rds'))
# saveRDS(df_vent_episodes, paste0(data_path, 'vent_episodes_', today(), '.rds'))
#
# df_vent_wide <- readRDS(paste0(data_path, 'vent_wide_2024-01-26.rds'))
#
# list2env(get_rds(file_path = data_path), envir = .GlobalEnv)


