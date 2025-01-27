# *****************************************************************************
# Libraries -------------------------------------------------------------------
# *****************************************************************************
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(stringr)
library(forcats)
library(readxl)
library(writexl)
library(gtsummary)
library(chonyepicdata)

# *****************************************************************************
# SECTION ---------------------------------------------------------------------
# *****************************************************************************


options(scipen = 3)

setwd('C:/Users/asg2195/Documents/GitHub/trisomy21_sedation/')

# Load configuration files. You may need to edit the file (located in a config folder) with your own filepath.
# Alternately you can just send in the correct filename.
load_config(yml_path = 'config/config.yml', useglobal = TRUE)
# data_path <- data_path_chony
data_path <- paste0(Sys.getenv('onedrive'), data_path)

df_encounters <- load_encounters(paste0(data_path, fname_encounter))

df_picu_startstop <- get_picu_intervals(paste0(data_path, fname_adt)) %>%
     arrange(mrn, enc_id, icu_start_date) %>%
     group_by(mrn) %>%
     filter(icu_start_date == min(icu_start_date)) %>%
     ungroup()

df_rass <- load_rass(paste0(data_path, fname_sedation_delirium)) %>%
     filter(enc_id %in% df_picu_startstop$enc_id)

# Get CAPD scores for all these patients
df_capd <- load_capd(paste0(data_path, fname_sedation_delirium)) %>%
     filter(enc_id %in% df_picu_startstop$enc_id) %>%
     arrange(mrn, enc_id, capd_time)

# Setting max_inter_ep_duration to NA means we use a last one carried forward (LOCF) approach
df_rass_interval <- get_rass_intervals(df_rass$enc_id, df_rass$rass, df_rass$rass_time, max_inter_ep_duration = NA)

df_coma_times <- df_rass_interval %>% filter(rass < -3) %>%
     select(id, coma_time_start = rass_time_start, coma_time_stop = rass_time_stop)

df_capd_intervals <- get_capd_intervals(id = df_capd$enc_id,
                                        capd = df_capd$capd,
                                        capd_time = df_capd$capd_time,
                                        coma_times = df_coma_times,
                                        max_inter_ep_duration = NA)
