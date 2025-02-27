# Get CAPD scores for all these patients
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

df_temp <- read_delim("C:/Users/asg2195/OneDrive - cumc.columbia.edu/Research/data/early_mobilization/Report 8D - Sedation and Delirium.txt",
                      delim = "|", escape_double = FALSE, trim_ws = TRUE, col_types = 'cccccccccc')

# *****************************************************************************
# SECTION ---------------------------------------------------------------------
# *****************************************************************************

# Load configuration files. You may need to edit the file (located in a config folder) with your own filepath.
# Alternately you can just send in the correct filename.
load_config(yml_path = 'C:/Users/asg2195/Github/early_mobilization/config/config.yml', useglobal = TRUE)
# data_path <- data_path_chony
data_path <- paste0(Sys.getenv('onedrive'), '/Research/data/early_mobilization/')

df_capd <- load_capd(paste0(data_path, fname_sedation_delirium),
                     col_mapping = list(enc_id = 'pat_enc_csn_id',
                                        mrn = 'mrn',
                                        component_name = 'display_name',
                                        component_value = 'measure_value_raw',
                                        capd_time = 'recorded_time'),
                     capd_coltypes = 'cccccccccc') %>%
     arrange(mrn, enc_id, capd_time)

df_rass <- load_rass(paste0(data_path, fname_sedation_delirium),
                     col_mapping = list(enc_id = 'pat_enc_csn_id',
                                        mrn = 'mrn',
                                        rass_id_col  = 'display_name',
                                        rass  = 'measure_value_raw',
                                        rass_time  = 'recorded_time')) %>%
     arrange(mrn, enc_id, rass_time)

df_rass_intervals <- get_rass_intervals(df_rass$enc_id, df_rass$rass, df_rass$rass_time)

df_coma_times <- get_coma_intervals(df_rass_intervals)

df_capd_rass <- full_join(df_capd, df_rass_intervals, by = join_by(x$enc_id == y$id, between(x$capd_time, y$rass_time_start, y$rass_time_stop, bounds = '[)'))) %>%
     arrange(enc_id, capd_time)

df_capd_rass %>%
     arrange(enc_id, capd_time) %>%
     filter(capd_not_indicated) %>%
     mutate(rass = factor(rass, levels = -5:4, ordered = TRUE),
            rass = fct_na_value_to_level(rass, 'None')) %>%
     # arrange(mrn, enc_id, capd_time, rass) %>%
     ggplot(aes(x = rass)) +
     geom_bar(aes(y = after_stat(prop), group = 1)) +
     labs(title = 'RASS when delirium screening marked \'unnecessary\'',
          x = 'RASS',
          y = 'Proportion')


df_capd_rass %>%
     arrange(enc_id, capd_time) %>%
     filter(capd_not_indicated) %>%
     mutate(rass = case_when(rass %in% c(-4, -5, 0) ~ 'Screening not indicated',
                             rass < 0 ~ 'Hypoactive',
                             rass > 0 ~ 'Hyperactive',
                             TRUE ~ 'RASS not checked'),
            rass = factor(rass, levels = c('Screening not indicated',
                                           'Hypoactive',
                                           'Hyperactive',
                                           'RASS not checked'),
                          ordered = TRUE)) %>%
     # arrange(mrn, enc_id, capd_time, rass) %>%
     ggplot(aes(x = rass)) +
     geom_bar(aes(y = after_stat(prop), group = 1)) +
     labs(title = 'Patient status when delirium screening marked \'unnecessary\'',
          x = NULL,
          y = 'Proportion') +
     ggpubr::theme_pubclean() +
     scale_fill_brewer(palette = 'blues')

df_capd_rass %>%
     arrange(enc_id, capd_time) %>%
     filter(capd_not_indicated) %>%
     mutate(rass = case_when(rass %in% c(-4, -5, 0) ~ 'Screening not indicated',
                             rass < 0 ~ 'RASS indicated',
                             rass > 0 ~ 'RASS indicated',
                             TRUE ~ 'RASS not checked'),
            rass = factor(rass, levels = c('Screening not indicated',
                                           'RASS indicated',
                                           'RASS not checked'),
                          ordered = TRUE)) %>%
     # arrange(mrn, enc_id, capd_time, rass) %>%
     ggplot(aes(x = rass)) +
     geom_bar(aes(y = after_stat(prop), group = 1)) +
     labs(title = 'Patient status when delirium screening marked \'unnecessary\'',
          x = NULL,
          y = 'Proportion') +
     ggpubr::theme_pubclean() +
     scale_fill_brewer(palette = 'blues')
