#' clean_resp_support
#'
#' Take respiratory support file loaded by \link{load_resp_support}. Clean it a little,
#' keep standard valuable rows, and put it into wide format so that each timestamp has
#' all concurrently-recorded values.
#'
#' @param df_resp A long-form data frame of respiratory settings and measurements
#'
#' @return A data frame in wide format with concurrently-recorded respiratory data
#' @export
#'
clean_resp_support <- function(df_resp) {

     # Required to avoid warnings when building package
     cpap_rt <- cpap_level <- mrn <- measure_name <- resp_meas_name <-
          flowsheet_measure_id <- measure_value  <-  NULL

     # Categorize useful data and get rid of the rest
     df_resp_wide <- suppressWarnings(
          df_resp %>%
               select(-display_name) %>%
               mutate(resp_meas_name = case_when(
                    flowsheet_measure_id == 3040102552 ~	'amp_hfov',
                    flowsheet_measure_id == 30446600302 ~	'bcpap_status',
                    flowsheet_measure_id == 3040102887 ~	'bipap_rate',
                    flowsheet_measure_id == 30440020202 ~	'bipap_status',
                    flowsheet_measure_id == 30421114 ~	'cpap_rt',
                    flowsheet_measure_id == 30470051201 ~	'cpap_level',
                    flowsheet_measure_id == 301610 ~	'delta_p',
                    flowsheet_measure_id == 3040109849 ~	'epap',
                    flowsheet_measure_id == 7075527 ~	'etco2',
                    flowsheet_measure_id == 301550 ~	'fio2',
                    flowsheet_measure_id == 3040102551 ~	'freq_hfov',
                    flowsheet_measure_id == 304250025 ~	'hfnc_status',
                    flowsheet_measure_id == 3040109848 ~	'ipap',
                    flowsheet_measure_id == 30421123 ~	'itime_niv',
                    flowsheet_measure_id == 316161 ~	'itime_vent',
                    flowsheet_measure_id == 3040102719 ~	'lda_airway',
                    flowsheet_measure_id == 316090 ~	'map_vent',
                    flowsheet_measure_id == 30421113 ~	'niv_mode',
                    flowsheet_measure_id == 3040109305 ~	'o2_deliv_method',
                    flowsheet_measure_id == 250026 ~	'o2_flow_rate',
                    flowsheet_measure_id == 301660 ~	'p_plat',
                    flowsheet_measure_id == 30421103 ~	'peep',
                    flowsheet_measure_id == 301650 ~	'pip_meas',
                    flowsheet_measure_id == 301580 ~	'rr_vent_meas',
                    flowsheet_measure_id == 301570 ~	'rr_vent_set',
                    flowsheet_measure_id == 304021145 ~	'pip_set',
                    flowsheet_measure_id == 3040102607 ~	'vent_mode',
                    flowsheet_measure_id == 315170 ~	'vent_patient',
                    flowsheet_measure_id == 3047074320 ~	'vent_status',
                    flowsheet_measure_id == 304112350 ~ 'vent_type',
                    flowsheet_measure_id == 301600 ~	'vt_e',
                    flowsheet_measure_id == 10 ~ 'spo2'
               )) %>%
               filter(!is.na(resp_meas_name) & !is.na(measure_value)) %>%
               select(-flowsheet_measure_id) %>%
               distinct() %>%
               pivot_wider(id_cols = c('enc_id', 'resp_meas_time'),
                           names_from = resp_meas_name,
                           values_from = measure_value) %>%
               # For some reason there are two cpap variables that almost always agree,
               # but occasionally don't. Keep the more frequently populated one, and if it doesn't have a value,
               # replace it with the other one.
               mutate(cpap_level = as.numeric(str_remove_all(cpap_level, '\\+|\\-')),
                      cpap_rt = as.numeric(str_remove_all(cpap_rt, '\\+|\\-'))) %>%
               mutate(cpap = if_else(is.na(cpap_level), cpap_rt, cpap_level)) %>%
               select(-cpap_level, -cpap_rt)
     )

     numeric_vars <- c('amp_hfov',
                       'bipap_rate',
                       'cpap',
                       'delta_p',
                       'epap',
                       'etco2',
                       'fio2',
                       'freq_hfov',
                       'ipap',
                       'itime_niv',
                       'itime_vent',
                       'map_vent',
                       'o2_flow_rate',
                       'p_plat',
                       'peep',
                       'pip_meas',
                       'pip_set',
                       'rr_vent_meas',
                       'rr_vent_set',
                       'spo2',
                       'vt_e')

     # Convert columns to numeric variables where able. Explicitly remove non-numerics.
     df_resp_wide <- df_resp_wide %>%
          mutate(across(numeric_vars, ~str_remove_all(.x, '[^0-9.]'))) %>%
          mutate(across(numeric_vars, ~if_else(.x == '', NA, .x))) %>%
          mutate(across(numeric_vars, as.numeric, na.rm = TRUE))

     # Remove any row where all values are NA
     df_resp_wide <- df_resp_wide %>% filter(dplyr::if_any(3:dim(df_resp_wide)[2], ~ !is.na(.)))

     return(df_resp_wide)
}
