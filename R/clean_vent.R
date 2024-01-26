#' clean_vent
#'
#' Take ventilator/respiratory file loaded by \link{load_vent}. Clean it a little,
#' keep standard valuable rows, and put it into wide format so that each timestamp has
#' all concurrently-recorded values.
#'
#' @param df_vent A long-form data frame of vent/respiratory settings and measurements
#'
#' @return A data frame in wide format with concurrently-recorded respiratory data
#' @export
#'
clean_vent <- function(df_vent) {

     # Required to avoid warnings when building package
     cpap_rt <- cpap_level <- mrn <- measurement_name <- vent_meas_name <-
          flowsheet_measure_name <- measure_value  <-  NULL

     # Categorize useful data and get rid of the rest
     df_vent_wide <- df_vent %>%
          select(-measurement_name) %>%
          mutate(vent_meas_name = case_when(
               flowsheet_measure_name == 'r fs ip vent delta p (amplitude) (set)' ~	'amp_hfov',
               flowsheet_measure_name == 'nyc ip rt r $$ neonatal cpap' ~	'bcpap_status',
               flowsheet_measure_name == 'r fs rt bipap total rate' ~	'bipap_rate',
               flowsheet_measure_name == 'nyc ip rt r $$ bipap' ~	'bipap_status',
               flowsheet_measure_name == 'nyc ip rt r continuous positive airway pressure cpap' ~	'cpap_rt',
               flowsheet_measure_name == 'r cpap level' ~	'cpap_level',
               flowsheet_measure_name == 'nyc ip r fs vent press support set above peep' ~	'delta_p',
               flowsheet_measure_name == 'nyc ip rt r niv expiratory positive airway pressure (epap)' ~	'epap',
               flowsheet_measure_name == 'nyc ip r fs rt etco2' ~	'etco2',
               flowsheet_measure_name == 'r fio2' ~	'fio2',
               flowsheet_measure_name == 'r fs ip vent hertz (set)' ~	'freq_hfov',
               flowsheet_measure_name == 'nyc ip rt r $$ high flow nasal' ~	'hfnc_status',
               flowsheet_measure_name == 'nyc ip rt r niv inspiratory positive airway pressure (ipap)' ~	'ipap',
               flowsheet_measure_name == 'nyc ip rt r niv inspiratory time' ~	'itime_niv',
               flowsheet_measure_name == 'r vent insp time (sec)- set' ~	'itime_vent',
               flowsheet_measure_name == 'r fs lda airway trigger' ~	'lda_airway',
               flowsheet_measure_name == 'r fs vent map' ~	'map_vent',
               flowsheet_measure_name == 'nyc ip rt r niv mode' ~	'niv_mode',
               flowsheet_measure_name == 'r oxygen delivery method' ~	'o2_deliv_method',
               flowsheet_measure_name == 'r oxygen flow rate' ~	'o2_flow_rate',
               flowsheet_measure_name == 'nyc ip rt r fs vent plateau pressure' ~	'p_plat',
               flowsheet_measure_name == 'nyc ip rt r vent peep set' ~	'peep',
               flowsheet_measure_name == 'r fs vent pip obs' ~	'pip_meas',
               flowsheet_measure_name == 'nyc ip rt r fs vent resp rate (measured)' ~	'rr_vent_meas',
               flowsheet_measure_name == 'r fs vent resp rate (set)' ~	'rr_vent_set',
               flowsheet_measure_name == 'nyc ip rt r insp pressure' ~	'pip_set',
               flowsheet_measure_name == 'r ip vent mode' ~	'vent_mode',
               flowsheet_measure_name == 'r fs resp ventilator patient' ~	'vent_patient',
               flowsheet_measure_name == 'r nyc ip rt $$ (adult) vent' ~	'vent_status',
               flowsheet_measure_name == 'nyc ip rt r niv tidal vol exhaled' ~	'vt_e'
          )) %>%
          filter(!is.na(vent_meas_name) & !is.na(measure_value)) %>%
          select(-flowsheet_measure_name) %>%
          distinct() %>%
          pivot_wider(id_cols = c('enc_id', 'vent_meas_time'),
                      names_from = vent_meas_name,
                      values_from = measure_value) %>%
          # For some reason there are two cpap variables that almost always agree,
          # but occasionally don't. Keep the more frequently populated one, and if it doesn't have a value,
          # replace it with the other one.
          mutate(across(starts_with('cpap'), ~ifelse(is.null(.), NA, .))) %>%
          mutate(cpap = if_else(is.na(cpap_level), cpap_rt, cpap_level)) %>%
          select(-cpap_level, -cpap_rt)

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
                       'rr_vent_meas',
                       'rr_vent_set',
                       'pip_set',
                       'vt_e')

     # Convert columns to numeric variables where able
     df_vent_wide <- df_vent_wide %>%
          mutate(across(numeric_vars, as.numeric))

     return(df_vent_wide)
}
