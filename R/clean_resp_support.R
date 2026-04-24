#' clean_resp_support
#'
#' Take respiratory support file loaded by \link{load_resp_support}. Clean it a little,
#' keep standard valuable rows, and put it into wide format so that each timestamp has
#' all concurrently-recorded values.
#'
#' @param df_resp A long-form data frame of respiratory settings and measurements
#' @param var_col Name of the column in \code{df_resp} that identifies each measurement.
#'   Default: \code{'flowsheet_measure_id'} (new Epic format, post-7/18/2024). Use
#'   \code{'flowsheet_measure_name'} for older data.
#' @param var_map Named character vector mapping values in \code{var_col} to standardized
#'   output column names. When \code{NULL} (default), the built-in mapping for
#'   \code{flowsheet_measure_id} values is used.
#'
#' @return A data frame in wide format with concurrently-recorded respiratory data
#' @export
#'
clean_resp_support <- function(df_resp,
                               var_col = 'flowsheet_measure_id',
                               var_map = NULL) {

     if (is.null(var_map)) {
          var_map <- c(
               '3040102552'  = 'amp_hfov',
               '30446600302' = 'bcpap_status',
               '3040102887'  = 'bipap_rate',
               '30440020202' = 'bipap_status',
               '30421114'    = 'cpap_rt',
               '30470051201' = 'cpap_level',
               '301610'      = 'delta_p',
               '3040109849'  = 'epap',
               '7075527'     = 'etco2',
               '301550'      = 'fio2',
               '3040102551'  = 'freq_hfov',
               '304250025'   = 'hfnc_status',
               '3040109848'  = 'ipap',
               '30421123'    = 'itime_niv',
               '316161'      = 'itime_vent',
               '3040102719'  = 'lda_airway',
               '316090'      = 'map_vent',
               '30421113'    = 'niv_mode',
               '3040109305'  = 'o2_deliv_method',
               '250026'      = 'o2_flow_rate',
               '301660'      = 'p_plat',
               '30421103'    = 'peep',
               '301650'      = 'pip_meas',
               '301580'      = 'rr_vent_meas',
               '301570'      = 'rr_vent_set',
               '304021145'   = 'pip_set',
               '3040102607'  = 'vent_mode',
               '315170'      = 'vent_patient',
               '3047074320'  = 'vent_status',
               '304112350'   = 'vent_type',
               '301600'      = 'vt_e',
               '10'          = 'spo2'
          )
     }

     # Required to avoid warnings when building package
     . <- cpap_rt <- cpap_level <- resp_meas_name <- measure_value <- NULL

     # Categorize useful data and get rid of the rest
     df_resp_wide <- suppressWarnings(
          df_resp %>%
               select(any_of(c('enc_id', 'resp_meas_time', var_col, 'measure_value'))) %>%
               mutate(resp_meas_name = var_map[as.character(.data[[var_col]])]) %>%
               filter(!is.na(resp_meas_name) & !is.na(measure_value)) %>%
               select(-any_of(var_col)) %>%
               distinct() %>%
               pivot_wider(id_cols = c('enc_id', 'resp_meas_time'),
                           names_from = resp_meas_name,
                           values_from = measure_value,
                           values_fn = first) %>%
               mutate(resp_meas_time = as_datetime(resp_meas_time)) %>%
               # For some reason there are two cpap variables that almost always agree,
               # but occasionally don't. Keep the more frequently populated one, and if it doesn't have a value,
               # replace it with the other one.
               mutate(across(any_of(c('cpap_level', 'cpap_rt')), ~as.numeric(str_remove_all(.x, '\\+|\\-')))) %>%
               mutate(cpap = dplyr::coalesce(
                    if ('cpap_level' %in% names(.)) .data[['cpap_level']] else NA_real_,
                    if ('cpap_rt' %in% names(.)) .data[['cpap_rt']] else NA_real_
               )) %>%
               select(-any_of(c('cpap_level', 'cpap_rt')))
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
          mutate(across(any_of(numeric_vars), ~str_remove_all(.x, '[^0-9.]'))) %>%
          mutate(across(any_of(numeric_vars), ~if_else(.x == '', NA, .x))) %>%
          mutate(across(any_of(numeric_vars), as.numeric))

     # Remove any row where all values are NA
     df_resp_wide <- df_resp_wide %>% filter(dplyr::if_any(3:dim(df_resp_wide)[2], ~ !is.na(.)))

     return(df_resp_wide)
}
