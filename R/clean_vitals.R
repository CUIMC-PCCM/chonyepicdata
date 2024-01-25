#' clean_vitals
#'
#' Take vitals file loaded by \link{load_vitals} and standardize it so that
#' it only includes heart rate, BP (arterial and non-invasive), respirations,
#' SpO2, cvp. Data will be output in wide-format with vitals recorded at the same time
#' taking up the same row.
#'
#' @param df_vitals A long-form data frame of vitals.
#'
#' @return A data frame in wide format with concurrently-recorded vital signs
#' @export
#'
clean_vitals <- function(df_vitals) {

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
          rename(map_ni = r_fs_map,
                 map_art=r_fs_map_a_line,
                 cvp = r_fs_device_cvp_mean,
                 spo2 = pulse_oximetry,
                 hr = pulse, resp = respirations)

     return(df_vitals_wide)
}
