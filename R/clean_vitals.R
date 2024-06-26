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

     # *****************************************************************************
     # Initialize variables --------------------------------------------------------
     # *****************************************************************************

     mrn <- enc_id <- vital_time <- hr <- sbp_ni <- dbp_ni <- map_ni <- sbp_art <-
          dbp_art <- map_art <- resp <- spo2 <- cvp <- NULL

     # *****************************************************************************
     # Function --------------------------------------------------------------------
     # *****************************************************************************


     # Required to avoid warnings when building package
     common_name <- cust_list_map_value <- flowsheet_name <- meas_value <-
          blood_pressure <- r_fs_arterial_line_blood_pressure <- r_fs_map <-
          r_fs_map_a_line <- r_fs_device_cvp_mean <- pulse_oximetry <-
          pulse <- respirations  <-  NULL

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
          separate_wider_delim(cols = blood_pressure, names = c('sbp_ni', 'dbp_ni'), delim = '/', too_few = 'align_start') %>%
          separate_wider_delim(cols = r_fs_arterial_line_blood_pressure, names = c('sbp_art', 'dbp_art'), delim = '/', too_few = 'align_start') %>%
          rename(map_ni = r_fs_map,
                 map_art=r_fs_map_a_line,
                 cvp = r_fs_device_cvp_mean,
                 spo2 = pulse_oximetry,
                 hr = pulse,
                 resp = respirations) %>%
          mutate(across(c(4:dplyr::last_col()), as.numeric)) %>%
          select(mrn, enc_id, vital_time, hr, sbp_ni, dbp_ni, map_ni, sbp_art, dbp_art, map_art, resp, spo2, cvp)

     # Add in MAP if it was not calculated
     df_vitals_wide <- df_vitals_wide %>%
          mutate(map_ni = round(dplyr::coalesce(map_ni, (1/3*sbp_ni + 2/3*dbp_ni))),
                 map_art = round(dplyr::coalesce(map_art, (1/3*sbp_art + 2/3*dbp_art))))

     return(df_vitals_wide)
}
