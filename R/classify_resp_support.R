#' classify_resp_support
#'
#' Determine different levels of respiratory support based upon a wide-format
#' dataset of encounter IDs and variables dictating different respiratory
#' flowsheet parameters
#'
#' @param df_resp_wide A data frame. It must be in wide format and contain
#' encounter IDs (named enc_id) and different respiratory parameters. The list
#' of parameters is fixed. This data frame is usually returned by \link{clean_resp_support}.
#' @param min_ep_duration A number representing the minimum time (in hours) that
#' an episode  of respiratory support needs to be in effect for it to be counted.
#' Respiratory support episodes with duration under this level will be discarded.
#' Default is 1 hour.
#' @param max_inter_ep_duration A number representing the maximum number of hours
#' in between episodes of respiratory support that will be tolerated. Default is
#' 4 hours. Respiratory therapists usually document every 4 hours at a minimum.
#' Each episode has a duration (the minimum allowed is defined by min_ep_duration).
#' If the time between two neighboring episodes is fewer hours than
#' max_inter_ep_duration, this amount of time will be added to the duration of
#' an episode. Otherwise, the duration of an episode will only be the time
#' between when the episode starts and stops. For example, if a patient is on CPAP
#' for 12 hours, and the next recorded value is 2 hours later and represents
#' switching to BiPAP, the "duration" of CPAP will be recorded as 14 hours.
#' However, if the same patient didn't have a new recorded value for 18 hours
#' representing BiPAP, the "duration" of CPAP will be recorded as just 12 hours.
#' The time between the end of CPAP and the beginning of BiPAP will be undefined.
#' @param verbose Logical, representing whether to output current process.
#' @return A data frame in long format encounter IDs, start and stop times of
#' respiratory support, and the duration of time
#' @export
#'
classify_resp_support <- function(df_resp_wide,
                                  min_ep_duration = 1,
                                  max_inter_ep_duration = 4,
                                  verbose = TRUE) {

     # Set all variables created externally to NULL to avoid warnings when building
     amp_hfov <- bipap_rate <- cpap <- current_support <- delta_p <- enc_id <- epap <- etco2 <- freq_hfov <- ipap <- itime_niv <- itime_vent <- joingroup <-
          lda_airway <- map_vent <- niv_mode <- o2_deliv_method <- o2_flow_rate <- peep <- pip_set <- resp_meas_time <- support_change <- support_episode <-
          support_time_start <- support_time_stop <- this_duration <- timediff <- timefromlast <- timetonext <- vent_mode <- vent_type <- NULL

     if(verbose) {
          print('Cleaning and arranging data by levels of support...')
     }

     # First limit to just variables we will use to define an active ventilator
     df_resp <- df_resp_wide %>%
          select(enc_id,
                 resp_meas_time,
                 o2_deliv_method,
                 ends_with('status'),
                 amp_hfov,
                 bipap_rate,
                 cpap,
                 delta_p,
                 epap,
                 etco2,
                 freq_hfov,
                 ipap,
                 itime_niv,
                 itime_vent,
                 lda_airway,
                 map_vent,
                 niv_mode,
                 o2_flow_rate,
                 peep,
                 pip_set,
                 vent_mode,
                 vent_type,
          ) %>%
          arrange(enc_id, resp_meas_time)

     # Determine whether a ventilator is active or inactive at any given time
     # Remove any areas where there is no data
     df_resp <- df_resp %>%
          mutate(
               # Ventilator is defined as INACTIVE based on documentation
               vent_inactive = case_when(
                    vent_status == 'stopped' ~ TRUE,
                    vent_type == 'niv' ~ TRUE,
                    o2_deliv_method %in% c(
                         'aerosol mask',
                         'bi-pap mask',
                         'blow-by',
                         'bubble cpap',
                         'cpap mask',
                         'cpap prongs',
                         'face mask, humidified',
                         'face tent',
                         'high flow face mask',
                         'high flow nasal cannula',
                         'hood',
                         'mist tent',
                         'nasal cannula',
                         'nasalcpap',
                         'nasal cushion',
                         'non-rebreather mask',
                         'partial non-rebreather mark',
                         'partial rebreather mask',
                         'pediatric hood or tent',
                         'prongs',
                         'room air',
                         'simple mask',
                         # 'venti-mask',
                         'venturi mask'
                    ) ~ TRUE,
                    vent_mode %in% c('niv nava', 'niv pc', 'niv ps') ~ TRUE,
                    hfnc_status %in% c('started', 'continued') ~ TRUE,
                    bipap_status %in% c('started', 'continued') ~ TRUE,
                    bcpap_status %in% c('started', 'continued') ~ TRUE),

               # IMV is defined as ACTIVE based on documentation
               imv_active = case_when(
                    # vent_status is sometimes started/continued for CPAP or BiPAP via a vent
                    vent_status %in% c('started', 'continued') & !vent_inactive ~ TRUE,

                    # Mean airway pressure is almost always only recorded with
                    # invasive ventilation. We identified a rare exception with
                    # NIV PC mode so need to account for this.
                    map_vent > 0 & !(vent_type == 'niv' | vent_mode == 'niv pc') ~ TRUE,

                    # ETCO2 plus any other vent-specific setting defines IMV
                    etco2 > 0 & (pip_set > 0 | o2_deliv_method == 'ventilator' | map_vent > 0 | peep > 0 | delta_p > 0 | itime_vent > 0) ~ TRUE,
                    vent_type == 'invasive' ~ TRUE,
                    lda_airway == 'endotracheal tube' ~ TRUE,
                    o2_deliv_method %in% c('endotracheal tube', 'nasotreacheal tube', 't-piece') ~ TRUE,
                    vent_mode %in% c('a/c',
                                     'aprv',
                                     'hfov',
                                     'hfpv',
                                     'nava',
                                     'pc',
                                     'pc-psv/vg',
                                     'pc/simv/vg/ps',
                                     'prvc',
                                     'ps',
                                     # 'ps/cpap',
                                     'simv',
                                     'simv-pc',
                                     'simv-prvc',
                                     'simv-vc',
                                     'vc') ~ TRUE,

                    # Any HFOV setting
                    amp_hfov > 0 | freq_hfov > 0 ~ TRUE

                    # peep > 0 ~ TRUE  # Can't use this - RTs frequently record PEEP as CPAP and vice versa
               ),

               # HFOV is active - do not use this for now
               # hfov_active = if_else(amp_hfov > 0 | freq_hfov > 0, TRUE, FALSE),
               hfov_active = FALSE,

               # BiPAP is defined as ACTIVE...
               bipap_active = case_when(
                    bipap_rate > 0 ~ TRUE,
                    str_detect(niv_mode, 'bipap|niv') ~ TRUE,
                    epap > 0 & ipap > 0 & (ipap > epap) ~ TRUE,
                    itime_niv > 0 ~ TRUE,
                    # bipap_status %in% c('started', 'continued') ~ TRUE,
                    vent_mode %in% c('niv pc') ~ TRUE,
                    o2_deliv_method %in% c('bi-pap mask') ~ TRUE
               ),

               # CPAP is ACTIVE...
               cpap_active = case_when(
                    niv_mode %in% c('cpap', 'bubble cpap', 'ncpap') ~ TRUE,
                    vent_mode %in% c('nasalcpap') ~ TRUE,
                    ipap == epap ~ TRUE,
                    cpap > 0 & !imv_active ~ TRUE,
                    bcpap_status %in% c('started', 'continued') ~ TRUE,
                    o2_deliv_method %in% c('bubble cpap', 'cpap prongs', 'cpap mask', 'nasal cushion') ~ TRUE
               ),

               # HFNC is ACTIVE
               hfnc_active = case_when(
                    # Tried this earlier but it over-identified patients when they were just on traditional NC
                    # o2_deliv_method %in% c('prongs', 'nasal cannula') & o2_flow_rate >=4 ~ TRUE,
                    o2_deliv_method == 'high flow nasal cannula' ~ TRUE,
                    vent_mode == 'high flow' & o2_deliv_method %in% c('prongs', 'nasal cannula') ~ TRUE,
                    hfnc_status %in% c('started', 'continued') ~ TRUE
               ),

               # NC, facemask, or other conventional o2 delivery method is ACTIVE
               simple_o2_active = case_when(
                    o2_flow_rate < 4 ~ TRUE,
                    o2_deliv_method %in% c('face mask, humidified',
                                           'venturi mask',
                                           'cool aerosol',
                                           'blow-by',
                                           'aerosol mask',
                                           'simple mask',
                                           'non-rebreather mask',
                                           'mist tent',
                                           'partial non-rebreather mask',
                                           'face tent',
                                           'partial rebreather mask',
                                           'hood',
                                           'heated aerosol',
                                           'pediatric hood or tent') ~ TRUE
               ),

               # No support at all
               no_support_active = case_when(
                    o2_deliv_method == 'room air' & !coalesce(imv_active,
                                                              bipap_active,
                                                              cpap_active,
                                                              hfov_active,
                                                              hfnc_active,
                                                              simple_o2_active,
                                                              default = FALSE) ~ TRUE
               ),

               trach_active = case_when(
                    o2_deliv_method %in% c('trach collar mist',
                                           'trach mask',
                                           'trach tube',
                                           'transtracheal catheter',
                                           'high flow trach adaptor') ~ TRUE,
                    lda_airway == 'tracheostomy' ~ TRUE
               )
          )

     # Remove a few weird edge cases
     df_resp <- df_resp %>%
          mutate(imv_active = case_when(
               !is.na(niv_mode) & !(niv_mode %in% c('standby', 'null')) & o2_deliv_method == 'ventilator' ~ FALSE,
               TRUE ~ imv_active
          ))

     # Save one data frame for all levels of respiratory support (or its definitive absence).
     # Only keep the "active" versus "inactive" variables we created above.
     # Thenm use these new variables to hierarchically create a new variable "current_support"
     # which is a factor-type that will mutually exclusively describe the current level
     # of respiratory support.
     # Use a LOCF (last one carried forward) strategy to ensure that we don't prematurely
     # end a respiratory support episode if nothing was charted for 4 hours.
     df_resp_all <- df_resp %>%
          filter(if_any(c('imv_active', 'vent_inactive', 'hfov_active', 'bipap_active',
                          'cpap_active', 'hfnc_active', 'simple_o2_active', 'no_support_active',
                          'trach_active'), ~ !is.na(.))) %>%
          dplyr::transmute(enc_id = enc_id,
                           resp_meas_time = resp_meas_time,
                           current_support = case_when(
                                     hfov_active ~ 'hfov',
                                     imv_active ~ 'imv',
                                     bipap_active ~ 'bipap',
                                     cpap_active ~ 'cpap',
                                     hfnc_active ~ 'hfnc',
                                     simple_o2_active ~ 'simple_o2',
                                     no_support_active ~ 'room_air'
                           )
                           # # We tried using LOCF but it caused durations
                           # # to be extended too far.
                           # current_support = zoo::na.locf(
                           #      case_when(
                           #           hfov_active ~ 'hfov',
                           #           imv_active ~ 'imv',
                           #           bipap_active ~ 'bipap',
                           #           cpap_active ~ 'cpap',
                           #           hfnc_active ~ 'hfnc',
                           #           simple_o2_active ~ 'simple_o2',
                           #           no_support_active ~ 'room_air'),
                           #      na.rm = FALSE,
                           #      maxgap = 4)
          ) %>%
          filter(!is.na(current_support))

     if(verbose) {
          print('Finding changed levels of support...')
     }

     # Create a new variable indicating change in support level,
     # and another that will flag each distinct episode of support
     # For some reason the variable support_episode sometimes jumps numbers,
     # but it increases monotonically so it can be adjusted at the end to make sure it
     # increases in +1 increments.
     df_resp_all <- df_resp_all %>%
          group_by(enc_id) %>%
          mutate(
               support_change = current_support != lag(current_support, default = 'first'),
               support_episode = cumsum(support_change),
          ) %>%
          ungroup()

     # Back this up for a later merge
     df_resp_wide_all_for_merge <- df_resp_all

     if(verbose) {
          print('Finding start/stop times of changed support levels...')
     }

     # Get start/stop times for each episode of support, and calculate a duration of time
     # The duration of time spans from the beginning of the new episode, to the beginning of
     # the *next* new episode
     df_resp_all <- df_resp_all %>%
          group_by(enc_id, support_episode) %>%
          summarize(
               support_time_start = min(resp_meas_time),
               support_time_stop = max(resp_meas_time)
          ) %>%
          ungroup() %>%
          left_join(df_resp_wide_all_for_merge, join_by('enc_id', 'support_time_start' == 'resp_meas_time', 'support_episode')) %>%
          select(enc_id, support_episode, current_support, support_time_start, support_time_stop) %>%
          group_by(enc_id) %>%
          # mutate(timediff = as.duration(lead(support_time_start, default = last(support_time_stop)) - support_time_start)) %>%
          mutate(timediff = as.duration(support_time_stop - support_time_start)) %>%
          ungroup() %>%
          arrange(enc_id, support_time_start)

     if(verbose) {
          print('Cleaning episodes shorter than [min_ep_duration] hours and stitching adjacent ones together..')
     }

     # Remove episodes with a duration that is less than 1 hour
     df_resp_all <- df_resp_all %>%
          filter(timediff >= hours(min_ep_duration))

     # Removing episodes may have now juxtaposed two episodes of the same type. If this is the case,
     # join them together. Only join if there are fewer than 4 hours between episodes
     df_resp_all <- df_resp_all %>%
          group_by(enc_id) %>%
          arrange(enc_id, support_time_start) %>%
          mutate(support_change = current_support != lag(current_support, default = 'first'),
                 timefromlast = as.duration(support_time_start - lag(support_time_stop)),
                 joingroup = if_else(timefromlast < hours(max_inter_ep_duration) & !support_change, TRUE, FALSE),
                 support_episode = cumsum(support_change & !joingroup)) %>%
          ungroup() %>%
          group_by(enc_id, support_episode) %>%
          mutate(support_time_start = min(support_time_start),
                 support_time_stop = max(support_time_stop)) %>%
          ungroup() %>%
          distinct(enc_id, support_episode, current_support, support_time_start, support_time_stop)

     # Re-calculate the duration of time of each episode. If the time between two episodes is fewer than 4 hours,
     # the total episode time will be "episode duration" + "time between episodes".
     # If the time between episodes is longer than 4 hours (the usual minimum time between RT charting),
     # then the time duration of the episode is just the episode itself. This should eliminate problems
     # where nothing is charted for days, usually indicating just room air requirement, which would
     # otherwise extend the duration of the prior episode too long.
     df_resp_all <- df_resp_all %>%
          group_by(enc_id) %>%
          mutate(this_duration = as.duration(support_time_stop - support_time_start),
                 timetonext = as.duration(lead(support_time_start, default = last(support_time_stop)) - support_time_stop),
                 timediff = if_else(timetonext < hours(max_inter_ep_duration), this_duration + timetonext, this_duration)) %>%
          ungroup() %>%
          arrange(enc_id, support_time_start) %>%
          select(-this_duration, -timetonext)

     # Re-number support episodes
     df_resp_all <- df_resp_all %>%
          group_by(enc_id) %>%
          arrange(enc_id, support_time_start) %>%
          mutate(support_episode = row_number()) %>%
          ungroup()

     return(df_resp_all)

}
