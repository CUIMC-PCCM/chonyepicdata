#' get_imv_startstop
#'
#' Find times where invasive mechanical ventilation (IMV) starts and stops, to determine
#' the exact interval and duration of IMV.
#'
#' @param df_vent_wide A wide-format data frame of vent/respiratory settings and measurements. This should
#' be obtained using [clean_vent]
#' @param min_inter_ep_duration The minimum duration of time (in hours) between episodes of respiratory support
#' for them to be counted as separate. This is designed to make sure that erroneous entries that would flag a patient
#' as extubated will be removed. Neighboring valid episodes will be joined together.
#' @param min_ep_duration Minimum duration of time (in hours) to count as a period of continuous ventilation. Default
#' is 12 hours. Shorter episodes will be removed.
#'
#' @return A data frame in long format with start and stop times of IMV for each patient/encounter
#' @export
#'
get_imv_startstop <- function(df_vent_wide, min_inter_ep_duration = 2, min_ep_duration = 12) {

     # Required to avoid warnings when building package
     enc_id <- vent_meas_time <- o2_deliv_method <- vent_mode <- niv_mode <-
          vent_type <- lda_airway <- peep <- trach_active <- first_trach_datetime <- vent_onoff <-
          vent_change <- vent_episode <- vent_time_start <- vent_time_stop <- timediff <- etco2 <-
          amp_hfov <- bipap_rate <- cpap <- delta_p <- epap <- freq_hfov <- ipap <- itime_niv <-
          itime_vent <- map_vent <- o2_flow_rate <- pip_set <- current_support <- NULL

     # # Vent variables
     # amp_hfov
     # bcpap_status
     # bipap_rate
     # bipap_status
     # cpap
     # delta_p
     # enc_id
     # epap
     # etco2
     # fio2
     # freq_hfov
     # hfnc_status
     # ipap
     # itime_niv
     # itime_vent
     # lda_airway
     # map_vent
     # niv_mode
     # o2_deliv_method
     # o2_flow_rate
     # p_plat
     # peep
     # pip_meas
     # pip_set
     # rr_vent_meas
     # rr_vent_set
     # vent_meas_time
     # vent_mode
     # vent_patient
     # vent_status
     # vt_e

     # # Vent modes
     # a/c
     # aprv
     # asv
     # automode
     # bilevel
     # bivent
     # hfov
     # hfpv
     # high flow
     # jet
     # nasalcpap
     # nava
     # niv nava
     # niv pc
     # niv ps
     # npv
     # null
     # pc
     # pc-psv/vg
     # pc/ac/vg
     # pc/simv/vg/ps
     # prvc
     # ps
     # ps/cpap
     # simv
     # simv-pc
     # simv-prvc
     # simv-vc
     # spontaneous
     # standby
     # vc

     # First limit to just variables we will use to define an active ventilator
     df_vent_temp <- df_vent_wide %>%
          select(enc_id,
                 vent_meas_time,
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
          arrange(enc_id, vent_meas_time)

     # Determine whether a ventilator is active or inactive at any given time
     # Remove any areas where there is no data
     df_vent_temp <- df_vent_temp %>%
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
                    vent_status %in% c('started', 'continued') & !vent_inactive ~ TRUE, # vent_status is sometimes
                    # started/continued for CPAP or BiPAP via a vent

                    # ETCO2 plus any other vent-specific setting defines IMV
                    etco2 > 0 & (pip_set > 0 | o2_deliv_method == 'ventilator' | map_vent > 0 | peep > 0 | delta_p > 0 | itime_vent > 0) ~ TRUE,
                    vent_type == 'invasive' ~ TRUE,
                    lda_airway == 'endotracheal tube' ~ TRUE,
                    o2_deliv_method %in% c('endotracheal tube', 'nasotreacheal tube', 'ventilator', 't-piece') ~ TRUE,
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

               # HFOV is active
               hfov_active = if_else(amp_hfov > 0 | freq_hfov > 0, TRUE, FALSE),

               # BiPAP is defined as ACTIVE...
               bipap_active = case_when(
                    bipap_rate > 0 ~ TRUE,
                    str_detect(niv_mode, 'bipap|niv') ~ TRUE,
                    epap > 0 ~ TRUE,
                    ipap > 0 ~ TRUE,
                    itime_niv > 0 ~ TRUE,
                    bipap_status %in% c('started', 'continued') ~ TRUE,
                    vent_mode %in% c('niv pc') ~ TRUE
               ),

               # CPAP is ACTIVE...
               cpap_active = case_when(
                    niv_mode %in% c('cpap', 'bubble cpap', 'ncpap') ~ TRUE,
                    vent_mode %in% c('nasalcpap') ~ TRUE,
                    cpap > 0 & !imv_active ~ TRUE,
                    bcpap_status %in% c('started', 'continued') ~ TRUE,
                    o2_deliv_method %in% c('bubble cpap') ~ TRUE
               ),

               # HFNC is ACTIVE
               hfnc_active = case_when(
                    o2_deliv_method %in% c('prongs', 'nasal cannula') & o2_flow_rate >=4 ~ TRUE,
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
     df_vent_temp <- df_vent_temp %>%
          mutate(imv_active = case_when(
               !is.na(niv_mode) & !(niv_mode %in% c('standby', 'null')) & o2_deliv_method == 'ventilator' ~ FALSE,
               TRUE ~ imv_active
          ))

     # Save one file for all levels of respiratory support (or its definitive absence)
     df_vent_temp_all <- df_vent_temp %>%
          filter(if_any(c('imv_active', 'vent_inactive', 'hfov_active', 'bipap_active',
                          'cpap_active', 'hfnc_active', 'simple_o2_active', 'no_support_active',
                          'trach_active'), ~ !is.na(.))) %>%
          dplyr::transmute(enc_id = enc_id,
                    vent_meas_time = vent_meas_time,
                    current_support = zoo::na.locf(
                         case_when(
                              hfov_active ~ 'hfov',
                              imv_active ~ 'imv',
                              bipap_active ~ 'bipap',
                              cpap_active ~ 'cpap',
                              hfnc_active ~ 'hfnc',
                              simple_o2_active ~ 'simple_o2',
                              no_support_active ~ 'room_air'),
                         na.rm = FALSE,
                         maxgap = min_inter_ep_duration)
          ) %>%
          filter(!is.na(current_support))

     # Get all levels of respiratory support (or its definitive absence)
     df_vent_temp <- df_vent_temp %>%
          # select(enc_id, vent_meas_time, imv_active, vent_inactive) %>%
          filter(if_any(c('imv_active', 'vent_inactive'), ~ !is.na(.)))

     # Save tracheostomy encounters for later use
     trach_enc_id <- df_vent_temp %>%
          filter(trach_active) %>%
          group_by(enc_id) %>%
          mutate(first_trach_datetime = min(vent_meas_time)) %>%
          ungroup() %>%
          select(enc_id, first_trach_datetime) %>%
          distinct()

     # Create a new variable indicating change in vent_onoff,
     # and another that will flag each episode of ventilation.
     # For some reason the variable vent_episode sometimes jumps numbers,
     # but it increases monotonically so it can be adjusted at the end to make sure it
     # increases in +1 increments.
     df_vent_temp <- df_vent_temp %>%
          mutate(vent_onoff = case_when(imv_active ~ TRUE,
                                        vent_inactive ~ FALSE,
                                        TRUE ~ NA)) %>%
          filter(!is.na(vent_onoff)) %>%
          group_by(enc_id) %>%
          mutate(
               vent_change = vent_onoff != lag(vent_onoff, default = dplyr::first(vent_onoff)),
               vent_episode = cumsum(vent_change) + 1
          ) %>%
          ungroup()

     # Back this up for a later merge
     df_vent_wide_for_merge <- df_vent_temp

     # Get start/stop times for each episode of IMV, and calculate a duration of time
     # The duration of time spans from the beginning of the new episode, to the beginning of
     # the *next* new episode
     df_vent_temp <- df_vent_temp %>%
          group_by(enc_id, vent_episode) %>%
          summarize(
               vent_time_start = min(vent_meas_time),
               vent_time_stop = max(vent_meas_time)
          ) %>%
          ungroup() %>%
          left_join(df_vent_wide_for_merge, join_by('enc_id', 'vent_time_start' == 'vent_meas_time', 'vent_episode')) %>%
          select(enc_id, vent_episode, vent_onoff, vent_time_start, vent_time_stop) %>%
          group_by(enc_id) %>%
          # mutate(vent_time_stop = lead(vent_time_start, default = last(vent_time_stop))) %>%
          mutate(timediff = as.duration(lead(vent_time_stop, default = last(vent_time_stop)) - vent_time_start)) %>%
          ungroup() %>%
          arrange(enc_id, vent_time_start)

     # The purpose of this section is to remove intervals between episodes of respiratory support that are
     # "too short", meaning their time is less than min_interep_duration. We will
     #   1. Filter so that we only include the "vented" episodes where vent_onoff is TRUE
     #   2. Find the amount of time from the end of each episode of ventilation, until
     #      the beginning of the next one.
     #   3. If the amount of time between episodes is less than min_interep_duration (default 2 hours),
     #      then remove it's episode_number (set to NA). The first row must always be equal to 1, however.
     #   4. "Fill" episode_number, moving down. This will cause episodes with brief intervals of
     #       time between them (brief meaning less than min_interep_duration) to take on the same episode_number
     #       as the preceding episode.
     #   5. Recalculate start/stop times based upon the new groups. The start is the minimum of vent_time_start,
     #      and the stop is the maximum of vent_time_stop.
     #   6. Recalculate timediff, which is the duration of time of the interval. Note that we can no longer
     #      use the last time of timediff as the start of the next interval, since we have removed short
     #      intervals and non-vented times where vent_onoff=FALSE between vent_episodes. This means that
     #      the next interval might be a lot further in the future.
     #      in the future so we would be inflating
     df_vent_temp <- df_vent_temp %>%
          group_by(enc_id) %>%
          filter(vent_onoff) %>%
          mutate(timetonext = as.duration(lead(vent_time_start, default = last(vent_time_stop)) - vent_time_stop),
                 vent_episode = case_when(
                      lag(timetonext) < hours(min_inter_ep_duration) ~ NA,
                      TRUE ~ vent_episode)) %>%
          mutate(vent_episode = ifelse(row_number() == n() & is.na(vent_episode), 1, vent_episode)) %>%
          fill(vent_episode) %>%
          ungroup() %>% group_by(enc_id, vent_episode) %>%
          summarize(
               # vent_onoff = dplyr::first(vent_onoff),
               vent_time_start = min(vent_time_start),
               vent_time_stop = max(vent_time_stop)
          ) %>%
          ungroup()

     # Recalculate timediff (the duration of the episode). Remove any episodes with
     # timediff shorter than min_ep_duration, where default is 24 hours
     df_vent_temp <- df_vent_temp %>%
          mutate(timediff = as.duration(vent_time_stop - vent_time_start)) %>%
          filter(timediff >= hours(min_ep_duration))

     # Reorganize so that the episodes of ventilation are numbered {1, 2, ..., k}
     # for each ENC_ID
     df_vent_temp <- df_vent_temp %>%
          group_by(enc_id) %>%
          arrange(enc_id, vent_episode) %>%
          mutate(vent_episode = row_number(vent_episode)) %>%
          ungroup()

     # Add in whether or not the patient had a tracheostomy, and if so, when
     # it was first recorded in the record for that encounter
     df_vent_episodes <- df_vent_temp %>%
          left_join(trach_enc_id)

     return(df_vent_episodes)
}
