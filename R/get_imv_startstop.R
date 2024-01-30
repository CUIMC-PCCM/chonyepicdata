#' get_imv_startstop
#'
#' Find times where invasive mechanical ventilation (IMV) starts and stops, to determine
#' the exact interval and duration of IMV.
#'
#' @param df_vent_wide A wide-format data frame of vent/respiratory settings and measurements. This should
#' be obtained using [clean_vent]
#' @param min_imv_time The minimum duration of time (in hours) an episode of IMV must last for it to be counted.
#' This is designed to make sure that erroneous entries that would flag a patient as extubated will be removed.
#' Neighboring valid episodes will be joined together.
#'
#' @return A data frame in long format with start and stop times of IMV for each patient/encounter
#' @export
#'
get_imv_startstop <- function(df_vent_wide, min_imv_time = 2) {

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
     df_vent_wide <- df_vent_wide %>%
          select(enc_id,
                 vent_meas_time,
                 ends_with('status'),
                 o2_deliv_method,
                 vent_mode,
                 niv_mode,
                 vent_type,
                 lda_airway,
                 peep) %>%
          arrange(enc_id, vent_meas_time)

     # Determine whether a ventilator is active or inactive at any given time
     # Remove any areas where there is no data
     df_vent_wide <- df_vent_wide %>%
          mutate(
               vent_active = case_when(
                    vent_status %in% c('continued', 'started') ~ TRUE,
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
                                     'vc') ~ TRUE
                    # peep > 0 ~ TRUE  # Can't use this - RTs frequently record PEEP as CPAP and vice versa
               ),
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
               trach_active = case_when(
                    o2_deliv_method %in% c('trach collar mist', 'trach mask', 'trach tube', 'transtracheal catheter') ~ TRUE,
                    lda_airway == 'tracheostomy' ~ TRUE
               )
          ) %>%
          # select(enc_id, vent_meas_time, vent_active, vent_inactive) %>%
          filter(if_any(c('vent_active', 'vent_inactive'), ~ !is.na(.)))

     trach_enc_id <- df_vent_wide %>%
          filter(trach_active) %>%
          group_by(enc_id) %>%
          mutate(first_trach_datetime = min(vent_meas_time)) %>%
          ungroup() %>%
          select(enc_id, first_trach_datetime) %>%
          distinct()

     # Remove a few weird edge cases
     df_vent_wide <- df_vent_wide %>%
          mutate(vent_active = case_when(
               !is.na(niv_mode) & !(niv_mode %in% c('standby', 'null')) & o2_deliv_method == 'ventilator' ~ FALSE,
               TRUE ~ vent_active
          ))

     # Create a new variable indicating change in vent_onoff,
     # and another that will flag each episode of ventilation.
     # For some reason the variable vent_episode sometimes jumps numbers,
     # but it increases monotonically so it can be adjusted at the end to make sure it
     # increases in +1 increments.
     df_vent_wide <- df_vent_wide %>%
          mutate(vent_onoff = case_when(vent_active ~ TRUE,
                                        vent_inactive ~ FALSE,
                                        TRUE ~ NA)) %>%
          filter(!is.na(vent_onoff)) %>%
          group_by(enc_id) %>%
          mutate(
               vent_change = vent_onoff != lag(vent_onoff, default = first(vent_onoff)),
               vent_episode = cumsum(vent_change) + 1
          ) %>%
          ungroup()

     # Back this up for a later merge
     df_vent_wide_for_merge <- df_vent_wide

     # Get start/stop times for each episode of IMV.
     # Need to adjust the stop time for each episode so that it is actually the
     # start time of the "next" episode (it ends when the new one begins)
     # Finally, remove all non-vented times
     df_vent_wide <- df_vent_wide %>%
          group_by(enc_id, vent_episode) %>%
          # filter(vent_onoff) %>%
          summarize(
               vent_time_start = min(vent_meas_time),
               vent_time_stop = max(vent_meas_time)
          ) %>%
          ungroup() %>%
          left_join(df_vent_wide_for_merge, join_by('enc_id', 'vent_time_start' == 'vent_meas_time', 'vent_episode')) %>%
          select(enc_id, vent_episode, vent_onoff, vent_time_start, vent_time_stop) %>%
          group_by(enc_id) %>%
          mutate(vent_time_stop = lead(vent_time_start, default = last(vent_time_stop))) %>%
          ungroup() %>%
          arrange(enc_id, vent_time_start) %>%
          mutate(timediff = as.duration(vent_time_stop - vent_time_start)) %>%
          filter(vent_onoff) %>% # This can be removed to also add non-vented times
          select(-vent_onoff)

     # Reorganize so that the episodes of ventilation are numbered {1, 2, ..., k}
     # for each ENC_ID
     df_vent_wide <- df_vent_wide %>%
          group_by(enc_id) %>%
          arrange(enc_id, vent_episode) %>%
          mutate(vent_episode = row_number(vent_episode)) %>%
          ungroup()

     # Add in whether or not the patient had a tracheostomy, and if so, when
     # it was first recorded in the record for that encounter
     df_vent_wide <- df_vent_wide %>%
          left_join(trach_enc_id)





     return(df_vent_wide)
}
