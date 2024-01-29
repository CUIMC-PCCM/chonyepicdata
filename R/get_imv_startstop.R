#' get_imv_startstop
#'
#' Find times where invasive mechanical ventilation (IMV) starts and stops, to determine
#' the exact interval and duration of IMV.
#'
#' @param df_vent_wide A wide-format data frame of vent/respiratory settings and measurements. This should
#' be obtained using [clean_vent]
#'
#' @return A data frame in long format with start and stop times of IMV for each patient/encounter
#' @export
#'
get_imv_startstop <- function(df_vent_wide) {

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
          select(enc_id, vent_meas_time, ends_with('status'), o2_deliv_method, vent_mode, vent_type, peep) %>%
          arrange(enc_id, vent_meas_time)

     # Determine whether a ventilator is active or inactive at any given time
     # Remove any areas where there is no data
     df_vent_wide2 <- df_vent_wide %>%
          mutate(
               vent_active = case_when(
                    vent_status %in% c('continued', 'started') ~ TRUE,
                    vent_type == 'invasive' ~ TRUE,
                    o2_deliv_method %in% c('endotracheal tube', 'nasotreacheal tube', 'ventilator', 't-piece') ~ TRUE,
                    vent_mode %in% c('a/c', 'aprv', 'hfov', 'hfpv', 'nava', 'pc', 'pc-psv/vg', 'pc/simv/vg/ps', 'prvc', 'ps', 'ps/cpap', 'simv',
                                     'simv-pc', 'simv-prvc', 'simv-vc', 'vc') ~ TRUE
                    # peep > 0 ~ TRUE  # Can't use this - RTs frequently record PEEP as CPAP and vice versa
               ),
               vent_inactive = case_when(
                    vent_status == 'stopped' ~ TRUE,
                    vent_type == 'niv' ~ TRUE,
                    o2_deliv_method %in% c('aerosol mask', 'bi-pap mask', 'blow-by', 'bubble cpap', 'cpap mask', 'cpap prongs', '
                                           face mask, humidified', 'face tent', 'high flow face mask', 'high flow nasal cannula', '
                                           hood', 'mist tent', 'nasal cannula', 'nasalcpap', 'nasal cushion', 'non-rebreather mask',
                                           'partial non-rebreather mark', 'partial rebreather mask', 'pediatric hood or tent',
                                           'prongs', 'room air', 'simple mask', 'venti-mask', 'venturi mask') ~ TRUE,
                    vent_mode %in% c('niv nava', 'niv pc', 'niv ps') ~ TRUE,
                    hfnc_status %in% c('started', 'continued') ~ TRUE,
                    bipap_status %in% c('started', 'continued') ~ TRUE,
                    bcpap_status %in% c('started', 'continued') ~ TRUE),
               trach_active = case_when(
                    o2_deliv_method %in% c('trach collar mist', 'trach mask', 'trach tube', 'transtracheal catheter') ~ TRUE)
          ) %>%
          # select(enc_id, vent_meas_time, vent_active, vent_inactive) %>%
          filter(if_any(c('vent_active', 'vent_inactive'), ~ !is.na(.)))

     #
     df_vent_wide3 <- df_vent_wide2 %>%
          mutate(vent_onoff = case_when(vent_active ~ TRUE,
                                        vent_inactive ~ FALSE,
                                        TRUE ~ NA)) %>%
          group_by(enc_id) %>%
          filter(vent_onoff != lead(vent_onoff)) %>%
          mutate(timetonext = as.duration(interval(vent_meas_time, lead(vent_meas_time))))


     # Create a new variable indicating change in vent_onoff
     df_vent_wide3 <- df_vent_wide2 %>%
          mutate(vent_onoff = case_when(vent_active ~ TRUE,
                                        vent_inactive ~ FALSE,
                                        TRUE ~ NA)) %>%
          filter(!is.na(vent_onoff)) %>%
          group_by(enc_id) %>%
          mutate(
               vent_change = vent_onoff != lag(vent_onoff, default = first(vent_onoff)),
               vent_episode = cumsum(vent_change | (vent_onoff & !lag(vent_onoff, default = first(vent_onoff)))) * 1) %>%
          ungroup() %>%
          filter(vent_onoff) %>%
          group_by(enc_id, vent_episode) %>%
          summarize(
               vent_time_start = min(vent_meas_time),
               vent_time_stop = max(vent_meas_time)
          )



     return(df_vent_wide)
}
