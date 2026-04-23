# Flowsheet measure name mapping for pre-7/18/2024 Epic respiratory flowsheet exports.
#
# In older data, measurements are identified by flowsheet_measure_name (character)
# rather than flowsheet_measure_id (numeric). Source this file or copy the vector
# into your project, then pass it to clean_resp_support():
#
# Usage:
#   source('path/to/resp_support_map_old.R')
#   clean_resp_support(df_resp,
#                      var_col = 'flowsheet_measure_name',
#                      var_map = resp_support_map_old)

resp_support_map_old <- c(
     'r fs ip vent delta p (amplitude) (set)'                       = 'amp_hfov',
     'nyc ip rt r $$ neonatal cpap'                                 = 'bcpap_status',
     'r fs rt bipap total rate'                                     = 'bipap_rate',
     'nyc ip rt r $$ bipap'                                         = 'bipap_status',
     'nyc ip rt r continuous positive airway pressure cpap'         = 'cpap_rt',
     'r cpap level'                                                 = 'cpap_level',
     'nyc ip r fs vent press support set above peep'                = 'delta_p',
     'nyc ip rt r niv expiratory positive airway pressure (epap)'   = 'epap',
     'nyc ip r fs rt etco2'                                         = 'etco2',
     'r fio2'                                                       = 'fio2',
     'r fs ip vent hertz (set)'                                     = 'freq_hfov',
     'nyc ip rt r $$ high flow nasal'                               = 'hfnc_status',
     'nyc ip rt r niv inspiratory positive airway pressure (ipap)'  = 'ipap',
     'nyc ip rt r niv inspiratory time'                             = 'itime_niv',
     'r vent insp time (sec)- set'                                  = 'itime_vent',
     'r fs lda airway trigger'                                      = 'lda_airway',
     'r fs vent map'                                                = 'map_vent',
     'nyc ip rt r niv mode'                                         = 'niv_mode',
     'r oxygen delivery method'                                     = 'o2_deliv_method',
     'r oxygen flow rate'                                           = 'o2_flow_rate',
     'nyc ip rt r fs vent plateau pressure'                         = 'p_plat',
     'nyc ip rt r vent peep set'                                    = 'peep',
     'r fs vent pip obs'                                            = 'pip_meas',
     'nyc ip rt r fs vent resp rate (measured)'                     = 'rr_vent_meas',
     'r fs vent resp rate (set)'                                    = 'rr_vent_set',
     'nyc ip rt r insp pressure'                                    = 'pip_set',
     'r ip vent mode'                                               = 'vent_mode',
     'r fs resp ventilator patient'                                 = 'vent_patient',
     'r nyc ip rt $$ (adult) vent'                                  = 'vent_status',
     'nyc ip rt r vent type'                                        = 'vent_type',
     'nyc ip rt r niv tidal vol exhaled'                            = 'vt_e',
     'pulse oximetry'                                               = 'spo2'
)
