

# Save one data frame for all levels of respiratory support (or its definitive absence).
# Only keep the "active" versus "inactive" variables we created above.
# Thenm use these new variables to hierarchically create a new variable "current_support"
# which is a factor-type that will mutually exclusively describe the current level
# of respiratory support.
# Use a LOCF (last one carried forward) strategy to ensure that we don't prematurely
# end a respiratory support episode if nothing was charted for 4 hours.
df_vent_temp_all <- df_vent_temp %>%
     filter(if_any(c('imv_active', 'vent_inactive', 'hfov_active', 'bipap_active',
                     'cpap_active', 'hfnc_active', 'simple_o2_active', 'no_support_active',
                     'trach_active'), ~ !is.na(.))) %>%
     transmute(enc_id = enc_id,
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
                    maxgap = 4)
     ) %>%
     filter(!is.na(current_support))

# Create a new variable indicating change in support level,
# and another that will flag each distinct episode of support
# For some reason the variable support_episode sometimes jumps numbers,
# but it increases monotonically so it can be adjusted at the end to make sure it
# increases in +1 increments.
df_vent_temp_all <- df_vent_temp_all %>%
     group_by(enc_id) %>%
     mutate(
          support_change = current_support != lag(current_support, default = dplyr::first(current_support)),
          support_change = if_else(row_number() == 1, TRUE, support_change),
          support_episode = cumsum(support_change),
     ) %>%
     ungroup()

# Back this up for a later merge
df_vent_wide_all_for_merge <- df_vent_temp_all

# Get start/stop times for each episode of support, and calculate a duration of time
# The duration of time spans from the beginning of the new episode, to the beginning of
# the *next* new episode
df_vent_temp_all <- df_vent_temp_all %>%
     group_by(enc_id, support_episode) %>%
     summarize(
          support_time_start = min(vent_meas_time),
          support_time_stop = max(vent_meas_time)
     ) %>%
     ungroup() %>%
     left_join(df_vent_wide_all_for_merge, join_by('enc_id', 'support_time_start' == 'vent_meas_time', 'support_episode')) %>%
     select(enc_id, support_episode, current_support, support_time_start, support_time_stop) %>%
     group_by(enc_id) %>%
     # mutate(vent_time_stop = lead(vent_time_start, default = last(vent_time_stop))) %>%
     mutate(timediff = as.duration(lead(support_time_start, default = last(support_time_stop)) - support_time_start)) %>%
     ungroup() %>%
     arrange(enc_id, support_time_start)

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
df_vent_temp_all2 <- df_vent_temp_all %>%
     group_by(enc_id) %>%
     # filter(support_onoff) %>%
     mutate(timetonext = as.duration(lead(support_time_start, default = last(support_time_stop)) - support_time_stop),
            support_episode = case_when(
                 lag(timetonext) < hours(min_inter_ep_duration) ~ NA,
                 TRUE ~ support_episode)) %>%
     mutate(support_episode = ifelse(row_number() == n() & is.na(support_episode), 1, support_episode)) %>%
     fill(support_episode) %>%
     ungroup() %>% group_by(enc_id, support_episode) %>%
     summarize(
          support_time_start = min(support_time_start),
          support_time_stop = max(support_time_stop)
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
