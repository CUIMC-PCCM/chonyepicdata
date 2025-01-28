# Create the data frame to export. Remove any row with NA data.
# Also remove negative numbers
df_capd_temp <- df_capd %>%
     filter(!is.na(capd)) %>%
     filter(capd >= 0) %>%
     select(id = enc_id, capd, capd_time) %>%
     arrange(id, capd_time)

# Remove any CAPD measures that were taken during periods of coma
# if(!is.null(coma_times)) {
     anti_capd_coma_join <- join_by(id, between(x$capd_time, y$coma_time_start, y$coma_time_stop, bounds = '()'))
     df_capd_temp <- anti_join(df_capd_temp, df_coma_times, by = anti_capd_coma_join) %>%
          distinct()
# }

# Label times of delirium
df_capd_temp <- df_capd_temp %>%
     mutate(delirious = capd >= 9)

# Find times where the CAPD changes and number the episodes
df_capd_temp <- df_capd_temp %>%
     group_by(id) %>%
     mutate(delirium_change = delirious != lag(delirious, default = FALSE),
            delirium_episode = cumsum(delirium_change*1)) %>%
     ungroup()

# Get the start and stop of each "interval" of CAPD.
# Also find the time until the "next" interval.
df_capd_temp <- df_capd_temp %>%
     group_by(id, delirium_episode) %>%
     reframe(delirious = as.logical(max(delirious)),
             delirium_time_start = min(capd_time),
             delirium_time_stop = max(capd_time)) %>%
     group_by(id) %>%
     mutate(timetonext = (lead(delirium_time_start, default = max(delirium_time_stop)) - delirium_time_stop)/dhours(1)) %>%
     ungroup()

# Update the end time of each interval to be end of this interval plus max_inter_ep_duration, or
# else the time when the next interval started (whichever came first).
# As a reminder, if max_inter_ep_duration is NA, then it will default to extending each interval
# until the start of the next CAPD. (The final CAPD will be unchanged.)
# If max_inter_ep_duration is zero, then no intervals are extended.
df_capd_temp <- df_capd_temp %>%
     group_by(id) %>%
     mutate(delirium_time_stop = pmin(delirium_time_stop + hours(12),
                                  lead(delirium_time_start, default = max(delirium_time_stop)), na.rm = TRUE),
            # delirium_interval_duration = as.duration(interval(delirium_time_start, delirium_time_stop))
            ) %>%
     select(-timetonext) %>%
     ungroup()

# Join delirium intervals with comatose intervals. A full join using overlap
# will ensure that all rows in X and Y (delirium and coma datasets) will be
# present. However, the overlap ensures that if the interval of coma overlaps at
# all with delirium, then they will be joined to a single row. This means that
# we can then make some rules about which type of interval supercedes another.
# capd_rass_join <- join_by(id == id, overlaps(x$delirium_time_start, x$delirium_time_stop, y$coma_time_start, y$coma_time_stop, bounds = '[)'))
# df_capd_coma <- full_join(df_capd_temp, df_coma_times, by = capd_rass_join)

# Now we have overlapping intervals in the same row. We need to make it so that, whenever delirium and
# df_capd_coma <- df_capd_coma %>%






#
# # If the coma_times data frame was sent, we should update to make sure we note
# # when the patient was in a coma, and therefore the CAPD is undefined
# if(!is.null(coma_times)) {
#
#      df_capd_temp <- df_capd_temp %>%
#           left_join(df_coma_times, by = 'id', relationship = 'many-to-many') %>%
#           filter(!(capd_time %within% interval(coma_time_start, coma_time_stop))) %>%
#           select(id, capd, capd_time) %>%
#           distinct()
#
#      # Update the time duration measurements when the patient was comatose
#      df_capd2 <- bind_rows(df_capd, coma_times) %>%
#           group_by(id) %>%
#           arrange(id, capd_time_start) %>%
#           mutate(capd_episode = row_number()) %>%
#           ungroup() %>%
#           mutate(capd_interval_duration = tidyr::replace_na(as.duration(interval(capd_time_start, capd_time_stop))))
# }
