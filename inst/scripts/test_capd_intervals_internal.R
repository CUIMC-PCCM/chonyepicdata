# Create the data frame to export. Remove any row with NA data.
# Also remove negative numbers
df_capd_temp <- df_capd %>%
     filter(!is.na(capd)) %>%
     filter(capd >= 0) %>%
     select(id = enc_id, capd, capd_time) %>%
     arrange(id, capd_time)

# If the coma_times data frame was sent, we should update to make sure we note
# when the patient was in a coma, and therefore the CAPD is undefined
if(!is.null(coma_times)) {

     df_capd2 <- df_capd_temp %>%
          left_join(df_coma_times, by = 'id', relationship = 'many-to-many') %>%
          filter(!(capd_time %within% interval(coma_time_start, coma_time_stop))) %>%
          select(id, capd, capd_time) %>%
          distinct()

     # Update the time duration measurements when the patient was comatose
     df_capd2 <- bind_rows(df_capd, coma_times) %>%
          group_by(id) %>%
          arrange(id, capd_time_start) %>%
          mutate(capd_episode = row_number()) %>%
          ungroup() %>%
          mutate(capd_interval_duration = tidyr::replace_na(as.duration(interval(capd_time_start, capd_time_stop))))
}
