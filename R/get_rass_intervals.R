#' get_rass_intervals
#'
#' Takes data fram from \link{load_rass} and creates intervals for each patient encounter.
#' Each interval contains the Richmond Agitation Sedation Scale (RASS) score,
#' as well as the duration of time spent at that interval.
#' An interval will last at most max_inter_ep_duration if nothing else is charted.
#'

#' @param id A character vector of IDs for each unique patient or encounter. Generally taken from
#' the variable PAT_ENC_CSN_ID, but can also create custom IDs. An example would be to use
#' PAT_ENC_CSN_ID but also append an identifier (01, 02, 03, etc.) for each PICU hospitalization
#' or episode of invasive mechanical ventilation.
#' @param rass A vector of integers ranging from -5 to +4, representing the RASS for each patient.
#' @param rass_time A vector of datetime or POSIXct entries corresponding to each RASS value
#' @param max_inter_ep_duration A number, the maximum number of hours that a particular RASS will be
#' extended in the event of a null recording. RASS may be sparsely recorded. For any recorded RASS,
#' the current RASS will be carried forward for \code{max_inter_ep_duration} hours. After this amount
#' of time, the interval will be ended. Set this variable to 0 and no intervals will be created. Set
#' it to NA and the interval will be extended forward indefinitely until a new RASS is recorded.
#'
#' @return A data frame of IDs, RASS, and start/stop times of the intervals, and a duration for each.
#' @export
#'
get_rass_intervals <- function(id, rass, rass_time, max_inter_ep_duration = 4) {

     # ***************************************************************
     # Initialize variables ------------------------------------------
     # ***************************************************************
     rass_change <- rass_episode <- rass_time_start <- rass_time_stop <-
          timetonext <- NULL

     # ***************************************************************
     # Error-catching ------------------------------------------------
     # ***************************************************************

     # Make sure variable ID is a character
     tryCatch({

          if(!is.character(id)) {
               id <- as.character(id)
          }
     },
     error = function(e) {
          cat('Error: variable \'id\' must either be a character, or be coercible to a character.')
          return(NULL)
     })

     # Make sure variable rass is an integer
     tryCatch({
          if(!is.integer(rass)) {
               rass <- as.integer(rass)
          }
     },
     error = function(e) {
          cat('Error: variable \'rass\' must either be an integer, or be coercible to an integer')
          return(NULL)
     })

     # Make sure variable rass_time is a datetime
     tryCatch({
          if(!lubridate::is.POSIXct(rass_time)) {
               rass_time <- lubridate::as_datetime(rass_time)
          }
     },
     error = function(e) {
          cat('Error: variable \'rass_time\' must be coercible to a datetime (POSIXct) format')
          return(NULL)
     })

     # Make sure all variables have the same length
     lengths <- c(length(id), length(rass), length(rass_time))
     if(length(unique(lengths)) > 1) {
          stop('Error: id, rass, and rass_time all must be the same length')
     }


     # ***************************************************************
     # Create the data frame -----------------------------------------
     # ***************************************************************

     # Create the data frame to export. Remove any row with RASS equal to NA
     df_rass <- dplyr::tibble(id = id, rass = rass, rass_time = rass_time) %>%
          filter(!is.na(rass))

     # Find times where the RASS changes and number the episodes
     df_rass <- df_rass %>%
          group_by(id) %>%
          mutate(rass_change = rass != lag(rass, default = 20),
                 rass_episode = cumsum(rass_change)) %>%
          ungroup()

     # Get the start and stop of each "interval" of RASS.
     # Also find the time until the "next" interval.
     df_rass <- df_rass %>%
          group_by(id, rass_episode) %>%
          dplyr::reframe(rass_time_start = min(rass_time),
                  rass_time_stop = max(rass_time)) %>%
          group_by(id) %>%
          mutate(timetonext = (lead(rass_time_start, default = max(rass_time_stop)) - rass_time_stop)/lubridate::dhours(1)) %>%
          ungroup()

     # Update the end time of each interval to be end of this interval plus max_inter_ep_duration, or
     # else the time when the next interval started (whichever came first).
     # As a reminder, if max_inter_ep_duration is NA, then it will default to extending each interval
     # until the start of the next RASS. (The final RASS will be unchanged.)
     # If max_inter_ep_duration is zero, then no intervals are extended.
     df_rass <- df_rass %>%
          group_by(id) %>%
          mutate(rass_time_stop = pmin(rass_time_stop + hours(max_inter_ep_duration),
                                       lead(rass_time_start, default = max(rass_time_stop)), na.rm = TRUE),
                 rass_interval_duration = as.duration(interval(rass_time_start, rass_time_stop))) %>%
          select(-timetonext)


     return(df_rass)
}
