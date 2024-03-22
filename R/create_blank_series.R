#' create_blank_series
#'
#' Send in ID keys, each with a start and stop datetime. This will create a long-format
#' data frame of blank values. This can be used in a number of different ways with
#' sparse EHR data. An example is to create a by-hour dataset of values for the entirety
#' of a patient's hospitalization. You can join a sparse, timestamped array with
#' a blank array created here. You can then choose different methods, such as last-one
#' carried forward (LOCF), to account for missing data on an hourly basis.
#'
#' @param id A unique character identifier for each patient.
#' @param starttime A datetime identifying the beginning of the time series.
#' @param stoptime A datetime identifying the end of the time series
#' @param increment A character specifying how often to increment the series.
#' Default is 'hours'. Can use anything that \code{sec()} would accept, such as 'mins',
#' 'days', etc.
#'
#' @return A long-format data frame consisting of IDs and timestamps. The timestamp
#' column can be renamed outside of this function to be used in joins.
#'
#' @export
#'
#' @md
create_blank_series <- function(id,
                                starttime,
                                stoptime,
                                increment = 'hours') {

     # *****************************************************************************
     # Error checking --------------------------------------------------------------
     # *****************************************************************************

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

     # Make sure variable starttime is a datetime
     tryCatch({
          if(!lubridate::is.POSIXct(starttime)) {
               starttime <- lubridate::as_datetime(starttime)
          }
     },
     error = function(e) {
          cat('Error: variable \'stoptime\' must be coercible to a datetime (POSIXct) format')
          return(NULL)
     })

     # Make sure variable stoptime is a datetime
     tryCatch({
          if(!lubridate::is.POSIXct(stoptime)) {
               stoptime <- lubridate::as_datetime(stoptime)
          }
     },
     error = function(e) {
          cat('Error: variable \'stoptime\' must be coercible to a datetime (POSIXct) format')
          return(NULL)
     })

     # Make sure variable increment is a valid setting
     if(!(increment %in% c('secs', 'mins', 'hours', 'days', 'weeks', 'months')))
     {
          stop('Variable \'increment\' must be one of \'secs\', \'mins\', \'hours\', \'days\', \'weeks\', \'months\'')
          return(NULL)
     }

     # Make sure all variables have the same length
     lengths <- c(length(id), length(starttime), length(stoptime))
     if(length(unique(lengths)) > 1) {
          stop('id, starttime, and stoptime all must be the same length')
     }

     # Make sure stoptime is always later than starttime
     if(any(stoptime <= starttime))
     {
          stop('starttime must always precede stoptime')
          return(NULL)
     }

     # *****************************************************************************
     # Function --------------------------------------------------------------------
     # *****************************************************************************

     df_startstop <- tibble(id = id, starttime = starttime, stoptime = stoptime) %>%
          rowwise() %>%
          mutate(starttime = lubridate::round_date(starttime, unit = increment),
                 stoptime = lubridate::round_date(stoptime, unit = increment)) %>%
          mutate(timestamp = list(seq(from = starttime, to = stoptime, by = increment))) %>%
          unnest(timestamp) %>%
          select(id, timestamp)

     return(df_startstop)
}
