#' get_rass_episodes
#'
#' Takes data fram from \link{load_rass} and creates intervals for each patient encounter.
#' An interval will last at most max_ep_duration if nothing else is charted.
#'

#' @param id A character vector of IDs for each unique patient or encounter. Generally taken from
#' the variable PAT_ENC_CSN_ID, but can also create custom IDs. An example would be to use
#' PAT_ENC_CSN_ID but also append an identifier (01, 02, 03, etc.) for each PICU hospitalization
#' or episode of invasive mechanical ventilation.
#' @param rass A vector of integers ranging from -5 to +4, representing the RASS for each patient.
#' @param rass_time A vector of datetime or POSIXct entries corresponding to each RASS value
#' @param max_ep_duration A number, the maximum number of hours that a particular RASS will be
#' extended in the event of a null recording. RASS may be sparsely recorded. For any recorded RASS,
#' the current RASS will be carried forward for \code{max_ep_duration} hours. After this amount
#' of time, the interval will be ended. Set this variable to 0 and no intervals will be created. Set
#' it to NA and the interval will be extended forward indefinitely until a new RASS is recorded.
#'
#' @return A data frame of IDs, RASS, and [lubridate::interval()] objects corresponding to each RASS.
#' @export
#'
#' @examples
get_rass_episodes <- function(id, rass, rass_time, max_ep_duration = 4) {

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

     # Make sure variable rass is an intrger
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
     # Function ------------------------------------------------------
     # ***************************************************************


     return(df_rass)
}
