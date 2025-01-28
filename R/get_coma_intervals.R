#' get_coma_intervals
#'
#' Takes data fram from \link{get_rass_intervals} and creates intervals during which
#' each patient encounter was comatose (RASS -4 or -5).
#'
#' @param id A character vector of IDs for each unique patient or encounter. Generally taken from
#' the variable PAT_ENC_CSN_ID, but can also create custom IDs. An example would be to use
#' PAT_ENC_CSN_ID but also append an identifier (01, 02, 03, etc.) for each PICU hospitalization
#' or episode of invasive mechanical ventilation.
#' @param df_rass_intervals A data frame returned by \link{get_rass_intervals}.
#'
#' @return A data frame with:
#' \itemize {
#'   \item \code{id}: The ID of the patient.
#'   \item \code{coma_time_start}: A POSIXct object when a coma episode started.
#'   \item \code{coma_time_stop}: A POSIXct object when a coma episode ended.
#'   }
#' @export
#'
#' @md
get_coma_intervals <- function(df_rass_intervals) {

     # ***************************************************************
     # Initialize variables ------------------------------------------
     # ***************************************************************
     # capd_change <- capd_episode <- capd_time_start <- capd_time_stop <-
          # timetonext <- NULL

     comavarnames <- c('id', 'rass_episode', 'rass', 'rass_time_start', 'rass_time_stop', 'rass_interval_duration')


     # ***************************************************************
     # Error-catching ------------------------------------------------
     # ***************************************************************

     # Make the correct type of data frame was sent in
     if(!all(names(df_rass_intervals) == comavarnames)) {
          stop(paste('df_rass_intervals must have column names id, rass_episode, rass_time_start, rass_time_stop, rass_interval_duration'))
     } else {
          z <- sapply(df_rass_intervals, class)
          if(!(z[[1]] == 'character' & z[[2]] == 'integer' & z[[3]] == 'integer' & z[[4]][1] == 'POSIXct' & z[[5]][1] == 'POSIXct' & z[[6]] == 'Duration')) {
               stop('columns of wrong type. Please use a data frame returned by the get_rass_intervals() function.')
          }
     }

     # ***************************************************************
     # Create the data frame -----------------------------------------
     # ***************************************************************

     df_coma_intervals <- df_rass_intervals %>%
          filter(rass <= -4) %>%
          select(id, coma_time_start = rass_time_start, coma_time_stop = rass_time_stop)

     return(df_coma_intervals)
}
