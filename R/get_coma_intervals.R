#' get_coma_intervals
#'
#' Takes data fram from \link{get_rass_intervals} and creates intervals during which
#' each patient encounter was comatose (RASS -4 or -5).
#'
#' @param df_rass_intervals A data frame returned by \code{\link{get_rass_intervals}}.
#'
#' @return A data frame with:
#' \itemize{
#'   \item \code{id}: The ID of the patient.
#'   \item \code{coma_time_start}: A POSIXct object when a coma episode started.
#'   \item \code{coma_time_stop}: A POSIXct object when a coma episode ended.
#'   }
#'
#' @export
#'
get_coma_intervals <- function(df_rass_intervals) {

     # ***************************************************************
     # Initialize variables ------------------------------------------
     # ***************************************************************
     capd_change <- capd_episode <- capd_time_start <- capd_time_stop <-
          timetonext <- id <- rass <- rass_time_start <- rass_time_stop <-
          coma_time_start <- coma_time_stop  <-  NULL

     comavarnames <- c('id', 'rass_episode', 'rass', 'rass_time_start', 'rass_time_stop', 'rass_interval_duration')


     # ***************************************df_rass_intervals************************
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
          select(id, coma_time_start = rass_time_start, coma_time_stop = rass_time_stop) %>%
          arrange(id, coma_time_start)

     return(df_coma_intervals)
}
