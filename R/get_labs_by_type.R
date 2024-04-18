#' get_labs_by_type
#'
#' Utility function. Returns all labs of a given type or types.
#'
#' @param df_labs A data frame of lab values
#' @param labnames Character vector of the lab name(s) that you want to filter on.
#' @param labvarnames Character vector that we will use to name columns.
#'
#' \code{labnames} and \code{labvarnames} must have the same length.
#'
#'
#' @return A data frame of filtered lab values, in wide format at each timepoint.
#' @export
#'


get_labs_by_type <- function(df_labs, labnames = NULL, labvarnames = NULL) {

     # *****************************************************************************
     # Variable initiation ---------------------------------------------------------
     # *****************************************************************************

     common_name <- enc_id <- specimen_taken_time <- NULL

     # *****************************************************************************
     # Error handling --------------------------------------------------------------
     # *****************************************************************************

     if(is.null(labnames)) {
          stop('must define labnames')
          return(NULL)
     }

     if(length(labnames) != length(labvarnames)) {
          stop('labnames and labvarnames must have the same length.')
          return(NULL)
     }

     # *****************************************************************************
     # Function --------------------------------------------------------------------
     # *****************************************************************************

     # First just get the labs we want
     df_labs <- df_labs %>% filter(common_name %in% labnames)

     # If there are two labs taken at the same time, just keep one
     df_labs <- df_labs %>%
          distinct(enc_id, common_name, specimen_taken_time, .keep_all = TRUE)

     # If more than 1 lab type is requested, make into a wide format
     df_labs <- df_labs %>%
          pivot_wider(id_cols = c('enc_id', 'specimen_taken_time'),
                      names_from = 'common_name',
                      values_from = 'result_value')

     # Rename columns if labvarnames exists
     if(!is.null(labvarnames)) {
          rename_vec <- stats::setNames(labnames, labvarnames)
          df_labs <- df_labs %>%
               rename(!!!rename_vec)
     }

}
