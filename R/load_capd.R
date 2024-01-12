#' load_capd
#'
#' Load Cornell Assessment of Pediatric Delirium (CAP-D) data from Epic flowsheets.
#' The datafile contains other types of flowsheet data, which are filtered out.
#' Columns must have the names defined in the dataset from November 2023 (for now).
#'
#' @param capd_filepath Path to the CAP-D data
#' @param capd_colpaths A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   If this isn't working well you can send in col_guess().
#' @param max_load The maximum number of rows to load. The default is \code{Inf}
#'
#' @return #' @return A data frame with:
#' \itemize{
#' \item \code{mrn}: Medical record number
#' \item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
#' \item \code{capd_time}: Datetime when the CAP-D was recorded
#' \item \code{capd}: Integer value of the summed components of all CAP-D. A score
#'    of 9 or more means the patient screened positive for delirium.
#'    Any time where a component of the CAP-D was recorded as NA will not be returned.
#'    Scores need to be complete.
#'
#' @export
#'
load_capd <- function(capd_filepath,
                      capd_colpaths = list(
                           col_character(),   # PAT_ENC_CSN_ID
                           col_character(),   # MRN
                           col_skip(),        # FLOWSHEET_GROUP
                           col_character(),   # COMMON_NAME
                           col_skip(),        # FLOWSHEET_NAME
                           col_number(),      # CUST_LIST_MAP_VALUE
                           col_skip(),        # MEAS_VALUE
                           col_skip(),        # UNITS
                           col_character()    # RECORDED_TIME
                      ),
                      max_load = Inf)
{
     df_capd <- read_delim(capd_filepath,
                           delim = '|',
                           col_types = capd_colpaths,
                           n_max = max_load) %>%
          clean_names() %>%
          mutate(
               capd_question = case_when(
                    common_name == 'Does the child male eye contact with the caregiver?' ~ 'capd_eye_contact',
                    common_name == 'Are the child\'s actions purposeful?' ~ 'capd_purposeful',
                    common_name == 'Is the child aware of his/her surroundings?' ~ 'capd_aware',
                    common_name == 'Does the child communicate needs and wants?' ~ 'capd_communicate',
                    common_name == 'Is the child restless?' ~ 'capd_restless',
                    common_name == 'Is the child inconsolable?' ~ 'capd_inconsolable',
                    common_name == 'Is the child underactive = very little movement while awake?' ~ 'capd_movement',
                    common_name == 'Does it take the child a long time to respond to interactions?' ~ 'capd_response_time'
               )) %>%
          filter(!is.na(capd_question)) %>%
          mutate(across(where(is.character), str_to_lower)) %>%
          mutate(recorded_time = ymd_hms(recorded_time)) %>%
          rename(enc_id = pat_enc_csn_id,
                 capd_time = recorded_time,
                 component_name = capd_question,
                 component_value = cust_list_map_value) %>%
          pivot_wider(id_cols = c(mrn, enc_id, capd_time),
                      names_from = 'component_name',
                      values_from = 'component_value',
                      values_fill = NA) %>%
          mutate(capd = capd_eye_contact + capd_purposeful + capd_aware + capd_communicate +
                                    capd_restless + capd_inconsolable + capd_movement + capd_response_time) %>%
          select(mrn, enc_id, capd_time, capd) %>%
          filter(!is.na(capd)) %>%
          arrange(mrn, enc_id, capd_time)

     return(df_capd)

}
