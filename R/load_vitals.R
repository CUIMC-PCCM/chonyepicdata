#' load_vitals
#'
#' Load a pipe-delimited .txt file containing vitals signs data into the EHR. Perform
#' minimal cleaning.
#'
#' @param vitals_filepath Path to the medication data
#' @param vitals_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   If this isn't working well you can send in col_guess().
#' @param vitals_to_load Names of vital signs to load, in array format
#' @param max_load Maximum number of rows
#'
#' @return A data frame with vital signs (and other related flowsheet data) loaded.
#' @export
load_vitals <- function(vitals_filepath,
                        vitals_coltypes = list(
                             col_character(),   # PAT_ENC_CSN_ID
                             col_character(),   # MRN
                             col_character(),   # FLOWSHEET_GROUP
                             col_character(),   # COMMON_NAME
                             col_character(),   # FLOWSHEET_NAME
                             col_character(),   # CUST_LIST_MAP_VALUE
                             col_character(),   # MEAS_VALUE
                             col_character(),   # UNITS
                             col_datetime()     # RECORDED_TIME
                        ),
                        vitals_to_load = NA,
                        max_load = Inf)

{

     # Required to avoid warnings when building package
     pat_enc_csn_id <- recorded_time <- flowsheet_name <- NULL

     # Load in all vitals
     suppressWarnings({
          df_vitals <- read_delim(vitals_filepath,
                                  col_types = vitals_coltypes,
                                  n_max = max_load,
                                  delim = '|') %>%
               clean_names() %>%
               mutate(across(where(is.character), str_to_lower)) %>%
               rename(enc_id = pat_enc_csn_id, vital_time = recorded_time)

     })

     # If a particular vital sign was specified, then just filter to that one
     if(!is.na(vitals_to_load))
     {
          try({
               vitals_to_load <- str_to_lower(vitals_to_load)
               df_vitals <- df_vitals %>%
                    filter(flowsheet_name %in% vitals_to_load)

          })
     }

     return(df_vitals)

}
