#' load_rass
#'
#' Load Richmond Agitation Sedation Scale data from Epic flowsheets.
#' The datafile contains other types of flowsheet data, such as delirium
#' (CAPD) scores. These are just filtered out. No processing is done
#' other than cleaning the dataset.
#'
#' @md
#' @param rass_filepath Path to the RASS data
#' @param rass_colpaths A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   If this isn't working well you can send in col_guess().
#' @param max_load The maximum number of rows to load. The default is \code{Inf}
#'
#' @return A data frame with:
#' \itemize{
#' \item \code{mrn}: Medical record number
#' \item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
#' \item \code{rass_time}: Datetime when the RASS was recorded
#' \item \code{rass}: Integer value of RASS ranging from -5 to +4 representing:
#' * -5 Unarousable
#' * -4 Deep sedation
#' * -3 Moderate sedation
#' * -2 Light sedation
#' * -1 Drowsy
#' * 0 Alert and calm
#' * 1 Restless
#' * 2 Agitated
#' * 3 Very agitated
#' * 4 Combative
#' }
#'
#' @export
load_rass <- function(rass_filepath,
                      rass_colpaths = list(
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
     require(tidyverse)
     require(janitor)
     require(forcats)
     require(lubridate)
     require(stringr)
     require(readr)

     df_rass <- read_delim(rass_filepath,
                           delim = '|',
                           col_types = rass_colpaths,
                           n_max = max_load) %>%
          clean_names() %>%
          filter(str_detect(common_name, '^RASS')) %>%
          mutate(across(where(is.character), str_to_lower)) %>%
          mutate(recorded_time = ymd_hms(recorded_time)) %>%
          rename(enc_id = pat_enc_csn_id,
                 rass_time = recorded_time,
                 rass = cust_list_map_value) %>%
          select(-common_name) %>%
          relocate(mrn)

     return(df_rass)

}
