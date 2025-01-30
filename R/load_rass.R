#' load_rass
#'
#' Load Richmond Agitation Sedation Scale data from Epic flowsheets.
#' The datafile contains other types of flowsheet data, such as delirium
#' (CAPD) scores. These are just filtered out. No processing is done
#' other than cleaning the dataset.
#'
#' @md
#' @param rass_filepath Path to the RASS data
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
#'

# New format
# MRN|PAT_ENC_CSN_ID|FSD_ID|LINE|DISPLAY_NAME|FLOWSHEET_MEASURE_ID|MEASURE_VALUE|UNITS|RECORDED_TIME

# Old format
# PAT_ENC_CSN_ID|MRN|FLOWSHEET_GROUP|COMMON_NAME|FLOWSHEET_NAME|CUST_LIST_MAP_VALUE|MEAS_VALUE|UNITS|RECORDED_TIME

load_rass <- function(rass_filepath,
                      max_load = Inf)

{
     # Required to avoid warnings when building package
     common_name <- recorded_time <- pat_enc_csn_id <- cust_list_map_value <- mrn <-
          display_name <- measure_value <- enc_id <- rass_time <- rass <- NULL

     df_rass <- read_delim(rass_filepath,
                           delim = '|',
                           col_types = cols(.default = col_character()),
                           n_max = max_load) %>%
          clean_names() %>%
          dplyr::filter(str_detect(display_name, 'RASS')) %>%
          mutate(across(where(is.character), str_to_lower)) %>%
          mutate(recorded_time = lubridate::ymd_hms(recorded_time)) %>%
          rename(enc_id = pat_enc_csn_id,
                 rass_time = recorded_time,
                 rass = measure_value) %>%
          select(mrn, enc_id, rass_time, rass) %>%
          mutate(rass = as.numeric(rass))

     return(df_rass)

}
