#' load_resp_support
#'
#' Load in respiratory settings, measurements, and other related respiratory data
#' from a pipe-delimited .txt file. This includes things like BiPAP, CPAP, high flow,
#' nitric oxide, as well as some related measurements like SpO2. The only cleaning
#' that is done is ensuring settings are in the correct format.
#'
#' @param resp_filepath Path to the ventilator data
#' @param resp_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   If this isn't working well you can send in col_guess().
#' @param resp_to_load Names of vent settings/measurements to load. Defaults to all.
#' @param max_load Maximum number of rows
#'
#' @return A data frame with vent/respiratory support flowsheet rows.
#' @export
load_resp_support <- function(resp_filepath,
                      resp_coltypes = list(
                           col_character(),   # PAT_ENC_CSN_ID
                           col_character(),   # MEASUREMENT_NAME
                           col_character(),   # FLOWSHEET_MEASURE_NAME
                           col_character(),   # MEASURE_VALUE
                           col_datetime()     # RECORDED_TIME
                      ),
                      resp_to_load = NA,
                      max_load = Inf)

{

     # Required to avoid warnings when building package
     pat_enc_csn_id <- recorded_time <- flowsheet_name <- NULL

     # Determine filetype and load using correct readr function
     fileext <- tools::file_ext(resp_filepath)

     # Load TXT file
     if(fileext == 'txt') {

          # Load in all vent data
          suppressWarnings({
               df_vent <- read_delim(resp_filepath,
                                     col_types = resp_coltypes,
                                     n_max = max_load,
                                     delim = '|') %>%
                    clean_names() %>%
                    mutate(across(where(is.character), str_to_lower)) %>%
                    rename(enc_id = pat_enc_csn_id, resp_meas_time = recorded_time)

          })
     }

     # Load CSV file
     else if(fileext == 'csv') {

          # Load in all vent data
          suppressWarnings({
               df_vent <- readr::read_csv(resp_filepath,
                                     col_types = resp_coltypes,
                                     n_max = max_load) %>%
                    clean_names() %>%
                    mutate(across(where(is.character), str_to_lower)) %>%
                    rename(enc_id = pat_enc_csn_id, resp_meas_time = recorded_time)

          })
     }

     # If a particular vent settings or measurements were specified, just filter to those
     if(!is.na(resp_to_load))
     {
          try({
               resp_to_load <- str_to_lower(resp_to_load)
               df_vent <- df_vent %>%
                    filter(flowsheet_name %in% resp_to_load)

          })
     }

     return(df_vent)

}
