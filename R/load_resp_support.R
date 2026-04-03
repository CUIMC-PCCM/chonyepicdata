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
#' @param col_names A named character vector mapping the standardized internal column names
#'   used by this package to the actual column names in your file (after \code{clean_names()}
#'   has been applied, i.e. lowercase snake_case). Override individual entries if your file
#'   uses different column names. The names on the left are fixed internal names; the values
#'   on the right should match your file's columns.
#' @param resp_to_load Names of vent settings/measurements to load (matched against the
#'   display_name column). Defaults to all.
#' @param max_load Maximum number of rows
#'
#' @return A data frame with vent/respiratory support flowsheet rows.
#' @export
load_resp_support <- function(resp_filepath,
                     resp_coltypes = list(
                          col_character(),   # MRN
                          col_character(),   # PAT_ENC_CSN_ID
                          col_character(),   # FSD_ID
                          col_character(),   # LINE
                          col_character(),   # DISPLAY_NAME
                          col_character(),   # FLOWSHEET_MEASURE_ID
                          col_character(),   # MEASURE_VALUE
                          col_character(),   # UNITS
                          col_datetime()     # RECORDED_TIME
                     ),
                     col_names = c(
                          enc_id               = 'pat_enc_csn_id',
                          display_name         = 'display_name',
                          flowsheet_measure_id = 'flowsheet_measure_id',
                          measure_value        = 'measure_value',
                          resp_meas_time       = 'recorded_time'
                     ),
                     resp_to_load = NA,
                     max_load = Inf)

{

     # Required to avoid warnings when building package
     pat_enc_csn_id <- recorded_time <- display_name <- NULL

     # Determine filetype and load using correct readr function
     fileext <- tools::file_ext(resp_filepath)

     # Load TXT file
     if(fileext == 'txt') {

          suppressWarnings({
               df_vent <- read_delim(resp_filepath,
                                     col_types = resp_coltypes,
                                     n_max = max_load,
                                     delim = '|') %>%
                    clean_names() %>%
                    mutate(across(where(is.character), str_to_lower))
          })
     }

     # Load CSV file
     else if(fileext == 'csv') {

          suppressWarnings({
               df_vent <- readr::read_csv(resp_filepath,
                                    col_types = resp_coltypes,
                                    n_max = max_load) %>%
                    clean_names() %>%
                    mutate(across(where(is.character), str_to_lower))
          })
     }

     # Rename columns to standardized internal names based on col_names mapping
     rename_vec <- setNames(col_names, names(col_names))
     df_vent <- dplyr::rename(df_vent, any_of(rename_vec))

     # If particular vent settings or measurements were specified, filter to those
     if(!identical(resp_to_load, NA))
     {
          resp_to_load <- str_to_lower(resp_to_load)
          df_vent <- df_vent %>%
               filter(display_name %in% resp_to_load)
     }

     return(df_vent)

}
