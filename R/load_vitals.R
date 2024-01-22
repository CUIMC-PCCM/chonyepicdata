#' load_vitals
#'
#' @param vitals_filepath Path to the medication data
#' @param vitals_coltypes
#' @param max_load
#'
#' @return
#' @export
#'
#' @examples
load_vitals <- function(vitals_filepath,
                      vitals_coltypes = list(
                           col_character(),     # PAT_ENC_CSN_ID
                           col_character(),       # MRN
                           col_character(),  # FLOWSHEET_GROUP
                           col_character(),  # COMMON_NAME
                           col_datetime(),   # FLOWSHEET_NAME
                           col_skip(),       # CUST_LIST_MAP_VALUE
                           col_character(),  # MEAS_VALUE
                           col_double(),     # UNITS
                           col_character()  # RECORDED_TIME
                      ),
                      max_load = Inf)

{

     PAT_ENC_CSN_ID|MRN|        FLOWSHEET_GROUP|        COMMON_NAME|FLOWSHEET_NAME|CUST_LIST_MAP_VALUE|MEAS_VALUE|UNITS|  RECORDED_TIME
     127237195|     1400095534| Vital signs|           Pulse, HR|   Pulse|         NULL|               128|       NULL|   2020-10-13 11:47:00.000


     suppressWarnings({
          df_meds <- read_delim(med_filepath,
                                col_types = med_coltypes,
                                n_max = max_load,
                                delim = '|') %>%
               clean_names() %>%
               mutate(across(where(is.character), str_to_lower)) %>%
               rename(enc_id = pat_enc_csn_id) %>%
               distinct(enc_id, med_name, dose, taken_time, .keep_all = TRUE)
     })

     return(df_meds)

}
