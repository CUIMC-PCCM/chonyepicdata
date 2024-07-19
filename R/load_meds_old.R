#' load_meds_old
#'
#' Load medication data from the Epic EHR, and perform some limited cleaning. File should be
#' a .txt in pipe-delimited ('|') format.
#'
#' NOTE: this is an older version. As of 7/18/2024 the format for medication data files changed.
#' Use load_meds() for that file.
#'
#' @param med_filepath Path to the medication data
#' @param med_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   If this isn't working well you can send in col_guess().
#' @param max_load The maximum number of rows to load. The default is \code{Inf}
#'
#' @return A data frame with raw medication data. Medication name is saved in character format
#' in the column 'med_name'. These should be cleaned and processed.
#'
#' @seealso [clean_meds()]
#' @export
#'
load_meds_old <- function(med_filepath,
                      med_coltypes = list(
                           col_number(),     # ORDER_MED_ID
                           col_skip(),       # LINE
                           col_character(),  # MRN
                           col_character(),  # PAT_ENC_CSN_ID
                           col_datetime(),   # ORDERING_DATE
                           col_character(),  # MED_NAME
                           col_double(),     # DOSE
                           col_character(),  # DOSE_UNIT
                           col_character(),  # CONCENTRATION
                           col_double(),     # INFUSION_RATE
                           col_character(),  # FREQUENCY
                           col_character(),  # ROUTE
                           col_datetime(),   # TAKEN_TIME
                           col_skip(),       # COMMENTS
                           col_character()   # RESULT
                      ),
                      max_load = Inf)

{

     # Required to avoid warnings when building package
     pat_enc_csn_id <- enc_id <- med_name <- dose <- taken_time <-  NULL

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
