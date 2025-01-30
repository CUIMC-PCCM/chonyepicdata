#' load_meds
#'
#' Load medication data from the Epic EHR, and perform some limited cleaning. File should be
#' a .txt in pipe-delimited ('|') format.
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
load_meds <- function(med_filepath,
                      med_coltypes = list(
                           MRN = col_character(),
                           PAT_ENC_CSN_ID = col_character(),
                           ORDER_ID = col_number(),
                           LINE = col_skip(),
                           ORDER_TIME = col_datetime(),
                           DEPARTMENT_NAME = col_skip(),
                           MEDICATION_NAME = col_character(),
                           DESCRIPTION = col_skip(),
                           DOSE = col_double(),
                           DOSE_UNIT = col_character(),
                           CONCENTRATION = col_character(),
                           INFUSION_RATE = col_double(),
                           FREQUENCY = col_character(),
                           ROUTE = col_character(),
                           TAKEN_TIME = col_datetime(),
                           COMMENTS = col_skip(),
                           RESULT = col_character()
                      ),
                      max_load = Inf)

{

     # Required to avoid warnings when building package
     pat_enc_csn_id <- enc_id <- med_name <- dose <- taken_time <-
          order_id <- order_time <- medication_name <- NULL

     suppressWarnings({
          df_meds <- read_delim(med_filepath,
                                col_types = med_coltypes,
                                n_max = max_load,
                                delim = '|') %>%
               clean_names() %>%
               mutate(across(where(is.character), str_to_lower)) %>%
               rename(enc_id = pat_enc_csn_id,
                      ordering_date = order_time,
                      order_med_id = order_id,
                      med_name = medication_name) %>%
               distinct(enc_id, med_name, dose, taken_time, .keep_all = TRUE)
     })

     return(df_meds)

}
