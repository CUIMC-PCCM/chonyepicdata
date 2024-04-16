#' load_labs
#'
#' Load laboratory data from the Epic EHR, and perform some limited cleaning. File should be
#' a .txt in pipe-delimited ('|') format.
#'
#' @param labs_filepath Path to the lab data
#' @param labs_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   If this isn't working well you can send in col_guess().
#' @param max_load The maximum number of rows to load. The default is \code{Inf}
#'
#' @return A data frame with raw laboratory data and timestamps.
#'
#' @export
#'


load_labs <- function(labs_filepath,
                      labs_coltypes = list(col_skip(),	          # ORDER_PROC_ID
                                           col_skip(),	          # ORDER_DATE
                                           col_skip(),	          # LINE
                                           col_character(),	     # PAT_ENC_CSN_ID
                                           col_datetime(),	     # ORDER_TIME
                                           col_datetime(),	     # SPECIMEN_TAKEN_TIME
                                           col_datetime(),	     # RESULT_TIME
                                           col_character(),	     # DESCRIPTION
                                           col_character(),	     # COMMON_NAME
                                           col_character(),	     # RESULT_VALUE
                                           col_character(),	     # REFERENCE_UNIT
                                           col_skip()	          # LAB_STATUS
                      ),
                      max_load = Inf)
{
     # Required to avoid warnings when building package
     pat_enc_csn_id <- enc_id <- labs_name <- dose <- taken_time <-  NULL

     suppressWarnings({
          df_labs <- read_delim(labs_filepath,
                                col_types = labs_coltypes,
                                n_max = max_load,
                                delim = '|') %>%
               clean_names() %>%
               mutate(across(where(is.character), str_to_lower)) %>%
               rename(enc_id = pat_enc_csn_id)
     })

     return(df_labs)
}
