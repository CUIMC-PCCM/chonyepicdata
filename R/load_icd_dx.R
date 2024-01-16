#' load_icd_dx
#'
#' Use this function to load a set of ICD10 diagnosis codes. ICD codes will be
#' loaded from many different sources, and across time. Outside of this function they can be joined to
#' particular patients or encounters.
#'
#' @param icd_dx_filepath Path to the ICD diagnosis code file.
#' @param coltypes_icd A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @return A data frame with:
#' \itemize{
#'   \item \code{mrn}: Medical record number
#'   \item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
#'   \item \code{icd10_code}: Numeric ICD10 code with the format XXXXXXX (the period character is removed)
#'   \item \code{dx_name}: Description of the ICD10 diagnosis
#'   \item \code{dx_date}: Date the diagnosis was placed
#'   \item \code{primary_dx_yn}: Is this a primary diagnosis (Y/N)? (Usually the one listed first)
#'   \item \code{dx_type}: Where did the diagnosis come from? (Billing records, problem list, etc)
#' }
#'
#' @export
load_icd_dx <- function(icd_dx_filepath,
                            coltypes_icd = list(
                                 col_character(),     # MRN
                                 col_character(),     # PAT_ENC_CSN_ID
                                 col_character(),     # ICD_10_CODE
                                 col_character(),     # DX_NAME
                                 col_datetime(),      # DX_DATE
                                 col_character(),     # PRIMARY_DX_YN (is this a primary dx Y/N)
                                 col_factor()         # DX_TYPE
                            ),
                            max_load = Inf)
{
     require(tidyverse)
     require(janitor)
     require(forcats)
     require(lubridate)
     require(stringr)
     require(readr)

     # Load the file and ensure names obey tidy conventtions
     df_icd <- read_delim(icd_dx_filepath,
                          col_types = coltypes_icd,
                          n_max = max_load,
                          delim = '|') %>%
          clean_names()

     # Update each entry so that all character-type data is in lowercase (it
     # is very rare for uppercase to encode anything useful, it just makes
     # matching more challenging.
     # The exception is ICD10 codes, which are usually reported as upper-case so
     # we will keep these as-is.
     df_icd <- df_icd %>%
          mutate(across(-icd_10_code,
                        ~ ifelse(is.character(.), str_to_lower(.), .))) %>%

          mutate(
               # Convert certain columns to correct formats, and ensure all ICD10 codes
               dx_date = as_date(dx_date)) %>%

          # Change names of relevant columns to fit the previosu schema
          rename(enc_id = pat_enc_csn_id, icd10_code = icd_10_code)

     # Some rows inexplicably have 2 or more ICD10 codes. Split into new rows
     df_icd <- df_icd %>%
          separate_rows(icd10_code, sep = ',') %>%
          mutate(icd10_code = str_trim(icd10_code))

     return(df_icd)
}
