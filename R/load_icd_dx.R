#' load_icd_dx
#'
#' After loading encounter data with function load_encounters(), use this function
#' to find all ICD diagnosis codes associated with each of these patients. ICD codes will be
#' loaded from many different sources, and across time. Because of this, also create
#' a column specifying whether the ICD diagnosis originated before, during, or after the
#' hospital encounter.
#'
#' @param df_encounters A data frame consisting of MRNs, admission dates "hospital_admission"date, discharge dates "hospital_discharge date"
#' @param icd_dx_filepath Path to the ICD diagnosis code file.
#' @param coltypes_icd A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @return A data frame with MRN, encounter ID, ICD10 code and description, dates of code entry, whether the diagnosis was primary (Y/N), and whether the diagnosis was made before, during, or the hospital encounter.
#'
#' @export
load_icd_dx <- function(df_encounters,
                        icd_dx_filepath,
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
     df_icd <- read_delim(paste0(data_path, fname_icd),
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

     # Make add columns stating whether a diagnosis was made before, during, or after the hospital interval
     df_icd <- df_icd %>% inner_join(hospital_intervals) %>%
          mutate(dx_post_hosp = if_else(dx_date > int_end(hosp_interval), TRUE, FALSE),
                 dx_in_hosp = if_else(dx_date %within% hosp_interval, TRUE, FALSE),
                 dx_pre_hosp = if_else(dx_date < int_start(hosp_interval), TRUE, FALSE))

     return(df_icd)

}
