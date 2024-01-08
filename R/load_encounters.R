#' Load hospital encounters
#'
#' Load inpatient hospital encounters from a .txt file from Epic-era data.
#' The text file should be pipe-delimited (delimter is '|' without the quotes).
#' This just contains basic demographic information to be used with other relational tables.
#'
#'
#'
#' @param encounter_filepath A complete file path to the encounter data file.
#' @param coltypes_enc A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @return A data frame with reformatted and cleaned encounter-level information:
#' \itemize{
#'     \item \code{MRN}: Medical record number
#'     \item \code{enc_id}: Encounter ID
#'     \item \code{dob}: Date of birth
#'     \item \code{hospital_admission_date}: Hospital admission datetime
#'     \item \code{hospital_discharge_date}: Hospital discharge datetime
#'     }
#'
#' @export
load_encounters <- function(encounter_filepath,
                            coltypes_enc = list(
                                 col_character(),      # MRN
                                 col_character(),      # PAT_ENC_CSN_ID
                                 col_skip(),           # PAT_NAME
                                 col_datetime(),       # BIRTH_DATE
                                 col_character(),      # SEX
                                 col_skip(),           # AGE_AT_ADMIT
                                 col_skip(),           # ETHNICITY
                                 col_skip(),           # ZIP_CODE
                                 col_datetime(),       # ADMISSION_DATE
                                 col_datetime()        # DISCHARGE_DATE
                            ),
                            max_load = Inf)
{
     require(tidyverse)
     require(janitor)
     require(forcats)
     require(lubridate)
     require(stringr)
     require(readr)

     df_encounters <- read_delim(paste0(encounter_filepath),
                                 delim = '|',
                                 col_types = coltypes_enc,
                                 n_max = max_load
     )

     df_encounters <- df_encounters %>%
          clean_names() %>%
          mutate(across(where(is.character), str_to_lower)) %>%
          mutate(
               dob = as_date(birth_date),
               sex = factor(sex, levels = c('male', 'female', 'other')),
               hospital_admission_date = admission_date,
               hospital_discharge_date = discharge_date) %>%
          select(-admission_date, -discharge_date) %>%
          rename(enc_id = pat_enc_csn_id)

     return(df_encounters)

}
