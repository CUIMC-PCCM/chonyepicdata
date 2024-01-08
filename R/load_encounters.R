#' Load hospital encounters
#'
#' Load inpatient hospital encounters from a .txt file from Epic-era data.
#' The text file should be pipe-delimited (delimter is '|' without the quotes).
#' This just contains basic demographic information lke dates os hospitalization,
#' age, and linking information to be used with other relational tables.
#'
#'
#'
#' @param encounter_filepath A complete file path to the encounter data file.
#' @param coltypes_enc A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @return A data frame with reformatted and cleaned encounter-level information, including MRN, enc_id (encounter ID), dob (date of birth), and hospital admission and discharge dates and datetimes
load_encounters <- function(encounter_filepath,
                            coltypes_enc = list(
                              col_character(),      # MRN
                              col_character(),      # PAT_ENC_CSN_ID
                              col_skip(),           # PAT_NAME
                              col_datetime(),       # BIRTH_DATE
                              col_character(),      # SEX
                              col_double(),         # AGE_AT_ADMIT
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
      hospital_admission_datetime = admission_date,
      hospital_admission_date = as_date(admission_date),
      hospital_discharge_datetime = discharge_date,
      hospital_discharge_date = as_date(discharge_date),
      admission_interval = interval(hospital_admission_date, hospital_discharge_date),
      agemonth_admit = floor((hospital_admission_date - dob)/dmonths(1)),
      ageyear_admit = round((hospital_admission_date - dob)/dyears(1), 1)) %>%
    select(-age_at_admit, -admission_date, -discharge_date) %>%
    rename(enc_id = pat_enc_csn_id)
#
#   # Remove last line (sometimes it is a raw text printout)
#   df_encounters <- df_encounters %>%
#     filter(!str_detect(mrn, 'completion'))
#
#
#   # Create intervals to ensure all events, etc. occurred during the hospitalization
#   # Note that we need to add 23:59:59 to the discharge time to ensure it doesn't cut off things that
#   # occurred on the last day
#   hospital_intervals <- df_encounters %>% select(mrn, enc_id, hospital_admission_date, hospital_discharge_date) %>%
#     distinct() %>%
#     mutate(hosp_interval = interval(hospital_admission_date, hospital_discharge_date + dhours(24)-dseconds(1))) %>%
#     select(-hospital_admission_date, -hospital_discharge_date)
#
#   df_encounters <- left_join(df_encounters, hospital-encounters)

  return(df_encounters)

}
