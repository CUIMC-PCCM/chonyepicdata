#' Load hospital encounters
#'
#' Load inpatient hospital encounters from a .txt file from Epic-era data.
#' The text file should be pipe-delimited (delimiter is '|' without the quotes).
#' This just contains basic demographic information to be used with other relational tables.
#'
#' By default, the function expects Epic's standard column names. If your file uses
#' different column names, pass a named list to \code{col_map} where each name is the
#' desired output column and each value is the corresponding input column name (as it
#' would appear after \code{janitor::clean_names()}, i.e. lowercase snake_case).
#' Any columns not listed in \code{col_map} are kept as-is.
#'
#' @param encounter_filepath A complete file path to the encounter data file.
#' @param col_map A named list mapping output column names to input column names
#'   (after \code{clean_names()} normalization). The default expects the following
#'   input columns: \code{mrn}, \code{pat_enc_csn_id}, \code{birth_date}, \code{sex},
#'   \code{ethnicity}, \code{hosp_admsn_time}, \code{hosp_disch_time}.
#'   Override individual entries to remap specific columns.
#' @param coltypes_enc A \code{cols()} specification passed to \code{read_delim}.
#'   Defaults to \code{NULL} (auto-detect all columns). Can be used to enforce specific
#'   types when auto-detection fails.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @return A data frame with all columns from the source file, with standard columns
#' renamed and transformed:
#' \itemize{
#'     \item \code{mrn}: Medical record number
#'     \item \code{enc_id}: Encounter ID
#'     \item \code{dob}: Date of birth (Date)
#'     \item \code{sex}: Sex (factor: male/female/other)
#'     \item \code{hospital_admission_date}: Hospital admission datetime
#'     \item \code{hospital_discharge_date}: Hospital discharge datetime
#'     }
#' Additional columns present in the file are retained unchanged.
#'
#' @export
load_encounters <- function(encounter_filepath,
                            col_map = list(
                                mrn                     = "mrn",
                                enc_id                  = "pat_enc_csn_id",
                                dob                     = "birth_date",
                                sex                     = "sex",
                                ethnicity               = "ethnicity",
                                hospital_admission_date = "hosp_admsn_time",
                                hospital_discharge_date = "hosp_disch_time"
                            ),
                            coltypes_enc = NULL,
                            max_load = Inf)
{

     # Required to avoid warnings when building package
     sex <- dob <- hospital_admission_date <- hospital_discharge_date <- NULL

     df_encounters <- read_delim(encounter_filepath,
                                 delim = '|',
                                 col_types = coltypes_enc,
                                 n_max = max_load
     ) %>%
          clean_names() %>%
          mutate(across(where(is.character), str_to_lower))

     # Rename columns based on col_map, skipping any that don't exist in the file
     rename_vec <- setNames(unlist(col_map), names(col_map))
     df_encounters <- df_encounters %>%
          rename(any_of(rename_vec)) %>%
          mutate(across(any_of(c("mrn", "enc_id")), as.character))

     # Apply standard transformations only for columns that are present
     if ("dob" %in% names(df_encounters)) {
          df_encounters <- df_encounters %>%
               mutate(dob = as_date(dob))
     }

     if ("sex" %in% names(df_encounters)) {
          df_encounters <- df_encounters %>%
               mutate(sex = factor(sex, levels = c('male', 'female', 'other')))
     }

     if ("hospital_admission_date" %in% names(df_encounters)) {
          df_encounters <- df_encounters %>%
               mutate(hospital_admission_date = as_datetime(hospital_admission_date))
     }

     if ("hospital_discharge_date" %in% names(df_encounters)) {
          df_encounters <- df_encounters %>%
               mutate(hospital_discharge_date = as_datetime(hospital_discharge_date))
     }

     return(df_encounters)

}
