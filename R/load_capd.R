#' load_capd
#'
#' Load Cornell Assessment of Pediatric Delirium (CAP-D) data from Epic flowsheets.
#' The datafile contains other types of flowsheet data, which are filtered out.
#' Columns must have the names defined in the dataset from November 2023 (for now).
#'
#' @param capd_filepath Path to the CAP-D data
#' @param col_mapping Defines mapping of columns to key variables that will be output. You can dynamically
#' specify these mappings when calling the function.
#' The default is:
#'       PAT_ENC_CSN_ID -> enc_id
#'       MRN -> mrn
#'       DISPLAY_NAME -> component_name
#'       MEASURE_VALUE -> component_value
#'       RECORDED_TIME -> capd_time
#' The names on the left hand side of the list argument must be kept consistent.
#' The names on the right-hand side are the names that are actually present in the .txt file
#' which will be mapped to these variables.
#' @param capd_coltypes A list of cols() specifications specifying how columns should be read.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   By default it is recommended to just send in 'ccccccc' where the length of the string of
#'   characters is the number of columns in the data.
#'   If this isn't working well you can send in col_guess() for each one.
#' @param max_load The maximum number of rows to load. The default is \code{Inf}
#'
#' @return #' @return A data frame with:
#' \itemize{
#' \item \code{mrn}: Medical record number
#' \item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
#' \item \code{capd_time}: Datetime when the CAP-D was recorded
#' \item \code{capd}: Integer value of the summed components of all CAP-D. A score
#'    of 9 or more means the patient screened positive for delirium.
#'    Any time where a component of the CAP-D was recorded as NA will not be returned.
#'    Scores need to be complete.
#'}
#' @export
#'
load_capd <- function(capd_filepath,
                      col_mapping = list(
                           enc_id = "pat_enc_csn_id",
                           mrn = "mrn",
                           component_name = "display_name",
                           component_value = "measure_value",
                           capd_time = "recorded_time"
                      ),
                      capd_coltypes = NULL,
                      max_load = Inf)
{

     # Required to avoid warnings when building package
     capd_question <- recorded_time <- pat_enc_csn_id <- cust_list_map_value <- mrn <-
          enc_id <- capd_time <- capd_eye_contact <- capd_purposeful <- capd_aware <-
          capd_communicate <- capd_restless <- capd_inconsolable <- capd_movement <-
          capd_response_time <- capd <- NULL

     # Read file with guessed column types unless specified
     df_capd <- read_delim(capd_filepath,
                           delim = '|',
                           col_types = if (is.null(capd_coltypes)) cols(.default = col_guess()) else capd_coltypes,
                           n_max = max_load) %>%
          clean_names()

     # Rename columns based on col_mapping
     df_capd <- df_capd %>%
          rename(
               enc_id = !!col_mapping$enc_id,
               mrn = !!col_mapping$mrn,
               component_name = !!col_mapping$component_name,
               component_value = !!col_mapping$component_value,
               capd_time = !!col_mapping$capd_time
          )

     # Check if all columns in col_mapping exist in the data
     missing_cols <- setdiff(names(col_mapping), colnames(df_capd))
     if(length(missing_cols) > 0) {
          stop(paste("The following columns were not found in the data: ", paste(missing_cols, collapse = ",")))
     }

     # Perform transformation into capd (summing across components)
     df_capd <- df_capd %>%
          mutate(capd_time = lubridate::ymd_hms(capd_time)) %>%
          mutate(component_value = as.numeric(component_value)) %>%
          mutate(component_name = stringr::str_to_lower(component_name)) %>%
          filter(!is.na(component_value)) %>%
          filter(component_name %in% c(
               "does the child make eye contact with the caregiver?",
               "are the child's actions purposeful?",
               "is the child aware of his/her surroundings?",
               "does the child communicate needs and wants?",
               "is the child restless?",
               "is the child inconsolable?",
               "is the child underactive = very little movement while awake?",
               "does it take the child a long time to respond to interactions?"
          )) %>%
          mutate(component_name = case_when(
               component_name == "does the child make eye contact with the caregiver?" ~ "capd_component_eye_contact",
               component_name == "are the child's actions purposeful?" ~ "capd_component_purposeful",
               component_name == "is the child aware of his/her surroundings?" ~ "capd_component_aware",
               component_name == "does the child communicate needs and wants?" ~ "capd_component_communicate",
               component_name == "is the child restless?" ~ "capd_component_restless",
               component_name == "is the child inconsolable?" ~ "capd_component_inconsolable",
               component_name == "is the child underactive = very little movement while awake?" ~ "capd_component_movement",
               component_name == "does it take the child a long time to respond to interactions?" ~ "capd_component_response_time"
          )) %>%
          filter(!is.na(component_name)) %>%
          tidyr::pivot_wider(id_cols = c(mrn, enc_id, capd_time),
                             names_from = "component_name",
                             values_from = "component_value",
                             values_fill = NA) %>%
          mutate(capd = rowSums(select(., starts_with("capd_component_")), na.rm = TRUE)) %>%
          select(mrn, enc_id, capd_time, capd) %>%
          filter(!is.na(capd)) %>%
          arrange(mrn, enc_id, capd_time)

     return(df_capd)

}
