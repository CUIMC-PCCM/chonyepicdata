#' load_rass
#'
#' Load Richmond Agitation Sedation Scale data from Epic flowsheets.
#' The datafile contains other types of flowsheet data, such as delirium
#' (CAPD) scores. These are just filtered out. No processing is done
#' other than cleaning the dataset.
#'
#' @md
#' @param rass_filepath Path to the RASS data
#' @param col_mapping List that defines mapping of columns to key variables that will be output. You can dynamically
#' specify these mappings when calling the function.
#' The default is:
#' \itemize{
#'   \item enc_id = "pat_enc_csn_id",
#'   \item mrn = "mrn",
#'   \item rass_id_col = "display_name",
#'   \item rass = "measure_value_raw",
#'   \item rass_time = "recorded_time"
#' }
#' The names on the left hand side of the list argument must be kept consistent.
#' The names on the right-hand side are the names that are actually present in the .txt file
#' which will be mapped to these variables.
#' @param rass_coltypes A list of cols() specifications specifying how columns should be read.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   By default it is recommended to just send in 'ccccccc' where the length of the string of
#'   characters is the number of columns in the data.
#'   If this isn't working well you can send in col_guess() for each one.
#' @param max_load The maximum number of rows to load. The default is \code{Inf}
#'
#' @return A data frame with:
#' \itemize{
#' \item \code{mrn}: Medical record number
#' \item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
#' \item \code{rass_time}: Datetime when the RASS was recorded
#' \item \code{rass}: Integer value of RASS ranging from -5 to +4 representing:
#' * -5 Unarousable
#' * -4 Deep sedation
#' * -3 Moderate sedation
#' * -2 Light sedation
#' * -1 Drowsy
#' * 0 Alert and calm
#' * 1 Restless
#' * 2 Agitated
#' * 3 Very agitated
#' * 4 Combative
#' }
#'
#' @export
#'

# New format
# MRN|PAT_ENC_CSN_ID|FSD_ID|LINE|DISPLAY_NAME|FLOWSHEET_MEASURE_ID|MEASURE_VALUE|UNITS|RECORDED_TIME

# Old format
# PAT_ENC_CSN_ID|MRN|FLOWSHEET_GROUP|COMMON_NAME|FLOWSHEET_NAME|CUST_LIST_MAP_VALUE|MEAS_VALUE|UNITS|RECORDED_TIME

load_rass <- function(rass_filepath,
                      col_mapping = list(
                           enc_id = "pat_enc_csn_id",
                           mrn = "mrn",
                           rass_id_col = "display_name",
                           rass = "measure_value_raw",
                           rass_time = "recorded_time"
                      ),
                      rass_coltypes = NULL,
                      max_load = Inf)

{
     # Required to avoid warnings when building package
     common_name <- recorded_time <- pat_enc_csn_id <- cust_list_map_value <- mrn <-
          display_name <- measure_value <- enc_id <- rass_time <- rass <- NULL

     df_rass <- read_delim(rass_filepath,
                           delim = '|',
                           col_types = if (is.null(rass_coltypes)) cols(.default = col_character()) else rass_coltypes,
                           n_max = max_load) %>%
          clean_names()

     # Rename columns based on col_mapping
     df_rass <- df_rass %>%
          rename(
               enc_id = !!col_mapping$enc_id,
               mrn = !!col_mapping$mrn,
               rass_id_col = !!col_mapping$rass_id_col,
               rass = !!col_mapping$rass,
               rass_time = !!col_mapping$rass_time
          )

     # Check if all columns in col_mapping exist in the data
     missing_cols <- setdiff(names(col_mapping), colnames(df_rass))
     if(length(missing_cols) > 0) {
          stop(paste("The following columns were not found in the data: ", paste(missing_cols, collapse = ",")))
     }

     df_rass <- df_rass %>%
          dplyr::filter(str_detect(rass_id_col, 'RASS')) %>%
          mutate(across(where(is.character), str_to_lower)) %>%
          mutate(rass_time = lubridate::ymd_hms(rass_time)) %>%
          select(mrn, enc_id, rass_time, rass) %>%
          mutate(rass = as.numeric(rass))

     return(df_rass)

}
