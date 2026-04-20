#' load_vitals
#'
#' Load a pipe-delimited .txt file containing vitals signs data into the EHR. Perform
#' minimal cleaning.
#'
#' By default, the function expects Epic's standard column names. If your file uses
#' different column names, pass a named list to \code{col_map} where each name is the
#' desired output column and each value is the corresponding input column name (as it
#' would appear after \code{janitor::clean_names()}, i.e. lowercase snake_case).
#' Any columns not listed in \code{col_map} are kept as-is.
#'
#' @param vitals_filepath Path to the vitals data
#' @param col_map A named list mapping output column names to input column names
#'   (after \code{clean_names()} normalization). The default expects the following
#'   input columns: \code{pat_enc_csn_id}, \code{mrn}, \code{flowsheet_group},
#'   \code{common_name}, \code{flowsheet_name}, \code{cust_list_map_value},
#'   \code{meas_value}, \code{units}, \code{recorded_time}.
#'   Override individual entries to remap specific columns.
#' @param coltypes_vitals A \code{cols()} specification passed to \code{read_delim}.
#'   Defaults to \code{NULL} (auto-detect all columns). Can be used to enforce specific
#'   types when auto-detection fails.
#' @param vitals_to_load Names of vital signs to load, as a character vector. Default
#'   \code{NA} loads all rows.
#' @param filter_col The output column name to filter \code{vitals_to_load} against.
#'   Defaults to \code{"flowsheet_name"}. Change this if you have remapped
#'   \code{flowsheet_name} to a different output name in \code{col_map}.
#' @param max_load Maximum number of rows
#'
#' @return A data frame with all columns from the source file, with standard columns
#' renamed:
#' \itemize{
#'     \item \code{enc_id}: Encounter ID (character)
#'     \item \code{mrn}: Medical record number (character)
#'     \item \code{flowsheet_group}: Flowsheet group
#'     \item \code{common_name}: Common name
#'     \item \code{flowsheet_name}: Flowsheet row name
#'     \item \code{cust_list_map_value}: Custom list mapped value
#'     \item \code{meas_value}: Measured value
#'     \item \code{units}: Units
#'     \item \code{vital_time}: Datetime the vital was recorded
#' }
#' Additional columns present in the file are retained unchanged.
#' @export
load_vitals <- function(vitals_filepath,
                        col_map = list(
                             enc_id              = "pat_enc_csn_id",
                             mrn                 = "mrn",
                             flowsheet_group     = "flowsheet_group",
                             common_name         = "common_name",
                             flowsheet_name      = "flowsheet_name",
                             cust_list_map_value = "cust_list_map_value",
                             meas_value          = "meas_value",
                             units               = "units",
                             vital_time          = "recorded_time"
                        ),
                        coltypes_vitals = NULL,
                        vitals_to_load = NA,
                        filter_col = "flowsheet_name",
                        max_load = Inf)

{

     # Required to avoid warnings when building package
     enc_id <- display_name <- measure_value <- NULL

     # Load in all vitals
     suppressWarnings({
          df_vitals <- read_delim(vitals_filepath,
                                  col_types = coltypes_vitals,
                                  n_max = max_load,
                                  delim = '|') %>%
               clean_names() %>%
               mutate(across(where(is.character), str_to_lower))
     })

     # Rename columns based on col_map, skipping any that don't exist in the file
     rename_vec <- setNames(unlist(col_map), names(col_map))
     df_vitals <- df_vitals %>%
          rename(any_of(rename_vec)) %>%
          mutate(across(any_of(c("enc_id", "mrn")), ~ trimws(as.character(.x))))

     # Fallbacks for common Epic export column name variants
     if (!'flowsheet_name' %in% names(df_vitals) && 'display_name'   %in% names(df_vitals))
          df_vitals <- rename(df_vitals, flowsheet_name = display_name)
     if (!'meas_value'     %in% names(df_vitals) && 'measure_value'  %in% names(df_vitals))
          df_vitals <- rename(df_vitals, meas_value = measure_value)

     # If a particular vital sign was specified, then just filter to that one
     if(!is.na(vitals_to_load))
     {
          try({
               vitals_to_load <- str_to_lower(vitals_to_load)
               df_vitals <- df_vitals %>%
                    filter(.data[[filter_col]] %in% vitals_to_load)

          })
     }

     return(df_vitals)

}
