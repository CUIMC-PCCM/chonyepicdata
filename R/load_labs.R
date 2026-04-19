#' load_labs
#'
#' Load laboratory data from the Epic EHR, and perform some limited cleaning. File should be
#' a .txt in pipe-delimited ('|') format.
#'
#' By default, the function expects Epic's standard column names. If your file uses
#' different column names, pass a named list to \code{col_map} where each name is the
#' desired output column and each value is the corresponding input column name (as it
#' would appear after \code{janitor::clean_names()}, i.e. lowercase snake_case).
#' Any columns not listed in \code{col_map} are kept as-is.
#'
#' @param labs_filepath Path to the lab data
#' @param col_map A named list mapping output column names to input column names
#'   (after \code{clean_names()} normalization). The default expects the following
#'   input columns: \code{pat_enc_csn_id}, \code{specimen_taken_time},
#'   \code{description}, \code{common_name}, \code{result_value}, \code{reference_unit}.
#'   Override individual entries to remap specific columns.
#' @param drop_cols A character vector of column names (after \code{clean_names()}
#'   normalization) to drop from the output. Defaults to columns that are not clinically
#'   useful in downstream analysis: \code{order_proc_id}, \code{order_date}, \code{line},
#'   \code{order_time}, \code{result_time}, \code{lab_status}. Set to \code{NULL} to
#'   retain all columns.
#' @param coltypes_labs A \code{cols()} specification passed to \code{read_delim}.
#'   Defaults to \code{NULL} (auto-detect all columns). Can be used to enforce specific
#'   types when auto-detection fails.
#' @param max_load The maximum number of rows to load. The default is \code{Inf}
#'
#' @return A data frame with all columns from the source file, with standard columns
#' renamed and transformed:
#' \itemize{
#'     \item \code{enc_id}: Encounter ID (character)
#'     \item \code{specimen_taken_time}: Specimen collection datetime
#'     \item \code{description}: Lab order description
#'     \item \code{common_name}: Common lab name
#'     \item \code{result_value}: Lab result value
#'     \item \code{reference_unit}: Unit of measure
#' }
#' Additional columns present in the file are retained unchanged.
#'
#' @export
#'


load_labs <- function(labs_filepath,
                      col_map = list(
                           enc_id              = "pat_enc_csn_id",
                           specimen_taken_time = "specimen_taken_time",
                           description         = "description",
                           common_name         = "common_name",
                           result_value        = "result_value",
                           reference_unit      = "reference_unit"
                      ),
                      drop_cols = c("order_proc_id", "order_date", "line",
                                    "order_time", "result_time", "lab_status"),
                      coltypes_labs = NULL,
                      max_load = Inf)
{
     # Required to avoid warnings when building package
     enc_id <- NULL

     suppressWarnings({
          df_labs <- read_delim(labs_filepath,
                                col_types = coltypes_labs,
                                n_max = max_load,
                                delim = '|') %>%
               clean_names() %>%
               mutate(across(where(is.character), str_to_lower))
     })

     # Rename columns based on col_map, skipping any that don't exist in the file
     rename_vec <- setNames(unlist(col_map), names(col_map))
     df_labs <- df_labs %>%
          rename(any_of(rename_vec)) %>%
          mutate(across(any_of("enc_id"), ~ trimws(as.character(.x)))) %>%
          select(-any_of(drop_cols))

     return(df_labs)
}
