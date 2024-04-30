#' load_generic_flowsheet_rows
#'
#' Send in a flowsheet file of an undefined type, and pull out the relevant rows
#' in a wide, timestamped format. This is because some flowsheets may have data that
#' requires pulling into a custom data frame witih one or several variables.
#'
#' @param flowsheet_filepath Path to the flowsheet
#' @param key_name Name of column(s) containing unique linking identifiers
#' @param time_col Column with timestamps, typically in MDY_HMS format
#' @param var_col Column containing the names of variables. Should only have one.
#' @param varnames The actual names of variables contained in \code{var_col}. Can
#' have multiple.
#' @param rename_vars What variable names from \code{varnames} should be renamed to. Must
#' match 1:1 with that variable.
#' @param measure_col Column with measurements corresponding to variables.
#' @param max_load Maximum number of rows to load.
#'
#' @return A data frame with the identity key and timestamped data points from the file.
#'
#' @export
#'

load_generic_flowsheet_rows <- function(flowsheet_filepath,
                                       key_name = 'PAT_ENC_CSN_ID',
                                       time_col = NULL,
                                       var_col = NULL,
                                       measure_col = NULL,
                                       varnames = NULL,
                                       rename_vars = NULL,
                                       max_load = Inf)
{

     # *****************************************************************************
     # Variable initiation ---------------------------------------------------------
     # *****************************************************************************

     common_name <- enc_id <- specimen_taken_time <- NULL

     # *****************************************************************************
     # Error handling --------------------------------------------------------------
     # *****************************************************************************

     if(is.null(var_col)) {
          stop('must define flowsheet column name to scan')
          return(NULL)
     }

     if(is.null(measure_col)) {
          stop('must define column with measurements')
          return(NULL)
     }

     if(is.null(time_col)) {
          stop('must define column with times')
          return(NULL)
     }

     if(length(varnames) != length(rename_vars)) {
          stop('varnames and rename_vars must have the same length.')
          return(NULL)
     }


     # *****************************************************************************
     # Function --------------------------------------------------------------------
     # *****************************************************************************

     suppressWarnings({
          df_flowsheet <- read_delim(flowsheet_filepath,
                                     col_select = c(key_name, time_col, var_col, measure_col),
                                     col_types = paste(rep('c', length(c(key_name, time_col, var_col, measure_col))), collapse = ''),
                                     n_max = max_load,
                                     delim = '|',
          ) %>%
               clean_names() %>%
               select(dplyr::all_of(c(str_to_lower(key_name), str_to_lower(time_col), str_to_lower(var_col), str_to_lower(measure_col)))) %>%
               mutate(across(where(is.character), str_to_lower))
     })

     # Make sure that all names are lower case
     key_name <- str_to_lower(key_name)
     time_col <- str_to_lower(time_col)
     var_col <- str_to_lower(var_col)
     measure_col <- sapply(measure_col, str_to_lower)
     varnames <- sapply(varnames, str_to_lower)
     rename_vars <- sapply(rename_vars, str_to_lower)

     # Filter out only the rows we want
     df_flowsheet <- df_flowsheet %>% filter(!!sym(var_col) %in% varnames)

     # If there are two labs taken at the same time, just keep one
     df_flowsheet <- df_flowsheet %>%
          distinct(!!!c(sym(key_name), sym(time_col), sym(var_col)), .keep_all = TRUE)

     # If more than 1 variable is requested, make into a wide format
     df_flowsheet <- df_flowsheet %>%
          pivot_wider(id_cols = dplyr::all_of(c(key_name, time_col)),
                      names_from = !!sym(var_col),
                      values_from = !!sym(measure_col))

     # Rename columns if labvarnames exists
     if(!is.null(rename_vars)) {
          rename_vec <- stats::setNames(varnames, rename_vars)
          df_flowsheet <- df_flowsheet %>%
               rename(!!!rename_vec)
     }

     # if(!is.null(var_transform)) {
     #      df_flowsheet <- df_flowsheet %>%
     #           mutate(across(dplyr::all_of(rename_vars), !!var_transform))
     # }

     return(df_flowsheet)

}
