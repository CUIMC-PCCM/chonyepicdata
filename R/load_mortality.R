#' load_mortality
#'
#' Load inpatient dates of death from a pipe-delimited .txt file.#'
#'
#' @param mortality_filepath Path to the mortality .txt file
#' @param coltypes_mortality A list of cols() specifications. Cols specifications are things
#'  like col_integer(), col_character(), and can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @return A data frame a list of MRNs and death dates
#'
#' @export
load_mortality <- function(mortality_filepath,
                           coltypes_mortality = list(
                                col_character(),      # MRN
                                col_datetime()        # DEATH_DATE
                           ),
                           max_load = Inf)
{

     # Required to avoid warnings when building package
     mrn <- death_date <- coltypes_mortality <- NULL

     df_mortality <- read_delim(paste0(mortality_filepath),
                                 delim = '|',
                                 col_types = coltypes_mortality,
                                 n_max = max_load
     )

     df_mortality <- df_mortality %>%
          clean_names() %>%
          mutate(death_date = lubridate::ymd_hms(death_date)) %>%
          filter(!is.na(death_date))

     return(df_mortality)
}
