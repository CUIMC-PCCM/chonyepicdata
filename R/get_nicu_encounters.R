#' get_nicu_encounters
#'
#' Load an ADT dataset and determine whether a patient was ever in the NICU
#'
#' @param adt_filepath Path to the ADT file.
#' @param adt_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @export
get_nicu_encounters <- function(adt_filepath,
                                adt_coltypes = list(
                                     col_character(),        # mrn
                                     col_character(),        # pat_enc_csn_id
                                     col_number(),           # event_id
                                     col_character(),        # event_type
                                     col_datetime(),         # effective_time
                                     col_skip(),             # department_id
                                     col_character(),        # department_name
                                     col_skip(),             # pat_class
                                     col_skip(),             # bed_label
                                     col_skip()              # patient_service
                                ),
                                max_load = Inf) {


     # Required to avoid warnings when building package
     pat_enc_csn_id <- effective_time <- adt_date <- event_id <- department_name <-
          mrn <- enc_id <- NULL

     # *****************************************************************************
     # Definitions -----------------------------------------------------------------
     # *****************************************************************************

     nicu_locations <- c('msch 9 north nicu',
                         'msch 7t nicu',
                         'zzmsch 7 nicu c',
                         'zzmsch 7 nicu b',
                         'msch 8 intr icu nursery')

     # *****************************************************************************
     # File loading ----------------------------------------------------------------
     # *****************************************************************************

     # Load the dataset
     # They key here is to define the department names, and the
     # event types. Department names are classified above.
     # Event types have already been filtered. I believe they have the following numeric types:
     #    1     Admission
     #    2     Discharge
     #    3     Transfer in
     #    4     Transfer out
     #    5/6   "Virtual" events that don't necessitate patient movement, such as for radiology studies

     df_adt <- read_delim(file = adt_filepath,
                          col_types = adt_coltypes,
                          delim = '|',
                          n_max = max_load) %>%
          clean_names() %>%

          # Convert all characters to lowercase
          mutate(across(where(is.character), stringr::str_to_lower)) %>%

          # Convert event types into a meaningful factor variable
          mutate(
               # Convert a few variable types...
               event_type = case_when(event_type == 'admission' ~ 'admit',
                                      event_type == 'discharge' ~ 'discharge',
                                      event_type == 'transfer in' ~ 'transfer_in',
                                      event_type == 'transfer out' ~ 'transfer_out')) %>%
          rename(enc_id = pat_enc_csn_id, adt_date = effective_time) %>%
          relocate(adt_date, .after = event_id)

     # Filter to only NICU locations
     df_adt <- df_adt %>% filter(department_name %in% nicu_locations)

     # Keep distinct encounters
     df_nicu <- df_adt %>% distinct(mrn, enc_id) %>%
          mutate(nicu = TRUE)

     return(df_nicu)


}
