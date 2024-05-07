#' get_or_times
#'
#' Read OR times from the ADT data. Returns times when patients entered and left the OR, IR, or other operative locations.
#'
#' @param adt_filepath Path to the ADT file.
#' @param adt_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @return
#'
#' #' @return A data frame with:
#' \itemize{
#'   \item \code{mrn}: Medical record number
#'   \item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
#'   \item \code{or_start_date}: Datetime for the start of each OR course
#'   \item \code{or_stop_date}: Datetime for the end of each OR course
#' }
#'
#' @export

get_or_times <- function(adt_filepath,
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
                         max_load = Inf)
{
     # Required to avoid warnings when building package
     pat_enc_csn_id <- effective_time <- adt_date <- event_id <- level_of_care <-
          department_name <- mrn <- enc_id <- event_type <- last_loc <- last_care_level <-
          last_row <- or <- last_or <- or_stop <- or_start <- or_event_date <- NULL

     # *****************************************************************************
     # Definitions -----------------------------------------------------------------
     # *****************************************************************************

     # ADT location types (called "departments")
     floor_locations <- c('msch 6 tower',
                          'msch 4 tower',
                          'msch 5 tower')

     step_down_locations <- c('msch 8 central')

     nicu_locations <- c('msch 9 north nicu',
                         'msch 7t nicu',
                         'zzmsch 7 nicu c',
                         'zzmsch 7 nicu b',
                         'msch 8 intr icu nursery')

     picu_locations <- c('msch 9 central picu',
                         'msch 11 central',
                         'msch 9 tower',
                         'msch 8 intr icu surge')

     or_locations <- c('msch operating room',
                       'msch invasive cardiology',
                       'msch endoscopy',
                       'mil operating room',
                       'mil cardiac cath',
                       'mil neuro ir imaging')
     # 'msch 4 north preop pacu')

     er_locations <- c('msch emergency',
                       'aln emergency',
                       'nypw emergency')

     adult_locations <- c('mil 4 sicu',
                          'mil 5 cticu',
                          'mil 9 hudson')

     virtual_locations <- c('msch xray imaging')

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

     # Define different levels based on arrays above
     df_adt <- df_adt %>%
          mutate(level_of_care = case_when(department_name %in% floor_locations ~ 'floor',
                                           department_name %in% step_down_locations ~ 'stepdown',
                                           department_name %in% nicu_locations ~ 'nicu',
                                           department_name %in% picu_locations ~ 'picu',
                                           department_name %in% or_locations ~ 'or',
                                           department_name %in% er_locations ~ 'ed',
                                           department_name %in% adult_locations ~ 'adult',
                                           department_name %in% virtual_locations ~ 'virtual',
                                           TRUE ~ 'OTHER'),
                 level_of_care = factor(level_of_care),
                 or = if_else(level_of_care == 'or', TRUE, FALSE, FALSE))



     # Note: sometimes the start date and stop date are very far apart. This is almost always due to
     # clerical error. There is really nothing to do about it. We could arbitrarily decide to use the
     # OR stop date - 24 hours, or the OR start date + 24 hours as the cap. But it isn't consistent
     # whether one or the other is more reliable, and they can both induce error so we will just use both.
     df_adt_simple <- df_adt %>%
          filter(or & event_type %in% c('admit', 'discharge', 'transfer_in', 'transfer_out')) %>%
          arrange(mrn, enc_id, adt_date) %>%
          distinct(mrn, enc_id, adt_date, event_type, .keep_all = T) %>%
          group_by(mrn, enc_id) %>%
          mutate(or_start = if_else(event_type %in% c('transfer_in', 'admit'), TRUE, FALSE),
                 or_stop = if_else(event_type %in% c('transfer_out', 'discharge'),  TRUE, FALSE)) %>%
          ungroup() %>%
          group_by(mrn, enc_id) %>%
          arrange(adt_date, .by_group = TRUE) %>%
          mutate(or_start_date = if_else(or_start, adt_date, lag(adt_date)),
                 or_stop_date = if_else(or_start, lead(adt_date), adt_date)) %>%
          ungroup() %>%
          distinct(mrn, enc_id, or_start_date, or_stop_date)

     return(df_adt_simple)
}
