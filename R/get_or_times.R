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

     # Only keep transfers to or from OR locations
     df_adt <- df_adt %>% filter(department_name %in% or_locations)

     # Group patients by admission date. Determine when patients changed locations.
     # We only need to use admits, discharges, and transfer_in events (can drop all others).
     # The strategy is to check, row-by-row for each patient, if the location for the current
     # row matches the location for the prior row. If they match, the patient did not move and
     # we can ultimately drop this row. If they don't match, this involves a change of
     # location.
     # Pre-admit refers to the first row, which by definition can't match.
     adt_temp <- df_adt %>%
          filter(event_type %in% c('admit', 'discharge', 'transfer_in', 'transfer_out')) %>%
          mutate(department_name = as.character(department_name)) %>%
          group_by(mrn, enc_id) %>%
          arrange(mrn, enc_id, adt_date) %>%
          mutate(
               # Location prior to this one. Set equal to pre_admit if it wasn't defined.
               last_loc = lag(department_name, default = 'pre_admit'),

               # Level of care prior to this one (OR or non-OR).
               # Set equal to pre_admit if it wasn't defined.
               last_care_level = lag(level_of_care, default = last(level_of_care)),
               last_care_level = forcats::fct_na_value_to_level(last_care_level, level = 'pre_admit'),

               # Create a classification for the last entry for a patient (which may or may
               # not be the discharge
               last_row = if_else(row_number() == dplyr::n(), TRUE, FALSE)) %>%

          ungroup() %>%

          # Only keep rows where the last level of care is different than the current level of care, which
          # defines a "meaningful" (i.e. outside OR to OR, or vice versa).
          # Keep first and last entries (usually admit/discharge) by default.
          filter(event_type == 'admit' | event_type == 'discharge' | last_row |
                      (as.character(last_care_level) != as.character(level_of_care)))


     # Get a value for "last OR" which is TRUE if the prior row for this patient was a
     # in the OR
     adt_temp <- adt_temp %>% group_by(mrn, enc_id) %>%
          mutate(
               last_or = lag(or),
               last_or = tidyr::replace_na(last_or, FALSE)) %>%
          ungroup()

     # Find the start and stop dates for the OR. There can be multiple per patient
     # If a patient was discharged from the OR, then set this as the stop date
     #   Whenever "last_or" is false and "or" is true, this is a transition that defines a new OR course.
     #   Whenever "last_or" is true and "or" is false, this transition defines the end of a new OR course.
     #   If the last event occurred within the OR, then this is also the stop of a OR course.
     adt_or <- adt_temp %>%
          group_by(mrn, enc_id) %>%
          arrange(mrn, enc_id, adt_date) %>%
          mutate(or_start = if_else(!last_or & or, TRUE, FALSE, FALSE),
                 or_stop = if_else(!or & last_or, TRUE, FALSE, FALSE),
                 or_stop = if_else(or & row_number() == dplyr::n(), TRUE, or_stop, or_stop),
                 any_or = if_else(any(or), TRUE, FALSE)) %>%
          ungroup()


     # Create a simple dataset of these OR events
     # the "values_fn = list" just suppresses an annoying warning about duplicates.
     # The output format is that each row will have an MRN, an event ID, and an OR start and the subsequent OR stop date.
     # These windows of time can be used to filter any other results.
     # If any event from another table occurred between a given or_start_date and or_stop_date, then
     # the event happened within an OR.
     # the start and stop dates are formatted as datetimes in case we need to be very specific about timepoints, but can also be
     # use the "floor" for the start date, and the "ceiling" for the stop date, to ensure the entire duration is captured.
     # the values_fn = list just suppresses an annoying warning about duplicates
     adt_or_simple <- adt_or %>%
          select(mrn, enc_id, or_start, or_stop, adt_date) %>%
          mutate(or_start_date = if_else(or_start, adt_date, NA_POSIXct_),
                 or_stop_date = if_else(or_stop, adt_date, NA_POSIXct_)) %>%
          tidyr::pivot_longer(cols = c('or_start_date', 'or_stop_date'),
                              names_to = 'or_event', values_to = 'or_event_date') %>%
          filter(!is.na(or_event_date)) %>%
          select(-or_start, -or_stop, -adt_date) %>%
          tidyr::pivot_wider(id_cols = c('mrn', 'enc_id'),
                             names_from = 'or_event',
                             values_from = 'or_event_date',
                             values_fn = list) %>%
          tidyr::unnest(cols = c('or_start_date', 'or_stop_date'))

     return(adt_or_simple)
}
