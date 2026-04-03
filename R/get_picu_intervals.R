#' get_picu_intervals
#'
#' Load an ADT dataset and determine when a patient entered/left the PICU
#'
#' @param adt_filepath Path to the ADT file.
#' @param adt_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default is infinity.
#'
#' @return A data frame with:
#' \itemize{
#'   \item \code{mrn}: Medical record number
#'   \item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
#'   \item \code{icu_start_date}: Datetime for the start of each ICU stay
#'   \item \code{icu_stop_date}: Datetime for the stop of each ICU stay
#' }
#'
#' @export
get_picu_intervals <- function(adt_filepath,
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
     last_row <- picu <- last_picu <- icu_stop <- icu_start <- icu_event_date <- NULL

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
                       'mil neuro ir imaging',
                       'msch 4 north preop pacu')

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
                 picu = if_else(level_of_care == 'picu', TRUE, FALSE, FALSE))

     # Only keep transfers to or from the ED, floor, PICU locations
     # Drop any "virtual" locations, and also any procedural locations like the
     # operating room or endoscopy suite because patients never "stay" in those spots after
     # the procedure
     df_adt <- df_adt %>%
          filter(!(department_name %in% c(virtual_locations, or_locations)))

     # Consolidate all transition logic into a single grouped pass:
     # - Fix first event as admit
     # - Compute lag-based transition columns
     # - Filter to meaningful events (location changes, admits, discharges)
     # - Compute ICU start/stop flags
     # Pre-admit refers to the first row, which by definition can't match a prior location.
     # OR and virtual locations were already removed above, so ICU -> OR -> ICU
     # correctly appears as a continuous ICU stay.
     adt_temp <- df_adt %>%
          filter(event_type %in% c('admit', 'discharge', 'transfer_in')) %>%
          mutate(department_name = as.character(department_name)) %>%
          group_by(mrn, enc_id) %>%
          arrange(adt_date, .by_group = TRUE) %>%
          mutate(
               # Fix first event as admit
               event_type     = if_else(row_number() == 1, 'admit', event_type),
               event_type     = factor(event_type),

               # Location and care level of the prior row
               last_loc        = tidyr::replace_na(lag(department_name), 'pre_admit'),
               last_care_level = forcats::fct_na_value_to_level(lag(level_of_care), level = 'pre_admit'),
               last_row        = row_number() == dplyr::n()
          ) %>%

          # Keep only meaningful transitions: admits, discharges, last rows, or care-level changes
          filter(event_type == 'admit' | event_type == 'discharge' | last_row |
                      (as.character(last_care_level) != as.character(level_of_care))) %>%

          # After filtering, re-compute lag on picu and derive ICU start/stop flags
          mutate(
               last_picu = tidyr::replace_na(lag(picu), FALSE),
               icu_start = !last_picu & picu,
               icu_stop  = (!picu & last_picu) | (picu & row_number() == dplyr::n())
          ) %>%
          ungroup()

     # Pair ICU start and stop dates by episode number.
     # Each ICU episode is numbered by cumulative starts per patient encounter.
     # This is more robust than pivot_longer/pivot_wider, which assumes matched list lengths.
     adt_icu_simple <- adt_temp %>%
          filter(icu_start | icu_stop) %>%
          group_by(mrn, enc_id) %>%
          mutate(icu_episode = cumsum(icu_start)) %>%
          group_by(mrn, enc_id, icu_episode) %>%
          summarize(
               icu_start_date = min(adt_date[icu_start]),
               icu_stop_date  = max(adt_date[icu_stop]),
               .groups = 'drop'
          ) %>%
          select(-icu_episode)

     return(adt_icu_simple)
}
