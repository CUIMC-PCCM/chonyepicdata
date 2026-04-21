#' get_dept_intervals
#'
#' Load an ADT dataset and determine discrete time intervals when a patient
#' was cared for within a specified department (or set of departments).
#'
#' OR locations (operating rooms, cath labs, etc.) are excluded from the
#' timeline before processing, so a trip to the OR and back does not split
#' an otherwise continuous stay into two intervals. Virtual locations (e.g.
#' radiology) are similarly excluded.
#'
#' @param dept_names A character vector of one or more department names to
#'   track. Matching is case-insensitive. Example: \code{"MSCH 8 CENTRAL"}.
#'   If \code{NULL} (the default), the function loads the ADT file, lists all
#'   available departments, and prompts the user to select one interactively.
#' @param adt_filepath Path to the ADT file.
#' @param adt_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and
#'   can be found within the 'readr' package documentation.
#' @param max_load A number, the maximum number of rows to load. The default
#'   is infinity.
#'
#' @return A data frame with one row per discrete stay interval:
#' \itemize{
#'   \item \code{mrn}: Medical record number
#'   \item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
#'   \item \code{dept_name}: Name of the department for this interval
#'   \item \code{dept_interval}: Sequential index of each stay within the encounter, in temporal order
#'   \item \code{dept_start_date}: Datetime when the patient entered the department
#'   \item \code{dept_stop_date}: Datetime when the patient left the department
#' }
#'
#' @export
get_dept_intervals <- function(dept_names = NULL,
                               adt_filepath,
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
     pat_enc_csn_id <- effective_time <- adt_date <- event_id <-
     department_name <- mrn <- enc_id <- event_type <-
     last_row <- current_dept <- last_current_dept <- last_current_dept_pre <-
     dept_start <- dept_stop <- dept_episode <- dept_interval <- dept_name <- NULL

     # *****************************************************************************
     # Definitions -----------------------------------------------------------------
     # *****************************************************************************

     or_locations <- c('msch operating room',
                       'msch invasive cardiology',
                       'msch endoscopy',
                       'mil operating room',
                       'mil cardiac cath',
                       'mil neuro ir imaging',
                       'msch 4 north preop pacu')

     virtual_locations <- c('msch xray imaging')

     # *****************************************************************************
     # File loading ----------------------------------------------------------------
     # *****************************************************************************

     df_adt <- read_delim(file = adt_filepath,
                          col_types = adt_coltypes,
                          delim = '|',
                          n_max = max_load) %>%
          clean_names() %>%
          mutate(across(where(is.character), stringr::str_to_lower)) %>%
          mutate(
               event_type = case_when(event_type == 'admission'    ~ 'admit',
                                      event_type == 'discharge'    ~ 'discharge',
                                      event_type == 'transfer in'  ~ 'transfer_in',
                                      event_type == 'transfer out' ~ 'transfer_out')
          ) %>%
          rename(enc_id = pat_enc_csn_id, adt_date = effective_time) %>%
          relocate(adt_date, .after = event_id)

     # Drop OR and virtual locations before processing so that a round-trip to
     # the OR does not appear as a departure from the target department.
     df_adt <- df_adt %>%
          filter(!(department_name %in% c(or_locations, virtual_locations)))

     # *****************************************************************************
     # Department selection --------------------------------------------------------
     # *****************************************************************************

     # If dept_names was not supplied, prompt the user to pick one interactively.
     if (is.null(dept_names)) {
          dept_options <- sort(unique(df_adt$department_name))
          choice <- menu(dept_options, title = "\nSelect a department (enter a number):")
          if (choice == 0) stop("No department selected.")
          dept_names_lc <- dept_options[choice]
     } else {
          dept_names_lc <- stringr::str_to_lower(dept_names)
     }

     # *****************************************************************************
     # Interval detection ----------------------------------------------------------
     # *****************************************************************************

     # Pass 1: track which listed department the patient is currently in (NA if none).
     # Each listed department is treated separately — a transfer directly between
     # two listed departments counts as leaving one and starting a new interval in
     # the other. last_current_dept_pre is computed on the pre-filter data and is
     # used only to decide which rows to keep; it is recomputed after filtering.
     adt_temp <- df_adt %>%
          filter(event_type %in% c('admit', 'discharge', 'transfer_in')) %>%
          group_by(mrn, enc_id) %>%
          arrange(adt_date, .by_group = TRUE) %>%
          mutate(
               event_type            = if_else(row_number() == 1, 'admit', event_type),
               event_type            = factor(event_type),
               current_dept          = if_else(department_name %in% dept_names_lc,
                                               department_name, NA_character_),
               last_current_dept_pre = lag(current_dept),
               last_row              = row_number() == dplyr::n()
          ) %>%

          # Keep admits, discharges, last rows, and any row where the tracked
          # department changed (entering, leaving, or switching between listed depts).
          filter(event_type == 'admit' | event_type == 'discharge' | last_row |
                      xor(is.na(current_dept), is.na(last_current_dept_pre)) |
                      (!is.na(current_dept) & !is.na(last_current_dept_pre) &
                           current_dept != last_current_dept_pre)) %>%

          # Pass 2: recompute lag on the filtered rows, then derive start/stop flags.
          mutate(
               last_current_dept = lag(current_dept),
               dept_start = !is.na(current_dept) &
                                 (is.na(last_current_dept) | current_dept != last_current_dept),
               dept_stop  = (!is.na(last_current_dept) &
                                  (is.na(current_dept) | current_dept != last_current_dept)) |
                            (!is.na(current_dept) & row_number() == dplyr::n())
          ) %>%
          ungroup()

     # *****************************************************************************
     # Pair starts and stops into intervals ----------------------------------------
     # *****************************************************************************

     # Each stay episode is numbered by cumulative starts per encounter.
     # dept_name is taken from the current_dept value at the start row.
     result <- adt_temp %>%
          filter(dept_start | dept_stop) %>%
          group_by(mrn, enc_id) %>%
          mutate(dept_episode = cumsum(dept_start)) %>%
          group_by(mrn, enc_id, dept_episode) %>%
          summarize(
               dept_name       = first(current_dept[dept_start]),
               dept_start_date = min(adt_date[dept_start]),
               dept_stop_date  = max(adt_date[dept_stop]),
               .groups = 'drop'
          ) %>%
          # When a patient transfers directly between two listed departments, the
          # transition row (dept_start=T AND dept_stop=T) is assigned to the next
          # episode by cumsum(), leaving the previous episode with no stop rows and
          # a dept_stop_date of -Inf. Fix by borrowing the next episode's start time.
          group_by(mrn, enc_id) %>%
          arrange(dept_start_date, .by_group = TRUE) %>%
          mutate(dept_stop_date = if_else(
               dept_stop_date < dept_start_date,
               lead(dept_start_date),
               dept_stop_date
          )) %>%
          ungroup() %>%
          arrange(mrn, enc_id, dept_start_date) %>%
          group_by(enc_id) %>%
          mutate(dept_interval = row_number()) %>%
          ungroup() %>%
          select(mrn, enc_id, dept_name, dept_interval, dept_start_date, dept_stop_date)

     return(result)
}
