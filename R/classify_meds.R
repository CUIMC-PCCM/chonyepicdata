#' classify_meds
#'
#' Classify continuous vasoactive infusions into clinical courses and
#' constant-dose intervals, normalising all doses to \code{mcg/kg/min}
#' (or \code{units/kg/min} for vasopressin) ready for Vasoactive-Inotropic
#' Score computation via \link{calc_vis}.
#'
#' A \strong{course} is a continuous run of a single drug from its first
#' non-zero dose to a stop event — either an explicit MAR stop record or a
#' gap between consecutive records that exceeds \code{gap_threshold_hours}.
#' A course may contain multiple \strong{intervals}: constant-dose stretches
#' bounded by dose changes within the course.
#'
#' Course boundaries are determined solely by explicit MAR stop records
#' (\code{mar_med_stopped}). Gap-based splitting is intentionally omitted:
#' because routine rate-verify records at the same dose are collapsed before
#' any gap could be measured, post-collapse gaps conflate "no dose change" with
#' "drug was off," making any threshold arbitrary and error-prone. When a stop
#' is not charted, the course ends at the time of the last MAR record.
#'
#' @param df_meds A data frame as returned by \link{load_meds}.
#' @param vasoactive_meds Character vector of drug-name patterns (partial
#'   matches via \code{stringr::str_detect}) to include. Defaults to the
#'   Gaies 2010 VIS drug set: epinephrine, norepinephrine, dopamine,
#'   dobutamine, milrinone, vasopressin.
#' @param mar_med_given Character vector of MAR action strings indicating
#'   active administration. Defaults to the same list used by
#'   \link{clean_meds}.
#' @param mar_med_stopped Character vector of MAR action strings indicating
#'   cessation. Defaults to the same list used by \link{clean_meds}.
#' @param patient_weights Optional data frame with columns \code{enc_id} and
#'   \code{dosing_weight} (kg). Required when any MAR record uses non-weight-
#'   based units (e.g.\ \code{mcg/hr}). If \code{NULL} and such units are
#'   found the function stops with a descriptive error.
#' @param time_limits Optional data frame with columns \code{enc_id} and a
#'   \code{\link[lubridate]{interval}} column restricting records to a
#'   specific window per encounter (e.g.\ a PICU stay). Mirrors the
#'   \code{time_limits} argument of \link{clean_meds}.
#'
#' @return A named list with two tibbles:
#'   \describe{
#'     \item{\code{$intervals}}{One row per constant-dose interval. Columns:
#'       \code{mrn}, \code{enc_id}, \code{med}, \code{course_id},
#'       \code{interval_id}, \code{t_start}, \code{t_end},
#'       \code{duration_hr}, \code{dose}, \code{dose_unit}.}
#'     \item{\code{$courses}}{One row per clinical course. Columns:
#'       \code{mrn}, \code{enc_id}, \code{med}, \code{course_id},
#'       \code{t_start}, \code{t_end}, \code{duration_hr},
#'       \code{max_dose}, \code{mean_dose}, \code{dose_unit},
#'       \code{n_intervals}.}
#'   }
#'
#' @seealso \link{load_meds}, \link{clean_meds}, \link{calc_vis}
#' @export
#'
#' @examples
#' \dontrun{
#' df_meds    <- load_meds("path/to/mar.txt")
#' classified <- classify_meds(df_meds, patient_weights = weights_df)
#' traj       <- calc_vis(classified)
#' traj |> dplyr::group_by(enc_id) |> dplyr::summarize(max_vis = max(vis))
#' }
classify_meds <- function(
     df_meds,
     vasoactive_meds     = c('epinephrine', 'norepinephrine', 'dopamine',
                              'dobutamine',  'milrinone',      'vasopressin'),
     mar_med_given       = .mar_given_codes(),
     mar_med_stopped     = .mar_stopped_codes(),
     patient_weights     = NULL,
     time_limits         = NA
) {

     # Suppress R CMD check notes for variables created inside dplyr verbs
     mrn <- enc_id <- med_name <- result <- dose_unit <- taken_time <-
          dose <- med <- mar_result <- units <- med_time <- picu_stay_num <-
          dosing_weight <- is_wt_based <- med_stop <- med_given <-
          remove_row_simple <- time_diff <- t_start <- t_end <- duration_hr <-
          course_id <- interval_id <- new_course <- max_dose <- mean_dose <-
          n_intervals <- duration_hr <- NULL

     # ------------------------------------------------------------------
     # 1. Rename columns to internal names (mirrors clean_meds convention)
     # ------------------------------------------------------------------
     df <- df_meds %>%
          dplyr::rename(
               med        = med_name,
               mar_result = result,
               units      = dose_unit,
               med_time   = taken_time
          ) %>%
          dplyr::mutate(
               dose = suppressWarnings(as.numeric(dose))
          )

     # ------------------------------------------------------------------
     # 2. Filter to given / stopped records
     #    Keep stopped records even when dose is NA (stop marker still needed)
     # ------------------------------------------------------------------
     df <- df %>%
          dplyr::filter(mar_result %in% c(mar_med_given, mar_med_stopped)) %>%
          dplyr::filter(!is.na(dose) | mar_result %in% mar_med_stopped) %>%
          dplyr::mutate(dose = tidyr::replace_na(dose, 0))

     # ------------------------------------------------------------------
     # 3. Filter to target vasoactive drugs; exclude topical / inhaled forms
     # ------------------------------------------------------------------
     vaso_pattern <- stringr::str_flatten(vasoactive_meds, '|')
     df <- df %>%
          dplyr::filter(stringr::str_detect(med, vaso_pattern)) %>%
          dplyr::filter(!stringr::str_detect(
               med,
               'topical|cream|ointment|ophthalm|nasal|inhaled|racepinephrine|racemic|lidocaine'
          ))

     if (nrow(df) == 0L) {
          message('classify_meds: no vasoactive infusion records found after filtering.')
          return(list(
               intervals = tibble::tibble(
                    mrn = character(), enc_id = character(), med = character(),
                    course_id = integer(), interval_id = integer(),
                    t_start = lubridate::POSIXct(), t_end = lubridate::POSIXct(),
                    duration_hr = numeric(), dose = numeric(), dose_unit = character()
               ),
               courses = tibble::tibble(
                    mrn = character(), enc_id = character(), med = character(),
                    course_id = integer(),
                    t_start = lubridate::POSIXct(), t_end = lubridate::POSIXct(),
                    duration_hr = numeric(), max_dose = numeric(),
                    mean_dose = numeric(), dose_unit = character(),
                    n_intervals = integer()
               )
          ))
     }

     # ------------------------------------------------------------------
     # 4. Keep continuous infusions only (drop boluses / oral doses)
     # ------------------------------------------------------------------
     df <- df %>%
          dplyr::filter(stringr::str_detect(units, 'hr|hour|min|minute'))

     # ------------------------------------------------------------------
     # 5. Optional time-limits filter (mirrors clean_meds logic)
     # ------------------------------------------------------------------
     if (!is.na(time_limits)[1]) {
          intcolname <- rlang::sym(names(time_limits)[2])
          time_limits <- time_limits %>%
               dplyr::group_by(enc_id) %>%
               dplyr::mutate(picu_stay_num = dplyr::row_number()) %>%
               dplyr::ungroup()
          df <- df %>%
               dplyr::inner_join(time_limits, multiple = 'all', by = 'enc_id',
                                 relationship = 'many-to-many') %>%
               dplyr::filter(med_time %within% !!intcolname) %>%
               dplyr::select(-!!intcolname) %>%
               tidyr::unite(col = 'enc_id', sep = '#', enc_id, picu_stay_num)
     }

     # ------------------------------------------------------------------
     # 6. Optional weight join
     # ------------------------------------------------------------------
     if (!is.null(patient_weights)) {
          df <- df %>% dplyr::left_join(patient_weights, by = 'enc_id')
     } else {
          df$dosing_weight <- NA_real_
     }

     # ------------------------------------------------------------------
     # 7. Standardise drug names to canonical short forms
     #    (norepinephrine must precede epinephrine in case_when)
     # ------------------------------------------------------------------
     df <- df %>%
          dplyr::mutate(med = dplyr::case_when(
               stringr::str_detect(med, 'norepinephrine') ~ 'norepi',
               stringr::str_detect(med, 'epinephrine')    ~ 'epi',
               stringr::str_detect(med, 'dopamine')       ~ 'dopa',
               stringr::str_detect(med, 'dobutamine')     ~ 'dobut',
               stringr::str_detect(med, 'milrinone')      ~ 'milrinone',
               stringr::str_detect(med, 'vasopressin')    ~ 'vasopressin',
               TRUE ~ med
          ))

     # ------------------------------------------------------------------
     # 8. Validate vasopressin units (must be units/..., not mcg/...)
     # ------------------------------------------------------------------
     vaso_mcg <- df %>%
          dplyr::filter(med == 'vasopressin',
                        stringr::str_detect(units, '^mcg'))
     if (nrow(vaso_mcg) > 0L) {
          bad <- paste(head(unique(vaso_mcg$enc_id), 5), collapse = ', ')
          stop('vasopressin records found with mcg-based units (expected units/kg/min or units/hr). ',
               'Check data integrity for enc_ids: ', bad)
     }

     # ------------------------------------------------------------------
     # 9. Validate weight availability for non-kg units
     # ------------------------------------------------------------------
     needs_wt <- df %>%
          dplyr::filter(!stringr::str_detect(units, 'kg'))
     if (nrow(needs_wt) > 0L && all(is.na(df$dosing_weight))) {
          bad <- paste(head(unique(needs_wt$enc_id), 10), collapse = ', ')
          n_extra <- max(0L, dplyr::n_distinct(needs_wt$enc_id) - 10L)
          stop(
               'Non-weight-based units detected but `patient_weights` is NULL.\n',
               'Affected enc_ids: ', bad,
               if (n_extra > 0) paste0(' (+ ', n_extra, ' more)') else '', '\n',
               'Provide `patient_weights` with columns enc_id and dosing_weight.'
          )
     }

     # ------------------------------------------------------------------
     # 10. Interval and course detection
     # ------------------------------------------------------------------
     df <- df %>%
          dplyr::arrange(mrn, enc_id, med, med_time,
                         dplyr::desc(mar_result %in% mar_med_stopped)) %>%
          dplyr::group_by(mrn, enc_id, med) %>%
          dplyr::mutate(
               med_given = mar_result %in% mar_med_given,
               med_stop  = mar_result %in% mar_med_stopped,

               # Force the last record in each group to be a stop so every
               # course has a defined end time
               med_stop  = dplyr::if_else(dplyr::row_number() == dplyr::n(),
                                          TRUE, med_stop),

               # Zero out dose for stop records and non-given records
               dose = dplyr::if_else(med_stop | !med_given, 0, dose),

               # Collapse consecutive records at the same dose (routine
               # rate-verify records); keep the first occurrence of each dose
               remove_row_simple = (enc_id == dplyr::lag(enc_id) &
                                         med  == dplyr::lag(med)  &
                                         dose == dplyr::lag(dose)),
               remove_row_simple = dplyr::if_else(
                    dplyr::row_number() == 1, FALSE, remove_row_simple)
          ) %>%
          dplyr::filter(!remove_row_simple) %>%
          dplyr::mutate(
               # Time to the next record — this is the duration of each dose state
               time_diff = as.numeric(
                    difftime(dplyr::lead(med_time), med_time, units = 'hours')),
               time_diff = dplyr::if_else(
                    dplyr::row_number() == dplyr::n(), 0, time_diff),

               t_start = med_time,
               t_end   = med_time + lubridate::dhours(time_diff),

               # Course detection: a new course begins only at the first record
               # or after an explicit stop (dose forced to 0). Gap-based
               # splitting is not used — see function documentation.
               new_course = (dplyr::row_number() == 1) |
                    (dplyr::lag(dose, default = 0) == 0),

               course_id = cumsum(new_course)
          ) %>%
          dplyr::ungroup()

     # Keep only dose-positive rows — these are the intervals
     intervals_raw <- df %>% dplyr::filter(dose > 0)

     if (nrow(intervals_raw) == 0L) {
          message('classify_meds: no positive-dose intervals found.')
     }

     # ------------------------------------------------------------------
     # 11. Per-row unit normalisation → canonical mcg/kg/min (units/kg/min)
     # ------------------------------------------------------------------
     intervals_raw <- intervals_raw %>%
          dplyr::mutate(
               is_wt_based = stringr::str_detect(units, 'kg'),

               # Weight axis: divide by dosing_weight when not already per-kg
               dose = dplyr::if_else(
                    !is_wt_based,
                    dose / dosing_weight,
                    dose
               ),

               # Time axis: convert per-hour to per-minute
               dose = dplyr::if_else(
                    stringr::str_detect(units, 'hr|hour'),
                    dose / 60,
                    dose
               ),

               # Mass axis: convert mg → mcg for non-vasopressin drugs
               dose = dplyr::if_else(
                    med != 'vasopressin' &
                         stringr::str_detect(units, '(?i)^mg|\\/mg'),
                    dose * 1000,
                    dose
               ),

               # Canonical unit label
               dose_unit = dplyr::if_else(
                    med == 'vasopressin',
                    'units/kg/min',
                    'mcg/kg/min'
               )
          )

     # ------------------------------------------------------------------
     # 12. Assign interval_id within each course
     # ------------------------------------------------------------------
     intervals_out <- intervals_raw %>%
          dplyr::group_by(mrn, enc_id, med, course_id) %>%
          dplyr::mutate(interval_id = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::select(mrn, enc_id, med, course_id, interval_id,
                        t_start, t_end, duration_hr = time_diff,
                        dose, dose_unit)

     # ------------------------------------------------------------------
     # 13. Roll up intervals → courses
     # ------------------------------------------------------------------
     courses_out <- intervals_out %>%
          dplyr::group_by(mrn, enc_id, med, course_id) %>%
          dplyr::summarize(
               t_start     = min(t_start),
               t_end       = max(t_end),
               # mean_dose must be computed BEFORE duration_hr is overwritten;
               # within summarize(), later expressions can reference earlier
               # computed scalars, which would corrupt the weighted average.
               max_dose    = max(dose,  na.rm = TRUE),
               mean_dose   = sum(dose * duration_hr, na.rm = TRUE) /
                    sum(duration_hr, na.rm = TRUE),
               duration_hr = as.numeric(difftime(t_end, t_start,
                                                 units = 'hours')),
               dose_unit   = dplyr::first(dose_unit),
               n_intervals = dplyr::n(),
               .groups     = 'drop'
          )

     list(intervals = intervals_out, courses = courses_out)
}
