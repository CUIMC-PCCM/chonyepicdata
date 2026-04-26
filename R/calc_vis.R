#' calc_vis
#'
#' Compute a Gaies 2010 Vasoactive-Inotropic Score (VIS) trajectory from the
#' output of \link{classify_meds}.
#'
#' VIS is a point-in-time, multi-drug-additive score:
#' \deqn{
#'   \text{VIS}(t) = \sum_{\text{drug}} \text{coefficient}_{\text{drug}}
#'   \times \text{active dose}_{\text{drug}}(t)
#' }
#' where any drug not running at time \eqn{t} contributes 0.  The function
#' therefore returns a \emph{trajectory} (one row per evaluation time), not a
#' single per-encounter summary.  Downstream summaries — max VIS, time-weighted
#' mean VIS, VIS-at-hour-24 — are computed by the caller with ordinary
#' \code{dplyr} operations (see Examples).
#'
#' \strong{Gaies 2010 formula (default coefficients):}
#' \deqn{
#'   \text{VIS} = \text{dopa} + \text{dobut}
#'   + 100 \cdot \text{epi} + 100 \cdot \text{norepi}
#'   + 10  \cdot \text{milrinone}
#'   + 10{,}000 \cdot \text{vasopressin}
#' }
#' All non-vasopressin doses must be in \code{mcg/kg/min}; vasopressin in
#' \code{units/kg/min}.  \link{classify_meds} normalises to these units
#' automatically.
#'
#' @param classified_meds The list returned by \link{classify_meds},
#'   containing \code{$intervals} and \code{$courses}.
#' @param mode One of \code{"event"} (default) or \code{"timegrid"}.
#'   \describe{
#'     \item{\code{"event"}}{Evaluate VIS at every dose-change event across all
#'       tracked drugs for each encounter. Produces a complete piecewise-constant
#'       trajectory with no resampling.}
#'     \item{\code{"timegrid"}}{Evaluate VIS only at user-supplied timestamps.
#'       Requires \code{time_grid}.}
#'   }
#' @param time_grid Data frame with columns \code{enc_id} (character) and
#'   \code{time} (POSIXct). Required when \code{mode = "timegrid"}.
#' @param components Logical. If \code{TRUE}, add one column per drug showing
#'   its individual weighted contribution to VIS. Default \code{FALSE}.
#' @param coefficients Named list of Gaies multipliers, one entry per drug
#'   canonical name. Default is the Gaies 2010 formula. Override to use a
#'   non-standard variant.
#'
#' @return A tibble with columns \code{enc_id}, \code{time}, \code{vis}.
#'   When \code{components = TRUE}, additional columns named after each drug
#'   (e.g.\ \code{epi}, \code{norepi}) show the per-drug VIS contribution.
#'   VIS is 0 when no tracked drug is active; \code{NA} rows in
#'   \code{time_grid} that have no encounter match are dropped.
#'
#' @seealso \link{classify_meds}, \link{load_meds}
#' @export
#'
#' @examples
#' \dontrun{
#' classified <- classify_meds(load_meds("mar.txt"), patient_weights = wts)
#' traj <- calc_vis(classified)
#'
#' # Max VIS during PICU stay
#' traj |> dplyr::group_by(enc_id) |> dplyr::summarize(max_vis = max(vis))
#'
#' # Time-weighted mean VIS
#' traj |>
#'   dplyr::group_by(enc_id) |>
#'   dplyr::arrange(time) |>
#'   dplyr::mutate(dur = as.numeric(difftime(dplyr::lead(time), time,
#'                                           units = "hours"))) |>
#'   dplyr::summarize(
#'     mean_vis = sum(vis * dur, na.rm = TRUE) / sum(dur, na.rm = TRUE))
#'
#' # Max VIS within each pSOFA window
#' psofa_windows |>
#'   dplyr::left_join(traj, by = "enc_id", relationship = "one-to-many") |>
#'   dplyr::filter(time >= t_start, time <= t_end) |>
#'   dplyr::group_by(enc_id, t_start) |>
#'   dplyr::summarize(max_vis = max(vis, na.rm = TRUE))
#' }
calc_vis <- function(
     classified_meds,
     mode         = c('event', 'timegrid'),
     time_grid    = NULL,
     components   = FALSE,
     coefficients = list(dopa = 1, dobut = 1, epi = 100, norepi = 100,
                         milrinone = 10, vasopressin = 10000)
) {

     mode <- match.arg(mode)

     # Suppress R CMD check notes
     enc_id <- time <- vis <- dose <- med <- t_start <- t_end <- NULL

     # ------------------------------------------------------------------
     # Input validation
     # ------------------------------------------------------------------
     if (!is.list(classified_meds) ||
         !all(c('intervals', 'courses') %in% names(classified_meds))) {
          stop('`classified_meds` must be the list returned by classify_meds(), ',
               'containing $intervals and $courses.')
     }

     if (mode == 'timegrid') {
          if (is.null(time_grid)) {
               stop('`time_grid` must be provided when mode = "timegrid".')
          }
          if (!all(c('enc_id', 'time') %in% names(time_grid))) {
               stop('`time_grid` must have columns enc_id and time (POSIXct).')
          }
     }

     intervals <- classified_meds$intervals

     if (nrow(intervals) == 0L) {
          message('calc_vis: intervals table is empty; returning empty trajectory.')
          out <- tibble::tibble(enc_id = character(), time = lubridate::POSIXct(),
                                vis = numeric())
          return(out)
     }

     # ------------------------------------------------------------------
     # Validate that every drug in intervals has a coefficient
     # ------------------------------------------------------------------
     present_drugs <- unique(intervals$med)
     missing_coef  <- setdiff(present_drugs, names(coefficients))
     if (length(missing_coef) > 0L) {
          stop('The following drugs appear in intervals but have no entry in ',
               '`coefficients`: ', paste(missing_coef, collapse = ', '), '.\n',
               'Add them or subset your data.')
     }

     # Validate units: all non-vasopressin should be mcg/kg/min;
     # vasopressin should be units/kg/min
     bad_units <- intervals %>%
          dplyr::filter(
               (med != 'vasopressin' & dose_unit != 'mcg/kg/min') |
               (med == 'vasopressin' & dose_unit != 'units/kg/min')
          )
     if (nrow(bad_units) > 0L) {
          stop('Some intervals have unexpected dose_unit values. ',
               'Ensure classify_meds() completed unit normalisation. ',
               'Unexpected units: ',
               paste(unique(bad_units$dose_unit), collapse = ', '))
     }

     # ------------------------------------------------------------------
     # Build the evaluation time points
     # ------------------------------------------------------------------
     if (mode == 'event') {
          # Union of all interval start/end times per encounter
          eval_times <- intervals %>%
               dplyr::select(enc_id, t_start, t_end) %>%
               tidyr::pivot_longer(c(t_start, t_end),
                                   names_to  = NULL,
                                   values_to = 'time') %>%
               dplyr::distinct(enc_id, time) %>%
               dplyr::arrange(enc_id, time)
     } else {
          eval_times <- time_grid %>%
               dplyr::select(enc_id, time) %>%
               dplyr::filter(enc_id %in% unique(intervals$enc_id)) %>%
               dplyr::distinct(enc_id, time) %>%
               dplyr::arrange(enc_id, time)
     }

     # ------------------------------------------------------------------
     # For each (enc_id, time), find active dose for every drug
     # ------------------------------------------------------------------
     # Use an overlap join: an interval is "active" at time T when
     # t_start <= T <= t_end.  We use dplyr::join_by() with between().
     active_join <- dplyr::join_by(
          enc_id,
          dplyr::between(x$time, y$t_start, y$t_end)
     )

     vis_long <- dplyr::left_join(eval_times, intervals, by = active_join) %>%
          # At a dose-change boundary two intervals share the same timestamp:
          # the ending interval (t_end == boundary) and the starting interval
          # (t_start == boundary). Keep only the one that started most recently
          # so the boundary is attributed to the incoming dose, not the outgoing.
          dplyr::group_by(enc_id, time, med) %>%
          dplyr::slice_max(t_start, n = 1L, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          # When no interval is active for a drug at this time, dose is NA → 0
          dplyr::mutate(dose = tidyr::replace_na(dose, 0)) %>%
          # Apply Gaies coefficient per drug
          dplyr::mutate(
               coef          = purrr::map_dbl(med, ~ coefficients[[.x]] %||% 0),
               weighted_dose = dose * coef
          )

     # ------------------------------------------------------------------
     # Sum across drugs to get VIS at each (enc_id, time)
     # ------------------------------------------------------------------
     if (components) {
          # Pivot wider so each drug has its own column, then sum for vis
          vis_wide <- vis_long %>%
               dplyr::select(enc_id, time, med, weighted_dose) %>%
               tidyr::pivot_wider(
                    names_from  = med,
                    values_from = weighted_dose,
                    values_fill = 0,
                    values_fn   = sum
               )

          drug_cols <- intersect(present_drugs, names(vis_wide))

          vis_out <- vis_wide %>%
               dplyr::mutate(vis = rowSums(dplyr::pick(dplyr::all_of(drug_cols)),
                                           na.rm = TRUE)) %>%
               dplyr::select(enc_id, time, vis, dplyr::all_of(drug_cols)) %>%
               dplyr::arrange(enc_id, time)
     } else {
          vis_out <- vis_long %>%
               dplyr::group_by(enc_id, time) %>%
               dplyr::summarize(vis = sum(weighted_dose, na.rm = TRUE),
                                .groups = 'drop') %>%
               dplyr::arrange(enc_id, time)
     }

     vis_out
}

# Null-coalescing operator used internally (avoids rlang dependency for this)
`%||%` <- function(x, y) if (!is.null(x)) x else y
