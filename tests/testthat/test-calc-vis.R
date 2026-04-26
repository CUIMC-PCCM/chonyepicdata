library(dplyr)
library(lubridate)

# ---------------------------------------------------------------------------
# Fixture helpers
# ---------------------------------------------------------------------------

t0 <- ymd_hms('2024-01-01 08:00:00')

# Build a minimal classified_meds$intervals tibble directly
make_intervals <- function(...) {
     rows <- list(...)
     tibble(
          mrn         = vapply(rows, `[[`, '', 'mrn'),
          enc_id      = vapply(rows, `[[`, '', 'enc_id'),
          med         = vapply(rows, `[[`, '', 'med'),
          course_id   = vapply(rows, `[[`, 1L, 'course_id'),
          interval_id = vapply(rows, `[[`, 1L, 'interval_id'),
          t_start     = do.call(c, lapply(rows, `[[`, 't_start')),
          t_end       = do.call(c, lapply(rows, `[[`, 't_end')),
          duration_hr = vapply(rows, `[[`, 0, 'duration_hr'),
          dose        = vapply(rows, `[[`, 0, 'dose'),
          dose_unit   = vapply(rows, `[[`, '', 'dose_unit')
     )
}

iv <- function(enc_id = 'E1', med, dose, dose_unit = 'mcg/kg/min',
               t_start, t_end, course_id = 1L, interval_id = 1L) {
     list(
          mrn         = 'M1',
          enc_id      = enc_id,
          med         = med,
          course_id   = as.integer(course_id),
          interval_id = as.integer(interval_id),
          t_start     = t_start,
          t_end       = t_end,
          duration_hr = as.numeric(difftime(t_end, t_start, units = 'hours')),
          dose        = as.numeric(dose),
          dose_unit   = dose_unit
     )
}

# Wrap a intervals tibble into the list structure classify_meds() returns
wrap <- function(intervals) {
     list(
          intervals = intervals,
          courses   = tibble()          # calc_vis() only reads $intervals
     )
}

# ---------------------------------------------------------------------------
# 1. Single drug, single interval — flat trajectory at expected VIS
# ---------------------------------------------------------------------------

test_that('single epi interval produces constant VIS = 100 * dose', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi', dose = 0.05,
             t_start = t0, t_end = t0 + hours(6))
     ))

     traj <- calc_vis(cm)

     expect_true(all(traj$vis == 0.05 * 100))
     expect_true(all(traj$enc_id == 'E1'))
})

# ---------------------------------------------------------------------------
# 2. Two drugs fully overlapping — VIS = sum of weighted doses
# ---------------------------------------------------------------------------

test_that('two overlapping drugs sum correctly; non-running drug contributes 0', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi',   dose = 0.05, t_start = t0, t_end = t0 + hours(6)),
          iv('E1', 'norepi', dose = 0.05, t_start = t0, t_end = t0 + hours(6))
     ))

     traj <- calc_vis(cm)

     expected_vis <- 0.05 * 100 + 0.05 * 100   # = 10
     expect_true(all(abs(traj$vis - expected_vis) < 1e-9))
})

# ---------------------------------------------------------------------------
# 3. Two non-overlapping drugs — max(vis) != sum of per-drug maxes
#    (regression test for the incorrect "course-level VIS" design)
# ---------------------------------------------------------------------------

test_that('non-overlapping drugs: max(vis) equals each drugs peak, not their sum', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi',   dose = 0.1,
             t_start = t0,              t_end = t0 + hours(4)),
          iv('E1', 'norepi', dose = 0.1,
             t_start = t0 + hours(8),  t_end = t0 + hours(12))
     ))

     traj <- calc_vis(cm)

     max_vis <- max(traj$vis)
     sum_of_maxes <- 0.1 * 100 + 0.1 * 100   # 20 — this would be wrong

     expect_equal(max_vis, 0.1 * 100)          # 10 — correct: drugs never overlap
     expect_true(max_vis < sum_of_maxes)

     # VIS should be 0 in the gap between the two drugs
     gap_rows <- traj %>%
          filter(time > t0 + hours(4), time < t0 + hours(8))
     if (nrow(gap_rows) > 0) {
          expect_true(all(gap_rows$vis == 0))
     }
})

# ---------------------------------------------------------------------------
# 4. Dose change mid-course — trajectory has a step at the change time
# ---------------------------------------------------------------------------

test_that('dose change produces a step in the trajectory at the correct time', {
     t_change <- t0 + hours(3)
     t_stop   <- t0 + hours(6)

     cm <- wrap(make_intervals(
          iv('E1', 'epi', dose = 0.05,
             t_start = t0,       t_end = t_change, interval_id = 1L),
          iv('E1', 'epi', dose = 0.10,
             t_start = t_change, t_end = t_stop,   interval_id = 2L)
     ))

     traj <- calc_vis(cm)

     vis_before <- traj %>% filter(time == t0)       %>% pull(vis)
     vis_after  <- traj %>% filter(time == t_change) %>% pull(vis)

     expect_equal(vis_before, 0.05 * 100, tolerance = 1e-9)
     expect_equal(vis_after,  0.10 * 100, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 5. timegrid mode — correct values inside, 0 outside active intervals
# ---------------------------------------------------------------------------

test_that('timegrid mode returns correct VIS at supplied times', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi', dose = 0.05,
             t_start = t0 + hours(2), t_end = t0 + hours(4))
     ))

     grid <- tibble(
          enc_id = 'E1',
          time   = c(t0,              # before infusion
                     t0 + hours(3),   # during infusion
                     t0 + hours(6))   # after infusion
     )

     traj <- calc_vis(cm, mode = 'timegrid', time_grid = grid)

     expect_equal(nrow(traj), 3L)

     vis_before <- traj %>% filter(time == t0)            %>% pull(vis)
     vis_during <- traj %>% filter(time == t0 + hours(3)) %>% pull(vis)
     vis_after  <- traj %>% filter(time == t0 + hours(6)) %>% pull(vis)

     expect_equal(vis_before, 0,          tolerance = 1e-9)
     expect_equal(vis_during, 0.05 * 100, tolerance = 1e-9)
     expect_equal(vis_after,  0,          tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 6. components = TRUE — per-drug columns sum to vis
# ---------------------------------------------------------------------------

test_that('components = TRUE: per-drug columns sum to vis row-wise', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi',  dose = 0.05, t_start = t0, t_end = t0 + hours(4)),
          iv('E1', 'dopa', dose = 5,    t_start = t0, t_end = t0 + hours(4))
     ))

     traj <- calc_vis(cm, components = TRUE)

     expect_true('epi'  %in% names(traj))
     expect_true('dopa' %in% names(traj))

     # Row-wise: epi_col + dopa_col == vis
     traj <- traj %>%
          mutate(col_sum = epi + dopa)
     expect_equal(traj$col_sum, traj$vis, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 7. Custom coefficients are honoured
# ---------------------------------------------------------------------------

test_that('custom coefficients override Gaies defaults', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi', dose = 1.0, t_start = t0, t_end = t0 + hours(2))
     ))

     traj_default <- calc_vis(cm)
     traj_custom  <- calc_vis(cm, coefficients = list(epi = 50))

     expect_equal(unique(traj_default$vis), 100, tolerance = 1e-9)
     expect_equal(unique(traj_custom$vis),   50, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 8. Drug in intervals but missing from coefficients — loud error
# ---------------------------------------------------------------------------

test_that('drug in intervals not in coefficients raises an error', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi', dose = 0.05, t_start = t0, t_end = t0 + hours(2))
     ))

     expect_error(
          calc_vis(cm, coefficients = list(norepi = 100)),
          regexp = 'epi'
     )
})

# ---------------------------------------------------------------------------
# 9. Unit mismatch — non-canonical dose_unit raises an error
# ---------------------------------------------------------------------------

test_that('non-canonical dose_unit in intervals raises an informative error', {
     bad_intervals <- make_intervals(
          iv('E1', 'epi', dose = 0.05, dose_unit = 'mcg/kg/hr',   # wrong unit
             t_start = t0, t_end = t0 + hours(2))
     )

     expect_error(
          calc_vis(wrap(bad_intervals)),
          regexp = 'dose_unit'
     )
})

# ---------------------------------------------------------------------------
# 10. Vasopressin uses units/kg/min and correct coefficient
# ---------------------------------------------------------------------------

test_that('vasopressin coefficient (10000) applied correctly', {
     cm <- wrap(make_intervals(
          iv('E1', 'vasopressin', dose = 0.0004, dose_unit = 'units/kg/min',
             t_start = t0, t_end = t0 + hours(2))
     ))

     traj <- calc_vis(cm)
     expect_equal(unique(traj$vis), 0.0004 * 10000, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 11. Multiple encounters — each stays independent
# ---------------------------------------------------------------------------

test_that('two encounters produce independent trajectories', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi', dose = 0.05, t_start = t0, t_end = t0 + hours(4)),
          iv('E2', 'epi', dose = 0.10, t_start = t0, t_end = t0 + hours(4))
     ))

     traj <- calc_vis(cm)

     vis_e1 <- traj %>% filter(enc_id == 'E1') %>% pull(vis) %>% unique()
     vis_e2 <- traj %>% filter(enc_id == 'E2') %>% pull(vis) %>% unique()

     expect_equal(vis_e1, 0.05 * 100, tolerance = 1e-9)
     expect_equal(vis_e2, 0.10 * 100, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 12. timegrid mode — missing time_grid argument errors
# ---------------------------------------------------------------------------

test_that('timegrid mode without time_grid errors informatively', {
     cm <- wrap(make_intervals(
          iv('E1', 'epi', dose = 0.05, t_start = t0, t_end = t0 + hours(2))
     ))

     expect_error(calc_vis(cm, mode = 'timegrid'), regexp = 'time_grid')
})
