library(dplyr)
library(lubridate)

# ---------------------------------------------------------------------------
# Fixture helpers
# ---------------------------------------------------------------------------

t0 <- ymd_hms('2024-01-01 08:00:00')

# Build a minimal load_meds()-style row
mar_row <- function(mrn = 'M1', enc_id = 'E1',
                    med_name, dose, dose_unit,
                    taken_time, result = 'started',
                    route = 'intravenous infusion',
                    frequency = 'continuous',
                    infusion_rate = dose) {
     tibble(
          mrn           = as.character(mrn),
          enc_id        = as.character(enc_id),
          order_med_id  = 1L,
          ordering_date = taken_time,
          med_name      = med_name,
          dose          = as.numeric(dose),
          dose_unit     = dose_unit,
          concentration = NA_character_,
          infusion_rate = as.numeric(infusion_rate),
          frequency     = frequency,
          route         = route,
          taken_time    = taken_time,
          result        = result
     )
}

# Convenience: build a data frame from multiple mar_row() calls
make_mar <- function(...) bind_rows(...)

# ---------------------------------------------------------------------------
# 1. Single infusion — one course, one interval
# ---------------------------------------------------------------------------

test_that('single infusion: one course, one interval, max_dose == mean_dose', {
     df <- make_mar(
          mar_row(med_name = 'epinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'epinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(4), result = 'stopped')
     )

     out <- classify_meds(df)

     expect_equal(nrow(out$intervals), 1L)
     expect_equal(nrow(out$courses),   1L)
     expect_equal(out$intervals$med,        'epi')
     expect_equal(out$intervals$course_id,  1L)
     expect_equal(out$intervals$interval_id, 1L)
     expect_equal(out$intervals$dose,       0.05)
     expect_equal(out$intervals$dose_unit,  'mcg/kg/min')
     expect_equal(out$intervals$duration_hr, 4, tolerance = 1e-6)

     expect_equal(out$courses$n_intervals, 1L)
     expect_equal(out$courses$max_dose,  out$courses$mean_dose)
})

# ---------------------------------------------------------------------------
# 2. Explicit stop splits into two courses
# ---------------------------------------------------------------------------

test_that('explicit stop followed by restart creates two courses', {
     df <- make_mar(
          mar_row(med_name = 'epinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'epinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(4), result = 'stopped'),
          mar_row(med_name = 'epinephrine', dose = 0.08, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(6), result = 'started'),
          mar_row(med_name = 'epinephrine', dose = 0.08, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(10), result = 'stopped')
     )

     out <- classify_meds(df)

     expect_equal(nrow(out$courses), 2L)
     expect_equal(out$courses$course_id, c(1L, 2L))
     expect_equal(out$courses$max_dose,  c(0.05, 0.08))
})

# ---------------------------------------------------------------------------
# 3. No stop record — course ends at last MAR record time (not indefinite)
# ---------------------------------------------------------------------------

test_that('undocumented stop: course ends at last MAR record, not indefinitely', {
     # Drug runs at constant dose; last record is a rate-verify (not a stop).
     # The course should end at the time of that last record.
     df <- make_mar(
          mar_row(med_name = 'dopamine', dose = 5, dose_unit = 'mcg/kg/min',
                  taken_time = t0,            result = 'started'),
          mar_row(med_name = 'dopamine', dose = 5, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(4), result = 'rate verify'),
          mar_row(med_name = 'dopamine', dose = 5, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(8), result = 'rate verify')  # last record, no stop
     )

     out <- classify_meds(df)

     expect_equal(nrow(out$courses), 1L)
     expect_equal(out$courses$t_end, t0 + hours(8))
})

test_that('large gap without stop stays one course (gap-based splitting removed)', {
     # Without gap detection, records at t0 and t0+15h with no stop between
     # them remain a single course — the ambiguity is acknowledged but
     # gap-based splitting is not applied.
     df <- make_mar(
          mar_row(med_name = 'dopamine', dose = 5, dose_unit = 'mcg/kg/min',
                  taken_time = t0,             result = 'started'),
          mar_row(med_name = 'dopamine', dose = 8, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(15), result = 'rate/dose change'),
          mar_row(med_name = 'dopamine', dose = 8, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(20), result = 'stopped')
     )

     out <- classify_meds(df)

     expect_equal(nrow(out$courses), 1L)
})

# ---------------------------------------------------------------------------
# 4. Dose change within one course → multiple intervals, max ≠ mean
# ---------------------------------------------------------------------------

test_that('dose titration produces multiple intervals; max_dose >= mean_dose', {
     df <- make_mar(
          mar_row(med_name = 'norepinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0,              result = 'started'),
          mar_row(med_name = 'norepinephrine', dose = 0.10, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(2),   result = 'rate/dose change'),
          mar_row(med_name = 'norepinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(4),   result = 'rate/dose change'),
          mar_row(med_name = 'norepinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(6),   result = 'stopped')
     )

     out <- classify_meds(df)

     expect_equal(nrow(out$courses), 1L)
     expect_equal(out$courses$n_intervals, 3L)   # 0.05, 0.10, 0.05
     expect_true(out$courses$max_dose  >= out$courses$mean_dose)
     expect_equal(out$courses$max_dose,  0.10)

     # Time-weighted mean: 2h@0.05 + 2h@0.10 + 2h@0.05 over 6h = 0.0667
     expect_equal(out$courses$mean_dose,
                  (0.05 * 2 + 0.10 * 2 + 0.05 * 2) / 6,
                  tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# 5. Multiple drugs — independent course_id sequences per drug
# ---------------------------------------------------------------------------

test_that('two drugs in same encounter have independent course_id sequences', {
     df <- make_mar(
          mar_row(med_name = 'epinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'epinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(6), result = 'stopped'),
          mar_row(med_name = 'norepinephrine', dose = 0.10, dose_unit = 'mcg/kg/min',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'norepinephrine', dose = 0.10, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(6), result = 'stopped')
     )

     out <- classify_meds(df)

     expect_setequal(out$courses$med, c('epi', 'norepi'))
     # Each drug has course_id starting at 1
     expect_equal(out$courses %>% filter(med == 'epi')   %>% pull(course_id), 1L)
     expect_equal(out$courses %>% filter(med == 'norepi') %>% pull(course_id), 1L)
})

# ---------------------------------------------------------------------------
# 6. Topical / inhaled epinephrine is excluded
# ---------------------------------------------------------------------------

test_that('topical and inhaled epinephrine are excluded', {
     df <- make_mar(
          mar_row(med_name = 'epinephrine topical', dose = 1, dose_unit = 'mcg/kg/min',
                  taken_time = t0, result = 'given'),
          mar_row(med_name = 'racepinephrine inhaled', dose = 1, dose_unit = 'mcg/kg/min',
                  taken_time = t0, result = 'given')
     )

     out <- classify_meds(df)
     expect_equal(nrow(out$intervals), 0L)
})

# ---------------------------------------------------------------------------
# 7. Unit handling — mcg/kg/min already canonical (no patient_weights needed)
# ---------------------------------------------------------------------------

test_that('mcg/kg/min input is unchanged; no patient_weights required', {
     df <- make_mar(
          mar_row(med_name = 'epinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'epinephrine', dose = 0.05, dose_unit = 'mcg/kg/min',
                  taken_time = t0 + hours(2), result = 'stopped')
     )

     expect_no_error(out <- classify_meds(df))
     expect_equal(out$intervals$dose,      0.05)
     expect_equal(out$intervals$dose_unit, 'mcg/kg/min')
})

# ---------------------------------------------------------------------------
# 8. Unit handling — mcg/kg/hr → divide by 60 → mcg/kg/min
# ---------------------------------------------------------------------------

test_that('mcg/kg/hr is converted to mcg/kg/min (dose / 60)', {
     df <- make_mar(
          mar_row(med_name = 'dobutamine', dose = 300, dose_unit = 'mcg/kg/hr',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'dobutamine', dose = 300, dose_unit = 'mcg/kg/hr',
                  taken_time = t0 + hours(2), result = 'stopped')
     )

     out <- classify_meds(df)
     expect_equal(out$intervals$dose,      300 / 60, tolerance = 1e-9)
     expect_equal(out$intervals$dose_unit, 'mcg/kg/min')
})

# ---------------------------------------------------------------------------
# 9. Unit handling — mcg/hr (non-weight-based) requires patient_weights
# ---------------------------------------------------------------------------

test_that('mcg/hr without patient_weights raises an informative error', {
     df <- make_mar(
          mar_row(med_name = 'epinephrine', dose = 100, dose_unit = 'mcg/hr',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'epinephrine', dose = 100, dose_unit = 'mcg/hr',
                  taken_time = t0 + hours(2), result = 'stopped')
     )

     expect_error(classify_meds(df), regexp = 'patient_weights')
})

test_that('mcg/hr with patient_weights is weight-normalised and time-normalised', {
     df <- make_mar(
          mar_row(enc_id = 'E1',
                  med_name = 'epinephrine', dose = 120, dose_unit = 'mcg/hr',
                  taken_time = t0, result = 'started'),
          mar_row(enc_id = 'E1',
                  med_name = 'epinephrine', dose = 120, dose_unit = 'mcg/hr',
                  taken_time = t0 + hours(2), result = 'stopped')
     )

     wts <- tibble(enc_id = 'E1', dosing_weight = 20)  # 20 kg patient
     out <- classify_meds(df, patient_weights = wts)

     # 120 mcg/hr / 20 kg / 60 min = 0.1 mcg/kg/min
     expect_equal(out$intervals$dose,      120 / 20 / 60, tolerance = 1e-9)
     expect_equal(out$intervals$dose_unit, 'mcg/kg/min')
})

# ---------------------------------------------------------------------------
# 10. Unit handling — mg/kg/hr → mcg/kg/min (× 1000 / 60)
# ---------------------------------------------------------------------------

test_that('mg/kg/hr is converted to mcg/kg/min (dose * 1000 / 60)', {
     df <- make_mar(
          mar_row(med_name = 'milrinone', dose = 0.3, dose_unit = 'mg/kg/hr',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'milrinone', dose = 0.3, dose_unit = 'mg/kg/hr',
                  taken_time = t0 + hours(2), result = 'stopped')
     )

     out <- classify_meds(df)
     expect_equal(out$intervals$dose,      0.3 * 1000 / 60, tolerance = 1e-9)
     expect_equal(out$intervals$dose_unit, 'mcg/kg/min')
})

# ---------------------------------------------------------------------------
# 11. Vasopressin — valid units/kg/min passes; mcg units raise error
# ---------------------------------------------------------------------------

test_that('vasopressin in units/kg/min is accepted unchanged', {
     df <- make_mar(
          mar_row(med_name = 'vasopressin', dose = 0.0004, dose_unit = 'units/kg/min',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'vasopressin', dose = 0.0004, dose_unit = 'units/kg/min',
                  taken_time = t0 + hours(4), result = 'stopped')
     )

     out <- classify_meds(df)
     expect_equal(out$intervals$dose_unit, 'units/kg/min')
     expect_equal(out$intervals$dose, 0.0004)
})

test_that('vasopressin in mcg units raises an informative error', {
     df <- make_mar(
          mar_row(med_name = 'vasopressin', dose = 0.4, dose_unit = 'mcg/kg/hr',
                  taken_time = t0, result = 'started'),
          mar_row(med_name = 'vasopressin', dose = 0.4, dose_unit = 'mcg/kg/hr',
                  taken_time = t0 + hours(2), result = 'stopped')
     )

     expect_error(classify_meds(df), regexp = 'vasopressin')
})
