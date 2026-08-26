library(dplyr)
library(lubridate)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

med_row <- function(mrn, enc_id, med_name, dose, dose_unit, route, frequency,
                     result, taken_time, infusion_rate = NA_real_) {
     tibble(
          mrn            = as.character(mrn),
          enc_id         = as.character(enc_id),
          order_med_id   = 1,
          ordering_date  = taken_time,
          med_name       = med_name,
          dose           = dose,
          dose_unit      = dose_unit,
          concentration  = NA_character_,
          infusion_rate  = infusion_rate,
          frequency      = frequency,
          route          = route,
          taken_time     = taken_time,
          result         = result
     )
}

# ---------------------------------------------------------------------------
# Scenario: multiple time windows per encounter (the case that used to blow
# up memory, since the old join built the full enc_id x window cross product
# before filtering by time)
# ---------------------------------------------------------------------------

test_that("clean_meds splits doses across multiple time windows for the same encounter", {
     day1 <- ymd_hms("2024-01-01 00:00:00")
     day2 <- ymd_hms("2024-01-02 00:00:00")

     df_meds <- bind_rows(
          # Falls in window 1
          med_row("M1", "E001", "morphine injection", 2, "mg", "intravenous",
                   "once", "given", day1 + hours(2)),
          # Falls in window 2
          med_row("M1", "E001", "morphine injection", 3, "mg", "intravenous",
                   "once", "given", day2 + hours(2)),
          # Falls outside both windows -- should be dropped entirely
          med_row("M1", "E001", "morphine injection", 5, "mg", "intravenous",
                   "once", "given", day1 - hours(5))
     )

     time_limits <- tibble(
          enc_id        = c("E001", "E001"),
          stay_interval = c(
               interval(day1, day1 + hours(23) + minutes(59)),
               interval(day2, day2 + hours(23) + minutes(59))
          )
     )

     result <- clean_meds(df_meds, time_limits = time_limits)

     expect_setequal(result$enc_id, c("E001#1", "E001#2"))

     stay1 <- result %>% filter(enc_id == "E001#1")
     stay2 <- result %>% filter(enc_id == "E001#2")

     expect_equal(stay1$cumul_dose_morphine_flat_dose, 2)
     expect_equal(stay2$cumul_dose_morphine_flat_dose, 3)
})

test_that("clean_meds handles many time windows per encounter without dropping or duplicating rows", {
     n_windows <- 60
     day_starts <- ymd_hms("2024-01-01 00:00:00") + days(0:(n_windows - 1))

     time_limits <- tibble(
          enc_id        = "E002",
          stay_interval = interval(day_starts, day_starts + hours(23) + minutes(59))
     )

     # One dose per day, landing inside that day's window
     df_meds <- bind_rows(lapply(seq_along(day_starts), function(i) {
          med_row("M2", "E002", "morphine injection", 1, "mg", "intravenous",
                   "once", "given", day_starts[i] + hours(1))
     }))

     result <- clean_meds(df_meds, time_limits = time_limits)

     # Each daily dose should land in its own numbered PICU stay, none dropped
     expect_equal(nrow(result), n_windows)
     expect_true(all(result$cumul_dose_morphine_flat_dose == 1))
     expect_setequal(result$enc_id, paste0("E002#", seq_len(n_windows)))
})
