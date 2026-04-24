library(dplyr)
library(lubridate)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

t0 <- ymd_hms("2024-08-01 10:00:00")

# Minimal new-format fixture (flowsheet_measure_id, post-7/18/2024)
df_new <- tibble::tibble(
     enc_id               = c(1L, 1L, 1L),
     resp_meas_time       = c(t0, t0, t0),
     flowsheet_measure_id = c(301550, 30421103, 10),  # fio2, peep, spo2
     measure_value        = c('0.4', '5', '98')
)

# Minimal old-format fixture (flowsheet_measure_name, pre-7/18/2024)
df_old <- tibble::tibble(
     enc_id                 = c(1L, 1L),
     resp_meas_time         = c(t0, t0),
     flowsheet_measure_name = c('r fio2', 'nyc ip rt r vent peep set'),
     measure_value          = c('0.5', '8')
)

# ---------------------------------------------------------------------------
# Test 1: Default call produces correct wide-format output
# ---------------------------------------------------------------------------

test_that("default call maps flowsheet_measure_id and returns wide numeric output", {
     result <- clean_resp_support(df_new)

     expect_equal(nrow(result), 1L)
     expect_true(all(c('enc_id', 'resp_meas_time', 'fio2', 'peep', 'spo2') %in% names(result)))
     expect_equal(result$fio2, 0.4)
     expect_equal(result$peep, 5)
     expect_equal(result$spo2, 98)
})

# ---------------------------------------------------------------------------
# Test 2: Custom var_col / var_map works (old format)
# ---------------------------------------------------------------------------

test_that("custom var_col and var_map correctly handles old flowsheet_measure_name format", {
     old_map <- c(
          'r fio2'                    = 'fio2',
          'nyc ip rt r vent peep set' = 'peep'
     )

     result <- clean_resp_support(df_old,
                                  var_col = 'flowsheet_measure_name',
                                  var_map = old_map)

     expect_equal(nrow(result), 1L)
     expect_true(all(c('fio2', 'peep') %in% names(result)))
     expect_equal(result$fio2, 0.5)
     expect_equal(result$peep, 8)
})

# ---------------------------------------------------------------------------
# Test 3: Unmapped rows are silently dropped
# ---------------------------------------------------------------------------

test_that("rows with unrecognized identifier values are dropped", {
     df_unknown <- tibble::tibble(
          enc_id               = 1L,
          resp_meas_time       = t0,
          flowsheet_measure_id = 9999999L,  # not in default map
          measure_value        = '42'
     )

     result <- clean_resp_support(df_unknown)

     expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# Test 4: clean_resp_support_old() emits deprecation warning
# ---------------------------------------------------------------------------

test_that("clean_resp_support_old() emits a deprecation warning", {
     expect_warning(clean_resp_support_old(df_new), "deprecated")
})
