library(dplyr)
library(lubridate)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

adt_row <- function(mrn, enc_id, event_id, event_type, time, dept) {
     tibble(
          mrn             = as.character(mrn),
          pat_enc_csn_id  = as.character(enc_id),
          event_id        = as.integer(event_id),
          event_type      = event_type,
          effective_time  = format(time, "%Y-%m-%d %H:%M:%S"),
          department_id   = 0L,
          department_name = dept,
          pat_class       = "Inpatient",
          bed_label       = "A1",
          patient_service = "PICU"
     )
}

make_adt_file <- function(...) {
     f <- tempfile(fileext = ".txt")
     readr::write_delim(bind_rows(...), f, delim = "|")
     f
}

t0 <- ymd_hms("2024-01-01 08:00:00")

# ---------------------------------------------------------------------------
# Scenario 1: Single department — simple admission / discharge
# ---------------------------------------------------------------------------

test_that("single dept: one interval row with correct dept_name and times", {
     f <- make_adt_file(
          adt_row("M001", "E001", 1, "Admission", t0,             "PICU A"),
          adt_row("M001", "E001", 2, "Discharge", t0 + hours(24), "PICU A")
     )

     result <- get_dept_intervals(dept_names = "PICU A", adt_filepath = f)

     expect_equal(nrow(result), 1L)
     expect_equal(result$dept_name,       "picu a")
     expect_equal(result$dept_interval,   1L)
     expect_equal(result$dept_start_date, t0)
     expect_equal(result$dept_stop_date,  t0 + hours(24))
})

# ---------------------------------------------------------------------------
# Scenario 2: Direct A → B transfer — two separate intervals
# ---------------------------------------------------------------------------

test_that("direct transfer between two listed depts produces two rows, one per dept", {
     t1 <- t0 + hours(12)
     t2 <- t0 + hours(24)

     f <- make_adt_file(
          adt_row("M001", "E001", 1, "Admission",   t0, "PICU A"),
          adt_row("M001", "E001", 2, "Transfer In", t1, "PICU B"),
          adt_row("M001", "E001", 3, "Discharge",   t2, "PICU B")
     )

     result <- get_dept_intervals(
          dept_names   = c("PICU A", "PICU B"),
          adt_filepath = f
     )

     expect_equal(nrow(result), 2L)
     expect_equal(result$dept_name,       c("picu a", "picu b"))
     expect_equal(result$dept_interval,   c(1L, 2L))
     expect_equal(result$dept_start_date, c(t0, t1))
     expect_equal(result$dept_stop_date,  c(t1, t2))
})

# ---------------------------------------------------------------------------
# Scenario 3: OR round-trip — should not split a continuous PICU stay
# ---------------------------------------------------------------------------

test_that("OR round-trip does not split a continuous stay into two intervals", {
     t1 <- t0 + hours(6)   # transfer to OR
     t2 <- t0 + hours(10)  # return from OR
     t3 <- t0 + hours(24)  # discharge

     f <- make_adt_file(
          adt_row("M001", "E001", 1, "Admission",   t0, "PICU A"),
          adt_row("M001", "E001", 2, "Transfer In", t1, "MSCH OPERATING ROOM"),
          adt_row("M001", "E001", 3, "Transfer In", t2, "PICU A"),
          adt_row("M001", "E001", 4, "Discharge",   t3, "PICU A")
     )

     result <- get_dept_intervals(dept_names = "PICU A", adt_filepath = f)

     expect_equal(nrow(result), 1L)
     expect_equal(result$dept_start_date, t0)
     expect_equal(result$dept_stop_date,  t3)
})

# ---------------------------------------------------------------------------
# Scenario 4: Multiple encounters — dept_interval resets per encounter
# ---------------------------------------------------------------------------

test_that("dept_interval resets to 1 for each new encounter", {
     t_e2 <- t0 + days(10)

     f <- make_adt_file(
          adt_row("M001", "E001", 1, "Admission", t0,              "PICU A"),
          adt_row("M001", "E001", 2, "Discharge", t0 + hours(24),  "PICU A"),
          adt_row("M001", "E002", 3, "Admission", t_e2,            "PICU A"),
          adt_row("M001", "E002", 4, "Discharge", t_e2 + hours(24), "PICU A")
     )

     result <- get_dept_intervals(dept_names = "PICU A", adt_filepath = f)

     expect_equal(nrow(result), 2L)
     expect_equal(result$dept_interval, c(1L, 1L))
})
