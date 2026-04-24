library(dplyr)
library(lubridate)
library(tidyr)

# ── Synthetic input builders ──────────────────────────────────────────────────

# Two encounters with PICU admission at hour 0
t0_e1 <- ymd_hms("2024-01-10 08:00:00")
t0_e2 <- ymd_hms("2024-02-05 14:00:00")

make_time_window_abs <- function() {
     tibble(
          enc_id  = c("E001", "E002"),
          t_start = c(t0_e1,              t0_e2),
          t_end   = c(t0_e1 + hours(24),  t0_e2 + hours(24))
     )
}

make_time_window_rel <- function() {
     tibble(
          enc_id   = c("E001", "E002"),
          ref_time = c(t0_e1, t0_e2)
     )
}

make_agem <- function() {
     tibble(enc_id = c("E001", "E002"), agem = c(72, 24))
}

make_dob <- function() {
     tibble(
          enc_id = c("E001", "E002"),
          dob    = c("2018-01-10", "2022-02-05")  # exactly 6y and 2y at admission
     )
}

# Labs in load_labs() output format
make_labs <- function() {
     bind_rows(
          # E001: platelet 80, creatinine 0.6, bilirubin 0.8, PaO2 300 — all within window
          tibble(enc_id = "E001", specimen_taken_time = t0_e1 + hours(2),
                 common_name = "platelet count, auto", result_value = "80"),
          tibble(enc_id = "E001", specimen_taken_time = t0_e1 + hours(3),
                 common_name = "creatinine",           result_value = "0.6"),
          tibble(enc_id = "E001", specimen_taken_time = t0_e1 + hours(3),
                 common_name = "bilirubin, total",     result_value = "0.8"),
          tibble(enc_id = "E001", specimen_taken_time = t0_e1 + hours(4),
                 common_name = "po2 (arterial)",       result_value = "200"),
          # E001: worse creatinine later in window
          tibble(enc_id = "E001", specimen_taken_time = t0_e1 + hours(12),
                 common_name = "creatinine",           result_value = "1.2"),
          # E001: lab outside window — should be ignored
          tibble(enc_id = "E001", specimen_taken_time = t0_e1 + hours(25),
                 common_name = "creatinine",           result_value = "5.0"),
          # E002: normal values
          tibble(enc_id = "E002", specimen_taken_time = t0_e2 + hours(2),
                 common_name = "platelet count, auto", result_value = "200"),
          tibble(enc_id = "E002", specimen_taken_time = t0_e2 + hours(2),
                 common_name = "creatinine",           result_value = "0.4"),
          tibble(enc_id = "E002", specimen_taken_time = t0_e2 + hours(2),
                 common_name = "bilirubin, total",     result_value = "0.5")
     )
}

# FiO2/SpO2 in expected pre-loaded format
make_fio2_spo2 <- function() {
     bind_rows(
          tibble(enc_id = "E001", recorded_time = t0_e1 + hours(4),
                 fio2 = 60, spo2 = NA_real_),
          tibble(enc_id = "E001", recorded_time = t0_e1 + hours(4) - minutes(30),
                 fio2 = NA_real_, spo2 = 88),
          tibble(enc_id = "E002", recorded_time = t0_e2 + hours(2),
                 fio2 = 21, spo2 = 97)
     )
}

# Vitals in clean_vitals() output format
make_vitals <- function() {
     bind_rows(
          tibble(enc_id = "E001", mrn = "M001", vital_time = t0_e1 + hours(2),
                 hr = 90, sbp_ni = 100, dbp_ni = 60, map_ni = 73,
                 sbp_art = NA, dbp_art = NA, map_art = NA, resp = 18, spo2 = 97, cvp = NA),
          tibble(enc_id = "E001", mrn = "M001", vital_time = t0_e1 + hours(6),
                 hr = 110, sbp_ni = 85, dbp_ni = 45, map_ni = 58,  # low MAP
                 sbp_art = NA, dbp_art = NA, map_art = NA, resp = 22, spo2 = 95, cvp = NA),
          tibble(enc_id = "E002", mrn = "M002", vital_time = t0_e2 + hours(2),
                 hr = 100, sbp_ni = 95, dbp_ni = 55, map_ni = 68,
                 sbp_art = NA, dbp_art = NA, map_art = NA, resp = 20, spo2 = 98, cvp = NA)
     )
}

# Meds in load_meds() output format — E001 on norepi 0.05 mcg/kg/min
make_meds <- function() {
     bind_rows(
          tibble(enc_id = "E001", mrn = "M001", med_name = "norepinephrine",
                 dose = 0.05, dose_unit = "mcg/kg/min", frequency = "continuous",
                 taken_time = t0_e1 + hours(3), result = "started",
                 order_med_id = 1, ordering_date = t0_e1),
          tibble(enc_id = "E002", mrn = "M002", med_name = "acetaminophen",
                 dose = 15, dose_unit = "mg/kg", frequency = "q6h",
                 taken_time = t0_e2 + hours(2), result = "given",
                 order_med_id = 2, ordering_date = t0_e2)
     )
}

# Resp episodes from classify_resp_support() — E001 on IMV, E002 on room air
make_resp_episodes <- function() {
     bind_rows(
          tibble(enc_id = "E001", support_episode = 1L, current_support = "imv",
                 support_time_start = t0_e1 - hours(1),
                 support_time_stop  = t0_e1 + hours(20),
                 timediff = as.duration(hours(21))),
          tibble(enc_id = "E002", support_episode = 1L, current_support = "room_air",
                 support_time_start = t0_e2 - hours(2),
                 support_time_stop  = t0_e2 + hours(48),
                 timediff = as.duration(hours(50)))
     )
}

# ── Input validation ──────────────────────────────────────────────────────────

test_that("errors when neither agem nor dob provided", {
     expect_error(
          assemble_psofa_data(
               labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
               fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
               time_window = make_time_window_abs()
          ),
          regexp = "agem or dob"
     )
})

test_that("errors when time_window has neither t_start/t_end nor ref_time", {
     bad_tw <- tibble(enc_id = "E001", some_col = 1)
     expect_error(
          assemble_psofa_data(
               labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
               fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
               time_window = bad_tw, agem = make_agem()
          ),
          regexp = "time_window"
     )
})

# ── Time window modes ─────────────────────────────────────────────────────────

test_that("absolute and relative modes return the same result", {
     args <- list(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          agem = make_agem()
     )
     result_abs <- do.call(assemble_psofa_data,
                           c(args, list(time_window = make_time_window_abs())))
     result_rel <- do.call(assemble_psofa_data,
                           c(args, list(time_window = make_time_window_rel(),
                                        t_min = 0, t_max = 24)))
     expect_equal(result_abs, result_rel)
})

# ── Age handling ──────────────────────────────────────────────────────────────

test_that("agem takes priority over dob when both provided", {
     result_agem <- assemble_psofa_data(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(),
          agem = tibble(enc_id = c("E001", "E002"), agem = c(999, 999)),
          dob  = make_dob()
     )
     expect_equal(result_agem$agem, c(999, 999))
})

test_that("dob correctly computes agem from window start", {
     result <- assemble_psofa_data(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(),
          dob = make_dob()
     )
     # E001: born 2018-01-10, admission 2024-01-10 = exactly 72 months
     # E002: born 2022-02-05, admission 2024-02-05 = exactly 24 months
     expect_equal(result$agem[result$enc_id == "E001"], 72)
     expect_equal(result$agem[result$enc_id == "E002"], 24)
})

# ── Output structure ──────────────────────────────────────────────────────────

test_that("returns expected columns including t_start", {
     result <- assemble_psofa_data(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(), agem = make_agem()
     )
     expected_cols <- c("enc_id", "t_start", "agem", "creatinine", "platelets", "tbili",
                        "pf_ratio", "sf_ratio", "map", "resp_support")
     expect_true(all(expected_cols %in% names(result)))
     expect_equal(nrow(result), 2L)
})

# ── Worst-value selection ─────────────────────────────────────────────────────

test_that("worst creatinine (max) is selected within window", {
     result <- assemble_psofa_data(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(), agem = make_agem()
     )
     # E001 has creatinine 0.6 at h2 and 1.2 at h12; both in window → worst = 1.2
     expect_equal(result$creatinine[result$enc_id == "E001"], 1.2)
})

test_that("lab after time window is excluded (post-window creatinine not used)", {
     result <- assemble_psofa_data(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(), agem = make_agem()
     )
     # E001 has creatinine = 5.0 at h25 (outside 24h window) — should not appear
     expect_true(is.na(result$creatinine[result$enc_id == "E001"]) ||
                      result$creatinine[result$enc_id == "E001"] < 5.0)
})

test_that("pre-window lab used as fallback when no in-window value exists", {
     # Remove E002's in-window creatinine; add only a pre-window measurement
     labs_no_inwindow_cr <- make_labs() %>%
          filter(!(enc_id == "E002" & common_name == "creatinine")) %>%
          bind_rows(
               tibble(enc_id = "E002", specimen_taken_time = t0_e2 - hours(6),
                      common_name = "creatinine", result_value = "2.5")
          )
     result <- assemble_psofa_data(
          labs = labs_no_inwindow_cr, vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(), agem = make_agem()
     )
     expect_equal(result$creatinine[result$enc_id == "E002"], 2.5)
})

test_that("pre-window MAP used as fallback when no in-window MAP exists", {
     vitals_prewindow_only <- make_vitals() %>%
          filter(enc_id != "E002") %>%
          bind_rows(
               tibble(enc_id = "E002", mrn = "M002", vital_time = t0_e2 - hours(2),
                      hr = 100, sbp_ni = 80, dbp_ni = 40, map_ni = 53,
                      sbp_art = NA, dbp_art = NA, map_art = NA,
                      resp = 20, spo2 = 98, cvp = NA)
          )
     result <- assemble_psofa_data(
          labs = make_labs(), vitals = vitals_prewindow_only, meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(), agem = make_agem()
     )
     expect_equal(result$map[result$enc_id == "E002"], 53)
})

test_that("worst MAP (min) is selected within window", {
     result <- assemble_psofa_data(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(), agem = make_agem()
     )
     # E001 has MAPs of 73 and 58 → worst = 58
     expect_equal(result$map[result$enc_id == "E001"], 58)
})

# ── Pressor and respiratory support ──────────────────────────────────────────

test_that("norepi detected for E001, not E002", {
     result <- assemble_psofa_data(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(), agem = make_agem()
     )
     expect_equal(result$norepi[result$enc_id == "E001"], 0.05)
     expect_true(is.na(result$norepi[result$enc_id == "E002"]))
})

test_that("resp_support TRUE for E001 (IMV), FALSE for E002 (room air)", {
     result <- assemble_psofa_data(
          labs = make_labs(), vitals = make_vitals(), meds = make_meds(),
          fio2_spo2 = make_fio2_spo2(), resp_episodes = make_resp_episodes(),
          time_window = make_time_window_abs(), agem = make_agem()
     )
     expect_true(result$resp_support[result$enc_id == "E001"])
     expect_false(result$resp_support[result$enc_id == "E002"])
})

# ── Multi-episode (same enc_id, different t_start) ────────────────────────────

test_that("two episodes for same enc_id produce two rows with per-episode values", {
     t_ep1 <- ymd_hms("2024-03-01 06:00:00")
     t_ep2 <- ymd_hms("2024-03-05 06:00:00")

     tw_multi <- tibble(
          enc_id  = c("E003", "E003"),
          t_start = c(t_ep1,              t_ep2),
          t_end   = c(t_ep1 + hours(24),  t_ep2 + hours(24))
     )

     labs_multi <- bind_rows(
          tibble(enc_id = "E003", specimen_taken_time = t_ep1 + hours(4),
                 common_name = "creatinine", result_value = "2.0"),
          tibble(enc_id = "E003", specimen_taken_time = t_ep2 + hours(4),
                 common_name = "creatinine", result_value = "0.5")
     )

     vitals_multi <- tibble(
          enc_id = "E003", mrn = "M003",
          vital_time = c(t_ep1 + hours(2), t_ep2 + hours(2)),
          hr = 90, sbp_ni = 90, dbp_ni = 55, map_ni = 67,
          sbp_art = NA, dbp_art = NA, map_art = NA, resp = 18, spo2 = 97, cvp = NA
     )

     meds_empty <- make_meds()[0, ]

     fio2_empty <- tibble(enc_id = character(), recorded_time = lubridate::POSIXct(),
                          fio2 = numeric(), spo2 = numeric())

     resp_multi <- tibble(
          enc_id = "E003", support_episode = 1L, current_support = "room_air",
          support_time_start = t_ep1 - hours(1),
          support_time_stop  = t_ep2 + hours(48),
          timediff = as.duration(hours(200))
     )

     agem_multi <- tibble(enc_id = "E003", agem = 60)

     result <- assemble_psofa_data(
          labs          = labs_multi,
          vitals        = vitals_multi,
          meds          = meds_empty,
          fio2_spo2     = fio2_empty,
          resp_episodes = resp_multi,
          time_window   = tw_multi,
          agem          = agem_multi
     )

     expect_equal(nrow(result), 2L)
     expect_true("t_start" %in% names(result))

     ep1 <- result[result$t_start == t_ep1, ]
     ep2 <- result[result$t_start == t_ep2, ]

     expect_equal(ep1$creatinine, 2.0)
     expect_equal(ep2$creatinine, 0.5)
})
