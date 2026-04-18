library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(janitor)

# ── Set these paths before running ────────────────────────────────────────────
repo <- "C:/Github/chonyepicdata"   # path to package root

# Optional: set real data file paths to run checks against actual data.
# Leave as "" to skip the real-data section entirely.
data_path <- paste0(Sys.getenv('onedrive'), '/Research/data/early_mobilization/')
real_enc_file    <- paste0(data_path, "Report 1A - Hospital Encounters.txt")   # pipe-delimited encounter file
real_labs_file   <- paste0(data_path, "Report 12 - Labs.txt")   # pipe-delimited labs file
real_vitals_file <- paste0(data_path, "Report 8A - Vitals.txt")   # pipe-delimited vitals file
# ─────────────────────────────────────────────────────────────────────────────

source(file.path(repo, "R/load_encounters.R"))
source(file.path(repo, "R/load_labs.R"))
source(file.path(repo, "R/load_vitals.R"))
source(file.path(repo, "R/clean_vitals.R"))

# ── Helpers ───────────────────────────────────────────────────────────────────

pass_count <- 0L
fail_count <- 0L

check <- function(label, expr) {
     result <- tryCatch(isTRUE(expr), error = function(e) FALSE)
     if (result) {
          cat(sprintf("  PASS  %s\n", label))
          pass_count <<- pass_count + 1L
     } else {
          cat(sprintf("  FAIL  %s\n", label))
          fail_count <<- fail_count + 1L
     }
}

write_pipe <- function(df, path) {
     write_delim(df, path, delim = "|")
}

# ── Synthetic data ─────────────────────────────────────────────────────────────

enc_standard <- tibble(
     MRN              = c("1001", "1002"),
     PAT_ENC_CSN_ID   = c("E001", "E002"),
     BIRTH_DATE       = c("2000-03-15", "1995-07-22"),
     SEX              = c("Male", "Female"),
     ETHNICITY        = c("Hispanic", "Non-Hispanic"),
     HOSP_ADMSN_TIME  = c("2024-01-10 08:00:00", "2024-02-05 14:30:00"),
     HOSP_DISCH_TIME  = c("2024-01-15 10:00:00", "2024-02-08 09:00:00")
)

enc_custom <- enc_standard %>%
     rename(patient_id = MRN, visit_id = PAT_ENC_CSN_ID,
            date_of_birth = BIRTH_DATE, gender = SEX,
            admit_time = HOSP_ADMSN_TIME, discharge_time = HOSP_DISCH_TIME)

labs_standard <- tibble(
     PAT_ENC_CSN_ID    = c("E001", "E001", "E002"),
     SPECIMEN_TAKEN_TIME = c("2024-01-11 06:00:00", "2024-01-12 06:00:00", "2024-02-06 07:00:00"),
     DESCRIPTION       = c("Complete Blood Count", "BMP", "LFT"),
     COMMON_NAME       = c("WBC", "Sodium", "ALT"),
     RESULT_VALUE      = c("8.5", "138", "32"),
     REFERENCE_UNIT    = c("K/uL", "mEq/L", "U/L"),
     ORDER_PROC_ID     = c("OP1", "OP2", "OP3"),
     ORDER_DATE        = c("2024-01-10", "2024-01-11", "2024-02-05"),
     LINE              = c(1, 1, 1),
     ORDER_TIME        = c("08:00", "08:00", "07:00"),
     RESULT_TIME       = c("2024-01-11 08:00:00", "2024-01-12 08:00:00", "2024-02-06 09:00:00"),
     LAB_STATUS        = c("Final", "Final", "Final")
)

labs_custom <- labs_standard %>%
     rename(encounter_id = PAT_ENC_CSN_ID, collection_time = SPECIMEN_TAKEN_TIME)

vitals_standard <- tibble(
     PAT_ENC_CSN_ID    = c("E001", "E001", "E001", "E001", "E002"),
     MRN               = c("1001", "1001", "1001", "1001", "1002"),
     FLOWSHEET_GROUP   = "Vitals",
     COMMON_NAME       = c("HR", "BP NI", "BP Art", "SpO2", "HR"),
     FLOWSHEET_NAME    = c("Pulse", "Blood Pressure", "R FS Arterial Line Blood Pressure",
                           "Pulse Oximetry", "Pulse"),
     CUST_LIST_MAP_VALUE = NA_character_,
     MEAS_VALUE        = c("88", "118/76", "110/70", "98", "72"),
     UNITS             = c("bpm", "mmHg", "mmHg", "%", "bpm"),
     RECORDED_TIME     = c("2024-01-10 09:00:00", "2024-01-10 09:00:00",
                           "2024-01-10 09:00:00", "2024-01-10 09:00:00",
                           "2024-02-05 15:00:00")
)

vitals_custom <- vitals_standard %>%
     rename(encounter_id = PAT_ENC_CSN_ID, measure_time = RECORDED_TIME)

# ── Write temp files ───────────────────────────────────────────────────────────

f_enc_std  <- tempfile(fileext = ".txt")
f_enc_cust <- tempfile(fileext = ".txt")
f_lab_std  <- tempfile(fileext = ".txt")
f_lab_cust <- tempfile(fileext = ".txt")
f_vit_std  <- tempfile(fileext = ".txt")
f_vit_cust <- tempfile(fileext = ".txt")

write_pipe(enc_standard,  f_enc_std)
write_pipe(enc_custom,    f_enc_cust)
write_pipe(labs_standard, f_lab_std)
write_pipe(labs_custom,   f_lab_cust)
write_pipe(vitals_standard, f_vit_std)
write_pipe(vitals_custom,   f_vit_cust)

# =============================================================================
cat("\n=== load_encounters ===\n")
# =============================================================================

cat("\n-- Default col_map (Epic standard columns) --\n")
enc <- load_encounters(f_enc_std)
check("has expected output columns",
      all(c("mrn", "enc_id", "dob", "sex", "ethnicity",
            "hospital_admission_date", "hospital_discharge_date") %in% names(enc)))
check("mrn is character",            is.character(enc$mrn))
check("enc_id is character",         is.character(enc$enc_id))
check("dob is Date",                 inherits(enc$dob, "Date"))
check("sex is factor",               is.factor(enc$sex))
check("hospital_admission_date is datetime", inherits(enc$hospital_admission_date, "POSIXct"))
check("hospital_discharge_date is datetime", inherits(enc$hospital_discharge_date, "POSIXct"))
check("2 rows loaded",               nrow(enc) == 2L)

cat("\n-- Custom col_map (non-standard column names) --\n")
enc2 <- load_encounters(f_enc_cust, col_map = list(
     mrn                     = "patient_id",
     enc_id                  = "visit_id",
     dob                     = "date_of_birth",
     sex                     = "gender",
     ethnicity               = "ethnicity",
     hospital_admission_date = "admit_time",
     hospital_discharge_date = "discharge_time"
))
check("has expected output columns",
      all(c("mrn", "enc_id", "dob", "sex", "ethnicity",
            "hospital_admission_date", "hospital_discharge_date") %in% names(enc2)))
check("dob is Date",                 inherits(enc2$dob, "Date"))
check("sex is factor",               is.factor(enc2$sex))
check("hospital_admission_date is datetime", inherits(enc2$hospital_admission_date, "POSIXct"))
check("values match standard output", identical(enc$mrn, enc2$mrn) && identical(enc$enc_id, enc2$enc_id))

cat("\n-- Partial col_map override (remap only enc_id) --\n")
enc3 <- load_encounters(f_enc_cust, col_map = list(
     mrn                     = "patient_id",
     enc_id                  = "visit_id",
     dob                     = "date_of_birth",
     sex                     = "gender",
     ethnicity               = "ethnicity",
     hospital_admission_date = "admit_time",
     hospital_discharge_date = "discharge_time"
))
check("enc_id correctly remapped",   all(enc3$enc_id == c("e001", "e002")))

# =============================================================================
cat("\n=== load_labs ===\n")
# =============================================================================

cat("\n-- Default col_map (Epic standard columns) --\n")
labs <- load_labs(f_lab_std)
check("has expected output columns",
      all(c("enc_id", "specimen_taken_time", "description",
            "common_name", "result_value", "reference_unit") %in% names(labs)))
check("enc_id is character",         is.character(labs$enc_id))
check("3 rows loaded",               nrow(labs) == 3L)
check("drop_cols are absent",        !any(c("order_proc_id", "order_date", "line",
                                             "order_time", "result_time", "lab_status") %in% names(labs)))

cat("\n-- Custom col_map (non-standard enc_id and specimen_taken_time) --\n")
labs2 <- load_labs(f_lab_cust, col_map = list(
     enc_id              = "encounter_id",
     specimen_taken_time = "collection_time",
     description         = "description",
     common_name         = "common_name",
     result_value        = "result_value",
     reference_unit      = "reference_unit"
))
check("has expected output columns",
      all(c("enc_id", "specimen_taken_time") %in% names(labs2)))
check("enc_id correctly remapped",   all(labs2$enc_id == c("e001", "e001", "e002")))
check("drop_cols still absent",      !any(c("order_proc_id", "order_date") %in% names(labs2)))

cat("\n-- drop_cols = NULL retains all columns --\n")
labs3 <- load_labs(f_lab_std, drop_cols = NULL)
check("order_proc_id retained when drop_cols = NULL", "order_proc_id" %in% names(labs3))

# =============================================================================
cat("\n=== load_vitals ===\n")
# =============================================================================

cat("\n-- Default col_map (Epic standard columns) --\n")
vit <- load_vitals(f_vit_std)
check("has expected output columns",
      all(c("enc_id", "mrn", "flowsheet_name", "meas_value", "vital_time") %in% names(vit)))
check("enc_id is character",         is.character(vit$enc_id))
check("mrn is character",            is.character(vit$mrn))
check("5 rows loaded",               nrow(vit) == 5L)

cat("\n-- vitals_to_load filter --\n")
vit_hr <- load_vitals(f_vit_std, vitals_to_load = "Pulse")
check("filtered to pulse rows only", all(vit_hr$flowsheet_name == "pulse"))
check("correct row count after filter", nrow(vit_hr) == 2L)

cat("\n-- Custom col_map (non-standard enc_id and vital_time) --\n")
vit2 <- load_vitals(f_vit_cust, col_map = list(
     enc_id              = "encounter_id",
     mrn                 = "mrn",
     flowsheet_group     = "flowsheet_group",
     common_name         = "common_name",
     flowsheet_name      = "flowsheet_name",
     cust_list_map_value = "cust_list_map_value",
     meas_value          = "meas_value",
     units               = "units",
     vital_time          = "measure_time"
))
check("has expected output columns",
      all(c("enc_id", "mrn", "vital_time") %in% names(vit2)))
check("custom column names not present in output",
      !any(c("encounter_id", "measure_time") %in% names(vit2)))
check("values match standard output", identical(vit$enc_id, vit2$enc_id))

# =============================================================================
cat("\n=== clean_vitals ===\n")
# =============================================================================

cat("\n-- Default (no name_map, standard flowsheet names) --\n")
vit_clean <- clean_vitals(vit)
check("wide format: one row per enc_id/vital_time",
      nrow(vit_clean) == n_distinct(vit[c("enc_id", "vital_time")]))
check("hr column present",           "hr" %in% names(vit_clean))
check("sbp_ni / dbp_ni present",     all(c("sbp_ni", "dbp_ni") %in% names(vit_clean)))
check("sbp_art / dbp_art present",   all(c("sbp_art", "dbp_art") %in% names(vit_clean)))
check("spo2 present",                "spo2" %in% names(vit_clean))
check("hr is numeric",               is.numeric(vit_clean$hr))
check("sbp_ni is numeric",           is.numeric(vit_clean$sbp_ni))
check("map_ni computed (not all NA)", !all(is.na(vit_clean$map_ni)))

cat("\n-- name_map: recode non-standard flowsheet labels --\n")
vit_nonstandard <- vit %>%
     mutate(flowsheet_name = dplyr::recode(flowsheet_name,
                                           "pulse"           = "heart rate",
                                           "blood pressure"  = "bp nonin",
                                           "pulse oximetry"  = "spo2"))
vit_clean2 <- clean_vitals(vit_nonstandard, name_map = c(
     "heart rate" = "pulse",
     "bp nonin"   = "blood pressure",
     "spo2"       = "pulse oximetry"
))
check("hr present after name_map recode",    "hr" %in% names(vit_clean2))
check("sbp_ni present after name_map recode", "sbp_ni" %in% names(vit_clean2))
check("spo2 present after name_map recode",  "spo2" %in% names(vit_clean2))
check("hr values match standard output",
      identical(
           vit_clean2 %>% filter(!is.na(hr)) %>% pull(hr),
           vit_clean  %>% filter(!is.na(hr)) %>% pull(hr)
      ))

# =============================================================================
cat(sprintf("\n=== Synthetic data results: %d passed, %d failed ===\n", pass_count, fail_count))
# =============================================================================

on.exit(unlink(c(f_enc_std, f_enc_cust, f_lab_std, f_lab_cust, f_vit_std, f_vit_cust)))

# =============================================================================
cat("\n=== Real data tests ===\n")
# =============================================================================

real_files <- c(real_enc_file, real_labs_file, real_vitals_file)
if (all(nchar(real_files) == 0)) {
     cat("Skipped (no file paths set)\n")
} else {

     real_pass <- 0L
     real_fail <- 0L

     rcheck <- function(label, expr) {
          result <- tryCatch(isTRUE(expr), error = function(e) FALSE)
          if (result) {
               cat(sprintf("  PASS  %s\n", label))
               real_pass <<- real_pass + 1L
          } else {
               cat(sprintf("  FAIL  %s\n", label))
               real_fail <<- real_fail + 1L
          }
     }

     # ── load_encounters ──────────────────────────────────────────────────────
     if (nchar(real_enc_file) > 0) {
          cat("\n-- load_encounters --\n")
          enc_real <- load_encounters(real_enc_file)

          cat(sprintf("  Rows: %d | Encounters: %d | Patients: %d\n",
                      nrow(enc_real), n_distinct(enc_real$enc_id), n_distinct(enc_real$mrn)))
          cat(sprintf("  Admission range: %s to %s\n",
                      format(min(enc_real$hospital_admission_date, na.rm = TRUE), "%Y-%m-%d"),
                      format(max(enc_real$hospital_admission_date, na.rm = TRUE), "%Y-%m-%d")))
          cat(sprintf("  Sex distribution: %s\n",
                      paste(names(table(enc_real$sex)), table(enc_real$sex), sep = "=", collapse = ", ")))

          na_rates <- sapply(enc_real[c("mrn", "enc_id", "dob", "sex",
                                        "hospital_admission_date", "hospital_discharge_date")],
                             function(x) round(mean(is.na(x)) * 100, 1))
          cat("  NA rates (%) for key columns:\n")
          for (nm in names(na_rates)) cat(sprintf("    %s: %.1f%%\n", nm, na_rates[[nm]]))

          rcheck("has expected output columns",
                 all(c("mrn", "enc_id", "dob", "sex", "ethnicity",
                       "hospital_admission_date", "hospital_discharge_date") %in% names(enc_real)))
          rcheck("mrn is character",            is.character(enc_real$mrn))
          rcheck("enc_id is character",         is.character(enc_real$enc_id))
          rcheck("dob is Date",                 inherits(enc_real$dob, "Date"))
          rcheck("sex is factor",               is.factor(enc_real$sex))
          rcheck("hospital_admission_date is datetime", inherits(enc_real$hospital_admission_date, "POSIXct"))
          rcheck("hospital_discharge_date is datetime", inherits(enc_real$hospital_discharge_date, "POSIXct"))
          rcheck("at least 1 row loaded",       nrow(enc_real) > 0)
          rcheck("no duplicate enc_ids",        !anyDuplicated(enc_real$enc_id))
          rcheck("admission before discharge",
                 all(enc_real$hospital_admission_date <= enc_real$hospital_discharge_date, na.rm = TRUE))
          rcheck("admissions not in the future",
                 all(enc_real$hospital_admission_date <= Sys.time(), na.rm = TRUE))
     }

     # ── load_labs ────────────────────────────────────────────────────────────
     if (nchar(real_labs_file) > 0) {
          cat("\n-- load_labs --\n")
          labs_real <- load_labs(real_labs_file)

          cat(sprintf("  Rows: %d | Encounters: %d\n",
                      nrow(labs_real), n_distinct(labs_real$enc_id)))
          cat(sprintf("  Distinct lab names (common_name): %d\n",
                      n_distinct(labs_real$common_name)))
          cat("  Top 5 lab types:\n")
          top5 <- sort(table(labs_real$common_name), decreasing = TRUE)[1:min(5, nlevels(factor(labs_real$common_name)))]
          for (nm in names(top5)) cat(sprintf("    %s: %d\n", nm, top5[[nm]]))

          na_rates <- sapply(labs_real[c("enc_id", "specimen_taken_time", "common_name", "result_value")],
                             function(x) round(mean(is.na(x)) * 100, 1))
          cat("  NA rates (%) for key columns:\n")
          for (nm in names(na_rates)) cat(sprintf("    %s: %.1f%%\n", nm, na_rates[[nm]]))

          rcheck("has expected output columns",
                 all(c("enc_id", "specimen_taken_time", "description",
                       "common_name", "result_value", "reference_unit") %in% names(labs_real)))
          rcheck("enc_id is character",   is.character(labs_real$enc_id))
          rcheck("at least 1 row loaded", nrow(labs_real) > 0)
          rcheck("drop_cols are absent",  !any(c("order_proc_id", "order_date", "line",
                                                  "order_time", "result_time", "lab_status") %in% names(labs_real)))
     }

     # ── load_vitals + clean_vitals ────────────────────────────────────────────
     if (nchar(real_vitals_file) > 0) {
          cat("\n-- load_vitals --\n")
          vit_real <- load_vitals(real_vitals_file,
                                  col_map = list(
                                       enc_id         = "pat_enc_csn_id",
                                       mrn            = "mrn",
                                       flowsheet_name = "display_name",
                                       meas_value     = "measure_value",
                                       vital_time     = "recorded_time"
                                  ))

          cat(sprintf("  Rows: %d | Encounters: %d\n",
                      nrow(vit_real), n_distinct(vit_real$enc_id)))
          cat(sprintf("  Distinct flowsheet names: %d\n", n_distinct(vit_real$flowsheet_name)))
          cat("  Top 8 flowsheet names:\n")
          top8 <- sort(table(vit_real$flowsheet_name), decreasing = TRUE)[1:min(8, nlevels(factor(vit_real$flowsheet_name)))]
          for (nm in names(top8)) cat(sprintf("    %s: %d\n", nm, top8[[nm]]))

          rcheck("has expected output columns",
                 all(c("enc_id", "mrn", "flowsheet_name", "meas_value", "vital_time") %in% names(vit_real)))
          rcheck("enc_id is character",   is.character(vit_real$enc_id))
          rcheck("mrn is character",      is.character(vit_real$mrn))
          rcheck("at least 1 row loaded", nrow(vit_real) > 0)

          cat("\n-- clean_vitals --\n")
          vit_real_clean <- clean_vitals(vit_real)

          cat(sprintf("  Wide rows: %d | Encounters: %d\n",
                      nrow(vit_real_clean), n_distinct(vit_real_clean$enc_id)))
          cat("  Columns present:", paste(names(vit_real_clean), collapse = ", "), "\n")

          vital_cols <- intersect(c("hr", "sbp_ni", "dbp_ni", "map_ni",
                                    "sbp_art", "dbp_art", "map_art", "resp", "spo2", "cvp"),
                                  names(vit_real_clean))
          if (length(vital_cols) > 0) {
               na_rates <- sapply(vit_real_clean[vital_cols],
                                  function(x) round(mean(is.na(x)) * 100, 1))
               cat("  NA rates (%) for vital columns:\n")
               for (nm in names(na_rates)) cat(sprintf("    %s: %.1f%%\n", nm, na_rates[[nm]]))
          }

          rcheck("at least 1 row in wide output",  nrow(vit_real_clean) > 0)
          rcheck("wide output has fewer rows than long",  nrow(vit_real_clean) < nrow(vit_real))
          rcheck("vital columns are numeric",
                 all(sapply(vit_real_clean[vital_cols], is.numeric)))
          if ("hr" %in% names(vit_real_clean))
               rcheck("hr values in plausible range (20-300)",
                      all(vit_real_clean$hr >= 20 & vit_real_clean$hr <= 300, na.rm = TRUE))
          if ("sbp_ni" %in% names(vit_real_clean))
               rcheck("sbp_ni values in plausible range (40-300)",
                      all(vit_real_clean$sbp_ni >= 40 & vit_real_clean$sbp_ni <= 300, na.rm = TRUE))
          if ("spo2" %in% names(vit_real_clean))
               rcheck("spo2 values in plausible range (50-100)",
                      all(vit_real_clean$spo2 >= 50 & vit_real_clean$spo2 <= 100, na.rm = TRUE))
     }

     cat(sprintf("\n=== Real data results: %d passed, %d failed ===\n", real_pass, real_fail))
}
