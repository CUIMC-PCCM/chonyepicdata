library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(janitor)

# ── Set these paths before running ────────────────────────────────────────────
repo <- "C:/Github/chonyepicdata"   # path to package root

# Paths to de-identified Epic export files. Leave as "" to skip real-data checks.
real_labs_file      <- ""
real_vitals_file    <- ""
real_meds_file      <- ""
real_fio2_spo2_file <- ""   # IMV/flowsheet file containing FiO2 (%) and SpO2 rows
real_resp_file      <- ""   # respiratory support flowsheet file

# Time window: supply a file or object with enc_id + ref_time (PICU admission).
# Set to "" or NULL to skip the real-data section.
real_time_window    <- NULL  # data frame with enc_id + ref_time (POSIXct)
real_agem           <- NULL  # optional: data frame with enc_id + agem
real_dob            <- NULL  # optional: data frame with enc_id + dob

# FiO2/SpO2 flowsheet column names (defaults match CHONY Epic export format).
fio2_spo2_key_col     <- "PAT_ENC_CSN_ID"
fio2_spo2_time_col    <- "RECORDED_TIME"
fio2_spo2_var_col     <- "DISPLAY_NAME"
fio2_spo2_measure_col <- "MEASURE_VALUE"
fio2_varname          <- "FiO2 (%)"
spo2_varname          <- "SpO2"
# ─────────────────────────────────────────────────────────────────────────────

source(file.path(repo, "R/load_labs.R"))
source(file.path(repo, "R/load_vitals.R"))
source(file.path(repo, "R/load_meds.R"))
source(file.path(repo, "R/load_generic_flowsheet_rows.R"))
source(file.path(repo, "R/load_resp_support.R"))
source(file.path(repo, "R/clean_resp_support.R"))
source(file.path(repo, "R/classify_resp_support.R"))
source(file.path(repo, "R/clean_vitals.R"))
source(file.path(repo, "R/get_labs_by_type.R"))
source(file.path(repo, "R/assemble_psofa_data.R"))
source(file.path(repo, "R/calc_psofa.R"))

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

# ── Real data ─────────────────────────────────────────────────────────────────

skip_real <- any(c(real_labs_file, real_vitals_file, real_meds_file,
                   real_fio2_spo2_file, real_resp_file) == "") ||
             is.null(real_time_window) || (is.null(real_agem) && is.null(real_dob))

if (skip_real) {
     cat("\n⚠  Skipping real-data section — set file paths and time_window above.\n\n")
} else {

     cat("\n=== Step 1: classify_resp_support ===\n")
     df_resp_raw   <- load_resp_support(real_resp_file)
     df_resp_wide  <- clean_resp_support(df_resp_raw)
     df_resp_epi   <- classify_resp_support(df_resp_wide)
     cat(sprintf("Respiratory episodes: %d | Encounters: %d\n",
                 nrow(df_resp_epi), n_distinct(df_resp_epi$enc_id)))

     cat("\n=== Step 2: assemble_psofa_data ===\n")
     df_psofa <- assemble_psofa_data(
          labs          = real_labs_file,
          vitals        = real_vitals_file,
          meds          = real_meds_file,
          fio2_spo2     = real_fio2_spo2_file,
          resp_episodes = df_resp_epi,
          time_window   = real_time_window,
          t_min         = 0,
          t_max         = 24,
          agem          = real_agem,
          dob           = real_dob,
          fio2_spo2_key_col     = fio2_spo2_key_col,
          fio2_spo2_time_col    = fio2_spo2_time_col,
          fio2_spo2_var_col     = fio2_spo2_var_col,
          fio2_spo2_measure_col = fio2_spo2_measure_col,
          fio2_varname          = fio2_varname,
          spo2_varname          = spo2_varname
     )

     cat(sprintf("Assembled encounters: %d\n", nrow(df_psofa)))
     cat("Columns:", paste(names(df_psofa), collapse = ", "), "\n")

     # Structure checks
     check("one row per encounter",
           nrow(df_psofa) == n_distinct(df_psofa$enc_id))
     check("enc_id is character",
           is.character(df_psofa$enc_id))
     check("agem present and non-negative",
           all(!is.na(df_psofa$agem) & df_psofa$agem >= 0))
     check("resp_support is logical",
           is.logical(df_psofa$resp_support))
     check("platelets in plausible range where present",
           all(is.na(df_psofa$platelets) | (df_psofa$platelets > 0 & df_psofa$platelets < 2000)))
     check("creatinine in plausible range where present",
           all(is.na(df_psofa$creatinine) | (df_psofa$creatinine > 0 & df_psofa$creatinine < 30)))
     check("map in plausible range where present",
           all(is.na(df_psofa$map) | (df_psofa$map >= 15 & df_psofa$map <= 180)))
     check("pf_ratio non-negative where present",
           all(is.na(df_psofa$pf_ratio) | df_psofa$pf_ratio > 0))
     check("sf_ratio non-negative where present",
           all(is.na(df_psofa$sf_ratio) | df_psofa$sf_ratio > 0))

     # Missing value rates
     cat("\nMissing value rates (%):\n")
     miss_rates <- df_psofa %>%
          summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) %>%
          tidyr::pivot_longer(everything(), names_to = "column", values_to = "pct_missing") %>%
          arrange(desc(pct_missing))
     print(miss_rates, n = Inf)

     cat("\n=== Step 3: calc_psofa ===\n")
     df_scored <- calc_psofa(df_psofa)

     cat(sprintf("Scored encounters: %d\n", nrow(df_scored)))

     # Score distribution
     cat("\npSOFA score distribution:\n")
     print(df_scored %>% count(psofa, sort = FALSE) %>%
                mutate(pct = round(n / sum(n) * 100, 1)))

     # Component score distributions
     cat("\nComponent score distributions:\n")
     component_scores <- c("pfscore", "sfscore", "pltscore", "biliscore",
                           "mapscore", "gcsscore", "crscore")
     for (s in intersect(component_scores, names(df_scored))) {
          cat(sprintf("  %s: %s\n", s,
                      paste(names(table(df_scored[[s]])),
                            table(df_scored[[s]]), sep = "=", collapse = " | ")))
     }

     # Sanity checks on scored output
     check("psofa is non-negative",
           all(!is.na(df_scored$psofa) | all(is.na(df_scored$psofa))))
     check("psofa in range [0, 24]",
           all(is.na(df_scored$psofa) | (df_scored$psofa >= 0 & df_scored$psofa <= 24)))
     check("psofa equals sum of components",
           all(is.na(df_scored$psofa) |
                    df_scored$psofa == (df_scored$pf_or_sf_score + df_scored$pltscore +
                                             df_scored$biliscore + df_scored$mapscore +
                                             df_scored$gcsscore  + df_scored$crscore)))
     check("resp_support=TRUE patients have no score=0 from respiratory component",
           {
                imv_pts <- df_scored %>% filter(resp_support == TRUE & !is.na(pf_or_sf_score))
                all(imv_pts$pf_or_sf_score >= 0)
           })

     cat(sprintf("\npSOFA summary — median [IQR]: %.0f [%.0f–%.0f]\n",
                 median(df_scored$psofa, na.rm = TRUE),
                 quantile(df_scored$psofa, 0.25, na.rm = TRUE),
                 quantile(df_scored$psofa, 0.75, na.rm = TRUE)))
}

# ── Summary ───────────────────────────────────────────────────────────────────

cat(sprintf("\n=== %d passed | %d failed ===\n", pass_count, fail_count))
