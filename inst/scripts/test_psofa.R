library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(janitor)

# ── Set these paths before running ────────────────────────────────────────────
repo <- "C:/Github/chonyepicdata"   # path to package root

# Paths to de-identified Epic export files. Leave as "" to skip real-data checks.
data_path <- paste0(Sys.getenv('onedrive'), '/Research/data/early_mobilization/')
real_labs_file      <- ''
real_vitals_file    <- ''
real_meds_file      <- ''
real_fio2_spo2_file <- ''   # IMV/flowsheet file containing FiO2 (%) and SpO2 rows
real_resp_file      <- ''   # respiratory support flowsheet file

# Max encounters to load for real data testing, set to Inf otherwise
max_encounters <- 100

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
                   real_fio2_spo2_file, real_resp_file) == "")

if (skip_real) {

     cat("\n⚠  Real-data file paths not set — running synthetic calc_psofa tests.\n\n")

     # Helper: build a single-row test data frame with sensible defaults
     base_row <- function(id, agem = 72, pf = NA_real_, sf = NA_real_,
                          plt = 200, tbili = 0.5, cr = 0.5,
                          map = 70, resp = FALSE, gcs = 15,
                          epi = NA_real_, norepi = NA_real_,
                          dopa = NA_real_, dobut = NA_real_) {
          data.frame(
               enc_id       = as.character(id),
               agem         = agem,
               pf_ratio     = pf,
               sf_ratio     = sf,
               platelets    = plt,
               tbili        = tbili,
               creatinine   = cr,
               map          = map,
               resp_support = resp,
               gcs          = gcs,
               epi          = epi,
               norepi       = norepi,
               dopa         = dopa,
               dobut        = dobut,
               stringsAsFactors = FALSE
          )
     }

     df_synth <- do.call(rbind, list(
          # pfscore thresholds (agem=72, agecat=5)
          base_row( 1, pf = 400, resp = FALSE),   # pfscore=0  (>=400)
          base_row( 2, pf = 350, resp = FALSE),   # pfscore=1  (300-399)
          base_row( 3, pf = 250, resp = FALSE),   # pfscore=2  (200-299)
          base_row( 4, pf = 150, resp = TRUE),    # pfscore=3  (100-199 + resp)
          base_row( 5, pf =  90, resp = TRUE),    # pfscore=4  (<100 + resp)
          base_row( 6, pf = 150, resp = FALSE),   # pfscore=0  (100-199, no resp → TRUE catch)
          # sfscore thresholds (pf_ratio=NA, agem=72)
          base_row( 7, sf = 300, resp = FALSE),   # sfscore=0  (>291)
          base_row( 8, sf = 275, resp = FALSE),   # sfscore=1  (264-291)
          base_row( 9, sf = 240, resp = FALSE),   # sfscore=2  (221-263)
          base_row(10, sf = 180, resp = TRUE),    # sfscore=3  (148-220 + resp)
          base_row(11, sf = 100, resp = TRUE),    # sfscore=4  (<148 + resp)
          # pltscore thresholds
          base_row(12, plt = 200),                # pltscore=0 (>=150)
          base_row(13, plt = 120),                # pltscore=1 (100-149)
          base_row(14, plt =  75),                # pltscore=2 (50-99)
          base_row(15, plt =  35),                # pltscore=3 (20-49)
          base_row(16, plt =  15),                # pltscore=4 (<20)
          # biliscore thresholds
          base_row(17, tbili =  0.5),             # biliscore=0 (<1.2)
          base_row(18, tbili =  1.5),             # biliscore=1 (1.2-1.9)
          base_row(19, tbili =  3.0),             # biliscore=2 (2-5.9)
          base_row(20, tbili =  8.0),             # biliscore=3 (6-11.9)
          base_row(21, tbili = 13.0),             # biliscore=4 (>=12)
          # mapscore (agem=72, agecat=5, threshold=65)
          base_row(22, map = 70),                               # mapscore=0 (above threshold, no pressors)
          base_row(23, map = 60),                               # mapscore=1 (below threshold)
          base_row(24, map = 70, dopa =  3),                    # mapscore=2 (low-dose dopa)
          base_row(25, map = 70, epi  =  0.05),                 # mapscore=3 (low-dose epi)
          base_row(26, map = 70, epi  =  0.15),                 # mapscore=4 (high-dose epi)
          # gcsscore thresholds
          base_row(27, gcs = 15),                # gcsscore=0 (15)
          base_row(28, gcs = 13),                # gcsscore=1 (13-14)
          base_row(29, gcs = 11),                # gcsscore=2 (10-12)
          base_row(30, gcs =  7),                # gcsscore=3 (6-9)
          base_row(31, gcs =  4),                # gcsscore=4 (<6)
          # crscore (agem=72, agecat=5)
          base_row(32, cr = 0.5),                # crscore=0 (<0.7)
          base_row(33, cr = 0.8),                # crscore=1 (0.7-1.0)
          base_row(34, cr = 1.3),                # crscore=2 (1.1-1.7)
          base_row(35, cr = 2.0),                # crscore=3 (1.8-2.5)
          base_row(36, cr = 3.0),                # crscore=4 (>=2.6)
          # age-specific MAP thresholds (one per agecat, map just below threshold)
          base_row(37, agem =   0, map = 40),    # agecat=1, threshold=46 → mapscore=1
          base_row(38, agem =   6, map = 50),    # agecat=2, threshold=55 → mapscore=1
          base_row(39, agem =  18, map = 55),    # agecat=3, threshold=60 → mapscore=1
          base_row(40, agem =  36, map = 58),    # agecat=4, threshold=62 → mapscore=1
          base_row(41, agem =  72, map = 60),    # agecat=5, threshold=65 → mapscore=1
          base_row(42, agem = 150, map = 62),    # agecat=6, threshold=67 → mapscore=1
          base_row(43, agem = 216, map = 65),    # agecat=7, threshold=70 → mapscore=1
          # known-total row: pfscore=3 + pltscore=2 + biliscore=2 + mapscore=1 + gcsscore=2 + crscore=3 = 13
          base_row(44, pf = 150, resp = TRUE, plt = 75, tbili = 3.0, cr = 2.0, map = 60, gcs = 11)
     ))

     df_ss <- calc_psofa(df_synth)

     cat("--- pfscore ---\n")
     check("pfscore=0 when pf>=400",                   df_ss$pfscore[1]  == 0L)
     check("pfscore=1 when pf in 300-399",             df_ss$pfscore[2]  == 1L)
     check("pfscore=2 when pf in 200-299",             df_ss$pfscore[3]  == 2L)
     check("pfscore=3 when pf in 100-199 + resp",      df_ss$pfscore[4]  == 3L)
     check("pfscore=4 when pf<100 + resp",             df_ss$pfscore[5]  == 4L)
     check("pfscore=0 when pf in 100-199 but no resp", df_ss$pfscore[6]  == 0L)

     cat("--- sfscore ---\n")
     check("sfscore=0 when sf>291",                    df_ss$sfscore[7]  == 0L)
     check("sfscore=1 when sf in 264-291",             df_ss$sfscore[8]  == 1L)
     check("sfscore=2 when sf in 221-263",             df_ss$sfscore[9]  == 2L)
     check("sfscore=3 when sf in 148-220 + resp",      df_ss$sfscore[10] == 3L)
     check("sfscore=4 when sf<148 + resp",             df_ss$sfscore[11] == 4L)

     cat("--- pltscore ---\n")
     check("pltscore=0 when plt>=150",                 df_ss$pltscore[12] == 0L)
     check("pltscore=1 when plt in 100-149",           df_ss$pltscore[13] == 1L)
     check("pltscore=2 when plt in 50-99",             df_ss$pltscore[14] == 2L)
     check("pltscore=3 when plt in 20-49",             df_ss$pltscore[15] == 3L)
     check("pltscore=4 when plt<20",                   df_ss$pltscore[16] == 4L)

     cat("--- biliscore ---\n")
     check("biliscore=0 when tbili<1.2",               df_ss$biliscore[17] == 0L)
     check("biliscore=1 when tbili in 1.2-1.9",        df_ss$biliscore[18] == 1L)
     check("biliscore=2 when tbili in 2.0-5.9",        df_ss$biliscore[19] == 2L)
     check("biliscore=3 when tbili in 6.0-11.9",       df_ss$biliscore[20] == 3L)
     check("biliscore=4 when tbili>=12",               df_ss$biliscore[21] == 4L)

     cat("--- mapscore ---\n")
     check("mapscore=0: MAP above threshold, no pressors",  df_ss$mapscore[22] == 0L)
     check("mapscore=1: MAP below agecat-5 threshold (65)", df_ss$mapscore[23] == 1L)
     check("mapscore=2: low-dose dopamine (0-5)",           df_ss$mapscore[24] == 2L)
     check("mapscore=3: low-dose epi (0-0.1)",              df_ss$mapscore[25] == 3L)
     check("mapscore=4: high-dose epi (>0.1)",              df_ss$mapscore[26] == 4L)

     cat("--- gcsscore ---\n")
     check("gcsscore=0 when gcs=15",                   df_ss$gcsscore[27] == 0L)
     check("gcsscore=1 when gcs in 13-14",             df_ss$gcsscore[28] == 1L)
     check("gcsscore=2 when gcs in 10-12",             df_ss$gcsscore[29] == 2L)
     check("gcsscore=3 when gcs in 6-9",               df_ss$gcsscore[30] == 3L)
     check("gcsscore=4 when gcs<6",                    df_ss$gcsscore[31] == 4L)

     cat("--- crscore (agecat 5: 60-143 months) ---\n")
     check("crscore=0 when cr<0.7",                    df_ss$crscore[32] == 0L)
     check("crscore=1 when cr in 0.7-1.0",             df_ss$crscore[33] == 1L)
     check("crscore=2 when cr in 1.1-1.7",             df_ss$crscore[34] == 2L)
     check("crscore=3 when cr in 1.8-2.5",             df_ss$crscore[35] == 3L)
     check("crscore=4 when cr>=2.6",                   df_ss$crscore[36] == 4L)

     cat("--- age-specific MAP thresholds ---\n")
     check("mapscore=1: agecat 1 (agem=0),   map=40 < 46",  df_ss$mapscore[37] == 1L)
     check("mapscore=1: agecat 2 (agem=6),   map=50 < 55",  df_ss$mapscore[38] == 1L)
     check("mapscore=1: agecat 3 (agem=18),  map=55 < 60",  df_ss$mapscore[39] == 1L)
     check("mapscore=1: agecat 4 (agem=36),  map=58 < 62",  df_ss$mapscore[40] == 1L)
     check("mapscore=1: agecat 5 (agem=72),  map=60 < 65",  df_ss$mapscore[41] == 1L)
     check("mapscore=1: agecat 6 (agem=150), map=62 < 67",  df_ss$mapscore[42] == 1L)
     check("mapscore=1: agecat 7 (agem=216), map=65 < 70",  df_ss$mapscore[43] == 1L)

     cat("--- total pSOFA ---\n")
     check("known total psofa=13 (pf=3+plt=2+bili=2+map=1+gcs=2+cr=3)",
           df_ss$psofa[44] == 13L)
     check("psofa equals sum of all component scores",
           all(is.na(df_ss$psofa) |
                    df_ss$psofa == (df_ss$pf_or_sf_score + df_ss$pltscore +
                                         df_ss$biliscore + df_ss$mapscore +
                                         df_ss$gcsscore  + df_ss$crscore)))

} else {

     cat("\n=== Step 1: classify_resp_support ===\n")
     df_resp_raw   <- load_resp_support(real_resp_file)

     # Limit real encounters if this was marked
     if(is.numeric(max_encounters)) {
          if(nrow(distinct(df_resp_raw, enc_id)) >= max_encounters) {
               df_resp_raw <- df_resp_raw %>%
                    group_by(enc_id) %>%
                    mutate(distinct_id = cur_group_id()) %>%
                    ungroup() %>%
                    filter(distinct_id < max_encounters) %>%
                    select(-distinct_id)

               enc_to_use <- pull(distinct(df_resp_raw, enc_id), enc_id)
          }
     }

     df_resp_wide  <- clean_resp_support(df_resp_raw)
     df_resp_epi   <- classify_resp_support(df_resp_wide)
     cat(sprintf("Respiratory episodes: %d | Encounters: %d\n",
                 nrow(df_resp_epi), n_distinct(df_resp_epi$enc_id)))

     # If no time window provided, use a broad window capturing all data.
     # Results will not be clinically meaningful but verify the pipeline runs.
     if (is.null(real_time_window)) {
          cat("⚠  time_window not provided — using broad window (all available data)\n")
          real_time_window <- df_resp_epi %>%
               distinct(enc_id) %>%
               mutate(t_start = ymd_hms("2000-01-01 00:00:00"),
                      t_end   = ymd_hms("2099-12-31 00:00:00"))
     }

     # If no age provided, randomize (0–17 years) for pipeline testing only.
     if (is.null(real_agem) && is.null(real_dob)) {
          cat("⚠  agem/dob not provided — randomizing ages for pipeline testing only\n")
          set.seed(42)
          real_agem <- real_time_window %>%
               distinct(enc_id) %>%
               mutate(agem = sample(1L:215L, n(), replace = TRUE))
     }

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
     check("psofa non-negative where present",
           all(is.na(df_scored$psofa) | df_scored$psofa >= 0))
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

     # ── Pressor diagnostic ────────────────────────────────────────────────────
     cat("\n=== pSOFA pressor diagnostic (all encounters in meds file) ===\n")
     df_meds_all <- load_meds(real_meds_file)
     n_enc_meds  <- n_distinct(df_meds_all$enc_id)

     pressor_pattern <- 'epinephrine|norepinephrine|dopamine|dobutamine'
     infusion_filter <- df_meds_all %>%
          filter(str_detect(med_name, pressor_pattern)) %>%
          filter(dose_unit %in% c('mcg/kg/min', 'milliunits/kg/min') |
                      frequency == 'continuous') %>%
          filter(!str_detect(med_name, 'topical|cream|ointment|ophthalm|nasal|inhaled')) %>%
          mutate(agent = case_when(
               str_detect(med_name, 'norepinephrine') ~ 'norepinephrine',
               str_detect(med_name, 'epinephrine')    ~ 'epinephrine',
               str_detect(med_name, 'dopamine')       ~ 'dopamine',
               str_detect(med_name, 'dobutamine')     ~ 'dobutamine'
          ))

     n_any_pressor <- n_distinct(infusion_filter$enc_id)
     cat(sprintf("Total encounters in meds file:    %d\n", n_enc_meds))
     cat(sprintf("Encounters with any pressor:      %d (%.1f%%)\n",
                 n_any_pressor, n_any_pressor / n_enc_meds * 100))

     cat("\nBy agent:\n")
     pressor_summary <- infusion_filter %>%
          group_by(agent) %>%
          summarize(n_enc = n_distinct(enc_id),
                    pct   = round(n_distinct(enc_id) / n_enc_meds * 100, 1),
                    .groups = 'drop') %>%
          arrange(desc(n_enc))
     print(pressor_summary)

     cat("\nSample matched med names (up to 5 per agent):\n")
     infusion_filter %>%
          distinct(agent, med_name) %>%
          group_by(agent) %>%
          slice_head(n = 5) %>%
          { cat(paste0("  ", .$agent, ": ", .$med_name, collapse = "\n"), "\n") }
}

# ── Summary ───────────────────────────────────────────────────────────────────

cat(sprintf("\n=== %d passed | %d failed ===\n", pass_count, fail_count))
