.libPaths(c('C:/Users/Andy/AppData/Local/R/win-library/4.4', .libPaths()))

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(janitor)
library(zoo)
library(purrr)

repo <- "C:/Github/chonyepicdata"
source(file.path(repo, "R/load_resp_support.R"))
source(file.path(repo, "R/clean_resp_support.R"))
source(file.path(repo, "R/classify_resp_support.R"))

data_file <- "C:/Users/Andy/OneDrive - Columbia University Irving Medical Center/Research/data/early_mobilization/Report 8E - Mechanical Ventilation_deidentified.txt"

# ── Step 1: load ───────────────────────────────────────────────────────────────
cat("\n=== Step 1: load_resp_support ===\n")
df_raw <- load_resp_support(data_file)
cat(sprintf("Rows: %d | Encounters: %d\n", nrow(df_raw), n_distinct(df_raw$enc_id)))
cat("Columns:", paste(names(df_raw), collapse = ", "), "\n")
cat("Timestamp sample:", head(as.character(df_raw$resp_meas_time), 3), "\n")

# ── Step 2: clean ──────────────────────────────────────────────────────────────
cat("\n=== Step 2: clean_resp_support ===\n")
df_wide <- clean_resp_support(df_raw)
cat(sprintf("Rows: %d | Encounters: %d\n", nrow(df_wide), n_distinct(df_wide$enc_id)))
cat("Columns:", paste(names(df_wide), collapse = ", "), "\n")

# Check for unexpected list columns (pivot_wider duplicate issue)
list_cols <- names(df_wide)[sapply(df_wide, is.list)]
if (length(list_cols) > 0) {
     cat("WARNING - list columns (duplicates not resolved):", paste(list_cols, collapse = ", "), "\n")
} else {
     cat("OK - no list columns\n")
}

# Check NA rates for key classification columns
key_cols <- c("o2_deliv_method", "vent_mode", "vent_type", "vent_status",
              "map_vent", "etco2", "peep", "hfnc_status", "bipap_status")
present <- intersect(key_cols, names(df_wide))
na_rates <- sapply(df_wide[present], function(x) round(mean(is.na(x)) * 100, 1))
cat("\nNA rates for key classification columns (%):\n")
print(na_rates)

# ── Step 3: classify ──────────────────────────────────────────────────────────
cat("\n=== Step 3: classify_resp_support ===\n")
df_episodes <- classify_resp_support(df_wide)
cat(sprintf("Episodes: %d | Encounters: %d\n", nrow(df_episodes), n_distinct(df_episodes$enc_id)))

# Support level distribution
cat("\nSupport level distribution:\n")
print(df_episodes %>% count(current_support, sort = TRUE) %>% mutate(pct = round(n / sum(n) * 100, 1)))

# Duration summary by support type
cat("\nEpisode duration summary (hours) by support level:\n")
print(df_episodes %>%
     mutate(hours = as.numeric(timediff, "hours")) %>%
     group_by(current_support) %>%
     summarise(n = n(), median_hrs = round(median(hours, na.rm=TRUE), 1),
               p25 = round(quantile(hours, .25, na.rm=TRUE), 1),
               p75 = round(quantile(hours, .75, na.rm=TRUE), 1),
               .groups = "drop") %>%
     arrange(desc(n)))

# Check for NA episodes or timestamps
na_support  <- sum(is.na(df_episodes$current_support))
na_start    <- sum(is.na(df_episodes$support_time_start))
na_stop     <- sum(is.na(df_episodes$support_time_stop))
na_timediff <- sum(is.na(df_episodes$timediff))
cat(sprintf("\nNA checks — current_support: %d | start: %d | stop: %d | timediff: %d\n",
            na_support, na_start, na_stop, na_timediff))

# Check episode numbering is sequential per encounter
ep_check <- df_episodes %>%
     group_by(enc_id) %>%
     summarise(ok = identical(support_episode, seq_len(n())), .groups = "drop")
cat(sprintf("Episode numbering sequential: %s (%d/%d encounters OK)\n",
            if (all(ep_check$ok)) "YES" else "NO",
            sum(ep_check$ok), nrow(ep_check)))

cat("\n=== Pipeline completed successfully ===\n")
