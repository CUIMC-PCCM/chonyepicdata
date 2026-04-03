suppressPackageStartupMessages({
     library(dplyr); library(readr); library(stringr)
     library(lubridate); library(tidyr); library(janitor)
     library(zoo); library(purrr)
})

# ── Set these paths before running ────────────────────────────────────────────
repo      <- "C:/Github/chonyepicdata"   # path to package root
data_file <- ""                          # path to de-identified resp support file
# ─────────────────────────────────────────────────────────────────────────────

source(file.path(repo, "R/load_resp_support.R"))
source(file.path(repo, "R/clean_resp_support.R"))
source(file.path(repo, "R/classify_resp_support.R"))

df_raw  <- load_resp_support(data_file)
df_wide <- clean_resp_support(df_raw)
df_eps  <- classify_resp_support(df_wide, verbose = FALSE)

first3 <- unique(df_eps$enc_id)[1:3]

df_eps %>%
     filter(enc_id %in% first3) %>%
     arrange(enc_id, support_episode) %>%
     as.data.frame() %>%
     print()
