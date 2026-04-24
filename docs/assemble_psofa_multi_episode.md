# Fix: assemble_psofa_data() multi-episode support

## Problem

When a patient has multiple ventilation episodes (multiple rows per `enc_id` in `time_window`),
`assemble_psofa_data()` returns duplicate `enc_id` rows in the output with no way to identify
which row corresponds to which episode. All intermediate data frames group and join by `enc_id`
alone, so values from different episodes are either collapsed together (labs, MAP, pressors) or
the output rows are ambiguous.

**Root cause:** `t_start` is used internally for time-window filtering but is dropped before
the final assembly (line 482: `tw %>% select(enc_id)`). All `group_by()` and `left_join()`
calls in intermediate steps use `enc_id` only.

## Fix

Thread `t_start` through all intermediate data frames as a secondary key alongside `enc_id`,
and include it in the final output. `t_end` does not need to be in the output but must remain
available during intermediate joins (it already is via the `inner_join(tw, ...)` pattern).

The output should gain a `t_start` column so callers can join pSOFA scores back to their
episode data. The docstring `@return` should be updated accordingly.

---

## Changes required in `R/assemble_psofa_data.R`

### 1. `lab_with_fallback()` helper (lines 182–211)

This helper is called for creatinine, platelets, and tbili. All three changes follow the
same pattern.

**Line 184** — early-exit branch for missing variable:
```r
# Before
out <- tw %>% select(enc_id)
# After
out <- tw %>% select(enc_id, t_start)
```

**Lines 192–194** — in-window worst value:
```r
# Before
group_by(enc_id) %>%
summarize(across(dplyr::all_of(var), ~ worst_fn(.x, na.rm = TRUE)), .groups = 'drop')
# After
group_by(enc_id, t_start) %>%
summarize(across(dplyr::all_of(var), ~ worst_fn(.x, na.rm = TRUE)), .groups = 'drop')
```

**Lines 198–201** — pre-window lookback (most recent value before each window):
```r
# Before
group_by(enc_id) %>%
dplyr::slice_max(specimen_taken_time, n = 1, with_ties = FALSE) %>%
ungroup() %>%
select(enc_id, dplyr::all_of(setNames(var, 'val_lb')))
# After
group_by(enc_id, t_start) %>%
dplyr::slice_max(specimen_taken_time, n = 1, with_ties = FALSE) %>%
ungroup() %>%
select(enc_id, t_start, dplyr::all_of(setNames(var, 'val_lb')))
```

**Lines 202–205** — assembly of in_win + lookback:
```r
# Before
out <- tw %>%
     select(enc_id) %>%
     left_join(in_win,    by = 'enc_id') %>%
     left_join(lookback,  by = 'enc_id')
# After
out <- tw %>%
     select(enc_id, t_start) %>%
     left_join(in_win,    by = c('enc_id', 't_start')) %>%
     left_join(lookback,  by = c('enc_id', 't_start'))
```

**Line 210** — return value:
```r
# Before
select(enc_id, dplyr::all_of(var))
# After
select(enc_id, t_start, dplyr::all_of(var))
```

---

### 2. MAP section (lines 365–387)

**Line 368** — in-window worst MAP:
```r
# Before
group_by(enc_id) %>%
summarize(map = min(map, na.rm = TRUE), .groups = 'drop')
# After
group_by(enc_id, t_start) %>%
summarize(map = min(map, na.rm = TRUE), .groups = 'drop')
```

**Lines 374–378** — pre-window lookback MAP:
```r
# Before
group_by(enc_id) %>%
dplyr::slice_max(vital_time, n = 1, with_ties = FALSE) %>%
ungroup() %>%
select(enc_id, map) %>%
rename(map_lb = map)
# After
group_by(enc_id, t_start) %>%
dplyr::slice_max(vital_time, n = 1, with_ties = FALSE) %>%
ungroup() %>%
select(enc_id, t_start, map) %>%
rename(map_lb = map)
```

**Lines 380–387** — final MAP assembly:
```r
# Before
df_map <- tw %>%
     select(enc_id) %>%
     left_join(df_map_window,   by = 'enc_id') %>%
     left_join(df_map_lookback, by = 'enc_id')
# ...
df_map <- df_map %>%
     mutate(map = dplyr::coalesce(map, map_lb)) %>%
     select(enc_id, map)
# After
df_map <- tw %>%
     select(enc_id, t_start) %>%
     left_join(df_map_window,   by = c('enc_id', 't_start')) %>%
     left_join(df_map_lookback, by = c('enc_id', 't_start'))
# ...
df_map <- df_map %>%
     mutate(map = dplyr::coalesce(map, map_lb)) %>%
     select(enc_id, t_start, map)
```

---

### 3. Pressors section (lines 418–443)

**Line 435** — group by episode:
```r
# Before
group_by(enc_id, med) %>%
summarize(max_dose = max(dose, na.rm = TRUE), .groups = 'drop')
# After
group_by(enc_id, t_start, med) %>%
summarize(max_dose = max(dose, na.rm = TRUE), .groups = 'drop')
```

**Line 438** — pivot to wide:
```r
# Before
tidyr::pivot_wider(id_cols = 'enc_id', names_from = 'med', values_from = 'max_dose')
# After
tidyr::pivot_wider(id_cols = c('enc_id', 't_start'), names_from = 'med', values_from = 'max_dose')
```

**Lines 441–443** — ensure all pressor columns exist (no change needed, just note that
`df_pressors` will now have `t_start` as a column and downstream joins should use
`c('enc_id', 't_start')`).

---

### 4. Respiratory support section (lines 454–463)

**Line 458** — group by episode:
```r
# Before
group_by(enc_id) %>%
# After
group_by(enc_id, t_start) %>%
```

**Line 463** — return columns:
```r
# Before
select(enc_id, resp_support)
# After
select(enc_id, t_start, resp_support)
```

---

### 5. FiO2/SpO2 and P/F + S/F ratio sections (lines 297–347, 470–479)

The `df_fio2_spo2` filtering already joins `tw` and filters `recorded_time >= t_start`.
For non-overlapping vent episodes, each observation falls into at most one window, so
`recorded_time` acts as an implicit episode key. No changes are needed in the ratio
*calculation* steps.

However, the final ratio summarization (lines 470–479) still groups by `enc_id` only:
```r
# Before
group_by(enc_id) %>%
summarize(
     pf_ratio = min(pf_ratio, na.rm = TRUE),
     sf_ratio = min(sf_ratio, na.rm = TRUE),
     .groups = 'drop'
)
```

To associate each ratio with its window, keep `t_start` in `df_fio2_spo2` (remove it from
the `select(-t_start, -t_end)` at line 305 — only drop `t_end`), carry it through
`df_fio2_look`, `df_spo2_look`, and `df_pao2_look`, and update the ratio summaries:

```r
# After — line 305
select(-t_end)   # keep t_start

# After — lines 472–477
group_by(enc_id, t_start) %>%
summarize(
     pf_ratio = min(pf_ratio, na.rm = TRUE),
     sf_ratio = min(sf_ratio, na.rm = TRUE),
     .groups = 'drop'
)
```

This requires also carrying `t_start` through `df_fio2_look`, `df_spo2_look`, `df_pao2_look`,
and updating `group_by(enc_id, recorded_time)` in `df_pf_ratio` / `df_sf_ratio` to
`group_by(enc_id, t_start, recorded_time)`.

---

### 6. Age (`df_agem`) — lines 164–173

Age is computed from `t_start`, so for multiple episodes it should technically vary
(negligibly in practice). For correctness, keep the per-episode row and join by
`c('enc_id', 't_start')` in the final assembly. No changes to the computation itself are
needed since `ref_times <- tw %>% select(enc_id, t_start)` already preserves `t_start`.
The `select(enc_id, agem)` at line 173 should become `select(enc_id, t_start, agem)`.

---

### 7. Final assembly (lines 481–492)

```r
# Before
df_psofa_wide <- tw %>%
     select(enc_id) %>%
     left_join(df_creatinine,   by = 'enc_id') %>%
     left_join(df_platelets,    by = 'enc_id') %>%
     left_join(df_tbili,        by = 'enc_id') %>%
     left_join(df_ratios,       by = 'enc_id') %>%
     left_join(df_map,          by = 'enc_id') %>%
     left_join(df_pressors,     by = 'enc_id') %>%
     left_join(df_resp_support, by = 'enc_id') %>%
     left_join(df_agem,         by = 'enc_id') %>%
     relocate(enc_id, agem)

# After
df_psofa_wide <- tw %>%
     select(enc_id, t_start) %>%
     left_join(df_creatinine,   by = c('enc_id', 't_start')) %>%
     left_join(df_platelets,    by = c('enc_id', 't_start')) %>%
     left_join(df_tbili,        by = c('enc_id', 't_start')) %>%
     left_join(df_ratios,       by = c('enc_id', 't_start')) %>%
     left_join(df_map,          by = c('enc_id', 't_start')) %>%
     left_join(df_pressors,     by = c('enc_id', 't_start')) %>%
     left_join(df_resp_support, by = c('enc_id', 't_start')) %>%
     left_join(df_agem,         by = c('enc_id', 't_start')) %>%
     relocate(enc_id, t_start, agem)
```

### 8. Update `@return` docstring (line 53–57)

Add `t_start` to the documented output columns.

---

## Verification

1. Single-episode patient: output should be identical to current behaviour (one row per enc_id,
   `t_start` added as new column).
2. Multi-episode patient: output should have one row per episode, each with the correct
   worst-value labs/vitals/meds for that window only.
3. Existing tests in `tests/testthat/` should still pass; add a test with a two-episode
   patient to confirm per-episode scoring.

## Files to modify

- `R/assemble_psofa_data.R` only
