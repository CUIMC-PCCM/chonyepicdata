# Refactor: clean_resp_support() to support configurable column/value mappings

## Status
Steps 1–3 are **complete** and committed to branch `claude/hardcore-antonelli-01dda1`.
Remaining: verification (see below).

## Context
`clean_resp_support()` hardcodes `flowsheet_measure_id` lookups (new Epic format,
post-7/18/2024). Adding `var_col` and `var_map` parameters makes the function flexible
enough to handle any Epic format without duplicating logic. The `_old()` variant becomes
unnecessary and will be deprecated in-place, with its mapping extracted for reuse.

---

## Design

```r
clean_resp_support <- function(df_resp,
                               var_col = 'flowsheet_measure_id',
                               var_map = NULL)
```

- **`var_col`**: column in `df_resp` that identifies each measurement (default: current behavior)
- **`var_map`**: named character vector mapping values in `var_col` → standardized output names.
  When `NULL` (default), the function uses the existing 31-entry mapping internally.

---

## Steps

### Step 1: Refactor `R/clean_resp_support.R` ✅
### Step 2: Extract old-format map to `inst/scripts/resp_support_map_old.R` ✅
### Step 3: Deprecate `R/clean_resp_support_old.R` ✅

---

## Verification
1. `devtools::load_all()` — no errors
2. `clean_resp_support(df_vent_new)` with no extra args produces same output as before
3. `source('inst/scripts/resp_support_map_old.R')` then
   `clean_resp_support(df_vent_old, var_col = 'flowsheet_measure_name', var_map = resp_support_map_old)` works
4. `clean_resp_support_old(df_vent_old)` emits deprecation message
