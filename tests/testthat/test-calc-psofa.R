library(dplyr)

# Helper: one row with all values normal for a school-age child
normal_row <- function(...) {
     tibble(
          enc_id       = "E001",
          agem         = 72,       # 6 years → agecat 5
          platelets    = 200,
          tbili        = 0.5,
          creatinine   = 0.6,
          pf_ratio     = 450,
          sf_ratio     = 350,
          map          = 70,
          resp_support = FALSE,
          epi          = NA_real_,
          norepi       = NA_real_,
          dopa         = NA_real_,
          dobut        = NA_real_
     ) %>% mutate(...)
}

# ── Column defaults ────────────────────────────────────────────────────────────

test_that("gcs defaults to 15 when column absent", {
     result <- calc_psofa(normal_row())
     expect_true("gcs" %in% names(result))
     expect_equal(result$gcs, 15L)
     expect_equal(result$gcsscore, 0L)
})

test_that("pressor columns default to NA when absent", {
     df <- normal_row() %>% select(-epi, -norepi, -dopa, -dobut)
     result <- calc_psofa(df)
     expect_equal(result$mapscore, 0L)
})

# ── Normal patient → all zeros ─────────────────────────────────────────────────

test_that("all-normal patient scores zero", {
     result <- calc_psofa(normal_row())
     expect_equal(result$psofa, 0L)
     expect_equal(result$pltscore,  0L)
     expect_equal(result$biliscore, 0L)
     expect_equal(result$mapscore,  0L)
     expect_equal(result$gcsscore,  0L)
     expect_equal(result$crscore,   0L)
})

# ── Platelet scoring ───────────────────────────────────────────────────────────

test_that("platelet score boundaries", {
     expect_equal(calc_psofa(normal_row(platelets = 150))$pltscore, 0L)
     expect_equal(calc_psofa(normal_row(platelets = 149))$pltscore, 1L)
     expect_equal(calc_psofa(normal_row(platelets = 100))$pltscore, 1L)
     expect_equal(calc_psofa(normal_row(platelets =  99))$pltscore, 2L)
     expect_equal(calc_psofa(normal_row(platelets =  50))$pltscore, 2L)
     expect_equal(calc_psofa(normal_row(platelets =  49))$pltscore, 3L)
     expect_equal(calc_psofa(normal_row(platelets =  20))$pltscore, 3L)
     expect_equal(calc_psofa(normal_row(platelets =  19))$pltscore, 4L)
})

# ── Bilirubin scoring ──────────────────────────────────────────────────────────

test_that("bilirubin score boundaries", {
     expect_equal(calc_psofa(normal_row(tbili = 1.19))$biliscore, 0L)
     expect_equal(calc_psofa(normal_row(tbili = 1.2))$biliscore,  1L)
     expect_equal(calc_psofa(normal_row(tbili = 1.9))$biliscore,  1L)
     expect_equal(calc_psofa(normal_row(tbili = 2.0))$biliscore,  2L)
     expect_equal(calc_psofa(normal_row(tbili = 5.9))$biliscore,  2L)
     expect_equal(calc_psofa(normal_row(tbili = 6.0))$biliscore,  3L)
     expect_equal(calc_psofa(normal_row(tbili = 11.9))$biliscore, 3L)
     expect_equal(calc_psofa(normal_row(tbili = 12.0))$biliscore, 4L)
})

# ── MAP / pressor scoring ──────────────────────────────────────────────────────

test_that("mapscore: low MAP without pressors uses age-specific threshold", {
     # agecat 5 (60-143 months): threshold MAP < 65 → score 1
     expect_equal(calc_psofa(normal_row(map = 64))$mapscore, 1L)
     expect_equal(calc_psofa(normal_row(map = 65))$mapscore, 0L)
})

test_that("mapscore: dobutamine gives score 2", {
     expect_equal(calc_psofa(normal_row(dobut = 5))$mapscore, 2L)
})

test_that("mapscore: low dopamine gives score 2", {
     expect_equal(calc_psofa(normal_row(dopa = 3))$mapscore, 2L)
     expect_equal(calc_psofa(normal_row(dopa = 5))$mapscore, 2L)
})

test_that("mapscore: dopamine > 5 gives score 3", {
     expect_equal(calc_psofa(normal_row(dopa = 5.1))$mapscore, 3L)
     expect_equal(calc_psofa(normal_row(dopa = 15))$mapscore,  3L)
})

test_that("mapscore: dopamine > 15 gives score 4", {
     expect_equal(calc_psofa(normal_row(dopa = 15.1))$mapscore, 4L)
})

test_that("mapscore bug fix: epi = 0 does NOT score as 3", {
     expect_equal(calc_psofa(normal_row(epi = 0))$mapscore, 0L)
})

test_that("mapscore: low epi gives score 3", {
     expect_equal(calc_psofa(normal_row(epi = 0.01))$mapscore, 3L)
     expect_equal(calc_psofa(normal_row(epi = 0.1))$mapscore,  3L)
})

test_that("mapscore: high epi gives score 4", {
     expect_equal(calc_psofa(normal_row(epi = 0.11))$mapscore, 4L)
})

test_that("mapscore: low norepi gives score 3, high gives score 4", {
     expect_equal(calc_psofa(normal_row(norepi = 0.05))$mapscore, 3L)
     expect_equal(calc_psofa(normal_row(norepi = 0.2))$mapscore,  4L)
})

# ── GCS scoring ───────────────────────────────────────────────────────────────

test_that("gcs score boundaries", {
     expect_equal(calc_psofa(normal_row(gcs = 15))$gcsscore, 0L)
     expect_equal(calc_psofa(normal_row(gcs = 14))$gcsscore, 1L)
     expect_equal(calc_psofa(normal_row(gcs = 13))$gcsscore, 1L)
     expect_equal(calc_psofa(normal_row(gcs = 12))$gcsscore, 2L)
     expect_equal(calc_psofa(normal_row(gcs = 10))$gcsscore, 2L)
     expect_equal(calc_psofa(normal_row(gcs =  9))$gcsscore, 3L)
     expect_equal(calc_psofa(normal_row(gcs =  6))$gcsscore, 3L)
     expect_equal(calc_psofa(normal_row(gcs =  5))$gcsscore, 4L)
})

# ── Respiratory scoring ────────────────────────────────────────────────────────

test_that("pfscore: low PF with resp support scores 3 or 4", {
     expect_equal(calc_psofa(normal_row(pf_ratio = 150, resp_support = TRUE))$pfscore,  3L)
     expect_equal(calc_psofa(normal_row(pf_ratio =  99, resp_support = TRUE))$pfscore,  4L)
     expect_equal(calc_psofa(normal_row(pf_ratio = 150, resp_support = FALSE))$pfscore, 0L)
})

test_that("sfscore used when pf_ratio is NA", {
     df <- normal_row(pf_ratio = NA_real_, sf_ratio = 150, resp_support = TRUE)
     result <- calc_psofa(df)
     expect_true(is.na(result$pfscore) | result$pfscore == 0L)
     expect_equal(result$sfscore, 3L)
     expect_equal(result$pf_or_sf_score, 3L)
})

test_that("pf_ratio takes priority over sf_ratio when both present", {
     df <- normal_row(pf_ratio = 150, sf_ratio = 150, resp_support = TRUE)
     result <- calc_psofa(df)
     expect_equal(result$pf_or_sf_score, result$pfscore)
})

# ── Age categories ────────────────────────────────────────────────────────────

test_that("age category boundaries are correct", {
     expect_equal(calc_psofa(normal_row(agem =   0))$agecat, 1L)  # < 1 month
     expect_equal(calc_psofa(normal_row(agem =   1))$agecat, 2L)  # 1–11 months
     expect_equal(calc_psofa(normal_row(agem =  11))$agecat, 2L)
     expect_equal(calc_psofa(normal_row(agem =  12))$agecat, 3L)  # 12–23 months
     expect_equal(calc_psofa(normal_row(agem =  23))$agecat, 3L)  # was gap in original
     expect_equal(calc_psofa(normal_row(agem =  24))$agecat, 4L)  # 24–59 months
     expect_equal(calc_psofa(normal_row(agem =  59))$agecat, 4L)
     expect_equal(calc_psofa(normal_row(agem =  60))$agecat, 5L)  # 60–143 months
     expect_equal(calc_psofa(normal_row(agem = 143))$agecat, 5L)
     expect_equal(calc_psofa(normal_row(agem = 144))$agecat, 6L)  # 144–215 months
     expect_equal(calc_psofa(normal_row(agem = 215))$agecat, 6L)
     expect_equal(calc_psofa(normal_row(agem = 216))$agecat, 7L)  # ≥ 216 months
})

# ── Creatinine scoring (spot-check two age categories) ────────────────────────

test_that("creatinine scoring for neonate (agecat 1)", {
     df_base <- normal_row(agem = 0)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 0.7))$crscore, 0L)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 0.8))$crscore, 1L)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 1.0))$crscore, 2L)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 1.2))$crscore, 3L)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 1.6))$crscore, 4L)
})

test_that("creatinine scoring for adolescent (agecat 7)", {
     df_base <- normal_row(agem = 216)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 1.0))$crscore, 0L)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 1.2))$crscore, 1L)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 2.0))$crscore, 2L)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 3.5))$crscore, 3L)
     expect_equal(calc_psofa(mutate(df_base, creatinine = 5.0))$crscore, 4L)
})

# ── Total score ───────────────────────────────────────────────────────────────

test_that("total psofa is sum of components", {
     result <- calc_psofa(normal_row(
          platelets = 30, tbili = 8, creatinine = 2.0,
          pf_ratio = 150, resp_support = TRUE, map = 60,
          epi = 0.05
     ))
     expect_equal(result$psofa,
                  result$pf_or_sf_score + result$pltscore + result$biliscore +
                       result$mapscore + result$gcsscore + result$crscore)
})
