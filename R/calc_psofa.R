#' calc_psofa
#'
#' Calculate a pediatric Sequential Organ Failure Assessment (pSOFA) score from
#' a wide-format data frame. Scores each organ system component and returns the
#' total score.
#'
#' @param df_psofa A wide-format data frame, typically produced by
#'   \link{assemble_psofa_data}. Required columns: \code{agem} (age in months),
#'   \code{platelets}, \code{tbili}, \code{creatinine}, \code{map},
#'   \code{resp_support} (logical), and at least one of \code{pf_ratio} or
#'   \code{sf_ratio}. Pressor columns (\code{epi}, \code{norepi}, \code{dopa},
#'   \code{dobut}) default to \code{NA} if absent. \code{gcs} defaults to
#'   \code{15} if absent.
#'
#' @return The input data frame with added component score columns
#'   (\code{agecat}, \code{pfscore}, \code{sfscore}, \code{pltscore},
#'   \code{biliscore}, \code{mapscore}, \code{gcsscore}, \code{crscore},
#'   \code{pf_or_sf_score}) and the total \code{psofa} score.
#'
#' @references Matics TJ, Sanchez-Pinto LN. Adaptation and Validation of a
#'   Pediatric Sequential Organ Failure Assessment Score and Evaluation of the
#'   Sepsis-3 Definitions in Critically Ill Children. JAMA Pediatr.
#'   2017;171(10):e172352.
#'
#' @export
calc_psofa <- function(df_psofa) {

     agem <- agecat <- pf_ratio <- sf_ratio <- platelets <- tbili <- creatinine <-
          map <- epi <- norepi <- dopa <- dobut <- gcs <- resp_support <-
          pfscore <- sfscore <- pltscore <- biliscore <- mapscore <- gcsscore <-
          crscore <- pf_or_sf_score <- psofa <- NULL

     if (!'gcs' %in% names(df_psofa)) {
          df_psofa <- df_psofa %>% mutate(gcs = 15L)
     }

     for (col in c('epi', 'norepi', 'dopa', 'dobut')) {
          if (!col %in% names(df_psofa)) df_psofa[[col]] <- NA_real_
     }

     df_psofa <- df_psofa %>%
          mutate(
               pf_ratio   = floor(pf_ratio),
               sf_ratio   = floor(sf_ratio),
               platelets  = floor(platelets),
               map        = floor(map),
               creatinine = round(creatinine, 1)
          )

     df_psofa <- df_psofa %>%
          mutate(
               agecat = case_when(
                    agem <   1 ~ 1L,
                    agem <  12 ~ 2L,
                    agem <  24 ~ 3L,
                    agem <  60 ~ 4L,
                    agem < 144 ~ 5L,
                    agem < 216 ~ 6L,
                    TRUE       ~ 7L
               ),

               pfscore = case_when(
                    is.na(pf_ratio)                          ~ NA_integer_,
                    pf_ratio %in% 300:399                    ~ 1L,
                    pf_ratio %in% 200:299                    ~ 2L,
                    pf_ratio %in% 100:199 & resp_support     ~ 3L,
                    pf_ratio < 100        & resp_support     ~ 4L,
                    TRUE                                     ~ 0L
               ),

               sfscore = case_when(
                    is.na(sf_ratio)                          ~ NA_integer_,
                    sf_ratio %in% 264:291                    ~ 1L,
                    sf_ratio %in% 221:263                    ~ 2L,
                    sf_ratio %in% 148:220 & resp_support     ~ 3L,
                    sf_ratio < 148        & resp_support     ~ 4L,
                    TRUE                                     ~ 0L
               ),

               pltscore = case_when(
                    platelets %in% 100:149 ~ 1L,
                    platelets %in%  50:99  ~ 2L,
                    platelets %in%  20:49  ~ 3L,
                    platelets < 20         ~ 4L,
                    TRUE                   ~ 0L
               ),

               biliscore = case_when(
                    tbili >= 1.2 & tbili <  2 ~ 1L,
                    tbili >= 2   & tbili <  6 ~ 2L,
                    tbili >= 6   & tbili < 12 ~ 3L,
                    tbili >= 12               ~ 4L,
                    TRUE                      ~ 0L
               ),

               mapscore = case_when(
                    dopa > 15 | epi > 0.1 | norepi > 0.1                                              ~ 4L,
                    (dopa > 5) | (epi > 0 & epi <= 0.1) | (norepi > 0 & norepi <= 0.1)               ~ 3L,
                    (dopa > 0 & dopa <= 5) | !is.na(dobut)                                            ~ 2L,
                    agecat == 1L & map < 46 ~ 1L,
                    agecat == 2L & map < 55 ~ 1L,
                    agecat == 3L & map < 60 ~ 1L,
                    agecat == 4L & map < 62 ~ 1L,
                    agecat == 5L & map < 65 ~ 1L,
                    agecat == 6L & map < 67 ~ 1L,
                    agecat == 7L & map < 70 ~ 1L,
                    TRUE                    ~ 0L
               ),

               gcsscore = case_when(
                    gcs < 6              ~ 4L,
                    between(gcs,  6,  9) ~ 3L,
                    between(gcs, 10, 12) ~ 2L,
                    between(gcs, 13, 14) ~ 1L,
                    TRUE                 ~ 0L
               ),

               crscore = case_when(
                    agecat == 1L & creatinine >= 1.6                       ~ 4L,
                    agecat == 1L & between(creatinine, 1.2, 1.5)           ~ 3L,
                    agecat == 1L & between(creatinine, 1.0, 1.1)           ~ 2L,
                    agecat == 1L & between(creatinine, 0.8, 0.9)           ~ 1L,
                    agecat == 2L & creatinine >= 1.2                       ~ 4L,
                    agecat == 2L & between(creatinine, 0.8, 1.1)           ~ 3L,
                    agecat == 2L & between(creatinine, 0.5, 0.7)           ~ 2L,
                    agecat == 2L & between(creatinine, 0.3, 0.4)           ~ 1L,
                    agecat == 3L & creatinine >= 1.5                       ~ 4L,
                    agecat == 3L & between(creatinine, 1.1, 1.4)           ~ 3L,
                    agecat == 3L & between(creatinine, 0.6, 1.0)           ~ 2L,
                    agecat == 3L & between(creatinine, 0.4, 0.5)           ~ 1L,
                    agecat == 4L & creatinine >= 2.3                       ~ 4L,
                    agecat == 4L & between(creatinine, 1.6, 2.2)           ~ 3L,
                    agecat == 4L & between(creatinine, 0.9, 1.5)           ~ 2L,
                    agecat == 4L & between(creatinine, 0.6, 0.8)           ~ 1L,
                    agecat == 5L & creatinine >= 2.6                       ~ 4L,
                    agecat == 5L & between(creatinine, 1.8, 2.5)           ~ 3L,
                    agecat == 5L & between(creatinine, 1.1, 1.7)           ~ 2L,
                    agecat == 5L & between(creatinine, 0.7, 1.0)           ~ 1L,
                    agecat == 6L & creatinine >= 4.2                       ~ 4L,
                    agecat == 6L & between(creatinine, 2.9, 4.1)           ~ 3L,
                    agecat == 6L & between(creatinine, 1.7, 2.8)           ~ 2L,
                    agecat == 6L & between(creatinine, 1.0, 1.6)           ~ 1L,
                    agecat == 7L & creatinine >= 5.0                       ~ 4L,
                    agecat == 7L & between(creatinine, 3.5, 4.9)           ~ 3L,
                    agecat == 7L & between(creatinine, 2.0, 3.4)           ~ 2L,
                    agecat == 7L & between(creatinine, 1.2, 1.9)           ~ 1L,
                    TRUE                                                   ~ 0L
               ),

               pf_or_sf_score = dplyr::coalesce(pfscore, sfscore, 0L),
               psofa = pf_or_sf_score + pltscore + biliscore + mapscore + gcsscore + crscore
          )

     return(df_psofa)
}
