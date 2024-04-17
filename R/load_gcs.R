#' load_gcs
#'
#' Loads Glasgow Coma Scores for all patients from a raw, pipe-delimited file.
#'
#' @param gcs_filepath Path to the file.
#' @param gcs_coltypes A list of cols() specifications.
#'   Cols specifications are things like col_integer(), col_character(), and can be found
#'   within the \code{\link[readr]{cols}} documentation from the \code{readr} package.
#'   If this isn't working well you can send in col_guess().
#' @param max_load Maximum number of rows
#'
#' @return A timestamped data frame with GCS subtypes and overall score. If data are NA, they are counted as zero.
#' They are also kept so that we can check. GCS verbal is allowed to be NA when the patient is intubated, but not
#' otherwise. There is no way to check this in this file, so we will need to cross-reference with output from
#' \link{classify_resp_support}.
#'
#' @export
#'

load_gcs <- function(gcs_filepath,
                     gcs_coltypes = list(
                          col_character(),   # PAT_ENC_CSN_ID
                          col_character(),   # MRN
                          col_skip(),        # FLOWSHEET_GROUP
                          col_character(),   # COMMON_NAME
                          col_skip(),        # FLOWSHEET_NAME
                          col_integer(),     # CUST_LIST_MAP_VALUE
                          col_skip(),        # MEAS_VALUE
                          col_skip(),        # UNITS
                          col_datetime()     # RECORDED_TIME
                     ),
                     max_load = Inf)
{
     # Load in all GCS values
     suppressWarnings({
          df_gcs <- read_delim(gcs_filepath,
                                col_types = gcs_coltypes,
                                n_max = max_load,
                                delim = '|') %>%
               clean_names() %>%
               mutate(across(where(is.character), str_to_lower)) %>%
               rename(enc_id = pat_enc_csn_id, gcs_time = recorded_time)

     })

     # Combine different types of motor, verbal, eye scores
     # We will calculate the overall scores ourselves in case there is ever a problem on the Epic side
     df_gcs <- df_gcs %>%
          mutate(gcs_subc = case_when(common_name == 'gcs verbal' ~ 'gcs_verbal',
                                      common_name == 'gcs 6m to 26 audio/visual' ~ 'gcs_verbal',
                                      common_name == 'gcs eye' ~ 'gcs_eye',
                                      common_name == 'gcs 6m to 2y eye' ~ 'gcs_eye',
                                      common_name == 'gcs motor' ~ 'gcs_motor',
                                      common_name == 'gcs 6m to 2y motor' ~ 'gcs_motor',
                                      # common_name == 'gcs total' ~ 'gcs',
                                      # common_name == 'gcs 6m to 2y total ' ~ 'gcs'
                                      ),
                 gcs_value = as.numeric(cust_list_map_value)) %>%
          filter(!is.na(gcs_subc)) %>%
          rename() %>%
          select(mrn, enc_id, gcs_time, gcs_subc, gcs_value) %>%
          distinct()

     # There may be some cases where multiple values are recorded at the same time. Keep the worst value.
     df_gcs <- df_gcs %>%
          group_by(mrn, enc_id, gcs_time, gcs_subc) %>%
          arrange(mrn, enc_id, gcs_time, gcs_subc, gcs_value) %>%
          slice_head(n=1) %>%
          ungroup()

     # Now make into a wide dataset. Calculate the overall score
     df_gcs <- df_gcs %>%
          pivot_wider(id_cols = c('mrn', 'enc_id', 'gcs_time'),
                      names_from = gcs_subc,
                      values_from = gcs_value,
                      values_fill = NA) %>%
          filter(!(is.na(gcs_verbal) & is.na(gcs_verbal) & is.na(gcs_eye)))

     # Calculate the overall score
     df_gcs$gcs <- rowSums(select(df_gcs, gcs_eye:gcs_verbal), na.rm = TRUE)

     return(df_gcs)

}
