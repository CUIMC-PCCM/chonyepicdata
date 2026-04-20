#' assemble_psofa_data
#'
#' Assemble a wide-format data frame containing the worst physiologic values
#' within a specified time window, suitable for pSOFA scoring via
#' \link{calc_psofa}.
#'
#' @param labs Path to a pipe-delimited labs file, or a pre-loaded data frame
#'   in the format returned by \link{load_labs}.
#' @param vitals Path to a pipe-delimited vitals file, or a pre-loaded data
#'   frame in the format returned by \link{load_vitals}.
#' @param meds Path to a pipe-delimited MAR file, or a pre-loaded data frame
#'   in the format returned by \link{load_meds}.
#' @param fio2_spo2 Path to a pipe-delimited flowsheet file containing FiO2
#'   and SpO2 rows, or a pre-loaded data frame with columns \code{enc_id},
#'   \code{recorded_time}, \code{fio2}, and \code{spo2}.
#' @param resp_episodes A pre-computed data frame of respiratory support
#'   episodes, as returned by \link{classify_resp_support}.
#' @param time_window A data frame with column \code{enc_id} and either:
#'   \itemize{
#'     \item \code{t_start} and \code{t_end} (POSIXct) for absolute mode, or
#'     \item \code{ref_time} (POSIXct) for relative mode, with offsets
#'       supplied via \code{t_min} and \code{t_max}.
#'   }
#' @param t_min Numeric. Hours after \code{ref_time} defining the start of the
#'   scoring window (relative mode only). Default \code{0}.
#' @param t_max Numeric. Hours after \code{ref_time} defining the end of the
#'   scoring window (relative mode only). Default \code{24}.
#' @param agem Optional data frame with columns \code{enc_id} and \code{agem}
#'   (age in months). Takes priority over \code{dob} if both are supplied.
#' @param dob Optional data frame with columns \code{enc_id} and \code{dob}
#'   (date of birth as Date or character in YYYY-MM-DD format). Age is computed
#'   from \code{dob} and the window start time. Ignored if \code{agem} is
#'   supplied.
#' @param vitals_col_map Optional named character vector passed to
#'   \link{load_vitals} to remap source column names. Only used when
#'   \code{vitals} is a file path.
#' @param vitals_name_map Optional named character vector passed to
#'   \link{clean_vitals} to remap flowsheet display names to canonical vital
#'   sign names.
#' @param fio2_spo2_key_col Name of the encounter ID column in the FiO2/SpO2
#'   flowsheet file. Default \code{'PAT_ENC_CSN_ID'}.
#' @param fio2_spo2_time_col Name of the timestamp column. Default
#'   \code{'RECORDED_TIME'}.
#' @param fio2_spo2_var_col Name of the variable name column. Default
#'   \code{'DISPLAY_NAME'}.
#' @param fio2_spo2_measure_col Name of the measurement column. Default
#'   \code{'MEASURE_VALUE'}.
#' @param fio2_varname Display name used for FiO2 rows in the flowsheet.
#'   Default \code{'FiO2 (\%)'}.
#' @param spo2_varname Display name used for SpO2 rows in the flowsheet.
#'   Default \code{'SpO2'}.
#'
#' @return A wide-format data frame with one row per encounter and columns:
#'   \code{enc_id}, \code{agem}, \code{platelets}, \code{tbili},
#'   \code{creatinine}, \code{pf_ratio}, \code{sf_ratio}, \code{map},
#'   \code{epi}, \code{norepi}, \code{dopa}, \code{dobut},
#'   \code{resp_support}.
#'
#' @export
assemble_psofa_data <- function(labs,
                                vitals,
                                meds,
                                fio2_spo2,
                                resp_episodes,
                                time_window,
                                t_min = 0,
                                t_max = 24,
                                agem = NULL,
                                dob  = NULL,
                                vitals_col_map  = NULL,
                                vitals_name_map = NULL,
                                fio2_spo2_key_col     = 'PAT_ENC_CSN_ID',
                                fio2_spo2_time_col    = 'RECORDED_TIME',
                                fio2_spo2_var_col     = 'DISPLAY_NAME',
                                fio2_spo2_measure_col = 'MEASURE_VALUE',
                                fio2_varname          = 'FiO2 (%)',
                                spo2_varname          = 'SpO2') {

     # *****************************************************************************
     # Variable initiation ---------------------------------------------------------
     # *****************************************************************************

     enc_id <- t_start <- t_end <- ref_time <- recorded_time <- specimen_taken_time <-
          common_name <- result_value <- fio2 <- spo2 <- fio2_time <- earliest_time <-
          pao2 <- pf_ratio <- sf_ratio <- map_ni <- map_art <- map <- vital_time <-
          flowsheet_name <- med_name <- dose_unit <- frequency <- taken_time <- dose <-
          result <- med <- max_dose <- current_support <- support_time_start <-
          support_time_stop <- resp_support <- platelets <- tbili_total <- dbili <- tbili <-
          creatinine <- epi <- norepi <- dopa <- dobut <- dob_col <- agem_val <-
          x <- y <- overlaps <- map_lb <- val_lb <- in_window <- NULL

     # *****************************************************************************
     # Input validation ------------------------------------------------------------
     # *****************************************************************************

     if (is.null(agem) && is.null(dob)) {
          stop('Either agem or dob must be provided to calculate pSOFA.')
     }

     tw_cols <- names(time_window)
     absolute_mode <- all(c('t_start', 't_end') %in% tw_cols)
     relative_mode <- 'ref_time' %in% tw_cols

     if (!absolute_mode && !relative_mode) {
          stop('time_window must contain either (t_start, t_end) for absolute mode or ref_time for relative mode.')
     }

     # *****************************************************************************
     # Resolve time window to absolute t_start / t_end per encounter ---------------
     # *****************************************************************************

     if (relative_mode && !absolute_mode) {
          time_window <- time_window %>%
               mutate(t_start = ref_time + lubridate::hours(t_min),
                      t_end   = ref_time + lubridate::hours(t_max))
     }

     tw <- time_window %>% select(enc_id, t_start, t_end) %>% distinct()

     # *****************************************************************************
     # Load data if file paths provided --------------------------------------------
     # *****************************************************************************

     if (is.character(labs))      labs      <- load_labs(labs)
     if (is.character(meds))      meds      <- load_meds(meds)

     if (is.character(labs$specimen_taken_time))
          labs <- labs %>% mutate(specimen_taken_time = lubridate::ymd_hms(specimen_taken_time, quiet = TRUE))

     if (is.character(vitals)) {
          load_args <- list(vitals_filepath = vitals)
          if (!is.null(vitals_col_map)) load_args$col_map <- vitals_col_map
          vitals <- do.call(load_vitals, load_args)
     }
     # Accept either raw long-format (has flowsheet_name) or pre-cleaned wide-format
     if ('flowsheet_name' %in% names(vitals)) {
          vitals_raw_names <- unique(vitals$flowsheet_name)
          vitals <- clean_vitals(vitals, name_map = vitals_name_map)
     } else {
          vitals_raw_names <- NULL
     }

     if (is.character(fio2_spo2)) {
          fio2_spo2 <- load_generic_flowsheet_rows(
               fio2_spo2,
               key_name    = fio2_spo2_key_col,
               time_col    = fio2_spo2_time_col,
               var_col     = fio2_spo2_var_col,
               measure_col = fio2_spo2_measure_col,
               varnames    = c(fio2_varname, spo2_varname),
               rename_vars = c('fio2', 'spo2')
          ) %>%
               rename(enc_id        = !!str_to_lower(fio2_spo2_key_col),
                      recorded_time = !!str_to_lower(fio2_spo2_time_col)) %>%
               mutate(recorded_time = lubridate::as_datetime(recorded_time),
                      fio2          = as.numeric(fio2),
                      spo2          = as.numeric(spo2))
     }

     # *****************************************************************************
     # Compute age in months -------------------------------------------------------
     # *****************************************************************************

     if (!is.null(agem)) {
          df_agem <- agem %>% select(enc_id, agem)
     } else {
          ref_times <- tw %>% select(enc_id, t_start)
          df_agem <- dob %>%
               select(enc_id, dob) %>%
               left_join(ref_times, by = 'enc_id') %>%
               mutate(dob  = lubridate::ymd(dob),
                      agem = lubridate::interval(dob, as.Date(t_start)) %/% months(1)) %>%
               select(enc_id, agem)
     }

     # *****************************************************************************
     # Labs: extract pSOFA-relevant values -----------------------------------------
     # *****************************************************************************

     # Helper: worst value in window, falling back to most recent pre-window value.
     # If no value exists at all, returns NA (calc_psofa treats NA as normal/0).
     lab_with_fallback <- function(df, var, worst_fn) {
          if (!var %in% names(df)) {
               out <- tw %>% select(enc_id)
               out[[var]] <- NA_real_
               return(out)
          }
          in_win <- df %>%
               inner_join(tw, by = 'enc_id') %>%
               filter(specimen_taken_time >= t_start, specimen_taken_time <= t_end,
                      !is.na(.data[[var]])) %>%
               group_by(enc_id) %>%
               summarize(across(dplyr::all_of(var), ~ worst_fn(.x, na.rm = TRUE)),
                         .groups = 'drop')
          lookback <- df %>%
               inner_join(tw, by = 'enc_id') %>%
               filter(specimen_taken_time < t_start, !is.na(.data[[var]])) %>%
               group_by(enc_id) %>%
               dplyr::slice_max(specimen_taken_time, n = 1, with_ties = FALSE) %>%
               ungroup() %>%
               select(enc_id, dplyr::all_of(setNames(var, 'val_lb')))
          out <- tw %>%
               select(enc_id) %>%
               left_join(in_win, by = 'enc_id') %>%
               left_join(lookback, by = 'enc_id')
          if (!var      %in% names(out)) out[[var]]      <- NA_real_
          if (!'val_lb' %in% names(out)) out[['val_lb']] <- NA_real_
          out %>%
               mutate(!!rlang::sym(var) := dplyr::coalesce(.data[[var]], val_lb)) %>%
               select(enc_id, dplyr::all_of(var))
     }

     psofa_labnames  <- c('platelet count, auto', 'po2 (arterial)',
                          'bilirubin, total', 'bilirubin, direct', 'creatinine')
     psofa_labrenames <- c('platelets', 'pao2', 'tbili_total', 'dbili', 'creatinine')

     labs_filtered <- labs %>%
          filter(enc_id %in% tw$enc_id) %>%
          mutate(common_name = if_else(
               stringr::str_detect(common_name, 'po2') &
                    stringr::str_detect(common_name, 'arterial') &
                    !stringr::str_detect(common_name, 'cord'),
               'po2 (arterial)',
               common_name
          ))
     n_lab_overlap <- sum(tw$enc_id %in% unique(labs$enc_id))
     if (n_lab_overlap == 0) {
          lab_ids <- unique(labs$enc_id)
          tw_ids  <- tw$enc_id
          message('WARNING: 0 enc_id matches between labs and time_window.')
          message('  Lab  enc_id: n=', length(lab_ids),
                  '  nchar=', paste(sort(unique(nchar(head(lab_ids)))), collapse='/'),
                  '  looks_numeric=', all(grepl('^[0-9.e+E-]+$', head(lab_ids))))
          message('  TW   enc_id: n=', length(tw_ids),
                  '  nchar=', paste(sort(unique(nchar(head(tw_ids)))),  collapse='/'),
                  '  looks_numeric=', all(grepl('^[0-9.e+E-]+$', head(tw_ids))))
          # Check if trimming whitespace or dropping decimal resolves the mismatch
          lab_trimmed <- trimws(lab_ids)
          lab_intstr  <- sub('\\.0+$', '', lab_ids)
          tw_intstr   <- sub('\\.0+$', '', tw_ids)
          message('  After trimws:       ', sum(tw_ids  %in% lab_trimmed), ' matches')
          message('  After drop .0:      ', sum(tw_intstr %in% lab_intstr), ' matches')
     }
     available_idx  <- psofa_labnames %in% unique(labs_filtered$common_name)
     avail_names    <- psofa_labnames[available_idx]
     avail_renames  <- psofa_labrenames[available_idx]
     message('pSOFA labs matched: ', paste(avail_renames, collapse = ', '),
             if (any(!available_idx)) paste0(' | NOT matched: ', paste(psofa_labnames[!available_idx], collapse = ', ')))
     if (any(!available_idx)) {
          all_names <- unique(labs_filtered$common_name)
          for (kw in c('creatinin', 'platelet', 'bili', 'po2', 'oxygen')) {
               hits <- all_names[grepl(kw, all_names, ignore.case = TRUE)]
               if (length(hits)) message('  Candidates [', kw, ']: ', paste(hits, collapse = ' | '))
          }
     }

     df_labs_psofa <- labs_filtered %>%
          get_labs_by_type(labnames = avail_names, labvarnames = avail_renames) %>%
          mutate(across(any_of(avail_renames), ~ stringr::str_remove_all(.x, '[^0-9.]')),
                 across(any_of(avail_renames), ~ dplyr::na_if(.x, '')),
                 across(any_of(avail_renames), as.numeric))

     # Ensure both bili columns exist before combining
     for (col in c('tbili_total', 'dbili')) {
          if (!col %in% names(df_labs_psofa)) df_labs_psofa[[col]] <- NA_real_
     }

     # Use worst (highest) of total and direct bilirubin
     df_labs_psofa <- df_labs_psofa %>%
          mutate(tbili = pmax(tbili_total, dbili, na.rm = TRUE),
                 tbili = dplyr::na_if(tbili, -Inf)) %>%
          select(-tbili_total, -dbili)

     # Intermittent labs: worst in window with pre-window lookback fallback
     df_creatinine <- lab_with_fallback(df_labs_psofa, 'creatinine', max)
     df_platelets  <- lab_with_fallback(df_labs_psofa, 'platelets',  min)
     df_tbili      <- lab_with_fallback(df_labs_psofa, 'tbili',      max)

     # PaO2: window-filtered time-series only (needs contemporaneous FiO2 for P/F)
     if ('pao2' %in% names(df_labs_psofa)) {
          df_pao2_ts <- df_labs_psofa %>%
               select(enc_id, specimen_taken_time, dplyr::all_of('pao2')) %>%
               filter(!is.na(.data[['pao2']])) %>%
               inner_join(tw, by = 'enc_id') %>%
               filter(specimen_taken_time >= t_start & specimen_taken_time <= t_end) %>%
               select(enc_id, specimen_taken_time, dplyr::all_of('pao2'))
     } else {
          df_pao2_ts <- tibble::tibble(enc_id = character(),
                                       specimen_taken_time = lubridate::POSIXct(),
                                       pao2 = numeric())
     }

     # *****************************************************************************
     # FiO2 / SpO2: clean and filter to time window --------------------------------
     # *****************************************************************************

     df_fio2_spo2 <- fio2_spo2 %>%
          filter(enc_id %in% tw$enc_id) %>%
          mutate(across(c(fio2, spo2), as.numeric)) %>%
          filter(fio2 >= 21 & fio2 <= 100 | is.na(fio2)) %>%
          filter(spo2 > 0   & spo2 <= 100 | is.na(spo2)) %>%
          filter(!(is.na(fio2) & is.na(spo2))) %>%
          inner_join(tw, by = 'enc_id') %>%
          filter(recorded_time >= t_start & recorded_time <= t_end) %>%
          select(-t_start, -t_end)

     # *****************************************************************************
     # P/F and S/F ratios (4-hour lookback) ----------------------------------------
     # *****************************************************************************

     df_fio2_look  <- df_fio2_spo2 %>% select(enc_id, fio2_time = recorded_time, fio2) %>% filter(!is.na(fio2))
     df_spo2_look  <- df_fio2_spo2 %>% select(enc_id, recorded_time, spo2)   %>% filter(!is.na(spo2))  %>% mutate(earliest_time = recorded_time - lubridate::hours(4))
     df_pao2_look  <- df_pao2_ts   %>% select(enc_id, recorded_time = specimen_taken_time, pao2) %>% filter(!is.na(pao2)) %>% mutate(earliest_time = recorded_time - lubridate::hours(4))

     lookback_by <- join_by(enc_id, between(x$fio2_time, y$earliest_time, y$recorded_time))

     df_pf_ratio <- inner_join(df_fio2_look, df_pao2_look, by = lookback_by) %>%
          group_by(enc_id, recorded_time) %>%
          mutate(pf_ratio = round(pao2 / fio2 * 100)) %>%
          arrange(enc_id, recorded_time, desc(fio2_time)) %>%
          slice_head(n = 1) %>%
          ungroup() %>%
          select(enc_id, recorded_time, pf_ratio)

     df_sf_ratio <- inner_join(df_fio2_look, df_spo2_look, by = lookback_by) %>%
          group_by(enc_id, recorded_time) %>%
          mutate(sf_ratio = round(spo2 / fio2 * 100)) %>%
          arrange(enc_id, recorded_time, desc(fio2_time)) %>%
          slice_head(n = 1) %>%
          ungroup() %>%
          select(enc_id, recorded_time, sf_ratio)

     # For SpO2 observations with no documented FiO2 in the 4-hour lookback window,
     # carry forward the last recorded FiO2 before that SpO2 (LOCF, no time limit),
     # or default to 21% (room air) if no FiO2 was ever recorded for that encounter.
     locf_by <- join_by(enc_id, recorded_time >= fio2_time)
     df_sf_fallback <- df_spo2_look %>%
          anti_join(df_sf_ratio, by = c('enc_id', 'recorded_time')) %>%
          left_join(df_fio2_look, by = locf_by) %>%
          group_by(enc_id, recorded_time) %>%
          arrange(enc_id, recorded_time, desc(fio2_time)) %>%
          slice_head(n = 1) %>%
          ungroup() %>%
          mutate(fio2     = dplyr::coalesce(fio2, 21),
                 sf_ratio = round(spo2 / fio2 * 100)) %>%
          select(enc_id, recorded_time, sf_ratio)
     df_sf_ratio <- bind_rows(df_sf_ratio, df_sf_fallback)

     # *****************************************************************************
     # MAP: worst in window with pre-window lookback --------------------------------
     # *****************************************************************************

     df_vitals_tw <- vitals %>% filter(enc_id %in% tw$enc_id)
     if (!'map_ni'  %in% names(df_vitals_tw)) df_vitals_tw[['map_ni']]  <- NA_real_
     if (!'map_art' %in% names(df_vitals_tw)) df_vitals_tw[['map_art']] <- NA_real_

     df_map_all <- df_vitals_tw %>%
          mutate(map_ni  = if_else(map_ni  %in% 15:180, map_ni,  NA_real_),
                 map_art = if_else(map_art %in% 15:180, map_art, NA_real_)) %>%
          filter(!(is.na(map_art) & is.na(map_ni))) %>%
          mutate(map = dplyr::coalesce(map_art, map_ni)) %>%
          select(enc_id, vital_time, map) %>%
          distinct()

     df_map_window <- df_map_all %>%
          inner_join(tw, by = 'enc_id') %>%
          filter(vital_time >= t_start & vital_time <= t_end & !is.na(map)) %>%
          group_by(enc_id) %>%
          summarize(map = min(map, na.rm = TRUE), .groups = 'drop')

     df_map_lookback <- df_map_all %>%
          inner_join(tw, by = 'enc_id') %>%
          filter(vital_time < t_start & !is.na(map)) %>%
          group_by(enc_id) %>%
          dplyr::slice_max(vital_time, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          select(enc_id, map) %>%
          rename(map_lb = map)

     df_map <- tw %>%
          select(enc_id) %>%
          left_join(df_map_window, by = 'enc_id') %>%
          left_join(df_map_lookback, by = 'enc_id')
     if (!'map_lb' %in% names(df_map)) df_map[['map_lb']] <- NA_real_
     df_map <- df_map %>%
          mutate(map = dplyr::coalesce(map, map_lb)) %>%
          select(enc_id, map)

     if (all(is.na(df_map$map))) {
          n_match <- n_distinct(df_vitals_tw$enc_id)
          message(sprintf("  \u26a0  MAP: 100%% missing. %d/%d tw encounters matched vitals.", n_match, nrow(tw)))
          if (!is.null(vitals_raw_names)) {
               bp_cands <- vitals_raw_names[str_detect(vitals_raw_names, 'map|blood|pressure|bp|arterial')]
               message("    Vitals BP/MAP candidates: ", paste(bp_cands, collapse = ' | '))
          } else {
               message("    clean_vitals was not called \u2014 vitals columns: ",
                       paste(names(vitals), collapse = ', '))
          }
     }

     # *****************************************************************************
     # Pressors: max dose within time window ---------------------------------------
     # *****************************************************************************

     mar_given <- c('anesthesia volume adjustment', 'bolus from bag (dual sign required)',
                    'bolus from bag', 'continue to inpatient floor', 'continued from or',
                    'continued from pre', 'given by other', 'given during downtime', 'given',
                    'handoff (dual sign required)', 'handoff', 'new bag',
                    'new bag/syringe/cartridge', 'override pull', 'rate change',
                    'rate verify', 'rate/dose change', 'rate/dose changed',
                    'rate/dose verify', 'bolus', 'restarted (dual sign required)',
                    'restarted', 'started during downtime', 'started',
                    'unheld by provider', 'verification')

     mar_stopped <- c('held', 'held by provider', 'mar hold',
                      'stopped (dual sign required)', 'stopped', 'stop infusion')

     df_pressors <- meds %>%
          filter(stringr::str_detect(med_name, 'epinephrine|norepinephrine|dopamine|dobutamine')) %>%
          filter(dose_unit %in% c('mcg/kg/min', 'milliunits/kg/min', 'units/hr') | frequency == 'continuous') %>%
          filter(!stringr::str_detect(med_name, 'topical|cream|ointment|ophthalm|nasal|inhaled')) %>%
          inner_join(tw, by = 'enc_id') %>%
          filter(taken_time >= t_start & taken_time <= t_end) %>%
          select(-t_start, -t_end) %>%
          mutate(dose = if_else(result %in% mar_stopped, 0, dose),
                 dose = tidyr::replace_na(dose, 0)) %>%
          filter(result %in% c(mar_given, mar_stopped)) %>%
          mutate(med = dplyr::case_when(
               stringr::str_detect(med_name, 'norepinephrine') ~ 'norepi',
               stringr::str_detect(med_name, 'epinephrine')    ~ 'epi',
               stringr::str_detect(med_name, 'dopamine')       ~ 'dopa',
               stringr::str_detect(med_name, 'dobutamine')     ~ 'dobut',
               TRUE                                            ~ 'other'
          )) %>%
          group_by(enc_id, med) %>%
          summarize(max_dose = max(dose, na.rm = TRUE), .groups = 'drop') %>%
          filter(!is.na(max_dose), max_dose > 0) %>%
          tidyr::pivot_wider(id_cols = 'enc_id', names_from = 'med', values_from = 'max_dose')

     # Ensure all pressor columns exist
     for (col in c('epi', 'norepi', 'dopa', 'dobut')) {
          if (!col %in% names(df_pressors)) df_pressors[[col]] <- NA_real_
     }

     # *****************************************************************************
     # Respiratory support: highest level within time window -----------------------
     # *****************************************************************************

     resp_levels <- c('room_air', 'simple_o2', 'hfnc', 'cpap', 'bipap', 'imv', 'hfov')

     resp_join <- join_by(enc_id, overlaps(x$t_start, x$t_end,
                                           y$support_time_start, y$support_time_stop))

     df_resp_support <- tw %>%
          left_join(resp_episodes, by = resp_join) %>%
          mutate(current_support = tidyr::replace_na(current_support, 'room_air'),
                 current_support = factor(current_support, levels = resp_levels, ordered = TRUE)) %>%
          group_by(enc_id) %>%
          filter(current_support == max(current_support)) %>%
          slice_head(n = 1) %>%
          ungroup() %>%
          mutate(resp_support = as.numeric(current_support) > 2) %>%
          select(enc_id, resp_support)

     # *****************************************************************************
     # Assemble wide dataset -------------------------------------------------------
     # *****************************************************************************

     # Summarize P/F and S/F to worst within window
     suppressWarnings(
          df_ratios <- full_join(df_pf_ratio, df_sf_ratio, by = c('enc_id', 'recorded_time')) %>%
               group_by(enc_id) %>%
               summarize(
                    pf_ratio = min(pf_ratio, na.rm = TRUE),
                    sf_ratio = min(sf_ratio, na.rm = TRUE),
                    .groups  = 'drop'
               ) %>%
               mutate(across(where(is.numeric), ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)))
     )

     df_psofa_wide <- tw %>%
          select(enc_id) %>%
          left_join(df_creatinine,  by = 'enc_id') %>%
          left_join(df_platelets,   by = 'enc_id') %>%
          left_join(df_tbili,       by = 'enc_id') %>%
          left_join(df_ratios,      by = 'enc_id') %>%
          left_join(df_map,         by = 'enc_id') %>%
          left_join(df_pressors,    by = 'enc_id') %>%
          left_join(df_resp_support, by = 'enc_id') %>%
          left_join(df_agem,         by = 'enc_id') %>%
          relocate(enc_id, agem)

     return(df_psofa_wide)
}
