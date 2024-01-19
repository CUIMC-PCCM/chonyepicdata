#' clean_meds
#'
#' Clean and process raw medication data loaded from Epic EHR. The majority of
#' this work is string matching and removal to extract meaningful medication
#' names, doses, and concentrations. Doses are not weight-based unless recorded
#' as such.
#'
#' @param df_meds Medication datafile. Must have columns with medication names,
#' doses, concentrations, and rates.
#' @param medlist An array of characters that will be matched to determine
#' medication names to keep. Partial matches can be used according to \code{\link[stringr]} rules.
#' Pass \code{NA} if you you don't want to filter at all.
#' @param mar_med_given An array of characters that specify whether a medication
#' was actually given to the patient. Default values are provided. Rows that
#' do not match this will be excluded.
#' @param mar_med_stopped An array of characters that specify if a medication was
#' definitively stopped. Default values are provided.
#' @param row_limit Maximum number of rows to allow. Useful for debugging very
#' large files.
#' @param time_limits Data table of encounter IDs and time intervals during
#' which medication doses should be included. The first column should be named
#' 'enc_id' and the second column should be formatted as time intervals using
#' \code{lubridate} interval()
#'
#'
#' @return A data frame with raw medication data. Medication name is saved in character format
#' in the column 'med_name'. These should be cleaned and processed.
#'
#' @seealso [clean_meds()]
#' @export
clean_meds <- function(df_meds,
                       medlist = # List of medications we will look at
                            med_str <- c(
                                 'midazolam',
                                 'lorazepam',
                                 'diazepam',
                                 'clonazepam',
                                 'alprazolam',
                                 'morphine',
                                 'fentanyl',
                                 'hydromorphone',
                                 'oxycodone',
                                 'clonidine',
                                 'ketamine',
                                 'pentobarbital',
                                 'quetiapine',
                                 'haloperidol',
                                 'risperidone',
                                 'olanzapine',
                                 'aripiprazole',
                                 'diphenhydramine',
                                 'hydroyxyzine',
                                 'rocuronium',
                                 'vecuronium',
                                 'cisatracurium'
                            ),
                       mar_med_given =   c('anesthesia volume adjustment',
                                           'bolus from bag (dual sign required)',
                                           'bolus from bag',
                                           'continue to inpatient floor',
                                           'continued from or',
                                           'continued from pre',
                                           'given by other',
                                           'given during downtime',
                                           'given',
                                           'handoff (dual sign required)',
                                           'handoff',
                                           'new bag',
                                           'new bag/syringe/cartridge',
                                           'override pull',
                                           'rate change',
                                           'rate verify',
                                           'rate/dose change',
                                           'rate/dose changed',
                                           'rate/dose verify',
                                           'rate/dose verify','bolus',
                                           'restarted (dual sign required)',
                                           'restarted',
                                           'started during downtime',
                                           'started',
                                           'verification'

                       ),

                       mar_med_stopped = c('held',
                                           'held by provider',
                                           'mar hold',
                                           'stopped (dual sign required)',
                                           'stopped',
                                           'stop infusion'),

                       row_limit = Inf,

                       time_limits = NA
)

{

     # *****************************************************************************
     # Additional definitions ------------------------------------------------------
     # *****************************************************************************

     # Create a character array to define meds given enterally
     enteral_med_string <- c('tablet',
                             'capsule',
                             'suspension',
                             'solution',
                             'oral',
                             'packet',
                             'soln',
                             'solid',
                             'liquid',
                             'reditabs',
                             'dissolvable',
                             'granules',
                             'tab',
                             'enteric',
                             'pack',
                             'coated',
                             'concentrate',
                             'delayed release',
                             'disintegrating',
                             'chewable',
                             'syrup',
                             'drops',
                             'elixer',
                             'lozenge'
     )
     enteral_med_string <- str_flatten(enteral_med_string, '|')

     # Create a character array of IV medications
     iv_med_string <- c(
          'injection',
          'infusion',
          'injectable',
          'inj',
          'bolus from bag',
          'bolus',
          'syringe',
          'iv syringe',
          'ivpb',
          'iv$',
          ' iv ',
          'premix',
          'nfusion',
          'dexmedetomidine'
     )
     iv_med_string <- str_flatten(iv_med_string, '|')

     # Flag oral versus IV medications using string matching
     meds_cleaned <- meds_cleaned %>%
          mutate(route = case_when(
               str_detect(med, iv_med_string) ~ 'iv',
               str_detect(med, enteral_med_string) ~ 'enteral',
               str_detect(med, 'patch') ~ 'patch',
               TRUE ~ 'other'
          ),
          route = factor(route))


     # *****************************************************************************
     # Initial cleanup -------------------------------------------------------------
     # *****************************************************************************

     # Small sample for testing purposes
     if(!is.infinite(row_limit)) {
          df_meds <- df_meds %>% dplyr::slice_head(n = row_limit)
     }

     # Rename a few things
     df_meds <- df_meds %>%
          rename(med = med_name,
                 mar_result = result,
                 units = dose_unit,
                 med_time = taken_time) %>%
          mutate(infusion_rate = if_else(infusion_rate != 'NULL', as.numeric(infusion_rate), NaN))

     # Clean up the data, remove any rows that indicate a med was ordered but not given
     df_meds <- df_meds %>%
          mutate(mar_result = factor(mar_result),
                 dose = as.numeric(dose)) %>%
          filter(mar_result %in% c(mar_med_given, mar_med_stopped)) %>%
          filter(!is.na(dose))

     # Remove any rows that do not match the list of medication names (if it
     # was provided)
     if(!is.na(medlist)) {
          med_str <- str_flatten(medlist, collapse = '|')
          df_meds <- df_meds %>%
               filter(str_detect(med, med_str))
     }

     # Remove any rows that are not within the user-specified time intervals
     # for a given encounter
     if(exists('time_limits')) {
          if(!is.na(time_limits)) {
               tryCatch({
                    intcolname <- sym(names(time_limits)[2])
                    intcolname <- sym(i)
                    df_meds <- df_meds %>%
                         inner_join(time_limits) %>%
                         filter(med_time %within% !!intcolname)
               },
               finally = print('Error in intervals defined by time_limits argument')
               )
          }
     }

     # *****************************************************************************
     # String processing -----------------------------------------------------------
     # *****************************************************************************

     # Get concentrations. Define this as something matching the format "mg/ml" or X mg / Y ml"), then
     # process into a numeric value (mg, units, mcg per mL). Some of the premixed bags
     # need to just be customized.
     df_meds <- df_meds %>%
          mutate(
               conc = str_match(med, '[:digit:]*\\.*[:digit:]+[:space:]*mg\\/[:digit:]*ml|[:digit:]*\\.*+[:space:]*mcg\\/[:digit:]*ml|[:digit:]*\\.*+[:space:]*units\\/[:digit:]*ml'),
               conc = if_else(str_detect(med, 'fentanyl') & str_detect(med, 'premix'),
                              '20 mcg/ml', conc),
               conc = str_remove_all(conc, '\\('),
               conc = str_remove_all(conc, '\\)'),
               conc_old = conc,
               conc_unit = str_remove_all(conc, '[:digit:]|\\.') %>% str_remove_all('\\/.+') %>% str_trim(),
               conc = str_remove_all(conc, '[:alpha:]| '),
               conc = str_replace(conc, '\\/$', '\\/1')) %>%
          tidyr::separate(col = conc, c('conc_num', 'conc_denom'), sep = '/') %>%
          mutate(conc_num = as.double(conc_num),
                 conc_denom = as.double(conc_denom),
                 conc = conc_num / conc_denom,
                 conc = if_else(conc_unit == 'mcg', conc / 1000, conc)) %>%
          select(-conc_num, -conc_denom)


     # Filter out fluids, fluid boluses, and a number of other types of medications
     # that we are not interested in
     meds_cleaned <- df_meds %>%
          mutate(med = str_to_lower(med),
                 med = str_remove_all(med, ','),
                 med = str_remove_all(med, '\\(pf\\)'),
                 med = str_remove_all(med, 'INV \\(GEN\\)'),
                 med = str_remove_all(med, ' \\(lip-prot-amyl\\)'),
                 med = str_remove_all(med, '\\(laxative\\) \\(child\\) '),
                 med = str_remove_all(med, '\\(urethral/mucosal\\) sterile '),
                 med = str_remove_all(med, '\\(adult\\)'),
                 med = str_remove_all(med, '\\(human\\)'),
                 med = str_remove_all(med, '\\(pediatric\\)'),
                 med = str_remove_all(med, '\\(cardiac\\)'),
                 med = str_remove_all(med, '\\(l-arginine\\)'),
                 med = str_remove_all(med, '\\(5 mg/ml\\)'),
                 med = str_remove_all(med, 'in nacl'),
                 #med = str_remove_all(med, 'in sodium chloride [:digit:]+ %'),
                 med = str_remove_all(med, 'in sodium chloride'),
                 med = str_remove_all(med, 'in dextrose'),
                 med = str_remove_all(med, 'sodium succinate'),
                 med = str_remove_all(med, 'sod suc'),
                 med = str_remove_all(med, ' pf '),
                 med = str_remove_all(med, 'concentrate'),
                 med = str_replace_all(med, 'morphine', 'morphine \\(morphine\\)'),
                 med = str_replace_all(med, '\\(human\\) \\(gammagard s/d\\)', '(\\gammagard\\)'),
                 med = str_replace_all(med, '\\(isophane\\) \\(humulin n\\)', '(\\isophane humulin n\\)'),
                 med = str_replace_all(med, '\\(isophane\\) \\(humulinnovolin\\)', '(\\isophane humulin novolin\\)'),
                 med = str_replace_all(med, 'epinephrine (epipen)', 'epipen (epipen)'),
                 med = str_remove_all(med, '[:space:]{2,}'),
                 med = str_remove_all(med, ' sodium')) %>%
          mutate(units = str_remove_all(units, 'of [:alpha:]+')) %>%
          filter(!str_detect(med, 'lipid emulsion')) %>%
          filter(!str_detect(med, 'dextrose (.+?) and sodium chloride (.+?)')) %>%
          filter(!str_detect(med, 'dextrose-nacl')) %>%
          filter(!str_detect(med, 'tpn')) %>%
          filter(!str_detect(med, 'sodium chloride (.+?) bolus')) %>%
          filter(!str_detect(med, 'sodium chloride')) %>%
          filter(!str_detect(med, 'sodium chloride (.+?) infusion')) %>%
          filter(!str_detect(med, 'lactated ringer\'s bolus'))  %>%
          filter(!str_detect(med, 'lactated ringer\'s infusion')) %>%
          filter(!str_detect(med, '^dextrose (.+?) bolus')) %>%
          filter(!str_detect(med, '^dextrose bolus')) %>%
          filter(!str_detect(med, '^dextrose \\([:digit:]+%\\)')) %>%
          filter(!str_detect(med, '^dextrose [:digit:]+%')) %>%
          filter(!str_detect(med, '^dextrose [:digit:]+ %')) %>%
          filter(!str_detect(med, '^dextrose 12.5 %')) %>%
          filter(!str_detect(med, 'dianeal')) %>%
          filter(!str_detect(med, 'plasma-lyte')) %>%
          filter(!str_detect(med, 'vaccine')) %>%
          filter(!str_detect(med, 'vfc')) %>%
          filter(!(str_detect(med, 'hydrocortisone') & str_detect(units, 'Application')))

     # Split the text "med" field into a generic and brand name.
     # Remove components of medication that are used for ionic salts
     # (i.e. for 'fentanyl citrate' remove the word citrate)
     meds_split <- meds_cleaned %>%
          mutate(med = str_replace_all(med, '([a-z])([0-9])', '\\1 \\2'),
                 med = str_remove_all(med, '\\(.+\\)'),
                 med = str_extract(med, '^.+?(?=\\s[:digit:])|^.+'),
                 med = str_remove_all(med, '\\)'),
                 med = str_remove_all(med, 'opioid]-induced pruritus'),
                 med = str_remove_all(med, 'tablet'),
                 med = str_remove_all(med, 'injection'),
                 med = str_remove_all(med, 'infusion'),
                 med = str_remove_all(med, 'injectable'),
                 med = str_remove_all(med, 'inj'),
                 med = str_remove_all(med, ' gel'),
                 med = str_remove_all(med, 'capsule'),
                 med = str_remove_all(med, 'solution'),
                 med = str_remove_all(med, 'suspension'),
                 med = str_remove_all(med, ' oral'),
                 med = str_remove_all(med, 'powder'),
                 med = str_remove_all(med, 'packet'),
                 med = str_remove_all(med, 'soln'),
                 med = str_remove_all(med, 'solid'),
                 med = str_remove_all(med, 'liquid'),
                 med = str_remove_all(med, 'powder'),
                 med = str_remove_all(med, 'packet'),
                 med = str_remove_all(med, 'anaphylaxis'),
                 med = str_remove_all(med, 'external'),
                 med = str_remove_all(med, 'reditabs'),
                 med = str_remove_all(med, 'dissolvable'),
                 med = str_remove_all(med, 'granules'),
                 med = str_remove_all(med, 'ointment'),
                 med = str_remove_all(med, 'rectal'),
                 med = str_remove_all(med, 'bolus from bag'),
                 med = str_remove_all(med, 'kit'),
                 med = str_remove_all(med, 'suppository'),
                 med = str_remove_all(med, ' tab'),
                 med = str_remove_all(med, ' chemo'),
                 med = str_remove_all(med, 'enteric'),
                 med = str_remove_all(med, ' pack'),
                 med = str_remove_all(med, 'bolus'),
                 med = str_remove_all(med, 'hfa'),
                 med = str_remove_all(med, 'coated'),
                 med = str_remove_all(med, 'concentrate'),
                 med = str_remove_all(med, 'delayed release'),
                 med = str_remove_all(med, 'syringe'),
                 med = str_remove_all(med, 'subcutaneous'),
                 med = str_remove_all(med, 'iv syringe'),
                 med = str_remove_all(med, 'ivpb'),
                 med = str_remove_all(med, 'nasal'),
                 med = str_remove_all(med, 'spray'),
                 med = str_remove_all(med, 'disintegrating'),
                 med = str_remove_all(med, 'ophthalmic'),
                 med = str_remove_all(med, 'chewable'),
                 med = str_remove_all(med, 'peripheral'),
                 med = str_remove_all(med, 'orderable'),
                 med = str_remove_all(med, 'central'),
                 med = str_remove_all(med, 'bromide'),
                 med = str_remove_all(med, 'hcl'),
                 med = str_remove_all(med, 'citrate'),
                 med = str_remove_all(med, 'pca'),
                 med = str_remove_all(med, '[:space:]for.+'),
                 med = str_remove_all(med, '[:space:]in[:space:].*'),
                 med = str_remove_all(med, '[:space:]in.*'),
                 med = str_remove_all(med, '[:space:]in$'),
                 med = str_remove_all(med, '[:space:]iv[:space:].*'),
                 med = str_remove_all(med, '[:space:]iv$'),
                 med = str_remove_all(med, '[:space:].+[:punct:][:space]*'),
                 med = str_remove_all(med, '(?<=^[:alnum:])'),
                 med = str_remove_all(med, 'generic'),
                 med = str_squish(med)
          ) %>%
          select(mrn, enc_id, med, dose, units, frequency, route, conc, conc_unit, infusion_rate,
                 med_time, mar_result)


     # Correct a few meds with confusing names
     meds_split <- meds_split %>% mutate(
          med = str_remove_all(med, ' injection'),    # careful with this! might mess up heparin locks
          med = str_remove_all(med, 'clinician bolus prn\\: '),
          med = str_remove_all(med, 'odt'),
          med = str_remove_all(med, 'solution'),
          med = str_replace_all(med, 'aspirin.+', 'aspirin'),
          med = str_replace_all(med, 'fentanyl.+', 'fentanyl'),
          med = str_replace_all(med, 'lidocaine-epinephrine', 'lidocaine with epi'),
          med = str_replace_all(med, 'methadone.+', 'methadone'),
          med = str_replace_all(med, 'methylprednisolone.+', 'methylprednisolone'),
          med = str_replace_all(med, 'midazolam.+', 'midazolam'),
          med = str_replace_all(med, 'prednisolone.+', 'prednisolone'),
          med = str_replace_all(med, 'racepinephrine', 'racemic epi'),
          med = str_replace_all(med, 'warfarin.+', 'warfarin'),
          med = str_replace_all(med, 'hydrocortisone.+', 'hydrocortisone'),
          med = str_remove_all(med, 'status epi'),
          med = str_trim(med)
     )

     # Remove meds dosed via patch as these are really hard to account for.
     # They are basically an infusion but hard to know how much is released
     # per hour
     med_split <- meds_split %>%
          filter(route != 'patch')

     # *****************************************************************************
     # Infusions vs bolus ----------------------------------------------------------
     # *****************************************************************************

     # Flag infusions (defined as having a per time unit) and boluses. We will need to
     # update later for weight-based dosing
     meds_split <- meds_split %>%
          mutate(is_infusion = if_else(str_detect(units, 'hr|hour|min|minute'), TRUE, FALSE),
                 is_bolus = !is_infusion,
                 frequency = if_else(is_infusion, NA, frequency),
                 infusion_rate = if_else(is_bolus, NA, infusion_rate)) %>%
          mutate(dose_conc = conc*infusion_rate) %>%
          relocate(dose_conc, .after = dose)

     # Most meds are weight based. Some are ordered adult-style, in units/hr.
     # For adult doses, we will need to back-calculate to weight based later.
     # For now, just flag meds with weight-based dosing
     meds_split <- meds_split %>%
          mutate(dose = signif(dose, 2),
                 wt_based = if_else(str_detect(units, 'kg'), TRUE, FALSE))

     ## *****************************************************************************
     ## Infusion start/stop times ---------------------------------------------------
     ## *****************************************************************************

     # First work with infusion medications
     meds_infusions <- meds_split %>%
          filter(is_infusion)

     # Process infusions to flag starts, stops, changes.
     # Change stopped doses to zero
     # Number each MAR administration
     meds_infusions2 <- meds_infusions %>%
          arrange(mrn, enc_id, med, med_time) %>%
          group_by(mrn, enc_id, med) %>%
          mutate(mar_num = dense_rank(med_time)) %>%
          ungroup() %>%
          group_by(mrn, enc_id, med) %>%
          mutate(
               # Med is by definition started at the first admin
               med_start = if_else(mar_num == 1, TRUE, FALSE),

               # Meds are only given if they have certain MAR comments
               med_given = if_else(mar_result %in% mar_med_given, TRUE, FALSE),

               # Meds are stopped if they fit certain MAR comments
               med_stop = if_else(mar_result %in% mar_med_stopped, TRUE, FALSE),

               # Difference between time of this and next row
               time_diff_next = round(as.numeric(difftime(lead(med_time), med_time, units = 'hours')), 1),

               # Make sure the last row has a non-NA time
               time_diff_next = if_else(row_number() == n(), 0, time_diff_next),

               # Difference between time of this and the last row
               time_diff_last = round(as.numeric(difftime(med_time, lag(med_time), units = 'hours')), 1),

               # Make sure the first row has a non-NA time
               time_diff_last = if_else(row_number() == 1, 0, time_diff_last),

               # Meds are stopped if nothing was charted for >24 hours
               # med_stop = if_else( (mrn == lead(mrn)) & (time_diff_last > 24), TRUE, med_stop),

               # Meds are always stopped at the last data entry per patient
               # (even if it was still charted as being given)
               # med_stop = if_else( mrn != lead(mrn), TRUE, med_stop),

               # Correct the last row so it is non-NA
               med_stop = if_else( row_number() == n(), TRUE, med_stop),

               # Meds are changed if the next dose is different from this dose (for the same patient)
               med_change = if_else((dose != lead(dose)), TRUE, FALSE),

               # Correct the last row so it is non-NA
               med_change = if_else(row_number() == n(), FALSE, med_change),

               # Dose is zero if the med was stopped
               dose = if_else(med_stop, 0, dose),

               # Dose is zero if there is an invalid MAR comment
               dose = if_else(!med_given, 0, dose)
          ) %>%
          ungroup() %>%

          # Account for sparse data by flagging rows for removal.
          # There are many rows that contain nursing administration
          # comments but no actual dosing change. Only need to keep
          # rows with changes.
          mutate(
               # Flag row for removal if a med was given, and not stopped, not changed, and not started
               remove_row = if_else(med_given & !(med_stop | med_change | med_start),
                                    TRUE, FALSE),

               # Simpler version -- flag for removal if the prior row was for
               # the same patient, and had the exact same med/dose, or the med was
               # not given (such as a bolus from the pump, which isn't an infusion)
               remove_row_simple = if_else(mrn == lag(mrn) & med == lag(med) & dose == lag(dose),
                                           TRUE, FALSE),

               # Correct the first row
               remove_row_simple = if_else(row_number() == 1, FALSE, remove_row_simple)
          )

     #' 1. Remove all of the flagged rows, and recalculate the difference in
     #'    time for each row
     #'        Since we set the last row for each patient to "stop" and a dose
     #'        of zero, this means that for some patients we will lose the
     #'        last row of med dosing. (Patients total doses will be lower than
     #'        reality if the nurse didn't ever chart stopping the med.") This
     #'        is ok and shouldn't be different among different patient groups.
     #' 2. Remove all of the rows with a dose of zero (these are just markers
     #'    for when meds were stopped).
     #' 3. Calculate a cumulative dose for each interval, which is just the
     #'    dose x the amount of time
     meds_infusions3 <- meds_infusions2 %>%
          filter(!remove_row_simple) %>%
          group_by(mrn, enc_id, med) %>%
          mutate(
               time_diff = round(as.numeric(difftime(lead(med_time), med_time, units = 'hours')), 1),
               time_diff = if_else(row_number() == n(), 0, time_diff)
          ) %>%
          filter(dose > 0) %>%
          mutate(
               interv_dose = dose * time_diff
          ) %>%
          ungroup()

     ## *****************************************************************************
     ## Bolus dose processing -------------------------------------------------------
     ## *****************************************************************************

     # Now process bolus doses.
     meds_bolus <- filter(is_bolus)

     # Only keep doses that were given
     meds_bolus_sedatives <- meds_bolus_sedatives %>%
          filter(mar_result %in% mar_med_given) %>%
          mutate(interv_dose = dose)


     return(df_meds)

}
