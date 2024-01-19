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
#' If argument is provided, no filtering will be performed at this level.
#' @param mar_med_given An array of characters that specify whether a medication
#' was actually given to the patient. Default values are provided. Rows that
#' do not match this will be excluded.
#' @param mar_med_stopped An array of characters that specify if a medication was
#' definitively stopped. Default values are provided.
#' @param row_limit Maximum number of rows to allow. Useful for debugging very
#' large files.
#'
#' @return A data frame with raw medication data. Medication name is saved in character format
#' in the column 'med_name'. These should be cleaned and processed.
#'
#' @seealso [clean_meds()]
#' @export
clean_meds <- function(df_meds,
                       medlist = NULL,
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

                       row_limit = Inf
)

{

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
     if(!is.null(medlist)) {
          df_meds <- df_meds %>%
               filter(str_detect(med, medlist))
     }

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

     return(df_meds)

}
