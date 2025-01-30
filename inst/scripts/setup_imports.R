usethis::use_import_from('janitor', 'clean_names')
usethis::use_import_from('dplyr', c('mutate', 'across', 'select', 'rename',
                                    'if_else', 'group_by', 'lag', 'where',
                                    'case_when', 'arrange','filter', 'relocate',
                                    'left_join', 'inner_join', 'bind_rows', 'summarize',
                                    'distinct', 'starts_with', 'ends_with', 'if_any',
                                    'join_by', 'lead', 'lag', 'n', 'last', 'tibble', 'between',
                                    'anti_join'))
usethis::use_import_from('readr', c('cols', 'read_delim', 'col_character','col_date','col_datetime','col_double','col_factor','col_guess','col_integer','col_logical','col_number','col_skip','col_time'))
usethis::use_import_from('magrittr', '%>%')
usethis::use_import_from('stringr', c('str_to_lower', 'str_detect', 'str_match', 'str_remove_all', 'str_trim', 'str_replace', 'str_replace_all', 'str_extract', 'str_squish'))
usethis::use_import_from('lubridate', c('%within%', 'interval', 'duration', 'as_date', 'NA_POSIXct_', 'hours', 'as.duration'))
usethis::use_import_from('tidyr', c('pivot_wider', 'pivot_longer', 'separate_wider_delim', 'fill'))
