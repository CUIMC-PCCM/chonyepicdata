#' ignore_unused_imports
#'
#' Null function to eliminate notes on build regarding unused imports
#'
ignore_unused_imports <- function() {
     glue::glue
     lifecycle::deprecated
     readr::readr_example
     forcats::as_factor
     dplyr::mutate
     janitor::clean_names
     lubridate::date
     rlang::run_on_load
     stringr::str_c
     tidyr::as_tibble
     withr::with_dir
     cli::cli
     magrittr::add

}
