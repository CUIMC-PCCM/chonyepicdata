#' get_rds
#'
#' Send a directory path and load all .rds files into the global environment. To be used
#' with previously-saved data files which will load must faster than .txt files.
#'
#' You can call \code{list2env(data_list, envir = .GlobalEnv)} to load all files
#' all into the global environment. Names will be the file name with the dates and
#' file extension .rds removed.
#'
#' @param file_path Path to the directory with .rds files
#'
#' @return data_list: a list with all data saved from each file
#' @export
get_rds <- function(file_path = getwd()) {
     # Get all file paths
     all_files <- list.files(path = file_path, pattern = "\\.rds", full.names = TRUE)

     # Read all your data into a list
     data_list <- lapply(all_files, readRDS)

     # Now just get names
     all_file_names <- list.files(path = file_path, pattern = "\\.rds", full.names = FALSE)
     all_file_names <- sub("_\\d{4}-\\d{2}-\\d{2}.*", "", all_file_names)

     # Assign file names to list elements
     names(data_list) <- all_file_names

     return(data_list)
}
