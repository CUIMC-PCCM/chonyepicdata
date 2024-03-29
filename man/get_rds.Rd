% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rds.R
\name{get_rds}
\alias{get_rds}
\title{get_rds}
\usage{
get_rds(file_path = getwd())
}
\arguments{
\item{file_path}{Path to the directory with .rds files}
}
\value{
data_list: a list with all data saved from each file
}
\description{
Send a directory path and load all .rds files into the global environment. To be used
with previously-saved data files which will load must faster than .txt files.
}
\details{
You can call \code{list2env(data_list, envir = .GlobalEnv)} to load all files
all into the global environment. Names will be the file name with the dates and
file extension .rds removed.
}
