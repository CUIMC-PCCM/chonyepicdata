% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rass_intervals.R
\name{get_rass_intervals}
\alias{get_rass_intervals}
\title{get_rass_intervals}
\usage{
get_rass_intervals(id, rass, rass_time, max_inter_ep_duration = 4)
}
\arguments{
\item{id}{A character vector of IDs for each unique patient or encounter. Generally taken from
the variable PAT_ENC_CSN_ID, but can also create custom IDs. An example would be to use
PAT_ENC_CSN_ID but also append an identifier (01, 02, 03, etc.) for each PICU hospitalization
or episode of invasive mechanical ventilation.}

\item{rass}{A vector of integers ranging from -5 to +4, representing the RASS for each patient.}

\item{rass_time}{A vector of datetime or POSIXct entries corresponding to each RASS value}

\item{max_inter_ep_duration}{A number, the maximum number of hours that a particular RASS will be
extended in the event of a null recording. RASS may be sparsely recorded. For any recorded RASS,
the current RASS will be carried forward for \code{max_ep_duration} hours. After this amount
of time, the interval will be ended. Set this variable to 0 and no intervals will be created. Set
it to NA and the interval will be extended forward indefinitely until a new RASS is recorded.}
}
\value{
A data frame of IDs, RASS, and start/stop times of the intervals, and a duration for each.
}
\description{
Takes data fram from \link{load_rass} and creates intervals for each patient encounter.
An interval will last at most max_inter_ep_duration if nothing else is charted.
}
