% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_capd.R
\name{load_capd}
\alias{load_capd}
\title{load_capd}
\usage{
load_capd(
  capd_filepath,
  col_mapping = list(enc_id = "pat_enc_csn_id", mrn = "mrn", component_name =
    "display_name", component_value = "measure_value", capd_time = "recorded_time"),
  capd_coltypes = NULL,
  max_load = Inf
)
}
\arguments{
\item{capd_filepath}{Path to the CAP-D data}

\item{col_mapping}{Defines mapping of columns to key variables that will be
output. You can dynamically specify these mappings when calling the
function. The default is: PAT_ENC_CSN_ID -> enc_id MRN -> mrn DISPLAY_NAME
-> component_name MEASURE_VALUE -> component_value RECORDED_TIME ->
capd_time The names on the left hand side of the list argument must be kept
consistent. The names on the right-hand side are the names that are actually
present in the .txt file which will be mapped to these variables.}

\item{capd_coltypes}{A list of cols() specifications specifying how columns
should be read. Cols specifications are things like col_integer(),
col_character(), and can be found within the \code{\link[readr]{cols}}
documentation from the \code{readr} package. By default it is recommended to
just send in 'ccccccc' where the length of the string of characters is the
number of columns in the data. If this isn't working well you can send in
col_guess() for each one.}

\item{max_load}{The maximum number of rows to load. The default is \code{Inf}}
}
\value{
#' @return A data frame with:
\itemize{
\item \code{mrn}: Medical record number
\item \code{enc_id}: Encounter ID, renamed from PAT_ENC_CSN_ID
\item \code{capd_time}: Datetime when the CAP-D was recorded
\item \code{capd}: Integer value of the summed components of all CAP-D. A score
of 9 or more means the patient screened positive for delirium.
Any time where a component of the CAP-D was recorded as NA will not be returned.
Scores need to be complete.
\item \code{capd_not_indicated} TRUE if the nurse explicitly recorded that a CAPD was
not indicated at this time. Currently this does not indicate whether the CAPD was
not necessary because the patient was clearly lucid for age and developmental stage,
or because the patient was comatose. The \code{capd} variable will be NA if
\code{capd_not_indicated} is TRUE. Use \code{filter(data_frame, !capd_not_indicated)}
to remove all of these rows if they are not necessary.
}
}
\description{
Load Cornell Assessment of Pediatric Delirium (CAP-D) data from Epic
flowsheets. The datafile contains other types of flowsheet data, which are
filtered out. Columns must have the names defined in the dataset from November
2023 (for now).
}
