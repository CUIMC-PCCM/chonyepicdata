#' deidentify_frame
#'
#' De-identify selected columns of a data frame based on their data type.
#'
#' \describe{
#'   \item{Character / factor}{Replaced with a 16-character hex surrogate
#'     derived from a salted SHA-256 hash. The mapping is deterministic within
#'     a single call: the same input value always produces the same surrogate,
#'     so relational keys (e.g. \code{mrn}, \code{enc_id}) remain joinable
#'     across tables de-identified with the same \code{salt}.}
#'   \item{Date / POSIXct}{Shifted by a random integer number of days drawn
#'     uniformly from \code{date_shift_range}. When \code{date_key} is
#'     supplied, every row with the same key value receives the same shift,
#'     preserving within-patient temporal relationships (e.g. the gap between
#'     admission and a lab result stays intact).}
#'   \item{Numeric / integer}{A warning is issued and the column is left
#'     unchanged. Numeric PHI (age, zip code, etc.) requires deliberate
#'     handling — generalise to ranges or add to \code{drop_cols} explicitly.}
#'   \item{Other types}{A warning is issued and the column is left unchanged.}
#' }
#'
#' @param df A data frame.
#' @param cols Character vector of column names to de-identify. Auto-detection
#'   of the appropriate method is based on the column's class.
#' @param date_key Optional string: name of a column (typically \code{mrn} or
#'   another patient-level identifier) used to assign a consistent date-shift
#'   per patient. If \code{NULL} and date/datetime columns are present in
#'   \code{cols}, each row is shifted independently — this breaks within-patient
#'   temporal relationships and a message is emitted.
#' @param date_shift_range Integer vector of length 2: the minimum and maximum
#'   number of days by which date columns may be shifted. Default
#'   \code{c(-365L, 365L)}.
#' @param salt Character string prepended to each value before hashing, making
#'   the surrogates resistant to dictionary attacks. A 32-character random
#'   alphanumeric salt is generated if \code{NULL}. Supply a fixed salt
#'   (and \code{seed}) when you need the same surrogates across multiple calls
#'   (e.g. de-identifying a medications table and a labs table so they can
#'   still be joined on \code{enc_id}).
#' @param shift_map Optional named integer vector mapping each unique
#'   \code{date_key} value to a day-shift offset. If supplied, \code{date_key}
#'   must also be provided and \code{date_shift_range} / \code{seed} are
#'   ignored for date columns. Use \code{\link{build_shift_map}} to generate a
#'   reusable map and pass the same object to multiple \code{deidentify_frame()}
#'   calls so dates shift identically across tables.
#' @param drop_cols Character vector of column names to remove from the output
#'   entirely (e.g. free-text notes, name fields).
#' @param seed Optional integer passed to \code{\link[base]{set.seed}} before
#'   generating the random salt and date shifts. Use for reproducibility.
#'
#' @return The input data frame with \code{cols} de-identified in-place and
#'   \code{drop_cols} removed. Row count and order are unchanged.
#'
#' @examples
#' \dontrun{
#' df_labs_deid <- deidentify_frame(
#'   df_labs,
#'   cols      = c('mrn', 'enc_id', 'result_date'),
#'   date_key  = 'mrn',
#'   drop_cols = c('patient_name', 'dob'),
#'   seed      = 42
#' )
#'
#' # De-identify multiple tables so they can still be joined on mrn / enc_id,
#' # and dates shift identically across all tables.
#' my_salt  <- 'correct-horse-battery-staple'
#' my_shifts <- build_shift_map(all_mrns, seed = 42)  # one shared map
#'
#' df_labs_deid <- deidentify_frame(df_labs,
#'   cols = c('mrn', 'enc_id', 'result_date'),
#'   date_key = 'mrn', salt = my_salt, shift_map = my_shifts)
#'
#' df_meds_deid <- deidentify_frame(df_meds,
#'   cols = c('mrn', 'enc_id', 'med_time'),
#'   date_key = 'mrn', salt = my_salt, shift_map = my_shifts)
#' }
#'
#' @export
deidentify_frame <- function(
     df,
     cols             = character(),
     date_key         = NULL,
     date_shift_range = c(-365L, 365L),
     salt             = NULL,
     shift_map        = NULL,
     drop_cols        = character(),
     seed             = NULL
) {

     if (!is.null(seed)) set.seed(seed)

     # ------------------------------------------------------------------
     # Input validation
     # ------------------------------------------------------------------
     if (!is.data.frame(df)) stop('`df` must be a data frame.')

     all_named_cols <- c(cols, drop_cols, date_key)
     missing_cols   <- setdiff(all_named_cols, names(df))
     if (length(missing_cols) > 0L) {
          stop('Column(s) not found in df: ',
               paste(missing_cols, collapse = ', '))
     }

     if (length(date_shift_range) != 2L || !is.numeric(date_shift_range)) {
          stop('`date_shift_range` must be a numeric vector of length 2.')
     }
     date_shift_range <- as.integer(date_shift_range)

     # ------------------------------------------------------------------
     # Generate salt
     # ------------------------------------------------------------------
     if (is.null(salt)) {
          salt <- paste(
               sample(c(letters, LETTERS, as.character(0:9)), 32L, replace = TRUE),
               collapse = ''
          )
     }

     # ------------------------------------------------------------------
     # Build (or validate) per-key date-shift lookup table.
     # Capture key values NOW — before the loop — because date_key may itself
     # be in cols and will be hashed before any date column is processed.
     # ------------------------------------------------------------------
     date_key_values <- NULL          # original key per row, used inside loop

     if (!is.null(date_key)) {
          date_key_values <- as.character(df[[date_key]])

          if (!is.null(shift_map)) {
               # User supplied a pre-built map — validate it covers all keys
               missing_keys <- setdiff(date_key_values, names(shift_map))
               if (length(missing_keys) > 0L) {
                    stop('`shift_map` is missing entries for key value(s): ',
                         paste(head(missing_keys, 5L), collapse = ', '),
                         if (length(missing_keys) > 5L)
                              paste0(' (+ ', length(missing_keys) - 5L, ' more)')
                         else '')
               }
          } else {
               # Generate a new map from all unique key values in this frame
               keys      <- unique(date_key_values)
               shifts    <- sample(
                    seq.int(date_shift_range[1L], date_shift_range[2L]),
                    length(keys),
                    replace = TRUE
               )
               shift_map <- stats::setNames(shifts, keys)
          }
     }

     # ------------------------------------------------------------------
     # Helper: hash a character vector with the shared salt
     # ------------------------------------------------------------------
     hash_col <- function(x) {
          vapply(as.character(x), function(v) {
               if (is.na(v)) return(NA_character_)
               substr(digest::digest(paste0(salt, v), algo = 'sha256'), 1L, 16L)
          }, character(1L), USE.NAMES = FALSE)
     }

     # ------------------------------------------------------------------
     # Process each column
     # ------------------------------------------------------------------
     for (col in cols) {
          v <- df[[col]]

          # ---- Dates and datetimes ----
          if (inherits(v, c('Date', 'POSIXct', 'POSIXlt', 'POSIXt'))) {

               if (!is.null(shift_map)) {
                    day_shifts <- unname(shift_map[date_key_values])
               } else {
                    message(
                         'deidentify_frame: no `date_key` supplied; column "', col,
                         '" will be shifted independently per row. ',
                         'Within-patient temporal relationships will NOT be preserved.'
                    )
                    day_shifts <- sample(
                         seq.int(date_shift_range[1L], date_shift_range[2L]),
                         nrow(df),
                         replace = TRUE
                    )
               }

               if (inherits(v, 'Date')) {
                    df[[col]] <- v + as.integer(day_shifts)
               } else {
                    # POSIXct / POSIXlt — shift in seconds
                    df[[col]] <- v + as.integer(day_shifts) * 86400L
               }

          # ---- Character and factor ----
          } else if (is.character(v) || is.factor(v)) {
               hashed <- hash_col(v)
               df[[col]] <- if (is.factor(v)) factor(hashed) else hashed

          # ---- Numeric / integer — warn, leave unchanged ----
          } else if (is.numeric(v) || is.integer(v)) {
               warning(
                    'deidentify_frame: column "', col, '" is numeric. ',
                    'Numeric PHI (e.g. age, zip code) should be generalised ',
                    'to ranges or added to `drop_cols`. Column left unchanged.',
                    call. = FALSE
               )

          # ---- Unrecognised type ----
          } else {
               warning(
                    'deidentify_frame: column "', col, '" has unrecognised type "',
                    paste(class(v), collapse = '/'), '" — skipped.',
                    call. = FALSE
               )
          }
     }

     # ------------------------------------------------------------------
     # Drop requested columns
     # ------------------------------------------------------------------
     if (length(drop_cols) > 0L) {
          df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
     }

     df
}

#' build_shift_map
#'
#' Generate a reusable named integer vector of per-patient date-shift offsets
#' for use with \code{\link{deidentify_frame}}.
#'
#' Pass the same \code{shift_map} to multiple \code{deidentify_frame()} calls
#' so that dates shift identically across all tables (labs, medications,
#' vitals, etc.), preserving join-ability while still de-identifying absolute
#' timestamps.
#'
#' @param keys Character vector of unique patient identifiers (e.g. all unique
#'   \code{mrn} values across your dataset).
#' @param date_shift_range Integer vector of length 2: the min and max number
#'   of days by which any date may be shifted. Default \code{c(-365L, 365L)}.
#' @param seed Optional integer for reproducibility.
#'
#' @return A named integer vector where each name is a key from \code{keys}
#'   and each value is the day offset assigned to that key.
#'
#' @examples
#' \dontrun{
#' all_mrns  <- unique(c(df_labs$mrn, df_meds$mrn, df_vitals$mrn))
#' my_shifts <- build_shift_map(all_mrns, seed = 42)
#'
#' df_labs_deid  <- deidentify_frame(df_labs,  cols = c('mrn', 'result_date'),
#'                                   date_key = 'mrn', shift_map = my_shifts)
#' df_meds_deid  <- deidentify_frame(df_meds,  cols = c('mrn', 'med_time'),
#'                                   date_key = 'mrn', shift_map = my_shifts)
#' }
#'
#' @export
build_shift_map <- function(
     keys,
     date_shift_range = c(-365L, 365L),
     seed             = NULL
) {
     if (!is.null(seed)) set.seed(seed)

     keys   <- as.character(unique(keys))
     shifts <- sample(
          seq.int(as.integer(date_shift_range[1L]), as.integer(date_shift_range[2L])),
          length(keys),
          replace = TRUE
     )
     stats::setNames(shifts, keys)
}
