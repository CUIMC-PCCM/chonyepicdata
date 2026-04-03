library(readr)
library(dplyr)

# ── Helper: prompt user with a question, return trimmed response ───────────────
ask <- function(prompt) trimws(readline(prompt))

# ── Helper: print column list ──────────────────────────────────────────────────
print_cols <- function(valid_cols) {
     cat(paste0("  ", seq_along(valid_cols), ". ", valid_cols, collapse = "\n"), "\n")
}

# ── Helper: prompt for a comma-separated list of numbers or names ──────────────
ask_cols <- function(prompt, valid_cols) {
     print_cols(valid_cols)
     repeat {
          response <- ask(prompt)
          if (response == "") return(character(0))
          tokens <- trimws(strsplit(response, ",")[[1]])
          # Convert numeric tokens to column names; leave names as-is
          selections <- sapply(tokens, function(x) {
               n <- suppressWarnings(as.integer(x))
               if (!is.na(n) && n >= 1 && n <= length(valid_cols)) valid_cols[n] else x
          }, USE.NAMES = FALSE)
          invalid <- setdiff(selections, valid_cols)
          if (length(invalid) == 0) return(selections)
          cat(sprintf("  Unknown column(s): %s\n  Please enter numbers from the list above.\n",
                      paste(invalid, collapse = ", ")))
          print_cols(valid_cols)
     }
}

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Select file
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n── Step 1: Select input file ─────────────────────────────────────────────\n")
cat("A file chooser dialog will open. Select your data file.\n")
input_file <- file.choose()
cat(sprintf("Selected: %s\n", input_file))

# ── Detect delimiter ──────────────────────────────────────────────────────────
fileext <- tools::file_ext(input_file)

cat("\nWhat delimiter does the file use?\n")
cat("  1 = pipe (|)   2 = comma (,)   3 = tab   4 = other\n")
delim_choice <- ask("Enter choice [default: auto-detect from extension]: ")

delim <- switch(delim_choice,
     "1" = "|",
     "2" = ",",
     "3" = "\t",
     "4" = ask("Enter delimiter character: "),
     if (fileext == "csv") "," else if (fileext == "tsv") "\t" else "|"
)
cat(sprintf("Using delimiter: %s\n", ifelse(delim == "\t", "tab", delim)))

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Load a preview to show columns
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n── Step 2: Loading file preview ──────────────────────────────────────────\n")
df_preview <- read_delim(input_file, delim = delim,
                         col_types = cols(.default = col_character()),
                         n_max = 5, show_col_types = FALSE)
all_cols <- names(df_preview)

cat(sprintf("Columns found (%d total).\n", length(all_cols)))

# ── Load full file ─────────────────────────────────────────────────────────────
cat("Loading full file...\n")
df <- read_delim(input_file, delim = delim,
                 col_types = cols(.default = col_character()),
                 show_col_types = FALSE)
cat(sprintf("Loaded %d rows, %d columns.\n", nrow(df), ncol(df)))

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Columns to de-identify (replace with sequential integers)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n── Step 3: ID columns to de-identify ────────────────────────────────────\n")
cat("Enter the column names to replace with sequential integers (comma-separated).\n")
cat("These are typically patient or encounter identifiers (e.g. MRN, PAT_ENC_CSN_ID).\n")
cat("Press Enter to skip.\n")
id_cols <- ask_cols("ID columns: ", all_cols)

if (length(id_cols) > 0) {
     cat(sprintf("Will de-identify: %s\n", paste(id_cols, collapse = ", ")))} else {
     cat("No ID columns selected.\n")
}

# ── Determine which ID column defines 'patient' for date shifting ──────────────
patient_col <- NULL
if (length(id_cols) > 0) {
     cat("\nWhich of these ID columns represents the patient (used to group timestamp shifts)?\n")
     cat("All encounters for the same patient will be shifted by the same amount.\n")
     print_cols(id_cols)
     repeat {
          response <- ask("Patient-level column [default: 1]: ")
          if (response == "") { patient_col <- id_cols[1]; break }
          n <- suppressWarnings(as.integer(response))
          if (!is.na(n) && n >= 1 && n <= length(id_cols)) { patient_col <- id_cols[n]; break }
          if (response %in% id_cols) { patient_col <- response; break }
          cat("  Please enter a number or name from the list above.\n")
          print_cols(id_cols)
     }
     cat(sprintf("Using '%s' to group date shifts.\n", patient_col))
}

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Timestamp columns
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n── Step 4: Timestamp columns ─────────────────────────────────────────────\n")
cat("Enter timestamp column names to shift (comma-separated).\n")
cat("All timestamps for the same patient shift by the same random offset,\n")
cat("so relative timing is preserved exactly.\n")
cat("Press Enter to skip.\n")
time_cols <- ask_cols("Timestamp columns: ", all_cols)

time_format <- "%Y-%m-%d %H:%M:%S"
if (length(time_cols) > 0) {
     cat(sprintf("Will shift: %s\n", paste(time_cols, collapse = ", ")))
     cat("\nSample values from selected timestamp column(s):\n")
     for (col in time_cols) {
          sample_vals <- head(unique(na.omit(df[[col]])), 6)
          cat(sprintf("  %s:\n", col))
          cat(paste0("    ", sample_vals, collapse = "\n"), "\n")
     }
     fmt_options <- c("%Y-%m-%d %H:%M:%OS", "%m/%d/%Y %H:%M", "%m/%d/%Y %I:%M %p")
     fmt_examples <- c("2024-06-14 08:30:00[.000]", "06/14/2024 08:30", "06/14/2024 08:30 AM")
     cat("\nWhat is the timestamp format?\n")
     for (i in seq_along(fmt_options)) {
          cat(sprintf("  %d. %-25s (e.g. %s)%s\n",
                      i, fmt_options[i], fmt_examples[i],
                      if (i == 1) "  [default]" else ""))
     }
     cat("  Or type a custom format string.\n")
     fmt_input <- ask("Format [press Enter for 1]: ")
     time_format <- if (fmt_input == "" || fmt_input == "1") {
          fmt_options[1]
     } else {
          n <- suppressWarnings(as.integer(fmt_input))
          if (!is.na(n) && n >= 1 && n <= length(fmt_options)) fmt_options[n] else fmt_input
     }
     cat(sprintf("Using format: %s\n", time_format))
} else {
     cat("No timestamp columns selected.\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 5. Columns to drop entirely
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n── Step 5: Columns to drop ───────────────────────────────────────────────\n")
cat("Enter any column names to remove entirely (comma-separated).\n")
cat("Press Enter to skip.\n")
drop_cols <- ask_cols("Columns to drop: ", all_cols)

if (length(drop_cols) > 0) {
     cat(sprintf("Will drop: %s\n", paste(drop_cols, collapse = ", ")))
} else {
     cat("No columns will be dropped.\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# 6. Optional subsampling
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n── Step 6: Subsampling ───────────────────────────────────────────────────\n")
if (!is.null(patient_col)) {
     n_patients <- n_distinct(df[[patient_col]])
     cat(sprintf("File contains %d unique patients.\n", n_patients))
     n_input <- ask("How many patients to keep? [press Enter to keep all]: ")
     max_patients <- if (n_input == "" || is.na(suppressWarnings(as.integer(n_input)))) {
          Inf
     } else {
          as.integer(n_input)
     }
} else {
     max_patients <- Inf
}

# ═══════════════════════════════════════════════════════════════════════════════
# 7. Output file
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n── Step 7: Output file ───────────────────────────────────────────────────\n")
default_output <- file.path(
     dirname(input_file),
     paste0(tools::file_path_sans_ext(basename(input_file)), "_deidentified.txt")
)
cat(sprintf("Default output location:\n  %s\n", default_output))
out_input <- ask("Output path [press Enter for default]: ")
output_file <- if (out_input == "") default_output else out_input

# ═══════════════════════════════════════════════════════════════════════════════
# 8. Apply de-identification
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n── Applying de-identification ────────────────────────────────────────────\n")

# Subsample
if (!is.null(patient_col) && is.finite(max_patients) && n_distinct(df[[patient_col]]) > max_patients) {
     set.seed(42)
     keep_ids <- sample(unique(df[[patient_col]]), max_patients)
     df <- filter(df, .data[[patient_col]] %in% keep_ids)
     cat(sprintf("Subsampled to %d patients (%d rows).\n", max_patients, nrow(df)))
}

# Shift timestamps BEFORE replacing IDs so the patient-level join key still matches
if (length(time_cols) > 0 && !is.null(patient_col)) {
     set.seed(99)
     date_shift_map <- tibble(pid = unique(df[[patient_col]])) %>%
          mutate(shift_days = sample(-1825:-365, n(), replace = TRUE))
     names(date_shift_map)[1] <- patient_col
     df <- left_join(df, date_shift_map, by = patient_col)
     for (col in time_cols) {
          df[[col]] <- format(
               as.POSIXct(df[[col]], format = time_format) +
                    as.difftime(df$shift_days, units = "days"),
               "%Y-%m-%d %H:%M:%S"
          )
          cat(sprintf("  Shifted timestamps in '%s'.\n", col))
     }
     df <- select(df, -shift_days)
} else if (length(time_cols) > 0) {
     # No patient column: shift entire dataset by one random offset
     set.seed(99)
     shift <- sample(-1825:-365, 1)
     for (col in time_cols) {
          df[[col]] <- format(
               as.POSIXct(df[[col]], format = time_format) +
                    as.difftime(shift, units = "days"),
               "%Y-%m-%d %H:%M:%S"
          )
          cat(sprintf("  Shifted timestamps in '%s' by %d days.\n", col, shift))
     }
}

# Replace ID columns with sequential integers
for (col in id_cols) {
     mapping <- tibble(orig = unique(df[[col]])) %>%
          mutate(new_val = as.character(row_number()))
     df[[col]] <- mapping$new_val[match(df[[col]], mapping$orig)]
     cat(sprintf("  Replaced '%s' with sequential integers.\n", col))
}

# Drop columns
if (length(drop_cols) > 0) {
     df <- select(df, -any_of(drop_cols))
     cat(sprintf("  Dropped columns: %s\n", paste(drop_cols, collapse = ", ")))
}

# ── Write output ───────────────────────────────────────────────────────────────
write_delim(df, output_file, delim = "|")
cat(sprintf("\nDone. De-identified file written to:\n  %s\n", output_file))
cat(sprintf("Final: %d rows, %d columns\n", nrow(df), ncol(df)))
if (!is.null(patient_col)) {
     cat(sprintf("       %d unique patients, %d unique encounters\n",
                 n_distinct(df[[patient_col]]),
                 if ("PAT_ENC_CSN_ID" %in% names(df)) n_distinct(df$PAT_ENC_CSN_ID) else NA))
}
