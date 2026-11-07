#' Pseudonymize subject identifiers consistently across datasets
#'
#' Replaces each subject's key (for example `USUBJID`) with a random pseudonym
#' that is the same in every dataset, so the datasets can still be joined.
#' Related identifier columns listed in `also` (for example `SUBJID`) are
#' replaced with the pseudonym's number so they stay aligned with the key, and
#' columns listed in `key_cols` that hold other subjects' keys (for example
#' `RSUBJID`) are mapped through the same pseudonyms.
#'
#' @param data A data frame, a named list of data frames linked by `key`, or
#'   the result of an earlier step such as [anon_subject_dates()]. Datasets
#'   without the `key` column are returned unchanged.
#' @param key Name of the subject identifier column.
#' @param also Names of related identifier columns that hold one value per
#'   subject (default `"SUBJID"`). They are replaced with the pseudonym's
#'   number. Columns not present in a dataset are skipped.
#' @param key_cols Names of columns that hold subject keys, possibly of other
#'   subjects (default `"RSUBJID"`, as in SDTM `RELSUB`). Each value is replaced
#'   with that subject's pseudonym; values that are not a known key are removed
#'   with a warning.
#' @param prefix Prefix for character pseudonyms, followed by a zero-padded
#'   number (e.g. `"SUBJ-042"`). Numeric keys get the number itself.
#' @param keep_secrets Logical. If `TRUE`, keep the mapping from keys to
#'   pseudonyms; read it with [anon_secrets()]. Default `FALSE`: the mapping is
#'   not kept anywhere.
#' @param seed Optional seed for the random assignment, for reproducible tests.
#'   Anyone with the seed and the original data can rebuild the mapping, so do
#'   not set it for data you share. The global random number state is restored
#'   afterwards.
#'
#' @details
#' Pseudonyms are numbered in random order, and rows of each dataset are then
#' sorted by the new key (keeping the order of rows within each subject), so
#' neither the pseudonyms nor the row order reveal the order of the original
#' keys. Column types and attributes such as variable labels are kept:
#' character keys get character pseudonyms, numeric keys get numbers, factors
#' stay factors. Value labels (haven `labels`), which describe the original
#' values, are dropped from replaced columns. Missing and blank keys are left
#' as they are. Values of `also` columns in rows without a key cannot be mapped
#' and are removed (set to missing), with a warning. `key`, `also` and
#' `key_cols` must not overlap.
#'
#' A warning lists any other character or factor column (values or factor
#' levels), in any dataset, that still contains original key values as whole
#' values or as whole words inside text (checked for character keys that are
#' not made only of digits, since numbers match many unrelated codes), and
#' any dataset without `key` that has a column named like `key`, `also` or
#' `key_cols` in any case. In datasets with `key`, a column named like one of
#' them in another case (such as `usubjid` beside `USUBJID`) is not replaced
#' and is named in a warning.
#' Original values of `also` columns (such as `SUBJID`) elsewhere are not
#' checked, because short numbers match many unrelated values. Missing values in
#' `also` columns stay missing. In `key`, `also` and `key_cols` columns, haven
#' user-defined missing values (`na_values`, `na_range`) become plain missing
#' values and the codes are dropped, because a code can't be told apart from a
#' real identifier.
#'
#' Random numbers come from R's random number generator. If your script calls
#' [set.seed()] before this step, anyone with the script and the data can
#' reproduce the assignment.
#'
#' Pseudonymized data remains personal data for anyone who holds the mapping or
#' can re-identify subjects by other means; measure the remaining risk with
#' [anon_risk()].
#'
#' @return An object of class `"anon_deid_step"`: a list with `data` (the
#'   transformed data, in the same form as the input), `log` (a tibble with
#'   `step`, `dataset`, `variable`, `n_changed`, `n_removed` and `detail`; the
#'   first row of each step records its settings; earlier steps are included
#'   when `data` was a step result), `settings` (a named list with each step's
#'   arguments, without the seed value) and
#'   `secrets` (`NULL` unless `keep_secrets = TRUE` here or in an earlier step).
#'   Saving the object (for example with [saveRDS()]) also saves its secrets.
#'
#' @seealso [anon_subject_dates()], [anon_secrets()], [anon_risk()]
#'
#' @examples
#' data <- list(
#'   dm = data.frame(USUBJID = c("01-701-1015", "01-701-1023"), SUBJID = c("1015", "1023")),
#'   vs = data.frame(USUBJID = c("01-701-1015", "01-701-1023"), VSSTRESN = c(70, 80))
#' )
#' result <- anon_subject_ids(data)
#' result$data
#'
#' @export
anon_subject_ids <- function(
  data,
  key = "USUBJID",
  also = "SUBJID",
  key_cols = "RSUBJID",
  prefix = "SUBJ-",
  keep_secrets = FALSE,
  seed = NULL
) {
  if (key %in% also || length(intersect(also, key_cols)) > 0) {
    stop("`key`, `also` and `key_cols` must not overlap.", call. = FALSE)
  }
  key_cols <- setdiff(key_cols, key)

  input <- as_dataset_list(data)
  data <- input$data
  keyed <- keyed_datasets(data, key)
  for (ds in keyed) {
    for (col in intersect(c(key, also, key_cols), names(data[[ds]]))) {
      data[[ds]][[col]] <- zap_user_missing(data[[ds]][[col]])
    }
  }
  keys <- subject_keys(data[keyed], key)

  numbers <- sample_int_with_seed(length(keys), length(keys), seed = seed)
  width <- nchar(as.character(length(keys)))
  padded <- formatC(numbers, width = width, flag = "0")
  pseudonyms <- paste0(prefix, padded)

  log <- deid_log_start(
    "subject_ids", length(keys), key, seed,
    settings = paste0(
      "also = ", deparse_list(also), ", key_cols = ", deparse_list(key_cols),
      ", prefix = \"", prefix, "\"; rows sorted by the new key"
    )
  )
  removed_also <- 0
  removed_key_cols <- 0

  for (ds in keyed) {
    df <- data[[ds]]
    idx <- match(normalize_keys(df[[key]]), keys)

    df[[key]] <- replace_ids(df[[key]], idx, pseudonyms, numbers)
    log <- deid_log_add(log, ds, key, "replaced with pseudonyms", sum(!is.na(idx)))

    for (col in intersect(also, names(df))) {
      check_one_value_per_subject(df[[col]], idx, col)
      present <- !is.na(blank_to_na(df[[col]]))
      unmapped <- is.na(idx) & present
      also_idx <- ifelse(present, idx, NA_integer_)
      removed_also <- removed_also + sum(unmapped)
      df[[col]] <- replace_ids(df[[col]], also_idx, padded, numbers, remove = unmapped)
      log <- deid_log_add(log, ds, col, "replaced with the pseudonym number", sum(!is.na(also_idx)), sum(unmapped))
    }

    for (col in setdiff(intersect(key_cols, names(df)), c(key, also))) {
      col_idx <- match(normalize_keys(df[[col]]), keys)
      unmapped <- is.na(col_idx) & !is.na(normalize_keys(df[[col]]))
      removed_key_cols <- removed_key_cols + sum(unmapped)
      df[[col]] <- replace_ids(df[[col]], col_idx, pseudonyms, numbers, remove = unmapped)
      log <- deid_log_add(log, ds, col, "replaced with pseudonyms", sum(!is.na(col_idx)), sum(unmapped))
    }

    data[[ds]] <- sort_by_new_key(df, idx, numbers)
  }
  log <- deid_log_unkeyed(log, data, keyed, key)

  if (removed_also > 0) {
    warning(
      removed_also, " value(s) of ", paste0("`", also, "`", collapse = ", "),
      " in rows without a `", key, "` could not be mapped and were removed.",
      call. = FALSE
    )
  }
  if (removed_key_cols > 0) {
    warning(
      removed_key_cols, " value(s) of ", paste0("`", key_cols, "`", collapse = ", "),
      " are not known `", key, "` values and were removed.",
      call. = FALSE
    )
  }
  if (!all(vapply(data[keyed], function(df) is.numeric(df[[key]]), logical(1)))) {
    warn_leftover_keys(data, keyed, key, keys, skip = c(also, key_cols))
  }
  warn_id_name_variants(data, keyed, c(key, also, key_cols))
  warn_unkeyed_identifiers(data, keyed, key, c(also, key_cols), check_dates = FALSE)

  secrets <- if (isTRUE(keep_secrets)) {
    new_ids <- if (any_numeric_key(data[keyed], key)) numbers else pseudonyms
    list(subject_ids = tibble::tibble(key = keys, pseudonym = new_ids))
  }
  settings <- list(subject_ids = list(
    key = key, also = also, key_cols = key_cols, prefix = prefix,
    keep_secrets = isTRUE(keep_secrets), seeded = !is.null(seed)
  ))
  new_deid_step(input$unwrap(data), log, secrets, previous = input$previous, settings = settings)
}

#' Shift each subject's dates by a random offset
#'
#' Gives every subject one random offset, in whole days, and shifts all of that
#' subject's dates in every dataset by it. Intervals within a subject (time
#' since dose, study days, visit windows) are unchanged, while calendar dates no
#' longer line up between subjects or with real dates. This is the "date
#' offsetting" technique described in EMA's Anonymisation Report Form
#' Instructions: a random offset, with the same offset applied to all dates for
#' each participant.
#'
#' @inheritParams anon_subject_ids
#' @param vars Names of date columns to shift. By default, every character or
#'   factor column whose name ends in `DTC` (any case; ISO 8601 dates, as in
#'   SDTM) and every Date or POSIXct column, in each dataset that has the `key`.
#'   In SDTM supplemental qualifier datasets (with `QNAM` and `QVAL`), `QVAL`
#'   values that are ISO 8601 dates with more than a bare year are shifted too
#'   (year-only values are left alone because they can't be told apart from
#'   numbers; a warning names the columns where they were kept). In columns not
#'   named `*DTC`, such as `QVAL` or a result column given in `vars`, only the
#'   values that are dates are shifted and other values are kept; bare
#'   four-digit numbers are kept too (with a warning),
#'   unless every value of the column, in every dataset, is a bare year (such
#'   as a column of birth years). Columns given in
#'   `vars` must be Date, POSIXct, or character or factor columns of ISO 8601
#'   dates.
#' @param max_days,min_days Each subject's offset is drawn uniformly from the
#'   whole numbers between `min_days` and `max_days` days, earlier or later.
#'   Defaults 30 and 365; `max_days` can be at most 3650. A warning is given
#'   when the range is so narrow, or includes 0, that offsets are easy to guess
#'   or some subjects keep their real dates.
#' @param partial How to handle partial ISO 8601 dates in character columns:
#'   `"shift"` (default) or `"remove"` (replace them with the column's missing
#'   value). With `"shift"`, a year-and-month date (`"2014-03"`, or SDTM
#'   `"2014-03--T08:00"` with the time kept) moves the middle of the month by
#'   the subject's offset and keeps month precision; a year-only date
#'   (`"2014"`, or SDTM `"2014---15"` with the day kept) moves by whole years,
#'   at least one, in the direction of the offset, so the real year is never
#'   kept. Both ends of an interval (`"2014-01-02/2014-01-05"`) are shifted.
#' @param keep_secrets Logical. If `TRUE`, keep each subject's offset; read it
#'   with [anon_secrets()]. Default `FALSE`.
#'
#' @details
#' Full ISO 8601 dates move by exactly the subject's offset and keep their
#' format and any time part. Date columns keep their type, and POSIXct columns
#' keep their type, time zone and clock time (the calendar date moves, so a
#' shift across a daylight saving change keeps 08:00 at 08:00). Column
#' attributes such as variable labels are kept; value labels (haven `labels`)
#' are dropped, and haven user-defined missing values (`na_values`, `na_range`)
#' in the key and date columns become plain missing values. A clock time that doesn't exist on the new date (inside a
#' daylight saving gap) moves forward by the gap. Study day variables (`--DY`) are not
#' changed and stay consistent with the shifted full dates, because each
#' subject's dates all move together.
#'
#' Year-and-month dates are shifted from the middle of the month, and year-only
#' dates by whole years, so the order between a partial date and a full date of
#' the same subject can change (for example a start month can end up after an
#' end date).
#'
#' Dates in rows without a key cannot be tied to a subject's offset and are
#' removed, with a warning. In `*DTC` columns, values that are not recognised
#' ISO 8601 dates, or whose year would fall outside 0001-9999, are removed with
#' a warning; this includes a month and day without a year (`"--12-15"`).
#' Time-only values (`"-----T07:15"`) and durations (`"P1D"`) have no date and
#' are kept. Text
#' columns given in `vars` must contain at least one recognised date. Datasets without the `key` column (for example trial-level domains
#' such as `TS`, `TV` and `TA`) are left unchanged and listed in the log; their
#' real trial dates can narrow down subjects' offsets. A warning lists any other
#' column in a keyed dataset that holds dates but was not shifted: Date or
#' POSIXct columns left out of `vars`, numeric columns with a SAS date format
#' (in a separate warning; convert them to Date or POSIXct to shift them), and
#' text columns with ISO 8601 dates as whole values, or dates inside text
#' (which can't be shifted) written as ISO 8601, with a month name
#' (`05JAN2014`, `UNK-JAN-2014`, `5 Jan 2014`, `Jan 5, 2014`, `JAN2014`) or as
#' numbers with separators (`01/05/2014`, `1/5/14`, `01.05.2014`,
#' `2014/01/05`), including the values kept in mixed columns such as `QVAL`.
#' Other formats, such as `20140105`, are not detected. It also lists any dataset without `key` that has such
#' columns or a column named like `key` in another case.
#'
#' Random numbers come from R's random number generator; see
#' [anon_subject_ids()]. Run this before or after [anon_subject_ids()]; each
#' subject keeps one offset either way. Under HIPAA, HHS does not address date
#' shifting directly, but Safe Harbor removes all elements of dates except the
#' year, so shifted dates do not meet it; see the package README.
#'
#' @return An object of class `"anon_deid_step"`, as for [anon_subject_ids()].
#'
#' @examples
#' data <- list(
#'   dm = data.frame(USUBJID = c("a", "b"), RFSTDTC = c("2014-01-02", "2013-07-19")),
#'   pc = data.frame(USUBJID = c("a", "a", "b"),
#'                   PCDTC = c("2014-01-02T08:00", "2014-01-03T09:30", "2013-07-19T08:00"))
#' )
#' anon_subject_dates(data)$data
#'
#' @export
anon_subject_dates <- function(
  data,
  key = "USUBJID",
  vars = NULL,
  max_days = 365,
  min_days = 30,
  partial = c("shift", "remove"),
  keep_secrets = FALSE,
  seed = NULL
) {
  partial <- match.arg(partial)
  validate_offset_range(max_days, min_days)

  input <- as_dataset_list(data)
  data <- input$data
  keyed <- keyed_datasets(data, key)
  for (ds in keyed) {
    for (col in intersect(c(key, vars), names(data[[ds]]))) {
      data[[ds]][[col]] <- zap_user_missing(data[[ds]][[col]])
    }
  }
  keys <- subject_keys(data[keyed], key)

  date_vars <- lapply(keyed, function(ds) dataset_date_vars(data[[ds]], vars, key, ds))
  names(date_vars) <- keyed
  if (!is.null(vars) && length(unlist(date_vars)) == 0) {
    stop("None of `vars` were found in a dataset with `", key, "`.", call. = FALSE)
  }
  for (ds in keyed) {
    for (col in date_vars[[ds]]) {
      data[[ds]][[col]] <- zap_user_missing(data[[ds]][[col]])
    }
  }
  # In mixed columns, bare years are kept as numbers unless the column holds
  # only years in every dataset.
  mixed_cols <- unique(grep("DTC$", unlist(date_vars), ignore.case = TRUE, value = TRUE, invert = TRUE))
  years_only_cols <- Filter(function(col) holds_only_years(data[keyed], col), mixed_cols)
  kept_years <- character(0)
  kept_qval_years <- character(0)

  offsets <- draw_offsets(length(keys), max_days, min_days, seed)

  log <- deid_log_start(
    "subject_dates", length(keys), key, seed,
    settings = paste0(
      "min_days = ", min_days, ", max_days = ", max_days, ", partial = \"", partial, "\"",
      if (!is.null(vars)) paste0(", vars = ", deparse_list(vars))
    )
  )
  totals <- c(no_key = 0, invalid = 0)

  for (ds in keyed) {
    df <- data[[ds]]
    subject_offset <- offsets[match(normalize_keys(df[[key]]), keys)]

    for (col in date_vars[[ds]]) {
      keep_non_dates <- col %in% mixed_cols
      shifted <- shift_dates_by_subject(
        df[[col]], subject_offset, partial, keep_non_dates,
        keep_years = keep_non_dates && !col %in% years_only_cols
      )
      if (shifted$kept_years > 0) {
        kept_years <- c(kept_years, paste0(ds, ".", col))
      }
      df[[col]] <- shifted$values
      totals <- totals + c(shifted$no_key, shifted$invalid)
      log <- deid_log_add(
        log, ds, col, shift_detail(shifted), shifted$shifted,
        shifted$no_key + shifted$invalid + shifted$partial
      )
    }

    if (is.null(vars) && all(c("QNAM", "QVAL") %in% names(df))) {
      shifted <- shift_qval_dates(df$QVAL, subject_offset, partial)
      if (shifted$kept_years > 0) {
        kept_qval_years <- c(kept_qval_years, paste0(ds, ".QVAL"))
      }
      df$QVAL <- shifted$values
      totals <- totals + c(shifted$no_key, shifted$invalid)
      if (shifted$shifted + shifted$no_key + shifted$invalid + shifted$partial > 0) {
        log <- deid_log_add(
          log, ds, "QVAL", paste0(shift_detail(shifted), " (date values only)"),
          shifted$shifted, shifted$no_key + shifted$invalid + shifted$partial
        )
      }
    }

    data[[ds]] <- df
  }
  log <- deid_log_unkeyed(log, data, keyed, key)

  if (totals[["no_key"]] > 0) {
    warning(totals[["no_key"]], " date value(s) in rows without a `", key, "` were removed.", call. = FALSE)
  }
  if (totals[["invalid"]] > 0) {
    warning(
      totals[["invalid"]], " value(s) are not recognised ISO 8601 dates, or would fall outside years 0001-9999, and were removed.",
      call. = FALSE
    )
  }
  if (length(kept_years) > 0) {
    warning(
      "These columns hold bare four-digit numbers that were kept, because the column ",
      "also holds other values: ", paste(kept_years, collapse = ", "),
      ". If they are years, they still show real years; redact or remove them.",
      call. = FALSE
    )
  }
  if (length(kept_qval_years) > 0) {
    warning(
      "These QVAL columns hold bare four-digit numbers, which are never shifted because they can't ",
      "be told apart from other numbers: ", paste(kept_qval_years, collapse = ", "),
      ". If some are years (such as a birth year), redact or remove them.",
      call. = FALSE
    )
  }
  warn_unshifted_iso_dates(data, keyed, date_vars, key, qval_processed = is.null(vars))
  warn_unkeyed_identifiers(data, keyed, key, character(0), check_dates = TRUE)

  secrets <- if (isTRUE(keep_secrets)) {
    list(date_offsets = tibble::tibble(key = keys, offset_days = offsets))
  }
  settings <- list(subject_dates = list(
    key = key, vars = vars, max_days = max_days, min_days = min_days, partial = partial,
    keep_secrets = isTRUE(keep_secrets), seeded = !is.null(seed)
  ))
  new_deid_step(input$unwrap(data), log, secrets, previous = input$previous, settings = settings)
}

#' Read the secrets kept by a de-identification step
#'
#' Returns the information needed to link de-identified data back to the
#' source, such as the mapping from keys to pseudonyms or each subject's date
#' offset. It exists only when a step was run with `keep_secrets = TRUE`. Keep
#' it separate from anything you share.
#'
#' Each table has a `key` column with the subject key as it was when that step
#' ran: the original key, or the pseudonym if [anon_subject_ids()] ran first.
#'
#' @param x An object of class `"anon_deid_step"`.
#'
#' @return A named list of tibbles, e.g. `subject_ids` or `date_offsets`.
#'
#' @examples
#' data <- data.frame(USUBJID = c("a", "b"))
#' result <- anon_subject_ids(data, keep_secrets = TRUE)
#' anon_secrets(result)
#'
#' @export
anon_secrets <- function(x) {
  if (!inherits(x, "anon_deid_step")) {
    stop("`x` must be the result of a de-identification step such as anon_subject_ids().", call. = FALSE)
  }
  if (is.null(x$secrets)) {
    stop(
      "This step holds no secrets: run the steps with `keep_secrets = TRUE`. anon() returns a ",
      "copy without secrets, so read them from the step you passed to anon().",
      call. = FALSE
    )
  }
  mget(sort(ls(x$secrets)), envir = x$secrets)
}

#' @export
print.anon_deid_step <- function(x, ...) {
  cat("De-identification steps:", paste(unique(x$log$step), collapse = ", "), "\n")
  cat("Secrets:", if (is.null(x$secrets)) "not kept" else "kept (read with anon_secrets())", "\n\n")
  print(as.data.frame(x$log), row.names = FALSE)
  invisible(x)
}

#' @export
print.anon_secrets <- function(x, ...) {
  cat("<anon_secrets: ", paste(sort(ls(x)), collapse = ", "),
      "; not shown, read with anon_secrets()>\n", sep = "")
  invisible(x)
}

#' @exportS3Method
as_anon_payload.anon_deid_step <- function(x, path = "$") {
  list(
    schema_version = anon_json_schema_version(),
    kind = "anon_deid_step",
    steps = unique(x$log$step),
    log = serialize_table_payload(x$log, path = paste0(path, "$log"))
  )
}

# helpers -----------------------------------------------------------------

# Secrets live in an environment so that print(), anon() and JSON output never
# show them by accident. Logs and secrets from an earlier step are carried on.
new_deid_step <- function(data, log, secrets, previous = NULL, settings = list()) {
  if (!is.null(previous)) {
    log <- rbind(previous$log, log)
    settings <- c(previous$settings, settings)
    names(settings) <- make.unique(names(settings), sep = "_")
    if (!is.null(previous$secrets)) {
      secrets <- c(mget(ls(previous$secrets), envir = previous$secrets), secrets)
      names(secrets) <- make.unique(names(secrets), sep = "_")
    }
  }
  secrets_env <- NULL
  if (length(secrets) > 0) {
    secrets_env <- list2env(secrets, envir = new.env(parent = emptyenv()))
    class(secrets_env) <- "anon_secrets"
  }
  structure(
    list(data = data, log = log, settings = settings, secrets = secrets_env),
    class = "anon_deid_step"
  )
}

as_dataset_list <- function(data) {
  previous <- NULL
  if (inherits(data, "anon_deid_step")) {
    previous <- data
    data <- data$data
  }
  if (is.data.frame(data)) {
    check_unique_columns(list(data = data))
    return(list(data = list(data = data), unwrap = function(x) x[[1]], previous = previous))
  }
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "") ||
    !all(vapply(data, is.data.frame, logical(1)))) {
    stop("`data` must be a data frame, a named list of data frames, or a step result.", call. = FALSE)
  }
  if (anyDuplicated(names(data))) {
    stop("Dataset names in `data` must be unique.", call. = FALSE)
  }
  check_unique_columns(data)
  list(data = data, unwrap = function(x) x, previous = previous)
}

# Columns are processed by name, so a repeated name would leave all but the
# first copy untouched.
check_unique_columns <- function(data) {
  for (ds in names(data)) {
    dup <- names(data[[ds]])[duplicated(names(data[[ds]]))]
    if (length(dup) > 0) {
      stop("Column names in `", ds, "` must be unique; `", dup[1], "` repeats.", call. = FALSE)
    }
  }
}

keyed_datasets <- function(data, key) {
  if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
    stop("`key` must be a single column name.", call. = FALSE)
  }
  keyed <- names(data)[vapply(data, function(df) key %in% names(df), logical(1))]
  if (length(keyed) == 0) {
    stop("Key column `", key, "` not found in any dataset.", call. = FALSE)
  }
  keyed
}

subject_keys <- function(data, key) {
  check_key_types(data, key)
  keys <- unlist(lapply(data, function(df) normalize_keys(df[[key]])), use.names = FALSE)
  unique(keys[!is.na(keys)])
}

# Replace identifiers by subject index, keeping the column's type and
# attributes (such as labels). Rows without a subject keep their value unless
# `remove` is TRUE for them.
replace_ids <- function(x, idx, labels, numbers, remove = rep(FALSE, length(x))) {
  has <- !is.na(idx)
  if (is.numeric(x)) {
    out <- x
    out[has] <- numbers[idx[has]]
    out[remove] <- NA
    return(drop_value_labels(out))
  }
  if (is.factor(x)) {
    values <- as.character(x)
    values[has] <- labels[idx[has]]
    values[remove] <- missing_text_like(values)
    return(with_attributes_of(factor(values), x))
  }
  out <- x
  out[has] <- labels[idx[has]]
  out[remove] <- missing_text_like(as.character(x))
  drop_value_labels(out)
}

# haven user-defined missing values (labelled_spss na_values and na_range)
# become plain missing values and the codes are dropped. In identifier columns
# a code can't be told apart from a real identifier, and a kept code could
# match a new pseudonym number.
zap_user_missing <- function(x) {
  if (!inherits(x, "haven_labelled_spss")) {
    return(x)
  }
  values <- vctrs::vec_data(x)
  values[is.na(x)] <- NA
  attrs <- attributes(x)
  attrs[c("na_values", "na_range")] <- NULL
  attributes(values) <- attrs
  values
}

# Value labels (haven "labels") describe the original values, so they are
# dropped when values are replaced or shifted.
drop_value_labels <- function(x) {
  attr(x, "labels") <- NULL
  attr(x, "na_values") <- NULL
  attr(x, "na_range") <- NULL
  x
}

# Rebuild attributes (labels, formats, extra classes) on a new factor.
with_attributes_of <- function(new, old) {
  extra <- attributes(old)[setdiff(names(attributes(old)), c("levels", "class", "names", "labels", "na_values", "na_range"))]
  attributes(new) <- c(attributes(new), extra)
  new
}

# The missing value a character column already uses: "" if it has blank
# strings (as in SDTM read from XPT), otherwise NA.
missing_text_like <- function(x) {
  if (any(!is.na(x) & x == "")) "" else NA_character_
}

check_one_value_per_subject <- function(x, idx, col) {
  value <- blank_to_na(x)
  if (is.numeric(value)) value <- key_as_character(value)
  pairs <- unique(data.frame(idx = idx, value = as.character(value))[!is.na(idx) & !is.na(value), ])
  if (anyDuplicated(pairs$idx)) {
    stop("`", col, "` has more than one value for some subjects; it can't be aligned with the pseudonyms.", call. = FALSE)
  }
}

validate_offset_range <- function(max_days, min_days) {
  whole <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x == round(x)
  if (!whole(max_days) || max_days < 1 || max_days > 3650) {
    stop("`max_days` must be a single whole number from 1 to 3650 (ten years).", call. = FALSE)
  }
  if (!whole(min_days) || min_days < 0 || min_days > max_days) {
    stop("`min_days` must be a single whole number between 0 and `max_days`.", call. = FALSE)
  }
  n_offsets <- 2 * (max_days - min_days + 1) - (min_days == 0)
  if (min_days == 0) {
    warning("`min_days = 0` lets some subjects keep their real dates.", call. = FALSE)
  }
  if (n_offsets < 30) {
    warning("Only ", n_offsets, " possible offsets; offsets are easy to guess.", call. = FALSE)
  }
}

dataset_date_vars <- function(df, vars, key, dataset) {
  if (!is.null(vars)) {
    selected <- setdiff(intersect(vars, names(df)), key)
    for (col in selected) {
      x <- df[[col]]
      not_dates <- if (inherits(x, c("Date", "POSIXct"))) {
        FALSE
      } else if (is.character(x) || is.factor(x)) {
        kinds <- parse_iso8601_dates(as.character(x))$kind
        any(kinds != "empty") && all(kinds[kinds != "empty"] == "invalid")
      } else {
        TRUE
      }
      if (not_dates) {
        stop("`", col, "` in `", dataset, "` is not a date column (Date, POSIXct, or ISO 8601 text).", call. = FALSE)
      }
    }
    return(selected)
  }
  is_date <- vapply(df, function(col) inherits(col, c("Date", "POSIXct")), logical(1))
  is_text <- vapply(df, function(col) is.character(col) || is.factor(col), logical(1))
  is_dtc <- grepl("DTC$", names(df), ignore.case = TRUE) & is_text
  setdiff(names(df)[is_date | is_dtc], key)
}

# Draw `size` indices from 1:n with replacement, restoring the global RNG state.
sample_with_seed <- function(n, size, seed = NULL) {
  if (is.null(seed)) {
    return(sample.int(n, size = size, replace = TRUE))
  }
  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  }
  set.seed(seed)
  sample.int(n, size = size, replace = TRUE)
}

# Shift each value by its subject's offset (days), keeping the column's type and
# attributes. Values without an offset, and text that is not ISO 8601, are
# removed. Returns the new values and counts.
shift_dates_by_subject <- function(x, offset, partial, keep_non_dates = FALSE, keep_years = keep_non_dates) {
  if (inherits(x, "haven_labelled")) {
    shifted <- shift_dates_by_subject(vctrs::vec_data(x), offset, partial, keep_non_dates, keep_years)
    attrs <- attributes(x)
    attrs[c("labels", "na_values", "na_range")] <- NULL
    attributes(shifted$values) <- attrs
    return(shifted)
  }
  if (is.factor(x)) {
    shifted <- shift_dates_by_subject(as.character(x), offset, partial, keep_non_dates, keep_years)
    shifted$values <- with_attributes_of(factor(shifted$values), x)
    return(shifted)
  }

  present <- if (is.character(x)) !is.na(x) & !grepl("^[[:space:]]*$", x, useBytes = TRUE) else !is.na(x)
  use <- ifelse(is.na(offset), 0, offset)
  invalid <- rep(FALSE, length(x))
  removed_partial <- rep(FALSE, length(x))
  unchanged <- rep(FALSE, length(x))
  year_kept <- rep(FALSE, length(x))

  out <- x
  if (is.character(x)) {
    kinds <- parse_iso8601_dates(x)$kind
    # Values with no date to shift: time-only values and durations, and in
    # mixed columns (not named *DTC) values that aren't dates at all. With
    # `keep_years`, bare years are kept too, as they are more likely numbers.
    unchanged <- present & kinds %in% c("time_only", "duration")
    if (keep_non_dates) {
      unchanged <- unchanged | (present & kinds == "invalid")
    }
    if (keep_years) {
      year_kept <- present & is_year_only_value(x, kinds)
      unchanged <- unchanged | year_kept
    }
    no_offset <- present & !unchanged & is.na(offset)
    new_values <- suppressWarnings(shift_iso8601(x, use, partial, year_step = TRUE))
    if (partial == "remove") {
      removed_partial <- present & !unchanged & !no_offset & is_partial_date(x, kinds)
    }
    invalid <- present & !unchanged & !no_offset & !removed_partial & !nzchar(new_values)
    out[] <- new_values
    out[unchanged] <- x[unchanged]
    out[no_offset | invalid | removed_partial] <- missing_text_like(x)
  } else if (inherits(x, "POSIXct")) {
    no_offset <- present & is.na(offset)
    tz <- attr(x, "tzone", exact = TRUE)
    tz <- if (is.null(tz) || length(tz) == 0) "" else tz[[1]]
    lt <- as.POSIXlt(x, tz = tz)
    lt$mday <- lt$mday + use
    lt$isdst <- -1L
    out[] <- as.POSIXct(lt, tz = tz)
    invalid <- present & !no_offset & !year_in_range(out)
    out[no_offset | invalid] <- NA
  } else {
    no_offset <- present & is.na(offset)
    out[] <- x + use
    invalid <- present & !no_offset & !year_in_range(out)
    out[no_offset | invalid] <- NA
  }
  out <- drop_value_labels(out)

  list(
    values = out,
    shifted = sum(present & !unchanged & !no_offset & !invalid & !removed_partial),
    no_key = sum(no_offset),
    invalid = sum(invalid),
    partial = sum(removed_partial),
    kept_years = sum(year_kept)
  )
}

# Whether a text column holds only bare years (ignoring missing and blank
# values) in every dataset that has it as text, and has some value somewhere.
holds_only_years <- function(data, col) {
  checks <- vapply(data, function(df) {
    x <- df[[col]]
    if (is.null(x) || inherits(x, c("Date", "POSIXct"))) {
      return(NA)
    }
    x <- if (is.factor(x)) as.character(x) else as.character(vctrs::vec_data(x))
    x <- x[!is.na(x) & !grepl("^[[:space:]]*$", x, useBytes = TRUE)]
    if (length(x) == 0) NA else all(is_year_only_value(x))
  }, logical(1))
  any(!is.na(checks)) && all(checks, na.rm = TRUE)
}

# Whether each Date or POSIXct value falls in years 0001-9999, the range ISO
# 8601 text can hold. Missing values count as in range.
year_in_range <- function(x) {
  year <- as.integer(format(x, "%Y"))
  is.na(x) | (!is.na(year) & year >= 1 & year <= 9999)
}

# Bare years, and intervals whose date ends are bare years ("1000/2000"). In
# columns that mix dates with other values these are more likely numbers.
is_year_only_value <- function(x, kinds = parse_iso8601_dates(x)$kind) {
  out <- kinds == "year"
  ends <- interval_end_kinds(x, kinds)
  if (!is.null(ends)) {
    dated <- ends$kind != "duration"
    out[unique(ends$value)] <- vapply(
      split(ends$kind[dated] == "year", factor(ends$value[dated], levels = unique(ends$value))),
      all, logical(1)
    )
  }
  out
}

# Partial dates, including intervals with a partial end.
is_partial_date <- function(x, kinds = parse_iso8601_dates(x)$kind) {
  partial_kinds <- c("month", "month_time", "year", "year_day")
  out <- kinds %in% partial_kinds
  ends <- interval_end_kinds(x, kinds)
  if (!is.null(ends)) {
    partial_end <- tapply(ends$kind %in% partial_kinds, ends$value, any)
    out[as.integer(names(partial_end))] <- partial_end
  }
  out
}

# The kinds of the two ends of each interval value, parsed in one call:
# a data frame with the value's position and the end's kind, or NULL.
interval_end_kinds <- function(x, kinds) {
  is_interval <- which(kinds == "interval")
  if (length(is_interval) == 0) {
    return(NULL)
  }
  ends <- strsplit(x[is_interval], "/", fixed = TRUE)
  data.frame(
    value = rep(is_interval, lengths(ends)),
    kind = parse_iso8601_dates(unlist(ends, use.names = FALSE))$kind
  )
}

# Shift QVAL values that are ISO 8601 dates with more than a bare year (which
# can't be told apart from a number); other values are untouched.
shift_qval_dates <- function(x, offset, partial = "shift") {
  text <- as.character(x)
  kinds <- parse_iso8601_dates(text)$kind
  year_only <- !is.na(text) & is_year_only_value(text, kinds)
  kept_years <- sum(year_only)
  is_date <- kinds %in% c("full", "month", "month_time", "year_day", "interval") & !year_only
  out <- x
  if (!any(is_date)) {
    if (is.factor(x)) out <- with_attributes_of(factor(text), x)
    return(list(values = out, shifted = 0, no_key = 0, invalid = 0, partial = 0, kept_years = kept_years))
  }
  no_offset <- is_date & is.na(offset)
  removed_partial <- if (partial == "remove") is_date & !no_offset & is_partial_date(text, kinds) else rep(FALSE, length(text))
  movable <- is_date & !no_offset & !removed_partial
  new_text <- text
  new_text[movable] <- suppressWarnings(shift_iso8601(text[movable], offset[movable], "shift", year_step = TRUE))
  failed <- movable & !nzchar(new_text)
  movable <- movable & !failed
  new_text[no_offset | removed_partial | failed] <- missing_text_like(text)
  if (is.factor(x)) {
    out <- with_attributes_of(factor(new_text), x)
  } else {
    out[] <- new_text
  }
  list(
    values = drop_value_labels(out), shifted = sum(movable), no_key = sum(no_offset),
    invalid = sum(failed), partial = sum(removed_partial), kept_years = kept_years
  )
}

shift_detail <- function(shifted) {
  removed <- c(
    if (shifted$no_key > 0) paste0(shifted$no_key, " removed (no key)"),
    if (shifted$invalid > 0) paste0(shifted$invalid, " removed (not a recognised date, or year out of range)"),
    if (isTRUE(shifted$partial > 0)) paste0(shifted$partial, " removed (partial)")
  )
  paste(c(paste0(shifted$shifted, " value(s) shifted"), removed), collapse = ", ")
}

# Warn about text columns, in any dataset, that still contain original keys as
# whole values or as whole words inside text. Columns named like the key or the
# `skip` columns, in any case, are not checked: they were just replaced, or
# warn_id_name_variants() or warn_unkeyed_identifiers() names them. Keys made only of
# digits are not searched for, as they match many unrelated numbers.
warn_leftover_keys <- function(data, keyed, key, keys, skip = character(0)) {
  keys <- valid_utf8_text(keys)
  keys <- keys[!grepl("^[0-9]+$", keys)]
  if (length(keys) == 0) {
    return(invisible())
  }
  key_pattern <- paste0(
    whole_value_prefix, "(?:", paste(escape_regex_chars(keys), collapse = "|"), ")", whole_value_suffix
  )
  hits <- character(0)
  for (ds in names(data)) {
    df <- data[[ds]]
    cols <- names(df)[!tolower(names(df)) %in% tolower(c(key, skip))]
    for (col in cols) {
      values <- text_values(df[[col]])
      if (length(values) > 0 && any(stringi::stri_detect_regex(values, key_pattern), na.rm = TRUE)) {
        hits <- c(hits, paste0(ds, ".", col))
      }
    }
  }
  if (length(hits) > 0) {
    warning(
      "These columns still contain original `", key, "` values, possibly inside text: ",
      paste(hits, collapse = ", "),
      ". Add them to `key_cols` if they hold only keys; otherwise redact or remove them.",
      call. = FALSE
    )
  }
}

# Distinct text values of a character column, or the levels of a factor
# (including unused levels), as UTF-8; NULL for other columns.
text_values <- function(x) {
  values <- if (is.factor(x)) levels(x) else if (is.character(x)) unique(as.character(x)) else NULL
  if (is.null(values)) {
    return(NULL)
  }
  valid_utf8_text(values)
}

# Non-missing values converted to UTF-8 from their declared encoding (such as
# latin1); values that are still not valid UTF-8 are dropped, because regular
# expressions can't search them.
valid_utf8_text <- function(x) {
  x <- enc2utf8(as.character(x[!is.na(x)]))
  x[validUTF8(x)]
}

# Warn about columns in keyed datasets that hold dates but were not shifted:
# unselected date columns, and dates kept as text in the mixed columns that
# were shifted (`vars` columns not named *DTC, and QVAL).
warn_unshifted_iso_dates <- function(data, keyed, date_vars, key, qval_processed) {
  hits <- character(0)
  sas_hits <- character(0)
  for (ds in keyed) {
    df <- data[[ds]]
    is_supp <- all(c("QNAM", "QVAL") %in% names(df))
    mixed <- c(
      date_vars[[ds]][!grepl("DTC$", date_vars[[ds]], ignore.case = TRUE)],
      if (is_supp && qval_processed) "QVAL"
    )
    others <- setdiff(names(df), c(key, date_vars[[ds]], mixed))
    sas <- sas_date_columns(df, others)
    hits <- c(
      hits,
      qualify(ds, iso_date_columns(df, setdiff(others, sas))),
      qualify(ds, Filter(function(col) has_dates_in_text(df[[col]], only_non_dates = TRUE), mixed))
    )
    sas_hits <- c(sas_hits, qualify(ds, sas))
  }
  if (length(hits) > 0) {
    warning(
      "These columns contain dates that were not shifted: ",
      paste(hits, collapse = ", "),
      ". If they hold ISO 8601 dates, list them in `vars` together with every other date column ",
      "(`vars` replaces the default selection); dates inside free text or in other formats ",
      "(such as 05JAN2014) can't be shifted, so redact or remove them.",
      call. = FALSE
    )
  }
  if (length(sas_hits) > 0) {
    warning(
      "These numeric columns have a SAS date or datetime format and were not shifted: ",
      paste(sas_hits, collapse = ", "),
      ". Convert them to Date or POSIXct (SAS counts from 1960-01-01) and list them in `vars`, or remove them.",
      call. = FALSE
    )
  }
}

# Columns, among `cols`, that hold dates: Date or POSIXct columns, numeric
# columns with a SAS date or datetime format, and text or factor columns with
# dates as whole values or inside text.
iso_date_columns <- function(df, cols) {
  is_date_col <- vapply(cols, function(col) {
    x <- df[[col]]
    inherits(x, c("Date", "POSIXct")) || has_dates_in_text(x)
  }, logical(1))
  c(cols[is_date_col], sas_date_columns(df, cols[!is_date_col]))
}

# "dataset.column" names; empty when there are no columns.
qualify <- function(dataset, cols) {
  if (length(cols) == 0) character(0) else paste0(dataset, ".", cols)
}

# Numeric columns, among `cols`, with a SAS date or datetime display format.
sas_date_columns <- function(df, cols) {
  is_sas <- vapply(cols, function(col) {
    x <- df[[col]]
    is.numeric(x) && grepl(sas_date_format_pattern, attr(x, "format.sas", exact = TRUE) %||% "", ignore.case = TRUE)
  }, logical(1))
  cols[is_sas]
}

# Whether a text or factor column holds ISO 8601-like dates as whole values, or
# dates inside text: ISO 8601, with a month name (05JAN2014, 5 Jan 2014,
# Jan 5, 2014, JAN2014) or numeric with separators (01/05/2014, 01.05.2014,
# 2014/01/05). With `only_non_dates`, values recognised as ISO 8601 dates
# (which were shifted) are not checked.
has_dates_in_text <- function(x, only_non_dates = FALSE) {
  values <- text_values(x)
  if (only_non_dates && length(values) > 0) {
    values <- values[parse_iso8601_dates(values)$kind == "invalid"]
  }
  length(values) > 0 && any(
    grepl(iso_date_like_pattern, values, useBytes = TRUE) |
      grepl(embedded_date_pattern, values, useBytes = TRUE) |
      grepl(month_name_date_pattern, values, ignore.case = TRUE, useBytes = TRUE) |
      grepl(slash_date_pattern, values, useBytes = TRUE)
  )
}

iso_date_like_pattern <- "^[0-9]{4}-([0-9]{2}(-[0-9]{2}|--T)|-{2}[0-9]{2})|^[0-9]{4}-[0-9]{2}$"

# A year-month or full date inside text, e.g. "Seen on 2014-01-05".
embedded_date_pattern <- "(^|[^0-9-])[0-9]{4}-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?([^0-9-]|$)"

month_names_pattern <- paste0(
  "(JAN(UARY)?|FEB(RUARY)?|MAR(CH)?|APR(IL)?|MAY|JUNE?|JULY?|AUG(UST)?|",
  "SEPT?(EMBER)?|OCT(OBER)?|NOV(EMBER)?|DEC(EMBER)?)"
)

# A date with a month name, as a whole value or inside text: "05JAN2014",
# "05-Jan-14", "05/JAN/2014", "UNK-JAN-2014", "5 January 2014",
# "5th January 2014", "Jan 5, 2014", "Jan 2014", "JAN2014", "2014JAN05". With
# spaces as separators a four-digit year is required, so "1 MAY 12 HRS" does
# not match.
month_name_date_pattern <- local({
  day <- "([0-9]{1,2}(ST|ND|RD|TH)?|UNK?|UN)"
  paste0(
    "(^|[^[:alnum:]])(",
    day, "[-/]?", month_names_pattern, "[-/]?([0-9]{4}|[0-9]{2})",
    "|", day, " ", month_names_pattern, ",? [0-9]{4}",
    "|", month_names_pattern, "\\.? [0-9]{1,2}(ST|ND|RD|TH)?,? [0-9]{4}",
    "|", month_names_pattern, "[- ]?[0-9]{4}",
    "|[0-9]{4}", month_names_pattern, "[0-9]{2}",
    ")([^[:alnum:]]|T[0-9]|$)"
  )
})

# A numeric date with separators: day, month and a two- or four-digit year
# ("01/05/2014", "1/5/14", "01.05.2014", "01-05-2014"), or a four-digit year
# first ("2014/01/05").
slash_date_pattern <- paste0(
  "(^|[^0-9/.-])(",
  "[0-9]{1,2}/[0-9]{1,2}/([0-9]{4}|[0-9]{2})",
  "|[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}",
  "|[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}",
  "|[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}",
  ")([^0-9/.-]|$)"
)

# SAS formats that display dates or date-times, or parts of a date: families
# with variants (E8601DA., YYMMDD10., NLDATE., EURDFDD10., ...), and names
# matched exactly, with an optional width, so that user-defined formats such
# as DAYGRP. or YEARCAT. don't match.
sas_date_format_pattern <- paste0(
  "^((E8601D|B8601D|IS8601D|YYMMDD|MMDDYY|DDMMYY|YYMM|MMYY|YYQ|NLDATE|NLDATM|EURDF).*",
  "|(DATE|DATETIME|DATEAMPM|MDYAMPM|DAY|MONTH|YEAR|QTRR?|MONNAME|DOWNAME|WEEKDAY|JULDAY|JULIAN|",
  "MONYY|YYMON|WEEK[UVW]|YYWEEK[UVW]|WEEKDATE|WEEKDATX|WORDDATE|WORDDATX|NENGO|MINGUO|HDATE|",
  "HEBDATE|DTDATE|DTWKDATX|DTYEAR|DTMONYY|DTYYQC)[0-9]*\\.?[0-9]*)$"
)

# Warn about datasets without the key that hold identifiers or dates, which
# pass through unchanged.
warn_unkeyed_identifiers <- function(data, keyed, key, id_cols, check_dates) {
  hits <- character(0)
  for (ds in setdiff(names(data), keyed)) {
    df <- data[[ds]]
    cols <- names(df)
    flagged <- c(
      cols[tolower(cols) %in% tolower(c(key, id_cols))],
      if (check_dates) dataset_date_vars(df, NULL, key, ds),
      if (check_dates) iso_date_columns(df, cols)
    )
    if (length(flagged) > 0) {
      hits <- c(hits, paste0(ds, " (", paste(unique(flagged), collapse = ", "), ")"))
    }
  }
  if (length(hits) > 0) {
    warning(
      "Datasets without `", key, "` were left unchanged but contain identifiers or dates: ",
      paste(hits, collapse = "; "), ".",
      call. = FALSE
    )
  }
}

# Warn about columns in keyed datasets named like `key`, `also` or `key_cols`
# in another case (such as `usubjid` beside `USUBJID`), which were not
# replaced.
warn_id_name_variants <- function(data, keyed, id_cols) {
  hits <- character(0)
  for (ds in keyed) {
    cols <- names(data[[ds]])
    hits <- c(hits, qualify(ds, cols[tolower(cols) %in% tolower(id_cols) & !cols %in% id_cols]))
  }
  if (length(hits) > 0) {
    warning(
      "These columns are named like identifier columns in another case and were not replaced: ",
      paste(hits, collapse = ", "),
      ". Add them to `also` or `key_cols`, or remove them.",
      call. = FALSE
    )
  }
}

deid_log_start <- function(step, n_subjects, key, seed, settings = NULL) {
  detail <- paste0(n_subjects, " subject(s) by `", key, "`")
  if (!is.null(settings)) {
    detail <- paste0(detail, "; ", settings)
  }
  if (!is.null(seed)) {
    detail <- paste0(detail, "; seed set, so the result is reproducible from the seed")
  }
  tibble::tibble(
    step = step, dataset = NA_character_, variable = NA_character_,
    n_changed = NA_integer_, n_removed = NA_integer_, detail = detail
  )
}

deid_log_add <- function(log, dataset, variable, detail, n_changed = NA, n_removed = 0) {
  rbind(log, tibble::tibble(
    step = log$step[1], dataset = dataset, variable = variable,
    n_changed = as.integer(n_changed), n_removed = as.integer(n_removed), detail = detail
  ))
}

deparse_list <- function(x) {
  if (length(x) == 0) "none" else paste0("c(", paste0("\"", x, "\"", collapse = ", "), ")")
}

any_numeric_key <- function(data, key) {
  any(vapply(data, function(df) is.numeric(df[[key]]), logical(1)))
}

# Sort rows by the new key so row order no longer follows the original keys.
# The order of rows within a subject, and of rows without a key, is kept.
sort_by_new_key <- function(df, idx, numbers) {
  if (nrow(df) < 2) {
    if (!tibble::is_tibble(df)) rownames(df) <- NULL
    return(df)
  }
  ord <- order(numbers[idx], seq_len(nrow(df)), na.last = TRUE)
  df <- vctrs::vec_slice(df, ord)
  if (!tibble::is_tibble(df)) {
    # Row names can hold original keys or reveal the original order.
    rownames(df) <- NULL
  }
  df
}

# One offset per subject, drawn uniformly from the whole numbers between
# min_days and max_days in either direction (0 counted once).
draw_offsets <- function(n, max_days, min_days, seed) {
  width <- max_days - min_days + 1
  total <- 2 * width - (min_days == 0)
  idx <- sample_with_seed(total, n, seed = seed)
  ifelse(
    idx <= width,
    min_days - 1 + idx,
    -(min_days - 1 + idx - width + (min_days == 0))
  )
}

deid_log_unkeyed <- function(log, data, keyed, key) {
  for (ds in setdiff(names(data), keyed)) {
    log <- deid_log_add(log, ds, NA_character_, paste0("no `", key, "` column; left unchanged"), 0, 0)
  }
  log
}
