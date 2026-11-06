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
#' Pseudonyms are numbered in random order, so they do not reveal the order of
#' the original keys. Column types and attributes such as labels are kept:
#' character keys get character pseudonyms, numeric keys get numbers, factors
#' stay factors. Missing and blank keys are left as they are. Values of `also`
#' columns in rows without a key cannot be mapped and are removed (set to
#' missing), with a warning.
#'
#' A warning lists any other character or factor column in a keyed dataset that
#' still contains original key values, and any dataset without `key` that has
#' an `also` or `key_cols` column or a column named like `key` in another case.
#' Identifiers embedded inside other text (for example in `--SPID`) are not
#' detected.
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
#'   transformed data, in the same form as the input), `log` (a tibble of what
#'   was changed, including earlier steps when `data` was a step result) and
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
  input <- as_dataset_list(data)
  data <- input$data
  keyed <- keyed_datasets(data, key)
  keys <- subject_keys(data[keyed], key)

  numbers <- sample_int_with_seed(length(keys), length(keys), seed = seed)
  width <- nchar(as.character(length(keys)))
  padded <- formatC(numbers, width = width, flag = "0")
  pseudonyms <- paste0(prefix, padded)

  log <- deid_log_start("subject_ids", length(keys), key, seed)
  removed_also <- 0
  removed_key_cols <- 0

  for (ds in keyed) {
    df <- data[[ds]]
    idx <- match(normalize_keys(df[[key]]), keys)

    df[[key]] <- replace_ids(df[[key]], idx, pseudonyms, numbers)
    log <- deid_log_add(log, ds, key, paste(sum(!is.na(idx)), "value(s) replaced with pseudonyms"))

    for (col in intersect(also, names(df))) {
      check_one_value_per_subject(df[[col]], idx, col)
      unmapped <- is.na(idx) & !is.na(blank_to_na(df[[col]]))
      removed_also <- removed_also + sum(unmapped)
      df[[col]] <- replace_ids(df[[col]], idx, padded, numbers, remove = unmapped)
      log <- deid_log_add(log, ds, col, paste(sum(!is.na(idx)), "value(s) replaced with the pseudonym number"))
    }

    for (col in setdiff(intersect(key_cols, names(df)), c(key, also))) {
      col_idx <- match(normalize_keys(df[[col]]), keys)
      unmapped <- is.na(col_idx) & !is.na(normalize_keys(df[[col]]))
      removed_key_cols <- removed_key_cols + sum(unmapped)
      df[[col]] <- replace_ids(df[[col]], col_idx, pseudonyms, numbers, remove = unmapped)
      log <- deid_log_add(log, ds, col, paste(sum(!is.na(col_idx)), "value(s) replaced with pseudonyms"))
    }

    data[[ds]] <- df
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
  warn_leftover_keys(data, keyed, key, keys)
  warn_unkeyed_identifiers(data, keyed, key, c(also, key_cols), check_dates = FALSE)

  secrets <- if (isTRUE(keep_secrets)) {
    list(subject_ids = tibble::tibble(key = keys, pseudonym = pseudonyms))
  }
  new_deid_step(input$unwrap(data), log, secrets, previous = input$previous)
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
#'   values that are full ISO 8601 dates are shifted too. Columns given in
#'   `vars` must be Date, POSIXct, or character or factor columns of ISO 8601
#'   dates.
#' @param max_days,min_days Each subject's offset is drawn uniformly from the
#'   whole numbers between `min_days` and `max_days` days, earlier or later.
#'   Defaults 30 and 365. A warning is given when the range is so narrow, or
#'   includes 0, that offsets are easy to guess or some subjects keep their real
#'   dates.
#' @param partial How to handle partial ISO 8601 dates in character columns:
#'   `"shift"` (default) or `"remove"` (replace them with `""`). With `"shift"`,
#'   a year-and-month date (`"2014-03"`) moves the middle of the month by the
#'   subject's offset and keeps month precision; a year-only date (`"2014"`)
#'   moves by whole years, at least one, in the direction of the offset, so the
#'   real year is never kept.
#' @param keep_secrets Logical. If `TRUE`, keep each subject's offset; read it
#'   with [anon_secrets()]. Default `FALSE`.
#'
#' @details
#' Full ISO 8601 dates move by exactly the subject's offset and keep their
#' format and any time part. Date columns keep their type, and POSIXct columns
#' keep their type, time zone and clock time (the calendar date moves, so a
#' shift across a daylight saving change keeps 08:00 at 08:00). Column
#' attributes such as labels are kept. Study day variables (`--DY`) are not
#' changed and stay consistent with the shifted full dates, because each
#' subject's dates all move together.
#'
#' Partial dates are shifted from the middle of their period, so the order
#' between a partial date and a full date of the same subject can change (for
#' example a start month can end up after an end date).
#'
#' Dates in rows without a key cannot be tied to a subject's offset and are
#' removed, with a warning. Values that are not ISO 8601 dates are removed with
#' a warning. Datasets without the `key` column (for example trial-level domains
#' such as `TS`, `TV` and `TA`) are left unchanged and listed in the log; their
#' real trial dates can narrow down subjects' offsets. A warning lists any other
#' character or factor column in a keyed dataset that contains ISO 8601 dates
#' but was not shifted, and any dataset without `key` that has date columns or a
#' column named like `key` in another case.
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
  keys <- subject_keys(data[keyed], key)

  date_vars <- lapply(keyed, function(ds) dataset_date_vars(data[[ds]], vars, key, ds))
  names(date_vars) <- keyed
  if (!is.null(vars) && length(unlist(date_vars)) == 0) {
    stop("None of `vars` were found in a dataset with `", key, "`.", call. = FALSE)
  }

  pool <- unique(c(-(max_days:min_days), min_days:max_days))
  offsets <- pool[sample_with_seed(length(pool), length(keys), seed = seed)]

  log <- deid_log_start("subject_dates", length(keys), key, seed)
  totals <- c(no_key = 0, invalid = 0)

  for (ds in keyed) {
    df <- data[[ds]]
    subject_offset <- offsets[match(normalize_keys(df[[key]]), keys)]

    for (col in date_vars[[ds]]) {
      shifted <- shift_dates_by_subject(df[[col]], subject_offset, partial)
      df[[col]] <- shifted$values
      totals <- totals + c(shifted$no_key, shifted$invalid)
      log <- deid_log_add(log, ds, col, shift_detail(shifted))
    }

    if (is.null(vars) && all(c("QNAM", "QVAL") %in% names(df))) {
      shifted <- shift_qval_dates(df$QVAL, subject_offset)
      df$QVAL <- shifted$values
      totals <- totals + c(shifted$no_key, 0)
      if (shifted$shifted + shifted$no_key > 0) {
        log <- deid_log_add(log, ds, "QVAL", paste0(shift_detail(shifted), " (ISO 8601 dates only)"))
      }
    }

    data[[ds]] <- df
  }
  log <- deid_log_unkeyed(log, data, keyed, key)

  if (totals[["no_key"]] > 0) {
    warning(totals[["no_key"]], " date value(s) in rows without a `", key, "` were removed.", call. = FALSE)
  }
  if (totals[["invalid"]] > 0) {
    warning(totals[["invalid"]], " value(s) are not ISO 8601 dates and were removed.", call. = FALSE)
  }
  warn_unshifted_iso_dates(data, keyed, date_vars, key)
  warn_unkeyed_identifiers(data, keyed, key, character(0), check_dates = TRUE)

  secrets <- if (isTRUE(keep_secrets)) {
    list(date_offsets = tibble::tibble(key = keys, offset_days = offsets))
  }
  new_deid_step(input$unwrap(data), log, secrets, previous = input$previous)
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
    stop("No secrets were kept; run the step with `keep_secrets = TRUE`.", call. = FALSE)
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
new_deid_step <- function(data, log, secrets, previous = NULL) {
  if (!is.null(previous)) {
    log <- rbind(previous$log, log)
    if (!is.null(previous$secrets)) {
      secrets <- c(mget(ls(previous$secrets), envir = previous$secrets), secrets)
    }
  }
  secrets_env <- NULL
  if (length(secrets) > 0) {
    secrets_env <- list2env(secrets, envir = new.env(parent = emptyenv()))
    class(secrets_env) <- "anon_secrets"
  }
  structure(
    list(data = data, log = log, secrets = secrets_env),
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
    return(list(data = list(data = data), unwrap = function(x) x[[1]], previous = previous))
  }
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "") ||
    !all(vapply(data, is.data.frame, logical(1)))) {
    stop("`data` must be a data frame, a named list of data frames, or a step result.", call. = FALSE)
  }
  list(data = data, unwrap = function(x) x, previous = previous)
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
    return(out)
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
  out
}

# Rebuild attributes (labels, formats, extra classes) on a new factor.
with_attributes_of <- function(new, old) {
  extra <- attributes(old)[setdiff(names(attributes(old)), c("levels", "class", "names"))]
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
  if (!whole(max_days) || max_days < 1) {
    stop("`max_days` must be a single whole number of at least 1.", call. = FALSE)
  }
  if (!whole(min_days) || min_days < 0 || min_days > max_days) {
    stop("`min_days` must be a single whole number between 0 and `max_days`.", call. = FALSE)
  }
  n_offsets <- 2 * (max_days - min_days + 1) - (min_days == 0)
  if (min_days == 0) {
    warning("`min_days = 0` lets some subjects keep their real dates.", call. = FALSE)
  } else if (n_offsets < 30) {
    warning("Only ", n_offsets, " possible offsets; offsets are easy to guess.", call. = FALSE)
  }
}

iso_full_date_pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}(T.*)?$"

dataset_date_vars <- function(df, vars, key, dataset) {
  if (!is.null(vars)) {
    selected <- setdiff(intersect(vars, names(df)), key)
    for (col in selected) {
      x <- df[[col]]
      if (!(inherits(x, c("Date", "POSIXct")) || is.character(x) || is.factor(x))) {
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
shift_dates_by_subject <- function(x, offset, partial) {
  if (is.factor(x)) {
    shifted <- shift_dates_by_subject(as.character(x), offset, partial)
    shifted$values <- with_attributes_of(factor(shifted$values), x)
    return(shifted)
  }

  present <- if (is.character(x)) !is.na(x) & nzchar(trimws(x)) else !is.na(x)
  no_offset <- present & is.na(offset)
  use <- ifelse(is.na(offset), 0, offset)
  invalid <- rep(FALSE, length(x))

  out <- x
  if (is.character(x)) {
    invalid <- present & !no_offset & parse_iso8601_dates(x)$kind == "invalid"
    out[] <- suppressWarnings(shift_iso8601(x, use, partial, year_step = TRUE))
    out[no_offset] <- missing_text_like(x)
    out[invalid] <- missing_text_like(x)
  } else if (inherits(x, "POSIXct")) {
    tz <- attr(x, "tzone", exact = TRUE)
    tz <- if (is.null(tz) || length(tz) == 0) "" else tz[[1]]
    lt <- as.POSIXlt(x, tz = tz)
    lt$mday <- lt$mday + use
    lt$isdst <- -1L
    out[] <- as.POSIXct(lt, tz = tz)
    out[no_offset] <- NA
  } else {
    out[] <- x + use
    out[no_offset] <- NA
  }

  list(
    values = out,
    shifted = sum(present & !no_offset & !invalid),
    no_key = sum(no_offset),
    invalid = sum(invalid)
  )
}

# Shift QVAL values that are full ISO 8601 dates; other values are untouched.
shift_qval_dates <- function(x, offset) {
  text <- as.character(x)
  is_date <- !is.na(text) & grepl(iso_full_date_pattern, text) &
    parse_iso8601_dates(text)$kind == "full"
  out <- x
  if (!any(is_date)) {
    return(list(values = out, shifted = 0, no_key = 0, invalid = 0))
  }
  no_offset <- is_date & is.na(offset)
  movable <- is_date & !no_offset
  new_text <- text
  new_text[movable] <- shift_iso8601(text[movable], offset[movable], "shift")
  new_text[no_offset] <- missing_text_like(text)
  if (is.factor(x)) {
    out <- with_attributes_of(factor(new_text), x)
  } else {
    out[] <- new_text
  }
  list(values = out, shifted = sum(movable), no_key = sum(no_offset), invalid = 0)
}

shift_detail <- function(shifted) {
  removed <- c(
    if (shifted$no_key > 0) paste0(shifted$no_key, " removed (no key)"),
    if (shifted$invalid > 0) paste0(shifted$invalid, " removed (not ISO 8601)")
  )
  paste(c(paste0(shifted$shifted, " value(s) shifted"), removed), collapse = ", ")
}

# Warn about text columns in keyed datasets that still contain original keys.
warn_leftover_keys <- function(data, keyed, key, keys) {
  hits <- character(0)
  for (ds in keyed) {
    df <- data[[ds]]
    for (col in setdiff(names(df), key)) {
      x <- df[[col]]
      if ((is.character(x) || is.factor(x)) && any(normalize_keys(x) %in% keys)) {
        hits <- c(hits, paste0(ds, ".", col))
      }
    }
  }
  if (length(hits) > 0) {
    warning(
      "These columns still contain original `", key, "` values: ",
      paste(hits, collapse = ", "), ". Add them to `key_cols` or remove them.",
      call. = FALSE
    )
  }
}

# Warn about text columns in keyed datasets that hold ISO 8601 dates but were
# not shifted.
warn_unshifted_iso_dates <- function(data, keyed, date_vars, key) {
  hits <- character(0)
  for (ds in keyed) {
    df <- data[[ds]]
    skip <- c(key, date_vars[[ds]], if (all(c("QNAM", "QVAL") %in% names(df))) "QVAL")
    for (col in setdiff(names(df), skip)) {
      x <- df[[col]]
      if ((is.character(x) || is.factor(x)) && any(grepl(iso_full_date_pattern, as.character(x)))) {
        hits <- c(hits, paste0(ds, ".", col))
      }
    }
  }
  if (length(hits) > 0) {
    warning(
      "These columns contain ISO 8601 dates but were not shifted: ",
      paste(hits, collapse = ", "), ". Add them to `vars` or remove them.",
      call. = FALSE
    )
  }
}

# Warn about datasets without the key that hold identifiers or dates, which
# pass through unchanged.
warn_unkeyed_identifiers <- function(data, keyed, key, id_cols, check_dates) {
  hits <- character(0)
  for (ds in setdiff(names(data), keyed)) {
    df <- data[[ds]]
    cols <- names(df)
    flagged <- c(
      intersect(id_cols, cols),
      cols[tolower(cols) == tolower(key)],
      if (check_dates) dataset_date_vars(df, NULL, key, ds)
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

deid_log_start <- function(step, n_subjects, key, seed) {
  detail <- paste0(n_subjects, " subject(s) by `", key, "`")
  if (!is.null(seed)) {
    detail <- paste0(detail, "; seed set, so the result is reproducible from the seed")
  }
  tibble::tibble(step = step, dataset = NA_character_, variable = NA_character_, detail = detail)
}

deid_log_add <- function(log, dataset, variable, detail) {
  rbind(log, tibble::tibble(step = log$step[1], dataset = dataset, variable = variable, detail = detail))
}

deid_log_unkeyed <- function(log, data, keyed, key) {
  for (ds in setdiff(names(data), keyed)) {
    log <- deid_log_add(log, ds, NA_character_, paste0("no `", key, "` column; left unchanged"))
  }
  log
}
