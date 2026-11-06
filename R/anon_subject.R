#' Pseudonymize subject identifiers consistently across datasets
#'
#' Replaces each subject's key (for example `USUBJID`) with a random pseudonym
#' that is the same in every dataset, so the datasets can still be joined.
#' Related identifier columns listed in `also` (for example `SUBJID`) are
#' replaced with the pseudonym's number so they stay aligned with the key.
#'
#' @param data A data frame, or a named list of data frames linked by `key`.
#'   Datasets without the `key` column are returned unchanged.
#' @param key Name of the subject identifier column.
#' @param also Names of related identifier columns that hold one value per
#'   subject (default `"SUBJID"`). They are replaced with the pseudonym's
#'   number. Columns not present in a dataset are skipped.
#' @param prefix Prefix for character pseudonyms, followed by a zero-padded
#'   number (e.g. `"SUBJ-042"`). Numeric keys get the number itself.
#' @param keep_secrets Logical. If `TRUE`, keep the mapping from original keys
#'   to pseudonyms; read it with [anon_secrets()]. Default `FALSE`: the mapping
#'   is not kept anywhere.
#' @param seed Optional seed for the random assignment, for reproducible tests.
#'   Anyone with the seed and the original data can rebuild the mapping, so do
#'   not set it for data you share. The global random number state is restored
#'   afterwards.
#'
#' @details
#' Pseudonyms are numbered in random order, so they do not reveal the order of
#' the original keys. Column types are kept: character keys get character
#' pseudonyms, numeric keys get numbers, factors stay factors. Missing and blank
#' keys are left as they are. Values of `also` columns in rows without a key
#' cannot be mapped and are removed (set to missing), with a warning.
#'
#' Pseudonymized data remains personal data for anyone who holds the mapping or
#' can re-identify subjects by other means; measure the remaining risk with
#' [anon_risk()].
#'
#' @return An object of class `"anon_deid_step"`: a list with `data` (the
#'   transformed data, in the same form as the input), `log` (a tibble of what
#'   was changed) and `secrets` (`NULL` unless `keep_secrets = TRUE`).
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

  secrets <- if (isTRUE(keep_secrets)) {
    list(subject_ids = tibble::tibble(original = keys, pseudonym = pseudonyms))
  }
  new_deid_step(input$unwrap(data), log, secrets)
}

#' Shift each subject's dates by a random offset
#'
#' Gives every subject one random offset, in whole days, and shifts all of that
#' subject's dates in every dataset by it. Intervals within a subject (time
#' since dose, study days, visit windows) are unchanged, while calendar dates no
#' longer line up between subjects or with real dates. This is the "date
#' offsetting" technique described by EMA and Health Canada.
#'
#' @inheritParams anon_subject_ids
#' @param vars Names of date columns to shift. By default, every character
#'   column whose name ends in `DTC` (ISO 8601 dates, as in SDTM) and every Date
#'   or POSIXct column, in each dataset that has the `key`.
#' @param max_days,min_days Each subject's offset is drawn uniformly from the
#'   whole numbers between `min_days` and `max_days` days, earlier or later.
#'   Defaults 30 and 365: nobody keeps their real dates or dates within a month
#'   of them.
#' @param partial How to handle partial ISO 8601 dates (`"2014-03"`, `"2014"`),
#'   as in [anon_date_shift()]: `"shift"` moves the middle of the period and
#'   keeps its precision, `"remove"` replaces them with `""`.
#' @param keep_secrets Logical. If `TRUE`, keep each subject's offset; read it
#'   with [anon_secrets()]. Default `FALSE`.
#'
#' @details
#' ISO 8601 strings keep their format and precision, and any time part is kept.
#' Date and POSIXct columns keep their type and time of day. Study day variables
#' (`--DY`) are not changed and stay consistent with the shifted dates, because
#' each subject's dates all move together.
#'
#' Dates in rows without a key cannot be tied to a subject's offset and are
#' removed (`""` or `NA`), with a warning. Datasets without the `key` column
#' (for example trial-level domains such as `TS`) are left unchanged; the log
#' lists them.
#'
#' Run this before [anon_subject_ids()] or after it; each subject keeps one
#' offset either way. Under HIPAA, shifted dates do not meet Safe Harbor, which
#' removes all elements of dates except the year; see the package README.
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

  date_vars <- lapply(data[keyed], function(df) dataset_date_vars(df, vars, key))
  if (!is.null(vars) && length(unlist(date_vars)) == 0) {
    stop("None of `vars` were found in a dataset with `", key, "`.", call. = FALSE)
  }

  pool <- unique(c(-(max_days:min_days), min_days:max_days))
  offsets <- pool[sample_with_seed(length(pool), length(keys), seed = seed)]

  log <- deid_log_start("subject_dates", length(keys), key, seed)
  removed <- 0

  for (ds in keyed) {
    df <- data[[ds]]
    subject_offset <- offsets[match(normalize_keys(df[[key]]), keys)]

    for (col in date_vars[[ds]]) {
      shifted <- shift_dates_by_subject(df[[col]], subject_offset, partial)
      df[[col]] <- shifted$values
      removed <- removed + shifted$removed
      log <- deid_log_add(
        log, ds, col,
        paste0(shifted$shifted, " value(s) shifted",
               if (shifted$removed > 0) paste0(", ", shifted$removed, " removed (no key)") else "")
      )
    }

    data[[ds]] <- df
  }
  log <- deid_log_unkeyed(log, data, keyed, key)

  if (removed > 0) {
    warning(
      removed, " date value(s) in rows without a `", key, "` were removed.",
      call. = FALSE
    )
  }

  secrets <- if (isTRUE(keep_secrets)) {
    list(date_offsets = tibble::tibble(original = keys, offset_days = offsets))
  }
  new_deid_step(input$unwrap(data), log, secrets)
}

#' Read the secrets kept by a de-identification step
#'
#' Returns the information needed to link de-identified data back to the
#' source, such as the mapping from original keys to pseudonyms or each
#' subject's date offset. It exists only when the step was run with
#' `keep_secrets = TRUE`. Keep it separate from anything you share.
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
  cat("De-identification step:", x$step, "\n")
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

# helpers -----------------------------------------------------------------

# Secrets live in an environment so that print(), anon() and JSON output never
# show them by accident.
new_deid_step <- function(data, log, secrets) {
  secrets_env <- NULL
  if (!is.null(secrets)) {
    secrets_env <- list2env(secrets, envir = new.env(parent = emptyenv()))
    class(secrets_env) <- "anon_secrets"
  }
  structure(
    list(step = log$step[1], data = data, log = log, secrets = secrets_env),
    class = "anon_deid_step"
  )
}

as_dataset_list <- function(data) {
  if (is.data.frame(data)) {
    return(list(data = list(data = data), unwrap = function(x) x[[1]]))
  }
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "") ||
    !all(vapply(data, is.data.frame, logical(1)))) {
    stop("`data` must be a data frame or a named list of data frames.", call. = FALSE)
  }
  list(data = data, unwrap = function(x) x)
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

# Replace identifiers by subject index, keeping the column's type. Rows without
# a subject keep their value unless `remove` is TRUE for them.
replace_ids <- function(x, idx, labels, numbers, remove = rep(FALSE, length(x))) {
  has <- !is.na(idx)
  if (is.numeric(x)) {
    out <- x
    out[has] <- numbers[idx[has]]
    out[remove] <- NA
    if (is.integer(x)) out <- as.integer(out)
    return(out)
  }
  was_factor <- is.factor(x)
  out <- as.character(x)
  out[has] <- labels[idx[has]]
  out[remove] <- missing_text_like(out)
  if (was_factor) factor(out) else out
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
}

dataset_date_vars <- function(df, vars, key) {
  is_date <- vapply(df, function(col) inherits(col, c("Date", "POSIXct")), logical(1))
  is_dtc <- grepl("DTC$", names(df)) & vapply(df, is.character, logical(1))
  candidates <- names(df)[is_date | is_dtc]
  if (!is.null(vars)) {
    candidates <- intersect(vars, names(df))
  }
  setdiff(candidates, key)
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

# Shift each value by its subject's offset (days). Values without an offset are
# removed. Returns the new values and counts.
shift_dates_by_subject <- function(x, offset, partial) {
  present <- if (is.character(x)) !is.na(x) & nzchar(trimws(x)) else !is.na(x)
  no_offset <- present & is.na(offset)
  use <- ifelse(is.na(offset), 0, offset)

  if (is.character(x)) {
    out <- shift_iso8601(x, use, partial)
    out[no_offset] <- ""
  } else if (inherits(x, "POSIXct")) {
    out <- x + use * 86400
    out[no_offset] <- NA
  } else {
    out <- x + use
    out[no_offset] <- NA
  }

  list(values = out, shifted = sum(present & !no_offset), removed = sum(no_offset))
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
