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
#'   the result of an earlier step such as [anon_subject_dates()]. In datasets
#'   without the `key` column the values are left unchanged; keys in their
#'   attributes and names are rewritten (see Details).
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
#' @param other_attributes What to do with attributes beyond the core ones
#'   (variable and dataset labels, comments, haven value labels and
#'   user-defined missing values, and names, which are always rewritten):
#'   custom text attributes, list- and data frame-valued attributes, Date or
#'   factor values held in attributes, and attributes of attributes.
#'   `"rewrite"` (default) rewrites what it can find in them, at any depth, as
#'   a best effort. `"drop"` removes them, and is the strict choice: nothing
#'   can hide in an attribute that is gone. Structural attributes are never
#'   dropped, and are structural only on the classes that use them: `class`,
#'   `names`, `row.names`, `dim` and `dimnames` on anything; `levels` and
#'   `contrasts` on factors; `tzone` on date-times; `units` on difftime and
#'   units objects; `tsp` on time series; `ptype` on vctrs objects (its text
#'   is still rewritten); `groups` on grouped and rowwise tibbles; `sorted`,
#'   `index` and `.internal.selfref` on data.tables; SAS, SPSS and Stata
#'   formats, and a numeric `display_width`, on vectors. Elsewhere an
#'   attribute with one of these names is text (for example free-text `units`
#'   on a number). The log counts the attributes dropped in each dataset.
#'
#' @details
#' Pseudonyms are numbered in random order, and rows of each dataset are then
#' sorted by the new key (keeping the order of rows within each subject), so
#' neither the pseudonyms nor the row order reveal the order of the original
#' keys. Column types and attributes such as variable labels are kept
#' (original keys in their text are rewritten, see below). Character keys get
#' character pseudonyms, numeric keys get numbers, factors stay factors. Value
#' labels (haven `labels`), which describe the original values, are dropped
#' from replaced columns. Missing and blank keys are left
#' as they are. Values of `also` columns in rows without a key cannot be mapped
#' and are removed (set to missing), with a warning. `key`, `also` and
#' `key_cols` must not overlap.
#'
#' Original keys in text that describes the data are replaced with the
#' subject's pseudonym, in every dataset (with or without `key`), when they
#' appear as a whole value or a whole word: in the attributes of columns, data
#' frames and the list of datasets (such as `label`, `comment` or a custom
#' note), at any depth (text in list or factor attributes, the values of data
#' frames held in attributes, names of attribute vectors, attributes of
#' attributes, attribute names); in the names of vector columns and the
#' dimnames of matrix columns; in haven value labels of columns that are not
#' replaced (the label names, and the label values of character columns) and
#' in character user-defined missing values (`na_values`, `na_range`) of those
#' columns; and in column names, character row names and dataset names.
#' Structural attributes (see `other_attributes`) are left alone, except that
#' text in a vctrs `ptype` is rewritten; so are environments, functions and S4
#' objects held in attributes (they can refer to themselves, are shared with
#' whoever else holds them, or must keep valid slots; S4 objects are named in
#' a warning). The grouping of a grouped or rowwise tibble (`groups`) and the
#' key and indices of a data.table (`sorted`, `index`) are renamed with the
#' columns. A grouped tibble held
#' in an attribute has its grouping values rewritten with its values. With
#' `other_attributes = "drop"`, attributes beyond the core ones are dropped
#' instead of rewritten. A label value or missing-value code is rewritten only
#' when no value of the column equals it or its rewrite (or, for `na_range`,
#' falls in either range), so the mapping from values to labels and which
#' values are missing don't change; a code kept this way is named in a
#' warning, as is a missing-value range whose rewritten ends would be in
#' descending order. Value labels whose codes the rewrite made equal are
#' merged into one entry, their original labels joined with `" / "` and then
#' rewritten (haven requires distinct codes; label text may repeat), and equal
#' missing-value codes are deduplicated; a warning names each. A rewritten name that clashes with another gets a numeric suffix
#' (`"_1"`), and settings and the log name the suffixed column; a column name
#' in the settings that was renamed differently in different datasets is
#' rewritten as text instead. Column and dataset names marked as `"bytes"`
#' are handled like other names and stay marked as `"bytes"`.
#' Keys made only of digits, and numeric keys, are not rewritten, for the same
#' reason they are not searched for below. The log has one row per dataset
#' with such rewrites (and one for the list of datasets), with counts by kind;
#' this and earlier steps' logs and settings are renamed and rewritten to
#' match. Values in the data are not rewritten, including the elements of list
#' columns and packed data frame columns, with everything inside them: a packed
#' column's own attributes (even its `label`) and names are left as they are,
#' and handling nested data is planned.
#'
#' A warning lists any other character or factor column (values or factor
#' levels), list column (all the text its elements hold: values, factor
#' levels, names and attributes, at any depth), or column of a packed data
#' frame column (named as `dataset.column[subcolumn]`, and deeper as
#' `dataset.column[sub][subsub]`, with the text in its attributes; the packed
#' column itself is named for its column names and attributes), in any
#' dataset, that still contains original key values as whole
#' values or as whole words inside text (checked for character keys that are
#' not made only of digits, since numbers match many unrelated codes), and
#' any dataset without `key` that has a column named like `key`, `also` or
#' `key_cols` in any case. In datasets with `key`, a column named like one of
#' them in another case (such as `usubjid` beside `USUBJID`) is not replaced
#' and is named in a warning. Only text or factor columns can be given in
#' `key_cols`; the others must be redacted or removed. Locations printed in
#' warnings have keys rewritten, as names inside packed columns can hold them.
#' Columns of S4 objects are not searched or changed, and are named in a
#' warning as data that can't be checked.
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
  seed = NULL,
  other_attributes = c("rewrite", "drop")
) {
  other_attributes <- match.arg(other_attributes)
  if (key %in% also || length(intersect(also, key_cols)) > 0) {
    stop("`key`, `also` and `key_cols` must not overlap.", call. = FALSE)
  }
  key_cols <- setdiff(key_cols, key)

  input <- as_dataset_list(data)
  data <- input$data
  local_text_cache()
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
  # Keys made only of digits, and numeric keys, match too much to be searched
  # for in text; other attributes are still dropped when asked.
  searched <- !any_numeric_key(data[keyed], key) && length(searchable_keys(keys)) > 0
  rewrite <- if (searched) key_rewriter(keys, pseudonyms) else identity
  rewritten <- rewrite_described_text(
    data, rewrite, log, "original keys replaced with pseudonyms", other = other_attributes
  )
  data <- rewritten$data
  log <- rewritten$log
  keyed <- rewritten$rename(keyed)
  input$previous <- rewrite_previous_step(input$previous, rewritten)
  log <- deid_log_unkeyed(log, data, keyed, key)
  # Warnings print locations, whose names can hold keys and dates (which this
  # step doesn't rewrite in data but must not print).
  locate <- function(x) format_locations(x, function(text) date_rewriter(rewrite(text)))

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
    warn_leftover_keys(data, keyed, key, keys, skip = c(also, key_cols), locate = locate)
  }
  warn_id_name_variants(data, keyed, c(key, also, key_cols), locate = locate)
  warn_unkeyed_identifiers(data, keyed, key, c(also, key_cols), check_dates = FALSE, locate = locate)
  warn_merged_codes(rewritten$merged, rewritten$deduplicated, locate = locate)
  warn_s4_columns(data, paste0("original `", key, "` values"), rewritten$s4, locate = locate)
  if (searched) {
    warn_kept_codes(rewritten$kept, paste0("original `", key, "` values"), locate = locate)
    warn_invalid_utf8(data, rewritten$invalid, locate = locate)
  }

  secrets <- if (isTRUE(keep_secrets)) {
    new_ids <- if (any_numeric_key(data[keyed], key)) numbers else pseudonyms
    list(subject_ids = tibble::tibble(key = keys, pseudonym = new_ids))
  }
  settings <- list(subject_ids = list(
    key = key, also = also, key_cols = key_cols, prefix = prefix,
    keep_secrets = isTRUE(keep_secrets), seeded = !is.null(seed), other_attributes = other_attributes
  ))
  settings <- rewritten$settings(settings)
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
#' attributes such as variable labels are kept, with the dates in them
#' replaced by `"[DATE]"` (see below); value labels (haven `labels`) are
#' dropped from shifted columns, and haven user-defined missing values
#' (`na_values`, `na_range`) in the key and date columns become plain missing
#' values. A clock time that doesn't exist on the new date (inside a daylight
#' saving gap) moves forward by the gap. Study day variables (`--DY`) are not
#' changed and stay consistent with the shifted full dates, because each
#' subject's dates all move together.
#'
#' Dates can't be tied to one subject's offset in text that describes the
#' data, so there they are replaced with `"[DATE]"` (the default date
#' replacement of [nlp_default_replacements()]): in every dataset, in the same
#' attributes and names that [anon_subject_ids()] rewrites keys in (character
#' attributes at any depth, vector names and dimnames, value labels and
#' user-defined missing values under the same rule, column, row and dataset
#' names), and in this and earlier steps' logs and settings, with
#' `other_attributes` as described there. Date and date-time values held in
#' attributes (Date, POSIXct, POSIXlt) become the text `"[DATE]"` (zero-length
#' ones, such as a vctrs prototype, hold no date and are left alone), and a
#' factor held in an attribute whose levels now read the same becomes text
#' rather than have its levels merged. Value labels whose codes become equal
#' are merged as described there. The forms
#' replaced are those found inside text by the warning described below: ISO
#' 8601 full and year-and-month dates, SDTM partial dates (`"2014-01--T08:00"`
#' becomes `"[DATE]T08:00"`, and `"2014---15"`), and dates with a month name or
#' as numbers with separators. The log has one row per dataset with such
#' replacements, with counts. Values in the data are not changed by this
#' rule.
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
#' are kept. Text columns given in `vars` must contain at least one recognised
#' date. In datasets without the `key` column (for example trial-level domains
#' such as `TS`, `TV` and `TA`) the values are left unchanged (dates in their
#' attributes and names are replaced as above), and they are listed in the
#' log; their real trial dates can narrow down subjects' offsets. A warning
#' lists any other
#' column in a keyed dataset that holds dates but was not shifted: Date or
#' POSIXct columns left out of `vars`, numeric columns with a SAS date format
#' (in a separate warning; convert them to Date or POSIXct to shift them), and
#' text columns with ISO 8601 dates as whole values, or dates inside text
#' (which can't be shifted) written as ISO 8601, with a month name
#' (`05JAN2014`, `UNK-JAN-2014`, `5 Jan 2014`, `Jan 5, 2014`, `JAN2014`) or as
#' numbers with separators (`01/05/2014`, `1/5/14`, `01.05.2014`,
#' `2014/01/05`), including the values kept in mixed columns such as `QVAL`,
#' list columns (all the text their elements hold) and the columns of packed
#' data frame columns. Other formats, such as `20140105`, `2014.01.05`,
#' `2014-1-5`, `Jan-05-2014`, `12/2014` and a month and day without a year
#' (`--12-15`), are not detected, nor replaced in attributes and names. It also
#' lists any dataset without `key` that has such columns or a column named
#' like `key` in another case.
#'
#' Both steps warn about text that can't be searched: text that isn't valid
#' UTF-8 (after converting text marked with another encoding, such as latin1)
#' or is marked as `"bytes"`. They count distinct texts and name the columns,
#' or datasets (`"(list)"` for attributes of the list of datasets and dataset
#' names), holding it. Convert it to UTF-8 (for example with [iconv()]) and
#' run the step again. Every location printed in the steps' warnings has
#' keys and dates rewritten and unprintable bytes escaped (`"<e9>"`): this
#' step shows original keys as `"[KEY]"`. When [anon_subject_ids()] ran
#' first, this step can't recognise original keys left in names inside packed
#' columns (data, which that step doesn't rewrite), so it shows the columns
#' inside packed columns by position (`dataset.column[2]`).
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
  seed = NULL,
  other_attributes = c("rewrite", "drop")
) {
  partial <- match.arg(partial)
  other_attributes <- match.arg(other_attributes)
  validate_offset_range(max_days, min_days)

  input <- as_dataset_list(data)
  data <- input$data
  # When the IDs step ran first, the keys here are pseudonyms, and original
  # keys left in names inside packed columns (data, which that step doesn't
  # rewrite) can't be recognised: such columns are named by position.
  ids_done <- !is.null(input$previous) && any(startsWith(names(input$previous$settings), "subject_ids"))
  local_text_cache(positional = ids_done)
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
  mixed_cols <- unique(grep("DTC$", unlist(date_vars), ignore.case = TRUE, value = TRUE, invert = TRUE, useBytes = TRUE))
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
  rewritten <- rewrite_described_text(
    data, date_rewriter, log, "dates replaced with [DATE]", dates = TRUE, other = other_attributes
  )
  data <- rewritten$data
  log <- rewritten$log
  names(date_vars) <- rewritten$rename(names(date_vars))
  for (ds in names(date_vars)) {
    date_vars[[ds]] <- rewritten$rename_columns(date_vars[[ds]], ds)
  }
  keyed <- rewritten$rename(keyed)
  input$previous <- rewrite_previous_step(input$previous, rewritten)
  log <- deid_log_unkeyed(log, data, keyed, key)
  # Warnings print locations, whose names can hold dates and, unless the IDs
  # step already ran, original keys: these are shown as "[KEY]".
  mask_keys <- if (ids_done) identity else key_rewriter(keys, rep("[KEY]", length(keys)))
  locate <- function(x) format_locations(x, function(text) date_rewriter(mask_keys(text)))

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
      "also holds other values: ", paste(locate(kept_years), collapse = ", "),
      ". If they are years, they still show real years; redact or remove them.",
      call. = FALSE
    )
  }
  if (length(kept_qval_years) > 0) {
    warning(
      "These QVAL columns hold bare four-digit numbers, which are never shifted because they can't ",
      "be told apart from other numbers: ", paste(locate(kept_qval_years), collapse = ", "),
      ". If some are years (such as a birth year), redact or remove them.",
      call. = FALSE
    )
  }
  warn_unshifted_iso_dates(data, keyed, date_vars, key, qval_processed = is.null(vars), locate = locate)
  warn_unkeyed_identifiers(data, keyed, key, character(0), check_dates = TRUE, locate = locate)
  warn_kept_codes(rewritten$kept, "dates", locate = locate)
  warn_merged_codes(rewritten$merged, rewritten$deduplicated, locate = locate)
  warn_s4_columns(data, "dates", rewritten$s4, locate = locate)
  warn_invalid_utf8(data, rewritten$invalid, locate = locate)

  secrets <- if (isTRUE(keep_secrets)) {
    list(date_offsets = tibble::tibble(key = keys, offset_days = offsets))
  }
  settings <- list(subject_dates = list(
    key = key, vars = vars, max_days = max_days, min_days = min_days, partial = partial,
    keep_secrets = isTRUE(keep_secrets), seeded = !is.null(seed), other_attributes = other_attributes
  ))
  settings <- rewritten$settings(settings)
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
    unmarked <- unmark_bytes_names(list(data = data))
    check_unique_columns(unmarked$data)
    return(list(
      data = unmarked$data, unwrap = function(x) unmarked$restore(x)[[1]], previous = previous
    ))
  }
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "") ||
    !all(vapply(data, is.data.frame, logical(1)))) {
    stop("`data` must be a data frame, a named list of data frames, or a step result.", call. = FALSE)
  }
  unmarked <- unmark_bytes_names(data)
  data <- unmarked$data
  if (anyDuplicated(names(data))) {
    stop("Dataset names in `data` must be unique.", call. = FALSE)
  }
  check_unique_columns(data)
  list(data = data, unwrap = unmarked$restore, previous = previous)
}

# R can't look up or translate names marked "bytes", so a data frame with such
# a column name can't be worked on by name. While a step runs, column and
# dataset names marked "bytes" are marked "unknown" (they are then searched
# if they are valid UTF-8, and reported as text that can't be searched if
# not); `restore` marks them "bytes" again, by position, in the result.
unmark_bytes_names <- function(data) {
  bytes_at <- function(x) if (is.null(x)) integer(0) else which(Encoding(x) == "bytes")
  set_encoding <- function(x, at, encoding) {
    Encoding(x)[at] <- encoding
    x
  }
  columns <- lapply(data, function(df) bytes_at(names(df)))
  groups <- lapply(data, function(df) bytes_at(names(attr(df, "groups", exact = TRUE))))
  datasets <- bytes_at(names(data))
  if (length(datasets) == 0 && all(lengths(columns) == 0) && all(lengths(groups) == 0)) {
    return(list(data = data, restore = function(x) x))
  }
  # The grouping of a grouped tibble names its columns too.
  mark_groups <- function(df, at, encoding) {
    if (length(at) > 0) {
      g <- attr(df, "groups", exact = TRUE)
      attr(g, "names") <- set_encoding(names(g), at, encoding)
      attr(df, "groups") <- g
    }
    df
  }
  for (i in seq_along(data)) {
    if (length(columns[[i]]) > 0) {
      attr(data[[i]], "names") <- set_encoding(names(data[[i]]), columns[[i]], "unknown")
    }
    data[[i]] <- mark_groups(data[[i]], groups[[i]], "unknown")
  }
  if (length(datasets) > 0) {
    names(data) <- set_encoding(names(data), datasets, "unknown")
  }
  restore <- function(x) {
    for (i in seq_along(x)) {
      if (length(columns[[i]]) > 0) {
        attr(x[[i]], "names") <- set_encoding(names(x[[i]]), columns[[i]], "bytes")
      }
      x[[i]] <- mark_groups(x[[i]], groups[[i]], "bytes")
    }
    if (length(datasets) > 0) {
      names(x) <- set_encoding(names(x), datasets, "bytes")
    }
    x
  }
  list(data = data, restore = restore)
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
  # useBytes: a name marked "bytes" can't be translated.
  is_dtc <- grepl("DTC$", names(df), ignore.case = TRUE, useBytes = TRUE) & is_text
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
# warn_id_name_variants() or warn_unkeyed_identifiers() names them. Keys made
# only of digits are not searched for, as they match many unrelated numbers.
# List columns and packed data frame columns are searched too, in all the text
# they hold (see column_leaves()). `locate` rewrites the printed locations,
# which can hold names inside packed columns.
warn_leftover_keys <- function(data, keyed, key, keys, skip = character(0), locate = identity) {
  keys <- searchable_keys(keys)
  if (length(keys) == 0) {
    return(invisible())
  }
  key_pattern <- key_text_regex(keys)
  hits <- character(0)
  nested <- FALSE
  for (ds in names(data)) {
    for (leaf in dataset_leaves(data[[ds]])) {
      if (name_key(leaf$top) %in% name_key(c(key, skip))) {
        next
      }
      values <- valid_utf8_text(leaf$text)
      if (length(values) > 0 && any(stringi::stri_detect_regex(values, key_pattern), na.rm = TRUE)) {
        hits <- c(hits, paste0(ds, ".", leaf$path))
        nested <- nested || isTRUE(leaf$nested)
      }
    }
  }
  if (length(hits) > 0) {
    advice <- if (nested) {
      paste0(
        ". Add text or factor columns to `key_cols` if they hold only keys; redact or remove the others, ",
        "including list columns, packed data frame columns and the columns inside them (shown as `ds.col[sub]`)."
      )
    } else {
      ". Add them to `key_cols` if they hold only keys; otherwise redact or remove them."
    }
    warning(
      "These columns still contain original `", key, "` values, possibly inside text: ",
      paste(locate(hits), collapse = ", "), advice,
      call. = FALSE
    )
  }
}

# Column names compared in any case. Names that can't be searched (not valid
# UTF-8, or marked "bytes") are compared as they are; the warning about
# unsearchable text names them.
name_key <- function(x) {
  x <- as_utf8(x)
  ok <- searchable_text(x)
  x[ok] <- tolower(x[ok])
  x
}

# Keys that are searched for in text: valid UTF-8 and not made only of digits,
# since numbers match many unrelated codes.
searchable_keys <- function(keys) {
  keys <- valid_utf8_text(keys)
  keys[!grepl("^[0-9]+$", keys)]
}

# An ICU regular expression matching any of `keys` as a whole value or a whole
# word inside text.
key_text_regex <- function(keys) {
  # Longest first, so that a key that starts another ("S-1" and "S-1-2") doesn't
  # match inside it.
  keys <- keys[order(-nchar(keys))]
  paste0(whole_value_prefix, "(?:", paste(escape_regex_chars(keys), collapse = "|"), ")", whole_value_suffix)
}

# Rewriting text that describes the data ----------------------------------

# The attributes of `x` that hold structure rather than text about the data,
# by the classes that use them: on any other object, an attribute with one of
# these names is ordinary text (a label or comment, or another attribute).
# Used by the steps, which never rewrite or drop them (except as noted), and
# by anon(), which doesn't redact them; the test oracle keeps its own copy.
# - always: `class`, `names`, `row.names`, `dim`, `dimnames` (names and
#   dimnames are rewritten as names, never dropped);
# - factors: `levels` (a column's values; the levels of a factor held in an
#   attribute are rewritten as text) and `contrasts`;
# - date-times: `tzone`; difftime and units objects: `units`; time series:
#   `tsp`;
# - vctrs objects: `ptype` (never dropped, but its content is rewritten);
# - grouped and rowwise tibbles: `groups`; data.tables: `sorted`, `index`
#   (column references, renamed with the columns) and `.internal.selfref`;
# - atomic vectors: SAS, SPSS and Stata formats, and a numeric
#   `display_width` (format names can't hold text about the data).
structural_attributes <- function(x) {
  out <- c("class", "names", "row.names", "dim", "dimnames")
  if (is.factor(x)) out <- c(out, "levels", "contrasts")
  if (inherits(x, "POSIXt")) out <- c(out, "tzone")
  if (inherits(x, c("difftime", "units"))) out <- c(out, "units")
  if (inherits(x, "ts")) out <- c(out, "tsp")
  if (inherits(x, c("vctrs_list_of", "vctrs_vctr"))) out <- c(out, "ptype")
  if (inherits(x, c("grouped_df", "rowwise_df"))) out <- c(out, "groups")
  if (inherits(x, "data.table")) out <- c(out, "sorted", "index", ".internal.selfref")
  if (is.atomic(x)) {
    out <- c(out, "format.sas", "informat.sas", "format.spss", "format.stata")
    if (is.numeric(attr(x, "display_width", exact = TRUE))) out <- c(out, "display_width")
  }
  out
}

# Attributes that are always rewritten, whatever `other_attributes` says:
# variable and dataset labels, comments, and haven value labels and
# user-defined missing values. Names and dimnames are rewritten too, and are
# never dropped.
core_attributes <- c("label", "comment", "labels", "na_values", "na_range")

# Whether attribute `name` of data frame `x` refers to its columns by name:
# the grouping of a grouped or rowwise tibble (dplyr), or the key and indices
# of a data.table. When columns are renamed, these are renamed with them;
# their values mirror the columns. On other objects, attributes with these
# names are ordinary attributes.
is_column_reference <- function(x, name) {
  (name == "groups" && inherits(x, c("grouped_df", "rowwise_df"))) ||
    (name %in% c("sorted", "index") && inherits(x, "data.table"))
}

# A function that replaces each of `keys` found as a whole value or a whole
# word in text with that subject's pseudonym, in one pass. Keys made only of
# digits are left alone, as in warn_leftover_keys().
key_rewriter <- function(keys, pseudonyms) {
  searchable <- searchable_keys(keys)
  if (length(searchable) == 0) {
    return(identity)
  }
  lookup <- stats::setNames(pseudonyms[match(searchable, as_utf8(keys))], searchable)
  pattern <- key_text_regex(searchable)
  function(x) {
    text <- as_utf8(x)
    hit <- which(searchable_text(text))
    hit <- hit[stringi::stri_detect_regex(text[hit], pattern)]
    for (i in hit) {
      at <- stringi::stri_locate_all_regex(text[i], pattern)[[1]]
      for (j in rev(seq_len(nrow(at)))) {
        stringi::stri_sub(text[i], at[j, 1], at[j, 2]) <- lookup[[stringi::stri_sub(text[i], at[j, 1], at[j, 2])]]
      }
      x[i] <- text[i]
    }
    x
  }
}

# Replaces each date that has_dates_in_text() would find, and SDTM partial
# dates inside text, with "[DATE]", the package's default replacement for
# dates.
date_rewriter <- function(x) {
  text <- as_utf8(x)
  hit <- which(searchable_text(text))
  # Every date form has a digit; looking for one first is much faster than the
  # case-insensitive search.
  hit <- hit[grepl("[0-9]", text[hit])]
  opts <- stringi::stri_opts_regex(case_insensitive = TRUE)
  hit <- hit[stringi::stri_detect_regex(text[hit], date_in_text_regex, opts_regex = opts)]
  x[hit] <- stringi::stri_replace_all_regex(
    text[hit], date_in_text_regex, nlp_default_replacements()$date, opts_regex = opts
  )
  x
}

# Apply `rewrite` (text in, text out) to the text that describes the data
# rather than its values, in every dataset (see rewrite_frame()), and to the
# attributes of the list of datasets and the dataset names. With `dates`,
# Date and date-time values held in attributes become "[DATE]" too. A
# rewritten name that clashes with another is made unique with a numeric
# suffix. Adds one row per changed dataset to `log`, with counts, renames
# datasets and columns in it and rewrites its details. Returns the data, the
# log, functions that rename datasets and columns in another step's log and
# rewrite settings, the number of texts that can't be searched (not valid
# UTF-8) by location, and the columns whose value-label or missing-value codes
# were kept (see rewrite_codes()).
rewrite_described_text <- function(data, rewrite, log, detail, dates = FALSE, other = "rewrite") {
  # Every text is rewritten in one call, so that regular expressions are
  # compiled once and repeated text is rewritten once.
  seen <- list()
  rewrite_datasets(new_rewrite_context(function(x) {
    seen[[length(seen) + 1]] <<- as.character(x)
    x
  }, dates, other), data)
  texts <- unique(as_utf8(unlist(seen, use.names = FALSE)))
  texts <- texts[searchable_text(texts)]
  new_texts <- rewrite(texts)
  changed <- new_texts != texts
  texts <- texts[changed]
  new_texts <- new_texts[changed]
  lookup <- function(x) {
    text <- as_utf8(x)
    ok <- which(searchable_text(text))
    i <- match(text[ok], texts)
    hit <- ok[!is.na(i)]
    x[hit] <- new_texts[i[!is.na(i)]]
    x
  }

  ctx <- new_rewrite_context(lookup, dates, other)
  ctx$relabel <- rewrite
  out <- rewrite_datasets(ctx, data)
  rename <- function(ds) {
    known <- !is.na(ds) & ds %in% out$old
    ds[known] <- out$new[match(ds[known], out$old)]
    ds
  }
  rename_columns <- function(cols, ds) {
    map <- out$columns[[ds]]
    known <- !is.na(cols) & cols %in% names(map)
    cols[known] <- unname(map[cols[known]])
    cols
  }
  # Column names in settings apply to every dataset: a name renamed the same
  # way wherever it appears takes its new name; anything else is rewritten as
  # text.
  renamed_old <- unlist(lapply(out$columns, function(map) names(map)[names(map) != map]), use.names = FALSE)
  renamed_new <- unlist(lapply(out$columns, function(map) unname(map[names(map) != map])), use.names = FALSE)
  same <- vapply(split(renamed_new, renamed_old), function(v) length(unique(v)) == 1, logical(1))
  first <- !duplicated(renamed_old) & renamed_old %in% names(same)[same]
  setting_map <- stats::setNames(renamed_new[first], renamed_old[first])
  rename_settings_text <- function(x) {
    known <- !is.na(x) & x %in% names(setting_map)
    x[known] <- unname(setting_map[x[known]])
    rewrite(x)
  }
  rename_detail <- function(x) {
    for (old in names(setting_map)) {
      x <- gsub(paste0("\"", old, "\""), paste0("\"", setting_map[[old]], "\""), x, fixed = TRUE)
    }
    rewrite(x)
  }
  rename_log <- function(log) {
    for (r in which(!is.na(log$dataset))) {
      log$dataset[r] <- rename(log$dataset[r])
      log$variable[r] <- rename_columns(log$variable[r], log$dataset[r])
    }
    log$detail <- rename_detail(log$detail)
    log
  }
  log <- rename_log(log)
  kinds <- c(attributes = "attribute value(s)", value_labels = "value label(s) or missing-value code(s)", names = "name(s)")
  add_row <- function(log, ds, counts) {
    rewritten <- counts[names(kinds)]
    if (sum(rewritten) > 0) {
      shown <- rewritten > 0
      log <- deid_log_add(
        log, ds, NA_character_,
        paste0(detail, " in attributes and names: ", paste(rewritten[shown], kinds[shown], collapse = ", ")),
        sum(rewritten)
      )
    }
    if (counts[["dropped"]] > 0) {
      log <- deid_log_add(log, ds, NA_character_, paste0(counts[["dropped"]], " other attribute(s) dropped"), 0, counts[["dropped"]])
    }
    log
  }
  for (ds in names(out$counts)) {
    log <- add_row(log, ds, out$counts[[ds]])
  }
  log <- add_row(log, NA_character_, out$list_counts)
  list(
    data = out$data, log = log, rename = rename, rename_columns = rename_columns, rename_log = rename_log,
    settings = function(settings) rapply(settings, rename_settings_text, classes = "character", how = "replace"),
    invalid = ctx$invalid, kept = unique(ctx$kept), merged = unique(ctx$merged),
    deduplicated = unique(ctx$deduplicated), s4 = unique(ctx$s4)
  )
}

# Earlier steps' logs and settings name the same datasets and columns, so they
# are renamed and rewritten the same way.
rewrite_previous_step <- function(previous, rewritten) {
  if (!is.null(previous)) {
    previous$log <- rewritten$rename_log(previous$log)
    previous$settings <- rewritten$settings(previous$settings)
  }
  previous
}

# Rewrites text through `rewrite`, counting changes by kind ("attributes",
# "value_labels" or "names") and other attributes dropped (with `other =
# "drop"`), and recording the texts that can't be searched by `where` (the
# location being rewritten: "dataset.column", "dataset", or "(list)" for the
# list of datasets) and where codes were kept or merged. `relabel` rewrites the
# labels joined when codes are merged.
new_rewrite_context <- function(rewrite, dates = FALSE, other = "rewrite") {
  ctx <- new.env(parent = emptyenv())
  ctx$rewrite <- rewrite
  ctx$dates <- dates
  ctx$other <- other
  ctx$counts <- c(attributes = 0, value_labels = 0, names = 0, dropped = 0)
  ctx$invalid <- list()
  ctx$kept <- character(0)
  ctx$merged <- character(0)
  ctx$deduplicated <- character(0)
  ctx$s4 <- character(0)
  ctx$relabel <- identity
  ctx$where <- "(list)"
  ctx
}

# Rewrite text, counting the changes as `kind` (not counted when NULL).
rewrite_text <- function(ctx, x, kind) {
  if (!is.character(x) || length(x) == 0) {
    return(x)
  }
  bad <- unsearchable_texts(x)
  if (length(bad) > 0) {
    ctx$invalid[[ctx$where]] <- unique(c(ctx$invalid[[ctx$where]], bad))
  }
  new <- ctx$rewrite(x)
  if (!is.null(kind)) {
    ctx$counts[[kind]] <- ctx$counts[[kind]] + sum(!is.na(x) & x != new)
  }
  new
}

# The data with every dataset rewritten by rewrite_frame(), then the
# attributes of the list and the dataset names. Returns the data, the old and
# new dataset names, each dataset's map from old to new column names, and the
# counts of changes by dataset and on the list.
rewrite_datasets <- function(ctx, data) {
  old <- names(data)
  ctx$where <- "(list)"
  new <- unique_rewritten(rewrite_text(ctx, old, NULL), old)
  counts <- list()
  columns <- list()
  for (i in seq_along(data)) {
    ctx$counts[] <- 0
    ctx$where <- new[i]
    df <- rewrite_frame(ctx, data[[i]])
    columns[[new[i]]] <- stats::setNames(names(df), names(data[[i]]))
    data[[i]] <- df
    counts[[new[i]]] <- ctx$counts + c(0, 0, old[i] != new[i], 0)
  }
  ctx$counts[] <- 0
  ctx$where <- "(list)"
  data <- rewrite_attributes(ctx, data, frame = TRUE)
  names(data) <- new
  list(data = data, old = old, new = new, columns = columns, counts = counts, list_counts = ctx$counts)
}

# Rewrite a data frame's text: the attributes of each column, column names,
# character row names, and the data frame's own attributes (renaming the
# columns in those that refer to them). Values are data and are not rewritten,
# nor are list-column elements or packed data frame columns (warnings name
# what they hold), unless `values` is TRUE: a data frame held in an attribute
# is text about the data, so its character values and factor levels are
# rewritten too. Attributes are set directly, so that methods such as those of
# grouped tibbles don't drop the others.
rewrite_frame <- function(ctx, df, values = FALSE) {
  where <- ctx$where
  old_cols <- names(df)
  new_cols <- unique_rewritten(rewrite_text(ctx, old_cols, "names"), old_cols)
  df <- rewrite_attributes(ctx, df, frame = TRUE, columns = stats::setNames(new_cols, old_cols), values = values)
  cls <- oldClass(df)
  x <- unclass(df)
  for (j in seq_along(x)) {
    ctx$where <- paste0(where, ".", new_cols[j])
    column <- x[[j]]
    new <- if (values) {
      rewrite_value(ctx, column)
    } else if (is.data.frame(column) || isS4(column)) {
      column
    } else {
      rewrite_attributes(ctx, column)
    }
    if (!identical(new, column)) {
      x[[j]] <- new
    }
  }
  ctx$where <- where
  if (!identical(new_cols, old_cols)) {
    attr(x, "names") <- new_cols
  }
  row_names <- attr(x, "row.names")
  if (is.character(row_names)) {
    new_rows <- unique_rewritten(rewrite_text(ctx, row_names, "names"), row_names)
    if (!identical(new_rows, row_names)) {
      attr(x, "row.names") <- new_rows
    }
  }
  class(x) <- cls
  # When nothing changed, the data frame itself is returned, so that what
  # refers to it (such as a data.table's .internal.selfref) stays valid.
  if (identical(x, df)) df else x
}

# Rewrite the attributes of `x` (a column, a data frame, the list of datasets,
# or a value held in an attribute): names and dimnames, value labels and
# user-defined missing values, labels and comments, and every other attribute
# that isn't structural (at any depth, see rewrite_value()), and the attribute
# names themselves. With `ctx$other` "drop", those other attributes are
# dropped instead and counted. With `frame`, `names` are column or dataset
# names, left to the caller, and `columns` maps the data frame's old column
# names to new ones, for the attributes that refer to columns; with `values`
# (a data frame held in an attribute), the values of a grouped tibble's
# `groups` are rewritten too. Environments, functions and S4 objects are left
# alone: they can refer to themselves, changing their attributes would change
# them for every holder, and an S4 object's slots must stay valid.
rewrite_attributes <- function(ctx, x, frame = FALSE, columns = NULL, values = FALSE) {
  attrs <- attributes(x)
  if (length(attrs) == 0 || is.environment(x) || is.function(x) || isS4(x)) {
    return(x)
  }
  structural <- structural_attributes(x)
  codes <- c("labels", "na_values", "na_range")
  column_values <- NULL
  for (name in names(attrs)) {
    value <- attrs[[name]]
    new <- if (name == "names") {
      if (frame) value else rewrite_text(ctx, value, "names")
    } else if (name == "dimnames") {
      rewrite_dimnames(ctx, value)
    } else if (frame && is_column_reference(x, name)) {
      refs <- if (is.null(columns)) value else rename_column_references(name, value, columns)
      if (values && name == "groups") rewrite_group_values(ctx, refs) else refs
    } else if (name == "ptype" && name %in% structural) {
      # A vctrs prototype is kept, but text in it (such as factor levels) is
      # rewritten like any value held in an attribute.
      rewrite_value(ctx, value)
    } else if (name %in% structural) {
      value
    } else if (name %in% codes && !frame) {
      if (is.null(column_values)) column_values <- underlying_text(x)
      rewrite_codes(ctx, name, value, column_values)
    } else if (ctx$other == "drop" && !name %in% core_attributes) {
      ctx$counts[["dropped"]] <- ctx$counts[["dropped"]] + 1
      NULL
    } else {
      if (isS4(value)) {
        ctx$s4 <- c(ctx$s4, ctx$where)
      }
      rewrite_value(ctx, value)
    }
    if (!identical(new, value)) {
      attr(x, name) <- new
    }
  }
  old <- names(attributes(x))
  new_names <- rewrite_text(ctx, old, "names")
  if (!identical(new_names, old)) {
    new_names <- unique_rewritten(new_names, old)
    for (k in which(new_names != old)) {
      value <- attr(x, old[k], exact = TRUE)
      attr(x, old[k]) <- NULL
      attr(x, new_names[k]) <- value
    }
  }
  x
}

# The `groups` of a grouped tibble held in an attribute, with the values of
# its grouping columns rewritten like the tibble's own values (not `.rows`).
rewrite_group_values <- function(ctx, groups) {
  if (!is.data.frame(groups)) {
    return(groups)
  }
  cls <- oldClass(groups)
  g <- unclass(groups)
  for (j in which(names(g) != ".rows")) {
    g[[j]] <- rewrite_value(ctx, g[[j]])
  }
  class(g) <- cls
  g
}

# A data frame attribute that refers to columns by name (see
# is_column_reference()), with the columns renamed: the columns of `groups`,
# the names in `sorted`, and the "__col1__col2" names of the indices held in
# `index`, read through the column names (which may hold "__" themselves).
rename_column_references <- function(name, value, columns) {
  rename <- function(cols) {
    known <- !is.na(cols) & cols %in% names(columns)
    cols[known] <- unname(columns[cols[known]])
    cols
  }
  if (name == "groups" && is.data.frame(value)) {
    attr(value, "names") <- rename(names(value))
  } else if (name == "sorted" && is.character(value)) {
    value <- rename(value)
  } else if (name == "index" && !is.null(attributes(value))) {
    indices <- names(attributes(value))
    for (k in seq_along(indices)) {
      parts <- split_index_name(indices[k], names(columns))
      if (is.null(parts)) {
        # Not an index of known columns: left as it is.
        next
      }
      new <- paste0("__", paste(rename(parts), collapse = "__"))
      if (new != indices[k]) {
        attrs <- attributes(value)
        names(attrs)[names(attrs) == indices[k]] <- new
        attributes(value) <- attrs
      }
    }
  }
  value
}

# The columns named by a data.table index name ("__a__b"), matched against the
# column names `cols` so that a name holding "__" is read whole; NULL when the
# name isn't made of them.
split_index_name <- function(index, cols) {
  if (!startsWith(index, "__")) {
    return(NULL)
  }
  rest <- substring(index, 3)
  if (!nzchar(rest)) {
    return(character(0))
  }
  for (col in cols[order(-nchar(cols))]) {
    if (identical(rest, col)) {
      return(col)
    }
    if (startsWith(rest, paste0(col, "__"))) {
      tail <- split_index_name(substring(rest, nchar(col) + 1), cols)
      if (!is.null(tail)) {
        return(c(col, tail))
      }
    }
  }
  NULL
}

# Rewrite a value held in an attribute: its text (character values, factor
# levels, list elements at any depth, data frames as in rewrite_frame() with
# their values) and its own attributes. With `ctx$dates`, Date and date-time
# values become the text "[DATE]" (zero-length ones are left alone).
# Environments, functions, S4 objects and other objects that aren't vectors
# are left alone.
rewrite_value <- function(ctx, v) {
  if (!is.atomic(v) && !is.list(v) || is.null(v) || isS4(v)) {
    return(v)
  }
  if (ctx$dates && inherits(v, c("Date", "POSIXt"))) {
    if (length(v) == 0) {
      # No date to hide; a zero-length date can be a prototype.
      return(v)
    }
    out <- rep(nlp_default_replacements()$date, length(v))
    out[is.na(v)] <- NA_character_
    names(out) <- names(v)
    ctx$counts[["attributes"]] <- ctx$counts[["attributes"]] + sum(!is.na(out))
    return(rewrite_attributes(ctx, out))
  }
  if (is.data.frame(v)) {
    return(rewrite_frame(ctx, v, values = TRUE))
  }
  if (is.factor(v)) {
    new <- rewrite_text(ctx, levels(v), "attributes")
    if (anyDuplicated(new)) {
      # Levels that now read the same become text rather than be merged.
      out <- new[as.integer(v)]
      names(out) <- names(v)
      v <- out
    } else if (!identical(new, levels(v))) {
      levels(v) <- new
    }
  } else if (is.character(v)) {
    v <- rewrite_text(ctx, v, "attributes")
  } else if (is.list(v) && !inherits(v, "POSIXlt")) {
    cls <- oldClass(v)
    v <- unclass(v)
    for (i in seq_along(v)) {
      if (!is.null(v[[i]])) {
        v[[i]] <- rewrite_value(ctx, v[[i]])
      }
    }
    class(v) <- cls
  }
  rewrite_attributes(ctx, v)
}

rewrite_dimnames <- function(ctx, dimnames) {
  for (i in seq_along(dimnames)) {
    if (!is.null(dimnames[[i]])) {
      dimnames[[i]] <- rewrite_text(ctx, dimnames[[i]], "names")
    }
  }
  if (!is.null(names(dimnames))) {
    names(dimnames) <- rewrite_text(ctx, names(dimnames), "names")
  }
  dimnames
}

# Value labels and user-defined missing values. Label names are text and are
# rewritten. A text code (a label value, a missing value or an end of a
# missing range) is rewritten only when no value of the column equals the
# code or its rewrite (or, for a range, falls in either range), so that the
# mapping from values to labels, and which values are missing, don't change;
# such a kept code is recorded in `ctx$kept`. Codes that the rewrite made
# equal are merged by merge_equal_codes(), and recorded in `ctx$merged`.
rewrite_codes <- function(ctx, name, codes, values) {
  new <- codes
  new_names <- NULL
  if (name == "labels" && !is.null(names(codes))) {
    new_names <- rewrite_text(ctx, names(codes), NULL)
  }
  if (is.character(codes)) {
    rewritten <- rewrite_text(ctx, unname(codes), NULL)
    changed <- !is.na(codes) & rewritten != codes
    if (any(changed)) {
      values <- unique(values[!is.na(values)])
      apply <- if (name == "na_range") {
        in_range <- function(r) length(r) == 2 && any(values >= r[[1]] & values <= r[[2]])
        # A range whose rewritten ends aren't in ascending order isn't valid.
        ascending <- length(rewritten) != 2 || isTRUE(rewritten[[1]] <= rewritten[[2]])
        changed & !in_range(codes) & !in_range(rewritten) & ascending
      } else {
        changed & !codes %in% values & !rewritten %in% values
      }
      if (any(changed & !apply)) {
        ctx$kept <- c(ctx$kept, ctx$where)
      }
      new[apply] <- rewritten[apply]
      ctx$counts[["value_labels"]] <- ctx$counts[["value_labels"]] + sum(apply)
    }
  }
  if (!is.null(new_names)) {
    ctx$counts[["value_labels"]] <- ctx$counts[["value_labels"]] + sum(!is.na(new_names) & new_names != names(codes))
    names(new) <- new_names
  }
  if (name != "na_range" && !anyDuplicated(unname(codes))) {
    merged <- merge_equal_codes(new, ctx$relabel, names(codes))
    if (merged$merged) {
      if (name == "labels") {
        ctx$merged <- c(ctx$merged, ctx$where)
      } else {
        ctx$deduplicated <- c(ctx$deduplicated, ctx$where)
      }
      new <- merged$codes
    }
  }
  new
}

# Value-label codes that a rewrite or redaction made equal, merged: haven
# requires distinct codes (label text may repeat). One entry is kept per
# code, where the first of them was; its label joins the original labels
# (`original_labels`), in their order, with " / ", and the joined label goes
# once through `relabel` (the same rewrite or redaction). Missing-value codes
# have no labels and are deduplicated. Returns the codes and whether any were
# merged.
merge_equal_codes <- function(codes, relabel = identity, original_labels = names(codes)) {
  plain <- unname(codes)
  if (!anyDuplicated(plain)) {
    return(list(codes = codes, merged = FALSE))
  }
  group <- match(plain, plain)
  first <- which(!duplicated(plain))
  out <- codes[first]
  if (!is.null(names(codes))) {
    joined <- vapply(first, function(i) {
      members <- which(group == i)
      if (length(members) == 1) {
        names(codes)[i]
      } else {
        relabel(paste(original_labels[members], collapse = " / "))
      }
    }, character(1))
    names(out) <- joined
  }
  list(codes = out, merged = TRUE)
}

# A column's values as text, for comparing with its value-label codes.
underlying_text <- function(x) {
  if (is.list(x)) {
    return(character(0))
  }
  as.character(if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else unclass(x))
}

# Rewritten names, with those that now clash with another made unique
# ("[DATE]", "[DATE]_1"); names that were not rewritten keep their spelling.
unique_rewritten <- function(new, old) {
  if (is.null(new) || !anyDuplicated(new)) {
    return(new)
  }
  changed <- new != old
  made <- make.unique(c(new[!changed], new[changed]), sep = "_")
  new[!changed] <- made[seq_len(sum(!changed))]
  new[changed] <- made[sum(!changed) + seq_len(sum(changed))]
  new
}

# Warn about columns whose value-label or missing-value codes still hold
# `what` (original keys, or dates), kept by rewrite_codes().
warn_kept_codes <- function(kept, what, locate = identity) {
  if (length(kept) > 0) {
    warning(
      "These columns keep value-label or missing-value codes that hold ", what,
      ", because rewriting them would change which values they label or which are missing, ",
      "or leave a missing-value range in descending order: ",
      paste(sort(locate(kept)), collapse = ", "), ". Redact or remove them.",
      call. = FALSE
    )
  }
}

# Warn about columns whose value labels or missing-value codes were merged
# because the rewrite made their codes equal (see merge_equal_codes()).
warn_merged_codes <- function(merged, deduplicated = character(0), locate = identity) {
  if (length(merged) > 0) {
    warning(
      "Value labels of these columns were merged because the rewrite made their codes equal: ",
      paste(sort(locate(merged)), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (length(deduplicated) > 0) {
    warning(
      "User-defined missing values of these columns were deduplicated because the rewrite made them equal: ",
      paste(sort(locate(deduplicated)), collapse = ", "), ".",
      call. = FALSE
    )
  }
}

# Warn about text, anywhere in the data, that can't be searched (not valid
# UTF-8, or marked as "bytes"), counting distinct texts by location: values
# by column (see column_leaves()), and texts found while rewriting attributes
# and names (`invalid`, a list of texts by location). `locate` rewrites the
# printed locations, which can hold names inside packed columns.
warn_invalid_utf8 <- function(data, invalid = list(), locate = identity) {
  texts <- invalid
  for (ds in names(data)) {
    for (leaf in dataset_leaves(data[[ds]])) {
      bad <- unsearchable_texts(leaf$text)
      if (length(bad) > 0) {
        where <- paste0(ds, ".", leaf$path)
        texts[[where]] <- unique(c(texts[[where]], bad))
      }
    }
  }
  if (length(texts) > 0) {
    warning(
      length(unique(unlist(texts, use.names = FALSE))), " distinct text(s) can't be searched (not valid UTF-8, or marked as ",
      "\"bytes\"): ", paste(sort(locate(names(texts))), collapse = ", "),
      ". Convert them to UTF-8 (for example with iconv()) and run the step again.",
      call. = FALSE
    )
  }
}

# Warn about columns holding S4 objects, and about S4 objects held in
# attributes (`in_attributes`, by location), whose contents can't be checked
# for `what` (original keys, or dates) or rewritten.
warn_s4_columns <- function(data, what, in_attributes = character(0), locate = identity) {
  hits <- character(0)
  for (ds in names(data)) {
    for (leaf in dataset_leaves(data[[ds]])) {
      if (isTRUE(leaf$s4)) {
        hits <- c(hits, paste0(ds, ".", leaf$path))
      }
    }
  }
  if (length(hits) > 0) {
    warning(
      "These columns hold S4 objects, which can't be checked for ", what, ": ",
      paste(locate(hits), collapse = ", "), ". Redact or remove them, or convert them to plain columns.",
      call. = FALSE
    )
  }
  if (length(in_attributes) > 0) {
    warning(
      "These columns or datasets hold S4 objects in attributes, which can't be checked for ", what,
      " and were kept as they are: ", paste(sort(locate(unique(in_attributes))), collapse = ", "),
      ". Use `other_attributes = \"drop\"` to remove them.",
      call. = FALSE
    )
  }
}

# Locations (such as "ds.col" or "ds.col[sub]") as a step's warnings print
# them. Text marked "bytes" or not valid UTF-8 can't be printed, so its bytes
# are shown escaped ("<e9>"); then names, which can hold keys or dates, go
# through `rewrite` (the step's rewriter). Every step warning that names a
# location uses this.
format_locations <- function(x, rewrite = identity) {
  x <- as.character(x)
  bytes <- Encoding(x) == "bytes"
  if (any(bytes)) {
    unmarked <- x[bytes]
    Encoding(unmarked) <- "unknown"
    x[bytes] <- unmarked
  }
  bad <- !is.na(x) & !validUTF8(x)
  x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
  rewrite(x)
}

# Text held in the data, searched by the warnings ---------------------------

# The text each column of a data frame holds, for the warnings that search
# it: a list of leaves with `path` (the column, or "column[subcolumn]" inside
# a packed data frame column, at any depth), `top` (the column), `text` (the
# distinct texts, including those that can't be searched), `date` (TRUE for a
# Date or date-time column), `nested` (TRUE for list columns, packed columns
# and what they hold, which can't be given in `key_cols`) and `s4` (TRUE for
# a column of S4 objects, which isn't searched). A character column holds its values, a factor its
# levels, a list column all the text its elements hold (see list_text()). A
# packed data frame column is data like a list column: each of its columns is
# a leaf, with the text in its attributes, and the packed column itself is a
# leaf holding its column names and attributes. Cached while a step runs, as
# several warnings search the same data frame.
dataset_leaves <- function(df) {
  id <- rlang::obj_address(df)
  cached <- text_cache[[id]]
  if (!is.null(cached)) {
    return(cached)
  }
  leaves <- column_leaves(df, "", inside = FALSE)
  if (isTRUE(text_cache$.active)) {
    assign(id, leaves, envir = text_cache)
  }
  leaves
}

column_leaves <- function(df, prefix, inside, top = NULL) {
  leaves <- list()
  # Columns are taken by position: a name marked "bytes" can't be looked up.
  for (j in seq_along(df)) {
    col <- names(df)[j]
    x <- .subset2(df, j)
    path <- if (!inside) {
      col
    } else if (isTRUE(text_cache$.positional)) {
      paste0(prefix, "[", j, "]")
    } else {
      paste0(prefix, "[", col, "]")
    }
    top_col <- if (is.null(top)) col else top
    if (isS4(x)) {
      leaves <- c(leaves, list(list(path = path, top = top_col, text = NULL, date = FALSE, s4 = TRUE, nested = inside)))
      next
    }
    if (is.data.frame(x)) {
      header <- c(names(x), list_text(list(attributes(x)[setdiff(names(attributes(x)), c("names", "row.names", "class"))])))
      leaves <- c(leaves, list(list(path = path, top = top_col, text = unique(header), date = FALSE, nested = TRUE)))
      leaves <- c(leaves, column_leaves(x, path, inside = TRUE, top = top_col))
      next
    }
    text <- if (is.factor(x)) {
      levels(x)
    } else if (is.character(x)) {
      as.character(x)
    } else if (is.list(x)) {
      list_text(x)
    }
    if (inside) {
      text <- c(text, list_text(list(attributes(x)[setdiff(names(attributes(x)), c("class", "levels"))])))
    }
    text <- unique(text)
    leaves <- c(leaves, list(list(
      path = path, top = top_col, text = text, date = inherits(x, c("Date", "POSIXct")),
      nested = inside || is.list(x)
    )))
  }
  leaves
}

# Leaves are cached here while a step runs; see local_text_cache().
text_cache <- new.env(parent = emptyenv())

# Turn on the cache of dataset_leaves() until `envir` (the step) exits.
# With `positional`, columns inside packed columns are named by position
# ("col[2]") in the leaves' paths, for a step that can't recognise original
# keys in their names.
local_text_cache <- function(envir = parent.frame(), positional = FALSE) {
  assign(".active", TRUE, envir = text_cache)
  assign(".positional", positional, envir = text_cache)
  do.call(on.exit, list(quote(rm(list = ls(text_cache, all.names = TRUE), envir = text_cache)), add = TRUE), envir = envir)
}

# Distinct text values of a character column, the levels of a factor
# (including unused levels), or the text held in a list column (see
# list_text()), as UTF-8; NULL for other columns.
text_values <- function(x) {
  values <- if (is.factor(x)) {
    levels(x)
  } else if (is.character(x)) {
    unique(as.character(x))
  } else if (is.list(x) && !is.data.frame(x)) {
    unique(list_text(x))
  }
  if (is.null(values)) {
    return(NULL)
  }
  valid_utf8_text(values)
}

# All text held in the elements of a list, at any depth: character values,
# factor levels, names, and the text in their attributes (except class).
# Environments, functions and other objects that aren't vectors are not
# entered. Walks one depth at a time over all elements together.
list_text <- function(x) {
  out <- list()
  level <- unclass(x)
  while (length(level) > 0) {
    types <- vapply(level, typeof, character(1))
    keep <- types %in% c("list", "character", "integer", "double", "logical", "complex", "raw")
    keep[keep] <- !vapply(level[keep], isS4, logical(1))
    level <- level[keep]
    types <- types[keep]
    is_list <- types == "list"
    is_text <- types == "character"
    is_factor <- types == "integer"
    is_factor[is_factor] <- vapply(level[is_factor], is.factor, logical(1))
    out[[length(out) + 1]] <- unlist(lapply(level[is_factor], levels), use.names = FALSE)
    out[[length(out) + 1]] <- unlist(level[is_text], use.names = FALSE)
    attrs <- lapply(level, attributes)
    attrs <- unlist(attrs[lengths(attrs) > 0], recursive = FALSE)
    attrs <- unname(attrs[!names(attrs) %in% c("class", "levels")])
    level <- c(unlist(lapply(level[is_list], unclass), recursive = FALSE, use.names = FALSE), attrs)
  }
  as.character(unlist(out, use.names = FALSE))
}

# Text converted to UTF-8 from its declared encoding (such as latin1). Text
# marked "bytes" is left as it is: it can't be converted or searched.
as_utf8 <- function(x) {
  x <- as.character(x)
  bytes <- Encoding(x) == "bytes"
  if (any(bytes)) {
    x[!bytes] <- enc2utf8(x[!bytes])
    return(x)
  }
  enc2utf8(x)
}

# Whether each text (already passed through as_utf8()) can be searched: not
# missing, valid UTF-8 and not marked "bytes".
searchable_text <- function(x) {
  !is.na(x) & Encoding(x) != "bytes" & validUTF8(x)
}

# The distinct texts, among `x`, that can't be searched.
unsearchable_texts <- function(x) {
  if (length(x) == 0) {
    return(character(0))
  }
  x <- as_utf8(x)
  unique(x[!is.na(x) & !searchable_text(x)])
}

# Non-missing values converted to UTF-8 from their declared encoding (such as
# latin1); values that can't be searched (still not valid UTF-8, or marked
# "bytes") are dropped, because regular expressions can't search them.
valid_utf8_text <- function(x) {
  x <- as_utf8(x[!is.na(x)])
  x[searchable_text(x)]
}

# Warn about columns in keyed datasets that hold dates but were not shifted:
# unselected date columns, and dates kept as text in the mixed columns that
# were shifted (`vars` columns not named *DTC, and QVAL).
warn_unshifted_iso_dates <- function(data, keyed, date_vars, key, qval_processed, locate = identity) {
  hits <- character(0)
  sas_hits <- character(0)
  for (ds in keyed) {
    df <- data[[ds]]
    is_supp <- all(c("QNAM", "QVAL") %in% names(df))
    mixed <- c(
      date_vars[[ds]][!grepl("DTC$", date_vars[[ds]], ignore.case = TRUE, useBytes = TRUE)],
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
      paste(locate(hits), collapse = ", "),
      ". If they hold ISO 8601 dates, list them in `vars` together with every other date column ",
      "(`vars` replaces the default selection); dates inside free text or in other formats ",
      "(such as 05JAN2014) can't be shifted, so redact or remove them.",
      call. = FALSE
    )
  }
  if (length(sas_hits) > 0) {
    warning(
      "These numeric columns have a SAS date or datetime format and were not shifted: ",
      paste(locate(sas_hits), collapse = ", "),
      ". Convert them to Date or POSIXct (SAS counts from 1960-01-01) and list them in `vars`, or remove them.",
      call. = FALSE
    )
  }
}

# Columns, among `cols`, that hold dates: Date or POSIXct columns, numeric
# columns with a SAS date or datetime format, and text, factor or list columns
# with dates as whole values or inside text, and inside packed data frame
# columns the columns that do ("column.subcolumn", see column_leaves()).
iso_date_columns <- function(df, cols) {
  hits <- character(0)
  hit_cols <- character(0)
  for (leaf in dataset_leaves(df)) {
    if (leaf$top %in% cols && (leaf$date || has_dates_in_values(valid_utf8_text(leaf$text)))) {
      hits <- c(hits, leaf$path)
      hit_cols <- c(hit_cols, leaf$top)
    }
  }
  c(hits, sas_date_columns(df, setdiff(cols, hit_cols)))
}

# "dataset.column" names; empty when there are no columns.
qualify <- function(dataset, cols) {
  if (length(cols) == 0) character(0) else paste0(dataset, ".", cols)
}

# Numeric columns, among `cols`, with a SAS date or datetime display format.
sas_date_columns <- function(df, cols) {
  is_sas <- vapply(cols, function(col) {
    x <- .subset2(df, match(col, names(df)))
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
  has_dates_in_values(values)
}

has_dates_in_values <- function(values) {
  length(values) > 0 && any(
    grepl(iso_date_like_pattern, values, useBytes = TRUE) |
      grepl(embedded_date_pattern, values, useBytes = TRUE) |
      grepl(month_name_date_pattern, values, ignore.case = TRUE, useBytes = TRUE) |
      grepl(slash_date_pattern, values, useBytes = TRUE)
  )
}

iso_date_like_pattern <- "^[0-9]{4}-([0-9]{2}(-[0-9]{2}|--T)|-{2}[0-9]{2})|^[0-9]{4}-[0-9]{2}$"

# A year-month or full date inside text, e.g. "Seen on 2014-01-05".
embedded_date_core <- "[0-9]{4}-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?"
embedded_date_pattern <- paste0("(^|[^0-9-])", embedded_date_core, "([^0-9-]|$)")

month_names_pattern <- paste0(
  "(JAN(UARY)?|FEB(RUARY)?|MAR(CH)?|APR(IL)?|MAY|JUNE?|JULY?|AUG(UST)?|",
  "SEPT?(EMBER)?|OCT(OBER)?|NOV(EMBER)?|DEC(EMBER)?)"
)

# A date with a month name, as a whole value or inside text: "05JAN2014",
# "05-Jan-14", "05/JAN/2014", "UNK-JAN-2014", "5 January 2014",
# "5th January 2014", "Jan 5, 2014", "Jan 2014", "JAN2014", "2014JAN05". With
# spaces as separators a four-digit year is required, so "1 MAY 12 HRS" does
# not match.
month_name_date_core <- local({
  day <- "([0-9]{1,2}(ST|ND|RD|TH)?|UNK?|UN)"
  paste0(
    day, "[-/]?", month_names_pattern, "[-/]?([0-9]{4}|[0-9]{2})",
    "|", day, " ", month_names_pattern, ",? [0-9]{4}",
    "|", month_names_pattern, "\\.? [0-9]{1,2}(ST|ND|RD|TH)?,? [0-9]{4}",
    "|", month_names_pattern, "[- ]?[0-9]{4}",
    "|[0-9]{4}", month_names_pattern, "[0-9]{2}"
  )
})
month_name_date_pattern <- paste0("(^|[^[:alnum:]])(", month_name_date_core, ")([^[:alnum:]]|T[0-9]|$)")

# A numeric date with separators: day, month and a two- or four-digit year
# ("01/05/2014", "1/5/14", "01.05.2014", "01-05-2014"), or a four-digit year
# first ("2014/01/05").
slash_date_core <- paste0(
  "[0-9]{1,2}/[0-9]{1,2}/([0-9]{4}|[0-9]{2})",
  "|[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}",
  "|[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}",
  "|[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}"
)
slash_date_pattern <- paste0("(^|[^0-9/.-])(", slash_date_core, ")([^0-9/.-]|$)")

# The dates found by has_dates_in_text() (ISO 8601 full and year-month dates,
# SDTM partial dates, month-name and numeric forms), anywhere in text, as one
# ICU regular expression whose boundaries are lookarounds, so that a
# replacement keeps the text around each date. Used case-insensitively, for
# month names.
date_in_text_regex <- paste0(
  # SDTM partial dates: year and month with a time ("2014-01--T08:00"; the
  # time is kept), and year and day ("2014---15").
  "(?<![0-9-])[0-9]{4}-(0[1-9]|1[0-2])--(?=T)",
  "|(?<![0-9-])[0-9]{4}---(0[1-9]|[12][0-9]|3[01])(?![0-9-])",
  "|(?<![0-9-])(?:", embedded_date_core, ")(?![0-9-])",
  "|(?<![[:alnum:]])(?:", month_name_date_core, ")(?=[^[:alnum:]]|T[0-9]|$)",
  "|(?<![0-9/.-])(?:", slash_date_core, ")(?![0-9/.-])"
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
warn_unkeyed_identifiers <- function(data, keyed, key, id_cols, check_dates, locate = identity) {
  hits <- character(0)
  for (ds in setdiff(names(data), keyed)) {
    df <- data[[ds]]
    cols <- names(df)
    flagged <- c(
      cols[name_key(cols) %in% name_key(c(key, id_cols))],
      if (check_dates) dataset_date_vars(df, NULL, key, ds),
      if (check_dates) iso_date_columns(df, cols)
    )
    if (length(flagged) > 0) {
      hits <- c(hits, paste0(locate(ds), " (", paste(locate(unique(flagged)), collapse = ", "), ")"))
    }
  }
  if (length(hits) > 0) {
    warning(
      "Datasets without `", key, "` kept their values unchanged but contain identifiers or dates: ",
      paste(hits, collapse = "; "), ".",
      call. = FALSE
    )
  }
}

# Warn about columns in keyed datasets named like `key`, `also` or `key_cols`
# in another case (such as `usubjid` beside `USUBJID`), which were not
# replaced.
warn_id_name_variants <- function(data, keyed, id_cols, locate = identity) {
  hits <- character(0)
  for (ds in keyed) {
    cols <- names(data[[ds]])
    hits <- c(hits, qualify(ds, cols[name_key(cols) %in% name_key(id_cols) & !cols %in% id_cols]))
  }
  if (length(hits) > 0) {
    warning(
      "These columns are named like identifier columns in another case and were not replaced: ",
      paste(locate(hits), collapse = ", "),
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
    log <- deid_log_add(log, ds, NA_character_, paste0("no `", key, "` column; values left unchanged"), 0, 0)
  }
  log
}
