#' Check that code tested on a de-identified copy should run on the original
#'
#' Compares a de-identified copy with the original data, property by
#' property, and reports what code written against the copy relies on that
#' no longer holds. Run it where the original data is, before sharing the
#' copy; the result holds counts, names and rule parameters only, so it can
#' be shared with the copy (see [anon_report()]).
#'
#' @param original A data frame or a named list of data frames: the data
#'   before de-identification.
#' @param deidentified The same as `original` after de-identification: a
#'   data frame, a named list of data frames, or the step that made it (such
#'   as an [anon_deidentify()] result). With a step, its log and settings
#'   say what was changed on purpose; without one, intended changes can't be
#'   told from others and are reported as `"warn"`.
#' @param key Name of the subject identifier column.
#' @param dm Name of the subject-level dataset whose keys every other
#'   dataset's keys must be found in, and whose `RFSTDTC` study days are
#'   counted from (`"adsl"` for ADaM data without `dm`).
#'
#' @details
#' Each check compares a property in the original with the same property in
#' the copy. It fails only when the property held in the original and is
#' broken in the copy. A property already broken in the original is
#' `"not applicable"`, with the original's count.
#'
#' A check that compared nothing is `"not applicable"`, with the reason.
#'
#' **Aligning rows.** [anon_subject_ids()] sorts rows by the new key, so the
#' copy's rows don't line up with the original's. Subjects are paired by
#' their rows, coded from the columns no step logged (and of the same class),
#' in every dataset with `key`, in turn: rows that match exactly; then, of
#' the rest, subjects with as many rows in every dataset and more than half
#' of them equal (rows sharing a code with more than 20 unpaired subjects
#' don't count, which keeps this linear; subjects with no other match stay
#' unpaired); then rows that match in the columns with no value more often
#' in the copy than in the original (a column changed throughout is left
#' out); then, among subjects alike in those columns, each subject's own
#' dates (their formats and their distances from the subject's first date,
#' which the per-subject offset keeps). A step's kept pseudonym map
#' (`keep_secrets = TRUE`) is used instead when there is one, and keys that
#' weren't pseudonymized pair directly. Within a subject, rows pair in
#' order. Subjects alike in every column no step logged and in their date
#' pattern give the same result for every check whichever way they pair, so
#' when the copy holds as many of them as the original, they are compared as
#' a class: each copy subject is paired, in a fixed order, with an original
#' whose values explain the cells a step logged (blanked, generalized or
#' derived where the step says so, and otherwise equal), and the checks run
#' on them. If no pairing explains them all, the subjects left fail. A class
#' with a different number of subjects in the copy can't be paired: those
#' subjects are counted (`pseudonyms` is `"warn"`), their rows are not
#' compared one by one, and the result is incomplete. A copy subject whose
#' dates match none of the originals alike in the other columns fails. Rows
#' of datasets without `key` pair by position.
#'
#' **Checks** (one row per dataset and check; `"(all)"` for checks across
#' datasets):
#' - `datasets`: the same dataset names.
#' - `columns`: the same columns in the same order.
#' - `types`: the same class and type for each column, and for the columns
#'   of nested data frames (packed columns, and data frames in list columns).
#' - `attributes`: the same dataset and column labels, formats
#'   (`format.sas`, `format.spss`, `format.stata`, `display_width`), time
#'   zones (`tzone`), factor levels (their set and order) and value labels.
#'   Value labels and levels changed in a column a step changed (the key,
#'   related IDs, dates, logged columns) are intended. In a dataset where the
#'   log of [anon_subject_ids()] or [anon_subject_dates()] counts labels it
#'   rewrote (keys and dates in them), a changed label is `"warn"`; elsewhere
#'   it fails, and so does a label lost.
#' - `rows`: the same number of rows, or fewer only by the rows of subjects
#'   the step dropped (`"warn"`, with counts). Rows of subjects that couldn't
#'   be paired are counted, and make the result incomplete.
#' - `row order`: within each subject, rows in the same order.
#' - `keys in dm`: every key of the dataset is in `dm`.
#' - `distinct keys`: the original's number of subjects, less those dropped.
#' - `pseudonyms`: each subject has one pseudonym in every dataset, and each
#'   pseudonym one subject: every copy subject's rows across datasets match
#'   one original subject's. Related IDs that end with the key in the
#'   original (`SUBJID` of `USUBJID`) must end with the pseudonym.
#' - `values`: every column no step logged keeps its values, row by row over
#'   the paired rows; the key, related IDs and dates have checks of their
#'   own. A cell a step logged may be blank where it blanked it, and changed
#'   where it generalized it (a fold only to the rule's category) or derived
#'   it again; a column logged for one test only is compared on the other
#'   rows. Packed data frame columns are compared the same way; data frames
#'   in list columns by their number of rows, and by their values when their
#'   rows keep their order (no key column, or one subject).
#' - `categorical values`: each value of a character or factor column is a
#'   value of the original column, a blank, or a value a step introduced on
#'   purpose (a fold category of a generalization rule, numbers rewritten
#'   from a generalized result). Key, related ID and date columns are
#'   checked by the other checks.
#' - `ISO 8601`: each non-blank value of a `*DTC` column, or of a column a
#'   profile's `"date"` rule names, has the format and precision of the
#'   original value in its row. Values removed on purpose (not ISO 8601 in
#'   the original, partial dates with `partial = "remove"`, rows without a
#'   key, columns a step blanked) are counted apart.
#' - `date intervals`: within each subject, every date (text, Date and
#'   date-time columns, in every dataset) moves by the subject's offset, so
#'   the interval between any two is kept; clock times are kept. Year-and-
#'   month dates are compared at month precision (the month of the
#'   original's day 15 moved by the offset, as [anon_subject_dates()] shifts
#'   them), and year-only dates must all move by the same number of years.
#' - `study days`: `--DY` recomputed from `--DTC` and `RFSTDTC` (SDTM rule,
#'   no day 0), and ADaM `*DY` from `*DT` and `TRTSDT`, agree with the
#'   copy's values wherever they agreed in the original.
#' - `numeric relations`: `CHG = AVAL - BASE`, `PCHG = 100 * CHG / BASE` and
#'   `--STRESN` equal to the number in `--STRESC`, wherever they held in the
#'   original. Rows where a step changed one of the values on purpose (and
#'   logged it) are counted apart; if the relation no longer holds in some of
#'   them, the check is `"warn"`.
#'
#' **Not checked:** business rules beyond these; values that a step logged
#' as changed (beyond being blanked, folded or generalized where it said);
#' values of data frames nested in list columns whose rows hold several
#' subjects (their order changes), and dates and keys inside nested data;
#' statistics (means and counts by group change with banding and
#' suppression). A pass is evidence that code relying on these properties
#' should behave the same on the original, not proof that it will.
#'
#' **Sharing the result.** It names datasets and columns as the copy does,
#' and never an original's name; `print()`, [as_anon_payload()] and
#' [as_anon_json()] don't apply your `pattern_list`. To share it redacted,
#' include it in a report: `anon_report(step, fidelity = result,
#' pattern_list = ...)`.
#'
#' **Cost.** A few passes over every column of every dataset, plus groupings
#' of the rows, linear in the number of rows: a few seconds for the
#' pharmaversesdtm and pharmaverseadam datasets.
#'
#' @return A tibble of class `"anon_fidelity"` with columns `dataset`,
#'   `check`, `status` (`"pass"`, `"fail"`, `"warn"` or `"not applicable"`),
#'   `n_checked`, `n_failed` and `detail` (counts only), and attributes
#'   `complete` (`TRUE` when nothing was left uncompared: no subjects that
#'   couldn't be paired or told apart, no dates of subjects with no known
#'   offset) and `passed` (`TRUE` when the result is complete and no check
#'   failed). It prints, and converts with [as_anon_payload()] and
#'   [as_anon_json()].
#'
#' @seealso [anon_deidentify()], [anon_report()]
#'
#' @examples
#' dm <- data.frame(
#'   USUBJID = sprintf("S%02d", 1:12),
#'   AGE = c(31, 34, 38, 42, 45, 47, 52, 55, 58, 61, 66, 69),
#'   SEX = rep(c("F", "M"), 6),
#'   RFSTDTC = format(as.Date("2020-01-01") + 0:11)
#' )
#' result <- anon_deidentify(list(dm = dm), max_risk = 1/3, seed = 1)
#' anon_check_fidelity(list(dm = dm), result)
#'
#' @export
anon_check_fidelity <- function(original, deidentified, key = "USUBJID", dm = "dm") {
  check_key_name(key)
  if (!is.character(dm) || length(dm) != 1 || is.na(dm) || !nzchar(dm)) {
    stop("`dm` must be a single dataset name.", call. = FALSE)
  }
  step <- if (inherits(deidentified, "anon_deid_step")) deidentified
  orig <- fidelity_datasets(original, "original")
  copy <- fidelity_datasets(if (is.null(step)) deidentified else step$data, "deidentified")
  intent <- fidelity_intent(step, key)
  # Names the keys or dates step rewrote: the original is compared under the
  # copy's names, so that no original name is shown.
  renamed <- fidelity_rename(orig, copy, intent)
  orig <- renamed$data

  rows <- list(fidelity_check_datasets(names(orig), names(copy), renamed$datasets))
  common <- intersect(names(orig), names(copy))
  orig <- orig[common]
  copy_common <- copy[common]
  align <- fidelity_align(orig, copy_common, key, intent)

  for (ds in common) {
    o <- orig[[ds]]
    co <- copy_common[[ds]]
    rows <- c(rows, list(
      fidelity_check_columns(ds, o, co, renamed$columns[[ds]] %||% 0L),
      fidelity_check_types(ds, o, co),
      fidelity_check_attributes(ds, o, co, key, intent)
    ))
    if (ds %in% align$keyed) {
      rows <- c(rows, list(
        fidelity_check_rows(ds, o, co, align, intent),
        fidelity_check_order(ds, align),
        fidelity_check_keys_in_dm(ds, align, dm),
        fidelity_check_distinct_keys(ds, align)
      ))
    } else {
      rows <- c(rows, list(fidelity_check_rows(ds, o, co, align, intent)))
    }
    pairs <- align$pairs[[ds]]
    rows <- c(rows, list(
      fidelity_check_values(ds, o, co, pairs, key, intent, align$dropped),
      fidelity_check_categorical(ds, o, co, pairs, key, intent),
      fidelity_check_iso(ds, o, co, pairs, key, intent)
    ))
  }
  if (length(align$keyed) > 0 && !dm %in% common) {
    rows <- c(rows, list(fidelity_row(
      "(all)", "keys in dm", "not applicable", 0L, 0L,
      paste0("no dataset `", dm, "`", if ("adsl" %in% common) "; for ADaM data, dm = \"adsl\" may be meant")
    )))
  }
  if (length(align$keyed) > 0) {
    rows <- c(rows, list(fidelity_check_pseudonyms(align, fidelity_related_ids(orig, copy_common, align, intent))))
  }
  rows <- c(rows, fidelity_check_intervals(orig, copy_common, align, key, intent))
  for (ds in common) {
    rows <- c(rows, list(
      fidelity_check_study_days(ds, orig, copy_common, align, key, dm),
      fidelity_check_relations(ds, orig[[ds]], copy_common[[ds]], align$pairs[[ds]], intent)
    ))
  }

  out <- do.call(rbind, Filter(Negate(is.null), rows))
  order_ds <- match(out$dataset, c("(all)", common))
  out <- out[order(order_ds, match(out$check, fidelity_check_names)), , drop = FALSE]
  new_anon_fidelity(out)
}

# Near pairing uses only row codes held by at most this many unpaired
# subjects (see fidelity_align()).
fidelity_near_cap <- 20L

# The checks, in the order they are listed.
fidelity_check_names <- c(
  "datasets", "columns", "types", "attributes", "rows", "row order", "keys in dm", "distinct keys", "pseudonyms",
  "values", "categorical values", "ISO 8601", "date intervals", "study days", "numeric relations"
)

# A check that compared nothing is "not applicable", never "pass". The
# result is complete when nothing was skipped for want of pairing or an
# offset, and passed when it is complete and no check failed.
new_anon_fidelity <- function(x) {
  nothing <- x$status == "pass" & x$n_checked == 0
  x$status[nothing] <- "not applicable"
  x$detail[nothing] <- paste0(x$detail[nothing], "; nothing compared")
  complete <- all(x$skipped == 0)
  x$skipped <- NULL
  out <- tibble::new_tibble(tibble::as_tibble(x), class = "anon_fidelity")
  attr(out, "complete") <- complete
  attr(out, "passed") <- complete && !any(x$status == "fail")
  out
}

#' @export
print.anon_fidelity <- function(x, ...) {
  counts <- table(factor(x$status, levels = c("pass", "warn", "fail", "not applicable")))
  cat(
    "Fidelity of the de-identified copy:", if (isTRUE(attr(x, "passed"))) "passed" else "FAILED",
    if (!isTRUE(attr(x, "complete"))) "(incomplete: some rows or dates couldn't be compared)", "\n"
  )
  cat(paste0(names(counts), ": ", counts, collapse = ", "), "\n\n")
  print(as.data.frame(x), row.names = FALSE, right = FALSE)
  invisible(x)
}

#' @exportS3Method
as_anon_payload.anon_fidelity <- function(x, path = "$") {
  list(
    schema_version = anon_json_schema_version(),
    kind = "anon_fidelity",
    passed = isTRUE(attr(x, "passed")),
    complete = isTRUE(attr(x, "complete")),
    checks = serialize_table_payload(as.data.frame(x), path = paste0(path, "$checks"))
  )
}

# helpers -----------------------------------------------------------------

# `skipped` counts what the check couldn't compare (rows of subjects that
# couldn't be paired, dates of subjects with no known offset); any makes the
# result incomplete.
fidelity_row <- function(dataset, check, status, n_checked, n_failed, detail, skipped = 0L) {
  data.frame(
    dataset = dataset, check = check, status = status, n_checked = as.integer(n_checked),
    n_failed = as.integer(n_failed), detail = detail, skipped = as.integer(skipped), stringsAsFactors = FALSE
  )
}

# pass/fail/warn from the failures: without a step, intended changes can't be
# told from others, so a change is a warning rather than a failure.
fidelity_status <- function(n_failed, n_warned = 0, strict = TRUE) {
  if (n_failed > 0) {
    if (strict) "fail" else "warn"
  } else if (n_warned > 0) {
    "warn"
  } else {
    "pass"
  }
}

fidelity_datasets <- function(x, what) {
  if (is.data.frame(x)) {
    return(list(data = x))
  }
  if (!is.list(x) || length(x) == 0 || is.null(names(x)) || any(is.na(names(x)) | names(x) == "") ||
    !all(vapply(x, is.data.frame, logical(1)))) {
    stop("`", what, "` must be a data frame or a named list of data frames.", call. = FALSE)
  }
  x
}

# What the step changed on purpose, read from its settings and log: related
# ID columns, whether keys were pseudonymized and dates shifted, the cells
# each step logged (dataset, column and, for result columns, the test), the
# generalization rules (for fold categories), the number of subjects dropped,
# the profile's date rules, and the kept pseudonym map, if any (used only to
# align rows; never reported).
fidelity_intent <- function(step, key) {
  out <- list(
    step = !is.null(step), ids = character(0), also = character(0), key_cols = character(0), pseudonymized = FALSE, dates = FALSE,
    partial = "shift", logged = fidelity_log_cells(NULL), shifted = list(), rules = list(), n_dropped = 0L,
    date_rules = character(0), map = NULL, rewrites = list()
  )
  if (is.null(step)) {
    return(out)
  }
  s <- step$settings
  pick <- function(kind) s[names(s) == kind | startsWith(names(s), paste0(kind, "_"))]
  ids_settings <- c(pick("subject_ids"), pick("deidentify"))
  out$ids <- unique(unlist(lapply(ids_settings, function(x) c(x$also, x$key_cols)), use.names = FALSE))
  out$also <- unique(unlist(lapply(ids_settings, `[[`, "also"), use.names = FALSE))
  out$key_cols <- unique(unlist(lapply(ids_settings, `[[`, "key_cols"), use.names = FALSE))
  out$pseudonymized <- length(pick("subject_ids")) > 0
  dates <- pick("subject_dates")
  out$dates <- length(dates) > 0
  if (out$dates) {
    out$partial <- utils::tail(dates, 1)[[1]]$partial %||% "shift"
  }
  out$logged <- fidelity_log_cells(step$log)
  out$rewrites <- fidelity_rewrites(step$log)
  shifted <- out$logged[out$logged$step == "subject_dates" & is.na(out$logged$test_col), , drop = FALSE]
  out$shifted <- split(shifted$column, shifted$dataset)
  out$rules <- unlist(lapply(pick("generalize"), `[[`, "rules"), recursive = FALSE)
  out$n_dropped <- sum(vapply(pick("suppress"), function(x) as.integer(x$n_dropped %||% 0L), integer(1)))
  rules <- unlist(lapply(pick("deidentify"), function(x) {
    r <- x$profile_rules
    if (is.null(r)) character(0) else r$rule[r$role == "date"]
  }), use.names = FALSE)
  # A rule's text starts with its variable or pattern ("VISIT_DATE in dm").
  out$date_rules <- unique(sub("[ ,].*$", "", rules))
  if (!is.null(step$secrets) && exists("subject_ids", envir = step$secrets, inherits = FALSE)) {
    out$map <- get("subject_ids", envir = step$secrets, inherits = FALSE)
  }
  out
}

# The log's rows that name a dataset and a variable, with the variable split
# into the column and, for rows of a test ("VSSTRESN (VSTESTCD = HEIGHT)"),
# the test column and test.
fidelity_log_cells <- function(log) {
  empty <- data.frame(
    step = character(0), dataset = character(0), derived = logical(0), column = character(0), test_col = character(0),
    test = character(0), stringsAsFactors = FALSE
  )
  if (is.null(log) || nrow(log) == 0) {
    return(empty)
  }
  log <- log[!is.na(log$dataset) & !is.na(log$variable), , drop = FALSE]
  if (nrow(log) == 0) {
    return(empty)
  }
  rx <- "^(.*) \\(([^ ]+) = (.*)\\)$"
  has_test <- grepl(rx, log$variable)
  data.frame(
    step = log$step, dataset = log$dataset, derived = startsWith(log$detail, "derived from"),
    column = ifelse(has_test, sub(rx, "\\1", log$variable), log$variable),
    test_col = ifelse(has_test, sub(rx, "\\2", log$variable), NA_character_),
    test = ifelse(has_test, sub(rx, "\\3", log$variable), NA_character_),
    stringsAsFactors = FALSE
  )
}

# By dataset, the attribute values and the value labels in which the keys or
# dates step rewrote keys or dates, as its log counts them ("... in
# attributes and names: 19 attribute value(s), 2 value label(s) ...").
fidelity_rewrites <- function(log) {
  rows <- log[log$step %in% c("subject_ids", "subject_dates") & !is.na(log$dataset) & is.na(log$variable) &
    grepl("in attributes and names", log$detail, fixed = TRUE), , drop = FALSE]
  count <- function(detail, what) {
    m <- regmatches(detail, regexpr(paste0("[0-9]+ ", what), detail))
    if (length(m) == 0) 0L else as.integer(sub(" .*", "", m))
  }
  out <- list()
  for (i in seq_len(nrow(rows))) {
    ds <- rows$dataset[i]
    prev <- out[[ds]] %||% list(attributes = 0L, labels = 0L)
    out[[ds]] <- list(
      attributes = prev$attributes + count(rows$detail[i], "attribute value"),
      labels = prev$labels + count(rows$detail[i], "value label")
    )
  }
  out
}

# Rows of `df` (a dataset `ds` of the copy) whose `col` cell a step logged,
# optionally only the given steps.
fidelity_logged_rows <- function(intent, ds, col, df, steps = NULL, derived = NULL) {
  lg <- intent$logged
  lg <- lg[lg$dataset == ds & lg$column == col, , drop = FALSE]
  if (!is.null(steps)) {
    lg <- lg[lg$step %in% steps, , drop = FALSE]
  }
  if (!is.null(derived)) {
    lg <- lg[lg$derived == derived, , drop = FALSE]
  }
  out <- rep(FALSE, nrow(df))
  for (i in seq_len(nrow(lg))) {
    if (is.na(lg$test_col[i])) {
      return(rep(TRUE, nrow(df)))
    }
    t <- df[[lg$test_col[i]]]
    if (!is.null(t) && !is.data.frame(t) && !is.list(t)) {
      t <- fidelity_text(t)
      out <- out | (!is.na(t) & t == lg$test[i])
    }
  }
  out
}

# Whether any step logged a change to `col` of `ds`.
fidelity_logged_column <- function(intent, ds, col, steps = NULL) {
  lg <- intent$logged
  any(lg$dataset == ds & lg$column == col & (if (is.null(steps)) TRUE else lg$step %in% steps))
}

# Values as plain vectors that compare as the user sees them: factors as
# text, haven-labelled values as their values, other classes as their data.
fidelity_plain <- function(x) {
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (inherits(x, "POSIXlt")) {
    x <- as.POSIXct(x)
  }
  if (is.object(x) || !is.null(attributes(x))) {
    x <- vctrs::vec_data(x)
    attributes(x) <- NULL
  }
  x
}

fidelity_text <- function(x) {
  if (is.factor(x)) as.character(x) else as.character(fidelity_plain(x))
}

fidelity_blank <- function(x) {
  is.na(x) | grepl("^[[:space:]]*$", x, useBytes = TRUE)
}

fidelity_is_text <- function(x) {
  is.character(x) || is.factor(x) || (inherits(x, "haven_labelled") && is.character(vctrs::vec_data(x)))
}

fidelity_is_date <- function(x) inherits(x, c("Date", "POSIXct", "POSIXlt"))

# Text date columns: *DTC, as anon_subject_dates() finds them, columns a
# profile's "date" rule names, and other text columns the dates step shifted
# dates in (such as QVAL of supplemental qualifiers).
fidelity_text_date_columns <- function(df, key, intent, ds = "") {
  cols <- setdiff(names(df), key)
  text <- cols[vapply(df[cols], fidelity_is_text, logical(1))]
  dtc <- grepl("DTC$", text, ignore.case = TRUE, useBytes = TRUE) | text %in% intent$shifted[[ds]]
  ruled <- rep(FALSE, length(text))
  for (r in intent$date_rules) {
    ruled <- ruled | profile_match(r, text)
  }
  text[dtc | ruled]
}

# Names only the original has are counted, not shown: they may hold keys or
# dates.
fidelity_check_datasets <- function(o, c, renamed) {
  missing <- setdiff(o, c)
  extra <- setdiff(c, o)
  n_failed <- length(missing) + length(extra)
  detail <- paste(c(
    paste0(length(o), " dataset(s)"),
    if (renamed > 0) paste0(renamed, " renamed by the keys or dates step (keys or dates in the name rewritten)"),
    if (length(missing) > 0) paste0(length(missing), " missing from the copy"),
    if (length(extra) > 0) paste0("not in the original: ", paste(extra, collapse = ", "))
  ), collapse = "; ")
  fidelity_row("(all)", "datasets", fidelity_status(n_failed, renamed), length(union(o, c)), n_failed, detail)
}

fidelity_check_columns <- function(ds, o, c, renamed) {
  same <- identical(names(o), names(c))
  missing <- setdiff(names(o), names(c))
  extra <- setdiff(names(c), names(o))
  n_failed <- length(missing) + length(extra)
  detail <- paste(c(
    paste0(ncol(o), " column(s)", if (same) ", in the same order"),
    if (renamed > 0) paste0(renamed, " renamed by the keys or dates step (keys or dates in the name rewritten)"),
    if (length(missing) > 0) paste0(length(missing), " missing from the copy"),
    if (length(extra) > 0) paste0("not in the original: ", paste(extra, collapse = ", ")),
    if (!same && n_failed == 0) "the same columns in another order"
  ), collapse = "; ")
  fidelity_row(ds, "columns", if (!same) "fail" else if (renamed > 0) "warn" else "pass", ncol(o), max(n_failed, !same), detail)
}

# Positions where a name differs and neither name is found on the other side:
# a rewritten name, not a column moved.
fidelity_renamed <- function(old, new) {
  old != new & !old %in% new & !new %in% old
}

# The original with the names the keys or dates step rewrote (keys and dates
# in dataset and column names, at any depth) replaced by the copy's: where
# the number of datasets, or of a data frame's columns, is the same and a
# name found only in the original has one found only in the copy at the same
# position. Counts the datasets and, by dataset,
# the columns renamed.
fidelity_rename <- function(orig, copy, intent) {
  out <- list(data = orig, datasets = 0L, columns = list())
  if (!intent$pseudonymized && !intent$dates) {
    return(out)
  }
  n <- 0L
  frame <- function(x, y) {
    if (!is.data.frame(x) || !is.data.frame(y) || ncol(x) != ncol(y)) {
      return(x)
    }
    differ <- fidelity_renamed(names(x), names(y))
    if (any(differ)) {
      n <<- n + sum(differ)
      names(x)[differ] <- names(y)[differ]
    }
    for (j in seq_along(x)) {
      a <- x[[j]]
      b <- y[[j]]
      if (is.data.frame(a)) {
        x[[j]] <- frame(a, b)
      } else if (is.list(a) && is.list(b) && !is.data.frame(b) && length(a) == length(b)) {
        # Elements are renamed on the bare list, then the column's class and
        # attributes (a list_of's prototype renamed too) are put back.
        attrs <- attributes(a)
        elements <- unclass(a)
        for (i in seq_along(elements)) {
          if (is.data.frame(elements[[i]]) && is.data.frame(b[[i]])) elements[[i]] <- frame(elements[[i]], b[[i]])
        }
        if (is.data.frame(attrs$ptype)) {
          attrs$ptype <- frame(attrs$ptype, attr(b, "ptype", exact = TRUE))
        }
        attributes(elements) <- attrs
        x[[j]] <- elements
      }
    }
    x
  }
  if (length(orig) == length(copy)) {
    differ <- fidelity_renamed(names(orig), names(copy))
    out$datasets <- sum(differ)
    names(orig)[differ] <- names(copy)[differ]
  }
  for (ds in intersect(names(orig), names(copy))) {
    n <- 0L
    orig[[ds]] <- frame(orig[[ds]], copy[[ds]])
    out$columns[[ds]] <- n
  }
  out$data <- orig
  out
}

# A column's class and type; for a data frame, those of its columns; for a
# list, those of the data frames it holds.
fidelity_type <- function(x) {
  if (inherits(x, "POSIXlt")) {
    return(paste(class(x), collapse = "/"))
  }
  base <- paste0(paste(class(x), collapse = "/"), " <", typeof(x), ">")
  if (is.data.frame(x)) {
    return(paste0(base, " (", paste0(names(x), ": ", vapply(x, fidelity_type, character(1)), collapse = ", "), ")"))
  }
  base
}

# The structure of the data frames held in a list column, one per distinct
# shape.
fidelity_list_frames <- function(x) {
  if (!is.list(x) || is.data.frame(x)) {
    return(character(0))
  }
  frames <- Filter(is.data.frame, x)
  unique(vapply(frames, fidelity_type, character(1)))
}

fidelity_check_types <- function(ds, o, c) {
  cols <- intersect(names(o), names(c))
  bad <- character(0)
  for (col in cols) {
    if (!identical(fidelity_type(o[[col]]), fidelity_type(c[[col]])) ||
      !all(fidelity_list_frames(c[[col]]) %in% fidelity_list_frames(o[[col]]))) {
      bad <- c(bad, col)
    }
  }
  detail <- if (length(bad) == 0) {
    paste0(length(cols), " column(s) with the same class and type, nested data frames included")
  } else {
    paste0("class or type changed: ", paste(bad, collapse = ", "))
  }
  fidelity_row(ds, "types", fidelity_status(length(bad)), length(cols), length(bad), detail)
}

fidelity_format_attributes <- c("format.sas", "format.spss", "format.stata", "display_width", "tzone")

# Column attributes compared, at any depth through packed data frames.
fidelity_check_attributes <- function(ds, o, c, key, intent) {
  # Labels count as rewritten by the keys or dates step only in a dataset
  # where its log says it rewrote some; elsewhere a changed label fails.
  rewrites <- intent$rewrites[[ds]] %||% list(attributes = 0L, labels = 0L)
  res <- list(checked = 0L, lost = character(0), changed = character(0), rewritten = character(0), dropped = 0L)
  label_change <- function(lo, lc, name, may_rewrite) {
    if (!is.null(lo) && is.null(lc)) {
      res$lost <<- c(res$lost, name)
    } else if (!identical(lo, lc)) {
      if (!is.null(lo) && may_rewrite) res$rewritten <<- c(res$rewritten, name) else res$changed <<- c(res$changed, name)
    }
  }
  label_change(attr(o, "label", exact = TRUE), attr(c, "label", exact = TRUE), "(dataset label)", rewrites$attributes > 0)
  walk <- function(o, c, prefix, top) {
    for (col in intersect(names(o), names(c))) {
      x <- o[[col]]
      y <- c[[col]]
      name <- paste0(prefix, col)
      res$checked <<- res$checked + 1L
      # Columns a step changed on purpose: the key, related IDs, dates and
      # logged columns.
      own <- top && (fidelity_logged_column(intent, ds, col) || col %in% c(key, intent$ids) ||
        ((intent$dates || intent$pseudonymized) && (fidelity_is_date(y) || grepl("DTC$", col, ignore.case = TRUE))))
      label_change(attr(x, "label", exact = TRUE), attr(y, "label", exact = TRUE), name, rewrites$attributes > 0)
      for (a in fidelity_format_attributes) {
        if (!identical(attr(x, a, exact = TRUE), attr(y, a, exact = TRUE))) res$changed <<- c(res$changed, name)
      }
      # Factor levels, their set and order.
      if (is.factor(x) && is.factor(y) && !identical(levels(x), levels(y))) {
        if (own) {
          res$dropped <<- res$dropped + 1L
        } else if (rewrites$attributes > 0 && length(levels(x)) == length(levels(y))) {
          res$rewritten <<- c(res$rewritten, name)
        } else {
          res$changed <<- c(res$changed, name)
        }
      }
      vo <- attr(x, "labels", exact = TRUE)
      vc <- attr(y, "labels", exact = TRUE)
      if (!identical(vo, vc)) {
        # Value labels a step dropped (or pruned to the values still
        # released) from a column it changed are intended.
        subset <- !is.null(vo) && (is.null(vc) || (all(names(vc) %in% names(vo)) &&
          all(vctrs::vec_in(unname(fidelity_plain(vc)), unname(fidelity_plain(vo))))))
        if (own && subset) {
          res$dropped <<- res$dropped + 1L
        } else if (rewrites$labels > 0 && !is.null(vo) && !is.null(vc) && length(vc) <= length(vo)) {
          # Keys or dates rewritten in the labels, merged where their codes
          # became equal.
          res$rewritten <<- c(res$rewritten, name)
        } else {
          res$lost <<- c(res$lost, name)
        }
      }
      if (is.data.frame(x) && is.data.frame(y)) {
        walk(x, y, paste0(name, "$"), FALSE)
      }
    }
  }
  walk(o, c, "", TRUE)
  failed <- unique(c(res$lost, res$changed))
  warned <- unique(setdiff(res$rewritten, failed))
  detail <- paste(c(
    paste0(res$checked, " column(s)"),
    if (length(res$lost) > 0) paste0("label or value labels lost or changed: ", paste(unique(res$lost), collapse = ", ")),
    if (length(res$changed) > 0) {
      paste0("label, format, time zone or factor levels changed: ", paste(unique(res$changed), collapse = ", "))
    },
    if (length(warned) > 0) paste0("labels rewritten by the keys or dates step: ", paste(warned, collapse = ", ")),
    if (res$dropped > 0) paste0(res$dropped, " with value labels or levels changed on purpose")
  ), collapse = "; ")
  fidelity_row(ds, "attributes", fidelity_status(length(failed), length(warned)), res$checked, length(failed), detail)
}

# Columns whose rows pair original and copy subjects: not the key or a
# related ID, not dates, not logged by any step, and of the same class. With
# `containment`, also no value more often in the copy than in the original:
# a column changed throughout (such as banded ages without a step, or a
# broken copy) is left out, so that subjects can still be paired and the
# values check can find the change.
fidelity_stable_columns <- function(ds, o, c, key, intent, containment = TRUE) {
  cols <- setdiff(intersect(names(o), names(c)), c(key, intent$ids, fidelity_text_date_columns(o, key, intent, ds)))
  keep <- vapply(cols, function(col) {
    x <- o[[col]]
    y <- c[[col]]
    if (is.list(x) || is.list(y) || fidelity_is_date(x) || !identical(class(x), class(y))) {
      return(FALSE)
    }
    if (fidelity_logged_column(intent, ds, col)) {
      return(FALSE)
    }
    if (!containment) {
      return(TRUE)
    }
    to <- vctrs::vec_count(fidelity_plain(x))
    tc <- vctrs::vec_count(fidelity_plain(y))
    m <- vctrs::vec_match(tc$key, to$key)
    !anyNA(m) && all(tc$count <= to$count[m])
  }, logical(1))
  cols[keep]
}

# Row codes shared by the original's and the copy's rows: equal codes, equal
# values in `cols`.
fidelity_row_codes <- function(o, c, cols) {
  if (length(cols) == 0) {
    return(list(o = rep(1L, nrow(o)), c = rep(1L, nrow(c))))
  }
  plain <- function(df) tibble::new_tibble(lapply(df[cols], fidelity_plain), nrow = nrow(df))
  ids <- vctrs::vec_group_id(vctrs::vec_rbind(plain(o), plain(c)))
  list(o = as.integer(ids[seq_len(nrow(o))]), c = as.integer(ids[nrow(o) + seq_len(nrow(c))]))
}

# Keys as text, blank and haven missing-value codes missing (the steps make
# such codes missing: they are no key).
fidelity_keys <- function(x) normalize_keys(zap_user_missing(x))

# Each row's subject: the key column's, or for a dataset without one, that of
# the nested data that gives its rows subjects (see "Nested data" in
# anon_subject_ids()). NULL when the rows have no subjects.
fidelity_owners <- function(df, key) {
  if (key %in% names(df)) {
    return(fidelity_keys(df[[key]]))
  }
  if (!any(vapply(df, is.list, logical(1)))) {
    return(NULL)
  }
  top <- nested_tables(df, key)[[1]]
  if (isTRUE(top$owns)) top$owners
}

# Pairs original and copy subjects and rows (see "Aligning rows" in the
# docs). Returns, for each dataset, the pairs of row positions (`o`, `c`) and
# the original subject of each pair (`s`, NA for rows without a key), and
# counts for the checks. Keys stay internal.
fidelity_align <- function(orig, copy, key, intent) {
  owners_o <- lapply(orig, fidelity_owners, key = key)
  owners_c <- lapply(copy, fidelity_owners, key = key)
  keyed <- names(orig)[vapply(names(orig), function(ds) {
    !is.null(owners_o[[ds]]) && !is.null(owners_c[[ds]])
  }, logical(1))]
  out <- list(
    keyed = keyed, pairs = list(), order_checked = list(), order_failed = list(),
    owners_o = owners_o[keyed], owners_c = owners_c[keyed]
  )
  # Datasets without the key pair by position.
  for (ds in setdiff(names(orig), keyed)) {
    out$pairs[[ds]] <- if (nrow(orig[[ds]]) == nrow(copy[[ds]])) {
      data.frame(o = seq_len(nrow(orig[[ds]])), c = seq_len(nrow(copy[[ds]])), s = rep(NA_character_, nrow(orig[[ds]])))
    } else {
      data.frame(o = integer(0), c = integer(0), s = character(0))
    }
  }
  if (length(keyed) == 0) {
    return(out)
  }

  per <- lapply(stats::setNames(keyed, keyed), function(ds) {
    o <- orig[[ds]]
    c <- copy[[ds]]
    all_cols <- fidelity_stable_columns(ds, o, c, key, intent, containment = !intent$step)
    stable_cols <- fidelity_stable_columns(ds, o, c, key, intent, containment = TRUE)
    full <- fidelity_row_codes(o, c, all_cols)
    stable <- if (identical(all_cols, stable_cols)) full else fidelity_row_codes(o, c, stable_cols)
    # Related IDs (RSUBJID) of each row: rows referring to a dropped subject
    # are dropped with it.
    refs <- lapply(intersect(intent$key_cols, names(o)), function(col) {
      if (is.list(o[[col]])) NULL else fidelity_keys(o[[col]])
    })
    list(
      ko = owners_o[[ds]], kc = owners_c[[ds]], fo = full$o, fc = full$c, co = stable$o, cc = stable$c,
      refs = Filter(Negate(is.null), refs)
    )
  })
  subjects_o <- unique(stats::na.omit(unlist(lapply(per, `[[`, "ko"), use.names = FALSE)))
  subjects_c <- unique(stats::na.omit(unlist(lapply(per, `[[`, "kc"), use.names = FALSE)))
  # Each subject's row codes in each dataset, sorted (which rows) and in
  # order (row order).
  signatures <- function(keys, codes, subjects, ordered) {
    ok <- !is.na(keys)
    groups <- split(codes[ok], factor(keys[ok], levels = subjects))
    vapply(groups, function(v) paste(if (ordered) v else sort(v), collapse = ","), character(1))
  }
  # A subject's sorted row codes in every dataset, as one text.
  global <- function(codes, subjects) {
    if (length(subjects) == 0) return(stats::setNames(character(0), character(0)))
    sigs <- lapply(codes, function(by_subject) {
      paste0("[", vapply(by_subject, function(v) paste(sort(v), collapse = ","), character(1)), "]")
    })
    stats::setNames(do.call(paste, c(unname(sigs), sep = "|")), subjects)
  }
  split_codes <- function(codes, keys, subjects) split(codes, factor(keys, levels = subjects))
  full_c <- lapply(per, function(p) split_codes(p$fc, p$kc, subjects_c))
  stable_c <- lapply(per, function(p) split_codes(p$cc, p$kc, subjects_c))
  global_full_c <- global(full_c, subjects_c)
  global_stable_c <- global(stable_c, subjects_c)
  secret <- intent$map
  use_secret <- !is.null(secret) && all(subjects_o %in% secret$key) && all(subjects_c %in% secret$pseudonym)
  by_key <- use_secret || (!intent$pseudonymized && all(subjects_c %in% subjects_o))
  out$how <- if (use_secret) {
    "by the step's kept pseudonym map"
  } else if (by_key) {
    "by key (keys were not pseudonymized)"
  } else {
    "by the rows of the columns no step changed"
  }
  sizes <- function(codes, subject) vapply(codes, function(x) length(x[[subject]]), integer(1))

  # Pairs subjects using the original's rows in `kept`, by dataset, in turn:
  # 1. subjects whose rows match exactly in every column no step logged;
  # 2. of the rest, an original and a copy subject with as many rows in every
  #    dataset, when more than half of their rows match position by position
  #    and no other subject matches as well (`near`);
  # 3. of the rest, subjects whose rows match in the columns that kept the
  #    original's values (a column changed throughout is left out);
  # 4. of the rest, subjects alike in those columns, by their own dates.
  # The values check then finds what differs between paired rows.
  pair_subjects <- function(kept) {
    full_o <- lapply(keyed, function(ds) split_codes(per[[ds]]$fo[kept[[ds]]], per[[ds]]$ko[kept[[ds]]], subjects_o))
    stable_o <- lapply(keyed, function(ds) split_codes(per[[ds]]$co[kept[[ds]]], per[[ds]]$ko[kept[[ds]]], subjects_o))
    names(full_o) <- names(stable_o) <- keyed
    global_full_o <- global(full_o, subjects_o)
    global_stable_o <- global(stable_o, subjects_o)
    res <- list(
      map = stats::setNames(rep(NA_character_, length(subjects_o)), subjects_o), dropped = character(0),
      near = character(0), n_unmatched = 0L, n_mismatched = 0L, n_surplus = 0L, n_alike = 0L, n_pattern = 0L,
      n_class = 0L, n_inconsistent = 0L
    )
    if (by_key) {
      target <- if (use_secret) secret$pseudonym[match(subjects_o, secret$key)] else subjects_o
      present <- target %in% subjects_c
      res$dropped <- subjects_o[!present]
      same <- present & global_full_o == global_full_c[match(target, subjects_c)]
      res$map[same] <- target[same]
      # A subject whose rows differ keeps its pseudonym when it has as many
      # rows in every dataset, or fewer (rows lost); otherwise its pseudonym
      # holds other rows.
      for (a in subjects_o[present & !same]) {
        b <- target[match(a, subjects_o)]
        if (fidelity_rows_lost(sizes(full_o, a), sizes(full_c, b))) {
          res$map[a] <- b
          res$near <- c(res$near, a)
        } else {
          res$n_mismatched <- res$n_mismatched + 1L
        }
      }
      res$n_unmatched <- sum(!subjects_c %in% target)
      return(res)
    }
    left_o <- subjects_o
    left_c <- subjects_c
    take <- function(pairs) {
      res$map[names(pairs)] <<- pairs
      left_o <<- setdiff(left_o, names(pairs))
      left_c <<- setdiff(left_c, pairs)
    }
    # Pairs of subjects whose signature only they hold, on each side.
    unique_pairs <- function(go, gc) {
      go <- go[left_o]
      gc <- gc[left_c]
      uo <- go[!go %in% go[duplicated(go)]]
      uc <- gc[!gc %in% gc[duplicated(gc)]]
      both <- intersect(uo, uc)
      stats::setNames(names(uc)[match(both, uc)], names(uo)[match(both, uo)])
    }
    take(unique_pairs(global_full_o, global_full_c))
    near <- fidelity_near_pairs(left_o, left_c, full_o, full_c, keyed, sizes)
    res$near <- names(near)
    take(near)
    take(unique_pairs(global_stable_o, global_stable_c))

    class_o <- split(left_o, factor(global_stable_o[left_o], levels = unique(global_stable_o[left_o])))
    class_c <- split(left_c, factor(global_stable_c[left_c], levels = unique(global_stable_c[left_c])))
    alike <- intersect(names(class_o), names(class_c))
    # Subjects alike in those columns are told apart by their own dates: each
    # date's distance from the subject's first date, and its format, which
    # the per-subject offset keeps.
    if (length(alike) > 0) {
      fp_o <- fidelity_fingerprints(orig, owners_o, unlist(class_o[alike]), keyed, key, intent, kept)
      fp_c <- fidelity_fingerprints(copy, owners_c, unlist(class_c[alike]), keyed, key, intent)
    }
    for (sig in names(class_o)) {
      a <- class_o[[sig]]
      b <- class_c[[sig]]
      if (length(b) == 0) {
        res$dropped <- c(res$dropped, a)
        next
      }
      fa <- fp_o[a]
      fb <- fp_c[b]
      # Pairs: a date pattern held by one original and one copy subject.
      once <- intersect(fa[!fa %in% fa[duplicated(fa)]], fb[!fb %in% fb[duplicated(fb)]])
      res$map[a[fa %in% once]] <- b[match(fa[fa %in% once], fb)]
      a <- a[!fa %in% once]
      b <- b[!fb %in% once]
      fa <- fp_o[a]
      fb <- fp_c[b]
      # Subjects with one date pattern, as many in the copy as in the
      # original, are alike in everything the checks compare but the cells a
      # step logged: they are paired as a class, each copy subject with an
      # original whose values explain its own (see fidelity_class_pairs()).
      done_a <- character(0)
      done_b <- character(0)
      for (fp in unique(fa)) {
        aa <- a[fa == fp]
        bb <- b[fb == fp]
        if (length(aa) != length(bb)) next
        paired <- fidelity_class_pairs(aa, bb, orig, copy, owners_o, owners_c, keyed, key, intent, kept)
        if (is.null(paired)) next
        res$map[names(paired)] <- paired
        res$n_class <- res$n_class + sum(!is.na(paired))
        res$n_inconsistent <- res$n_inconsistent + sum(is.na(paired))
        done_a <- c(done_a, aa)
        done_b <- c(done_b, bb)
      }
      a <- setdiff(a, done_a)
      b <- setdiff(b, done_b)
      fa <- fp_o[a]
      fb <- fp_c[b]
      # A copy subject whose date pattern no original subject of its kind
      # has was changed; as many originals as are missing count as dropped,
      # those whose pattern the copy lacks first; the rest can't be told
      # apart and aren't compared row by row.
      levels <- unique(c(fa, fb))
      res$n_pattern <- res$n_pattern + sum(pmax(table(factor(fb, levels)) - table(factor(fa, levels)), 0))
      n_drop <- max(length(a) - length(b), 0)
      if (n_drop > 0) {
        res$dropped <- c(res$dropped, utils::head(a[order(fa %in% fb)], n_drop))
      }
      res$n_alike <- res$n_alike + min(length(a), length(b))
      res$n_surplus <- res$n_surplus + max(length(b) - length(a), 0)
    }
    res$n_unmatched <- length(unlist(class_c[setdiff(names(class_c), names(class_o))]))
    res
  }
  # Rows that refer to a dropped subject in a related ID are dropped too, so
  # their owners are paired without them: pair again until the dropped
  # subjects no longer change.
  kept <- lapply(per, function(p) rep(TRUE, length(p$ko)))
  for (i in 1:5) {
    res <- pair_subjects(kept)
    now <- lapply(per, function(p) {
      gone <- rep(FALSE, length(p$ko))
      for (r in p$refs) gone <- gone | (r %in% res$dropped & !p$ko %in% res$dropped)
      !gone
    })
    if (identical(now, kept)) break
    kept <- now
  }
  map <- res$map
  dropped <- res$dropped
  n_unmatched <- res$n_unmatched
  n_mismatched <- res$n_mismatched
  n_surplus <- res$n_surplus
  n_alike <- res$n_alike
  out$n_subjects <- length(subjects_o)
  out$n_aligned <- sum(!is.na(map))
  out$n_alike <- n_alike
  out$n_unmatched <- n_unmatched
  out$n_mismatched <- n_mismatched
  out$n_surplus <- n_surplus
  out$n_near <- length(res$near)
  out$n_pattern <- res$n_pattern
  out$n_class <- res$n_class
  out$n_inconsistent <- res$n_inconsistent
  out$dropped <- dropped

  # Rows pair in order within each paired subject whose rows are in the same
  # order; rows without a key pair in order when there are as many.
  mapped <- names(map)[!is.na(map)]
  for (ds in keyed) {
    p <- per[[ds]]
    # Rows referring to a dropped subject aren't paired.
    ko <- ifelse(kept[[ds]], p$ko, NA)
    rows_o <- split(seq_along(ko), factor(ko, levels = subjects_o))
    rows_c <- split(seq_along(p$kc), factor(p$kc, levels = subjects_c))
    ord_o <- signatures(ko, p$co, subjects_o, TRUE)
    ord_c <- signatures(p$kc, p$cc, subjects_c, TRUE)
    here <- mapped[lengths(rows_o[mapped]) > 0]
    # Subjects paired by most of their rows pair row by row in order (the
    # values check finds their rows that differ); their order can't be
    # checked.
    near <- intersect(here, res$near)
    exact <- setdiff(here, near)
    in_order <- ord_o[exact] == ord_c[map[exact]]
    out$order_checked[[ds]] <- length(exact)
    out$order_failed[[ds]] <- sum(!in_order)
    # A near pair whose copy lost rows pairs each copy row with the first
    # unpaired original row of the same code (full columns), in order.
    lost <- near[lengths(rows_c[map[near]]) < lengths(rows_o[near])]
    good <- c(exact[in_order], setdiff(near, lost))
    o_idx <- unlist(rows_o[good], use.names = FALSE)
    c_idx <- unlist(rows_c[map[good]], use.names = FALSE)
    s_idx <- rep(good, lengths(rows_o[good]))
    for (a in lost) {
      ro <- rows_o[[a]]
      rc <- rows_c[[map[[a]]]]
      at <- fidelity_match_rows(p$fo[ro], p$fc[rc])
      o_idx <- c(o_idx, ro[at[!is.na(at)]])
      c_idx <- c(c_idx, rc[!is.na(at)])
      s_idx <- c(s_idx, rep(a, sum(!is.na(at))))
    }
    none_o <- which(is.na(p$ko))
    none_c <- which(is.na(p$kc))
    if (length(none_o) > 0 && length(none_o) == length(none_c)) {
      o_idx <- c(o_idx, none_o)
      c_idx <- c(c_idx, none_c)
      s_idx <- c(s_idx, rep(NA_character_, length(none_o)))
    }
    out$pairs[[ds]] <- data.frame(o = o_idx, c = c_idx, s = s_idx, stringsAsFactors = FALSE)
    # Rows of dropped subjects go, and so do rows that refer to one in a
    # related ID column (RSUBJID).
    gone <- p$ko %in% dropped | !kept[[ds]]
    out$dropped_rows[[ds]] <- sum(gone)
    out$dropped_subjects[[ds]] <- sum(unique(stats::na.omit(p$ko)) %in% dropped)
    out$kept_subjects[[ds]] <- length(unique(stats::na.omit(p$ko[!gone])))
  }
  out
}

# Near pairs among the subjects left unpaired: an original and a copy subject
# with as many rows in every dataset, or fewer in the copy (rows lost, see
# fidelity_rows_lost()), more than half of the original's row codes (and
# codes of two rows in a row) found in the copy's, each the other's only best
# match. Candidates share informative codes (held by at most
# `fidelity_near_cap` of the left-over original subjects), which keeps this
# linear in the number of rows; subjects with none stay unpaired and are
# reported. Returns the copy subject of each paired original, by name.
fidelity_near_pairs <- function(left_o, left_c, codes_o, codes_c, keyed, sizes) {
  found <- stats::setNames(character(0), character(0))
  if (length(left_o) == 0 || length(left_c) == 0) {
    return(found)
  }
  # Each subject's row codes, and codes of two rows in a row (much rarer
  # than single codes when values repeat across subjects), with how often it
  # holds each.
  tokens <- function(codes, subjects) {
    parts <- lapply(seq_along(keyed), function(k) {
      v <- codes[[keyed[k]]][subjects]
      n <- lengths(v)
      if (sum(n) == 0) {
        return(data.frame(s = character(0), token = character(0), stringsAsFactors = FALSE))
      }
      flat <- unlist(v, use.names = FALSE)
      owner <- rep(subjects, n)
      last <- cumsum(n)
      follows <- setdiff(seq_along(flat), last)
      data.frame(
        s = c(owner, owner[follows]),
        token = c(paste(k, flat), if (length(follows) > 0) paste(k, flat[follows], flat[follows + 1L], sep = "-")),
        stringsAsFactors = FALSE
      )
    })
    t <- do.call(rbind, parts)
    counted <- vctrs::vec_count(t)
    data.frame(s = counted$key$s, token = counted$key$token, n = counted$count, stringsAsFactors = FALSE)
  }
  to <- tokens(codes_o, left_o)
  tc <- tokens(codes_c, left_c)
  total <- tapply(to$n, to$s, sum)
  holders <- table(to$token)
  to <- to[to$token %in% names(holders)[holders <= fidelity_near_cap], , drop = FALSE]
  hits <- merge(to, tc, by = "token", suffixes = c("_o", "_c"))
  if (nrow(hits) == 0) {
    return(found)
  }
  hits$n <- pmin(hits$n_o, hits$n_c)
  counts <- stats::aggregate(n ~ s_o + s_c, data = hits, FUN = sum)
  size_o <- lapply(stats::setNames(left_o, left_o), function(a) sizes(codes_o, a))
  size_c <- lapply(stats::setNames(left_c, left_c), function(b) sizes(codes_c, b))
  fits <- mapply(function(a, b) fidelity_rows_lost(size_o[[a]], size_c[[b]]), counts$s_o, counts$s_c)
  counts <- counts[fits & 2 * counts$n > total[counts$s_o], , drop = FALSE]
  if (nrow(counts) == 0) {
    return(found)
  }
  best_o <- tapply(counts$n, counts$s_o, max)
  best_c <- tapply(counts$n, counts$s_c, max)
  top <- counts[counts$n == best_o[counts$s_o] & counts$n == best_c[counts$s_c], , drop = FALSE]
  top <- top[!top$s_o %in% top$s_o[duplicated(top$s_o)] & !top$s_c %in% top$s_c[duplicated(top$s_c)], , drop = FALSE]
  stats::setNames(top$s_c, top$s_o)
}

# Whether a copy subject's rows by dataset (`c`) are the original's (`o`) or
# fewer, still in every dataset where the original had some: rows lost, not
# a subject gone from a dataset.
fidelity_rows_lost <- function(o, c) all(c <= o & (o == 0 | c > 0))

# For each copy row code, the position of an original row with the same
# code, each original row used once, in order; NA where none is left.
fidelity_match_rows <- function(o, c) {
  out <- rep(NA_integer_, length(c))
  used <- rep(FALSE, length(o))
  for (j in seq_along(c)) {
    i <- which(!used & o == c[j])[1]
    if (!is.na(i)) {
      out[j] <- i
      used[i] <- TRUE
    }
  }
  out
}

# Pairs a class of original subjects (`a`) with as many copy subjects (`b`)
# that are alike in every column no step logged and in their date pattern.
# The checks give the same result whichever way such subjects pair, apart
# from the cells a step logged, so each copy subject is paired with an
# original whose cells explain its own: every compared cell (the values
# check's columns, in every keyed dataset, row by row) must equal the
# original's, unless the step may have changed it there (blanked where a
# blanking step logged it, generalized, folded to the rule's category, or
# derived again), which matches any original value. Subjects are grouped by
# their cells, and a maximum flow between the groups gives a pairing that
# explains every copy subject when there is one, assigned in order of the
# subjects' positions. Returns the copy subject of each original (NA for
# those no pairing explains: a change), or NULL when the groups are too many
# to compare (the class then stays unpaired).
fidelity_class_pairs <- function(a, b, orig, copy, owners_o, owners_c, keyed, key, intent, kept) {
  cells <- function(data, owners, subjects, side) {
    parts <- list()
    for (ds in keyed) {
      df <- data[[ds]]
      own <- owners[[ds]]
      rows <- which(!is.na(own) & own %in% subjects & (if (side == "o") kept[[ds]] else TRUE))
      if (length(rows) == 0) next
      skip <- c(key, intent$ids, fidelity_text_date_columns(df, key, intent, ds))
      for (col in setdiff(names(df), skip)) {
        x <- df[[col]]
        if (is.list(x) || !is.null(dim(x)) || fidelity_is_date(x)) next
        v <- if (fidelity_is_text(x)) fidelity_text(x) else as.character(fidelity_plain(x))
        v[fidelity_blank(v)] <- ""
        v <- v[rows]
        if (side == "c" && intent$step) {
          blank <- v == ""
          free <- (blank & fidelity_logged_rows(intent, ds, col, df, steps = c("remove", "suppress", "deidentify", "generalize"))[rows]) |
            fidelity_rewritten_rows(intent, ds, col, df)[rows] |
            (!is.na(fidelity_fold_values(intent, ds, col, df)[rows]) & v == fidelity_fold_values(intent, ds, col, df)[rows]) |
            fidelity_logged_rows(intent, ds, col, df, steps = "deidentify", derived = TRUE)[rows]
          v[free] <- "\r*"
        }
        parts[[length(parts) + 1]] <- data.frame(s = own[rows], v = v, stringsAsFactors = FALSE)
      }
    }
    if (length(parts) == 0) {
      return(stats::setNames(rep("", length(subjects)), subjects))
    }
    long <- do.call(vctrs::vec_rbind, parts)
    out <- tapply(long$v, factor(long$s, levels = subjects), paste, collapse = "\x1f")
    stats::setNames(ifelse(is.na(out), "", out), subjects)
  }
  ta <- cells(orig, owners_o, a, "o")
  tb <- cells(copy, owners_c, b, "c")
  ga <- unique(ta)
  gb <- unique(tb)
  if (length(ga) * length(gb) > 250000) {
    return(NULL)
  }
  split_cells <- function(t) strsplit(t, "\x1f", fixed = TRUE)
  ca <- split_cells(ga)
  cb <- split_cells(gb)
  fits <- matrix(FALSE, length(ga), length(gb))
  for (j in seq_along(gb)) {
    wild <- cb[[j]] == "\r*"
    fits[, j] <- vapply(ca, function(x) {
      length(x) == length(cb[[j]]) && all(wild | x == cb[[j]])
    }, logical(1))
  }
  flow <- fidelity_max_flow(as.vector(table(factor(ta, ga))), as.vector(table(factor(tb, gb))), fits)
  out <- stats::setNames(rep(NA_character_, length(a)), a)
  left_a <- split(a, factor(ta, ga))
  left_b <- split(b, factor(tb, gb))
  for (i in seq_along(ga)) {
    for (j in which(flow[i, ] > 0)) {
      k <- flow[i, j]
      out[left_a[[i]][seq_len(k)]] <- left_b[[j]][seq_len(k)]
      left_a[[i]] <- left_a[[i]][-seq_len(k)]
      left_b[[j]] <- left_b[[j]][-seq_len(k)]
    }
  }
  out
}

# Maximum flow from groups of originals (`supply`, subjects per group) to
# groups of copies (`demand`) along the pairs `fits` allows, by augmenting
# paths. Returns the flow by pair of groups.
fidelity_max_flow <- function(supply, demand, fits) {
  flow <- matrix(0L, length(supply), length(demand))
  left_s <- supply
  left_d <- demand
  repeat {
    # Breadth-first search from the groups of originals with subjects left,
    # through fitting pairs forward and flow backward, to a group of copies
    # with subjects left.
    from_s <- rep(NA_integer_, length(supply))
    from_d <- rep(NA_integer_, length(demand))
    seen_s <- left_s > 0
    queue <- which(seen_s)
    end <- NA_integer_
    while (length(queue) > 0 && is.na(end)) {
      i <- queue[1]
      queue <- queue[-1]
      for (j in which(fits[i, ] & is.na(from_d))) {
        from_d[j] <- i
        if (left_d[j] > 0) {
          end <- j
          break
        }
        for (i2 in which(flow[, j] > 0 & !seen_s)) {
          seen_s[i2] <- TRUE
          from_s[i2] <- j
          queue <- c(queue, i2)
        }
      }
    }
    if (is.na(end)) break
    j <- end
    left_d[j] <- left_d[j] - 1L
    repeat {
      i <- from_d[j]
      flow[i, j] <- flow[i, j] + 1L
      if (is.na(from_s[i])) {
        left_s[i] <- left_s[i] - 1L
        break
      }
      j <- from_s[i]
      flow[i, j] <- flow[i, j] - 1L
    }
  }
  flow
}

# Each subject's dates as a pattern that the per-subject offset keeps: for
# every date cell of the subject (keyed datasets, date columns and rows in
# order), its format with the digits masked, and for a full date its
# distance in days from the subject's first full date and its clock time.
# Cells a step may blank or remove on purpose (columns another step logged,
# values that aren't ISO 8601, partial dates with partial = "remove") count
# as blank, so the pattern is the same in the original and the copy.
fidelity_fingerprints <- function(data, owners, subjects, keyed, key, intent, kept = NULL) {
  pieces <- list()
  for (ds in keyed) {
    df <- data[[ds]]
    own <- owners[[ds]]
    use <- !is.na(own) & own %in% subjects
    if (!is.null(kept)) use <- use & kept[[ds]]
    if (!any(use)) next
    cols <- c(fidelity_text_date_columns(df, key, intent, ds), names(df)[vapply(df, fidelity_is_date, logical(1))])
    lg <- intent$logged
    cols <- setdiff(cols, lg$column[lg$dataset == ds & lg$step != "subject_dates"])
    for (col in cols) {
      x <- df[[col]][use]
      if (inherits(x, "POSIXlt")) x <- as.POSIXct(x)
      if (inherits(x, "Date")) {
        day <- as.numeric(x)
        text <- ifelse(is.na(x), "-", "D")
      } else if (inherits(x, "POSIXct")) {
        tz <- attr(x, "tzone", exact = TRUE)
        tz <- if (is.null(tz) || length(tz) == 0) "" else tz[[1]]
        day <- as.numeric(as.Date(format(x, "%Y-%m-%d", tz = tz)))
        text <- ifelse(is.na(x), "-", format(x, "%H:%M:%OS6", tz = tz))
      } else {
        x <- fidelity_text(x)
        parsed <- fidelity_parse_dates(x)
        full <- parsed$kind == "full"
        day <- ifelse(full, as.numeric(parsed$reference), NA)
        text <- ifelse(full, paste0(fidelity_date_shape(substr(x, 1, 10)), substring(x, 11)), fidelity_date_shape(x))
        gone <- fidelity_blank(x) | parsed$kind == "invalid" |
          (intent$partial == "remove" & is_partial_date(x, parsed$kind))
        text[gone] <- "-"
        day[gone] <- NA
      }
      pieces[[length(pieces) + 1]] <- data.frame(s = own[use], day = day, text = text, stringsAsFactors = FALSE)
    }
  }
  out <- stats::setNames(rep("", length(subjects)), subjects)
  if (length(pieces) == 0) {
    return(out)
  }
  cells <- do.call(vctrs::vec_rbind, pieces)
  first <- tapply(cells$day, cells$s, function(d) if (all(is.na(d))) NA_real_ else d[!is.na(d)][1])
  cells$text <- ifelse(is.na(cells$day), cells$text, paste0(cells$text, "@", cells$day - first[cells$s]))
  fp <- tapply(cells$text, factor(cells$s, levels = subjects), paste, collapse = "|")
  out[names(fp)] <- ifelse(is.na(fp), "", fp)
  out
}

fidelity_check_rows <- function(ds, o, c, align, intent) {
  if (!ds %in% align$keyed) {
    n_failed <- abs(nrow(o) - nrow(c))
    return(fidelity_row(ds, "rows", fidelity_status(n_failed), nrow(o), n_failed,
      paste0(nrow(o), " row(s) in the original, ", nrow(c), " in the copy")))
  }
  drop_rows <- align$dropped_rows[[ds]]
  drop_subjects <- align$dropped_subjects[[ds]]
  expected <- nrow(o) - drop_rows
  n_failed <- abs(nrow(c) - expected)
  # Dropping is intended only when the step says it dropped as many subjects.
  recorded <- !intent$step || intent$n_dropped == length(align$dropped)
  # Rows of subjects that couldn't be paired aren't compared row by row.
  unpaired <- max(nrow(o) - drop_rows - nrow(align$pairs[[ds]]), 0L)
  detail <- paste0(nrow(o), " row(s) in the original, ", nrow(c), " in the copy")
  if (drop_subjects > 0) {
    detail <- paste0(
      detail, "; ", drop_rows, " row(s) of ", drop_subjects, " subject(s) missing from the copy",
      if (intent$step) paste0(" (the step dropped ", intent$n_dropped, " subject(s))")
    )
  } else if (drop_rows > 0) {
    detail <- paste0(detail, "; ", drop_rows, " row(s) missing from the copy")
  }
  if (nrow(c) < expected) {
    detail <- paste0(detail, "; ", expected - nrow(c), " more row(s) missing from the copy, of subjects it kept")
  } else if (nrow(c) > expected) {
    detail <- paste0(detail, "; ", nrow(c) - expected, " row(s) more than expected")
  }
  if (unpaired > 0) {
    detail <- paste0(detail, "; ", unpaired, " row(s) of subjects that couldn't be paired, not compared row by row")
  }
  status <- if (n_failed > 0 || (drop_rows > 0 && !recorded)) "fail" else if (drop_rows > 0) "warn" else "pass"
  fidelity_row(ds, "rows", status, nrow(o), if (status == "fail") max(n_failed, drop_rows) else 0L, detail,
    skipped = unpaired)
}

fidelity_check_order <- function(ds, align) {
  checked <- align$order_checked[[ds]]
  failed <- align$order_failed[[ds]]
  fidelity_row(ds, "row order", fidelity_status(failed), checked, failed,
    paste0(checked, " paired subject(s); ", failed, " with rows in another order"))
}

fidelity_check_keys_in_dm <- function(ds, align, dm) {
  if (identical(ds, dm) || !dm %in% align$keyed) {
    return(NULL)
  }
  missing <- function(owners) {
    k <- owners[[ds]]
    sum(!is.na(k) & !k %in% owners[[dm]])
  }
  in_orig <- missing(align$owners_o)
  in_copy <- missing(align$owners_c)
  n <- sum(!is.na(align$owners_c[[ds]]))
  if (in_orig > 0) {
    return(fidelity_row(ds, "keys in dm", "not applicable", n, in_copy,
      paste0(in_orig, " row(s) of the original have a key not in `", dm, "`; ", in_copy, " in the copy")))
  }
  fidelity_row(ds, "keys in dm", fidelity_status(in_copy), n, in_copy,
    paste0(in_copy, " of ", n, " keyed row(s) with a key not in `", dm, "`"))
}

fidelity_check_distinct_keys <- function(ds, align) {
  n_o <- length(unique(stats::na.omit(align$owners_o[[ds]])))
  n_c <- length(unique(stats::na.omit(align$owners_c[[ds]])))
  dropped <- align$dropped_subjects[[ds]]
  expected <- align$kept_subjects[[ds]]
  n_failed <- abs(n_c - expected)
  fidelity_row(ds, "distinct keys", fidelity_status(n_failed), n_o, n_failed, paste0(
    n_o, " subject(s) in the original, ", n_c, " in the copy, ", dropped, " dropped",
    if (expected < n_o - dropped) paste0("; ", n_o - dropped - expected, " more with all rows referring to a dropped subject")
  ))
}

# Related IDs that end with the key in the original (SUBJID "1015" of
# USUBJID "01-701-1015") must end with the pseudonym in the copy, row by row:
# rows given another subject's key (or IDs) fail.
fidelity_related_ids <- function(orig, copy, align, intent) {
  checked <- 0L
  failed <- 0L
  for (ds in align$keyed) {
    for (col in intersect(intent$also, intersect(names(orig[[ds]]), names(copy[[ds]])))) {
      if (is.list(orig[[ds]][[col]]) || is.list(copy[[ds]][[col]])) next
      held <- function(df, owners) {
        id <- fidelity_keys(df[[col]])
        if (length(id) != length(owners)) return(list(n = 0L, bad = 1L))
        ok <- !is.na(id) & !is.na(owners)
        list(n = sum(ok), bad = sum(!endsWith(owners[ok], id[ok])))
      }
      o <- held(orig[[ds]], align$owners_o[[ds]])
      if (o$n == 0 || o$bad > 0) next
      c <- held(copy[[ds]], align$owners_c[[ds]])
      checked <- checked + c$n
      failed <- failed + c$bad
    }
  }
  list(checked = checked, failed = failed)
}

fidelity_check_pseudonyms <- function(align, related) {
  failed <- align$n_unmatched + align$n_mismatched + align$n_surplus + align$n_pattern + align$n_inconsistent +
    related$failed
  detail <- paste0(
    align$n_aligned, " of ", align$n_subjects, " subject(s) paired ", align$how,
    if (align$n_alike > 0) paste0(
      "; ", align$n_alike, " alike in every unchanged column and in their dates, not compared row by row"
    ),
    if (align$n_class > 0) paste0(
      "; ", align$n_class, " of them alike in every unchanged column and in their dates, paired as a class"
    ),
    if (align$n_inconsistent > 0) paste0(
      "; ", align$n_inconsistent, " alike subject(s) whose values no pairing within their class explains"
    ),
    if (align$n_pattern > 0) paste0(
      "; ", align$n_pattern, " copy subject(s) whose dates match no original subject alike in the other columns"
    ),
    if (length(align$dropped) > 0) paste0("; ", length(align$dropped), " missing from the copy"),
    if (align$n_near > 0) paste0("; ", align$n_near, " paired by most of their rows (see the values check)"),
    if (align$n_unmatched > 0) paste0("; ", align$n_unmatched, " copy subject(s) whose rows match no original subject"),
    if (align$n_mismatched > 0) paste0("; ", align$n_mismatched, " subject(s) whose pseudonym holds another subject's rows"),
    if (align$n_surplus > 0) paste0("; ", align$n_surplus, " copy subject(s) more than the original's"),
    if (related$checked > 0) paste0(
      "; related IDs follow the key in ", related$checked - related$failed, " of ", related$checked, " row(s)"
    )
  )
  status <- if (failed > 0) "fail" else if (align$n_alike > 0) "warn" else "pass"
  fidelity_row("(all)", "pseudonyms", status, align$n_subjects, failed, detail, skipped = align$n_alike)
}

# Fold categories a generalization rule gives `col` of `ds`, by row of `df`.
fidelity_fold_values <- function(intent, ds, col, df) {
  out <- rep(NA_character_, nrow(df))
  for (r in intent$rules) {
    if (!identical(r$rule, "fold") || !identical(r$dataset, ds) || is.null(r$to)) next
    if (identical(r$column, col)) {
      out[] <- r$to
    } else if (identical(r$result_col, col) && !is.null(r$test_col) && !is.null(df[[r$test_col]])) {
      hit <- fidelity_text(df[[r$test_col]]) %in% r$test
      out[hit] <- r$to
    }
  }
  out
}

# Rows of `df` whose `col` a generalization step rewrote with a rule that
# isn't a fold (numbers rewritten as text, such as `VSSTRESC` from a banded
# `VSSTRESN`).
fidelity_rewritten_rows <- function(intent, ds, col, df) {
  lg <- intent$logged
  lg <- lg[lg$step == "generalize" & lg$dataset == ds & lg$column == col, , drop = FALSE]
  out <- rep(FALSE, nrow(df))
  for (i in seq_len(nrow(lg))) {
    fold <- any(vapply(intent$rules, function(r) {
      identical(r$rule, "fold") && identical(r$dataset, ds) &&
        (if (is.na(lg$test[i])) identical(r$column, col) else identical(r$test, lg$test[i]))
    }, logical(1)))
    if (fold) next
    if (is.na(lg$test_col[i])) {
      out[] <- TRUE
    } else if (!is.null(df[[lg$test_col[i]]])) {
      out <- out | fidelity_text(df[[lg$test_col[i]]]) %in% lg$test[i]
    }
  }
  out
}

# Every column that no step logged as changed keeps its values, row by row
# over the paired rows (the key, related IDs and dates have checks of their
# own). Cells a step logged may be blank where it blanked them, and changed
# where it generalized them (a fold only to its category) or derived them
# again. Columns logged for one test only are compared on the other rows.
# Packed data frame columns are compared the same way, their rows being
# their parent's; data frames in list columns by their number of rows, and by
# their values when their rows keep their order (no key column, or one
# subject).
fidelity_check_values <- function(ds, o, c, pairs, key, intent, dropped = character(0)) {
  res <- list(cells = 0L, blanked = 0L, changed = 0L, bad = integer(0), rows = 0L)
  blank_steps <- c("remove", "suppress", "deidentify", "generalize")
  compare <- function(x, y, name, where, df_c, rows_c) {
    a <- if (fidelity_is_text(x)) fidelity_text(x) else fidelity_plain(x)
    b <- if (fidelity_is_text(y)) fidelity_text(y) else fidelity_plain(y)
    if (is.character(a) != is.character(b)) {
      a <- as.character(a)
      b <- as.character(b)
    }
    blank_a <- if (is.character(a)) fidelity_blank(a) else is.na(a)
    blank_b <- if (is.character(b)) fidelity_blank(b) else is.na(b)
    same <- (blank_a & blank_b) | (!blank_a & !blank_b & a == b)
    same[is.na(same)] <- FALSE
    diff <- !same
    res$cells <<- res$cells + length(a)
    if (!any(diff)) return(invisible())
    logged <- function(...) fidelity_logged_rows(intent, where, name, df_c, ...)[rows_c]
    blanked <- diff & blank_b & logged(steps = blank_steps)
    fold_to <- fidelity_fold_values(intent, where, name, df_c)[rows_c]
    changed <- diff & !blanked & !blank_b & (
      fidelity_rewritten_rows(intent, where, name, df_c)[rows_c] |
        (!is.na(fold_to) & as.character(b) == fold_to) |
        logged(steps = "deidentify", derived = TRUE)
    )
    if (!intent$step) {
      blanked[] <- FALSE
      changed[] <- FALSE
    }
    res$blanked <<- res$blanked + sum(blanked)
    res$changed <<- res$changed + sum(changed)
    n_bad <- sum(diff & !blanked & !changed)
    if (n_bad > 0) res$bad[name] <<- n_bad
  }
  skip <- function(df, ds_name) c(key, intent$ids, fidelity_text_date_columns(df, key, intent, ds_name))
  walk <- function(o, c, ro, rc, prefix, where) {
    for (col in setdiff(intersect(names(o), names(c)), skip(o, where))) {
      x <- o[[col]]
      y <- c[[col]]
      name <- paste0(prefix, col)
      if (fidelity_is_date(x) || fidelity_is_date(y)) next
      if (is.data.frame(x) && is.data.frame(y)) {
        walk(x, y, ro, rc, paste0(name, "$"), paste0(where, ".", col))
      } else if (is.list(x) && is.list(y)) {
        for (i in seq_along(ro)) {
          a <- x[[ro[i]]]
          b <- y[[rc[i]]]
          if (!is.data.frame(a) || !is.data.frame(b)) next
          res$rows <<- res$rows + 1L
          # Rows of dropped subjects go from nested data too.
          if (key %in% names(a) && length(dropped) > 0) {
            kept_rows <- !fidelity_keys(a[[key]]) %in% dropped
            if (!all(kept_rows)) {
              if (sum(kept_rows) != nrow(b)) {
                label <- paste0(name, "[[]] rows")
                res$bad[label] <<- (if (label %in% names(res$bad)) res$bad[[label]] else 0L) + 1L
              }
              next
            }
          }
          if (nrow(a) != nrow(b)) {
            label <- paste0(name, "[[]] rows")
            res$bad[label] <<- (if (label %in% names(res$bad)) res$bad[[label]] else 0L) + 1L
            next
          }
          ordered <- !key %in% names(a) || length(unique(stats::na.omit(fidelity_keys(a[[key]])))) <= 1
          if (ordered && nrow(a) > 0) {
            walk(a, b, seq_len(nrow(a)), seq_len(nrow(b)), paste0(name, "[[]]$"), paste0(where, ".", col, "[[]]"))
          }
        }
      } else if (!is.list(x) && !is.list(y) && is.null(dim(x)) && is.null(dim(y))) {
        compare(x[ro], y[rc], col, where, c, rc)
      }
    }
  }
  if (nrow(pairs) == 0) {
    return(NULL)
  }
  walk(o, c, pairs$o, pairs$c, "", ds)
  n_failed <- sum(res$bad)
  detail <- paste(c(
    paste0(res$cells, " value(s) of paired rows compared"),
    if (res$blanked > 0) paste0(res$blanked, " blanked on purpose"),
    if (res$changed > 0) paste0(res$changed, " generalized or derived on purpose"),
    if (n_failed > 0) paste0(
      n_failed, " differ", if (!intent$step) " (no step to say they are intended)", ": ",
      paste0(names(res$bad), " (", res$bad, ")", collapse = ", ")
    )
  ), collapse = "; ")
  fidelity_row(ds, "values", fidelity_status(n_failed, strict = intent$step), res$cells + res$rows, n_failed, detail)
}

fidelity_check_categorical <- function(ds, o, c, pairs, key, intent) {
  cols <- intersect(names(o), names(c))
  cols <- cols[vapply(cols, function(col) fidelity_is_text(o[[col]]) && fidelity_is_text(c[[col]]), logical(1))]
  cols <- setdiff(cols, c(key, intent$ids, fidelity_text_date_columns(c, key, intent, ds)))
  if (length(cols) == 0) {
    return(NULL)
  }
  checked <- 0L
  folded <- 0L
  rewritten <- 0L
  bad <- integer(0)
  for (col in cols) {
    v <- fidelity_text(c[[col]])
    present <- !fidelity_blank(v)
    new <- present & !v %in% fidelity_text(o[[col]])
    checked <- checked + sum(present)
    if (!any(new)) next
    fold_to <- fidelity_fold_values(intent, ds, col, c)
    fold <- new & !is.na(fold_to) & v == fold_to
    rewrite <- new & !fold & fidelity_rewritten_rows(intent, ds, col, c)
    folded <- folded + sum(fold)
    rewritten <- rewritten + sum(rewrite)
    n_bad <- sum(new & !fold & !rewrite)
    if (n_bad > 0) bad[col] <- n_bad
  }
  n_failed <- sum(bad)
  detail <- paste(c(
    paste0(checked, " non-blank value(s) in ", length(cols), " column(s)"),
    if (folded > 0) paste0(folded, " fold value(s) from the step's rules"),
    if (rewritten > 0) paste0(rewritten, " rewritten from generalized numbers"),
    if (n_failed > 0) paste0(
      n_failed, " value(s) not in the original column", if (!intent$step) " (no step to say they are intended)", ": ",
      paste0(names(bad), " (", bad, ")", collapse = ", ")
    )
  ), collapse = "; ")
  fidelity_row(ds, "categorical values", fidelity_status(n_failed, strict = intent$step), checked, n_failed, detail)
}

# Text with each digit as "9": equal for values of the same format and
# precision.
fidelity_date_shape <- function(x) {
  u <- unique(x)
  gsub("[0-9]", "9", u)[match(x, u)]
}

fidelity_check_iso <- function(ds, o, c, pairs, key, intent) {
  cols <- intersect(fidelity_text_date_columns(o, key, intent, ds), names(c))
  cols <- cols[vapply(cols, function(col) fidelity_is_text(c[[col]]), logical(1))]
  if (length(cols) == 0) {
    return(NULL)
  }
  checked <- 0L
  shape <- 0L
  appeared <- 0L
  removed_ok <- 0L
  removed_bad <- 0L
  failed_cols <- character(0)
  for (col in cols) {
    ov <- fidelity_text(o[[col]])[pairs$o]
    cv <- fidelity_text(c[[col]])[pairs$c]
    ob <- fidelity_blank(ov)
    cb <- fidelity_blank(cv)
    checked <- checked + sum(!ob)
    bad_shape <- !ob & !cb & fidelity_date_shape(ov) != fidelity_date_shape(cv)
    new <- ob & !cb
    removed <- !ob & cb
    if (any(removed)) {
      kinds <- parse_iso8601_dates(ov[removed])$kind
      # Intended removals: not ISO 8601 in the original, partial dates with
      # partial = "remove", rows without a key, cells a step blanked.
      blanked <- fidelity_logged_rows(intent, ds, col, c, steps = c("remove", "suppress", "deidentify"))[pairs$c][removed]
      ok <- kinds == "invalid" | (key %in% names(o) & is.na(pairs$s[removed])) | blanked |
        (intent$partial == "remove" & is_partial_date(ov[removed], kinds))
      if (!intent$step) ok <- rep(FALSE, length(ok))
      removed_ok <- removed_ok + sum(ok)
      removed_bad <- removed_bad + sum(!ok)
      if (any(!ok)) failed_cols <- c(failed_cols, col)
    }
    shape <- shape + sum(bad_shape)
    appeared <- appeared + sum(new)
    if (any(bad_shape | new)) failed_cols <- c(failed_cols, col)
  }
  n_failed <- shape + appeared + removed_bad
  detail <- paste(c(
    paste0(checked, " non-blank value(s) in ", length(cols), " column(s) of paired rows"),
    if (shape > 0) paste0(shape, " with another format or precision"),
    if (appeared > 0) paste0(appeared, " blank in the original but not in the copy"),
    if (removed_ok > 0) paste0(removed_ok, " removed on purpose"),
    if (removed_bad > 0) paste0(removed_bad, " removed", if (intent$step) " with no reason in the step" else ""),
    if (length(failed_cols) > 0) paste0("in ", paste(unique(failed_cols), collapse = ", "))
  ), collapse = "; ")
  status <- if (shape + appeared > 0) "fail" else fidelity_status(removed_bad, strict = intent$step)
  fidelity_row(ds, "ISO 8601", status, checked, n_failed, detail)
}

# Each date cell of paired rows as compared by the interval check: `type`
# "full" (with `delta`, days moved, and `same_time`), "month" (with `mid`,
# the original's day 15, and the copy's text), "year" (`years` moved,
# `same_rest`), "same" (values without a date, which must be kept), or "bad"
# (the copy's value isn't the same kind of date).
fidelity_date_cells <- function(ov, cv) {
  n <- length(ov)
  out <- data.frame(
    i = seq_len(n), type = rep(NA_character_, n), delta = rep(NA_real_, n), same_time = rep(TRUE, n),
    mid = rep(as.Date(NA), n), orig_text = rep(NA_character_, n), copy_text = rep(NA_character_, n),
    years = rep(NA_real_, n), ok = rep(TRUE, n), stringsAsFactors = FALSE
  )
  if (inherits(ov, "POSIXlt")) ov <- as.POSIXct(ov)
  if (inherits(cv, "POSIXlt")) cv <- as.POSIXct(cv)
  if (inherits(ov, "Date") && inherits(cv, "Date")) {
    use <- !is.na(ov) & !is.na(cv)
    out$type[use] <- "full"
    out$delta[use] <- as.numeric(cv[use]) - as.numeric(ov[use])
    return(out[use, , drop = FALSE])
  }
  if (inherits(ov, "POSIXct") && inherits(cv, "POSIXct")) {
    tz <- attr(ov, "tzone", exact = TRUE)
    tz <- if (is.null(tz) || length(tz) == 0) "" else tz[[1]]
    use <- !is.na(ov) & !is.na(cv)
    out$type[use] <- "full"
    od <- as.Date(format(ov[use], "%Y-%m-%d", tz = tz))
    cd <- as.Date(format(cv[use], "%Y-%m-%d", tz = tz))
    out$delta[use] <- as.numeric(cd - od)
    out$same_time[use] <- format(ov[use], "%H:%M:%OS6", tz = tz) == format(cv[use], "%H:%M:%OS6", tz = tz)
    return(out[use, , drop = FALSE])
  }
  if (!is.character(ov) || !is.character(cv)) {
    return(out[0, , drop = FALSE])
  }
  use <- !fidelity_blank(ov) & !fidelity_blank(cv)
  out <- out[use, , drop = FALSE]
  ov <- ov[use]
  cv <- cv[use]
  po <- fidelity_parse_dates(ov)
  pc <- fidelity_parse_dates(cv)
  # Both ends of intervals are cells of their own.
  intervals <- which(po$kind == "interval" & pc$kind == "interval")
  if (length(intervals) > 0) {
    split_end <- function(x, k) vapply(strsplit(x, "/", fixed = TRUE), function(p) p[k], character(1))
    ends <- vctrs::vec_rbind(
      fidelity_date_cells(split_end(ov[intervals], 1), split_end(cv[intervals], 1)),
      fidelity_date_cells(split_end(ov[intervals], 2), split_end(cv[intervals], 2))
    )
    ends$i <- out$i[intervals][ends$i]
    keep <- setdiff(seq_along(ov), intervals)
    rest <- fidelity_date_cells(ov[keep], cv[keep])
    rest$i <- out$i[keep][rest$i]
    return(vctrs::vec_rbind(rest, ends))
  }
  ko <- po$kind
  kc <- pc$kind
  same_kind <- ko == kc | (ko %in% c("month", "month_time") & kc %in% c("month", "month_time")) |
    (ko %in% c("year", "year_day") & kc %in% c("year", "year_day"))
  full <- ko == "full" & kc == "full"
  out$type[full] <- "full"
  out$delta[full] <- as.numeric(pc$reference[full] - po$reference[full])
  out$same_time[full] <- substring(ov[full], 11) == substring(cv[full], 11)
  month <- ko %in% c("month", "month_time") & same_kind
  out$type[month] <- "month"
  out$mid[month] <- po$reference[month]
  out$orig_text[month] <- ov[month]
  out$copy_text[month] <- cv[month]
  year <- ko %in% c("year", "year_day") & same_kind
  out$type[year] <- "year"
  out$years[year] <- as.numeric(substr(cv[year], 1, 4)) - as.numeric(substr(ov[year], 1, 4))
  out$same_time[year] <- substring(ov[year], 5) == substring(cv[year], 5)
  same <- ko %in% c("time_only", "duration")
  out$type[same] <- "same"
  out$ok[same] <- ov[same] == cv[same]
  invalid <- ko == "invalid"
  out$type[is.na(out$type) & !invalid] <- "bad"
  out$ok[out$type %in% "bad"] <- FALSE
  out[!invalid, , drop = FALSE]
}

# parse_iso8601_dates() on each distinct value once: dates repeat across
# the rows of a subject and across parameters.
fidelity_parse_dates <- function(x) {
  u <- unique(x)
  parsed <- parse_iso8601_dates(u)
  m <- match(x, u)
  list(kind = parsed$kind[m], reference = parsed$reference[m])
}

# The most common value of each group (the subject's offset).
fidelity_mode <- function(x, group) {
  ok <- !is.na(x) & !is.na(group)
  if (!any(ok)) {
    return(stats::setNames(numeric(0), character(0)))
  }
  counts <- as.data.frame(table(group = group[ok], x = x[ok]), stringsAsFactors = FALSE)
  counts <- counts[counts$Freq > 0, , drop = FALSE]
  counts <- counts[order(counts$group, -counts$Freq, abs(as.numeric(counts$x))), , drop = FALSE]
  first <- !duplicated(counts$group)
  stats::setNames(as.numeric(counts$x[first]), counts$group[first])
}

fidelity_check_intervals <- function(orig, copy, align, key, intent) {
  cells <- list()
  for (ds in align$keyed) {
    o <- orig[[ds]]
    c <- copy[[ds]]
    pairs <- align$pairs[[ds]]
    pairs <- pairs[!is.na(pairs$s), , drop = FALSE]
    text_cols <- fidelity_text_date_columns(o, key, intent, ds)
    date_cols <- names(o)[vapply(o, fidelity_is_date, logical(1))]
    cols <- intersect(unique(c(text_cols, date_cols)), names(c))
    for (col in cols) {
      x <- o[[col]]
      y <- c[[col]]
      if (fidelity_is_text(x)) x <- fidelity_text(x)
      if (fidelity_is_text(y)) y <- fidelity_text(y)
      cl <- fidelity_date_cells(x[pairs$o], y[pairs$c])
      if (nrow(cl) == 0) next
      cl$s <- pairs$s[cl$i]
      cl$dataset <- ds
      cells[[length(cells) + 1]] <- cl
    }
  }
  if (length(cells) == 0) {
    return(list())
  }
  cells <- do.call(vctrs::vec_rbind, cells)
  offset <- fidelity_mode(ifelse(cells$type == "full", cells$delta, NA), cells$s)
  years <- fidelity_mode(ifelse(cells$type == "year", cells$years, NA), cells$s)
  subject_offset <- offset[cells$s]
  full <- cells$type == "full"
  cells$ok[full] <- cells$delta[full] == subject_offset[full] & cells$same_time[full]
  month <- cells$type == "month"
  unknown <- month & is.na(subject_offset)
  known <- month & !unknown
  expected <- paste0(
    format(cells$mid[known] + subject_offset[known], "%Y-%m"), substring(cells$orig_text[known], 8)
  )
  cells$ok[known] <- expected == cells$copy_text[known]
  # Year-only dates of a subject all move by one number of whole years: the
  # offset's, max(1, round(|offset| / 365.25)), in a direction of their own;
  # none when the dates didn't move.
  year <- cells$type == "year"
  step <- ifelse(subject_offset == 0, 0, pmax(1, round(abs(subject_offset) / 365.25)))
  cells$ok[year] <- cells$years[year] == years[cells$s[year]] & cells$same_time[year] &
    (is.na(step[year]) | abs(cells$years[year]) == step[year])
  cells$checked <- !unknown

  lapply(unique(cells$dataset), function(ds) {
    x <- cells[cells$dataset == ds, , drop = FALSE]
    n <- sum(x$checked)
    n_failed <- sum(x$checked & !x$ok)
    detail <- paste0(
      n, " date(s) of ", length(unique(x$s)), " subject(s) compared with the subject's offset",
      if (n_failed > 0) paste0("; ", n_failed, " moved by another offset, lost their clock time or precision") else "",
      if (sum(!x$checked) > 0) {
        paste0("; ", sum(!x$checked), " month date(s) of subjects with no full date not compared")
      } else {
        ""
      }
    )
    fidelity_row(ds, "date intervals", fidelity_status(n_failed), n, n_failed, detail, skipped = sum(!x$checked))
  })
}

# Full dates as Date (other values missing).
fidelity_full_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    x <- as.POSIXct(x)
    tz <- attr(x, "tzone", exact = TRUE)
    return(as.Date(format(x, "%Y-%m-%d", tz = if (is.null(tz) || length(tz) == 0) "" else tz[[1]])))
  }
  if (!fidelity_is_text(x)) {
    return(rep(as.Date(NA), length(x)))
  }
  parsed <- fidelity_parse_dates(fidelity_text(x))
  out <- parsed$reference
  out[parsed$kind != "full"] <- NA
  out
}

# A reference date (RFSTDTC, TRTSDT) for each row of dataset `ds`: its own
# column, or the subject's value in `from`.
fidelity_reference <- function(data, ds, col, from, key) {
  df <- data[[ds]]
  if (col %in% names(df)) {
    return(fidelity_full_date(df[[col]]))
  }
  for (f in from) {
    ref <- data[[f]]
    if (!is.null(ref) && col %in% names(ref) && key %in% names(ref) && key %in% names(df)) {
      return(fidelity_full_date(ref[[col]])[match(fidelity_keys(df[[key]]), fidelity_keys(ref[[key]]))])
    }
  }
  NULL
}

fidelity_check_study_days <- function(ds, orig, copy, align, key, dm) {
  o <- orig[[ds]]
  c <- copy[[ds]]
  pairs <- align$pairs[[ds]]
  dy_cols <- grep("DY$", intersect(names(o), names(c)), value = TRUE)
  parts <- character(0)
  checked <- 0L
  failed <- 0L
  agree_orig <- 0L
  any_pair <- FALSE
  for (dy in dy_cols) {
    prefix <- sub("DY$", "", dy)
    if (!nzchar(prefix) || !is.numeric(fidelity_plain(o[[dy]]))) next
    if (paste0(prefix, "DTC") %in% names(o) && fidelity_is_text(o[[paste0(prefix, "DTC")]])) {
      date_col <- paste0(prefix, "DTC")
      ref_col <- "RFSTDTC"
      from <- dm
    } else if (paste0(prefix, "DT") %in% names(o) && inherits(o[[paste0(prefix, "DT")]], "Date")) {
      date_col <- paste0(prefix, "DT")
      ref_col <- "TRTSDT"
      from <- "adsl"
    } else {
      next
    }
    if (!date_col %in% names(c)) next
    ref_o <- fidelity_reference(orig, ds, ref_col, from, key)
    ref_c <- fidelity_reference(copy, ds, ref_col, from, key)
    if (is.null(ref_o) || is.null(ref_c)) next
    day <- function(date, ref) {
      d <- as.numeric(date - ref)
      d + (d >= 0)
    }
    calc_o <- day(fidelity_full_date(o[[date_col]]), ref_o)[pairs$o]
    calc_c <- day(fidelity_full_date(c[[date_col]]), ref_c)[pairs$c]
    val_o <- as.numeric(fidelity_plain(o[[dy]]))[pairs$o]
    val_c <- as.numeric(fidelity_plain(c[[dy]]))[pairs$c]
    comparable <- !is.na(calc_o) & !is.na(val_o)
    if (!any(comparable)) next
    any_pair <- TRUE
    held <- comparable & calc_o == val_o
    broken <- held & !(!is.na(calc_c) & !is.na(val_c) & calc_c == val_c)
    agree_c <- sum(!is.na(calc_c) & !is.na(val_c) & calc_c == val_c)
    n_c <- sum(!is.na(calc_c) & !is.na(val_c))
    checked <- checked + sum(held)
    failed <- failed + sum(broken)
    agree_orig <- agree_orig + sum(held)
    parts <- c(parts, paste0(
      dy, " from ", date_col, " and ", ref_col, ": agrees in ", sum(held), " of ", sum(comparable),
      " row(s) of the original, ", agree_c, " of ", n_c, " of the copy"
    ))
  }
  if (!any_pair) {
    return(NULL)
  }
  status <- if (agree_orig == 0) "not applicable" else fidelity_status(failed)
  fidelity_row(ds, "study days", status, checked, failed, paste(parts, collapse = "; "))
}

fidelity_close <- function(a, b) {
  abs(a - b) <= 1e-8 * pmax(1, abs(a), abs(b))
}

# The numeric relations of a dataset: for each, the columns involved and a
# function giving TRUE (holds), FALSE (broken) or NA (can't be evaluated).
fidelity_relations <- function(df) {
  out <- list()
  has <- function(cols) all(cols %in% names(df)) && all(vapply(df[cols], function(x) is.numeric(fidelity_plain(x)), logical(1)))
  if (has(c("CHG", "AVAL", "BASE"))) {
    out[["CHG = AVAL - BASE"]] <- list(cols = c("CHG", "AVAL", "BASE"), eval = function(d) {
      chg <- as.numeric(fidelity_plain(d$CHG))
      aval <- as.numeric(fidelity_plain(d$AVAL))
      base <- as.numeric(fidelity_plain(d$BASE))
      ifelse(is.na(chg) | is.na(aval) | is.na(base), NA, fidelity_close(chg, aval - base))
    })
  }
  if (has(c("PCHG", "CHG", "BASE"))) {
    out[["PCHG = 100 * CHG / BASE"]] <- list(cols = c("PCHG", "CHG", "BASE"), eval = function(d) {
      pchg <- as.numeric(fidelity_plain(d$PCHG))
      chg <- as.numeric(fidelity_plain(d$CHG))
      base <- as.numeric(fidelity_plain(d$BASE))
      ifelse(is.na(pchg) | is.na(chg) | is.na(base) | base == 0, NA, fidelity_close(pchg, 100 * chg / base))
    })
  }
  stresn <- grep("STRESN$", names(df), value = TRUE)
  for (n_col in stresn) {
    c_col <- sub("STRESN$", "STRESC", n_col)
    if (!c_col %in% names(df) || !fidelity_is_text(df[[c_col]]) || !is.numeric(fidelity_plain(df[[n_col]]))) next
    local({
      nc <- n_col
      cc <- c_col
      out[[paste0(nc, " = ", cc)]] <<- list(cols = c(nc, cc), eval = function(d) {
        n <- as.numeric(fidelity_plain(d[[nc]]))
        text <- fidelity_text(d[[cc]])
        parsed <- suppressWarnings(as.numeric(trimws(text)))
        blank <- fidelity_blank(text)
        ifelse(blank & is.na(n), NA, ifelse(is.na(parsed) | is.na(n), is.na(parsed) & is.na(n), fidelity_close(parsed, n)))
      })
    })
  }
  out
}

fidelity_check_relations <- function(ds, o, c, pairs, intent) {
  relations <- fidelity_relations(o)
  relations <- relations[vapply(relations, function(r) all(r$cols %in% names(c)), logical(1))]
  if (length(relations) == 0 || nrow(pairs) == 0) {
    return(NULL)
  }
  oo <- vctrs::vec_slice(o, pairs$o)
  cc <- vctrs::vec_slice(c, pairs$c)
  checked <- 0L
  failed <- 0L
  warned <- 0L
  held_total <- 0L
  parts <- character(0)
  for (name in names(relations)) {
    r <- relations[[name]]
    held <- r$eval(oo) %in% TRUE
    now <- r$eval(cc)
    changed <- rep(FALSE, nrow(cc))
    logged <- rep(FALSE, nrow(cc))
    for (col in r$cols) {
      a <- fidelity_plain(oo[[col]])
      b <- fidelity_plain(cc[[col]])
      diff <- !((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
      changed <- changed | diff
      logged <- logged | (diff & fidelity_logged_rows(intent, ds, col, c)[pairs$c])
    }
    on_purpose <- held & logged
    broken_on_purpose <- on_purpose & now %in% FALSE
    broken <- held & !on_purpose & !(now %in% TRUE)
    checked <- checked + sum(held & !on_purpose)
    held_total <- held_total + sum(held)
    failed <- failed + sum(broken)
    warned <- warned + sum(broken_on_purpose)
    parts <- c(parts, paste0(
      name, ": holds in ", sum(held), " row(s) of the original",
      if (sum(on_purpose) > 0) paste0(", ", sum(on_purpose), " changed on purpose (", sum(broken_on_purpose), " no longer holding)"),
      if (sum(broken) > 0) paste0(", ", sum(broken), " broken")
    ))
  }
  status <- if (held_total == 0) "not applicable" else fidelity_status(failed, warned, strict = intent$step)
  fidelity_row(ds, "numeric relations", status, checked, failed, paste(parts, collapse = "; "))
}
