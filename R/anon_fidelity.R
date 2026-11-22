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
#' **Aligning rows.** [anon_subject_ids()] sorts rows by the new key, so the
#' copy's rows don't line up with the original's. Subjects are paired by
#' their rows: each row is coded from the columns that no step changed (not
#' in the step's log, same class, and no value more often in the copy than
#' in the original), and a subject's rows in every dataset with `key` must
#' match one original subject's. A step's kept pseudonym map
#' (`keep_secrets = TRUE`) is used instead when there is one, and keys that
#' weren't pseudonymized pair directly. Within a subject, rows pair in
#' order. Subjects whose rows are the same as another subject's in every
#' unchanged column can't be told apart: they are counted, and their rows
#' are not compared one by one. Rows of datasets without `key` pair by
#' position.
#'
#' **Checks** (one row per dataset and check; `"(all)"` for checks across
#' datasets):
#' - `datasets`: the same dataset names.
#' - `columns`: the same columns in the same order.
#' - `types`: the same class and type for each column, and for the columns
#'   of nested data frames (packed columns, and data frames in list columns).
#' - `attributes`: the same column labels, formats (`format.sas`,
#'   `format.spss`, `format.stata`, `display_width`) and value labels. Value
#'   labels dropped from a column a step logged are intended. Labels
#'   rewritten by [anon_subject_ids()] or [anon_subject_dates()] (keys and
#'   dates in them) are `"warn"`; a label lost is a failure.
#' - `rows`: the same number of rows, or fewer only by the rows of subjects
#'   the step dropped (`"warn"`, with counts).
#' - `row order`: within each subject, rows in the same order.
#' - `keys in dm`: every key of the dataset is in `dm`.
#' - `distinct keys`: the original's number of subjects, less those dropped.
#' - `pseudonyms`: each subject has one pseudonym in every dataset, and each
#'   pseudonym one subject: every copy subject's rows across datasets match
#'   one original subject's.
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
#' **Not checked:** business rules beyond these, free text, statistics (means
#' and counts by group change with banding and suppression), and the values
#' themselves. A pass is evidence that code relying on these properties
#' should behave the same on the original, not proof that it will.
#'
#' **Cost.** One pass over every column of every dataset, plus a grouping
#' of the rows; a few seconds for the pharmaversesdtm and pharmaverseadam
#' datasets.
#'
#' @return A tibble of class `"anon_fidelity"` with columns `dataset`,
#'   `check`, `status` (`"pass"`, `"fail"`, `"warn"` or `"not applicable"`),
#'   `n_checked`, `n_failed` and `detail` (counts only), and an attribute
#'   `passed`: `TRUE` when no check failed. It prints, and converts with
#'   [as_anon_payload()] and [as_anon_json()].
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
      fidelity_check_categorical(ds, o, co, pairs, key, intent),
      fidelity_check_iso(ds, o, co, pairs, key, intent)
    ))
  }
  if (length(align$keyed) > 0) {
    rows <- c(rows, list(fidelity_check_pseudonyms(align)))
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

# The checks, in the order they are listed.
fidelity_check_names <- c(
  "datasets", "columns", "types", "attributes", "rows", "row order", "keys in dm", "distinct keys", "pseudonyms",
  "categorical values", "ISO 8601", "date intervals", "study days", "numeric relations"
)

new_anon_fidelity <- function(x) {
  x <- tibble::as_tibble(x)
  out <- tibble::new_tibble(x, class = "anon_fidelity")
  attr(out, "passed") <- !any(x$status == "fail")
  out
}

#' @export
print.anon_fidelity <- function(x, ...) {
  counts <- table(factor(x$status, levels = c("pass", "warn", "fail", "not applicable")))
  cat("Fidelity of the de-identified copy:", if (isTRUE(attr(x, "passed"))) "passed" else "FAILED", "\n")
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
    checks = serialize_table_payload(as.data.frame(x), path = paste0(path, "$checks"))
  )
}

# helpers -----------------------------------------------------------------

fidelity_row <- function(dataset, check, status, n_checked, n_failed, detail) {
  data.frame(
    dataset = dataset, check = check, status = status, n_checked = as.integer(n_checked),
    n_failed = as.integer(n_failed), detail = detail, stringsAsFactors = FALSE
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
    step = !is.null(step), ids = character(0), key_cols = character(0), pseudonymized = FALSE, dates = FALSE,
    partial = "shift", logged = fidelity_log_cells(NULL), shifted = list(), rules = list(), n_dropped = 0L,
    date_rules = character(0), map = NULL
  )
  if (is.null(step)) {
    return(out)
  }
  s <- step$settings
  pick <- function(kind) s[names(s) == kind | startsWith(names(s), paste0(kind, "_"))]
  ids_settings <- c(pick("subject_ids"), pick("deidentify"))
  out$ids <- unique(unlist(lapply(ids_settings, function(x) c(x$also, x$key_cols)), use.names = FALSE))
  out$key_cols <- unique(unlist(lapply(ids_settings, `[[`, "key_cols"), use.names = FALSE))
  out$pseudonymized <- length(pick("subject_ids")) > 0
  dates <- pick("subject_dates")
  out$dates <- length(dates) > 0
  if (out$dates) {
    out$partial <- utils::tail(dates, 1)[[1]]$partial %||% "shift"
  }
  out$logged <- fidelity_log_cells(step$log)
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
    step = character(0), dataset = character(0), column = character(0), test_col = character(0),
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
    step = log$step, dataset = log$dataset,
    column = ifelse(has_test, sub(rx, "\\1", log$variable), log$variable),
    test_col = ifelse(has_test, sub(rx, "\\2", log$variable), NA_character_),
    test = ifelse(has_test, sub(rx, "\\3", log$variable), NA_character_),
    stringsAsFactors = FALSE
  )
}

# Rows of `df` (a dataset `ds` of the copy) whose `col` cell a step logged,
# optionally only the given steps.
fidelity_logged_rows <- function(intent, ds, col, df, steps = NULL) {
  lg <- intent$logged
  lg <- lg[lg$dataset == ds & lg$column == col, , drop = FALSE]
  if (!is.null(steps)) {
    lg <- lg[lg$step %in% steps, , drop = FALSE]
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

fidelity_format_attributes <- c("format.sas", "format.spss", "format.stata", "display_width")

# Column attributes compared, at any depth through packed data frames.
fidelity_check_attributes <- function(ds, o, c, key, intent) {
  rewritten <- intent$pseudonymized || intent$dates
  res <- list(checked = 0L, lost = character(0), changed = character(0), rewritten = character(0), dropped = 0L)
  walk <- function(o, c, prefix, top) {
    for (col in intersect(names(o), names(c))) {
      x <- o[[col]]
      y <- c[[col]]
      name <- paste0(prefix, col)
      res$checked <<- res$checked + 1L
      lo <- attr(x, "label", exact = TRUE)
      lc <- attr(y, "label", exact = TRUE)
      if (!is.null(lo) && is.null(lc)) {
        res$lost <<- c(res$lost, name)
      } else if (!identical(lo, lc)) {
        if (rewritten && !is.null(lo)) res$rewritten <<- c(res$rewritten, name) else res$changed <<- c(res$changed, name)
      }
      for (a in fidelity_format_attributes) {
        if (!identical(attr(x, a, exact = TRUE), attr(y, a, exact = TRUE))) res$changed <<- c(res$changed, name)
      }
      vo <- attr(x, "labels", exact = TRUE)
      vc <- attr(y, "labels", exact = TRUE)
      if (!identical(vo, vc)) {
        # Value labels a step dropped (or pruned to the values still
        # released) from a column it logged are intended.
        logged <- top && (fidelity_logged_column(intent, ds, col) || col %in% c(key, intent$ids) ||
          (intent$dates && (fidelity_is_date(y) || grepl("DTC$", col, ignore.case = TRUE))))
        subset <- !is.null(vo) && (is.null(vc) || (all(names(vc) %in% names(vo)) &&
          all(vctrs::vec_in(unname(fidelity_plain(vc)), unname(fidelity_plain(vo))))))
        if (logged && subset) {
          res$dropped <<- res$dropped + 1L
        } else if (rewritten && !is.null(vo) && !is.null(vc)) {
          # Rewritten keys or dates in the labels, merged where their codes
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
    if (length(res$changed) > 0) paste0("label or format changed: ", paste(unique(res$changed), collapse = ", ")),
    if (length(warned) > 0) paste0("labels rewritten by the keys or dates step: ", paste(warned, collapse = ", ")),
    if (res$dropped > 0) paste0(res$dropped, " with value labels dropped or pruned on purpose")
  ), collapse = "; ")
  fidelity_row(ds, "attributes", fidelity_status(length(failed), length(warned)), res$checked, length(failed), detail)
}

# Columns whose rows can pair original and copy subjects: not the key or a
# related ID, not dates, not logged by any step, the same class, and no value
# more often in the copy than in the original.
fidelity_stable_columns <- function(ds, o, c, key, intent) {
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
    codes <- fidelity_row_codes(o, c, fidelity_stable_columns(ds, o, c, key, intent))
    # Related IDs (RSUBJID) of each row: rows referring to a dropped subject
    # are dropped with it.
    refs <- lapply(intersect(intent$key_cols, names(o)), function(col) {
      if (is.list(o[[col]])) NULL else fidelity_keys(o[[col]])
    })
    list(ko = owners_o[[ds]], kc = owners_c[[ds]], co = codes$o, cc = codes$c, refs = Filter(Negate(is.null), refs))
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
  global <- function(sigs, n) {
    if (n == 0) return(character(0))
    do.call(paste, c(unname(lapply(sigs, function(s) paste0("[", s, "]"))), sep = "|"))
  }
  sig_c <- lapply(per, function(p) signatures(p$kc, p$cc, subjects_c, FALSE))
  global_c <- stats::setNames(global(sig_c, length(subjects_c)), subjects_c)
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

  # Each subject's row codes in order, by dataset.
  codes_c <- lapply(per, function(p) split(p$cc, factor(p$kc, levels = subjects_c)))
  # Pairs subjects using the original's rows in `kept`, by dataset. Subjects
  # whose rows match exactly are paired first. Of the rest, an original and a
  # copy subject with as many rows in every dataset are paired when most of
  # their rows match, position by position, and no other subject matches as
  # well (`near`: their differing rows are failures of the rows check).
  pair_subjects <- function(kept) {
    codes_o <- lapply(keyed, function(ds) {
      p <- per[[ds]]
      split(p$co[kept[[ds]]], factor(p$ko[kept[[ds]]], levels = subjects_o))
    })
    names(codes_o) <- keyed
    sorted_sig <- function(codes) vapply(codes, function(v) paste(sort(v), collapse = ","), character(1))
    global_o <- stats::setNames(global(lapply(codes_o, sorted_sig), length(subjects_o)), subjects_o)
    res <- list(
      map = stats::setNames(rep(NA_character_, length(subjects_o)), subjects_o), dropped = character(0),
      near = character(0), n_unmatched = 0L, n_mismatched = 0L, n_surplus = 0L, n_alike = 0L
    )
    sizes <- function(codes, subject) vapply(codes, function(x) length(x[[subject]]), integer(1))
    # Rows of original subject `a` equal, position by position, to those of
    # copy subject `b`; NA when the subjects don't have as many rows in every
    # dataset.
    score <- function(a, b) {
      if (!identical(sizes(codes_o, a), sizes(codes_c, b))) {
        return(NA_integer_)
      }
      sum(vapply(keyed, function(ds) sum(codes_o[[ds]][[a]] == codes_c[[ds]][[b]]), integer(1)))
    }
    near_pairs <- function(left_o, left_c) {
      found <- stats::setNames(rep(NA_character_, length(left_o)), left_o)
      if (length(left_o) == 0 || length(left_c) == 0) {
        return(found)
      }
      scores <- matrix(NA_integer_, length(left_o), length(left_c))
      for (i in seq_along(left_o)) {
        for (j in seq_along(left_c)) scores[i, j] <- score(left_o[i], left_c[j])
      }
      total <- vapply(left_o, function(a) sum(sizes(codes_o, a)), integer(1))
      for (i in seq_along(left_o)) {
        row <- scores[i, ]
        if (all(is.na(row))) next
        best <- max(row, na.rm = TRUE)
        j <- which(row == best)
        if (length(j) == 1 && 2 * best > total[i] && sum(scores[, j] == best, na.rm = TRUE) == 1) {
          found[i] <- left_c[j]
        }
      }
      found
    }
    if (by_key) {
      target <- if (use_secret) secret$pseudonym[match(subjects_o, secret$key)] else subjects_o
      present <- target %in% subjects_c
      res$dropped <- subjects_o[!present]
      same <- present & global_o == global_c[match(target, subjects_c)]
      res$map[same] <- target[same]
      # A subject whose rows differ keeps its pseudonym when it has as many
      # rows in every dataset; otherwise its pseudonym holds other rows.
      for (a in subjects_o[present & !same]) {
        b <- target[match(a, subjects_o)]
        if (!is.na(score(a, b))) {
          res$map[a] <- b
          res$near <- c(res$near, a)
        } else {
          res$n_mismatched <- res$n_mismatched + 1L
        }
      }
      res$n_unmatched <- sum(!subjects_c %in% target)
      return(res)
    }
    class_o <- split(subjects_o, factor(global_o, levels = unique(global_o)))
    class_c <- split(subjects_c, factor(global_c, levels = unique(global_c)))
    single_missing <- character(0)
    for (sig in names(class_o)) {
      a <- class_o[[sig]]
      b <- class_c[[sig]]
      if (length(a) == 1 && length(b) == 1) {
        res$map[a] <- b
      } else if (length(a) == 1 && length(b) == 0) {
        single_missing <- c(single_missing, a)
      } else {
        # Subjects alike in every unchanged column: as many as are missing
        # from the copy count as dropped; the others can't be paired.
        n_drop <- max(length(a) - length(b), 0)
        if (n_drop > 0) res$dropped <- c(res$dropped, utils::tail(a, n_drop))
        if (length(b) > 0) res$n_alike <- res$n_alike + min(length(a), length(b))
        res$n_surplus <- res$n_surplus + max(length(b) - length(a), 0)
      }
    }
    unmatched_c <- subjects_c[!global_c %in% names(class_o)]
    found <- near_pairs(single_missing, unmatched_c)
    res$map[names(found)[!is.na(found)]] <- found[!is.na(found)]
    res$near <- names(found)[!is.na(found)]
    res$dropped <- c(res$dropped, names(found)[is.na(found)])
    res$n_unmatched <- length(setdiff(unmatched_c, found))
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
    # Subjects paired by most of their rows pair row by row in order; their
    # rows that differ are counted, and their order can't be checked.
    near <- intersect(here, res$near)
    exact <- setdiff(here, near)
    in_order <- ord_o[exact] == ord_c[map[exact]]
    out$order_checked[[ds]] <- length(exact)
    out$order_failed[[ds]] <- sum(!in_order)
    out$differ_rows[[ds]] <- sum(vapply(near, function(a) {
      sum(p$co[rows_o[[a]]] != p$cc[rows_c[[map[[a]]]]])
    }, integer(1)))
    good <- c(exact[in_order], near)
    o_idx <- unlist(rows_o[good], use.names = FALSE)
    c_idx <- unlist(rows_c[map[good]], use.names = FALSE)
    s_idx <- rep(good, lengths(rows_o[good]))
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

fidelity_check_rows <- function(ds, o, c, align, intent) {
  if (!ds %in% align$keyed) {
    n_failed <- abs(nrow(o) - nrow(c))
    return(fidelity_row(ds, "rows", fidelity_status(n_failed), nrow(o), n_failed,
      paste0(nrow(o), " row(s) in the original, ", nrow(c), " in the copy")))
  }
  drop_rows <- align$dropped_rows[[ds]]
  drop_subjects <- align$dropped_subjects[[ds]]
  differ <- align$differ_rows[[ds]] %||% 0L
  expected <- nrow(o) - drop_rows
  n_failed <- abs(nrow(c) - expected) + differ
  # Dropping is intended only when the step says it dropped as many subjects.
  recorded <- !intent$step || intent$n_dropped == length(align$dropped)
  detail <- paste0(nrow(o), " row(s) in the original, ", nrow(c), " in the copy")
  if (drop_subjects > 0) {
    detail <- paste0(detail, "; ", drop_rows, " row(s) of ", drop_subjects, " subject(s) missing from the copy",
      if (intent$step) paste0(" (the step dropped ", intent$n_dropped, " subject(s))"))
  }
  if (differ > 0) {
    detail <- paste0(detail, "; ", differ, " paired row(s) differ from the original in columns no step changed")
  }
  status <- if (n_failed > 0 || (drop_subjects > 0 && !recorded)) "fail" else if (drop_subjects > 0) "warn" else "pass"
  fidelity_row(ds, "rows", status, nrow(o), if (status == "fail") max(n_failed, drop_rows) else 0L, detail)
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

fidelity_check_pseudonyms <- function(align) {
  failed <- align$n_unmatched + align$n_mismatched + align$n_surplus
  detail <- paste0(
    align$n_aligned, " of ", align$n_subjects, " subject(s) paired ", align$how,
    if (align$n_alike > 0) paste0("; ", align$n_alike, " alike in every unchanged column, not compared row by row"),
    if (length(align$dropped) > 0) paste0("; ", length(align$dropped), " missing from the copy"),
    if (align$n_near > 0) paste0("; ", align$n_near, " paired by most of their rows (see the rows check)"),
    if (align$n_unmatched > 0) paste0("; ", align$n_unmatched, " copy subject(s) whose rows match no original subject"),
    if (align$n_mismatched > 0) paste0("; ", align$n_mismatched, " subject(s) whose pseudonym holds another subject's rows"),
    if (align$n_surplus > 0) paste0("; ", align$n_surplus, " copy subject(s) more than the original's")
  )
  fidelity_row("(all)", "pseudonyms", fidelity_status(failed), align$n_subjects, failed, detail)
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
  year <- cells$type == "year"
  cells$ok[year] <- cells$years[year] == years[cells$s[year]] & cells$same_time[year]
  cells$checked <- !unknown

  lapply(unique(cells$dataset), function(ds) {
    x <- cells[cells$dataset == ds, , drop = FALSE]
    n <- sum(x$checked)
    n_failed <- sum(x$checked & !x$ok)
    detail <- paste0(
      n, " date(s) of ", length(unique(x$s)), " subject(s) compared with the subject's offset",
      if (n_failed > 0) paste0("; ", n_failed, " moved by another offset, lost their clock time or precision") else "",
      if (sum(!x$checked) > 0) paste0("; ", sum(!x$checked), " month date(s) of subjects with no full date not checked") else ""
    )
    fidelity_row(ds, "date intervals", fidelity_status(n_failed), n, n_failed, detail)
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
