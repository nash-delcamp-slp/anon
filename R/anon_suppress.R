#' Suppress quasi-identifiers until every subject reaches the target
#'
#' For subjects whose group is still smaller than the target after
#' generalization, blanks quasi-identifier cells until every released
#' pattern is shared by enough records (local suppression), or removes those
#' subjects from every dataset. Risk is k-anonymity on the released table
#' (see "After suppression" in [anon_risk()]): a blanked cell is a value of
#' its own.
#'
#' @inheritParams anon_risk
#' @param data A data frame, a named list of data frames linked by `key`, or
#'   the result of an earlier step such as [anon_generalize()].
#' @param order Names of quasi-identifiers (as [anon_risk()] names them) from
#'   least to most useful, to choose between blanking options that are
#'   otherwise equal: the most useful are blanked last. Those not listed
#'   follow, in the order of `quasi`. Default: the order of `quasi`, its
#'   first quasi-identifier being the first blanked.
#' @param method `"cells"` (default) blanks cells; `"drop"` removes the
#'   subjects in groups smaller than the target from every dataset.
#' @param unresolved With `method = "cells"`, what to do with subjects still
#'   in groups smaller than the target when nothing is left to blank:
#'   `"drop"` (default) removes them from every dataset, with a warning;
#'   `"keep"` keeps them, with a warning.
#' @param key_cols Names of columns holding other subjects' keys (default
#'   `"RSUBJID"`, as in SDTM `RELSUB`): rows that refer to a removed subject
#'   are removed too, so no reference is left dangling.
#' @param also Names of related identifier columns (default `"SUBJID"`): when
#'   subjects are removed, their values (of at least 5 characters, as
#'   [anon_subject_ids()] searches them) are looked for in what remains.
#' @param everywhere Logical. If `TRUE` (default), a quasi-identifier given as
#'   a column (such as `AGE` in `dm`) is blanked for the subject in every
#'   dataset with the `key` that has a column of that name (such as `AGE` in
#'   an ADaM `adsl` or `adae`), and in data frames with the `key` nested in
#'   any dataset. If `FALSE`, only in its own dataset, and a warning names the
#'   other datasets that hold a column of that name.
#'
#' @details
#' **The measure.** Each released record's pattern is its visible
#' quasi-identifier values plus the places of its blanked cells, a blanked
#' cell counting as a value of its own; a subject's `k` is the number of
#' released records with exactly its pattern, and every pattern must be
#' shared by at least the target group size. Real missing values are values,
#' distinct from blanked cells. Cells blanked by an earlier `anon_suppress()`
#' in the same chain count as blanked. An attacker who knows a subject's
#' values then finds at least `k` records that could be it, and records
#' blanked everywhere only hide among each other.
#'
#' **Cells.** The step repeats, on the records in groups smaller than the
#' target: for each quasi-identifier still shown on some of them, it tries
#' blanking that quasi-identifier on all of them. If a try brings records
#' into groups of at least the target, it takes the try that brings the most
#' (then needs the fewest cells, then blanks the least useful by `order`) and
#' blanks only the records it brings in. Otherwise it takes the try that
#' merges the most small patterns (then the least useful) and blanks it on
#' all of them. Every step blanks at least one cell, so it ends; records
#' still short when everything is blanked (or with fewer subjects than the
#' target group size) are unresolved. The result is deterministic and
#' doesn't depend on the order of rows, but it isn't guaranteed to blank the
#' fewest cells. Blanking only joins records into groups and removing small
#' groups leaves the others as they are, so groups of at least the target
#' stay so.
#'
#' A quasi-identifier is blanked in **every row** of that subject: the column
#' in every row of the subject in its dataset (and, with `everywhere`, in
#' every other dataset with the key that has the column), and for an
#' [anon_quasi_values()] specification every row of the test (not only the
#' measured one), with its other results: for SDTM `--ORRES`, `--STRESC` and
#' `--STRESN`; for ADaM `AVAL`, `AVALC`, `BASE`, `BASEC`, `CHG` and `PCHG`.
#' Blanks are missing values: `""` for text unless the column already uses
#' `NA` for missing text (it has `NA` and no `""`), `NA` for numbers and
#' factors (a factor's levels held only by blanked values are dropped).
#' Columns keep their type and attributes. The step records which cells it
#' blanked (by subject key, as the data holds it, never values), and later
#' steps carry the record on, so [anon_risk()] can measure the result.
#'
#' **Drop.** Subjects in groups smaller than the target are removed (the other
#' groups stay as they are). Removed subjects' rows go
#' from every dataset with the `key`, from datasets whose rows belong to
#' subjects through a packed column with the key or a list column of data
#' frames holding one subject per row, from data frames with the key nested
#' at any depth, and with the rows that refer to them in `key_cols`. If any
#' removed subject's key, or a value of an `also` column, is still found in
#' the text values of the data, a warning names the datasets.
#'
#' **When the target isn't met.** `settings$suppress$meets_target` is `FALSE`,
#' with reasons in `settings$suppress$not_met`, when the measured risk misses
#' the target, when rows of removed subjects remain, or when a dataset
#' holding `AGE` as a quasi-identifier also holds a birth date more precise
#' than a year (`BRTHDTC`, or a Date `BRTHDT`): with a reference date such as
#' `RFSTDTC`, it gives the age back exactly, even after the dates were
#' offset (both move together). Remove birth dates or reduce them to the year
#' first. Other dates can give a quasi-identifier back the same way (an age
#' at an event from `--DTC`, an ADaM `AAGE`), and so can derived columns
#' (`AGEGR1` from `AGE`, `RACEN` from `RACE`): list them as quasi-identifiers
#' or generalize them together.
#'
#' The log has one row per quasi-identifier (and result column) blanked, with
#' the number of cells blanked and of subjects, one row per dataset with rows
#' removed, and the risk before and after; it holds no values. The settings
#' record the arguments and where each quasi-identifier comes from (not
#' `filter`).
#'
#' Each step of the loop groups the records once per quasi-identifier still
#' shown on records short of the target; 10,000 subjects with 12
#' quasi-identifiers, all of them unique at first, take about a second.
#'
#' @return An object of class `"anon_deid_step"`, as for [anon_subject_ids()],
#'   with `suppressed`, the record of blanked cells (dataset, column or test,
#'   and subject key).
#'
#' @seealso [anon_risk()], [anon_generalize()]
#'
#' @examples
#' dm <- data.frame(
#'   USUBJID = sprintf("S%02d", 1:6),
#'   AGE = c(30, 30, 30, 40, 40, 50),
#'   SEX = c("F", "F", "M", "M", "M", "F")
#' )
#' result <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/2)
#' result$data
#' anon_risk(result, quasi = c("AGE", "SEX"), max_risk = 1/2)
#'
#' @export
anon_suppress <- function(
  data,
  quasi,
  key = "USUBJID",
  max_risk = 0.09,
  order = NULL,
  method = c("cells", "drop"),
  unresolved = c("drop", "keep"),
  key_cols = "RSUBJID",
  also = "SUBJID",
  everywhere = TRUE
) {
  method <- match.arg(method)
  unresolved <- match.arg(unresolved)
  k_target <- risk_k_target(max_risk)
  check_key_name(key)
  inputs <- risk_inputs(data, quasi, key)
  single <- is.data.frame(if (inherits(data, "anon_deid_step")) data$data else data)
  input <- as_dataset_list(data)
  data <- input$data
  # A single data frame is measured as one, so `quasi` can name its columns.
  measured <- function(data) if (single) data[[1]] else data
  sources <- inputs$sources
  qnames <- inputs$qnames
  rank <- suppress_rank(order, qnames)
  keys <- inputs$table[[key]]
  n <- length(keys)

  # The released patterns: each value as a code (a missing value is a
  # value), cells blanked by an earlier step 0, a value of their own.
  codes <- released_codes(inputs$table[qnames], inputs$blanked)
  # A real missing value can't be blanked: nothing would change in the data.
  real_na <- vapply(inputs$table[qnames], vctrs::vec_detect_missing, logical(n))
  real_na <- matrix(real_na, n) & codes != 0L
  codes[real_na] <- -abs(codes[real_na])
  blanked <- matrix(FALSE, n, length(qnames))
  before <- risk_summary(pattern_group_sizes(codes), max_risk, k_target, length(qnames))
  dropped <- character(0)
  not_met <- character(0)

  if (method == "cells") {
    while (n >= k_target) {
      k <- pattern_group_sizes(codes)
      small <- which(k < k_target)
      open <- which(colSums(codes[small, , drop = FALSE] > 0L) > 0)
      if (length(small) == 0 || length(open) == 0) break
      choice <- suppress_step(codes, small, open, k_target, rank)
      cells <- cbind(choice$rows, choice$q)
      codes[cells] <- 0L
      blanked[cells] <- TRUE
    }
    still <- which(pattern_group_sizes(codes) < k_target)
    if (length(still) > 0) {
      if (unresolved == "drop") {
        dropped <- keys[still]
        warning(
          length(still), " subject(s) could not reach groups of at least ", k_target,
          " by blanking and were removed from every dataset.",
          call. = FALSE
        )
      } else {
        warning(
          length(still), " subject(s) remain in groups smaller than ", k_target,
          " (unresolved = \"keep\").",
          call. = FALSE
        )
      }
    }
  } else {
    # Groups are exact patterns, so removing the small ones leaves the others
    # as they are.
    dropped <- keys[pattern_group_sizes(codes) < k_target]
  }

  # Blank the cells in the data.
  log_rows <- list()
  record <- list()
  for (q in which(colSums(blanked) > 0)) {
    blanked_keys <- keys[blanked[, q]]
    src <- sources[q, ]
    out <- suppress_source(data[[src$dataset]], src, key, blanked_keys)
    data[[src$dataset]] <- out$df
    for (row in out$log) {
      log_rows <- c(log_rows, list(c(list(dataset = src$dataset), row, list(n_subjects = length(blanked_keys)))))
    }
    record <- c(record, list(tibble::tibble(
      dataset = src$dataset, column = src$column, test_col = src$test_col, test = src$test,
      result_col = src$result_col, key = blanked_keys
    )))
    if (!is.na(src$column)) {
      copies <- setdiff(datasets_with_column(data, src$column, key), src$dataset)
      if (everywhere) {
        for (ds in union(src$dataset, copies)) {
          out <- blank_keyed_copies(data[[ds]], src$column, key, blanked_keys, top = ds != src$dataset)
          data[[ds]] <- out$df
          if (out$n > 0) {
            log_rows <- c(log_rows, list(list(
              dataset = ds, variable = src$column, n_changed = out$n, n_subjects = length(blanked_keys),
              note = paste0(" (copy of `", src$column, "` in `", dataset_label(src$dataset), "`)")
            )))
          }
        }
      } else if (length(copies) > 0) {
        warning(
          "`", src$column, "` was blanked in `", dataset_label(src$dataset), "` only; these datasets hold it too: ",
          paste(copies, collapse = ", "), ". Use everywhere = TRUE, or remove or blank it there.",
          call. = FALSE
        )
        not_met <- c(not_met, paste0("copies of `", src$column, "` not blanked"))
      }
    }
  }
  record <- if (length(record) > 0) do.call(rbind, record)

  removed_rows <- list()
  if (length(dropped) > 0) {
    also_values <- related_values(data, key, also, dropped)
    for (ds in names(data)) {
      out <- drop_subject_rows(data[[ds]], key, key_cols, dropped)
      data[[ds]] <- out$df
      if (out$n > 0) {
        removed_rows[[ds]] <- out$n
      }
    }
    left <- names(data)[vapply(data, holds_values, logical(1), values = c(dropped, also_values))]
    if (length(left) > 0) {
      warning(
        "Rows or values of removed subjects remain in these datasets: ", paste(left, collapse = ", "),
        " (their keys or `", paste(also, collapse = "`, `"), "` values are still found in text values). ",
        "Remove them before sharing.",
        call. = FALSE
      )
      not_met <- c(not_met, "values of removed subjects remain")
    }
  }

  if (!is.null(record)) {
    record <- record[!record$key %in% dropped, , drop = FALSE]
  }

  ages <- intersect(sources$dataset[!is.na(sources$column) & toupper(sources$column) == "AGE"], names(data))
  births <- precise_birth_dates(data, ages)
  if (length(births) > 0) {
    warn_birth_dates(births)
    not_met <- c(not_met, "birth dates more precise than a year next to AGE")
  }

  remaining <- !keys %in% dropped
  after <- if (any(remaining)) {
    risk_summary(pattern_group_sizes(codes[remaining, , drop = FALSE]), max_risk, k_target, length(qnames))
  }
  if (is.null(after) || !isTRUE(after$meets_target)) {
    not_met <- c(not_met, "measured risk above the target")
  }
  describe_risk <- function(s) {
    if (is.null(s)) "no subjects left" else paste0("min k ", s$min_k, ", ", s$n_below_k, " subject(s) below ", k_target)
  }
  log <- deid_log_start(
    "suppress", n, key, NULL,
    settings = paste0(
      "method = \"", method, "\", max_risk = ", format(max_risk, digits = 7), " (groups of at least ", k_target, ")",
      if (method == "cells") paste0(", order = ", deparse_list(qnames[order(rank)]), ", unresolved = \"", unresolved, "\""),
      "; k counts the released records with the same pattern (a blanked cell is a value of its own)",
      "; before: ", describe_risk(before), "; after: ", describe_risk(after),
      if (length(not_met) > 0) paste0("; target not met: ", paste(unique(not_met), collapse = ", "))
    )
  )
  for (row in log_rows) {
    log <- deid_log_add(
      log, row$dataset, row$variable,
      paste0("blanked for ", row$n_subjects, " subject(s)", row$note),
      row$n_changed, 0
    )
  }
  if (length(dropped) > 0) {
    log <- deid_log_add(
      log, NA_character_, NA_character_,
      paste0(length(dropped), " subject(s) at risk removed from every dataset"), 0, 0
    )
    for (ds in names(removed_rows)) {
      log <- deid_log_add(log, ds, NA_character_, "rows of removed subjects (at any depth)", 0, removed_rows[[ds]])
    }
  }

  settings <- list(suppress = list(
    key = key, quasi = sources, max_risk = max_risk, k_target = k_target, order = qnames[order(rank)],
    method = method, unresolved = unresolved, key_cols = key_cols, also = also, everywhere = everywhere,
    n_dropped = length(dropped), min_k = if (!is.null(after)) after$min_k else NA_integer_,
    meets_target = length(not_met) == 0, not_met = unique(not_met)
  ))
  new_deid_step(input$unwrap(data), log, NULL, previous = input$previous, settings = settings, suppressed = record)
}

# helpers -----------------------------------------------------------------

# Each quasi-identifier's rank from least (1) to most useful.
suppress_rank <- function(order, qnames) {
  if (is.null(order)) {
    return(seq_along(qnames))
  }
  if (!is.character(order) || anyNA(order) || anyDuplicated(order)) {
    stop("`order` must be a character vector of quasi-identifier names without duplicates.", call. = FALSE)
  }
  unknown <- setdiff(order, qnames)
  if (length(unknown) > 0) {
    stop(
      "`order` names `", unknown[1], "`, which is not a quasi-identifier; they are: ",
      paste(qnames, collapse = ", "), ".",
      call. = FALSE
    )
  }
  match(qnames, c(order, setdiff(qnames, order)))
}

# One step of local suppression on the records in groups smaller than the
# target (`small`, rows of `codes`: positive for a value that can be
# blanked, negative for a real missing value, 0 for a blank): for each
# quasi-identifier still shown
# on some of them (`open`), try blanking it on all of them. If some try
# brings records into groups of at least the target, take the one that
# brings the most (then needs the fewest cells, then is the least useful by
# `rank`) and blank only the records it brings in. Otherwise take the try
# that merges the most small patterns (then the least useful) and blank it
# on all of them: each step blanks at least one cell, so the loop ends.
suppress_step <- function(codes, small, open, k_target, rank) {
  patterns <- function(m) vctrs::vec_group_id(as.data.frame(m[small, , drop = FALSE]))
  n_patterns <- max(patterns(codes))
  tries <- lapply(open, function(q) {
    trial <- codes
    shown <- small[codes[small, q] > 0L]
    trial[shown, q] <- 0L
    k <- pattern_group_sizes(trial)
    rescued <- small[k[small] >= k_target]
    list(
      q = q, rescued = rescued, n_rescued = length(rescued),
      cells = sum(codes[rescued, q] > 0L), merged = n_patterns - max(patterns(trial))
    )
  })
  n_rescued <- vapply(tries, `[[`, numeric(1), "n_rescued")
  cells <- vapply(tries, `[[`, numeric(1), "cells")
  merged <- vapply(tries, `[[`, numeric(1), "merged")
  if (any(n_rescued > 0)) {
    best <- tries[[order(-n_rescued, cells, rank[open])[1]]]
    rows <- best$rescued
  } else {
    best <- tries[[order(-merged, rank[open])[1]]]
    rows <- small
  }
  rows <- rows[codes[rows, best$q] > 0L]
  list(q = best$q, rows = rows)
}

# Blank a quasi-identifier for some subjects in every row of its dataset: a
# column, or every row of a test with its other results.
suppress_source <- function(df, src, key, blanked_keys) {
  rows <- normalize_keys(df[[key]]) %in% blanked_keys
  if (!is.na(src$column)) {
    out <- blank_rows(df, src$column, which(rows))
    return(list(df = out$df, log = list(list(variable = src$column, n_changed = out$n, note = ""))))
  }
  tg <- list(test_col = src$test_col, test = src$test)
  rows <- which(rows & test_rows(df, tg))
  log <- list()
  for (col in result_siblings(df, src$result_col)) {
    out <- blank_rows(df, col, rows)
    df <- out$df
    note <- if (col != src$result_col) paste0(", with `", src$result_col, "`") else ""
    log <- c(log, list(list(variable = test_variable(col, src$test_col, src$test), n_changed = out$n, note = note)))
  }
  list(df = df, log = log)
}

# The missing value for blanked text: "" unless the column already uses NA
# for missing text (it has NA and no "").
blank_text_value <- function(text) {
  if (any(is.na(text)) && !any(!is.na(text) & text == "")) NA_character_ else ""
}

# Set a column's values at `rows` to its missing value.
blank_rows <- function(df, col, rows) {
  x <- df[[col]]
  if (is.null(x) || length(rows) == 0) {
    return(list(df = df, n = 0L))
  }
  data <- if (is.factor(x)) as.character(x) else vctrs::vec_data(x)
  value <- if (is.character(data) && !is.factor(x)) blank_text_value(data) else NA
  new <- replace_at(x, rows, value)
  n <- sum(changed_values(vctrs::vec_slice(x, rows), vctrs::vec_slice(new, rows)))
  list(df = set_generalized_column(df, col, new, n > 0), n = n)
}

# Datasets that hold a column of this name in a data frame with the key, at
# the top or nested.
datasets_with_column <- function(data, col, key) {
  names(data)[vapply(data, function(df) {
    found <- FALSE
    map_keyed_frames(df, key, function(f, packed) {
      if (col %in% names(f)) found <<- TRUE
      f
    })
    found
  }, logical(1))]
}

# Blank `col` for the subjects `blanked_keys` in every data frame with the key
# in `df` (the dataset itself only when `top`).
blank_keyed_copies <- function(df, col, key, blanked_keys, top = TRUE) {
  n <- 0L
  is_top <- TRUE
  out <- map_keyed_frames(df, key, function(f, packed) {
    this_top <- is_top
    is_top <<- FALSE
    if ((this_top && !top) || !col %in% names(f)) {
      return(f)
    }
    b <- blank_rows(f, col, which(normalize_keys(f[[key]]) %in% blanked_keys))
    n <<- n + b$n
    b$df
  })
  list(df = out, n = n)
}

# Apply `fn(frame, packed)` to every data frame in `df` that has the key
# column: `df` itself, packed data frame columns (`packed = TRUE`: their
# rows must be kept), and data frames in list columns (also inside plain
# lists), at any depth.
map_keyed_frames <- function(df, key, fn, packed = FALSE) {
  if (key %in% names(df)) {
    df <- fn(df, packed)
  }
  for (j in seq_along(df)) {
    col <- .subset2(df, j)
    new <- if (is.data.frame(col)) {
      map_keyed_frames(col, key, fn, packed = TRUE)
    } else if (is.list(col) && !isS4(col) && !inherits(col, "POSIXlt")) {
      map_list_frames(col, key, fn)
    } else {
      col
    }
    if (!identical(new, col)) {
      df <- set_column(df, j, new)
    }
  }
  df
}

map_list_frames <- function(x, key, fn) {
  for (i in seq_along(x)) {
    e <- x[[i]]
    new <- if (is.data.frame(e)) {
      map_keyed_frames(e, key, fn)
    } else if (is.list(e) && is.null(oldClass(e))) {
      map_list_frames(e, key, fn)
    } else {
      e
    }
    if (!identical(new, e)) {
      x[[i]] <- new
    }
  }
  x
}

# The subject of each row of a data frame without the key column: from a
# packed column with the key, or a list column whose data frames hold
# exactly one subject in every row. NULL if neither.
row_subjects <- function(df, key) {
  for (col in unclass(df)) {
    if (is.data.frame(col) && key %in% names(col)) {
      return(normalize_keys(col[[key]]))
    }
  }
  held <- function(e) {
    if (is.data.frame(e)) {
      return(if (key %in% names(e)) normalize_keys(e[[key]]))
    }
    if (is.list(e) && is.null(oldClass(e))) {
      return(unlist(lapply(e, held), use.names = FALSE))
    }
    NULL
  }
  for (col in unclass(df)) {
    if (!is.data.frame(col) && is.list(col) && !isS4(col) && !inherits(col, "POSIXlt")) {
      per_row <- lapply(col, function(e) unique(stats::na.omit(held(e))))
      if (length(per_row) > 0 && all(lengths(per_row) == 1)) {
        return(unlist(per_row))
      }
    }
  }
  NULL
}

# Remove the rows of `dropped` subjects from a dataset, at any depth: rows
# with the key (or referring to them in `key_cols`), rows whose subject
# comes from nested data (see row_subjects()), and rows of data frames with
# the key in list columns.
drop_subject_rows <- function(df, key, key_cols, dropped) {
  n <- 0L
  if (!key %in% names(df)) {
    owners <- row_subjects(df, key)
    if (!is.null(owners) && any(owners %in% dropped)) {
      keep <- !owners %in% dropped
      n <- n + sum(!keep)
      df <- vctrs::vec_slice(df, keep)
    }
  }
  df <- map_keyed_frames(df, key, function(f, packed) {
    if (packed) {
      return(f)
    }
    remove <- normalize_keys(f[[key]]) %in% dropped
    for (col in intersect(key_cols, names(f))) {
      remove <- remove | normalize_keys(f[[col]]) %in% dropped
    }
    if (!any(remove)) {
      return(f)
    }
    n <<- n + sum(remove)
    vctrs::vec_slice(f, !remove)
  })
  list(df = df, n = n)
}

# Values of the `also` columns (of at least 5 characters) of the subjects
# `dropped`, from data frames with the key.
related_values <- function(data, key, also, dropped) {
  out <- character(0)
  for (df in data) {
    map_keyed_frames(df, key, function(f, packed) {
      rows <- normalize_keys(f[[key]]) %in% dropped
      for (col in intersect(also, names(f))) {
        v <- normalize_keys(f[[col]])[rows]
        out <<- c(out, v[!is.na(v) & nchar(v) >= 5])
      }
      f
    })
  }
  unique(out)
}

# Text without surrounding spaces, also when it isn't valid UTF-8.
trim_bytes <- function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", as.character(x), useBytes = TRUE)

# Whether any text value of a data frame (at any depth) equals one of
# `values`.
holds_values <- function(df, values) {
  if (length(values) == 0) {
    return(FALSE)
  }
  for (col in unclass(df)) {
    if (is.data.frame(col)) {
      if (holds_values(col, values)) return(TRUE)
    } else if (is.character(col) || is.factor(col)) {
      if (any(trim_bytes(col) %in% values)) return(TRUE)
    } else if (is.list(col) && !isS4(col) && !inherits(col, "POSIXlt")) {
      for (e in col) {
        if (is.data.frame(e) && holds_values(e, values)) return(TRUE)
        if ((is.character(e) || is.factor(e)) && any(trim_bytes(e) %in% values)) return(TRUE)
        if (is.list(e) && is.null(oldClass(e)) && holds_values(list(e), values)) return(TRUE)
      }
    }
  }
  FALSE
}

# Columns of the datasets `datasets` holding birth dates more precise than a
# year: text BRTHDTC with a month, or Date/date-time birth dates.
precise_birth_dates <- function(data, datasets) {
  out <- character(0)
  for (ds in datasets) {
    df <- data[[ds]]
    for (col in names(df)[toupper(names(df)) %in% c("BRTHDTC", "BRTHDT", "BRTHDTM")]) {
      x <- df[[col]]
      precise <- if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
        any(!is.na(x))
      } else if (is.character(x) || is.factor(x)) {
        any(parse_iso8601_dates(as.character(x))$kind %in% c("full", "month", "month_time"))
      } else {
        FALSE
      }
      if (precise) out <- c(out, paste0(ds, ".", col))
    }
  }
  out
}

warn_birth_dates <- function(births) {
  warning(
    "Birth dates more precise than a year are next to a generalized or suppressed `AGE`: ",
    paste(births, collapse = ", "), ". With a reference date such as RFSTDTC they give the age back ",
    "exactly, even after dates are offset; remove them or reduce them to the year.",
    call. = FALSE
  )
}
