#' Suppress the quasi-identifiers of subjects still at risk
#'
#' For subjects whose group is still smaller than the target after
#' generalization, blanks the fewest quasi-identifiers needed to reach the
#' target (local suppression), or removes those subjects from every dataset.
#' Risk is measured as [anon_risk()] does with `na = "wildcard"`: a blank
#' value matches any value.
#'
#' @inheritParams anon_risk
#' @param data A data frame, a named list of data frames linked by `key`, or
#'   the result of an earlier step such as [anon_generalize()].
#' @param order Names of quasi-identifiers (as [anon_risk()] names them) from
#'   least to most useful, to choose between blanking options of the same
#'   size: the most useful are blanked last. Those not listed follow, in the
#'   order of `quasi`. Default: the order of `quasi`, its first
#'   quasi-identifier being the first blanked.
#' @param method `"cells"` (default) blanks quasi-identifiers of the subjects
#'   at risk; `"drop"` removes those subjects from every dataset.
#' @param unresolved With `method = "cells"`, what to do with subjects still
#'   at risk when nothing is left to blank: `"drop"` (default) removes them
#'   from every dataset, with a warning; `"keep"` keeps them, with a warning.
#' @param key_cols Names of columns holding other subjects' keys (default
#'   `"RSUBJID"`, as in SDTM `RELSUB`): rows that refer to a removed subject
#'   are removed too, so no reference is left dangling.
#'
#' @details
#' **Cells.** Subjects at risk are handled one at a time, the smallest group
#' first (then in the order subjects first appear in the data). For each, the
#' step finds the smallest set of its quasi-identifiers whose blanking gives it
#' a group of at least the target size, counting every subject that agrees
#' with it on the quasi-identifiers left; among sets of the same size it
#' blanks the least useful ones (by `order`: a set that keeps a more useful
#' quasi-identifier wins). With more than 12 quasi-identifiers holding a value
#' it adds them one at a time instead, each time the one that grows the group
#' most. Blanking can only make groups larger, so a subject handled stays at
#' the target; the result is deterministic, but not guaranteed to blank the
#' fewest cells overall (as in sdcMicro's local suppression). A subject with
#' every quasi-identifier blank matches everyone, so the target is always
#' reached when there are at least as many subjects as the target group
#' size; otherwise every subject is unresolved.
#'
#' A quasi-identifier is blanked in **every row** of that subject: the column
#' in every row of the subject in its dataset, and for an [anon_quasi_values()]
#' specification every row of the test (not only the measured one), with the
#' test's `--ORRES`, `--STRESC` and `--STRESN` together. Blanks are SDTM
#' missing values: `""` for text if the column already uses blank strings,
#' otherwise `NA`, and `NA` for numbers and factors (a factor's levels held
#' only by blanked values are dropped). Columns keep their type and
#' attributes.
#'
#' **Drop.** Subjects at risk are removed from every dataset that has the
#' `key` column, with the rows that refer to them in `key_cols`; removing
#' subjects can make other groups smaller, so this repeats until no subject
#' is at risk. Datasets without the `key` column and data frames nested in
#' the datasets are not changed (a warning names datasets holding nested data
#' frames with the key).
#'
#' The log has one row per quasi-identifier (and result column) blanked, with
#' the number of cells blanked and of subjects, one row per dataset with rows
#' removed, and the risk before and after; it holds no values. The settings
#' record the arguments and where each quasi-identifier comes from (not
#' `filter`).
#'
#' @return An object of class `"anon_deid_step"`, as for [anon_subject_ids()].
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
#' anon_risk(result, quasi = c("AGE", "SEX"), max_risk = 1/2, na = "wildcard")
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
  key_cols = "RSUBJID"
) {
  method <- match.arg(method)
  unresolved <- match.arg(unresolved)
  k_target <- risk_k_target(max_risk)
  single <- is.data.frame(if (inherits(data, "anon_deid_step")) data$data else data)
  input <- as_dataset_list(data)
  data <- input$data
  check_key_name(key)
  # A single data frame is measured as one, so `quasi` can name its columns.
  measured <- function(data) if (single) data[[1]] else data
  table <- risk_quasi_table(measured(data), quasi, key)
  sources <- attr(table, "sources")
  qnames <- setdiff(names(table), key)
  rank <- suppress_rank(order, qnames)
  keys <- table[[key]]
  n <- length(keys)

  codes <- quasi_codes(table[qnames])
  k <- quasi_group_sizes(table[qnames], "wildcard")
  before <- risk_summary(k, max_risk, k_target, length(qnames))
  blanked <- matrix(FALSE, n, length(qnames))
  dropped <- character(0)

  if (method == "cells") {
    if (n < k_target) {
      to_resolve <- integer(0)
    } else {
      to_resolve <- which(k < k_target)
    }
    while (length(to_resolve) > 0) {
      i <- to_resolve[order(k[to_resolve], to_resolve)][1]
      to_resolve <- setdiff(to_resolve, i)
      cols <- which(codes[i, ] != 0L)
      if (length(cols) == 0) {
        next
      }
      matches <- vapply(cols, function(q) codes[, q] == codes[i, q] | codes[, q] == 0L, logical(n))
      matches <- matrix(matches, nrow = n)
      blank <- cols[suppress_choice(matches, k_target, rank[cols])]
      if (length(blank) == 0) {
        next
      }
      old <- rowSums(matches) == length(cols)
      new <- rowSums(matches[, !cols %in% blank, drop = FALSE]) == length(cols) - length(blank)
      gained <- new & !old
      k[gained] <- k[gained] + 1L
      k[i] <- sum(new)
      codes[i, blank] <- 0L
      blanked[i, blank] <- TRUE
      to_resolve <- intersect(to_resolve, which(k < k_target))
    }
    still <- which(k < k_target)
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
    remaining <- seq_len(n)
    repeat {
      group_k <- quasi_group_sizes(table[remaining, qnames], "wildcard")
      at_risk <- remaining[group_k < k_target]
      if (length(at_risk) == 0) break
      dropped <- c(dropped, keys[at_risk])
      remaining <- setdiff(remaining, at_risk)
      if (length(remaining) == 0) break
    }
  }

  log_rows <- list()
  for (q in which(colSums(blanked) > 0)) {
    blanked_keys <- keys[blanked[, q]]
    src <- sources[q, ]
    out <- suppress_source(data[[src$dataset]], src, key, blanked_keys)
    data[[src$dataset]] <- out$df
    for (row in out$log) {
      log_rows <- c(log_rows, list(c(list(dataset = src$dataset), row, list(n_subjects = length(blanked_keys)))))
    }
  }
  removed_rows <- list()
  if (length(dropped) > 0) {
    for (ds in names(data)) {
      df <- data[[ds]]
      if (!key %in% names(df)) {
        next
      }
      remove <- normalize_keys(df[[key]]) %in% dropped
      for (col in intersect(key_cols, names(df))) {
        remove <- remove | normalize_keys(df[[col]]) %in% dropped
      }
      if (any(remove)) {
        data[[ds]] <- vctrs::vec_slice(df, !remove)
        removed_rows[[ds]] <- sum(remove)
      }
    }
    nested <- names(data)[vapply(data, holds_nested_key, logical(1), key = key)]
    if (length(nested) > 0) {
      warning(
        "Subjects were removed from the datasets with `", key, "` only; data frames nested in these datasets ",
        "still hold rows with `", key, "`, including any of the removed subjects: ", paste(nested, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  after <- if (length(dropped) < n) {
    risk_summary(
      quasi_group_sizes(risk_quasi_table(measured(data), quasi, key)[qnames], "wildcard"),
      max_risk, k_target, length(qnames)
    )
  }
  describe_risk <- function(s) {
    if (is.null(s)) "no subjects left" else paste0("min k ", s$min_k, ", ", s$n_below_k, " subject(s) below ", k_target)
  }
  log <- deid_log_start(
    "suppress", n, key, NULL,
    settings = paste0(
      "method = \"", method, "\", max_risk = ", format(max_risk, digits = 7), " (groups of at least ", k_target, ")",
      if (method == "cells") paste0(", order = ", deparse_list(qnames[order(rank)]), ", unresolved = \"", unresolved, "\""),
      "; missing values match any value; before: ", describe_risk(before), "; after: ", describe_risk(after)
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
      paste0(length(dropped), " subject(s) at risk removed from every dataset with `", key, "`"), 0, 0
    )
    for (ds in names(removed_rows)) {
      log <- deid_log_add(log, ds, NA_character_, "rows of removed subjects", 0, removed_rows[[ds]])
    }
  }

  settings <- list(suppress = list(
    key = key, quasi = sources, max_risk = max_risk, k_target = k_target, order = qnames[order(rank)],
    method = method, unresolved = unresolved, key_cols = key_cols,
    n_dropped = length(dropped), meets_target = isTRUE(after$meets_target)
  ))
  new_deid_step(input$unwrap(data), log, NULL, previous = input$previous, settings = settings)
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

# Which of a subject's quasi-identifiers to blank (positions in the columns
# of `matches`, a logical matrix of which subjects agree with it on each of
# its quasi-identifiers that hold a value): the smallest set whose blanking
# gives it at least `target` subjects that agree on the rest, and among those
# the one that spares the most useful (`rank`). Every subset is scored at
# once from the counts of each pattern of agreement (a sum over supersets);
# with more than 12 quasi-identifiers, they are added one at a time instead.
suppress_choice <- function(matches, target, rank) {
  m <- ncol(matches)
  weight <- 2^(rank - 1)
  if (m > 12) {
    blank <- integer(0)
    keep <- seq_len(m)
    repeat {
      if (sum(rowSums(matches[, keep, drop = FALSE]) == length(keep)) >= target || length(keep) == 0) {
        return(blank)
      }
      gain <- vapply(keep, function(q) {
        rest <- setdiff(keep, q)
        sum(rowSums(matches[, rest, drop = FALSE]) == length(rest))
      }, numeric(1))
      best <- keep[order(-gain, rank[keep])[1]]
      blank <- c(blank, best)
      keep <- setdiff(keep, best)
    }
  }
  bits <- 2^(seq_len(m) - 1)
  pattern <- as.integer(matches %*% bits)
  size <- 2^m
  agree <- tabulate(pattern + 1L, nbins = size)
  # agree[K + 1] becomes the number of subjects that agree on every
  # quasi-identifier in K (patterns that contain K).
  idx <- 0:(size - 1)
  for (b in bits) {
    without <- idx[bitwAnd(idx, b) == 0]
    agree[without + 1] <- agree[without + 1] + agree[without + b + 1]
  }
  full <- size - 1
  blank_set <- full - idx
  n_blank <- rowSums(outer(blank_set, bits, function(s, b) bitwAnd(s, b) != 0))
  cost <- as.vector(outer(blank_set, bits, function(s, b) bitwAnd(s, b) != 0) %*% weight)
  ok <- which(agree >= target)
  best <- ok[order(n_blank[ok], cost[ok])[1]]
  which(bitwAnd(blank_set[best], bits) != 0)
}

# Blank a quasi-identifier for some subjects in every row of its dataset: a
# column, or every row of a test with its --ORRES, --STRESC and --STRESN.
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

# Set a column's values at `rows` to its missing value: "" for text that
# already uses blank strings, otherwise NA.
blank_rows <- function(df, col, rows) {
  x <- df[[col]]
  data <- if (is.factor(x)) as.character(x) else vctrs::vec_data(x)
  value <- if (is.character(data)) missing_text_like(data) else NA
  new <- replace_at(x, rows, value)
  n <- sum(changed_values(vctrs::vec_slice(x, rows), vctrs::vec_slice(new, rows)))
  list(df = set_generalized_column(df, col, new, n > 0), n = n)
}

# Whether a data frame holds, in packed or list columns at any depth, a data
# frame with the key column.
holds_nested_key <- function(df, key) {
  for (col in unclass(df)) {
    if (is.data.frame(col)) {
      if (key %in% names(col) || holds_nested_key(col, key)) return(TRUE)
    } else if (is.list(col) && !isS4(col)) {
      for (e in col) {
        if (is.data.frame(e) && (key %in% names(e) || holds_nested_key(e, key))) return(TRUE)
        if (is.list(e) && !is.data.frame(e) && !isS4(e) && holds_nested_key(list(e), key)) return(TRUE)
      }
    }
  }
  FALSE
}
