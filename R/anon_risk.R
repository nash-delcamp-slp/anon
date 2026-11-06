#' Measure re-identification risk from quasi-identifiers
#'
#' Measures how easily subjects could be singled out from the combination of
#' their quasi-identifiers (indirect identifiers such as age, sex, race,
#' ethnicity, site, height or weight). Each subject's group is the set of
#' subjects with exactly the same quasi-identifier values; `k` is the group's
#' size and the subject's risk is `1 / k`. Quasi-identifiers can come from
#' several datasets linked by `key`.
#'
#' @param data A data frame, a named list of data frames linked by `key`, or
#'   the result of a de-identification step such as [anon_subject_ids()].
#' @param quasi The quasi-identifiers to measure. For a single data frame, a
#'   character vector of column names. For a list of data frames, a list whose
#'   named elements are character vectors of columns in the dataset of that
#'   name (each subject must have one value per column, as in `dm`), and whose
#'   unnamed elements are [anon_quasi_values()] specifications for values that
#'   vary by row, such as baseline height and weight in `vs`.
#' @param key Name of the subject identifier column shared by the datasets. It
#'   must have the same type (character or factor, or numeric) in every
#'   dataset used.
#' @param max_risk Highest acceptable re-identification risk for any subject
#'   (the maximum risk, `1 / k` for the smallest group). The target group size
#'   is the smallest `k` with `1 / k <= max_risk`, allowing for floating-point
#'   error so that 0.1 and 1/3 give 10 and 3. The default, 0.09, is the
#'   threshold Health Canada and EMA encourage for public release of clinical
#'   information, and requires groups of at least 12. Health Canada equates
#'   0.09 with a cell size of 11, but `1 / 11` is about 0.0909; to use groups of
#'   11, pass `max_risk = 1/11`. See Details.
#'
#' @details
#' Subjects are all non-missing key values found in the datasets used by
#' `quasi`; rows with a missing or blank key are dropped with a warning, and a
#' warning is given if keys differ only by leading or trailing spaces. A subject
#' with no value for a quasi-identifier (for example, missing from `vs`) gets
#' `NA` for it, and `NA` is treated as a value of its own when forming groups.
#' Blank strings, which is how SAS transport (XPT) files store missing text, are
#' treated as missing. Values are otherwise grouped by exact equality.
#'
#' Risk is measured on one value per subject for each quasi-identifier. A copy
#' shared with others usually contains every row of a dataset (for example
#' weight at every visit), and any row can identify a subject as well as the
#' measured one. The measurement only describes a shared copy if every row of
#' each quasi-identifier is generalized or removed in the same way.
#'
#' Risk is measured against the subjects in `data` only. Health Canada and EMA
#' allow or encourage measuring risk against a larger reference population,
#' which gives lower risk; measuring within the data, the smallest population,
#' is the strictest reading.
#'
#' The default target comes from Health Canada's *Public Release of Clinical
#' Information* guidance. Its "Risk Threshold" section "encourages adopting a 9%
#' re-identification risk threshold (risk=0.09)", and its section on measuring
#' risk states "Adopting a risk threshold of risk=0.09 equates to a target cell
#' size of 11 patients"
#' (<https://www.canada.ca/en/health-canada/services/drug-health-product-review-approval/profile-public-release-clinical-information-guidance/document.html>).
#' EMA's Anonymisation Report Form Instructions (EMA/482639/2023) state that
#' "Health Canada PRCI and EMA Policy 0070 guidance encourages a 9%
#' re-identification risk threshold (risk=0.09)" and describe the target as a
#' maximum "that should not be exceeded"
#' (<https://www.ema.europa.eu/en/documents/other/anonymisation-report-form-instructions_en.pdf>).
#' Because `1 / 11` (about 0.0909) exceeds 0.09, the default requires groups of
#' at least 12 so that the threshold is never exceeded; `max_risk = 1/11`
#' follows Health Canada's stated cell size instead. Both set the threshold for public release of clinical documents; using it for
#' other sharing is a deliberately conservative choice. GDPR and HIPAA set no
#' numerical re-identification risk threshold. Meeting the target is evidence about re-identification
#' risk from the chosen quasi-identifiers, not proof that data is anonymous or
#' de-identified under any regime.
#'
#' The `drivers` table shows what leaving out each quasi-identifier on its own
#' would achieve. It cannot see variables that identify the same subjects
#' together (for example height and weight), it shows the effect of removing a
#' variable rather than generalizing it, and it does not measure information
#' loss. Use it to decide what to try next, then measure again.
#'
#' @return An object of class `"anon_risk"`, a list with:
#'   - `summary`: one row with `n_subjects`, `n_quasi`, `max_risk_target`
#'     (the `max_risk` argument), `k_target` (the group size it requires),
#'     `n_unique` (subjects in a group of one), `min_k`, `max_risk` (the
#'     observed maximum, `1 / min_k`), `avg_risk` (mean of `1 / k` over
#'     subjects, equal to the number of groups divided by `n_subjects`),
#'     `n_below_k` (subjects in groups smaller than `k_target`),
#'     `prop_below_k` and `meets_target` (`min_k >= k_target`).
#'   - `drivers`: one row per quasi-identifier with the number of distinct
#'     non-missing values (`n_values`), the number of missing values
#'     (`n_missing`), and `n_unique_without`, `n_below_k_without`,
#'     `min_k_without` and `avg_risk_without` (the result if that variable were
#'     left out). Rows are ordered so the variable whose removal helps most
#'     comes first: by `n_below_k_without`, then `n_unique_without`, then
#'     `avg_risk_without` (all ascending), then the order of `quasi`.
#'   - `quasi`, `key`: the quasi-identifier names and key used.
#'   - `sources`: one row per quasi-identifier giving where it comes from:
#'     `variable`, `dataset`, and either `column` or `test_col`, `test` and
#'     `result_col`, plus `filtered` (whether a `filter` was used). The filter
#'     itself is not stored, because it may contain identifying values.
#'
#'   The object holds counts only, no subject-level rows or key values. Use
#'   [anon_risk_subjects()] for per-subject results.
#'
#' @seealso [anon_risk_subjects()] for per-subject group sizes.
#'
#' @examples
#' dm <- data.frame(
#'   USUBJID = sprintf("S%02d", 1:6),
#'   AGE = c(60, 60, 60, 70, 70, 85),
#'   SEX = c("F", "F", "F", "M", "M", "M")
#' )
#' anon_risk(dm, quasi = c("AGE", "SEX"), max_risk = 1/3)
#'
#' vs <- data.frame(
#'   USUBJID = rep(dm$USUBJID, each = 2),
#'   VSTESTCD = rep(c("HEIGHT", "WEIGHT"), 6),
#'   VSSTRESN = c(160, 60, 160, 60, 165, 62, 180, 80, 180, 80, 175, 90),
#'   VSBLFL = "Y"
#' )
#' # Dropping any single variable changes nothing here: subjects 1-2 and 4-5
#' # match on every variable, and HEIGHT and WEIGHT single out the same
#' # subjects, which leave-one-out drivers cannot show.
#' anon_risk(
#'   list(dm = dm, vs = vs),
#'   quasi = list(
#'     dm = c("AGE", "SEX"),
#'     anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN",
#'                       filter = ~ VSBLFL == "Y")
#'   ),
#'   max_risk = 0.5
#' )
#'
#' @export
anon_risk <- function(data, quasi, key = "USUBJID", max_risk = 0.09) {
  k_target <- risk_k_target(max_risk)
  table <- risk_quasi_table(data, quasi, key)
  quasi_names <- setdiff(names(table), key)
  group_k <- quasi_group_sizes(table[quasi_names])

  structure(
    list(
      summary = risk_summary(group_k, max_risk, k_target, length(quasi_names)),
      drivers = risk_drivers(table[quasi_names], k_target),
      quasi = quasi_names,
      sources = attr(table, "sources"),
      key = key
    ),
    class = "anon_risk"
  )
}

#' Per-subject re-identification risk
#'
#' Returns each subject's group size and risk from the same calculation as
#' [anon_risk()]. The result contains source key values: use it to find which
#' subjects are at risk, and don't share it.
#'
#' @inheritParams anon_risk
#'
#' @return A tibble with one row per subject: the `key` (as character), `k`
#'   (group size) and `risk` (`1 / k`).
#'
#' @examples
#' dm <- data.frame(
#'   USUBJID = sprintf("S%02d", 1:4),
#'   AGE = c(60, 60, 70, 85)
#' )
#' anon_risk_subjects(dm, quasi = "AGE")
#'
#' @export
anon_risk_subjects <- function(data, quasi, key = "USUBJID") {
  table <- risk_quasi_table(data, quasi, key)
  group_k <- quasi_group_sizes(table[setdiff(names(table), key)])
  stats::setNames(
    tibble::tibble(table[[key]], group_k, 1 / group_k),
    c(key, "k", "risk")
  )
}

#' Specify quasi-identifier values that vary by row
#'
#' Describes how to take one value per subject from a long dataset, such as a
#' findings domain, for use in [anon_risk()]. For each test code in `tests`,
#' the first non-missing, non-blank `result_col` value per subject (in row
#' order, after `filter`) becomes a quasi-identifier. Only that value is
#' measured; other rows for the same test identify the subject just as well, so
#' they need the same treatment before data is shared.
#'
#' @param dataset Name of the dataset in the list passed to [anon_risk()].
#' @param test_col Name of the column holding test codes (e.g. `"VSTESTCD"`).
#' @param tests Test codes to use (e.g. `c("HEIGHT", "WEIGHT")`).
#' @param result_col Name of the column holding the result (e.g. `"VSSTRESN"`).
#' @param filter Optional one-sided formula evaluated in the dataset to select
#'   rows, e.g. `~ VSBLFL == "Y"` for baseline values. Variables not found in
#'   the dataset are looked up where the formula was created. It must return
#'   `TRUE` or `FALSE` for each row. It applies to every test in `tests`; use
#'   separate specifications when tests need different filters (for example,
#'   height recorded once at screening without a baseline flag). A warning is
#'   given when a test has no values.
#' @param names Names for the resulting quasi-identifiers. Defaults to `tests`.
#'
#' @return An object of class `"anon_quasi_values"`.
#'
#' @examples
#' anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN",
#'                   filter = ~ VSBLFL == "Y")
#'
#' @export
anon_quasi_values <- function(dataset, test_col, tests, result_col, filter = NULL, names = tests) {
  for (arg in c("dataset", "test_col", "result_col")) {
    value <- get(arg)
    if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(value)) {
      stop("`", arg, "` must be a single string.", call. = FALSE)
    }
  }
  if (!is.character(tests) || length(tests) == 0 || anyNA(tests)) {
    stop("`tests` must be a character vector of test codes.", call. = FALSE)
  }
  if (anyDuplicated(tests)) {
    stop("`tests` must not contain duplicates.", call. = FALSE)
  }
  if (!is.null(filter) && !rlang::is_formula(filter)) {
    stop("`filter` must be NULL or a one-sided formula such as `~ VSBLFL == \"Y\"`.", call. = FALSE)
  }
  if (!is.character(names) || length(names) != length(tests) || anyNA(names) || any(!nzchar(names))) {
    stop("`names` must be a character vector with one name per test.", call. = FALSE)
  }
  if (anyDuplicated(names)) {
    stop("`names` must not contain duplicates.", call. = FALSE)
  }
  structure(
    list(
      dataset = dataset,
      test_col = test_col,
      tests = tests,
      result_col = result_col,
      filter = filter,
      names = names
    ),
    class = "anon_quasi_values"
  )
}

#' @export
print.anon_risk <- function(x, ...) {
  s <- x$summary
  cat("Re-identification risk\n")
  cat("======================\n\n")
  cat("Quasi-identifiers:", paste(x$quasi, collapse = ", "), "\n")
  cat("Subjects:", s$n_subjects, "\n")
  cat("Target: maximum risk", format(s$max_risk_target, digits = 7),
      paste0("(groups of at least ", s$k_target, ")"), "\n")
  cat("Maximum risk:", format(s$max_risk, digits = 7),
      paste0("(smallest group: ", s$min_k, ")"),
      " Average risk:", format(s$avg_risk, digits = 7), "\n")
  cat("Unique subjects (group of 1):", s$n_unique, "\n")
  cat("Subjects in groups smaller than ", s$k_target, ": ", s$n_below_k, " ",
      paste0("(", format(round(100 * s$prop_below_k, 1)), "%)"), "\n", sep = "")
  cat("Meets target:", if (isTRUE(s$meets_target)) "yes" else "no", "\n\n")
  cat("Drivers (`_without` columns: result if that variable alone were left out):\n")
  print(as.data.frame(x$drivers), row.names = FALSE)
  invisible(x)
}

#' @export
print.anon_quasi_values <- function(x, ...) {
  cat("Quasi-identifier values from `", x$dataset, "`: ",
      paste0(x$names, " = first non-missing ", x$result_col, " where ",
             x$test_col, " == \"", x$tests, "\"", collapse = "; "),
      "\n", sep = "")
  if (!is.null(x$filter)) {
    cat("Filter:", rlang::expr_text(rlang::f_rhs(x$filter)), "\n")
  }
  invisible(x)
}

#' @exportS3Method
as_anon_payload.anon_risk <- function(x, path = "$") {
  list(
    schema_version = anon_json_schema_version(),
    kind = "anon_risk",
    key = x$key,
    quasi = x$quasi,
    summary = serialize_table_payload(x$summary, path = paste0(path, "$summary")),
    drivers = serialize_table_payload(x$drivers, path = paste0(path, "$drivers"))
  )
}

# helpers -----------------------------------------------------------------

# Smallest group size k with 1 / k <= max_risk. A tolerance of a few units in
# the last place keeps exact reciprocals such as 0.1 (k = 10) from rounding up
# without letting a slightly smaller max_risk round down.
risk_k_target <- function(max_risk) {
  if (!is.numeric(max_risk) || length(max_risk) != 1 || !is.finite(max_risk) ||
    max_risk <= 0 || max_risk > 1) {
    stop("`max_risk` must be a single number greater than 0 and at most 1.", call. = FALSE)
  }
  ceiling(1 / max_risk * (1 - 8 * .Machine$double.eps))
}

# One row per subject: the key (character) and one column per quasi-identifier.
risk_quasi_table <- function(data, quasi, key) {
  if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
    stop("`key` must be a single column name.", call. = FALSE)
  }
  if (key %in% c("k", "risk")) {
    stop("`key` must not be named \"k\" or \"risk\", which are used for results; rename the column first.", call. = FALSE)
  }
  if (inherits(data, "anon_deid_step")) {
    data <- data$data
  }
  single <- is.data.frame(data)
  if (single) {
    data <- list(data = data)
  }
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "") ||
    !all(vapply(data, is.data.frame, logical(1)))) {
    stop("`data` must be a data frame or a named list of data frames.", call. = FALSE)
  }
  if (anyDuplicated(names(data))) {
    stop("Dataset names in `data` must be unique.", call. = FALSE)
  }

  specs <- normalize_quasi_specs(quasi, data, single)
  table <- build_quasi_table(data, specs, key)
  if (nrow(table) == 0) {
    stop("No subjects found: the datasets used by `quasi` have no non-missing `", key, "` values.", call. = FALSE)
  }
  table
}

normalize_quasi_specs <- function(quasi, data, single) {
  if (length(quasi) == 0) {
    stop("`quasi` must name at least one quasi-identifier.", call. = FALSE)
  }
  if (is.character(quasi)) {
    if (!single) {
      stop(
        "`quasi` must name the dataset for each column when `data` is a list, ",
        "e.g. `list(dm = c(\"AGE\", \"SEX\"))`.",
        call. = FALSE
      )
    }
    quasi <- list(data = quasi)
  }
  if (inherits(quasi, "anon_quasi_values")) {
    quasi <- list(quasi)
  }

  spec_names <- names(quasi)
  if (is.null(spec_names)) {
    spec_names <- rep("", length(quasi))
  }

  lapply(seq_along(quasi), function(i) {
    spec <- quasi[[i]]
    if (inherits(spec, "anon_quasi_values")) {
      return(spec)
    }
    if (!is.character(spec) || spec_names[i] == "") {
      stop(
        "Each element of `quasi` must be a named character vector of columns ",
        "or an `anon_quasi_values()` specification.",
        call. = FALSE
      )
    }
    if (anyDuplicated(spec)) {
      stop("Columns for `", dataset_label(spec_names[i]), "` in `quasi` must not contain duplicates.", call. = FALSE)
    }
    list(dataset = spec_names[i], columns = spec)
  })
}

build_quasi_table <- function(data, specs, key) {
  datasets <- unique(vapply(specs, function(s) s$dataset, character(1)))
  for (ds in datasets) {
    if (!ds %in% names(data)) {
      stop("Dataset `", ds, "` not found in `data`.", call. = FALSE)
    }
    if (!key %in% names(data[[ds]])) {
      stop("Key column `", key, "` not found in `", dataset_label(ds), "`.", call. = FALSE)
    }
  }
  check_key_types(data[datasets], key)

  all_keys <- unlist(lapply(datasets, function(ds) normalize_keys(data[[ds]][[key]])), use.names = FALSE)
  n_missing_keys <- sum(is.na(all_keys))
  if (n_missing_keys > 0) {
    warning(n_missing_keys, " row(s) with a missing or blank `", key, "` were dropped.", call. = FALSE)
  }
  keys <- unique(all_keys[!is.na(all_keys)])
  n_space_clashes <- length(keys) - length(unique(trimws(keys)))
  if (n_space_clashes > 0) {
    warning(
      n_space_clashes, " `", key, "` value(s) differ from another only by leading or trailing spaces; ",
      "they are treated as different subjects.",
      call. = FALSE
    )
  }

  columns <- list()
  sources <- list()
  for (spec in specs) {
    df <- data[[spec$dataset]]
    values <- if (inherits(spec, "anon_quasi_values")) {
      quasi_values_by_subject(df, spec, key, keys)
    } else {
      quasi_columns_by_subject(df, spec, key, keys)
    }
    columns <- c(columns, values)
    sources <- c(sources, list(quasi_sources(spec, names(values))))
  }
  sources <- do.call(rbind, sources)

  source_id <- paste(sources$dataset, sources$variable, sep = "\u001f")
  if (anyDuplicated(source_id)) {
    dup <- which(duplicated(source_id))[1]
    stop(
      "`", sources$variable[dup], "` from `", dataset_label(sources$dataset[dup]),
      "` appears more than once in `quasi`.",
      call. = FALSE
    )
  }

  col_names <- names(columns)
  repeated <- col_names %in% col_names[duplicated(col_names)]
  col_names[repeated] <- paste0(sources$dataset[repeated], ".", col_names[repeated])
  if (anyDuplicated(col_names)) {
    stop("Quasi-identifier names must be unique; `", col_names[duplicated(col_names)][1], "` repeats.", call. = FALSE)
  }
  if (key %in% col_names) {
    stop("A quasi-identifier must not be named like the key `", key, "`.", call. = FALSE)
  }
  names(columns) <- col_names
  sources$variable <- col_names

  table <- tibble::as_tibble(c(stats::setNames(list(keys), key), columns))
  attr(table, "sources") <- sources
  table
}

# Where each quasi-identifier comes from, one row per variable.
quasi_sources <- function(spec, variables) {
  values_spec <- inherits(spec, "anon_quasi_values")
  n <- length(variables)
  tibble::tibble(
    variable = variables,
    dataset = spec$dataset,
    column = if (values_spec) NA_character_ else spec$columns,
    test_col = if (values_spec) spec$test_col else NA_character_,
    test = if (values_spec) spec$tests else NA_character_,
    result_col = if (values_spec) spec$result_col else NA_character_,
    filtered = values_spec && !is.null(spec$filter)
  )
}

# Keys as character, with blank strings (as read from SAS transport files)
# treated as missing.
normalize_keys <- function(x) {
  blank_to_na(key_as_character(x))
}

# Blank or whitespace-only strings become NA; other values are unchanged.
# Factors are returned as character.
blank_to_na <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    x[!is.na(x) & grepl("^[[:space:]]*$", x, useBytes = TRUE)] <- NA_character_
  }
  x
}

# Keys are matched as text, so the key column must have the same kind of type
# in every dataset: text (character or factor) or numeric.
check_key_types <- function(data, key) {
  kinds <- vapply(data, function(df) {
    x <- df[[key]]
    if (is.character(x) || is.factor(x)) "text" else if (is.numeric(x)) "numeric" else class(x)[1]
  }, character(1))
  if (length(unique(kinds)) > 1) {
    stop(
      "Key column `", key, "` has different types across datasets (",
      paste0(names(kinds), ": ", kinds, collapse = ", "),
      "). Convert it to the same type first.",
      call. = FALSE
    )
  }
}

# Numeric keys are written value by value so that distinct numbers stay
# distinct: whole numbers in full ("1", "1000000000000001"); other numbers with
# 15 significant digits when that converts back to the same number ("2.5",
# "0.1"), otherwise 17, which identifies every double exactly.
key_as_character <- function(x) {
  if (is.numeric(x)) {
    out <- sprintf("%.15g", x)
    inexact <- !is.na(x) & suppressWarnings(as.numeric(out)) != x
    out[inexact] <- sprintf("%.17g", x[inexact])
    whole <- !is.na(x) & is.finite(x) & x == trunc(x)
    out[whole] <- sprintf("%.0f", x[whole])
    out[is.na(x)] <- NA_character_
    out
  } else {
    as.character(x)
  }
}

quasi_columns_by_subject <- function(df, spec, key, keys) {
  missing <- setdiff(spec$columns, names(df))
  if (length(missing) > 0) {
    stop("Column `", missing[1], "` not found in `", dataset_label(spec$dataset), "`.", call. = FALSE)
  }

  df <- df[c(key, spec$columns)]
  df[] <- lapply(df, blank_to_na)
  df[[key]] <- normalize_keys(df[[key]])
  df <- df[!is.na(df[[key]]), , drop = FALSE]
  distinct <- vctrs::vec_unique(df)
  if (anyDuplicated(distinct[[key]])) {
    stop(
      "Quasi-identifiers in `", dataset_label(spec$dataset), "` have more than one value per subject; ",
      "use `anon_quasi_values()` to choose one value per subject.",
      call. = FALSE
    )
  }

  rows <- match(keys, distinct[[key]])
  stats::setNames(lapply(spec$columns, function(col) distinct[[col]][rows]), spec$columns)
}

quasi_values_by_subject <- function(df, spec, key, keys) {
  missing <- setdiff(c(spec$test_col, spec$result_col), names(df))
  if (length(missing) > 0) {
    stop("Column `", missing[1], "` not found in `", dataset_label(spec$dataset), "`.", call. = FALSE)
  }

  df[[spec$result_col]] <- blank_to_na(df[[spec$result_col]])
  df_all_keys <- normalize_keys(df[[key]])
  keep <- !is.na(df_all_keys) & !is.na(df[[spec$test_col]]) &
    df[[spec$test_col]] %in% spec$tests & !is.na(df[[spec$result_col]])
  if (!is.null(spec$filter)) {
    selected <- rlang::eval_tidy(rlang::as_quosure(spec$filter), data = df)
    if (!is.logical(selected) || !length(selected) %in% c(1L, nrow(df))) {
      stop("`filter` for `", spec$dataset, "` must return TRUE or FALSE for each row.", call. = FALSE)
    }
    keep <- keep & !is.na(selected) & selected
  }
  df <- df[keep, , drop = FALSE]
  df_keys <- df_all_keys[keep]

  values <- lapply(spec$tests, function(test) {
    in_test <- df[[spec$test_col]] == test
    if (!any(in_test)) {
      warning(
        "No values for `", test, "` in `", spec$dataset, "`",
        if (!is.null(spec$filter)) " after `filter`" else "",
        "; it is missing for every subject.",
        call. = FALSE
      )
    }
    test_keys <- df_keys[in_test]
    test_values <- df[[spec$result_col]][in_test]
    first <- !duplicated(test_keys)
    test_values[first][match(keys, test_keys[first])]
  })
  stats::setNames(values, spec$names)
}

dataset_label <- function(ds) {
  if (identical(ds, "data")) "data" else ds
}

# Size of each subject's group of identical quasi-identifier values, by exact
# equality. NA is a value of its own.
quasi_group_sizes <- function(quasi_table) {
  if (nrow(quasi_table) == 0) {
    return(integer(0))
  }
  ids <- vctrs::vec_group_id(quasi_table)
  tabulate(ids)[ids]
}

risk_summary <- function(group_k, max_risk_target, k_target, n_quasi) {
  n <- length(group_k)
  min_k <- min(group_k)
  tibble::tibble(
    n_subjects = n,
    n_quasi = n_quasi,
    max_risk_target = max_risk_target,
    k_target = k_target,
    n_unique = sum(group_k == 1),
    min_k = min_k,
    max_risk = 1 / min_k,
    avg_risk = mean(1 / group_k),
    n_below_k = sum(group_k < k_target),
    prop_below_k = mean(group_k < k_target),
    meets_target = min_k >= k_target
  )
}

risk_drivers <- function(quasi_table, k_target) {
  vars <- names(quasi_table)
  rows <- lapply(vars, function(v) {
    rest <- quasi_table[setdiff(vars, v)]
    group_k <- if (ncol(rest) == 0) rep(nrow(quasi_table), nrow(quasi_table)) else quasi_group_sizes(rest)
    values <- quasi_table[[v]]
    tibble::tibble(
      variable = v,
      n_values = vctrs::vec_unique_count(values[!is.na(values)]),
      n_missing = sum(is.na(values)),
      n_unique_without = sum(group_k == 1),
      n_below_k_without = sum(group_k < k_target),
      min_k_without = min(group_k),
      avg_risk_without = mean(1 / group_k)
    )
  })
  drivers <- do.call(rbind, rows)
  drivers[order(
    drivers$n_below_k_without,
    drivers$n_unique_without,
    drivers$avg_risk_without,
    seq_len(nrow(drivers))
  ), , drop = FALSE]
}
