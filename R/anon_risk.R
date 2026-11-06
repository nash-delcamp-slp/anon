#' Measure re-identification risk from quasi-identifiers
#'
#' Measures how easily subjects could be singled out from the combination of
#' their quasi-identifiers (indirect identifiers such as age, sex, race,
#' ethnicity, site, height or weight). Each subject's group is the set of
#' subjects with exactly the same quasi-identifier values; `k` is the group's
#' size and the subject's risk is `1 / k`. Quasi-identifiers can come from
#' several datasets linked by `key`.
#'
#' @param data A data frame, or a named list of data frames linked by `key`.
#' @param quasi The quasi-identifiers to measure. For a single data frame, a
#'   character vector of column names. For a list of data frames, a list whose
#'   named elements are character vectors of columns in the dataset of that
#'   name (each subject must have one value per column, as in `dm`), and whose
#'   unnamed elements are [anon_quasi_values()] specifications for values that
#'   vary by row, such as baseline height and weight in `vs`.
#' @param key Name of the subject identifier column shared by the datasets.
#' @param k Target smallest group size. The default, 11, is the cell size that
#'   Health Canada equates with its encouraged re-identification risk threshold
#'   of 0.09 for public release of clinical information (see Details).
#'
#' @details
#' Subjects are all key values found in the datasets used by `quasi`. A subject
#' with no value for a quasi-identifier (for example, missing from `vs`) gets
#' `NA` for it, and `NA` is treated as a value of its own when forming groups.
#'
#' Risk is measured against the subjects in `data` only. Health Canada and EMA
#' allow risk to be measured against a larger reference population, which gives
#' lower risk; measuring within the data is the strictest reading.
#'
#' The threshold comes from Health Canada's *Public Release of Clinical
#' Information* guidance, which "encourages adopting a 9% re-identification
#' risk threshold (risk=0.09)" and states that it "equates to a target cell
#' size of 11 patients"
#' (<https://www.canada.ca/en/health-canada/services/drug-health-product-review-approval/profile-public-release-clinical-information-guidance/document.html>).
#' EMA's Anonymisation Report Form Instructions (EMA/482639/2023) repeat the
#' 0.09 threshold for quantitative anonymisation under EMA Policy 0070. Both
#' set it for public release of clinical documents; using it for other sharing
#' is a deliberately conservative choice. GDPR and HIPAA set no numerical
#' threshold. Meeting the target is evidence about re-identification risk from
#' the chosen quasi-identifiers, not proof that data is anonymous or
#' de-identified under any regime.
#'
#' @return An object of class `"anon_risk"`, a list with:
#'   - `summary`: one row with `n_subjects`, `n_quasi`, `k_target`,
#'     `n_unique` (subjects in a group of one), `min_k`, `max_risk`
#'     (`1 / min_k`), `avg_risk` (mean of `1 / k`), `n_below_k`,
#'     `prop_below_k` and `meets_target` (`min_k >= k_target`).
#'   - `drivers`: one row per quasi-identifier with `n_values` (distinct
#'     non-missing values), `n_missing`, and `n_unique_without`,
#'     `n_below_k_without` and `min_k_without` (the result if that variable
#'     were left out), ordered from the variable whose removal helps most.
#'   - `subjects`: one row per subject with the `key`, `k` and `risk`.
#'   - `quasi`, `key`: the quasi-identifier names and key used.
#'
#'   `subjects` contains the source key values. Printing and [as_anon_payload()]
#'   show only `summary` and `drivers`.
#'
#' @examples
#' dm <- data.frame(
#'   USUBJID = sprintf("S%02d", 1:6),
#'   AGE = c(60, 60, 60, 70, 70, 85),
#'   SEX = c("F", "F", "F", "M", "M", "M")
#' )
#' anon_risk(dm, quasi = c("AGE", "SEX"), k = 3)
#'
#' vs <- data.frame(
#'   USUBJID = rep(dm$USUBJID, each = 2),
#'   VSTESTCD = rep(c("HEIGHT", "WEIGHT"), 6),
#'   VSSTRESN = c(160, 60, 160, 60, 165, 62, 180, 80, 180, 80, 175, 90),
#'   VSBLFL = "Y"
#' )
#' anon_risk(
#'   list(dm = dm, vs = vs),
#'   quasi = list(
#'     dm = c("AGE", "SEX"),
#'     anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN",
#'                       filter = ~ VSBLFL == "Y")
#'   ),
#'   k = 2
#' )
#'
#' @export
anon_risk <- function(data, quasi, key = "USUBJID", k = 11) {
  if (!is.numeric(k) || length(k) != 1 || !is.finite(k) || k < 1 || k != round(k)) {
    stop("`k` must be a single whole number of at least 1.", call. = FALSE)
  }

  single <- is.data.frame(data)
  if (single) {
    data <- list(data = data)
  }
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "")) {
    stop("`data` must be a data frame or a named list of data frames.", call. = FALSE)
  }

  specs <- normalize_quasi_specs(quasi, data, single)
  table <- build_quasi_table(data, specs, key)
  quasi_names <- setdiff(names(table), key)

  group_k <- quasi_group_sizes(table[quasi_names])

  structure(
    list(
      summary = risk_summary(group_k, k, length(quasi_names)),
      drivers = risk_drivers(table[quasi_names], k),
      subjects = stats::setNames(
        tibble::tibble(table[[key]], group_k, 1 / group_k),
        c(key, "k", "risk")
      ),
      quasi = quasi_names,
      key = key
    ),
    class = "anon_risk"
  )
}

#' Specify quasi-identifier values that vary by row
#'
#' Describes how to take one value per subject from a long dataset, such as a
#' findings domain, for use in [anon_risk()]. For each test code in `tests`,
#' the first non-missing `result` value per subject (in row order, after
#' `filter`) becomes a quasi-identifier.
#'
#' @param dataset Name of the dataset in the list passed to [anon_risk()].
#' @param test Name of the column holding test codes (e.g. `"VSTESTCD"`).
#' @param tests Test codes to use (e.g. `c("HEIGHT", "WEIGHT")`).
#' @param result Name of the column holding the result (e.g. `"VSSTRESN"`).
#' @param filter Optional one-sided formula evaluated in the dataset to select
#'   rows, e.g. `~ VSBLFL == "Y"` for baseline values. It applies to every test
#'   in `tests`; use separate specifications when tests need different filters
#'   (for example, height recorded once at screening without a baseline flag).
#'   A warning is given when a test has no values.
#' @param names Names for the resulting quasi-identifiers. Defaults to `tests`.
#'
#' @return An object of class `"anon_quasi_values"`.
#'
#' @examples
#' anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN",
#'                   filter = ~ VSBLFL == "Y")
#'
#' @export
anon_quasi_values <- function(dataset, test, tests, result, filter = NULL, names = tests) {
  if (!is.null(filter) && !rlang::is_formula(filter)) {
    stop("`filter` must be NULL or a one-sided formula such as `~ VSBLFL == \"Y\"`.", call. = FALSE)
  }
  if (length(names) != length(tests)) {
    stop("`names` must have the same length as `tests`.", call. = FALSE)
  }
  structure(
    list(
      dataset = dataset,
      test = test,
      tests = tests,
      result = result,
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
  cat("Smallest group (k):", s$min_k, " Target k:", s$k_target, "\n")
  cat("Maximum risk:", format(signif(s$max_risk, 3)),
      " Average risk:", format(signif(s$avg_risk, 3)), "\n")
  cat("Unique subjects:", s$n_unique, "\n")
  cat("Subjects below target:", s$n_below_k,
      paste0("(", format(round(100 * s$prop_below_k, 1)), "%)"), "\n")
  cat("Meets target:", if (isTRUE(s$meets_target)) "yes" else "no", "\n\n")
  cat("Drivers (result if each variable were left out):\n")
  print(as.data.frame(x$drivers), row.names = FALSE)
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
    list(dataset = spec_names[i], columns = spec)
  })
}

# One row per subject: the key and one column per quasi-identifier.
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

  keys <- unique(unlist(lapply(datasets, function(ds) as.character(data[[ds]][[key]])), use.names = FALSE))
  keys <- keys[!is.na(keys)]

  columns <- list()
  sources <- character(0)
  for (spec in specs) {
    df <- data[[spec$dataset]]
    values <- if (inherits(spec, "anon_quasi_values")) {
      quasi_values_by_subject(df, spec, key, keys)
    } else {
      quasi_columns_by_subject(df, spec, key, keys)
    }
    columns <- c(columns, values)
    sources <- c(sources, rep(spec$dataset, length(values)))
  }

  col_names <- names(columns)
  repeated <- col_names %in% col_names[duplicated(col_names)]
  col_names[repeated] <- paste0(sources[repeated], ".", col_names[repeated])
  names(columns) <- col_names

  tibble::as_tibble(c(stats::setNames(list(keys), key), columns))
}

quasi_columns_by_subject <- function(df, spec, key, keys) {
  missing <- setdiff(spec$columns, names(df))
  if (length(missing) > 0) {
    stop("Column `", missing[1], "` not found in `", dataset_label(spec$dataset), "`.", call. = FALSE)
  }

  distinct <- unique(df[c(key, spec$columns)])
  distinct <- distinct[!is.na(distinct[[key]]), , drop = FALSE]
  if (anyDuplicated(as.character(distinct[[key]]))) {
    stop(
      "Quasi-identifiers in `", dataset_label(spec$dataset), "` have more than one value per subject; ",
      "use `anon_quasi_values()` to choose one value per subject.",
      call. = FALSE
    )
  }

  rows <- match(keys, as.character(distinct[[key]]))
  stats::setNames(lapply(spec$columns, function(col) distinct[[col]][rows]), spec$columns)
}

quasi_values_by_subject <- function(df, spec, key, keys) {
  missing <- setdiff(c(spec$test, spec$result), names(df))
  if (length(missing) > 0) {
    stop("Column `", missing[1], "` not found in `", dataset_label(spec$dataset), "`.", call. = FALSE)
  }

  keep <- !is.na(df[[spec$test]]) & df[[spec$test]] %in% spec$tests & !is.na(df[[spec$result]])
  if (!is.null(spec$filter)) {
    selected <- rlang::eval_tidy(rlang::f_rhs(spec$filter), data = df)
    keep <- keep & !is.na(selected) & selected
  }
  df <- df[keep, , drop = FALSE]

  values <- lapply(spec$tests, function(test) {
    rows <- df[df[[spec$test]] == test, , drop = FALSE]
    if (nrow(rows) == 0) {
      warning(
        "No values for `", test, "` in `", spec$dataset, "`",
        if (!is.null(spec$filter)) " after `filter`" else "",
        "; it is missing for every subject.",
        call. = FALSE
      )
    }
    rows <- rows[!duplicated(as.character(rows[[key]])), , drop = FALSE]
    rows[[spec$result]][match(keys, as.character(rows[[key]]))]
  })
  stats::setNames(values, spec$names)
}

dataset_label <- function(ds) {
  if (identical(ds, "data")) "data" else ds
}

# Size of each subject's group of identical quasi-identifier values. NA is a
# value of its own.
quasi_group_sizes <- function(quasi_table) {
  if (nrow(quasi_table) == 0) {
    return(integer(0))
  }
  parts <- lapply(quasi_table, function(v) {
    out <- as.character(v)
    out[is.na(v)] <- "\u0001NA"
    out
  })
  combo <- do.call(paste, c(parts, sep = "\u001f"))
  counts <- table(combo)
  as.integer(counts[combo])
}

risk_summary <- function(group_k, k_target, n_quasi) {
  n <- length(group_k)
  min_k <- if (n > 0) min(group_k) else NA_integer_
  tibble::tibble(
    n_subjects = n,
    n_quasi = n_quasi,
    k_target = k_target,
    n_unique = sum(group_k == 1),
    min_k = min_k,
    max_risk = 1 / min_k,
    avg_risk = if (n > 0) mean(1 / group_k) else NA_real_,
    n_below_k = sum(group_k < k_target),
    prop_below_k = if (n > 0) mean(group_k < k_target) else NA_real_,
    meets_target = n > 0 && min_k >= k_target
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
      n_values = length(unique(values[!is.na(values)])),
      n_missing = sum(is.na(values)),
      n_unique_without = sum(group_k == 1),
      n_below_k_without = sum(group_k < k_target),
      min_k_without = if (length(group_k) > 0) min(group_k) else NA_integer_
    )
  })
  drivers <- do.call(rbind, rows)
  drivers[order(drivers$n_below_k_without, drivers$n_unique_without, -drivers$min_k_without), , drop = FALSE]
}
