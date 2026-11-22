#' Rules that generalize quasi-identifier values
#'
#' Each function returns a rule for [anon_generalize()]: `anon_band()` puts
#' numbers into bands, `anon_round()` rounds them, and `anon_fold()` merges
#' small categories into one. A rule is also a function: call it on a vector
#' to apply it, as in `anon_band(5)(ages)`. Rules keep the vector's type,
#' class and attributes, so code written for the original values still runs.
#'
#' @param width Width of each band, a positive number (for example 5 for
#'   5-year age bands). Bands start at `bottom` when given, otherwise at 0:
#'   with `width = 5` they are 0-4.99..., 5-9.99..., and so on, whatever the
#'   data's range; with `bottom = 18` they are 18-22.99..., 23-27.99...
#' @param top,bottom Optional limits: values at or above `top` become `top`,
#'   and values at or below `bottom` become `bottom`, instead of being banded
#'   (top and bottom coding). HIPAA Safe Harbor, for example, groups ages
#'   over 89 into one category, which `top = 90` gives.
#' @param value The number that stands for a band: `"lower"` (default), its
#'   lower bound, or `"midpoint"`, its middle, kept within `bottom` and `top`.
#'   Either is a representative, not a subject's real value.
#' @param digits Number of decimal places to round to, as in [round()];
#'   negative values round to tens (`-1`), hundreds (`-2`) and so on.
#' @param min_n Smallest number of subjects a category must hold to be kept.
#'   Categories held by fewer subjects become `to`. Subjects are counted, not
#'   rows (see Details).
#' @param to The value small categories become, such as `"OTHER"`. There is
#'   no default: choose a value that is allowed in the column (for SDTM, a
#'   term in the variable's controlled terminology). A number for a numeric
#'   column.
#'
#' @details
#' **Bands and rounding** apply to numeric vectors (double or integer, also
#' haven-labelled). The result stays numeric; an integer vector stays integer,
#' and a rule that would give it a number that isn't whole (a midpoint of an
#' odd width, say) is an error, as is one too large for an integer. Missing
#' values, haven user-defined missing values, and values that carry a haven
#' value label (codes such as 99 for "not done") are left as they are.
#' Rounding follows [round()].
#'
#' **Folding** applies to character, factor and numeric vectors (also
#' haven-labelled). Missing and blank values are never folded or counted, and
#' `to` itself is never folded (it can stay small: measure again with
#' [anon_risk()] and suppress what remains with [anon_suppress()]). A factor
#' keeps its class: the folded levels are removed (with unused levels held by
#' fewer than `min_n` subjects) and `to` is added as the last level if it
#' isn't one already. Called on a vector, each element counts as one subject
#' unless `subjects` gives the subject of each element. In
#' [anon_generalize()], subjects are counted by the key, across every dataset
#' where the same column has the same rule.
#'
#' haven value labels are kept where they still describe the values: a fold
#' drops the labels of folded categories, and a band or rounding drops the
#' label of any code that values moved into or out of.
#'
#' @return An object of class `"anon_generalize_rule"`, a function
#'   `function(x, subjects = NULL)` that returns `x` generalized. `subjects`
#'   (used by `anon_fold()` only) gives the subject of each element of `x`.
#'
#' @seealso [anon_generalize()] to apply rules to datasets, [anon_rule()] for
#'   test results such as weight in `vs`.
#'
#' @examples
#' ages <- c(23, 37, 41, 58, 64, 91)
#' anon_band(10, top = 90)(ages)
#' anon_band(10, value = "midpoint")(ages)
#' anon_round(-1)(c(153.2, 171.8))
#'
#' race <- c("WHITE", "WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
#' anon_fold(2, to = "OTHER")(race)
#' anon_fold(2, to = "OTHER")
#'
#' @name generalize_rules
NULL

#' @rdname generalize_rules
#' @export
anon_band <- function(width, top = NULL, bottom = NULL, value = c("lower", "midpoint")) {
  value <- match.arg(value)
  check_rule_number(width, "width")
  if (width <= 0) {
    stop("`width` must be greater than 0.", call. = FALSE)
  }
  if (!is.null(top)) check_rule_number(top, "top")
  if (!is.null(bottom)) check_rule_number(bottom, "bottom")
  if (!is.null(top) && !is.null(bottom) && bottom >= top) {
    stop("`bottom` must be below `top`.", call. = FALSE)
  }
  new_generalize_rule("band", list(width = width, top = top, bottom = bottom, value = value))
}

#' @rdname generalize_rules
#' @export
anon_round <- function(digits) {
  check_rule_number(digits, "digits")
  if (digits != round(digits) || abs(digits) > 15) {
    stop("`digits` must be a whole number from -15 to 15.", call. = FALSE)
  }
  new_generalize_rule("round", list(digits = digits))
}

#' @rdname generalize_rules
#' @export
anon_fold <- function(min_n, to) {
  check_rule_number(min_n, "min_n")
  if (min_n != round(min_n) || min_n < 1) {
    stop("`min_n` must be a whole number of at least 1.", call. = FALSE)
  }
  if (missing(to)) {
    stop(
      "`to` is required: the value small categories become, such as \"OTHER\". ",
      "Choose one that is allowed in the column.",
      call. = FALSE
    )
  }
  if (!(is.character(to) || is.numeric(to)) || length(to) != 1 || is.na(to) || is.factor(to)) {
    stop("`to` must be a single string or number.", call. = FALSE)
  }
  if (is.character(to) && !nzchar(trimws(to))) {
    stop("`to` must not be blank: blank is how missing text is stored.", call. = FALSE)
  }
  if (is.numeric(to) && !is.finite(to)) {
    stop("`to` must be a finite number.", call. = FALSE)
  }
  new_generalize_rule("fold", list(min_n = min_n, to = to))
}

#' Generalize the results of a test
#'
#' Pairs an [anon_quasi_values()] specification with a rule, for
#' [anon_generalize()]: the rule applies to every row of each test in the
#' specification, and the test's other result columns are kept consistent
#' with it.
#'
#' @param spec An [anon_quasi_values()] specification, such as the one given
#'   to [anon_risk()].
#' @param rule A rule made by [anon_band()], [anon_round()] or [anon_fold()].
#'
#' @details
#' The rule applies to the `result_col` of **every row** of each test, not
#' only the row [anon_risk()] measures: any row identifies a subject as well
#' as the baseline one. `filter` is not used here, for that reason.
#'
#' When `result_col` is a numeric SDTM result (`--STRESN`) and the rule is a
#' band or rounding, the text results of the same rows follow it:
#' - `--STRESC` is rewritten from the generalized `--STRESN`;
#' - `--ORRES` is rewritten from the generalized `--STRESN`: as it is where
#'   the units are the same (`--ORRESU` equals `--STRESU`), and otherwise
#'   times the row's own conversion factor (original `--ORRES` / original
#'   `--STRESN`), so it is never finer than `--STRESN` and unit conversion
#'   checks still pass. Without a factor (`--STRESN` missing or 0, or no unit
#'   columns and different values), it is blanked;
#'
#' each in its own form (plain, with thousands separators such as
#' `"1,234"`, or in scientific notation such as `"2.5E2"`) and with as many
#' decimals as it had (in the same units, more where the generalized number
#' needs them, such as a midpoint of `62.5` for `"64"`). Other results, such
#' as `"<5"` or `"ND"`, are left unchanged and counted in the log, with a
#' warning where the row has a numeric result.
#'
#' For ADaM, with `result_col = "AVAL"`, `AVALC` is rewritten from the
#' generalized `AVAL`, `BASE` is generalized by the same rule (it is the
#' baseline `AVAL`) with `BASEC` rewritten from it, and `CHG` (`AVAL - BASE`),
#' `PCHG` (`100 * CHG / BASE`) and `R2BASE` (`AVAL / BASE`) are recomputed
#' from the generalized values. Where the generalized `BASE` is 0 (a band or
#' rounding can make it so), `PCHG` and `R2BASE` become missing. Other
#' derived columns (such as `BASETYPE` analyses, `CRIT` flags or `AVALCAT`)
#' are not handled: generalize or remove them. Other result columns are
#' generalized alone.
#'
#' With [anon_fold()], `result_col` must be text; on the rows folded, the
#' other text results (`--ORRES` or `--STRESC`; `AVALC`, `BASEC`) become `to`
#' too, and numeric ones become missing. Only the categories found in the
#' test's rows count (a factor's levels are shared by every test).
#'
#' @return An object of class `"anon_rule_values"`.
#'
#' @seealso [anon_generalize()]
#'
#' @examples
#' anon_rule(
#'   anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN"),
#'   anon_round(0)
#' )
#'
#' @export
anon_rule <- function(spec, rule) {
  if (!inherits(spec, "anon_quasi_values")) {
    stop("`spec` must be an `anon_quasi_values()` specification.", call. = FALSE)
  }
  if (!inherits(rule, "anon_generalize_rule")) {
    stop("`rule` must be made by `anon_band()`, `anon_round()` or `anon_fold()`.", call. = FALSE)
  }
  structure(list(spec = spec, rule = rule), class = "anon_rule_values")
}

#' @export
print.anon_generalize_rule <- function(x, ...) {
  cat("<anon rule> ", describe_rule(x), "\n", sep = "")
  invisible(x)
}

#' @export
print.anon_rule_values <- function(x, ...) {
  s <- x$spec
  cat("<anon rule> ", describe_rule(x$rule), " on `", s$dataset, "`: ", s$result_col, " where ", s$test_col,
      " is ", paste0("\"", s$tests, "\"", collapse = ", "), " (every row)\n", sep = "")
  invisible(x)
}

#' Generalize quasi-identifiers across datasets
#'
#' Applies generalization rules ([anon_band()], [anon_round()],
#' [anon_fold()]) to columns and test results of linked datasets, to bring
#' the re-identification risk measured by [anon_risk()] down. Every column
#' keeps its type, class and attributes, and the data keeps its rows in the
#' same order, so code tested on the generalized copy still runs on the
#' original.
#'
#' @param data A data frame, a named list of data frames linked by `key`, or
#'   the result of an earlier step such as [anon_subject_ids()].
#' @param rules A list of rules. Elements named by a dataset are named lists
#'   of rules for that dataset's columns, as in
#'   `list(dm = list(AGE = anon_band(5, top = 90), RACE = anon_fold(12, "OTHER")))`;
#'   the rule applies to the column in every row. Unnamed elements are
#'   [anon_rule()] pairs of an [anon_quasi_values()] specification and a rule,
#'   which apply to every row of the test and keep its text results
#'   consistent. For a single data frame, a named list of rules by column,
#'   such as `list(AGE = anon_band(5))`, is enough.
#' @param key Name of the subject identifier column, used by [anon_fold()] to
#'   count subjects.
#' @param keep_secrets Logical. If `TRUE`, keep which categories were folded,
#'   with their numbers of subjects; read them with [anon_secrets()] (table
#'   `generalize_folded`). Default `FALSE`: they appear nowhere, because the
#'   names of small categories are themselves rare values.
#'
#' @details
#' A column listed for several datasets (such as `AGE` in `dm` and in an ADaM
#' `adsl`) is generalized in each dataset where it is listed, and only there:
#' list every dataset that holds it. [anon_fold()] rules with the same
#' parameters on columns of the same name count subjects across all those
#' datasets together, so the same categories are folded everywhere.
#'
#' Rules apply to the columns of the datasets themselves. Data frames nested
#' in them (packed columns and list columns, see [anon_subject_ids()]) are
#' not generalized. A warning names the datasets with the key that hold a
#' generalized column under the same name without a rule for it.
#'
#' A birth date more precise than a year (`BRTHDTC`, or a Date `BRTHDT`) in a
#' dataset whose `AGE` is generalized gives the age back with a reference
#' date such as `RFSTDTC`, even after the dates are offset: a warning names
#' it. Remove birth dates or reduce them to the year. Other dates and derived
#' columns can give a quasi-identifier back the same way (see
#' [anon_suppress()]).
#'
#' The log has one row per dataset, column and test, counting the values
#' changed; it holds no values, and the settings record the rules. The step
#' can be chained with [anon_subject_ids()], [anon_subject_dates()] and
#' [anon_suppress()] in any order: rules name columns, which those steps
#' don't rename (unless the names hold keys or dates).
#'
#' Generalized values are representatives (a band's lower bound or midpoint,
#' a rounded number, a folded category), not the subjects' real values. Tell
#' whoever uses the data, for example with the log in a report.
#'
#' @return An object of class `"anon_deid_step"`, as for [anon_subject_ids()].
#'
#' @seealso [generalize_rules], [anon_rule()], [anon_risk()],
#'   [anon_suppress()]
#'
#' @examples
#' dm <- data.frame(
#'   USUBJID = sprintf("S%02d", 1:6),
#'   AGE = c(34, 37, 52, 58, 91, 93),
#'   RACE = c("WHITE", "WHITE", "WHITE", "ASIAN", "WHITE", "WHITE")
#' )
#' vs <- data.frame(
#'   USUBJID = rep(dm$USUBJID, each = 2),
#'   VSTESTCD = "WEIGHT",
#'   VSORRES = c("70.4", "71.0", "65.2", "66.0", "80.9", "79.5",
#'               "150.2", "151.0", "60.3", "60.0", "55.5", "56.1"),
#'   VSORRESU = rep(c("kg", "kg", "kg", "kg", "LB", "kg"), each = 2),
#'   VSSTRESC = c("70.4", "71.0", "65.2", "66.0", "80.9", "79.5",
#'                "68.13", "68.49", "60.3", "60.0", "55.5", "56.1"),
#'   VSSTRESN = c(70.4, 71.0, 65.2, 66.0, 80.9, 79.5, 68.13, 68.49, 60.3, 60.0, 55.5, 56.1),
#'   VSSTRESU = "kg"
#' )
#' result <- anon_generalize(
#'   list(dm = dm, vs = vs),
#'   rules = list(
#'     dm = list(AGE = anon_band(10, top = 90), RACE = anon_fold(2, "OTHER")),
#'     anon_rule(anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN"), anon_round(0))
#'   )
#' )
#' result$data$dm
#' result$data$vs
#' result$log
#'
#' @export
anon_generalize <- function(data, rules, key = "USUBJID", keep_secrets = FALSE) {
  single <- is.data.frame(if (inherits(data, "anon_deid_step")) data$data else data)
  input <- as_dataset_list(data)
  data <- input$data
  check_key_name(key)
  targets <- generalize_targets(rules, data, single)

  # Categories to fold, by group of fold rules counted together.
  fold_groups <- list()
  for (i in seq_along(targets)) {
    tg <- targets[[i]]
    if (rule_kind(tg$rule) == "fold") {
      group <- tg$fold_group
      fold_groups[[group]] <- rbind(fold_groups[[group]], fold_pairs(data, tg, key))
    }
  }
  fold_sets <- lapply(fold_groups, function(pairs) {
    params <- rule_params(targets[[pairs$target[1]]]$rule)
    counts <- tapply(pairs$subject, pairs$category, function(s) length(unique(s[!is.na(s)])))
    folded <- setdiff(names(counts)[counts < params$min_n], rule_category_id(params$to))
    list(categories = folded, counts = counts[folded], datasets = unique(pairs$dataset))
  })

  key_data <- Filter(function(df) key %in% names(df), data)
  n_subjects <- length(subject_keys(key_data, key))
  log <- deid_log_start(
    "generalize", n_subjects, key, NULL,
    settings = paste0("rules: ", paste(vapply(targets, describe_target, character(1)), collapse = "; "))
  )
  folded_secret <- list()
  for (tg in targets) {
    fold_set <- if (rule_kind(tg$rule) == "fold") fold_sets[[tg$fold_group]]
    applied <- if (tg$type == "column") {
      generalize_column(data[[tg$dataset]], tg, fold_set$categories)
    } else {
      generalize_test(data[[tg$dataset]], tg, key, fold_set$categories)
    }
    data[[tg$dataset]] <- applied$df
    for (row in applied$log) {
      log <- deid_log_add(log, tg$dataset, row$variable, row$detail, row$n_changed, 0)
    }
    held <- if (!is.null(fold_set)) fold_set$counts > 0
    if (any(held)) {
      folded_secret <- c(folded_secret, list(tibble::tibble(
        dataset = tg$dataset, variable = target_variable(tg), category = fold_set$categories[held],
        to = as.character(rule_params(tg$rule)$to), n_subjects = as.integer(fold_set$counts[held])
      )))
    }
  }

  ages <- unique(vapply(Filter(function(tg) tg$type == "column" && toupper(tg$column) == "AGE", targets), `[[`, character(1), "dataset"))
  births <- precise_birth_dates(data, ages)
  if (length(births) > 0) {
    warn_birth_dates(births)
  }
  warn_uncovered_copies(data, targets, key)

  secrets <- if (isTRUE(keep_secrets)) {
    folded <- if (length(folded_secret) > 0) do.call(rbind, folded_secret) else tibble::tibble(
      dataset = character(0), variable = character(0), category = character(0),
      to = character(0), n_subjects = integer(0)
    )
    list(generalize_folded = folded)
  }
  settings <- list(generalize = list(
    key = key, rules = lapply(targets, target_settings), keep_secrets = isTRUE(keep_secrets)
  ))
  new_deid_step(input$unwrap(data), log, secrets, previous = input$previous, settings = settings)
}

# helpers -----------------------------------------------------------------

check_rule_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    stop("`", name, "` must be a single number.", call. = FALSE)
  }
}

# A rule is a function of a vector (and, for folds, the subject of each
# element) that keeps its kind and parameters in its environment.
new_generalize_rule <- function(kind, params) {
  rule <- function(x, subjects = NULL) {
    generalize_values(kind, params, x, subjects)$values
  }
  structure(rule, class = c(paste0("anon_", kind), "anon_generalize_rule", "function"))
}

rule_kind <- function(rule) environment(rule)$kind
rule_params <- function(rule) environment(rule)$params

format_rule_value <- function(x) {
  if (is.character(x)) paste0("\"", x, "\"") else format(x, digits = 15)
}

# The rule as the call that makes it, such as `anon_band(width = 5, top = 90)`.
describe_rule <- function(rule) {
  params <- Filter(Negate(is.null), rule_params(rule))
  if (identical(params$value, "lower")) {
    params$value <- NULL
  }
  args <- vapply(names(params), function(n) paste0(n, " = ", format_rule_value(params[[n]])), character(1))
  paste0("anon_", rule_kind(rule), "(", paste(args, collapse = ", "), ")")
}

# What a rule does, for the log.
rule_detail <- function(rule) {
  p <- rule_params(rule)
  switch(rule_kind(rule),
    band = paste0(
      "banded: width ", format_rule_value(p$width),
      if (!is.null(p$top)) paste0(", at or above ", format_rule_value(p$top), " set to it"),
      if (!is.null(p$bottom)) paste0(", at or below ", format_rule_value(p$bottom), " set to it"),
      ", ", if (p$value == "lower") "lower bound" else "midpoint", " as the value"
    ),
    round = paste0("rounded to ", p$digits, " decimal place(s)"),
    fold = paste0("categories of fewer than ", p$min_n, " subjects became ", format_rule_value(p$to))
  )
}

# Each rule as a target: a column of a dataset, or a test's results.
generalize_targets <- function(rules, data, single) {
  if (!is.list(rules) || inherits(rules, c("anon_generalize_rule", "anon_rule_values")) || length(rules) == 0) {
    stop("`rules` must be a non-empty list of rules.", call. = FALSE)
  }
  rule_names <- names(rules)
  if (is.null(rule_names)) {
    rule_names <- rep("", length(rules))
  }
  # A single data frame: rules may be given by column.
  is_rule <- vapply(rules, inherits, logical(1), "anon_generalize_rule")
  if (single && any(is_rule)) {
    if (!all(is_rule | rule_names == "") || any(rule_names[is_rule] == "")) {
      stop("For a single data frame, give rules by column, e.g. `list(AGE = anon_band(5))`.", call. = FALSE)
    }
    rules <- c(list(data = rules[is_rule]), unname(rules[!is_rule]))
    rule_names <- c("data", rep("", sum(!is_rule)))
  }

  targets <- list()
  for (i in seq_along(rules)) {
    element <- rules[[i]]
    ds <- rule_names[i]
    if (inherits(element, "anon_rule_values")) {
      if (nzchar(ds)) {
        stop("`anon_rule()` elements of `rules` must not be named.", call. = FALSE)
      }
      spec <- element$spec
      check_rule_dataset(data, spec$dataset)
      df <- data[[spec$dataset]]
      for (col in c(spec$test_col, spec$result_col)) {
        if (!col %in% names(df)) {
          stop("Column `", col, "` not found in `", dataset_label(spec$dataset), "`.", call. = FALSE)
        }
      }
      for (test in spec$tests) {
        targets <- c(targets, list(list(
          type = "test", dataset = spec$dataset, test_col = spec$test_col, test = test,
          result_col = spec$result_col, rule = element$rule
        )))
      }
      next
    }
    if (!nzchar(ds) || !is.list(element) || inherits(element, "anon_generalize_rule") ||
      is.null(names(element)) || any(names(element) == "") ||
      !all(vapply(element, inherits, logical(1), "anon_generalize_rule"))) {
      stop(
        "Each element of `rules` must be named by a dataset and hold rules named by column, ",
        "e.g. `dm = list(AGE = anon_band(5))`, or be an `anon_rule()`.",
        call. = FALSE
      )
    }
    check_rule_dataset(data, ds)
    for (col in names(element)) {
      if (!col %in% names(data[[ds]])) {
        stop("Column `", col, "` not found in `", dataset_label(ds), "`.", call. = FALSE)
      }
      targets <- c(targets, list(list(type = "column", dataset = ds, column = col, rule = element[[col]])))
    }
  }

  ids <- vapply(targets, function(tg) {
    paste(tg$dataset, if (tg$type == "column") tg$column else paste(tg$test_col, tg$test, tg$result_col), sep = "\u001f")
  }, character(1))
  if (anyDuplicated(ids)) {
    dup <- targets[[which(duplicated(ids))[1]]]
    stop("`", target_variable(dup), "` in `", dataset_label(dup$dataset), "` has more than one rule.", call. = FALSE)
  }
  for (i in seq_along(targets)) {
    tg <- targets[[i]]
    if (rule_kind(tg$rule) == "fold") {
      # Folds of columns with the same name and parameters are counted
      # together across datasets; a test's are counted on their own.
      targets[[i]]$fold_group <- if (tg$type == "column") {
        paste("column", tg$column, describe_rule(tg$rule), sep = "\u001f")
      } else {
        paste("test", ids[i], describe_rule(tg$rule), sep = "\u001f")
      }
      targets[[i]]$index <- i
    }
  }
  targets
}

check_rule_dataset <- function(data, ds) {
  if (!ds %in% names(data)) {
    stop("Dataset `", ds, "` named in `rules` not found in `data`.", call. = FALSE)
  }
}

target_variable <- function(tg) {
  if (tg$type == "column") tg$column else test_variable(tg$result_col, tg$test_col, tg$test)
}

test_variable <- function(col, test_col, test) paste0(col, " (", test_col, " = ", test, ")")

describe_target <- function(tg) {
  paste0(tg$dataset, " ", target_variable(tg), ": ", describe_rule(tg$rule))
}

# A target as recorded in the step's settings: where it applies and the
# rule's parameters (no values from the data).
target_settings <- function(tg) {
  where <- if (tg$type == "column") {
    list(dataset = tg$dataset, column = tg$column)
  } else {
    list(dataset = tg$dataset, test_col = tg$test_col, test = tg$test, result_col = tg$result_col)
  }
  c(where, list(rule = rule_kind(tg$rule)), Filter(Negate(is.null), rule_params(tg$rule)))
}

# The (category, subject) pairs a fold target holds, for counting subjects.
fold_pairs <- function(data, tg, key) {
  df <- data[[tg$dataset]]
  if (!key %in% names(df)) {
    stop(
      "`anon_fold()` counts subjects by `", key, "`, which `", dataset_label(tg$dataset), "` doesn't have.",
      call. = FALSE
    )
  }
  rows <- if (tg$type == "column") rep(TRUE, nrow_of(df)) else test_rows(df, tg)
  x <- df[[if (tg$type == "column") tg$column else tg$result_col]]
  if (tg$type == "test" && !is_text_vector(x)) {
    stop(
      "`anon_fold()` on a test needs a text result column; `", tg$result_col, "` in `",
      dataset_label(tg$dataset), "` isn't text.",
      call. = FALSE
    )
  }
  cats <- fold_categories(vctrs::vec_slice(x, rows))
  if (tg$type == "test") {
    # A factor's levels are shared by every test: only this test's values count.
    cats$levels <- character(0)
  }
  subjects <- normalize_keys(df[[key]])[rows]
  pairs <- data.frame(
    category = c(cats$id[cats$present], setdiff(cats$levels, cats$id[cats$present])),
    subject = c(subjects[cats$present], rep(NA_character_, length(setdiff(cats$levels, cats$id[cats$present])))),
    stringsAsFactors = FALSE
  )
  pairs$dataset <- rep(tg$dataset, nrow(pairs))
  pairs$target <- rep(tg$index, nrow(pairs))
  pairs
}

is_text_vector <- function(x) {
  is.factor(x) || is.character(if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else x)
}

# Rows of a test, at any row (not only the measured one).
test_rows <- function(df, tg) {
  tests <- df[[tg$test_col]]
  !is.na(tests) & as.character(tests) == tg$test
}

generalize_column <- function(df, tg, fold_set) {
  x <- df[[tg$column]]
  out <- generalize_values(rule_kind(tg$rule), rule_params(tg$rule), x, fold_set = fold_set, what = tg$column)
  df <- set_generalized_column(df, tg$column, out$values, any(out$changed) || !identical(levels(out$values), levels(x)))
  list(df = df, log = list(list(variable = tg$column, detail = rule_detail(tg$rule), n_changed = sum(out$changed))))
}

# Write a changed column back, keeping the data frame's class; a grouped or
# rowwise tibble whose grouping column changed is regrouped.
set_generalized_column <- function(df, col, value, changed) {
  if (!changed) {
    return(df)
  }
  df <- set_column(df, match(col, names(df)), value)
  if (inherits(df, c("grouped_df", "rowwise_df")) && col %in% names(attr(df, "groups", exact = TRUE))) {
    df <- vctrs::vec_slice(df, seq_len(nrow_of(df)))
  }
  df
}

# Generalize every row of a test, keeping its other results consistent.
generalize_test <- function(df, tg, key, fold_set) {
  rows <- which(test_rows(df, tg))
  kind <- rule_kind(tg$rule)
  params <- rule_params(tg$rule)
  x <- df[[tg$result_col]]
  if (kind != "fold" && (is.factor(x) || !is.numeric(if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else x))) {
    stop(
      "`anon_", kind, "()` on a test needs a numeric result column (such as `--STRESN`); `",
      tg$result_col, "` in `", dataset_label(tg$dataset), "` isn't numeric.",
      call. = FALSE
    )
  }
  where <- function(col) test_variable(col, tg$test_col, tg$test)
  old_result <- vctrs::vec_slice(x, rows)
  out <- generalize_values(kind, params, old_result, fold_set = fold_set, what = tg$result_col)
  log <- list(list(variable = where(tg$result_col), detail = rule_detail(tg$rule), n_changed = sum(out$changed)))
  df <- set_generalized_column(df, tg$result_col, replace_rows(x, rows, out$values), any(out$changed))

  siblings <- result_siblings(df, tg$result_col)
  if (kind == "fold") {
    hit <- rows[out$changed]
    for (col in setdiff(siblings, tg$result_col)) {
      y <- df[[col]]
      if (is_text_vector(y)) {
        new <- replace_at(y, hit, as.character(params$to))
        n <- sum(changed_values(vctrs::vec_slice(y, hit), vctrs::vec_slice(new, hit)))
        detail <- paste0("set to ", format_rule_value(as.character(params$to)), " where `", tg$result_col, "` was folded")
      } else {
        new <- replace_at(y, hit, NA)
        n <- sum(changed_values(vctrs::vec_slice(y, hit), vctrs::vec_slice(new, hit)))
        detail <- paste0("set to missing where `", tg$result_col, "` was folded")
      }
      df <- set_generalized_column(df, col, new, n > 0)
      log <- c(log, list(list(variable = where(col), detail = detail, n_changed = n)))
    }
    return(list(df = df, log = log))
  }

  new_result <- as.double(vctrs::vec_data(out$values))
  old_values <- as.double(vctrs::vec_data(old_result))
  unparsed <- character(0)
  text_sibling <- function(df, col, base, ratio, same) {
    y <- df[[col]]
    if (is.null(y) || !is_text_vector(y)) {
      return(list(df = df, log = NULL))
    }
    out <- rewrite_number_text(y, rows, base, ratio, same, kind, params, col)
    if (out$n_unparsed_with_result > 0) unparsed <<- c(unparsed, where(col))
    detail <- paste0(
      out$n_from, " rewritten from the generalized `", tg$result_col, "`",
      if (out$n_ratio > 0) paste0(" (", out$n_ratio, " in other units, by each row's own conversion factor)"),
      if (out$n_own > 0) paste0(", ", out$n_own, " generalized on their own (", rule_detail(tg$rule), ")"),
      if (out$n_blanked > 0) paste0(", ", out$n_blanked, " blanked (no conversion factor)"),
      " at each value's decimals",
      if (out$n_unparsed > 0) paste0("; ", out$n_unparsed, " value(s) that aren't numbers left unchanged")
    )
    list(df = set_generalized_column(df, col, out$values, out$n > 0), log = list(variable = where(col), detail = detail, n_changed = out$n))
  }
  add <- function(res) {
    df <<- res$df
    if (!is.null(res$log)) log <<- c(log, list(res$log))
  }
  if (tg$result_col == "AVAL") {
    # ADaM: AVALC from AVAL; BASE by the same rule (it is the baseline AVAL),
    # BASEC from it, and CHG and PCHG recomputed from the generalized values.
    add(text_sibling(df, "AVALC", new_result, rep(1, length(rows)), TRUE))
    new_base <- NULL
    if ("BASE" %in% names(df) && is.numeric(vctrs::vec_data(df$BASE)) && !is.factor(df$BASE)) {
      b <- generalize_values(kind, params, vctrs::vec_slice(df$BASE, rows), what = "BASE")
      df <- set_generalized_column(df, "BASE", replace_rows(df$BASE, rows, b$values), any(b$changed))
      log <- c(log, list(list(variable = where("BASE"), detail = rule_detail(tg$rule), n_changed = sum(b$changed))))
      new_base <- as.double(vctrs::vec_data(b$values))
      add(text_sibling(df, "BASEC", new_base, rep(1, length(rows)), TRUE))
    }
    if (!is.null(new_base)) {
      chg <- round(new_result - new_base, 10)
      nonzero <- !is.na(new_base) & new_base != 0
      for (col in intersect(c("CHG", "PCHG", "R2BASE"), names(df))) {
        y <- df[[col]]
        if (is.factor(y) || !is.numeric(vctrs::vec_data(y))) next
        value <- switch(col,
          CHG = chg,
          PCHG = ifelse(nonzero, round(100 * chg / new_base, 10), NA_real_),
          R2BASE = ifelse(nonzero, round(new_result / new_base, 10), NA_real_)
        )
        old <- vctrs::vec_data(y)[rows]
        value[is.na(old)] <- NA
        new <- replace_rows(y, rows, if (is.integer(old)) as.integer(round(value)) else value)
        n <- sum(changed_values(old, vctrs::vec_data(new)[rows]))
        df <- set_generalized_column(df, col, new, n > 0)
        log <- c(log, list(list(variable = where(col), detail = "recomputed from the generalized AVAL and BASE", n_changed = n)))
      }
    }
  } else if (grepl("STRESN$", tg$result_col)) {
    prefix <- sub("STRESN$", "", tg$result_col)
    add(text_sibling(df, paste0(prefix, "STRESC"), new_result, rep(1, length(rows)), TRUE))
    # --ORRES in the same units follows --STRESN; in other units, it follows
    # it by each row's own factor (original --ORRES / --STRESN).
    orres <- paste0(prefix, "ORRES")
    if (orres %in% names(df)) {
      same <- same_units(df, paste0(prefix, c("ORRESU", "STRESU")), rows)
      own <- parse_number_text(as.character(text_values(df[[orres]]))[rows])$value
      ratio <- ifelse(same, 1, ifelse(!is.na(old_values) & old_values != 0, own / old_values, NA_real_))
      add(text_sibling(df, orres, new_result, ratio, same))
    }
  }
  if (length(unparsed) > 0) {
    warning(
      "Results in `", dataset_label(tg$dataset), "` that aren't numbers were left unchanged in rows of a generalized ",
      "test: ", paste(unparsed, collapse = ", "), ". Check that they don't hold a subject's real value.",
      call. = FALSE
    )
  }
  list(df = df, log = log)
}

# The result columns of the same test as `col` found in the data frame: for
# SDTM, --ORRES, --STRESC and --STRESN with the same prefix; for ADaM AVAL
# or AVALC, AVAL, AVALC, BASE, BASEC, CHG, PCHG and R2BASE.
result_siblings <- function(df, col) {
  if (col %in% c("AVAL", "AVALC")) {
    return(intersect(c("AVAL", "AVALC", "BASE", "BASEC", "CHG", "PCHG", "R2BASE"), names(df)))
  }
  if (!grepl("(ORRES|STRESC|STRESN)$", col)) {
    return(col)
  }
  prefix <- sub("(ORRES|STRESC|STRESN)$", "", col)
  intersect(paste0(prefix, c("ORRES", "STRESC", "STRESN")), names(df))
}

text_values <- function(y) if (inherits(y, "haven_labelled")) vctrs::vec_data(y) else as.character(y)

# Rewrite the numbers in a text result column `y` at `rows` from the
# generalized numeric result `base` times `ratio` (1 for the same units),
# keeping each value's form and decimals. Where `base` is missing, a value
# in the same units (`same`) is generalized on its own, and one in other
# units is blanked (there is no factor to follow). Text that isn't a number
# is left as it is, and counted.
rewrite_number_text <- function(y, rows, base, ratio, same, kind, params, col) {
  text <- as.character(text_values(y))[rows]
  parsed <- parse_number_text(text)
  avail <- parsed$ok & !is.na(base) & !is.na(ratio)
  own <- parsed$ok & !avail & same
  blank <- parsed$ok & !avail & !same
  value <- rep(NA_real_, length(rows))
  value[avail] <- base[avail] * ratio[avail]
  value[own] <- generalize_values(kind, params, parsed$value[own], what = col)$values
  new_text <- text
  exact <- (avail & ratio == 1) | own
  scaled <- avail & ratio != 1
  new_text[exact] <- format_number(value[exact], parsed$decimals[exact], parsed$style[exact], extend = TRUE)
  new_text[scaled] <- format_number(value[scaled], parsed$decimals[scaled], parsed$style[scaled], extend = FALSE)
  new_text[blank] <- blank_text_value(as.character(text_values(y)))
  changed <- (is.na(new_text) != is.na(text)) | (!is.na(new_text) & !is.na(text) & new_text != text)
  out <- replace_at(y, rows[changed], new_text[changed])
  list(
    values = out, n = sum(changed), n_from = sum(changed & avail), n_ratio = sum(changed & scaled),
    n_own = sum(changed & own), n_blanked = sum(changed & blank),
    n_unparsed = sum(!parsed$ok & !is.na(text) & nzchar(trimws(text))),
    n_unparsed_with_result = sum(!parsed$ok & !is.na(text) & nzchar(trimws(text)) & !is.na(base))
  )
}

# Whether --ORRESU equals --STRESU at `rows` (missing and blank count as
# equal); FALSE where either column is absent.
same_units <- function(df, units, rows) {
  if (!all(units %in% names(df))) {
    return(rep(FALSE, length(rows)))
  }
  a <- blank_to_na(as.character(df[[units[1]]])[rows])
  b <- blank_to_na(as.character(df[[units[2]]])[rows])
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

# Text that is a number: plain ("58", "-1.50", ".5"), with thousands
# separators ("1,234.5") or in scientific notation ("2.5E2"). Its value,
# number of decimals (of the mantissa for scientific notation) and form.
parse_number_text <- function(x) {
  t <- trimws(x)
  plain <- !is.na(t) & grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)$", t)
  comma <- !is.na(t) & grepl("^[-+]?[0-9]{1,3}(,[0-9]{3})+([.][0-9]*)?$", t)
  sci <- !is.na(t) & grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)[eE][-+]?[0-9]+$", t)
  ok <- plain | comma | sci
  value <- rep(NA_real_, length(x))
  value[ok] <- as.numeric(gsub(",", "", t[ok], fixed = TRUE))
  mantissa <- sub("[eE].*$", "", t)
  decimals <- ifelse(ok & grepl(".", mantissa, fixed = TRUE), nchar(sub("^[^.]*[.]", "", mantissa)), 0L)
  style <- ifelse(sci, ifelse(grepl("e", t, fixed = TRUE), "e", "E"), ifelse(comma, ",", ""))
  list(ok = ok, value = value, decimals = as.integer(decimals), style = style)
}

# Numbers written in a form (see parse_number_text()) with `decimals`
# decimals; with `extend`, more where the number needs them to be written
# exactly (a midpoint of 62.5 for "64").
format_number <- function(v, decimals, style, extend = TRUE) {
  out <- character(length(v))
  for (i in seq_along(v)) {
    x <- v[i]
    if (style[i] %in% c("e", "E")) {
      e <- if (x == 0) 0 else floor(log10(abs(x)))
      m <- round(x / 10^e, decimals[i])
      if (abs(m) >= 10) {
        e <- e + 1
        m <- round(x / 10^e, decimals[i])
      }
      out[i] <- paste0(sprintf("%.*f", decimals[i], m + 0), style[i], e)
      next
    }
    d <- if (extend) max(decimals[i], number_decimals(x)) else decimals[i]
    x <- round(x, d) + 0
    out[i] <- if (style[i] == ",") formatC(x, format = "f", digits = d, big.mark = ",") else sprintf("%.*f", d, x)
  }
  out
}

# The fewest decimals (up to 15) that write `x` exactly.
number_decimals <- function(x) {
  for (d in 0:15) {
    if (isTRUE(abs(round(x, d) - x) <= 1e-9 * max(1, abs(x)))) {
      return(d)
    }
  }
  15L
}

# Which elements differ between two vectors of the same kind.
changed_values <- function(old, new) {
  a <- if (is.factor(old)) as.character(old) else vctrs::vec_data(old)
  b <- if (is.factor(new)) as.character(new) else vctrs::vec_data(new)
  (is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
}

# `x` with its values at `rows` replaced by `values` (generalized values of
# the same kind as `x`), keeping its type and attributes.
replace_rows <- function(x, rows, values) {
  if (is.factor(x)) {
    return(replace_at(x, rows, as.character(values)))
  }
  data <- vctrs::vec_data(x)
  data[rows] <- vctrs::vec_data(values)
  attrs <- attributes(x)
  if (inherits(x, "haven_labelled")) {
    attrs["labels"] <- list(attr(values, "labels", exact = TRUE))
  }
  attributes(data) <- attrs
  data
}

# `x` with the values at `at` set to `new` (text, a number or NA), keeping its
# type and attributes. A factor gets the new values as levels (added at the
# end) and loses the levels that only the replaced values held.
replace_at <- function(x, at, new) {
  if (length(at) == 0) {
    return(x)
  }
  if (is.factor(x)) {
    old <- as.character(x)
    values <- old
    values[at] <- new
    used_before <- unique(old[!is.na(old)])
    gone <- setdiff(used_before, values)
    added <- setdiff(unique(values[!is.na(values)]), levels(x))
    out <- factor(values, levels = c(setdiff(levels(x), gone), added))
    if (length(gone) == 0 && length(added) == 0) {
      attributes(out) <- attributes(x)
      return(out)
    }
    return(with_attributes_of(out, x))
  }
  data <- vctrs::vec_data(x)
  data[at] <- new
  attributes(data) <- attributes(x)
  data
}

# Values of a vector as category ids (text; numbers written exactly), and
# which are present (not missing, blank or user-defined missing). A factor's
# levels are categories too, even unused.
fold_categories <- function(x) {
  data <- if (is.factor(x)) as.character(x) else vctrs::vec_data(x)
  id <- if (is.numeric(data)) key_as_character(data) else as.character(data)
  present <- !is.na(id) & !grepl("^[[:space:]]*$", id) & !user_missing(x, data)
  list(id = id, present = present, levels = if (is.factor(x)) levels(x) else character(0))
}

rule_category_id <- function(to) if (is.numeric(to)) key_as_character(to) else as.character(to)

# haven user-defined missing values (na_values, na_range).
user_missing <- function(x, data) {
  out <- rep(FALSE, length(data))
  na_values <- attr(x, "na_values", exact = TRUE)
  if (!is.null(na_values)) {
    out <- out | (!is.na(data) & data %in% na_values)
  }
  na_range <- attr(x, "na_range", exact = TRUE)
  if (length(na_range) == 2) {
    out <- out | (!is.na(data) & data >= na_range[1] & data <= na_range[2])
  }
  out
}

# Apply a rule to a vector: the new values and which elements changed. For
# folds, `fold_set` gives the categories to fold (counted elsewhere), or they
# are counted here from `subjects` (each element its own subject by default).
generalize_values <- function(kind, params, x, subjects = NULL, fold_set = NULL, what = "x") {
  if (kind == "fold") {
    return(fold_values(params, x, subjects, fold_set, what))
  }
  if (is.factor(x) || !(is.null(oldClass(x)) || inherits(x, "haven_labelled")) ||
    !is.numeric(vctrs::vec_data(x))) {
    stop("`", what, "` must be numeric for `anon_", kind, "()`.", call. = FALSE)
  }
  data <- vctrs::vec_data(x)
  labels <- attr(x, "labels", exact = TRUE)
  # Values carrying a value label (such as a code for "not done") are codes,
  # not measurements: they are left as they are, as are missing values.
  skip <- is.na(data) | user_missing(x, data) | (!is.null(labels) & data %in% labels)
  new <- as.double(data)
  new[!skip] <- numeric_rule(kind, params, new[!skip])
  if (is.integer(data)) {
    # round() with negative digits can land a hair off a whole number.
    near <- !skip & abs(new - round(new)) < 1e-6
    new[near] <- round(new[near])
    if (any(is.finite(new[!skip]) & abs(new[!skip]) > .Machine$integer.max)) {
      stop(
        "`anon_", kind, "()` would give `", what, "`, an integer column, numbers too large for an integer; ",
        "convert the column to double first.",
        call. = FALSE
      )
    }
    if (any(new[!skip] != round(new[!skip]))) {
      stop(
        "`anon_", kind, "()` would give `", what, "`, an integer column, numbers that aren't whole; ",
        "use value = \"lower\", or convert the column to double first.",
        call. = FALSE
      )
    }
    new <- as.integer(new)
  }
  changed <- !skip & new != data
  out <- new
  attributes(out) <- attributes(x)
  # A value label stays only if no value moved into or out of its code.
  if (!is.null(labels)) {
    keep <- vapply(seq_along(labels), function(i) {
      identical(which(data == labels[[i]]), which(new == labels[[i]]))
    }, logical(1))
    attr(out, "labels") <- labels[keep]
  }
  list(values = out, changed = changed)
}

numeric_rule <- function(kind, params, v) {
  if (kind == "round") {
    return(round(v, params$digits))
  }
  w <- params$width
  # Bands start at `bottom` when given, otherwise at 0. Rounding the quotient
  # first keeps 0.3 / 0.1 in the band of 0.3.
  start <- if (is.null(params$bottom)) 0 else params$bottom
  out <- start + floor(round((v - start) / w, 9)) * w
  if (params$value == "midpoint") {
    out <- out + w / 2
  }
  out <- round(out, max(number_decimals(start), number_decimals(if (params$value == "midpoint") w / 2 else w)))
  # Representatives stay within [bottom, top].
  if (!is.null(params$top)) {
    out <- pmin(out, params$top)
    out[v >= params$top] <- params$top
  }
  if (!is.null(params$bottom)) {
    out <- pmax(out, params$bottom)
    out[v <= params$bottom] <- params$bottom
  }
  out
}

fold_values <- function(params, x, subjects, fold_set, what) {
  data <- if (is.factor(x)) as.character(x) else vctrs::vec_data(x)
  if ((!is.factor(x) && !(is.null(oldClass(x)) || inherits(x, "haven_labelled"))) ||
    !(is.character(data) || is.numeric(data))) {
    stop("`", what, "` must be character, factor or numeric for `anon_fold()`.", call. = FALSE)
  }
  to <- params$to
  if (is.numeric(data)) {
    if (!is.numeric(to)) {
      stop("`to` must be a number for `", what, "`, which is numeric.", call. = FALSE)
    }
    if (is.integer(data)) {
      if (to != round(to)) stop("`to` must be a whole number for `", what, "`, an integer column.", call. = FALSE)
      to <- as.integer(to)
    }
  } else {
    to <- rule_category_id(to)
  }
  cats <- fold_categories(x)
  if (is.null(fold_set)) {
    if (is.null(subjects)) {
      subjects <- seq_along(data)
    }
    if (length(subjects) != length(data)) {
      stop("`subjects` must give the subject of each element of `x`.", call. = FALSE)
    }
    held <- cats$id[cats$present]
    counts <- tapply(subjects[cats$present], held, function(s) length(unique(s[!is.na(s)])))
    unused <- setdiff(cats$levels, held)
    counts <- c(counts, stats::setNames(rep(0L, length(unused)), unused))
    fold_set <- setdiff(names(counts)[counts < params$min_n], rule_category_id(params$to))
  }
  hit <- cats$present & cats$id %in% fold_set
  if (is.factor(x)) {
    gone <- intersect(levels(x), fold_set)
    if (!any(hit) && length(gone) == 0) {
      return(list(values = x, changed = hit))
    }
    values <- data
    values[hit] <- to
    lev <- setdiff(levels(x), fold_set)
    if (any(hit) && !to %in% lev) {
      lev <- c(lev, to)
    }
    return(list(values = with_attributes_of(factor(values, levels = lev), x), changed = hit))
  }
  new <- data
  new[hit] <- to
  attributes(new) <- attributes(x)
  labels <- attr(x, "labels", exact = TRUE)
  if (!is.null(labels)) {
    attr(new, "labels") <- labels[!rule_category_id(vctrs::vec_data(labels)) %in% fold_set]
  }
  list(values = new, changed = hit)
}

# Warn when a generalized column also appears, under the same name, in
# datasets with the key where no rule generalizes it.
warn_uncovered_copies <- function(data, targets, key) {
  columns <- Filter(function(tg) tg$type == "column", targets)
  for (col in unique(vapply(columns, `[[`, character(1), "column"))) {
    covered <- vapply(Filter(function(tg) tg$column == col, columns), `[[`, character(1), "dataset")
    others <- setdiff(datasets_with_column(data, col, key), covered)
    if (length(others) > 0) {
      warning(
        "`", col, "` is generalized in ", paste0("`", covered, "`", collapse = ", "),
        " but not in these datasets, which hold it too: ", paste(others, collapse = ", "),
        ". List each dataset in `rules`.",
        call. = FALSE
      )
    }
  }
}
