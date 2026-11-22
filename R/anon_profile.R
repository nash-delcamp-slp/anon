#' De-identification profiles
#'
#' A profile tells [anon_deidentify()] what each variable is: a direct
#' identifier to remove, the subject key and related IDs, a date to offset, a
#' quasi-identifier with its generalization ladder, a variable derived from a
#' quasi-identifier, or a variable to keep. `anon_profile_sdtm()` and
#' `anon_profile_adam()` describe standard CDISC SDTM and ADaM variables;
#' `anon_profile()` and `anon_profile_rule()` build a profile for other data,
#' or rules to add to a standard one with [anon_profile_combine()].
#'
#' @param ... For `anon_profile()`: rules made by `anon_profile_rule()`,
#'   profiles, or data frames with the columns of a profile (as
#'   `as.data.frame()` returns them), in order.
#' @param name A name for the profile, shown when it is printed and in
#'   warnings when profiles are combined.
#' @param variable Names of the variables the rule applies to: an exact name
#'   (`"AGE"`), an SDTM domain wildcard where `--` stands for the two-letter
#'   domain prefix (`"--DTC"` matches `VSDTC` and `LBDTC`, `"--STDTC"`
#'   matches `AESTDTC` and `RFSTDTC`), or a pattern with `*` (any characters)
#'   and `?` (one character), such as `"*DT"`. Several names make one rule
#'   each, sharing the other arguments. For a test (`test_col` given), the
#'   result column (`"VSSTRESN"`, `"AVAL"`).
#' @param role What the variable is:
#'   - `"remove"`: a direct identifier, blanked in every row (the column is
#'     kept, with its type and attributes, so code that uses it still runs);
#'   - `"key"`, `"also"`, `"key_cols"`: the subject key and related identifier
#'     columns, passed to [anon_subject_ids()] as `key`, `also` and
#'     `key_cols`;
#'   - `"date"`: a date offset per subject by [anon_subject_dates()];
#'   - `"quasi"`: a quasi-identifier, measured, generalized up its `ladder`
#'     and suppressed;
#'   - `"derived"`: a variable derived from the quasi-identifiers in `from`,
#'     kept where it still follows from their released values and blanked
#'     elsewhere (see [anon_deidentify()]);
#'   - `"duration"`: a duration from the date in `from` to the date in
#'     `variable`, measured among the residual signals (see
#'     [anon_quasi_duration()]) and not changed;
#'   - `"keep"`: kept as it is.
#' @param datasets Names of the datasets the rule is limited to, exact or
#'   patterns with `*` and `?` (such as `"ad*"`). Default: every dataset.
#' @param quasi_name For `"quasi"` rules, the name of the quasi-identifier.
#'   Rules with the same name are copies of one quasi-identifier: such as
#'   baseline weight in SDTM `vs`, in ADaM `WEIGHTBL` and in `advs`. A
#'   quasi-identifier is measured from its first copy found in the data (in
#'   the order of the rules, then of the datasets), generalized at the same
#'   level in every copy, and blanked for the same subjects in every copy.
#'   Default: `test` for a test, otherwise `variable`.
#' @param test_col,test For a quasi-identifier or derived variable held in
#'   the rows of a test, such as baseline weight in `vs`: the column holding
#'   test codes (`"VSTESTCD"`, `"PARAMCD"`) and the test code (`"WEIGHT"`).
#'   `variable` is then the result column.
#' @param flag For a test quasi-identifier, the column flagging the baseline
#'   record (`"VSBLFL"`, `"ABLFL"`; `"Y"` marks it). The measured value is
#'   the subject's first flagged result, or, for a subject without a flagged
#'   result (or a dataset without the column), its first result. Every row of
#'   the test is generalized and blanked, not only the measured one.
#' @param ladder For a quasi-identifier, its generalization ladder: a list of
#'   rules ([anon_band()], [anon_round()], [anon_fold()]), from the finest to
#'   the coarsest, each applied to the original values; the string
#'   `"remove"`, as the last step, blanks the quasi-identifier in every row.
#'   An empty list: the quasi-identifier can only be suppressed. For a
#'   duration, one [anon_band()] giving the width of its bands in days.
#' @param from For a derived variable, the names of the quasi-identifiers it
#'   is derived from; for a duration, the start date column.
#' @param priority For a quasi-identifier, its usefulness when cells are
#'   suppressed: lower numbers are blanked first (passed to [anon_suppress()]
#'   as `order`). Quasi-identifiers without one follow, in profile order.
#' @param min_n For the standard profiles, the smallest number of subjects a
#'   category must hold to be kept by their [anon_fold()] steps (`RACE`,
#'   `SITEID`, `COUNTRY`). Default 12, the group size the default target of
#'   [anon_deidentify()] (`max_risk = 0.09`) requires; set it to the group
#'   size of your target.
#' @param x A profile.
#' @param row.names,optional Not used.
#'
#' @details
#' **Structure.** A profile is a tibble of rules with class
#' `"anon_profile"` and columns `variable`, `datasets` (a list of character
#' vectors, empty for every dataset), `role`, `quasi_name`, `test_col`,
#' `test`, `flag`, `ladder` (a list of lists of rules), `from` (a list of
#' character vectors), `priority` and `profile` (the name of the profile the
#' rule came from). `as.data.frame()` returns it as a data frame to edit, and
#' `anon_profile()` turns an edited data frame back into a profile.
#'
#' **Which rule applies.** For a column of a dataset, among the rules (other
#' than test rules) whose `variable` and `datasets` match it, a rule limited
#' to datasets wins over one that isn't, an exact name over a pattern, and a
#' later rule over an earlier one. Columns matched by no rule are kept as
#' they are, except that dates are offset and subject IDs replaced wherever
#' [anon_subject_dates()] and [anon_subject_ids()] find them. Test rules
#' apply to the rows of their test in every dataset (in `datasets`) with the
#' test and result columns.
#'
#' **Dates.** [anon_deidentify()] offsets dates with
#' [anon_subject_dates()]'s own detection: every Date and date-time column
#' and every text column whose name ends in `DTC`, at any depth. The `"date"`
#' rules name them; a column matched by a `"date"` rule that isn't such a
#' column (a numeric SAS date, say) is named in a warning.
#'
#' **`anon_profile_sdtm()`.**
#' - remove: `BRTHDTC` (with `AGE` and an offset `RFSTDTC`, a birth date
#'   gives the age back exactly, even after offsetting, and a birth year
#'   undoes `AGE` bands, so it is removed rather than reduced to the year),
#'   `INVNAM`, `INVID`;
#' - key `USUBJID`, also `SUBJID`, key_cols `RSUBJID`;
#' - dates: `--DTC`, `--STDTC`, `--ENDTC` and every other `*DTC`;
#' - quasi-identifiers, each with its ladder: `AGE` (5-, 10- and 20-year
#'   bands, ages of 90 and over as 90), `SEX` (suppressed only), `RACE`
#'   (categories of fewer than `min_n` subjects folded into `"OTHER"`, a
#'   CDISC controlled term), `ETHNIC` (suppressed only: its controlled
#'   terminology has no neutral other value), `SITEID` (small sites merged
#'   into `"OTHER"`, then removed), `COUNTRY` (small countries folded into
#'   `"OTHER"`, then removed), and baseline `HEIGHT` and `WEIGHT` from `vs`
#'   (`VSSTRESN` with `VSORRES` and `VSSTRESC`; flag `VSBLFL`, else the first
#'   record: height is often recorded once, at screening, without the flag),
#'   rounded to units, then to tens, then in bands of 20;
#' - derived: `BMI` and `BSA` results in `vs`, from `HEIGHT` and `WEIGHT`
#'   (blanked where those changed);
#' - duration: `RFSTDTC` to `RFENDTC`, in bands of 7 days;
#' - keep: `AGEU` (the unit of `AGE`, not identifying), `--DY`, `--STDY` and
#'   `--ENDY` (study days, which offsetting keeps exact).
#'
#' Suppression priority, from the first blanked: `SITEID`, `COUNTRY`,
#' `ETHNIC`, `RACE`, `HEIGHT`, `WEIGHT`, `AGE`, `SEX`.
#'
#' **`anon_profile_adam()`.** Rules match ADSL variables in every dataset
#' that carries them (ADSL, BDS and OCCDS datasets alike).
#' - remove: `BRTHDT`, `BRTHDTM`, `BRTHDTC`, `INVNAM`, `INVID`;
#' - key, also and key_cols as for SDTM;
#' - dates: `*DT`, `*DTM` and `*DTC` (such as `TRTSDT`, `TRTEDT`, `RANDDT`,
#'   `ADT`, `ADTM`);
#' - quasi-identifiers: `AGE`, `SEX`, `RACE`, `ETHNIC`, `SITEID` and
#'   `COUNTRY` as for SDTM; `HEIGHT` and `WEIGHT` as copies in
#'   `HEIGHTBL`/`HTBL` and `WEIGHTBL`/`WTBL`, in the `AVAL` of their `PARAMCD`
#'   rows (flag `ABLFL`; `AVALC`, `BASE`, `BASEC`, `CHG`, `PCHG` and `R2BASE`
#'   follow, see [anon_rule()]) and in the SDTM results (`VSSTRESN`) that BDS
#'   datasets carry, with the SDTM ladders;
#' - derived: `RACEN`, `RACEGR*` from `RACE`; `SEXN` from `SEX`; `ETHNICN`
#'   from `ETHNIC`; `AGEGR*` (such as `AGEGR1` and `AGEGR1N`) from `AGE`;
#'   `REGION*` from `COUNTRY`; `BMIBL` from `HEIGHT` and `WEIGHT`; `AVALCAT*`
#'   and `AVALCA*N` of the `HEIGHT` and `WEIGHT` rows from them; and the
#'   `BMI` and `BSA` parameters, from `HEIGHT` and `WEIGHT`;
#' - duration: `TRTSDT` to `TRTEDT`, in bands of 7 days.
#'
#' In BDS datasets, results (`AVAL`, `AVALC`, `BASE`, `BASEC`, `CHG`, `PCHG`,
#' `R2BASE`) are not quasi-identifiers, except for the baseline height and
#' weight parameters: a lab value or a blood pressure is not something an
#' attacker would know. Add rules for other parameters that are.
#'
#' **Combining.** [anon_profile_combine()] (or `c()`) joins profiles, such as
#' the SDTM and ADaM profiles for a list holding both. A rule of a later
#' profile replaces a rule of an earlier one with the same `variable`,
#' `datasets`, `test_col` and `test` (in its place); a replacement with
#' another role is named in a warning. Quasi-identifiers with the same name
#' are one quasi-identifier: one set of generalizations, applied to every
#' copy.
#'
#' @return An object of class `"anon_profile"`; for `anon_profile_rule()`, a
#'   profile holding the rules.
#'
#' @seealso [anon_deidentify()]
#'
#' @examples
#' anon_profile_sdtm()
#'
#' # A profile for data that isn't CDISC: a key, a date and two
#' # quasi-identifiers.
#' anon_profile(
#'   anon_profile_rule("ID", "key"),
#'   anon_profile_rule("VISIT_DATE", "date"),
#'   anon_profile_rule("AGE", "quasi", ladder = list(anon_band(10, top = 90))),
#'   anon_profile_rule("SEX", "quasi"),
#'   name = "registry"
#' )
#'
#' # The SDTM and ADaM profiles together, for a list holding both.
#' c(anon_profile_sdtm(), anon_profile_adam())
#'
#' @name anon_profile
NULL

profile_roles <- c("remove", "key", "also", "key_cols", "date", "quasi", "derived", "duration", "keep")

#' @rdname anon_profile
#' @export
anon_profile <- function(..., name = "custom") {
  parts <- list(...)
  if (length(name) != 1 || !is.character(name) || is.na(name)) {
    stop("`name` must be a single string.", call. = FALSE)
  }
  rows <- lapply(parts, function(p) {
    if (!is.data.frame(p)) {
      stop("Each argument must be a rule made by `anon_profile_rule()`, a profile or a data frame of rules.", call. = FALSE)
    }
    profile_table(p)
  })
  table <- if (length(rows) == 0) profile_table(NULL) else do.call(rbind, rows)
  ids <- profile_rule_ids(table)
  if (anyDuplicated(ids)) {
    dup <- which(duplicated(ids))[1]
    stop(
      "More than one rule for `", table$variable[dup], "`", profile_scope_text(table$datasets[[dup]]),
      if (!is.na(table$test[dup])) paste0(" (", table$test_col[dup], " = ", table$test[dup], ")"),
      ". Combine profiles with `anon_profile_combine()` to replace a rule.",
      call. = FALSE
    )
  }
  new_profile(table, name)
}

#' @rdname anon_profile
#' @export
anon_profile_rule <- function(
  variable,
  role,
  datasets = character(0),
  quasi_name = NULL,
  test_col = NULL,
  test = NULL,
  flag = NULL,
  ladder = list(),
  from = character(0),
  priority = NA_integer_
) {
  if (!is.character(variable) || length(variable) == 0 || anyNA(variable) || any(!nzchar(variable))) {
    stop("`variable` must be a character vector of variable names or patterns.", call. = FALSE)
  }
  if (!is.character(role) || length(role) != 1 || !role %in% profile_roles) {
    stop("`role` must be one of ", paste0("\"", profile_roles, "\"", collapse = ", "), ".", call. = FALSE)
  }
  if (!is.character(datasets) || anyNA(datasets)) {
    stop("`datasets` must be a character vector of dataset names or patterns.", call. = FALSE)
  }
  single <- function(x, arg) {
    if (is.null(x)) {
      return(NA_character_)
    }
    if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
      stop("`", arg, "` must be a single string.", call. = FALSE)
    }
    x
  }
  test_col <- single(test_col, "test_col")
  test <- single(test, "test")
  flag <- single(flag, "flag")
  if (is.na(test_col) != is.na(test)) {
    stop("Give both `test_col` and `test`, or neither.", call. = FALSE)
  }
  if (!is.na(test) && !role %in% c("quasi", "derived")) {
    stop("Only quasi-identifiers and derived variables can be tests.", call. = FALSE)
  }
  if (!is.na(flag) && (is.na(test) || role != "quasi")) {
    stop("`flag` is for a quasi-identifier held in a test.", call. = FALSE)
  }
  if (!is.null(quasi_name)) {
    quasi_name <- single(quasi_name, "quasi_name")
    if (role != "quasi") stop("`quasi_name` is for quasi-identifiers.", call. = FALSE)
  }
  check_profile_ladder(ladder, role)
  if (!is.character(from) || anyNA(from)) {
    stop("`from` must be a character vector.", call. = FALSE)
  }
  if (role == "derived" && length(from) == 0) {
    stop("A derived variable needs `from`: the quasi-identifiers it is derived from.", call. = FALSE)
  }
  if (role == "duration" && (length(from) != 1 || length(ladder) != 1 || !inherits(ladder[[1]], "anon_band"))) {
    stop("A duration needs `from` (its start date column) and a `ladder` of one `anon_band()` (its bands of days).", call. = FALSE)
  }
  if (!role %in% c("derived", "duration") && length(from) > 0) {
    stop("`from` is for derived variables and durations.", call. = FALSE)
  }
  if (length(priority) != 1 || (!is.na(priority) && (!is.numeric(priority) || priority != round(priority)))) {
    stop("`priority` must be a single whole number.", call. = FALSE)
  }
  if (!is.na(priority) && role != "quasi") {
    stop("`priority` is for quasi-identifiers.", call. = FALSE)
  }
  n <- length(variable)
  name <- if (role == "quasi") {
    if (!is.null(quasi_name)) quasi_name else if (!is.na(test)) test else variable
  } else {
    NA_character_
  }
  table <- tibble::tibble(
    variable = variable,
    datasets = rep(list(datasets), n),
    role = role,
    quasi_name = rep(name, length.out = n),
    test_col = test_col,
    test = test,
    flag = flag,
    ladder = rep(list(ladder), n),
    from = rep(list(from), n),
    priority = as.integer(priority),
    profile = NA_character_
  )
  new_profile(table, "rules")
}

#' @rdname anon_profile
#' @export
anon_profile_sdtm <- function(min_n = 12) {
  check_profile_min_n(min_n)
  anon_profile(
    profile_common_rules(min_n, dates = c("--DTC", "--STDTC", "--ENDTC", "*DTC")),
    anon_profile_rule("BRTHDTC", "remove"),
    sdtm_test_quasi("VSTESTCD", "VSSTRESN", "VSBLFL"),
    anon_profile_rule("VSSTRESN", "derived", test_col = "VSTESTCD", test = "BMI", from = c("HEIGHT", "WEIGHT")),
    anon_profile_rule("VSSTRESN", "derived", test_col = "VSTESTCD", test = "BSA", from = c("HEIGHT", "WEIGHT")),
    anon_profile_rule("RFENDTC", "duration", from = "RFSTDTC", ladder = list(anon_band(7))),
    anon_profile_rule(c("AGEU", "--DY", "--STDY", "--ENDY"), "keep"),
    name = "sdtm"
  )
}

#' @rdname anon_profile
#' @export
anon_profile_adam <- function(min_n = 12) {
  check_profile_min_n(min_n)
  ladder <- measure_ladder()
  anon_profile(
    profile_common_rules(min_n, dates = c("*DT", "*DTM", "*DTC")),
    anon_profile_rule(c("BRTHDT", "BRTHDTM", "BRTHDTC"), "remove"),
    anon_profile_rule(c("HEIGHTBL", "HTBL"), "quasi", quasi_name = "HEIGHT", ladder = ladder, priority = 5),
    anon_profile_rule(c("WEIGHTBL", "WTBL"), "quasi", quasi_name = "WEIGHT", ladder = ladder, priority = 6),
    anon_profile_rule("AVAL", "quasi", test_col = "PARAMCD", test = "HEIGHT", flag = "ABLFL", ladder = ladder, priority = 5),
    anon_profile_rule("AVAL", "quasi", test_col = "PARAMCD", test = "WEIGHT", flag = "ABLFL", ladder = ladder, priority = 6),
    sdtm_test_quasi("VSTESTCD", "VSSTRESN", "VSBLFL"),
    anon_profile_rule(c("RACEN", "RACEGR*"), "derived", from = "RACE"),
    anon_profile_rule("SEXN", "derived", from = "SEX"),
    anon_profile_rule("ETHNICN", "derived", from = "ETHNIC"),
    anon_profile_rule("AGEGR*", "derived", from = "AGE"),
    anon_profile_rule("REGION*", "derived", from = "COUNTRY"),
    anon_profile_rule("BMIBL", "derived", from = c("HEIGHT", "WEIGHT")),
    anon_profile_rule(c("AVALCAT*", "AVALCA*N"), "derived", test_col = "PARAMCD", test = "HEIGHT", from = "HEIGHT"),
    anon_profile_rule(c("AVALCAT*", "AVALCA*N"), "derived", test_col = "PARAMCD", test = "WEIGHT", from = "WEIGHT"),
    anon_profile_rule("AVAL", "derived", test_col = "PARAMCD", test = "BMI", from = c("HEIGHT", "WEIGHT")),
    anon_profile_rule("AVAL", "derived", test_col = "PARAMCD", test = "BSA", from = c("HEIGHT", "WEIGHT")),
    anon_profile_rule("VSSTRESN", "derived", test_col = "VSTESTCD", test = "BMI", from = c("HEIGHT", "WEIGHT")),
    anon_profile_rule("VSSTRESN", "derived", test_col = "VSTESTCD", test = "BSA", from = c("HEIGHT", "WEIGHT")),
    anon_profile_rule("TRTEDT", "duration", from = "TRTSDT", ladder = list(anon_band(7))),
    anon_profile_rule("AGEU", "keep"),
    name = "adam"
  )
}

#' @rdname anon_profile
#' @export
anon_profile_combine <- function(...) {
  profiles <- list(...)
  if (length(profiles) == 0 || !all(vapply(profiles, inherits, logical(1), "anon_profile"))) {
    stop("Give one or more profiles, such as `anon_profile_sdtm()`.", call. = FALSE)
  }
  table <- profiles[[1]]
  names_all <- attr(table, "profile_name")
  for (p in profiles[-1]) {
    ids <- profile_rule_ids(table)
    new_ids <- profile_rule_ids(p)
    conflicts <- character(0)
    for (i in seq_len(nrow(p))) {
      at <- match(new_ids[i], ids)
      if (is.na(at)) {
        table <- rbind(table, p[i, ])
        ids <- c(ids, new_ids[i])
        next
      }
      if (table$role[at] != p$role[i]) {
        conflicts <- c(conflicts, paste0(
          "`", p$variable[i], "`", profile_scope_text(p$datasets[[i]]),
          if (!is.na(p$test[i])) paste0(" (", p$test_col[i], " = ", p$test[i], ")"),
          ": ", table$role[at], " (", table$profile[at], ") replaced by ", p$role[i], " (", p$profile[i], ")"
        ))
      }
      table[at, ] <- p[i, ]
    }
    if (length(conflicts) > 0) {
      warning("Rules with another role replaced earlier ones: ", paste(conflicts, collapse = "; "), ".", call. = FALSE)
    }
    names_all <- c(names_all, attr(p, "profile_name"))
  }
  new_profile(table, paste(names_all, collapse = " + "))
}

#' @rdname anon_profile
#' @export
c.anon_profile <- function(...) {
  anon_profile_combine(...)
}

#' @rdname anon_profile
#' @export
as.data.frame.anon_profile <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- tibble::as_tibble(unclass_profile(x))
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' @export
print.anon_profile <- function(x, ...) {
  cat("<anon_profile> ", attr(x, "profile_name"), ": ", nrow(x), " rule(s)\n", sep = "")
  text <- profile_text(x)
  for (role in intersect(profile_roles, text$role)) {
    rows <- text[text$role == role, , drop = FALSE]
    cat("\n", role, ":\n", sep = "")
    for (i in seq_len(nrow(rows))) {
      cat("  ", rows$rule[i], "\n", sep = "")
    }
  }
  invisible(x)
}

# helpers -----------------------------------------------------------------

check_profile_min_n <- function(min_n) {
  check_rule_number(min_n, "min_n")
  if (min_n != round(min_n) || min_n < 1) {
    stop("`min_n` must be a whole number of at least 1.", call. = FALSE)
  }
}

# Rules shared by the SDTM and ADaM profiles: direct identifiers, subject
# IDs, dates (with the given patterns) and the demographic
# quasi-identifiers.
profile_common_rules <- function(min_n, dates) {
  fold <- anon_fold(min_n, "OTHER")
  anon_profile(
    anon_profile_rule(c("INVNAM", "INVID"), "remove"),
    anon_profile_rule("USUBJID", "key"),
    anon_profile_rule("SUBJID", "also"),
    anon_profile_rule("RSUBJID", "key_cols"),
    anon_profile_rule(dates, "date"),
    anon_profile_rule(
      "AGE", "quasi",
      ladder = list(anon_band(5, top = 90), anon_band(10, top = 90), anon_band(20, top = 90)), priority = 7
    ),
    anon_profile_rule("SEX", "quasi", priority = 8),
    anon_profile_rule("RACE", "quasi", ladder = list(fold), priority = 4),
    anon_profile_rule("ETHNIC", "quasi", priority = 3),
    anon_profile_rule("SITEID", "quasi", ladder = list(fold, "remove"), priority = 1),
    anon_profile_rule("COUNTRY", "quasi", ladder = list(fold, "remove"), priority = 2),
    name = "rules"
  )
}

# Height and weight rounded to units, then to tens, then in bands of 20.
measure_ladder <- function() list(anon_round(0), anon_round(-1), anon_band(20))

# Baseline height and weight in SDTM findings results.
sdtm_test_quasi <- function(test_col, result_col, flag) {
  ladder <- measure_ladder()
  anon_profile(
    anon_profile_rule(result_col, "quasi", test_col = test_col, test = "HEIGHT", flag = flag, ladder = ladder, priority = 5),
    anon_profile_rule(result_col, "quasi", test_col = test_col, test = "WEIGHT", flag = flag, ladder = ladder, priority = 6),
    name = "rules"
  )
}

check_profile_ladder <- function(ladder, role) {
  if (!is.list(ladder) || inherits(ladder, "anon_generalize_rule")) {
    stop("`ladder` must be a list of rules.", call. = FALSE)
  }
  if (length(ladder) > 0 && !role %in% c("quasi", "duration")) {
    stop("`ladder` is for quasi-identifiers and durations.", call. = FALSE)
  }
  for (i in seq_along(ladder)) {
    step <- ladder[[i]]
    is_remove <- identical(step, "remove")
    if (!inherits(step, "anon_generalize_rule") && !is_remove) {
      stop(
        "Each step of `ladder` must be a rule made by `anon_band()`, `anon_round()` or `anon_fold()`, ",
        "or \"remove\".",
        call. = FALSE
      )
    }
    if (is_remove && i != length(ladder)) {
      stop("\"remove\" can only be the last step of `ladder`.", call. = FALSE)
    }
  }
}

# A profile table: the rules of `p` (a profile, rules or a data frame with
# the profile's columns) with their types checked.
profile_table <- function(p) {
  cols <- c("variable", "datasets", "role", "quasi_name", "test_col", "test", "flag", "ladder", "from", "priority", "profile")
  if (is.null(p)) {
    return(tibble::tibble(
      variable = character(0), datasets = list(), role = character(0), quasi_name = character(0),
      test_col = character(0), test = character(0), flag = character(0), ladder = list(), from = list(),
      priority = integer(0), profile = character(0)
    ))
  }
  name <- attr(p, "profile_name")
  p <- unclass_profile(p)
  if (!"profile" %in% names(p)) {
    p$profile <- NA_character_
  }
  missing <- setdiff(cols, names(p))
  if (length(missing) > 0) {
    stop("A profile's rules need the columns ", paste0("`", missing, "`", collapse = ", "), ".", call. = FALSE)
  }
  p <- tibble::as_tibble(p[cols])
  p$profile[is.na(p$profile)] <- if (is.null(name) || identical(name, "rules")) NA_character_ else name
  # Check each rule as anon_profile_rule() does.
  for (i in seq_len(nrow(p))) {
    anon_profile_rule(
      p$variable[i], p$role[i], datasets = p$datasets[[i]],
      quasi_name = if (p$role[i] == "quasi") p$quasi_name[i],
      test_col = if (!is.na(p$test_col[i])) p$test_col[i], test = if (!is.na(p$test[i])) p$test[i],
      flag = if (!is.na(p$flag[i])) p$flag[i], ladder = p$ladder[[i]], from = p$from[[i]], priority = p$priority[i]
    )
  }
  p$priority <- as.integer(p$priority)
  p
}

unclass_profile <- function(p) {
  attr(p, "profile_name") <- NULL
  class(p) <- setdiff(class(p), "anon_profile")
  p
}

# Rules not yet in a named profile (from anon_profile_rule(), or the parts of
# a standard profile, named "rules") keep `profile` missing until they are.
new_profile <- function(table, name) {
  if (!identical(name, "rules")) {
    table$profile[is.na(table$profile)] <- name
  }
  table <- tibble::as_tibble(unclass_profile(table))
  structure(table, profile_name = name, class = c("anon_profile", class(table)))
}

# What identifies a rule for replacement: its variable, datasets and test.
profile_rule_ids <- function(table) {
  vapply(seq_len(nrow(table)), function(i) {
    paste(table$variable[i], paste(sort(table$datasets[[i]]), collapse = ","), table$test_col[i], table$test[i], sep = "\u001f")
  }, character(1))
}

profile_scope_text <- function(datasets) {
  if (length(datasets) == 0) "" else paste0(" in ", paste(datasets, collapse = ", "))
}

# Each step of a ladder as text.
describe_ladder_step <- function(step) if (identical(step, "remove")) "remove" else describe_rule(step)

# One line of text per rule, for printing and for the settings of
# anon_deidentify().
profile_text <- function(x) {
  rule <- vapply(seq_len(nrow(x)), function(i) {
    paste0(
      x$variable[i],
      if (!is.na(x$test[i])) paste0(" where ", x$test_col[i], " = ", x$test[i]),
      profile_scope_text(x$datasets[[i]]),
      if (x$role[i] == "quasi" && !identical(x$quasi_name[i], if (is.na(x$test[i])) x$variable[i] else x$test[i])) {
        paste0(" (copy of ", x$quasi_name[i], ")")
      },
      if (!is.na(x$flag[i])) paste0(", baseline flag ", x$flag[i]),
      if (length(x$ladder[[i]]) > 0) paste0(", ladder: ", paste(vapply(x$ladder[[i]], describe_ladder_step, character(1)), collapse = " > ")),
      if (x$role[i] == "quasi" && length(x$ladder[[i]]) == 0) ", suppressed only",
      if (length(x$from[[i]]) > 0) paste0(", from ", paste(x$from[[i]], collapse = ", ")),
      if (!is.na(x$priority[i])) paste0(", priority ", x$priority[i])
    )
  }, character(1))
  tibble::tibble(role = x$role, rule = rule, profile = x$profile)
}

# Whether names match a rule's variable: an exact name, an SDTM domain
# wildcard ("--" for any two-letter prefix) or a pattern with * and ?.
profile_match <- function(pattern, names) {
  if (startsWith(pattern, "--")) {
    rest <- substring(pattern, 3)
    rx <- paste0("^[A-Za-z]{2}", gsub("\\\\\\*", ".*", gsub("\\\\\\?", ".", escape_regex(rest))), "$")
    return(grepl(rx, names, useBytes = TRUE))
  }
  if (grepl("[*?]", pattern)) {
    return(grepl(utils::glob2rx(pattern), names, useBytes = TRUE))
  }
  !is.na(names) & names == pattern
}

escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

profile_is_pattern <- function(pattern) startsWith(pattern, "--") | grepl("[*?]", pattern)

profile_in_scope <- function(datasets, ds) {
  length(datasets) == 0 || any(vapply(datasets, function(p) profile_match(p, ds), logical(1)))
}
