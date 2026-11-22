#' De-identify linked datasets with a profile
#'
#' Turns a list of SDTM and/or ADaM datasets (or other data described by a
#' profile) into a copy whose measured re-identification risk meets a target,
#' with as little damage as possible: it removes direct identifiers,
#' pseudonymizes subjects, offsets their dates, generalizes quasi-identifiers
#' up their ladders, and suppresses what remains.
#'
#' @inheritParams anon_suppress
#' @param data A data frame, a named list of data frames linked by `key`, or
#'   the result of an earlier step.
#' @param profile An [anon_profile()], such as [anon_profile_sdtm()],
#'   [anon_profile_adam()], or both combined with `c()` for a list holding
#'   SDTM and ADaM datasets.
#' @param key Name of the subject identifier column. It must be the profile's
#'   `"key"` rule, if it has one.
#' @param max_suppressed Largest share of quasi-identifier cells (subjects
#'   times the quasi-identifiers that vary) that suppression may blank before
#'   the search generalizes further. Default 0.05.
#' @param unresolved What to do with subjects still in groups smaller than
#'   the target when nothing is left to blank: `"drop"` (default) removes
#'   them from every dataset, `"keep"` keeps them (see [anon_suppress()]).
#' @param keep_secrets Logical. If `TRUE`, keep the pseudonym mapping, the
#'   date offsets and the folded categories; read them with [anon_secrets()].
#'   Default `FALSE`: they are kept nowhere.
#' @param seed Optional seed, for reproducible tests: the pseudonyms are drawn
#'   with `seed` and the date offsets with `seed + 1`. Anyone with the seed
#'   and the original data can rebuild the mapping and the offsets, so don't
#'   set it for data you share.
#'
#' @details
#' **Steps.**
#' 1. Direct identifiers (`"remove"` rules) are blanked in every row of the
#'    datasets themselves; the columns are kept, with their type and
#'    attributes, so code that uses them still runs.
#' 2. [anon_subject_ids()] replaces the key, `"also"` and `"key_cols"`
#'    columns.
#' 3. [anon_subject_dates()] offsets every date of each subject by one
#'    random offset (its own detection: Date and date-time columns, and text
#'    columns named `*DTC`, at any depth).
#' 4. The risk is measured ([anon_risk()]) on the quasi-identifiers the
#'    profile finds in the data, each from its first copy found (the source).
#' 5. **Ladder search** (greedy, as Datafly): while the suppression needed to
#'    meet the target would blank more than `max_suppressed` of the
#'    quasi-identifier cells, it takes one step up the ladder of the
#'    quasi-identifier whose next step leaves the least to suppress (ties:
#'    the step that loses the fewest distinct values, then profile order).
#'    The suppression needed is estimated by running [anon_suppress()]'s
#'    algorithm on the measured values, one value per subject; a subject it
#'    can't resolve counts as all its cells. It stops when within the budget
#'    or when every ladder is exhausted; then it keeps the state, among those
#'    it went through, that needed the least suppression (the earliest on
#'    ties). Last, it steps back down wherever a step isn't needed: the
#'    suppression needed stays within the budget, or no larger than the
#'    search reached (most useful quasi-identifiers first). Each ladder step
#'    applies to the original values. A quasi-identifier with one value for
#'    every subject can't split a group: it is measured, but neither
#'    generalized nor suppressed.
#' 6. The chosen steps are applied to every copy of each quasi-identifier
#'    ([anon_generalize()], in every dataset with the key that the rule
#'    covers; `"remove"` blanks every copy in every row), then
#'    [anon_suppress()] blanks what remains, with `everywhere = TRUE` and the
#'    profile's priorities as `order`. Cells blanked in a source are blanked
#'    in its other copies for the same subjects (copies under another name,
#'    such as `WEIGHTBL` in `adsl` for weight in `vs`, and test copies, such
#'    as the `WEIGHT` rows of `advs`).
#' 7. Derived variables are kept where they still follow from the released
#'    values of their quasi-identifiers, and blanked elsewhere (below). The
#'    risk is measured again.
#'
#' **Derived variables.** A derived variable is left as it is in rows where
#' its quasi-identifiers didn't change. Where they did, with one
#' quasi-identifier in the same row (such as `AGEGR1` with `AGE` in `adsl`):
#' - from a text quasi-identifier (`RACEN` from `RACE`), it takes the value
#'   that the released value had in the original data (the code of
#'   `"OTHER"`, say) when the original data maps each value to one derived
#'   value, and is blanked otherwise or where there is none;
#' - from a numeric one (`AGEGR1` from `AGE`), it takes a category only when
#'   the derived values are intervals of the quasi-identifier in the original
#'   data and every value the released one can stand for (all ages of a band,
#'   whole numbers when the original values are) lies within one category:
#'   between its lowest and highest observed values, the lowest category
#'   being open below and the highest above (between two categories, where
#'   the cut lies isn't known); otherwise it is blanked.
#'
#' A derived variable with several quasi-identifiers (`BMIBL`), or whose
#' quasi-identifiers aren't in the same row (the `BMI` parameter), is blanked
#' wherever one of them changed for the row or the subject; for a result
#' column (`AVAL`, `--STRESN`), with the other results of the row. Blanking
#' keeps values truthful: a derived value recomputed from a representative
#' (such as a BMI from a rounded weight) would not be any subject's.
#'
#' **When the target is met.** `settings$deidentify$meets_target` is `TRUE`
#' only when the risk measured on the released data meets the target and
#' [anon_suppress()] found no other reason not to (copies of a
#' quasi-identifier that differ, birth dates next to `AGE`, values of removed
#' subjects left); reasons are in `settings$deidentify$not_met`. No
#' quasi-identifier found in the data is a reason too.
#'
#' **Residual signals.** Meeting the target is about the quasi-identifiers
#' only. Other things a person might know single subjects out too: treatment
#' duration (the profile's `"duration"` rules, which offsetting doesn't
#' change), the number of rows a subject has in each dataset, and the set of
#' tests (`--TESTCD`, `PARAMCD`) a subject has. `signals` counts, for each,
#' the subjects unique on it alone and in groups smaller than the target,
#' alone and together with the released quasi-identifiers. They are
#' measured, not changed (changing dates would break study days). When
#' subjects are in small groups on them, the copy is not anonymous against
#' someone who knows them: say so wherever it is shared.
#'
#' The log has rows for every step and every ladder move, with counts and no
#' values; the settings hold the profile (as text), the levels chosen, the
#' risk before and after and the residual signals.
#'
#' Meeting the target is evidence about re-identification risk from the
#' profile's quasi-identifiers, not proof that the data is anonymous under
#' any regime (see [anon_risk()]). Shifted dates do not meet HIPAA Safe
#' Harbor.
#'
#' @return An object of class `"anon_deid_step"` (see [anon_subject_ids()]),
#'   chainable with the other steps, with the steps' logs and settings
#'   (`settings$deidentify` for this function), the record of suppressed
#'   cells, and also `risk_before` and `risk_after` (from [anon_risk()]) and
#'   `signals` (a tibble with `signal`, `dataset`, `n_subjects`, `n_unique`,
#'   `n_below_k` and `n_below_k_with_quasi`). Secrets only with
#'   `keep_secrets = TRUE`.
#'
#' @seealso [anon_profile()], [anon_risk()], [anon_generalize()],
#'   [anon_suppress()]
#'
#' @examples
#' dm <- data.frame(
#'   USUBJID = sprintf("S%02d", 1:12),
#'   AGE = c(31, 34, 38, 42, 45, 47, 52, 55, 58, 61, 66, 69),
#'   SEX = rep(c("F", "M"), 6),
#'   RFSTDTC = format(as.Date("2020-01-01") + 0:11)
#' )
#' result <- anon_deidentify(list(dm = dm), max_risk = 1/3, seed = 1)
#' result$data$dm
#' result$settings$deidentify$meets_target
#'
#' @export
anon_deidentify <- function(
  data,
  profile = anon_profile_sdtm(),
  key = "USUBJID",
  max_risk = 0.09,
  max_suppressed = 0.05,
  unresolved = c("drop", "keep"),
  keep_secrets = FALSE,
  seed = NULL
) {
  unresolved <- match.arg(unresolved)
  if (!inherits(profile, "anon_profile")) {
    stop("`profile` must be an `anon_profile()`, such as `anon_profile_sdtm()`.", call. = FALSE)
  }
  check_key_name(key)
  k_target <- risk_k_target(max_risk)
  if (!is.numeric(max_suppressed) || length(max_suppressed) != 1 || is.na(max_suppressed) ||
    max_suppressed < 0 || max_suppressed > 1) {
    stop("`max_suppressed` must be a single number from 0 to 1.", call. = FALSE)
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed))) {
    stop("`seed` must be NULL or a single number.", call. = FALSE)
  }
  keys_in_profile <- unique(profile$variable[profile$role == "key"])
  if (length(keys_in_profile) > 0 && !identical(keys_in_profile, key)) {
    stop(
      "The profile's key is ", paste0("`", keys_in_profile, "`", collapse = ", "), ", not `", key,
      "`: pass it as `key`, or change the profile.",
      call. = FALSE
    )
  }

  # A single data frame goes through the steps as a list of one, named
  # "data", and comes back as a data frame.
  previous <- if (inherits(data, "anon_deid_step")) data
  single <- is.data.frame(if (is.null(previous)) data else previous$data)
  if (single) {
    if (is.null(previous)) data <- list(data = data) else previous$data <- list(data = previous$data)
  }
  input <- as_dataset_list(if (is.null(previous)) data else previous)
  d <- input$data

  # 1. Direct identifiers.
  columns <- profile_columns(profile, d)
  also <- profile_role_columns(profile, d, columns, "also", key)
  key_cols <- profile_role_columns(profile, d, columns, "key_cols", key)
  keyed <- names(d)[vapply(d, function(df) key %in% names(df), logical(1))]
  log <- deid_log_start(
    "remove", length(subject_keys(d[keyed], key)), key, NULL,
    settings = "direct identifiers blanked in every row; columns kept with their type and attributes"
  )
  removed <- columns[columns$role == "remove", , drop = FALSE]
  for (i in seq_len(nrow(removed))) {
    ds <- removed$dataset[i]
    out <- blank_rows(d[[ds]], removed$column[i], seq_len(nrow_of(d[[ds]])))
    d[[ds]] <- out$df
    log <- deid_log_add(log, ds, removed$column[i], "blanked in every row (direct identifier)", out$n, 0)
  }
  step <- new_deid_step(
    input$unwrap(d), log, NULL, previous = input$previous,
    settings = list(remove = list(columns = paste0(removed$dataset, ".", removed$column)))
  )

  # 2-3. Subjects and dates.
  step <- anon_subject_ids(step, key = key, also = also, key_cols = key_cols, keep_secrets = keep_secrets, seed = seed)
  step <- anon_subject_dates(
    step, key = key, keep_secrets = keep_secrets, seed = if (!is.null(seed)) seed + 1, also = also
  )
  base <- step
  warn_unshifted_profile_dates(profile, base$data, key)

  # 4. Quasi-identifiers and the risk before.
  qis <- profile_quasi(profile, base$data, key)
  # One measure per quasi-identifier: a column named by its dataset, or a
  # test.
  specs <- do.call(c, c(list(list()), lapply(qis, function(q) {
    spec <- quasi_spec(q, key)
    if (inherits(spec, "anon_quasi_values")) list(spec) else spec
  })))
  risk_before <- NULL
  search <- NULL
  if (length(qis) > 0) {
    risk_before <- anon_risk(base, specs, key = key, max_risk = max_risk)
    inputs <- suppressWarnings(risk_inputs(base, specs, key))
    qvar <- inputs$qnames
    rank <- quasi_rank(qis)
    # A quasi-identifier with one value for every subject can't split any
    # group: it is measured, but neither generalized nor suppressed.
    constant <- vapply(qvar, function(v) vctrs::vec_unique_count(inputs$table[[v]]) <= 1, logical(1))
    for (q in which(constant)) {
      qis[[q]]$constant <- TRUE
    }

    # 5. Ladder search.
    searched <- which(!constant)
    if (length(searched) > 0) {
      search <- ladder_search(inputs, qis[searched], qvar[searched], k_target, rank(rank[searched]), max_suppressed)
      for (i in seq_along(searched)) {
        q <- searched[i]
        qis[[q]]$ladder <- search$ladders[[i]]
        qis[[q]]$level <- search$levels[i]
        qis[[q]]$step <- if (search$levels[i] > 0) qis[[q]]$ladder[[search$levels[i]]]
      }
    }
  }
  removed_q <- vapply(qis, function(q) identical(q$step, "remove"), logical(1))
  constant_q <- vapply(qis, function(q) isTRUE(q$constant), logical(1))

  # 6. Generalize every copy, then suppress.
  rules <- generalize_copy_rules(qis[!removed_q])
  gen <- if (length(rules) > 0) anon_generalize(base, rules, key = key, keep_secrets = keep_secrets) else base
  active <- which(!removed_q)
  suppressed_q <- which(!removed_q & !constant_q)
  sup <- gen
  if (length(suppressed_q) > 0) {
    sup <- anon_suppress(
      gen, specs[suppressed_q], key = key, max_risk = max_risk, order = qvar[suppressed_q][order(rank[suppressed_q])],
      unresolved = unresolved, key_cols = key_cols, also = also, everywhere = TRUE
    )
  }

  # 7. Copies, removed quasi-identifiers and derived variables.
  d <- sup$data
  final_keys <- subject_keys(Filter(function(df) key %in% names(df), d), key)
  dropped <- setdiff(subject_keys(Filter(function(df) key %in% names(df), base$data), key), final_keys)
  aligned <- lapply(base$data, function(df) drop_subject_rows(df, key, key_cols, dropped)$df)
  log <- deid_log_start(
    "deidentify", length(final_keys), key, seed,
    settings = paste0(
      "profile ", attr(profile, "profile_name"), ", max_risk = ", format(max_risk, digits = 7),
      " (groups of at least ", k_target, "), max_suppressed = ", format(max_suppressed, digits = 7)
    )
  )
  for (move in search$moves) {
    log <- deid_log_add(log, NA_character_, move$quasi, move$detail, NA, 0)
  }
  record <- list()
  for (q in qis[removed_q]) {
    for (copy in q$copies) {
      out <- blank_copy(d[[copy$dataset]], copy, key, NULL)
      d[[copy$dataset]] <- out$df
      for (row in out$log) {
        log <- deid_log_add(log, copy$dataset, row$variable, paste0("removed (", q$name, " blanked in every row)"), row$n, 0)
      }
    }
  }
  for (q in qis[!removed_q]) {
    keys <- source_blanked_keys(sup$suppressed, q$copies[[q$source]])
    if (length(keys) == 0) next
    for (copy in q$copies[-q$source]) {
      if (copy$type == "column" && q$copies[[q$source]]$type == "column" && copy$column == q$copies[[q$source]]$column) {
        # anon_suppress() blanked copies of the same name (everywhere = TRUE).
        next
      }
      out <- blank_copy(d[[copy$dataset]], copy, key, keys)
      d[[copy$dataset]] <- out$df
      for (row in out$log) {
        log <- deid_log_add(
          log, copy$dataset, row$variable,
          paste0("blanked for ", length(keys), " subject(s) (copy of ", q$name, ", suppressed in its source)"), row$n, 0
        )
      }
      record <- c(record, list(copy_record(copy, keys)))
    }
  }
  changed <- quasi_changed_keys(qis, aligned, d, key)
  derived <- derive_columns(profile, qis, aligned, d, key, changed)
  d <- derived$data
  for (row in derived$log) {
    log <- deid_log_add(log, row$dataset, row$variable, row$detail, row$n, 0)
  }
  record <- if (length(record) > 0) do.call(rbind, record)
  final <- new_deid_step(d, log, NULL, previous = sup, settings = list(), suppressed = record)

  # The risk after, the reasons the target isn't met and residual signals.
  not_met <- if (!is.null(sup$settings$suppress) && length(suppressed_q) > 0) {
    setdiff(sup$settings[[last_setting(sup$settings, "suppress")]]$not_met, "measured risk above the target")
  } else {
    character(0)
  }
  risk_after <- NULL
  if (length(active) > 0) {
    risk_after <- tryCatch(
      suppressWarnings(anon_risk(final, specs[active], key = key, max_risk = max_risk)),
      error = function(e) NULL
    )
    if (is.null(risk_after) || !isTRUE(risk_after$summary$meets_target)) {
      not_met <- c(not_met, "measured risk above the target")
    }
  } else if (length(qis) == 0) {
    not_met <- c(not_met, "no quasi-identifiers found in the data")
  } else if (length(final_keys) < k_target) {
    not_met <- c(not_met, "fewer subjects than the target group size")
  }
  signals <- residual_signals(profile, final, qis[active], specs[active], key, k_target)

  log <- final$log
  first <- which(log$step == "deidentify")[1]
  log$detail[first] <- paste0(
    log$detail[first],
    "; levels: ", describe_levels(qis),
    "; before: ", describe_risk_summary(risk_before, k_target),
    "; after: ", describe_risk_summary(risk_after, k_target),
    if (length(not_met) > 0) paste0("; target not met: ", paste(unique(not_met), collapse = ", ")),
    if (any(signals$n_below_k_with_quasi > 0)) "; residual signals single out subjects (see `signals`)"
  )
  final$log <- log
  final$settings$deidentify <- list(
    key = key,
    profile = attr(profile, "profile_name"),
    profile_rules = profile_text(profile),
    max_risk = max_risk, k_target = k_target, max_suppressed = max_suppressed, unresolved = unresolved,
    also = also, key_cols = key_cols,
    quasi = quasi_settings(qis),
    moves = search_moves_table(search),
    estimated_suppressed = if (!is.null(search)) search$estimate$prop else NA_real_,
    exhausted = isTRUE(search$exhausted),
    n_dropped = length(dropped),
    risk_before = if (!is.null(risk_before)) risk_before$summary,
    risk_after = if (!is.null(risk_after)) risk_after$summary,
    signals = signals,
    meets_target = length(not_met) == 0, not_met = unique(not_met),
    keep_secrets = isTRUE(keep_secrets), seeded = !is.null(seed)
  )
  final$risk_before <- risk_before
  final$risk_after <- risk_after
  final$signals <- signals
  if (single) {
    final$data <- final$data[[1]]
  }
  final
}

# helpers -----------------------------------------------------------------

# The name of the last settings entry of a step kind (settings of repeated
# steps are made unique with "_1", "_2").
last_setting <- function(settings, step) {
  utils::tail(names(settings)[names(settings) == step | startsWith(names(settings), paste0(step, "_"))], 1)
}

# The rule (by row of the profile) that applies to each column of each
# dataset, with its role: among the rules (other than test rules) whose
# variable and datasets match, one limited to datasets wins over one that
# isn't, an exact name over a pattern, and a later rule over an earlier one.
profile_columns <- function(profile, data) {
  plain <- which(is.na(profile$test_col))
  out <- list()
  for (ds in names(data)) {
    cols <- names(data[[ds]])
    if (length(cols) == 0) next
    best <- rep(NA_integer_, length(cols))
    score <- rep(-Inf, length(cols))
    for (i in plain) {
      if (!profile_in_scope(profile$datasets[[i]], ds)) next
      hit <- profile_match(profile$variable[i], cols)
      s <- 2 * (length(profile$datasets[[i]]) > 0) + !profile_is_pattern(profile$variable[i])
      take <- hit & s >= score
      best[take] <- i
      score[take] <- s
    }
    found <- !is.na(best)
    if (any(found)) {
      out <- c(out, list(tibble::tibble(
        dataset = ds, column = cols[found], rule = best[found], role = profile$role[best[found]],
        quasi_name = profile$quasi_name[best[found]]
      )))
    }
  }
  if (length(out) == 0) {
    return(tibble::tibble(dataset = character(0), column = character(0), rule = integer(0), role = character(0), quasi_name = character(0)))
  }
  do.call(rbind, out)
}

# The columns found in the data with a role, for the IDs step (`also`,
# `key_cols`), without the key.
profile_role_columns <- function(profile, data, columns, role, key) {
  setdiff(unique(columns$column[columns$role == role]), key)
}

# Warn about columns the profile names as dates that the dates step doesn't
# offset: not Date, date-time or text named *DTC (numeric SAS dates are
# named in the dates step's own warning).
warn_unshifted_profile_dates <- function(profile, data, key) {
  columns <- profile_columns(profile, data)
  dates <- columns[columns$role == "date", , drop = FALSE]
  odd <- character(0)
  for (i in seq_len(nrow(dates))) {
    df <- data[[dates$dataset[i]]]
    if (!key %in% names(df)) next
    x <- df[[dates$column[i]]]
    shifted <- inherits(x, c("Date", "POSIXct", "POSIXlt")) ||
      ((is.character(x) || is.factor(x)) && grepl("DTC$", dates$column[i], ignore.case = TRUE, useBytes = TRUE))
    sas <- is.numeric(x) && grepl(sas_date_format_pattern, attr(x, "format.sas", exact = TRUE) %||% "", ignore.case = TRUE)
    if (!shifted && !sas && !all(vctrs::vec_detect_missing(blank_to_na(x)))) {
      odd <- c(odd, paste0(dates$dataset[i], ".", dates$column[i]))
    }
  }
  if (length(odd) > 0) {
    warning(
      "These columns are dates in the profile but were not offset (only Date, date-time and text columns ",
      "named *DTC are): ", paste(odd, collapse = ", "), ". Convert them to Date, or remove them.",
      call. = FALSE
    )
  }
}

# The quasi-identifiers found in the data: each with its copies (columns
# and test results, in the order of the profile's rules and then of the
# datasets), its ladder and priority (from the rule of its source, the first
# copy that has one value per subject), in the order of its first rule.
profile_quasi <- function(profile, data, key) {
  keyed <- names(data)[vapply(data, function(df) key %in% names(df), logical(1))]
  columns <- profile_columns(profile, data[keyed])
  columns <- columns[columns$role == "quasi", , drop = FALSE]
  copies <- list()
  for (i in which(profile$role == "quasi")) {
    if (is.na(profile$test_col[i])) {
      hit <- columns[columns$rule == i, , drop = FALSE]
      for (j in seq_len(nrow(hit))) {
        copies <- c(copies, list(list(
          type = "column", dataset = hit$dataset[j], column = hit$column[j], rule = i, name = profile$quasi_name[i]
        )))
      }
      next
    }
    for (ds in keyed) {
      df <- data[[ds]]
      if (!profile_in_scope(profile$datasets[[i]], ds) || !profile$test_col[i] %in% names(df)) next
      result <- names(df)[profile_match(profile$variable[i], names(df))][1]
      tests <- df[[profile$test_col[i]]]
      if (is.na(result) || !any(!is.na(tests) & as.character(tests) == profile$test[i])) next
      copies <- c(copies, list(list(
        type = "test", dataset = ds, test_col = profile$test_col[i], test = profile$test[i], result_col = result,
        flag = if (!is.na(profile$flag[i]) && profile$flag[i] %in% names(df)) profile$flag[i] else NA_character_,
        rule = i, name = profile$quasi_name[i]
      )))
    }
  }
  if (length(copies) == 0) {
    return(list())
  }
  names_found <- unique(vapply(copies, `[[`, character(1), "name"))
  lapply(names_found, function(name) {
    mine <- Filter(function(cp) cp$name == name, copies)
    measurable <- vapply(mine, function(cp) cp$type == "test" || one_value_per_subject(data[[cp$dataset]], cp$column, key), logical(1))
    source <- if (any(measurable)) which(measurable)[1] else 1L
    rule <- mine[[source]]$rule
    list(
      name = name, copies = mine, source = source, ladder = profile$ladder[[rule]], priority = profile$priority[rule],
      first_rule = min(vapply(mine, `[[`, integer(1), "rule")), level = 0L, step = NULL
    )
  })
}

one_value_per_subject <- function(df, col, key) {
  pairs <- vctrs::vec_unique(data.frame(k = normalize_keys(df[[key]]), v = blank_to_na(vctrs::vec_data(df[[col]]))))
  !anyDuplicated(pairs$k[!is.na(pairs$k)])
}

# The measure of a quasi-identifier, from its source: a column, or a test's
# baseline value (see baseline_rows()).
quasi_spec <- function(q, key) {
  cp <- q$copies[[q$source]]
  if (cp$type == "column") {
    return(stats::setNames(list(cp$column), cp$dataset))
  }
  filter <- if (!is.na(cp$flag)) {
    rlang::new_formula(
      NULL,
      rlang::call2("baseline_rows", rlang::sym(cp$flag), rlang::sym(key), rlang::sym(cp$test_col)),
      env = environment(anon_deidentify)
    )
  }
  anon_quasi_values(cp$dataset, cp$test_col, cp$test, cp$result_col, filter = filter, names = q$name)
}

# Rows flagged as baseline ("Y"), and every row of a subject's test without a
# flagged row (so that its first result is measured).
baseline_rows <- function(flag, key, test) {
  flagged <- !is.na(flag) & as.character(flag) == "Y"
  group <- paste(key_as_character(key), as.character(test), sep = "\u001f")
  has_flag <- group %in% group[flagged]
  flagged | !has_flag
}

# Each quasi-identifier's rank from least (1) to most useful: by priority,
# then without one, in profile order.
quasi_rank <- function(qis) {
  priority <- vapply(qis, function(q) if (is.na(q$priority)) Inf else as.numeric(q$priority), numeric(1))
  first <- vapply(qis, `[[`, integer(1), "first_rule")
  match(seq_along(qis), order(priority, first))
}

# Greedy search of the ladder levels (see anon_deidentify()), on the
# measured values (one per subject).
ladder_search <- function(inputs, qis, qvar, k_target, rank, max_suppressed) {
  table <- inputs$table
  keys <- table[[1]]
  n <- nrow(table)
  nq <- length(qis)
  cache <- list()
  values_at <- function(q, level) {
    id <- paste(q, level)
    if (is.null(cache[[id]])) {
      x <- table[[qvar[q]]]
      cache[[id]] <<- if (level == 0) {
        x
      } else {
        step <- qis[[q]]$ladder[[level]]
        if (identical(step, "remove")) vctrs::vec_init(x, n) else step(x, subjects = keys)
      }
    }
    cache[[id]]
  }
  # Ladder steps that don't apply to the values (such as a fold to text on
  # a numeric column) are left out, with a warning.
  skipped <- character(0)
  for (q in seq_len(nq)) {
    ok <- vapply(seq_along(qis[[q]]$ladder), function(level) {
      !inherits(tryCatch(values_at(q, level), error = function(e) e), "error")
    }, logical(1))
    if (!all(ok)) {
      skipped <- c(skipped, paste0(qis[[q]]$name, ": ", paste(vapply(qis[[q]]$ladder[!ok], describe_ladder_step, character(1)), collapse = ", ")))
      qis[[q]]$ladder <- qis[[q]]$ladder[ok]
      cache <- cache[!startsWith(names(cache), paste0(q, " "))]
    }
  }
  if (length(skipped) > 0) {
    warning(
      "These ladder steps don't apply to the values of their quasi-identifier and were left out: ",
      paste(skipped, collapse = "; "), ".",
      call. = FALSE
    )
  }
  estimate <- function(levels) {
    tab <- tibble::as_tibble(stats::setNames(lapply(seq_len(nq), function(q) values_at(q, levels[q])), qvar))
    res <- suppress_cells(suppress_codes(tab, matrix(FALSE, n, nq)), k_target, rank)
    resolved <- setdiff(seq_len(n), res$still)
    cells <- sum(res$blanked[resolved, , drop = FALSE]) + nq * length(res$still)
    list(cells = cells, prop = if (n * nq > 0) cells / (n * nq) else 0, n_unresolved = length(res$still))
  }
  distinct <- function(q, level) {
    x <- values_at(q, level)
    vctrs::vec_unique_count(x[!vctrs::vec_detect_missing(x)])
  }
  depth <- vapply(qis, function(q) length(q$ladder), integer(1))
  first <- vapply(qis, `[[`, integer(1), "first_rule")
  levels <- rep(0L, nq)
  current <- estimate(levels)
  visited <- list(list(levels = levels, estimate = current))
  moves <- list()
  while (current$prop > max_suppressed) {
    open <- which(levels < depth)
    if (length(open) == 0) break
    trials <- lapply(open, function(q) {
      next_levels <- levels
      next_levels[q] <- levels[q] + 1L
      list(q = q, levels = next_levels, estimate = estimate(next_levels), lost = distinct(q, levels[q]) - distinct(q, levels[q] + 1L))
    })
    cells <- vapply(trials, function(t) t$estimate$cells, numeric(1))
    lost <- vapply(trials, `[[`, numeric(1), "lost")
    best <- trials[[order(cells, lost, first[open])[1]]]
    q <- best$q
    moves <- c(moves, list(list(
      quasi = qis[[q]]$name, level = best$levels[q], step = describe_ladder_step(qis[[q]]$ladder[[best$levels[q]]]),
      cells_before = current$cells, cells_after = best$estimate$cells, prop_after = best$estimate$prop,
      distinct_before = distinct(q, levels[q]), distinct_after = distinct(q, best$levels[q]),
      detail = paste0(
        "ladder: ", qis[[q]]$name, " to step ", best$levels[q], " (", describe_ladder_step(qis[[q]]$ladder[[best$levels[q]]]),
        "); distinct values ", distinct(q, levels[q]), " to ", distinct(q, best$levels[q]),
        "; estimated cells to suppress ", current$cells, " to ", best$estimate$cells,
        " (", format(round(100 * best$estimate$prop, 1), nsmall = 1), "% of ", n * nq, ")"
      )
    )))
    levels <- best$levels
    current <- best$estimate
    visited <- c(visited, list(list(levels = levels, estimate = current)))
  }
  exhausted <- current$prop > max_suppressed
  if (exhausted) {
    # Every ladder is exhausted: keep the state that needed the least
    # suppression (the earliest on ties).
    pick <- which.min(vapply(visited, function(v) v$estimate$cells, numeric(1)))
    if (pick < length(visited)) {
      kept <- visited[[pick]]
      moves <- c(moves, list(list(
        quasi = NA_character_, level = NA_integer_, step = NA_character_, cells_before = current$cells,
        cells_after = kept$estimate$cells, prop_after = kept$estimate$prop, distinct_before = NA_integer_,
        distinct_after = NA_integer_,
        detail = paste0(
          "ladders exhausted above max_suppressed; back to the levels after move ", pick - 1,
          ", which needed the least suppression (", kept$estimate$cells, " cells)"
        )
      )))
      levels <- kept$levels
      current <- kept$estimate
    }
  }
  # Steps back down: a greedy step taken on a plateau (or one that a later
  # step made unneeded) is undone wherever the suppression needed stays
  # within the budget, or no larger than the search reached; the most
  # useful quasi-identifiers first.
  limit <- current$cells
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (q in order(-rank)) {
      while (levels[q] > 0) {
        lower <- levels
        lower[q] <- levels[q] - 1L
        e <- estimate(lower)
        if (!(e$prop <= max_suppressed || e$cells <= limit)) break
        moves <- c(moves, list(list(
          quasi = qis[[q]]$name, level = lower[q],
          step = if (lower[q] == 0) NA_character_ else describe_ladder_step(qis[[q]]$ladder[[lower[q]]]),
          cells_before = current$cells, cells_after = e$cells, prop_after = e$prop,
          distinct_before = distinct(q, levels[q]), distinct_after = distinct(q, lower[q]),
          detail = paste0(
            "ladder: ", qis[[q]]$name, " back to ",
            if (lower[q] == 0) "its original values" else paste0("step ", lower[q], " (", describe_ladder_step(qis[[q]]$ladder[[lower[q]]]), ")"),
            ", not needed; estimated cells to suppress ", current$cells, " to ", e$cells,
            " (", format(round(100 * e$prop, 1), nsmall = 1), "% of ", n * nq, ")"
          )
        )))
        levels <- lower
        current <- e
        changed <- TRUE
      }
    }
  }
  list(levels = levels, ladders = lapply(qis, `[[`, "ladder"), estimate = current, moves = moves, exhausted = exhausted)
}

# anon_generalize() rules for the chosen steps of the quasi-identifiers, on
# every copy.
generalize_copy_rules <- function(qis) {
  rules <- list()
  for (q in qis) {
    if (is.null(q$step)) next
    for (cp in q$copies) {
      if (cp$type == "column") {
        if (is.null(rules[[cp$dataset]])) rules[[cp$dataset]] <- list()
        rules[[cp$dataset]][[cp$column]] <- q$step
      } else {
        rules <- c(rules, list(anon_rule(anon_quasi_values(cp$dataset, cp$test_col, cp$test, cp$result_col), q$step)))
      }
    }
  }
  rules
}

# The source of a copy, as anon_suppress() names it.
copy_source <- function(cp) {
  if (cp$type == "column") {
    tibble::tibble(dataset = cp$dataset, column = cp$column, test_col = NA_character_, test = NA_character_, result_col = NA_character_)
  } else {
    tibble::tibble(dataset = cp$dataset, column = NA_character_, test_col = cp$test_col, test = cp$test, result_col = cp$result_col)
  }
}

copy_record <- function(cp, keys) {
  src <- copy_source(cp)
  tibble::tibble(
    dataset = src$dataset, column = src$column, test_col = src$test_col, test = src$test,
    result_col = src$result_col, key = keys
  )
}

# Keys whose cells of a source copy were blanked, from the step's record.
source_blanked_keys <- function(mask, cp) {
  if (is.null(mask) || nrow(mask) == 0) {
    return(character(0))
  }
  hit <- mask$dataset == cp$dataset & if (cp$type == "column") {
    !is.na(mask$column) & mask$column == cp$column
  } else {
    !is.na(mask$test) & mask$test_col == cp$test_col & mask$test == cp$test &
      result_family(mask$result_col) == result_family(cp$result_col)
  }
  unique(mask$key[hit])
}

# Blank a copy for the subjects `keys` (all rows when NULL): a column, or
# every row of a test with its other results.
blank_copy <- function(df, cp, key, keys) {
  rows <- if (is.null(keys)) rep(TRUE, nrow_of(df)) else normalize_keys(df[[key]]) %in% keys
  if (cp$type == "column") {
    out <- blank_rows(df, cp$column, which(rows))
    return(list(df = out$df, log = list(list(variable = cp$column, n = out$n))))
  }
  rows <- which(rows & test_rows(df, cp))
  log <- list()
  for (col in result_siblings(df, cp$result_col)) {
    out <- blank_rows(df, col, rows)
    df <- out$df
    log <- c(log, list(list(variable = test_variable(col, cp$test_col, cp$test), n = out$n)))
  }
  list(df = df, log = log)
}

# Rows of a copy: every row for a column, the test's rows for a test.
copy_rows <- function(df, cp) {
  if (cp$type == "column") seq_len(nrow_of(df)) else which(test_rows(df, cp))
}

copy_column <- function(cp) if (cp$type == "column") cp$column else cp$result_col

# For each quasi-identifier, the subjects (keys) whose value changed in any
# copy (generalized, blanked or removed).
quasi_changed_keys <- function(qis, before, after, key) {
  stats::setNames(lapply(qis, function(q) {
    out <- character(0)
    for (cp in q$copies) {
      old <- before[[cp$dataset]]
      new <- after[[cp$dataset]]
      rows <- copy_rows(old, cp)
      col <- copy_column(cp)
      hit <- changed_values(vctrs::vec_slice(blank_to_na(old[[col]]), rows), vctrs::vec_slice(blank_to_na(new[[col]]), rows))
      out <- c(out, normalize_keys(old[[key]])[rows][hit])
    }
    unique(out[!is.na(out)])
  }), vapply(qis, `[[`, character(1), "name"))
}

# Derived variables (see anon_deidentify()): kept where their
# quasi-identifiers didn't change, else mapped from a single
# quasi-identifier in the same row when that is exact, else blanked.
derive_columns <- function(profile, qis, before, data, key, changed) {
  log <- list()
  by_name <- stats::setNames(qis, vapply(qis, `[[`, character(1), "name"))
  keyed <- names(data)[vapply(data, function(df) key %in% names(df), logical(1))]
  columns <- profile_columns(profile, data[keyed])
  targets <- list()
  for (j in which(columns$role == "derived")) {
    targets <- c(targets, list(list(
      dataset = columns$dataset[j], column = columns$column[j], rule = columns$rule[j], type = "column"
    )))
  }
  for (i in which(profile$role == "derived" & !is.na(profile$test_col))) {
    for (ds in keyed) {
      df <- data[[ds]]
      if (!profile_in_scope(profile$datasets[[i]], ds) || !profile$test_col[i] %in% names(df)) next
      for (col in names(df)[profile_match(profile$variable[i], names(df))]) {
        targets <- c(targets, list(list(
          dataset = ds, column = col, rule = i, type = "test", test_col = profile$test_col[i], test = profile$test[i]
        )))
      }
    }
  }
  for (tg in targets) {
    ds <- tg$dataset
    df <- data[[ds]]
    old_df <- before[[ds]]
    rows <- if (tg$type == "column") seq_len(nrow_of(df)) else which(test_rows(df, tg))
    if (length(rows) == 0) next
    from <- intersect(profile$from[[tg$rule]], names(by_name))
    if (length(from) == 0) next
    # A copy of each quasi-identifier in the same rows, if there is one.
    row_copy <- lapply(by_name[from], function(q) {
      for (cp in q$copies) {
        if (cp$dataset != ds) next
        if (tg$type == "column" && cp$type == "column") return(cp)
        if (tg$type == "test" && cp$type == "test" && cp$test_col == tg$test_col && cp$test == tg$test) return(cp)
      }
      NULL
    })
    keys <- normalize_keys(df[[key]])[rows]
    hit <- rep(FALSE, length(rows))
    for (name in from) {
      cp <- row_copy[[name]]
      hit <- hit | if (is.null(cp)) {
        keys %in% changed[[name]]
      } else {
        col <- copy_column(cp)
        changed_values(vctrs::vec_slice(blank_to_na(old_df[[col]]), rows), vctrs::vec_slice(blank_to_na(df[[col]]), rows))
      }
    }
    if (!any(hit)) next
    target_cols <- if (tg$type == "test") result_siblings(df, tg$column) else tg$column
    if (length(from) == 1 && !is.null(row_copy[[from]]) && length(target_cols) == 1) {
      q <- by_name[[from]]
      cp <- row_copy[[from]]
      mapped <- map_derived(old_df[[copy_column(cp)]][rows], df[[copy_column(cp)]][rows], old_df[[tg$column]][rows], hit, q$step)
      assign_rows <- rows[mapped$from > 0]
      if (length(assign_rows) > 0) {
        y <- df[[tg$column]]
        y <- vctrs::vec_assign(y, assign_rows, vctrs::vec_slice(old_df[[tg$column]], rows[mapped$from[mapped$from > 0]]))
        n_kept <- sum(changed_values(vctrs::vec_slice(old_df[[tg$column]], assign_rows), vctrs::vec_slice(y, assign_rows)))
        df <- set_generalized_column(df, tg$column, y, n_kept > 0)
      }
      out <- blank_rows(df, tg$column, rows[mapped$blank])
      df <- out$df
      where <- if (tg$type == "test") test_variable(tg$column, tg$test_col, tg$test) else tg$column
      detail <- paste0(
        "derived from ", from, ": ", sum(hit & !mapped$blank), " row(s) where ", from, " changed keep a value that follows from it",
        "; ", out$n, " blanked"
      )
      log <- c(log, list(list(dataset = ds, variable = where, detail = detail, n = out$n)))
    } else {
      for (col in target_cols) {
        out <- blank_rows(df, col, rows[hit])
        df <- out$df
        where <- if (tg$type == "test") test_variable(col, tg$test_col, tg$test) else col
        log <- c(log, list(list(
          dataset = ds, variable = where,
          detail = paste0("derived from ", paste(from, collapse = ", "), ": blanked where ", if (length(from) > 1) "one of them" else "it", " changed"),
          n = out$n
        )))
      }
    }
    data[[ds]] <- df
  }
  list(data = data, log = log)
}

# For rows where the quasi-identifier changed (`hit`): the row of the
# original data whose derived value the row takes (`from`, 0 for none), and
# which rows are blanked. `old`, `new`: the quasi-identifier before and
# after; `derived`: the derived variable before; `step`: the ladder step
# applied.
map_derived <- function(old, new, derived, hit, step) {
  n <- length(old)
  from <- integer(n)
  blank <- hit
  old_v <- blank_to_na(if (inherits(old, "haven_labelled")) vctrs::vec_data(old) else old)
  new_v <- blank_to_na(if (inherits(new, "haven_labelled")) vctrs::vec_data(new) else new)
  categories <- fold_categories(derived)
  d_id <- categories$id
  d_id[!categories$present] <- NA
  present <- !vctrs::vec_detect_missing(old_v)
  if (is.numeric(old_v) && !is.factor(old_v)) {
    # Intervals: each derived category's observed range of the
    # quasi-identifier, which must not overlap.
    ok <- present & !is.na(d_id)
    if (!any(ok) || !inherits(step, "anon_generalize_rule") || !rule_kind(step) %in% c("band", "round")) {
      return(list(from = from, blank = blank))
    }
    cats <- unique(d_id[ok])
    lo <- vapply(cats, function(c) min(old_v[ok & d_id == c]), numeric(1))
    hi <- vapply(cats, function(c) max(old_v[ok & d_id == c]), numeric(1))
    o <- order(lo)
    if (length(cats) > 1 && any(hi[o][-length(o)] >= lo[o][-1])) {
      return(list(from = from, blank = blank))
    }
    # The categories cover every value: the lowest is open below and the
    # highest above. Between two categories, where the cut lies isn't known.
    lo[o[1]] <- -Inf
    hi[o[length(o)]] <- Inf
    whole <- all(old_v[present] == round(old_v[present]))
    for (i in which(hit & !vctrs::vec_detect_missing(new_v))) {
      range <- released_range(new_v[i], step, whole)
      c <- which(lo <= range[1] & range[2] <= hi)
      if (length(c) == 1) {
        from[i] <- which(ok & d_id == cats[c])[1]
        blank[i] <- FALSE
      }
    }
    return(list(from = from, blank = blank))
  }
  # Text: the original data must map each value to one derived value.
  src <- as.character(old_v)
  pairs <- unique(data.frame(s = src[present], d = d_id[present], stringsAsFactors = FALSE))
  if (anyDuplicated(pairs$s)) {
    return(list(from = from, blank = blank))
  }
  target <- as.character(new_v)
  for (i in which(hit & !is.na(target))) {
    j <- which(present & src == target[i])[1]
    if (!is.na(j)) {
      from[i] <- j
      blank[i] <- FALSE
    }
  }
  list(from = from, blank = blank)
}

# The range of original values a released value can stand for under a band
# or rounding (whole numbers only when `whole`), as c(low, high), both
# included.
released_range <- function(v, step, whole) {
  p <- rule_params(step)
  if (rule_kind(step) == "round") {
    half <- 0.5 * 10^(-p$digits)
    lo <- v - half
    hi <- v + half
    if (whole) {
      lo <- ceiling(lo)
      hi <- floor(hi)
    }
    return(c(lo, hi))
  }
  w <- p$width
  lo <- if (p$value == "midpoint") v - w / 2 else v
  hi <- lo + w
  if (!is.null(p$top) && v >= p$top) {
    lo <- min(lo, p$top) - if (p$value == "midpoint") w else 0
    hi <- Inf
  }
  if (!is.null(p$bottom) && v <= p$bottom) {
    lo <- -Inf
    hi <- max(hi, p$bottom + w)
  }
  if (whole) {
    # The band excludes its upper bound.
    c(ceiling(lo), if (is.finite(hi)) ceiling(hi) - 1 else hi)
  } else {
    c(lo, hi)
  }
}

# Residual signals (see anon_deidentify()): for each, the subjects unique on
# it alone, in groups smaller than the target alone, and together with the
# released quasi-identifiers.
residual_signals <- function(profile, step, qis, specs, key, k_target) {
  data <- step$data
  keyed <- names(data)[vapply(data, function(df) key %in% names(df), logical(1))]
  codes <- NULL
  if (length(specs) > 0) {
    inputs <- tryCatch(suppressWarnings(risk_inputs(step, specs, key)), error = function(e) NULL)
    if (!is.null(inputs)) {
      keys <- inputs$table[[key]]
      codes <- suppress_codes(inputs$table[inputs$qnames], inputs$blanked)
    }
  }
  if (is.null(codes)) {
    keys <- subject_keys(data[keyed], key)
    codes <- matrix(0L, length(keys), 0)
  }
  out <- list()
  add <- function(signal, ds, values) {
    k_alone <- tabulate(vctrs::vec_group_id(values))[vctrs::vec_group_id(values)]
    with <- pattern_group_sizes(cbind(codes, vctrs::vec_group_id(values)))
    out[[length(out) + 1]] <<- tibble::tibble(
      signal = signal, dataset = ds, n_subjects = length(keys), n_unique = sum(k_alone == 1),
      n_below_k = sum(k_alone < k_target), n_below_k_with_quasi = sum(with < k_target)
    )
  }
  if (length(keys) == 0) {
    return(empty_signals())
  }
  # Each duration from the first dataset holding both dates (copies, such as
  # ADSL variables in a BDS dataset, give the same).
  for (i in which(profile$role == "duration")) {
    for (ds in keyed) {
      df <- data[[ds]]
      if (!profile_in_scope(profile$datasets[[i]], ds) || !all(c(profile$from[[i]], profile$variable[i]) %in% names(df))) next
      width <- rule_params(profile$ladder[[i]][[1]])$width
      spec <- anon_quasi_duration(ds, profile$from[[i]], profile$variable[i], width = width)
      values <- quasi_duration_by_subject(df, spec, key, keys)[[1]]
      add(paste0("duration ", profile$from[[i]], " to ", profile$variable[i], " (bands of ", format(width), " days)"), ds, values)
      break
    }
  }
  for (ds in keyed) {
    df <- data[[ds]]
    k <- normalize_keys(df[[key]])
    k <- k[!is.na(k)]
    if (length(k) > length(unique(k))) {
      add("rows per subject", ds, as.integer(table(factor(k, levels = unique(c(keys, k))))[keys]))
    }
    test_col <- names(df)[grepl("TESTCD$", names(df), useBytes = TRUE) | names(df) == "PARAMCD"][1]
    if (!is.na(test_col)) {
      tk <- normalize_keys(df[[key]])
      tests <- as.character(df[[test_col]])
      ok <- !is.na(tk) & !is.na(tests)
      sets <- tapply(tests[ok], tk[ok], function(t) paste(sort(unique(t)), collapse = "\u001f"))
      add(paste0("set of ", test_col), ds, unname(sets[keys]))
    }
  }
  if (length(out) == 0) {
    return(empty_signals())
  }
  do.call(rbind, out)
}

empty_signals <- function() {
  tibble::tibble(
    signal = character(0), dataset = character(0), n_subjects = integer(0), n_unique = integer(0),
    n_below_k = integer(0), n_below_k_with_quasi = integer(0)
  )
}

describe_levels <- function(qis) {
  if (length(qis) == 0) {
    return("no quasi-identifiers found")
  }
  paste(vapply(qis, function(q) {
    paste0(q$name, " ", if (isTRUE(q$constant)) "as is (one value)" else if (is.null(q$step)) "as is" else describe_ladder_step(q$step))
  }, character(1)), collapse = ", ")
}

describe_risk_summary <- function(r, k_target) {
  if (is.null(r)) {
    return("not measured")
  }
  s <- if (inherits(r, "anon_risk")) r$summary else r
  paste0("min k ", s$min_k, ", ", s$n_below_k, " subject(s) below ", k_target)
}

# The quasi-identifiers as recorded in the settings: where each is measured,
# its copies, and the ladder step chosen (no values).
quasi_settings <- function(qis) {
  if (length(qis) == 0) {
    return(tibble::tibble(
      quasi = character(0), source = character(0), copies = character(0), level = integer(0), step = character(0),
      constant = logical(0)
    ))
  }
  where <- function(cp) {
    if (cp$type == "column") paste0(cp$dataset, ".", cp$column) else paste0(cp$dataset, ".", test_variable(cp$result_col, cp$test_col, cp$test))
  }
  tibble::tibble(
    quasi = vapply(qis, `[[`, character(1), "name"),
    source = vapply(qis, function(q) where(q$copies[[q$source]]), character(1)),
    copies = vapply(qis, function(q) paste(vapply(q$copies, where, character(1)), collapse = ", "), character(1)),
    level = vapply(qis, function(q) as.integer(q$level), integer(1)),
    step = vapply(qis, function(q) if (is.null(q$step)) NA_character_ else describe_ladder_step(q$step), character(1)),
    constant = vapply(qis, function(q) isTRUE(q$constant), logical(1))
  )
}

search_moves_table <- function(search) {
  if (is.null(search) || length(search$moves) == 0) {
    return(tibble::tibble(
      quasi = character(0), level = integer(0), step = character(0), cells_before = numeric(0),
      cells_after = numeric(0), prop_after = numeric(0)
    ))
  }
  do.call(rbind, lapply(search$moves, function(m) {
    tibble::tibble(
      quasi = m$quasi, level = as.integer(m$level), step = m$step, cells_before = m$cells_before,
      cells_after = m$cells_after, prop_after = m$prop_after
    )
  }))
}
