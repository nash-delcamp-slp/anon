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
#'   times the measures of the quasi-identifiers that vary) that suppression
#'   may blank before the search stops generalizing; above it, the search
#'   climbs the ladders even when a step saves no cells. Default 0.05.
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
#'    datasets and of the data frames nested in them (packed and list
#'    columns, at any depth); the columns are kept, with their type and
#'    attributes, so code that uses them still runs, but without value
#'    labels or missing-value codes, which can hold the identifier. An exact
#'    `"remove"` name found nowhere is noted in the log.
#' 2. [anon_subject_ids()] replaces the key, `"also"` and `"key_cols"`
#'    columns. A dataset without the key whose rows an `"also"` column ties
#'    to subjects (see [anon_profile()]) takes the key for these steps, and
#'    is released without it.
#' 3. [anon_subject_dates()] offsets every date of each subject by one
#'    random offset (its own detection: Date and date-time columns, and text
#'    columns named `*DTC`, at any depth); text columns matched by the
#'    profile's `"date"` rules (such as `VISIT_DATE`, in the datasets with the
#'    key) are offset by the same offsets, as `*DTC` columns are: values that
#'    aren't ISO 8601 dates (`10MAR2021`, `03/10/2021`, `--03-10`) are
#'    removed, with a warning, and bare years move by whole years. So are
#'    those of the data frames nested in them, each row with the offset of
#'    the subject that owns it (rows no subject owns are blanked).
#' 4. The quasi-identifiers the profile finds are measured ([anon_risk()]).
#'    Each is measured from its source: the copy with one value per subject
#'    that holds a value for the most subjects. Every other copy that shows
#'    some subject another value than the source (such as `WEIGHTBL` from
#'    another visit than the `vs` baseline, or a copy holding subjects the
#'    source doesn't) is measured too, as a measure of its own: it is
#'    generalized at the quasi-identifier's level and suppressed like the
#'    others, so the released combination is what is measured. Copies are
#'    never overwritten with the source's values. A column copy with more
#'    than one value for a subject (such as `AAGE` across events) can't be
#'    measured: a warning names it, and the target is not met. A
#'    quasi-identifier with no copy that holds one value per subject can't
#'    be measured at all: a warning names it, it is released as it is, the
#'    variables derived from it are blanked, and the target is not met.
#' 5. **Ladder search** (greedy, as Datafly), on the measured values (one
#'    per subject and measure). The cost of a state is the cells
#'    suppression would blank, estimated with [anon_suppress()]'s algorithm
#'    (a subject it can't resolve counts as all its cells), plus every cell
#'    of a removed quasi-identifier. While above `max_suppressed` of the
#'    cells, or while a step lowers the cost, it takes one step up the
#'    ladder of the quasi-identifier whose next step costs least (ties: the
#'    step that loses the fewest distinct values, then profile order). When
#'    every ladder is exhausted above the budget, it keeps the state that
#'    cost least (the earliest on ties). Last, it steps back down wherever
#'    that doesn't raise the cost (most useful quasi-identifiers first), so
#'    steps taken on a plateau are undone. Each ladder step applies to the
#'    original values. A quasi-identifier with one value for every subject
#'    in every measure can't split a group: it is measured, but neither
#'    generalized nor suppressed.
#' 6. The chosen steps are applied to every copy of each quasi-identifier
#'    ([anon_generalize()], in every dataset with the key that the rule
#'    covers; `"remove"` blanks every copy in every row), then
#'    [anon_suppress()] blanks what remains in every measure, with
#'    `everywhere = TRUE` and the profile's priorities as `order` (they
#'    break ties). Cells blanked in a source are blanked for the same
#'    subjects in its copies that aren't measured on their own (copies
#'    under another name, and test copies such as the `WEIGHT` rows of
#'    `advs`).
#' 7. Derived variables are kept where they still follow from the released
#'    values of their quasi-identifiers, and blanked elsewhere (below). Value
#'    labels of generalized or suppressed columns keep only the values still
#'    released. The risk is measured again.
#'
#' **Nested data.** Columns of the data frames nested in a dataset with the
#' key (packed columns and the data frames in list columns, at any depth)
#' take the dataset's rules. Those of quasi-identifiers and derived
#' variables are measured and changed like datasets of their own, each row
#' with the subject that owns it (see [anon_subject_ids()]): a nested copy
#' of `AGE` is generalized and suppressed with it, and measured on its own
#' where it shows some subject another value. They are named after where
#' they are, such as `dm.extra` for packed column `extra` of `dm` and
#' `dm.visits[[]]` for the data frames in list column `visits`. Rows that no
#' subject owns are blanked in those columns, and logged.
#'
#' **Derived variables.** A derived variable is left as it is where no
#' ladder step applies to its quasi-identifiers and the subject's cells of
#' them weren't blanked. Otherwise (in every row, when a ladder step
#' applies; whether a value happened to stay the same, as an age on a
#' band's lower bound, doesn't matter), with one quasi-identifier in the
#' same row (such as `AGEGR1` with `AGE` in `adsl`):
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
#' in every row when a ladder step applies to one of them, and otherwise for
#' the subjects whose cells of one of them were blanked; for a result column
#' (`AVAL`, `--STRESN`), with the other results of the row. Blanking keeps
#' values truthful: a derived value recomputed from a representative (such
#' as a BMI from a rounded weight) would not be any subject's. `from` can
#' name the column of a copy (`AAGEGR1` from `AAGE`); a column copy named as
#' the quasi-identifier (`AGE` rather than `AAGE`) is preferred. A derived
#' value missing in the data stays missing. A derived variable whose `from`
#' names a quasi-identifier that isn't measured (not in the data, or with no
#' copy that can be measured, such as `BMIBL` without height and weight) is
#' blanked in every row, with a warning.
#'
#' **When the target is met.** `settings$deidentify$meets_target` is `TRUE`
#' only when the risk measured on the released data meets the target and
#' [anon_suppress()] found no other reason not to (copies of a
#' quasi-identifier that differ, birth dates next to `AGE`, values of removed
#' subjects left); reasons are in `settings$deidentify$not_met`. No
#' quasi-identifier found in the data is a reason too. Last, an audit walks
#' every dataset at every depth for what the steps left as it was, each
#' kind named in a warning and a reason: columns and supplemental
#' qualifiers that look like quasi-identifiers without a rule (see
#' [anon_profile()]); datasets without the key (and no `"also"` column that
#' ties their rows to subjects) holding columns the profile names or that
#' look like quasi-identifiers; profile date columns that weren't offset
#' (such as a number of days); and quasi-identifier or derived columns
#' holding lists.
#'
#' **Residual signals.** Meeting the target is about the quasi-identifiers
#' only. Other things a person might know single subjects out too: treatment
#' duration (the profile's `"duration"` rules, which offsetting doesn't
#' change; in days and in the rule's bands), the number of rows a subject
#' has in each dataset, the visits (`VISITNUM`, `AVISITN`) and study days
#' (`--DY`, `ADY`) it has, the set of tests (`--TESTCD`, `PARAMCD`) it has,
#' and, for each generalized test, the set of its released values across
#' all rows (such as weights at every visit, not only the measured
#' baseline). Columns that hold a duration exactly (`TRTDURD`, `ADURN`,
#' `--DUR`) are kept and not measured. `signals` counts, for each,
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
#'   (`settings$deidentify` for this function; `deidentify_1` and so on when
#'   it runs again on its own result), the record of suppressed
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

  # Rules for the ADaM companions (XN, XL, XC) of the IDs and quasi-identifiers.
  profile <- profile_with_companions(profile, key)

  # A single data frame goes through the steps as a list of one, named
  # "data", and comes back as a data frame.
  previous <- if (inherits(data, "anon_deid_step")) data
  single <- is.data.frame(if (is.null(previous)) data else previous$data)
  if (single) {
    if (is.null(previous)) data <- list(data = data) else previous$data <- list(data = previous$data)
  }
  input <- as_dataset_list(if (is.null(previous)) data else previous)
  d <- input$data

  # 1. Direct identifiers, at every depth.
  columns <- profile_columns(profile, d)
  also <- profile_role_columns(profile, d, columns, "also", key)
  key_cols <- profile_role_columns(profile, d, columns, "key_cols", key)
  # Datasets without the key whose rows map to subjects through an `also`
  # column take the key for the steps, and lose it at the end.
  mapped <- map_keyless_datasets(d, key, columns)
  d <- mapped$data
  keyed <- names(d)[vapply(d, function(df) key %in% names(df), logical(1))]
  gaps <- warn_profile_gaps(profile, d, columns, key)
  log <- deid_log_start(
    "remove", length(subject_keys(d[keyed], key)), key, NULL,
    settings = paste0(
      "direct identifiers blanked in every row, nested data included; columns kept with their type and ",
      "attributes, without value labels or missing-value codes"
    )
  )
  removed <- remove_identifiers(profile, d)
  d <- removed$data
  for (row in removed$log) {
    log <- deid_log_add(log, row$dataset, row$variable, row$detail, row$n, 0)
  }
  for (i in seq_along(mapped$added)) {
    log <- deid_log_add(
      log, mapped$added[i], mapped$through[i],
      paste0("rows tied to subjects through `", mapped$through[i], "` (no `", key, "` column): processed as a dataset with it"),
      nrow_of(d[[mapped$added[i]]]), 0
    )
  }
  step <- new_deid_step(
    input$unwrap(d), log, NULL, previous = input$previous,
    settings = list(remove = list(columns = removed$columns, not_found = removed$not_found))
  )

  # 2-3. Subjects and dates. The offsets are kept for the profile's own date
  # columns, and dropped afterwards unless `keep_secrets`.
  step <- anon_subject_ids(step, key = key, also = also, key_cols = key_cols, keep_secrets = keep_secrets, seed = seed)
  extra_dates <- profile_extra_dates(profile, step$data, key)
  step <- with_extra_dates_muffled(extra_dates, anon_subject_dates(
    step, key = key, keep_secrets = keep_secrets || length(extra_dates) > 0,
    seed = if (!is.null(seed)) seed + 1, also = also
  ))

  # Quasi-identifier, derived, companion and profile text date columns of
  # data frames nested in the datasets are measured and changed as datasets
  # of their own, and written back at the end. The profile carries the
  # datasets they come from, for the rules' `datasets`.
  lifted <- lift_nested_quasi(profile, step$data, key)
  attr(profile, "scope") <- lifted$scope
  step$data <- lifted$data
  if (length(extra_dates) > 0) {
    step <- shift_extra_dates(step, lifted_extra_dates(profile, step$data, key, lifted), key, keep_secrets)
  }
  base <- step

  # 4. Quasi-identifiers and the risk before: each measured from its source
  # and from every copy that shows some subject another value (or holds
  # subjects the source doesn't).
  qis <- profile_quasi(profile, base$data, key)
  # A quasi-identifier with no copy that holds one value per subject can't
  # be measured at all.
  unmeasurable <- vapply(qis, function(q) is.na(q$source), logical(1))
  unmeasured_q <- unlist(lapply(qis[unmeasurable], function(q) {
    paste0(q$name, " (", paste(vapply(q$copies, copy_where, character(1)), collapse = ", "), ")")
  }))
  qis <- qis[!unmeasurable]
  if (length(unmeasured_q) > 0) {
    warning(
      "These quasi-identifiers have no copy with one value per subject and can't be measured: ",
      paste(unmeasured_q, collapse = "; "), ". They are released as they are, and the variables derived ",
      "from them are blanked.",
      call. = FALSE
    )
  }
  measures <- quasi_measures(qis, base$data, key)
  unmeasured <- attr(measures, "unmeasured")
  if (length(unmeasured) > 0) {
    warning(
      "These copies of quasi-identifiers hold more than one value for a subject and can't be measured: ",
      paste(unmeasured, collapse = ", "), ". They are generalized and blanked with their quasi-identifier; ",
      "the risk they add isn't measured.",
      call. = FALSE
    )
  }
  specs <- measure_specs(measures)
  mq <- vapply(measures, `[[`, integer(1), "q")
  risk_before <- NULL
  search <- NULL
  qvar <- character(0)
  rank <- integer(0)
  if (length(qis) > 0) {
    risk_before <- anon_risk(base, specs, key = key, max_risk = max_risk)
    inputs <- suppressWarnings(risk_inputs(base, specs, key))
    qvar <- inputs$qnames
    rank <- quasi_rank(qis)[mq]
    # A quasi-identifier with one value for every subject in every measure
    # can't split any group: it is measured, but neither generalized nor
    # suppressed.
    column_constant <- vapply(qvar, function(v) vctrs::vec_unique_count(inputs$table[[v]]) <= 1, logical(1))
    for (q in seq_along(qis)) {
      qis[[q]]$constant <- all(column_constant[mq == q])
    }
    constant <- vapply(qis, `[[`, logical(1), "constant")

    # 5. Ladder search.
    searched <- which(!constant)
    if (length(searched) > 0) {
      cols <- which(mq %in% searched)
      search <- ladder_search(
        inputs$table, qvar[cols], match(mq[cols], searched), qis[searched], k_target, rank(rank[cols]), max_suppressed
      )
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

  # 6. Generalize every copy, then suppress every measure.
  rules <- generalize_copy_rules(qis[!removed_q])
  gen <- if (length(rules) > 0) anon_generalize(base, rules, key = key, keep_secrets = keep_secrets) else base
  active <- which(!removed_q[mq])
  suppressed_m <- which(!removed_q[mq] & !constant_q[mq])
  sup <- gen
  if (length(suppressed_m) > 0) {
    sup <- anon_suppress(
      gen, specs[suppressed_m], key = key, max_risk = max_risk, order = qvar[suppressed_m][order(rank[suppressed_m])],
      unresolved = unresolved, key_cols = key_cols, also = also, everywhere = TRUE
    )
  }

  # 7. Copies, removed quasi-identifiers and derived variables.
  d <- sup$data
  final_keys <- subject_keys(Filter(function(df) key %in% names(df), d), key)
  dropped <- setdiff(subject_keys(Filter(function(df) key %in% names(df), base$data), key), final_keys)
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
  # Cells blanked in a source are blanked in the copies that aren't measured
  # on their own (those are suppressed as measures).
  for (qi in which(!removed_q)) {
    q <- qis[[qi]]
    keys <- source_blanked_keys(sup$suppressed, q$copies[[q$source]])
    if (length(keys) == 0) next
    measured <- vapply(measures[mq == qi], `[[`, integer(1), "copy")
    for (ci in setdiff(seq_along(q$copies), measured)) {
      copy <- q$copies[[ci]]
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
  record <- if (length(record) > 0) do.call(rbind, record)
  touched <- quasi_touched(qis, rbind(sup$suppressed, record))
  aligned <- lapply(base$data, function(df) drop_subject_rows(df, key, key_cols, dropped)$df)
  derived <- derive_columns(profile, qis, aligned, d, key, touched)
  d <- drop_unused_labels(derived$data, qis, touched)
  for (row in derived$log) {
    log <- deid_log_add(log, row$dataset, row$variable, row$detail, row$n, 0)
  }
  final <- new_deid_step(d, log, NULL, previous = sup, settings = list(), suppressed = record)

  # The risk after, the reasons the target isn't met and residual signals.
  not_met <- if (!is.null(sup$settings$suppress) && length(suppressed_m) > 0) {
    setdiff(sup$settings[[last_setting(sup$settings, "suppress")]]$not_met, "measured risk above the target")
  } else {
    character(0)
  }
  if (length(unmeasured) > 0 || length(unmeasured_q) > 0) {
    not_met <- c(not_met, "copies of quasi-identifiers that vary within a subject aren't measured")
  }
  not_met <- c(not_met, gaps)
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
  signals <- residual_signals(profile, final, qis, specs[active], key, k_target, skip = names(lifted$scope))
  lowered <- lower_nested_quasi(final$data, lifted, key, key_cols, dropped)
  final$data <- lowered$data
  for (row in lowered$log) {
    final$log <- rbind(final$log, tibble::tibble(
      step = "deidentify", dataset = row$dataset, variable = row$variable, n_changed = as.integer(row$n),
      n_removed = 0L, detail = row$detail
    ))
  }
  # What the released data still holds that the profile names, or that looks
  # like a quasi-identifier, and that no step processed.
  not_met <- c(not_met, audit_release(profile, final$data, key, k_target))
  for (ds in mapped$added) {
    final$data[[ds]] <- final$data[[ds]][setdiff(names(final$data[[ds]]), key)]
  }

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
  settings_name <- utils::tail(make.unique(c(names(final$settings), "deidentify"), sep = "_"), 1)
  final$settings[[settings_name]] <- list(
    key = key,
    profile = attr(profile, "profile_name"),
    profile_rules = profile_text(profile),
    max_risk = max_risk, k_target = k_target, max_suppressed = max_suppressed, unresolved = unresolved,
    also = also, key_cols = key_cols,
    quasi = quasi_settings(qis, measures),
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

# The profile with rules for the companions (see anon_profile()) of the key,
# of the "also" columns and of the quasi-identifier columns: for each exact
# name X of such a rule (and `key`), XN, XL and XC, in the datasets of X's
# rule, are "also" for the key and "also" columns and derived from X for a
# quasi-identifier. The companion rules lose to every other rule that
# matches a column, in the datasets that rule covers (see profile_columns());
# names that a rule for every dataset matches get none.
profile_with_companions <- function(profile, key) {
  plain <- which(is.na(profile$test_col) & lengths(profile$datasets) == 0)
  ruled <- function(v) any(vapply(plain, function(i) profile_match(profile$variable[i], v), logical(1)))
  rules <- list()
  add <- function(x, role, datasets, from = character(0)) {
    names <- paste0(x, c("N", "L", "C"))
    names <- names[!vapply(names, ruled, logical(1))]
    if (length(names) > 0) {
      rules[[length(rules) + 1]] <<- unclass_profile(anon_profile_rule(names, role, datasets = datasets, from = from))
    }
  }
  if (!ruled(key)) add(key, "also", character(0))
  for (i in which(is.na(profile$test_col))) {
    x <- profile$variable[i]
    if (profile_is_pattern(x)) next
    if (profile$role[i] %in% c("key", "also")) {
      add(x, "also", profile$datasets[[i]])
    } else if (profile$role[i] == "quasi") {
      add(x, "derived", profile$datasets[[i]], from = x)
    }
  }
  if (length(rules) == 0) {
    return(profile)
  }
  extra <- do.call(rbind, rules)
  extra$profile <- "companions"
  new_profile(rbind(unclass_profile(profile), extra), attr(profile, "profile_name"))
}

# The rule (by row of the profile) that applies to each column of each
# dataset, with its role: among the rules (other than test rules) whose
# variable and datasets match, one limited to datasets wins over one that
# isn't, an exact name over a pattern, and a later rule over an earlier one;
# a companion rule (see profile_with_companions()) loses to every other.
profile_columns <- function(profile, data) {
  plain <- which(is.na(profile$test_col))
  scope <- attr(profile, "scope")
  companion <- profile$profile %in% "companions"
  out <- list()
  for (ds in names(data)) {
    cols <- names(data[[ds]])
    if (length(cols) == 0) next
    best <- rep(NA_integer_, length(cols))
    score <- rep(-Inf, length(cols))
    for (i in plain) {
      if (!profile_in_scope(profile$datasets[[i]], ds, scope)) next
      hit <- profile_match(profile$variable[i], cols)
      s <- 2 * (length(profile$datasets[[i]]) > 0) + !profile_is_pattern(profile$variable[i]) - 10 * companion[i]
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

# Datasets without the key whose rows map to subjects through an `also`
# column (such as `SUBJID`, or `USUBJIDN`): every row holds a value that the
# datasets with the key give exactly one subject. They get the key column,
# last, so that the steps process them as datasets with the key (the
# `also` column is replaced, dates are offset, quasi-identifiers measured
# and generalized), and anon_deidentify() removes it at the end. A dataset
# without such a column, or with a row whose value is missing, ties to no
# subject or to several, or whose `also` columns disagree, is left as it is
# (the final audit names it). Returns the data, the datasets given the key
# (`added`) and the column each was tied through (`through`).
map_keyless_datasets <- function(data, key, columns) {
  out <- list(data = data, added = character(0), through = character(0))
  keyed <- names(data)[vapply(data, function(df) key %in% names(df), logical(1))]
  if (length(keyed) == 0) {
    return(out)
  }
  plain_key <- function(x) {
    x <- zap_user_missing(x)
    if (is.factor(x)) as.character(x) else vctrs::vec_data(x)
  }
  key_values <- tryCatch(vctrs::vec_c(!!!unname(lapply(data[keyed], function(df) plain_key(df[[key]])))), error = function(e) NULL)
  if (is.null(key_values)) {
    return(out)
  }
  key_text <- normalize_keys(key_values)
  for (ds in setdiff(names(data), keyed)) {
    df <- data[[ds]]
    candidates <- columns$column[columns$dataset == ds & columns$role == "also"]
    if (nrow_of(df) == 0 || length(candidates) == 0) next
    found <- list()
    for (a in candidates) {
      x <- df[[a]]
      if (is.list(x)) next
      held <- unlist(lapply(data[keyed], function(k) {
        if (a %in% names(k) && !is.list(k[[a]])) normalize_keys(zap_user_missing(k[[a]])) else rep(NA_character_, nrow_of(k))
      }), use.names = FALSE)
      ok <- !is.na(held) & !is.na(key_text)
      pairs <- vctrs::vec_unique(data.frame(v = held[ok], k = key_text[ok]))
      ambiguous <- pairs$v[duplicated(pairs$v)]
      v <- normalize_keys(zap_user_missing(x))
      at <- which(ok)[match(v, held[ok])]
      if (anyNA(at) || any(v %in% ambiguous)) next
      found[[a]] <- at
    }
    if (length(found) == 0) next
    subjects <- lapply(found, function(at) key_text[at])
    if (!all(vapply(subjects, identical, logical(1), subjects[[1]]))) next
    df[[key]] <- vctrs::vec_slice(key_values, found[[1]])
    out$data[[ds]] <- df
    out$added <- c(out$added, ds)
    out$through <- c(out$through, names(found)[1])
  }
  out
}

# The final audit of anon_deidentify(): every dataset, at every depth
# (nested_tables()), checked for what the profile names, or what looks like
# a quasi-identifier, that the steps didn't process. Each kind found is
# named in a warning and returned as a reason the target is not met:
# - in a dataset without the key column, any column that a `quasi` or
#   `derived` rule matches, or that looks like a quasi-identifier without a
#   rule (see look_alike_names()), since only datasets with the key are
#   measured; and, in its data frames whose rows no subject owns (see
#   nested_tables(): the subject steps process the others), any column of
#   a `key`, `also`, `key_cols`, `date` or `duration` rule;
# - in a dataset with the key, a column that looks like a quasi-identifier
#   without a rule, unless it holds one value that at least `k_target`
#   subjects share (or none);
# - a column of a `date` or `duration` rule that wasn't offset: one that
#   isn't a Date, a date-time or text (a number of days, say, whose origin
#   isn't known), text other than *DTC outside the datasets with the key, or
#   a list column holding text;
# - a `quasi` or `derived` column holding a list (not data frames, whose
#   columns are walked), a column of the `key` rule other than the key (in
#   another case), and a `remove` column not blank.
audit_release <- function(profile, data, key, k_target) {
  found <- list(keyless = character(0), look = character(0), dates = character(0), left = character(0))
  ids <- c("key", "also", "key_cols", "date", "duration")
  for (ds in names(data)) {
    df <- data[[ds]]
    has_key <- key %in% names(df)
    if (!has_key && nrow_of(df) == 0) next
    tables <- nested_tables(df, key)
    row_keys <- if (has_key) table_row_keys(tables)
    keyless <- character(0)
    for (t in seq_along(tables)) {
      tb <- tables[[t]]
      f <- tb$df
      cols <- names(f)
      if (length(cols) == 0) next
      ruled <- profile_columns(profile, stats::setNames(list(f), ds))
      role <- ruled$role[match(cols, ruled$column)]
      where <- paste0(ds, ".", if (t == 1) cols else paste0(tb$path, "[", cols, "]"))
      # Columns of data frames (packed, or in list columns) are tables of
      # their own.
      frames <- vapply(f, function(x) is.data.frame(x) || (is.list(x) && length(x) > 0 && holds_only_frames(x)), logical(1))
      look <- is.na(role) & look_alike_names(cols) & !frames & cols != key
      if (!has_key) {
        keyless <- c(keyless, where[(role %in% c("quasi", "derived") | look | (!tb$keyed & role %in% ids)) & !frames])
        if (!tb$keyed) next
      } else {
        for (j in which(look)) {
          if (!shared_constant(f[[j]], row_keys[[t]], k_target)) found$look <- c(found$look, where[j])
        }
      }
      for (j in which(role %in% c("date", "duration") & !frames & cols != key)) {
        if (!offset_date_column(f[[j]], cols[j], has_key)) found$dates <- c(found$dates, where[j])
      }
      lists <- vapply(f, is.list, logical(1)) & !frames
      left <- (has_key & role %in% c("quasi", "derived") & lists) | (role %in% "key" & cols != key) |
        (role %in% "remove" & !vapply(f, function(x) is.list(x) || all(vctrs::vec_detect_missing(blank_to_na(vctrs::vec_data(x)))), logical(1)))
      found$left <- c(found$left, where[left])
    }
    if (length(keyless) > 0) {
      found$keyless <- c(found$keyless, paste0(ds, " (", paste(unique(sub("^[^.]*[.]", "", keyless)), collapse = ", "), ")"))
    }
  }
  if (length(found$keyless) > 0) {
    warning(
      "These datasets have no `", key, "` column, and no `also` column that ties every row to a subject, but hold ",
      "identifiers, quasi-identifiers, dates or columns that look like them: ", paste(found$keyless, collapse = "; "),
      ". They are released as they are: add the key, or remove them.",
      call. = FALSE
    )
  }
  if (length(found$look) > 0) {
    warning(
      "These columns look like quasi-identifiers or birth dates but no profile rule matches them: ",
      paste(found$look, collapse = ", "), ". Add rules for them (as quasi-identifiers, derived or removed), ",
      "or keep them knowingly.",
      call. = FALSE
    )
  }
  if (length(found$dates) > 0) {
    warning(
      "These columns are dates in the profile but were not offset (only Date, date-time and text columns are): ",
      paste(found$dates, collapse = ", "), ". Convert them to Date, or remove them.",
      call. = FALSE
    )
  }
  if (length(found$left) > 0) {
    warning(
      "These columns match a profile rule but were left as they are (a quasi-identifier or derived variable ",
      "holding a list, the key named in another case, or a direct identifier not blank): ",
      paste(found$left, collapse = ", "), ".",
      call. = FALSE
    )
  }
  c(
    if (length(found$keyless) > 0) "datasets without the key hold identifiers, quasi-identifiers or dates",
    if (length(found$look) > 0) "columns that look like quasi-identifiers have no profile rule",
    if (length(found$dates) > 0) "profile date columns were not offset",
    if (length(found$left) > 0) "columns that match a profile rule were left as they are"
  )
}

# Whether every element of a list is a data frame (or NULL).
holds_only_frames <- function(x) {
  all(vapply(x, function(e) is.null(e) || is.data.frame(e), logical(1)))
}

# Each table's rows' subjects (normalized keys, missing for rows that no
# subject owns), for tables made by nested_tables() of a dataset with the key.
table_row_keys <- function(tables) {
  keys <- vector("list", length(tables))
  keys[[1]] <- tables[[1]]$owners
  for (t in seq_along(tables)[-1]) {
    tb <- tables[[t]]
    keys[[t]] <- if (!tb$keyed) rep(NA_character_, nrow_of(tb$df)) else if (tb$owns) tb$owners else keys[[tb$parent]][tb$parent_row]
  }
  keys
}

# Whether a column without a rule that looks like a quasi-identifier can be
# released: it holds no value, or one value (missing values count as one)
# that at least `k_target` subjects share.
shared_constant <- function(x, keys, k_target) {
  if (is.list(x)) {
    return(FALSE)
  }
  v <- blank_to_na(vctrs::vec_data(x))
  if (all(vctrs::vec_detect_missing(v))) {
    return(TRUE)
  }
  vctrs::vec_unique_count(v) == 1 && length(unique(keys[!is.na(keys)])) >= k_target
}

# Whether the dates step or anon_deidentify() offset a column of a `date`
# rule in rows that subjects own: Date and date-time columns, text named
# *DTC, and, in a dataset with the key (`keyed`), the profile's other text
# dates, at any depth. A number (of days, say) isn't: its origin isn't
# known. A column with no value has nothing to offset.
offset_date_column <- function(x, col, keyed) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(TRUE)
  }
  if (is.list(x)) {
    text <- list_column_content(x)$text
    return(all(is.na(blank_to_na(text))))
  }
  (is_text_date_type(x) && (keyed || grepl("DTC$", col, ignore.case = TRUE, useBytes = TRUE))) ||
    all(vctrs::vec_detect_missing(blank_to_na(vctrs::vec_data(x))))
}

is_text_date_type <- function(x) {
  is.character(x) || is.factor(x) || (inherits(x, "haven_labelled") && is.character(vctrs::vec_data(x)))
}

# Blank the columns of `remove` rules in every row, in the datasets and in
# the data frames nested in them (packed columns and list columns, at any
# depth; a nested column takes the rules of its dataset), dropping their
# value labels, missing-value codes and factor levels, which can hold the
# identifier. The log has one row per column (nested ones by path), and one
# per exact `remove` name found nowhere.
remove_identifiers <- function(profile, data) {
  log <- list()
  found <- character(0)
  removed <- character(0)
  for (ds in names(data)) {
    data[[ds]] <- map_frames(data[[ds]], function(f, path) {
      cols <- profile_columns(profile, stats::setNames(list(f), ds))
      for (col in cols$column[cols$role == "remove"]) {
        out <- blank_rows(f, col, seq_len(nrow_of(f)))
        f <- out$df
        x <- f[[col]]
        if (any(c("labels", "na_values", "na_range") %in% names(attributes(x))) || is.factor(x)) {
          attr(x, "labels") <- NULL
          attr(x, "na_values") <- NULL
          attr(x, "na_range") <- NULL
          f <- set_column(f, match(col, names(f)), drop_unused_levels(x))
        }
        where <- paste0(path, if (nzchar(path)) "[" else "", col, if (nzchar(path)) "]" else "")
        found <<- c(found, col)
        removed <<- c(removed, paste0(ds, ".", where))
        log <<- c(log, list(list(dataset = ds, variable = where, detail = "blanked in every row (direct identifier)", n = out$n)))
      }
      f
    })
  }
  wanted <- profile$variable[profile$role == "remove" & !profile_is_pattern(profile$variable)]
  not_found <- setdiff(unique(wanted), found)
  for (v in not_found) {
    log <- c(log, list(list(dataset = NA_character_, variable = v, detail = "direct identifier in the profile, not found in the data", n = 0L)))
  }
  list(data = data, log = log, columns = removed, not_found = not_found)
}

# Apply `fn(frame, path)` to a data frame and to every data frame nested in
# it: packed columns (path "col") and the data frames in list columns (path
# "col[[]]"), also inside plain lists, at any depth.
map_frames <- function(df, fn, path = "") {
  df <- fn(df, path)
  for (j in seq_along(df)) {
    col <- .subset2(df, j)
    name <- names(df)[j]
    inner <- paste0(path, if (nzchar(path)) "[" else "", name, if (nzchar(path)) "]" else "")
    new <- if (is.data.frame(col)) {
      map_frames(col, fn, inner)
    } else if (is.list(col) && !isS4(col) && !inherits(col, "POSIXlt")) {
      map_list_frames_all(col, fn, paste0(inner, "[[]]"))
    } else {
      col
    }
    if (!identical(new, col)) {
      df <- set_column(df, j, new)
    }
  }
  df
}

map_list_frames_all <- function(x, fn, path) {
  for (i in seq_along(x)) {
    e <- x[[i]]
    new <- if (is.data.frame(e)) {
      map_frames(e, fn, path)
    } else if (is.list(e) && is.null(oldClass(e))) {
      map_list_frames_all(e, fn, path)
    } else {
      e
    }
    if (!identical(new, e)) {
      x[[i]] <- new
    }
  }
  x
}

# Data frames nested in the datasets with the key (packed and list columns,
# at any depth, see nested_tables()) that hold columns with a
# quasi-identifier or derived rule, or profile text dates (see
# extra_date_column(); the rules of their dataset), are added to `data` as
# datasets of their own, so that they are measured, generalized,
# suppressed, derived and offset like the others: the key of each row's subject,
# those columns and the row's position in its nested table
# (`.anon_nested_row`), named after where they are (`dm.extra`,
# `dm.visits[[]]`). Rows that no subject of the dataset owns are left out
# (and blanked by lower_nested_quasi()). `scope` maps each added dataset to its dataset, for
# the rules' `datasets`; `tables` records, for lower_nested_quasi(), each
# nested table's columns and the dataset it went to (none when no row has a
# subject); `base` holds the datasets as they were.
lift_nested_quasi <- function(profile, data, key) {
  row_col <- ".anon_nested_row"
  out <- list(data = data, scope = character(0), tables = list(), base = list())
  for (ds in names(data)) {
    df <- data[[ds]]
    if (!key %in% names(df) || !any(vapply(df, is.list, logical(1)))) next
    tables <- nested_tables(df, key)
    if (length(tables) < 2) next
    keys <- vector("list", length(tables))
    keys[[1]] <- tables[[1]]$owners
    for (t in seq_along(tables)[-1]) {
      tb <- tables[[t]]
      n <- nrow_of(tb$df)
      keys[[t]] <- if (!tb$keyed) rep(NA_character_, n) else if (tb$owns) tb$owners else keys[[tb$parent]][tb$parent_row]
      ruled <- profile_columns(profile, stats::setNames(list(tb$df), ds))
      cols <- setdiff(ruled$column[ruled$role %in% c("quasi", "derived")], key)
      cols <- cols[!vapply(cols, function(col) is.list(tb$df[[col]]), logical(1))]
      dates <- ruled$column[ruled$role %in% c("date", "duration")]
      cols <- c(cols, setdiff(dates[vapply(dates, function(col) extra_date_column(tb$df[[col]], col, key), logical(1))], cols))
      if (length(cols) == 0) next
      # Each row's key as the dataset holds it (same type in every dataset).
      at <- match(keys[[t]], tables[[1]]$owners, incomparables = NA)
      owned <- which(!is.na(at))
      name <- NULL
      if (length(owned) > 0) {
        name <- make.unique(c(names(out$data), paste0(ds, ".", tb$path)), sep = "_")[length(out$data) + 1]
        lifted <- c(
          stats::setNames(list(vctrs::vec_slice(df[[key]], at[owned])), key),
          lapply(stats::setNames(cols, cols), function(col) vctrs::vec_slice(tb$df[[col]], owned)),
          stats::setNames(list(owned), row_col)
        )
        out$data[[name]] <- vctrs::new_data_frame(lifted, n = length(owned))
        out$scope[[name]] <- ds
      }
      out$tables <- c(out$tables, list(list(
        dataset = ds, table = t, path = tb$path, top = tb$top, columns = cols, lifted = name, n_unowned = n - length(owned)
      )))
      out$base[[ds]] <- df
    }
  }
  out
}

# Write the released values of the lifted data frames (see
# lift_nested_quasi()) back where they came from, and remove the lifted
# datasets. Rows of a nested table that no subject owns are blanked in its
# lifted columns; rows of subjects removed from the data are removed from
# the nested data as anon_suppress() removes them. The log has a row for each
# nested table with blanked rows that no subject owns.
lower_nested_quasi <- function(data, lifted, key, key_cols, dropped) {
  log <- list()
  for (ds in unique(vapply(lifted$tables, `[[`, character(1), "dataset"))) {
    tables <- nested_tables(lifted$base[[ds]], key)
    entries <- Filter(function(e) e$dataset == ds, lifted$tables)
    for (e in entries) {
      tb <- tables[[e$table]]
      n <- nrow_of(tb$df)
      at <- if (is.null(e$lifted)) rep(NA_integer_, n) else match(seq_len(n), data[[e$lifted]]$.anon_nested_row)
      for (col in e$columns) {
        j <- match(col, names(tb$df))
        if (!is.null(e$lifted)) {
          tb$df <- set_column(tb$df, j, vctrs::vec_slice(data[[e$lifted]][[col]], at))
        }
        blanked <- blank_rows(tb$df, col, which(is.na(at)))
        tb$df <- blanked$df
        tb$changed <- union(tb$changed, j)
        if (e$n_unowned > 0) {
          log <- c(log, list(list(
            dataset = ds, variable = paste0(e$path, "[", col, "]"), n = e$n_unowned,
            detail = paste0("blanked in ", e$n_unowned, " nested row(s) that no subject owns (no key ties them to a subject)")
          )))
        }
      }
      tables[[e$table]] <- tb
    }
    written <- drop_subject_rows(assemble_tables(tables), key, key_cols, dropped)$df
    df <- data[[ds]]
    for (top in unique(vapply(entries, `[[`, character(1), "top"))) {
      df <- set_column(df, match(top, names(df)), written[[top]])
    }
    data[[ds]] <- df
  }
  data[names(lifted$scope)] <- NULL
  list(data = data, log = log)
}

# Whether a column of a `date` or `duration` rule is one of the profile's
# text dates that anon_deidentify() offsets itself: text not named *DTC
# (those the dates step offsets), other than the key.
extra_date_column <- function(x, col, key) {
  col != key && !grepl("DTC$", col, ignore.case = TRUE, useBytes = TRUE) && is_text_date_type(x)
}

# The profile's text dates (see extra_date_column()) in the datasets with
# the key, at every depth: each with its dataset, column and `where`, as
# the dates step names it in its warning about unshifted dates (nested
# columns by position, "dm.visits[[]][2]").
profile_extra_dates <- function(profile, data, key) {
  out <- list()
  for (ds in names(data)) {
    df <- data[[ds]]
    if (!key %in% names(df)) next
    tables <- nested_tables(df, key, positional = TRUE)
    for (t in seq_along(tables)) {
      tb <- tables[[t]]
      ruled <- profile_columns(profile, stats::setNames(list(tb$df), ds))
      for (col in ruled$column[ruled$role %in% c("date", "duration")]) {
        if (!extra_date_column(tb$df[[col]], col, key)) next
        where <- paste0(ds, ".", if (t == 1) col else table_column_path(tb, match(col, names(tb$df)), positional = TRUE))
        out <- c(out, list(list(dataset = ds, column = col, where = where)))
      }
    }
  }
  out
}

# The profile's text dates in the datasets once nested data is lifted (see
# lift_nested_quasi()): those of the datasets, and those of the lifted data
# frames, logged where they are nested ("visits[[]][VISIT_DATE]" in "dm").
lifted_extra_dates <- function(profile, data, key, lifted) {
  out <- list()
  for (ds in names(data)) {
    df <- data[[ds]]
    if (!key %in% names(df)) next
    ruled <- profile_columns(profile, stats::setNames(list(df), ds))
    entry <- Filter(function(e) identical(e$lifted, ds), lifted$tables)
    for (col in ruled$column[ruled$role %in% c("date", "duration")]) {
      if (!extra_date_column(df[[col]], col, key)) next
      out <- c(out, list(if (length(entry) > 0) {
        list(dataset = ds, column = col, log_dataset = entry[[1]]$dataset, log_variable = paste0(entry[[1]]$path, "[", col, "]"))
      } else {
        list(dataset = ds, column = col, log_dataset = ds, log_variable = col)
      }))
    }
  }
  out
}

# Run `expr` (the dates step) with its warning about unshifted date columns
# left without the columns that anon_deidentify() offsets itself.
with_extra_dates_muffled <- function(extra, expr) {
  if (length(extra) == 0) {
    return(expr)
  }
  ours <- vapply(extra, `[[`, character(1), "where")
  prefix <- "These columns contain dates that were not shifted: "
  withCallingHandlers(expr, warning = function(w) {
    m <- conditionMessage(w)
    if (!startsWith(m, prefix)) return()
    listed <- sub("\\. If they hold ISO 8601 dates.*$", "", substring(m, nchar(prefix) + 1))
    rest <- setdiff(strsplit(listed, ", ", fixed = TRUE)[[1]], ours)
    if (length(rest) > 0) {
      warning(paste0(prefix, paste(rest, collapse = ", "), substring(m, nchar(prefix) + nchar(listed) + 1)), call. = FALSE)
    }
    invokeRestart("muffleWarning")
  })
}

# Offset the profile's extra date columns (see lifted_extra_dates()) by each
# subject's offset, read from the dates step's secrets, which are then
# dropped unless `keep_secrets` (secrets of earlier steps are kept). As in
# *DTC columns, values that aren't ISO 8601 dates are removed and bare years
# move by whole years.
shift_extra_dates <- function(step, extra, key, keep_secrets) {
  secrets <- anon_secrets(step)
  offsets_name <- utils::tail(grep("^date_offsets", names(secrets), value = TRUE), 1)
  offsets <- secrets[[offsets_name]]
  log <- step$log
  n_invalid <- 0
  for (e in extra) {
    df <- step$data[[e$dataset]]
    k <- normalize_keys(df[[key]])
    at <- match(k, as.character(offsets$key))
    x <- df[[e$column]]
    text <- if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else as.character(x)
    shifted <- shift_dates_by_subject(
      x, offsets$offset_days[at], "shift", keep_non_dates = FALSE, keep_years = FALSE,
      missing = if (is.character(x) || is.factor(x)) blank_text_value(text),
      year_sign = offsets$year_direction[at]
    )
    n_invalid <- n_invalid + shifted$invalid
    df[[e$column]] <- shifted$values
    step$data[[e$dataset]] <- df
    log <- rbind(log, tibble::tibble(
      step = "subject_dates", dataset = e$log_dataset, variable = e$log_variable, n_changed = as.integer(shifted$shifted),
      n_removed = as.integer(shifted$no_key + shifted$invalid + shifted$partial),
      detail = paste0(shift_detail(shifted), " (a date in the profile)")
    ))
  }
  step$log <- log
  name <- last_setting(step$settings, "subject_dates")
  step$settings[[name]]$profile_dates <- vapply(extra, function(e) paste0(e$log_dataset, ".", e$log_variable), character(1))
  if (!isTRUE(keep_secrets)) {
    rm(list = offsets_name, envir = step$secrets)
    if (length(ls(step$secrets)) == 0) step$secrets <- NULL
    step$settings[[name]]$keep_secrets <- FALSE
  }
  if (n_invalid > 0) {
    warning(
      n_invalid, " value(s) of date columns in the profile are not recognised ISO 8601 dates, or would fall ",
      "outside years 0001-9999, and were removed.",
      call. = FALSE
    )
  }
  step
}

# Whether column names look like a quasi-identifier or a birth date: an age,
# a birth date, a site, a numeric ID, a country, a sex or gender, a race, an
# ethnicity, a height, a weight, a BMI, a body surface area or a renal
# function, in any case; units (`*U`) aside.
look_alike_names <- function(x) {
  looks <- paste0(
    "^(A?AGE[A-Z0-9]*|.*BRTH.*|.*BIRTH.*|SITE[A-Z0-9]*|.*IDN|COUNTRY.*|SEX.*|GENDER.*|RACE.*|ETHNIC.*|",
    "HEIGHT.*|HGT.*|HT|HTBL|WEIGHT.*|WGT.*|WT|WTBL|BMI.*|BSA.*|CRCL.*|EGFR.*)$"
  )
  grepl(looks, x, ignore.case = TRUE, useBytes = TRUE) & !grepl("U$", x, ignore.case = TRUE, useBytes = TRUE)
}

# Warnings about what the profile may have missed (columns that look like
# quasi-identifiers without a rule are found by audit_release()): `keep`
# rules that win over a quasi-identifier rule for a column; and QNAM values
# of supplemental qualifier datasets that look like quasi-identifiers (see
# look_alike_names()) and that no test rule covers, whose QVAL values are
# released as they are, so they are returned as a reason the target is not
# met.
warn_profile_gaps <- function(profile, data, columns, key) {
  shadowed <- character(0)
  quasi <- which(profile$role == "quasi" & is.na(profile$test_col))
  for (ds in names(data)) {
    df <- data[[ds]]
    if (!key %in% names(df)) next
    kept <- columns$column[columns$dataset == ds & columns$role == "keep"]
    for (col in kept) {
      if (any(vapply(quasi, function(i) profile_in_scope(profile$datasets[[i]], ds, attr(profile, "scope")) && profile_match(profile$variable[i], col), logical(1)))) {
        shadowed <- c(shadowed, paste0(ds, ".", col))
      }
    }
  }
  if (length(shadowed) > 0) {
    warning(
      "These columns are kept by a `keep` rule although a quasi-identifier rule matches them too: ",
      paste(shadowed, collapse = ", "), ". They are released as they are and not measured.",
      call. = FALSE
    )
  }
  qnam_rules <- which(profile$role %in% c("quasi", "derived") & profile$test_col %in% "QNAM")
  supp <- character(0)
  for (ds in names(data)) {
    df <- data[[ds]]
    if (!all(c("QNAM", "QVAL", key) %in% names(df))) next
    qnams <- unique(as.character(df$QNAM))
    qnams <- qnams[!is.na(qnams) & look_alike_names(qnams)]
    covered <- vapply(qnams, function(v) {
      any(vapply(qnam_rules, function(i) profile_in_scope(profile$datasets[[i]], ds, attr(profile, "scope")) && profile_match(profile$test[i], v), logical(1)))
    }, logical(1))
    if (any(!covered)) supp <- c(supp, paste0(ds, " (", paste(qnams[!covered], collapse = ", "), ")"))
  }
  if (length(supp) > 0) {
    warning(
      "These supplemental qualifiers look like quasi-identifiers but no profile rule covers them: ",
      paste(supp, collapse = "; "), ". Their QVAL values are released as they are.",
      call. = FALSE
    )
  }
  if (length(supp) > 0) "supplemental qualifiers that look like quasi-identifiers have no profile rule"
}

# The quasi-identifiers found in the data: each with its copies (columns
# and test results, in the order of the profile's rules and then of the
# datasets; a test pattern gives a copy per test code found), its ladder
# and priority (from the rule of its source), in the order of its first
# rule. The source is the copy with one value per subject that has a value
# for the most subjects (the first on ties).
profile_quasi <- function(profile, data, key) {
  keyed <- names(data)[vapply(data, function(df) key %in% names(df), logical(1))]
  columns <- profile_columns(profile, data[keyed])
  # A column holding a list (or a packed data frame, whose columns are
  # lifted) can't be measured or generalized: audit_release() names it.
  columns <- columns[columns$role == "quasi" & !held_lists(data, columns), , drop = FALSE]
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
      if (!profile_in_scope(profile$datasets[[i]], ds, attr(profile, "scope")) || !profile$test_col[i] %in% names(df)) next
      result <- names(df)[profile_match(profile$variable[i], names(df))][1]
      codes <- unique(as.character(df[[profile$test_col[i]]]))
      codes <- codes[!is.na(codes) & profile_match(profile$test[i], codes)]
      if (is.na(result)) next
      for (code in sort(codes)) {
        copies <- c(copies, list(list(
          type = "test", dataset = ds, test_col = profile$test_col[i], test = code, result_col = result,
          flag = if (!is.na(profile$flag[i]) && profile$flag[i] %in% names(df)) profile$flag[i] else NA_character_,
          rule = i, name = profile$quasi_name[i]
        )))
      }
    }
  }
  if (length(copies) == 0) {
    return(list())
  }
  names_found <- unique(vapply(copies, `[[`, character(1), "name"))
  lapply(names_found, function(name) {
    mine <- Filter(function(cp) cp$name == name, copies)
    held <- vapply(mine, function(cp) {
      df <- data[[cp$dataset]]
      if (cp$type == "column" && !one_value_per_subject(df, cp$column, key)) {
        return(-1)
      }
      rows <- copy_rows(df, cp)
      values <- blank_to_na(vctrs::vec_data(vctrs::vec_slice(df[[copy_column(cp)]], rows)))
      keys <- normalize_keys(df[[key]])[rows]
      as.numeric(length(unique(keys[!is.na(keys) & !is.na(values)])))
    }, numeric(1))
    # No copy with one value per subject: the quasi-identifier can't be
    # measured (source NA).
    source <- if (any(held >= 0)) which(held == max(held))[1] else NA_integer_
    rule <- mine[[if (is.na(source)) 1L else source]]$rule
    list(
      name = name, copies = mine, source = source, ladder = profile$ladder[[rule]], priority = profile$priority[rule],
      first_rule = min(vapply(mine, `[[`, integer(1), "rule")), level = 0L, step = NULL, constant = FALSE
    )
  })
}

# For each row of a profile_columns() table, whether its column holds a list.
held_lists <- function(data, columns) {
  vapply(seq_len(nrow(columns)), function(j) is.list(data[[columns$dataset[j]]][[columns$column[j]]]), logical(1))
}

one_value_per_subject <- function(df, col, key) {
  pairs <- vctrs::vec_unique(data.frame(k = normalize_keys(df[[key]]), v = blank_to_na(vctrs::vec_data(df[[col]]))))
  !anyDuplicated(pairs$k[!is.na(pairs$k)])
}

# The measure of a copy: a column, or a test's baseline value (see
# baseline_rows()), named `name`.
copy_spec <- function(cp, key, name) {
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
  anon_quasi_values(cp$dataset, cp$test_col, cp$test, cp$result_col, filter = filter, names = name)
}

# What anon_deidentify() measures: for each quasi-identifier, its source, and
# every other copy that shows some subject a value other than the source's
# (or holds subjects the source doesn't), each as a measure of its own,
# generalized at the quasi-identifier's level and suppressed like the others.
# Copies are compared on the subjects they hold (rows with the key; for a
# test, rows of the test). A column copy with more than one value for a
# subject can't be measured: it is listed in the attribute "unmeasured".
quasi_measures <- function(qis, data, key) {
  out <- list()
  unmeasured <- character(0)
  value_of <- function(cp) {
    df <- data[[cp$dataset]]
    rows <- copy_rows(df, cp)
    if (cp$type == "test" && !is.na(cp$flag)) {
      tests <- baseline_rows(df[[cp$flag]], df[[key]], df[[cp$test_col]])
      rows <- intersect(rows, which(tests))
    }
    keys <- normalize_keys(df[[key]])[rows]
    v <- vctrs::vec_slice(df[[copy_column(cp)]], rows)
    v <- blank_to_na(if (is.factor(v)) as.character(v) else vctrs::vec_data(v))
    text <- if (is.numeric(v)) key_as_character(v) else as.character(v)
    ok <- !is.na(keys)
    first <- ok & !duplicated(ifelse(ok, keys, NA_character_), incomparables = NA)
    stats::setNames(text[first], keys[first])
  }
  for (qi in seq_along(qis)) {
    q <- qis[[qi]]
    src <- q$copies[[q$source]]
    out <- c(out, list(list(q = qi, copy = q$source, spec = copy_spec(src, key, q$name))))
    reference <- value_of(src)
    for (ci in setdiff(seq_along(q$copies), q$source)) {
      cp <- q$copies[[ci]]
      if (cp$type == "column" && !one_value_per_subject(data[[cp$dataset]], cp$column, key)) {
        unmeasured <- c(unmeasured, paste0(cp$dataset, ".", cp$column))
        next
      }
      mine <- value_of(cp)
      theirs <- reference[names(mine)]
      differs <- (is.na(mine) != is.na(theirs)) | (!is.na(mine) & !is.na(theirs) & mine != theirs)
      if (any(differs)) {
        name <- if (cp$type == "column") cp$column else paste0(q$name, " (", cp$dataset, " ", cp$result_col, " ", cp$test, ")")
        out <- c(out, list(list(q = qi, copy = ci, spec = copy_spec(cp, key, name))))
      }
    }
  }
  attr(out, "unmeasured") <- unmeasured
  out
}

# The measures' specifications, as anon_risk() takes them: a column named by
# its dataset, or a test.
measure_specs <- function(measures) {
  do.call(c, c(list(list()), lapply(measures, function(m) {
    if (inherits(m$spec, "anon_quasi_values")) list(m$spec) else m$spec
  })))
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
# measured values (one per subject): `table`, its measure columns `qvar`,
# the quasi-identifier of each (`mq`, positions in `qis`) and each column's
# rank. A quasi-identifier's level applies to all its measures.
ladder_search <- function(table, qvar, mq, qis, k_target, rank, max_suppressed) {
  keys <- table[[1]]
  n <- nrow(table)
  nq <- length(qis)
  ncols <- length(qvar)
  cache <- list()
  values_at <- function(m, level) {
    id <- paste(m, level)
    if (is.null(cache[[id]])) {
      x <- table[[qvar[m]]]
      cache[[id]] <<- if (level == 0) {
        x
      } else {
        step <- qis[[mq[m]]]$ladder[[level]]
        if (identical(step, "remove")) vctrs::vec_init(x, n) else step(x, subjects = keys)
      }
    }
    cache[[id]]
  }
  # Ladder steps that don't apply to the values of a measure (such as a fold
  # to text on a numeric column) are left out, with a warning.
  skipped <- character(0)
  for (q in seq_len(nq)) {
    ok <- vapply(seq_along(qis[[q]]$ladder), function(level) {
      all(vapply(which(mq == q), function(m) !inherits(tryCatch(values_at(m, level), error = function(e) e), "error"), logical(1)))
    }, logical(1))
    if (!all(ok)) {
      skipped <- c(skipped, paste0(qis[[q]]$name, ": ", paste(vapply(qis[[q]]$ladder[!ok], describe_ladder_step, character(1)), collapse = ", ")))
      qis[[q]]$ladder <- qis[[q]]$ladder[ok]
      cache <- cache[!vapply(strsplit(names(cache), " "), function(x) mq[as.integer(x[1])] == q, logical(1))]
    }
  }
  if (length(skipped) > 0) {
    warning(
      "These ladder steps don't apply to the values of their quasi-identifier and were left out: ",
      paste(skipped, collapse = "; "), ".",
      call. = FALSE
    )
  }
  # The cells a state costs: those suppression blanks, every cell of a
  # subject it can't resolve, and every cell of a removed quasi-identifier.
  estimate <- function(levels) {
    tab <- tibble::as_tibble(stats::setNames(lapply(seq_len(ncols), function(m) values_at(m, levels[mq[m]])), qvar))
    res <- suppress_cells(suppress_codes(tab, matrix(FALSE, n, ncols)), k_target, rank)
    resolved <- setdiff(seq_len(n), res$still)
    removed <- vapply(seq_len(ncols), function(m) {
      level <- levels[mq[m]]
      level > 0 && identical(qis[[mq[m]]]$ladder[[level]], "remove")
    }, logical(1))
    cells <- sum(res$blanked[resolved, !removed, drop = FALSE]) + sum(!removed) * length(res$still) + n * sum(removed)
    list(cells = cells, prop = if (n * ncols > 0) cells / (n * ncols) else 0, n_unresolved = length(res$still))
  }
  source_col <- match(seq_len(nq), mq)
  distinct <- function(q, level) {
    x <- values_at(source_col[q], level)
    vctrs::vec_unique_count(x[!vctrs::vec_detect_missing(x)])
  }
  describe_state <- function(e) paste0(e$cells, " (", format(round(100 * e$prop, 1), nsmall = 1), "% of ", n * ncols, ")")
  depth <- vapply(qis, function(q) length(q$ladder), integer(1))
  first <- vapply(qis, `[[`, integer(1), "first_rule")
  levels <- rep(0L, nq)
  current <- estimate(levels)
  visited <- list(list(levels = levels, estimate = current))
  moves <- list()
  # Climb while above the budget, or while a step lowers the cells needed.
  repeat {
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
    if (current$prop <= max_suppressed && best$estimate$cells >= current$cells) break
    q <- best$q
    moves <- c(moves, list(list(
      quasi = qis[[q]]$name, level = best$levels[q], step = describe_ladder_step(qis[[q]]$ladder[[best$levels[q]]]),
      cells_before = current$cells, cells_after = best$estimate$cells, prop_after = best$estimate$prop,
      detail = paste0(
        "ladder: ", qis[[q]]$name, " to step ", best$levels[q], " (", describe_ladder_step(qis[[q]]$ladder[[best$levels[q]]]),
        "); distinct values ", distinct(q, levels[q]), " to ", distinct(q, best$levels[q]),
        "; estimated cells to blank ", current$cells, " to ", describe_state(best$estimate)
      )
    )))
    levels <- best$levels
    current <- best$estimate
    visited <- c(visited, list(list(levels = levels, estimate = current)))
  }
  exhausted <- current$prop > max_suppressed
  if (exhausted) {
    # Every ladder is exhausted: keep the state that needed the fewest cells
    # (the earliest on ties).
    pick <- which.min(vapply(visited, function(v) v$estimate$cells, numeric(1)))
    if (pick < length(visited)) {
      kept <- visited[[pick]]
      moves <- c(moves, list(list(
        quasi = NA_character_, level = NA_integer_, step = NA_character_, cells_before = current$cells,
        cells_after = kept$estimate$cells, prop_after = kept$estimate$prop,
        detail = paste0(
          "ladders exhausted above max_suppressed; back to the levels after move ", pick - 1,
          ", which needed the fewest cells (", kept$estimate$cells, ")"
        )
      )))
      levels <- kept$levels
      current <- kept$estimate
    }
  }
  # Steps back down where a step isn't needed: one taken on a plateau, or
  # made unneeded by a later step, is undone when the cells needed don't
  # grow; the most useful quasi-identifiers first.
  q_rank <- vapply(seq_len(nq), function(q) min(rank[mq == q]), numeric(1))
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (q in order(-q_rank)) {
      while (levels[q] > 0) {
        lower <- levels
        lower[q] <- levels[q] - 1L
        e <- estimate(lower)
        if (e$cells > current$cells) break
        moves <- c(moves, list(list(
          quasi = qis[[q]]$name, level = lower[q],
          step = if (lower[q] == 0) NA_character_ else describe_ladder_step(qis[[q]]$ladder[[lower[q]]]),
          cells_before = current$cells, cells_after = e$cells, prop_after = e$prop,
          detail = paste0(
            "ladder: ", qis[[q]]$name, " back to ",
            if (lower[q] == 0) "its original values" else paste0("step ", lower[q], " (", describe_ladder_step(qis[[q]]$ladder[[lower[q]]]), ")"),
            ", not needed; estimated cells to blank ", current$cells, " to ", describe_state(e)
          )
        )))
        levels <- lower
        current <- e
        changed <- TRUE
      }
    }
  }
  list(
    levels = levels, ladders = lapply(qis, `[[`, "ladder"), estimate = current, moves = moves,
    exhausted = current$prop > max_suppressed
  )
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
# every row of a test with its other results. A factor column blanked in
# every row keeps no levels, which would name the removed values.
blank_copy <- function(df, cp, key, keys) {
  rows <- if (is.null(keys)) rep(TRUE, nrow_of(df)) else normalize_keys(df[[key]]) %in% keys
  if (cp$type == "column") {
    out <- blank_rows(df, cp$column, which(rows))
    if (is.null(keys) && is.factor(out$df[[cp$column]])) {
      out$df <- set_column(out$df, match(cp$column, names(out$df)), drop_unused_levels(out$df[[cp$column]]))
    }
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

# A factor without the levels that none of its values has, keeping its other
# attributes.
drop_unused_levels <- function(x) {
  if (!is.factor(x)) {
    return(x)
  }
  used <- levels(x)[levels(x) %in% as.character(x)]
  out <- match(as.character(x), used)
  attributes(out) <- attributes(x)
  attr(out, "levels") <- used
  out
}

# Rows of a copy: every row for a column, the test's rows for a test.
copy_rows <- function(df, cp) {
  if (cp$type == "column") seq_len(nrow_of(df)) else which(test_rows(df, cp))
}

copy_column <- function(cp) if (cp$type == "column") cp$column else cp$result_col

# Value labels of quasi-identifier columns that were generalized or
# suppressed keep only the entries whose values are still in the column: a
# label of a value no longer released would name it.
drop_unused_labels <- function(data, qis, touched) {
  for (qi in seq_along(qis)) {
    if (!isTRUE(touched[[qi]]$all) && length(touched[[qi]]$keys) == 0) next
    for (cp in qis[[qi]]$copies) {
      if (cp$type != "column") next
      x <- data[[cp$dataset]][[cp$column]]
      labels <- attr(x, "labels", exact = TRUE)
      if (is.null(labels)) next
      keep <- vctrs::vec_data(labels) %in% vctrs::vec_data(x)
      if (all(keep)) next
      attr(x, "labels") <- labels[keep]
      data[[cp$dataset]] <- set_column(data[[cp$dataset]], match(cp$column, names(data[[cp$dataset]])), x)
    }
  }
  data
}

# For each quasi-identifier, whether a ladder step applies to it (`all`: then
# every row is generalized or removed) and the subjects (keys) whose cells
# were blanked in any of its copies, from the record of suppressed cells.
quasi_touched <- function(qis, mask) {
  stats::setNames(lapply(qis, function(q) {
    keys <- unique(unlist(lapply(q$copies, function(cp) source_blanked_keys(mask, cp)), use.names = FALSE))
    list(all = !is.null(q$step), keys = keys)
  }), vapply(qis, `[[`, character(1), "name"))
}

# Derived variables (see anon_deidentify()): kept where no ladder step
# applies to their quasi-identifiers and the subject's cells weren't
# blanked; elsewhere mapped from a single quasi-identifier in the same row
# when that is exact, else blanked. `from` names quasi-identifiers or the
# column of one of their copies (such as AAGE); a derived variable with one
# that isn't measured is blanked in every row, with a warning.
derive_columns <- function(profile, qis, before, data, key, touched) {
  log <- list()
  no_source <- character(0)
  qnames <- vapply(qis, `[[`, character(1), "name")
  keyed <- names(data)[vapply(data, function(df) key %in% names(df), logical(1))]
  columns <- profile_columns(profile, data[keyed])
  targets <- list()
  for (j in which(columns$role == "derived" & !held_lists(data, columns))) {
    targets <- c(targets, list(list(
      dataset = columns$dataset[j], column = columns$column[j], rule = columns$rule[j], type = "column"
    )))
  }
  for (i in which(profile$role == "derived" & !is.na(profile$test_col))) {
    for (ds in keyed) {
      df <- data[[ds]]
      if (!profile_in_scope(profile$datasets[[i]], ds, attr(profile, "scope")) || !profile$test_col[i] %in% names(df)) next
      for (col in names(df)[profile_match(profile$variable[i], names(df))]) {
        targets <- c(targets, list(list(
          dataset = ds, column = col, rule = i, type = "test", test_col = profile$test_col[i], test = profile$test[i]
        )))
      }
    }
  }
  # Each `from` entry as a quasi-identifier, and the column of the copy it
  # names (NA for the quasi-identifier itself).
  resolve <- function(f) {
    if (f %in% qnames) {
      return(list(q = match(f, qnames), column = NA_character_, label = f))
    }
    for (qi in seq_along(qis)) {
      for (cp in qis[[qi]]$copies) {
        if (cp$type == "column" && cp$column == f) return(list(q = qi, column = f, label = f))
      }
    }
    NULL
  }
  for (tg in targets) {
    ds <- tg$dataset
    df <- data[[ds]]
    old_df <- before[[ds]]
    rows <- if (tg$type == "column") seq_len(nrow_of(df)) else which(test_rows(df, tg))
    if (length(rows) == 0) next
    from <- lapply(profile$from[[tg$rule]], resolve)
    absent <- profile$from[[tg$rule]][vapply(from, is.null, logical(1))]
    if (length(absent) > 0) {
      # A quasi-identifier it is derived from isn't measured (not in the
      # data, or with no copy that can be measured), so nothing tells whether
      # the derived value still follows from released values: it is blanked
      # in every row.
      target_cols <- if (tg$type == "test") result_siblings(df, tg$column) else tg$column
      for (col in target_cols) {
        out <- blank_rows(df, col, rows)
        df <- out$df
        where <- if (tg$type == "test") test_variable(col, tg$test_col, tg$test) else col
        no_source <- c(no_source, paste0(ds, ".", where))
        log <- c(log, list(list(
          dataset = ds, variable = where, n = out$n,
          detail = paste0("derived from ", paste(absent, collapse = ", "), ", not measured: blanked in every row")
        )))
      }
      data[[ds]] <- df
      next
    }
    labels <- vapply(from, `[[`, character(1), "label")
    # A copy of each quasi-identifier in the same rows, if there is one: for
    # a column, the copy named as the quasi-identifier (AGE rather than AAGE)
    # first.
    row_copy <- lapply(from, function(f) {
      copies <- qis[[f$q]]$copies
      named <- vapply(copies, function(cp) cp$type == "column" && cp$column == qis[[f$q]]$name, logical(1))
      for (cp in copies[order(!named)]) {
        if (cp$dataset != ds) next
        if (tg$type == "column" && cp$type == "column" && (is.na(f$column) || cp$column == f$column)) return(cp)
        if (tg$type == "test" && cp$type == "test" && cp$test_col == tg$test_col && cp$test == tg$test) return(cp)
      }
      NULL
    })
    keys <- normalize_keys(df[[key]])[rows]
    hit <- rep(FALSE, length(rows))
    for (f in from) {
      t <- touched[[f$q]]
      hit <- hit | if (isTRUE(t$all)) rep(TRUE, length(rows)) else keys %in% t$keys
    }
    if (!any(hit)) next
    target_cols <- if (tg$type == "test") result_siblings(df, tg$column) else tg$column
    if (length(from) == 1 && !is.null(row_copy[[1]]) && length(target_cols) == 1) {
      q <- qis[[from[[1]]$q]]
      cp <- row_copy[[1]]
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
        "derived from ", labels, ": ", sum(hit & !mapped$blank), " row(s) where ", labels,
        " was generalized or suppressed keep a value that follows from it; ", out$n, " blanked"
      )
      log <- c(log, list(list(dataset = ds, variable = where, detail = detail, n = out$n)))
    } else {
      for (col in target_cols) {
        out <- blank_rows(df, col, rows[hit])
        df <- out$df
        where <- if (tg$type == "test") test_variable(col, tg$test_col, tg$test) else col
        log <- c(log, list(list(
          dataset = ds, variable = where,
          detail = paste0(
            "derived from ", paste(labels, collapse = ", "), ": blanked where ",
            if (length(from) > 1) "one of them" else "it", " was generalized or suppressed"
          ),
          n = out$n
        )))
      }
    }
    data[[ds]] <- df
  }
  if (length(no_source) > 0) {
    warning(
      "These derived variables come from quasi-identifiers that aren't measured (not found in the data, or ",
      "with no copy that holds one value per subject) and were blanked in every row: ",
      paste(unique(no_source), collapse = ", "), ".",
      call. = FALSE
    )
  }
  list(data = data, log = log)
}

# For rows where the quasi-identifier changed (`hit`): the row of the
# original data whose derived value the row takes (`from`, 0 for none), and
# which rows are blanked. Only rows that had a derived value take one; a
# missing derived value stays missing. `old`, `new`: the quasi-identifier before and
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
    # Each category's first row, and each distinct released value's category.
    first_row <- match(cats, ifelse(ok, d_id, NA))
    rows <- which(hit & !vctrs::vec_detect_missing(new_v) & categories$present)
    values <- unique(new_v[rows])
    category <- vapply(values, function(v) {
      range <- released_range(v, step, whole)
      c <- which(lo <= range[1] & range[2] <= hi)
      if (length(c) == 1) c else NA_integer_
    }, integer(1))
    c <- category[match(new_v[rows], values)]
    from[rows[!is.na(c)]] <- first_row[c[!is.na(c)]]
    blank[rows[!is.na(c)]] <- FALSE
    return(list(from = from, blank = blank))
  }
  # Text: the original data must map each value to one derived value (rows
  # without a derived value aside).
  src <- as.character(old_v)
  mapped <- present & !is.na(d_id)
  pairs <- unique(data.frame(s = src[mapped], d = d_id[mapped], stringsAsFactors = FALSE))
  if (anyDuplicated(pairs$s)) {
    return(list(from = from, blank = blank))
  }
  target <- as.character(new_v)
  rows <- which(hit & !is.na(target) & categories$present)
  j <- match(target[rows], ifelse(mapped, src, NA))
  from[rows[!is.na(j)]] <- j[!is.na(j)]
  blank[rows[!is.na(j)]] <- FALSE
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
# released quasi-identifiers. Datasets in `skip` (nested data measured as
# datasets) give no signals of their own.
residual_signals <- function(profile, step, qis, specs, key, k_target, skip = character(0)) {
  data <- step$data
  keyed <- setdiff(names(data)[vapply(data, function(df) key %in% names(df), logical(1))], skip)
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
  # ADSL variables in a BDS dataset, give the same), in days and in the
  # rule's bands.
  for (i in which(profile$role == "duration")) {
    for (ds in keyed) {
      df <- data[[ds]]
      if (!profile_in_scope(profile$datasets[[i]], ds, attr(profile, "scope")) || !all(c(profile$from[[i]], profile$variable[i]) %in% names(df))) next
      for (width in unique(c(1, rule_params(profile$ladder[[i]][[1]])$width))) {
        spec <- anon_quasi_duration(ds, profile$from[[i]], profile$variable[i], width = width)
        values <- quasi_duration_by_subject(df, spec, key, keys)[[1]]
        add(paste0(
          "duration ", profile$from[[i]], " to ", profile$variable[i],
          if (width == 1) " (days)" else paste0(" (bands of ", format(width), " days)")
        ), ds, values)
      }
      break
    }
  }
  # The set of released values of each generalized test across all its rows
  # (not only the measured one), per subject.
  for (q in qis) {
    if (is.null(q$step) || identical(q$step, "remove")) next
    for (cp in q$copies) {
      if (cp$type != "test") next
      df <- data[[cp$dataset]]
      rows <- copy_rows(df, cp)
      if (length(rows) == 0) next
      add(paste0("set of ", q$name, " values across rows (", cp$result_col, " where ", cp$test_col, " = ", cp$test, ")"),
          cp$dataset, row_sets(df, rows, copy_column(cp), key, keys))
    }
  }
  for (ds in keyed) {
    df <- data[[ds]]
    k <- normalize_keys(df[[key]])
    k <- k[!is.na(k)]
    if (length(k) > length(unique(k))) {
      add("rows per subject", ds, as.integer(table(factor(k, levels = unique(c(keys, k))))[keys]))
    }
    # Visits and study days a subject has.
    for (col in names(df)[names(df) %in% c("VISITNUM", "AVISITN", "ADY") | grepl("^[A-Z]{2}DY$", names(df), useBytes = TRUE)]) {
      if (length(k) > length(unique(k))) {
        add(paste0("set of ", col), ds, row_sets(df, seq_len(nrow_of(df)), col, key, keys))
      }
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

# Each subject's set of the values of `col` in `rows` (blanks as "<blank>"),
# by `keys`.
row_sets <- function(df, rows, col, key, keys) {
  k <- normalize_keys(df[[key]])[rows]
  v <- blank_to_na(vctrs::vec_data(vctrs::vec_slice(df[[col]], rows)))
  v <- ifelse(is.na(v), "<blank>", if (is.numeric(v)) key_as_character(v) else as.character(v))
  ok <- !is.na(k)
  sets <- tapply(v[ok], k[ok], function(x) paste(sort(unique(x)), collapse = "\u001f"))
  unname(sets[keys])
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
quasi_settings <- function(qis, measures) {
  if (length(qis) == 0) {
    return(tibble::tibble(
      quasi = character(0), source = character(0), copies = character(0), measured = character(0),
      level = integer(0), step = character(0), constant = logical(0)
    ))
  }
  where <- copy_where
  tibble::tibble(
    quasi = vapply(qis, `[[`, character(1), "name"),
    source = vapply(qis, function(q) where(q$copies[[q$source]]), character(1)),
    copies = vapply(qis, function(q) paste(vapply(q$copies, where, character(1)), collapse = ", "), character(1)),
    # The copies measured on their own, besides the source.
    measured = vapply(seq_along(qis), function(qi) {
      own <- Filter(function(m) m$q == qi && m$copy != qis[[qi]]$source, measures)
      paste(vapply(own, function(m) where(qis[[qi]]$copies[[m$copy]]), character(1)), collapse = ", ")
    }, character(1)),
    level = vapply(qis, function(q) as.integer(q$level), integer(1)),
    step = vapply(qis, function(q) if (is.null(q$step)) NA_character_ else describe_ladder_step(q$step), character(1)),
    constant = vapply(qis, function(q) isTRUE(q$constant), logical(1))
  )
}

# Where a copy is, as text: "dataset.column", or the test's result column.
copy_where <- function(cp) {
  if (cp$type == "column") paste0(cp$dataset, ".", cp$column) else paste0(cp$dataset, ".", test_variable(cp$result_col, cp$test_col, cp$test))
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
