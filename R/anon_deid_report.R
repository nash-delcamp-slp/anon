# The De-identification section of anon_report() and anon_prompt_bundle() --
#
# What a de-identification step changed, for the agent that reads the copy:
# caveats, risk, settings, the step log and residual signals, and the
# fidelity table if given. Counts, names and rule parameters only: never data
# values, pseudonym maps, offsets or original keys (the step's secrets are
# not read).

# The steps among the objects a report describes: `envir` itself, or the
# steps it holds (in a list or an environment), named as the report names
# them. With `selection`, the steps selected by name or through one of
# their datasets (`step$dm`).
report_deid_steps <- function(envir, selection = NULL) {
  if (inherits(envir, "anon_deid_step")) {
    return(list(step = envir))
  }
  objects <- if (is.environment(envir)) {
    mget(ls(envir = envir), envir = envir)
  } else if (is.list(envir)) {
    envir
  } else {
    list()
  }
  if (length(objects) == 0) {
    return(list())
  }
  if (is.null(names(objects))) names(objects) <- paste0("x", seq_along(objects))
  missing_names <- is.na(names(objects)) | names(objects) == ""
  names(objects)[missing_names] <- paste0("x", which(missing_names))
  steps <- Filter(function(x) inherits(x, "anon_deid_step"), objects)
  if (!is.null(selection)) {
    picked <- vapply(names(steps), function(name) {
      any(selection == name | startsWith(selection, paste0(name, "$")))
    }, logical(1))
    steps <- steps[picked]
  }
  steps
}

# The section for one step.
deid_section <- function(step, fidelity = NULL) {
  s <- step$settings
  deid <- if (!is.null(s$deidentify)) s[[last_setting(s, "deidentify")]]
  log <- tibble::as_tibble(step$log[c("step", "dataset", "variable", "n_changed", "n_removed", "detail")])
  signals <- if (!is.null(deid)) deid$signals else step$signals
  out <- list(
    caveats = deid_caveats(step, deid, fidelity),
    risk = deid_risk_table(deid),
    meets_target = if (!is.null(deid)) isTRUE(deid$meets_target) else NA,
    not_met = if (!is.null(deid)) as.character(deid$not_met) else character(0),
    settings = deid_settings_table(s, deid),
    levels = deid_levels_table(s, deid),
    profile_rules = if (!is.null(deid$profile_rules)) tibble::as_tibble(deid$profile_rules),
    signals = if (!is.null(signals)) tibble::as_tibble(signals),
    steps = log,
    fidelity = if (!is.null(fidelity)) tibble::as_tibble(as.data.frame(fidelity))
  )
  Filter(Negate(is.null), out)
}

deid_risk_table <- function(deid) {
  if (is.null(deid)) {
    return(NULL)
  }
  row <- function(when, r) {
    if (is.null(r)) {
      return(NULL)
    }
    tibble::tibble(
      measured = when, n_subjects = r$n_subjects, min_k = r$min_k, k_target = r$k_target,
      n_below_k = r$n_below_k, max_risk = r$max_risk, max_risk_target = r$max_risk_target,
      meets_target = r$meets_target
    )
  }
  out <- rbind(row("before", deid$risk_before), row("after", deid$risk_after))
  out
}

# The step's rule parameters, one row per setting, as text.
deid_settings_table <- function(s, deid) {
  pick <- function(kind) s[names(s) == kind | startsWith(names(s), paste0(kind, "_"))]
  rows <- list()
  add <- function(step, setting, value) {
    rows[[length(rows) + 1]] <<- tibble::tibble(step = step, setting = setting, value = paste(value, collapse = ", "))
  }
  if (!is.null(deid)) {
    add("deidentify", "profile", deid$profile)
    add("deidentify", "max_risk", format(deid$max_risk, digits = 7))
    add("deidentify", "groups of at least", deid$k_target)
    add("deidentify", "max_suppressed", format(deid$max_suppressed, digits = 7))
    add("deidentify", "unresolved", deid$unresolved)
    add("deidentify", "subjects dropped", deid$n_dropped)
  }
  for (x in pick("remove")) {
    if (length(x$columns) > 0) add("remove", "blanked in every row", x$columns)
  }
  for (x in pick("subject_ids")) {
    add("subject_ids", "pseudonyms", paste0("prefix \"", x$prefix, "\" for ", paste(c(x$key, x$also, x$key_cols), collapse = ", ")))
  }
  for (x in pick("subject_dates")) {
    add("subject_dates", "offsets", paste0(
      "one per subject, ", x$min_days, " to ", x$max_days, " days earlier or later; partial dates: ", x$partial
    ))
  }
  for (x in pick("generalize")) {
    for (r in x$rules) add("generalize", deid_rule_where(r), deid_rule_text(r))
  }
  for (x in pick("suppress")) {
    add("suppress", "target", paste0(
      "max_risk ", format(x$max_risk, digits = 7), " (groups of at least ", x$k_target, "), method ", x$method,
      ", unresolved ", x$unresolved, ", subjects dropped ", x$n_dropped
    ))
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}

# Where a generalization rule from a step's settings applies ("dm.AGE",
# "vs.VSSTRESN (VSTESTCD = HEIGHT)"), and the rule as the call that makes it.
deid_rule_where <- function(r) {
  if (!is.null(r$column)) {
    paste0(r$dataset, ".", r$column)
  } else {
    paste0(r$dataset, ".", r$result_col, " (", r$test_col, " = ", r$test, ")")
  }
}

deid_rule_text <- function(r) {
  params <- r[setdiff(names(r), c("dataset", "column", "test_col", "test", "result_col", "rule"))]
  if (identical(params$value, "lower")) {
    params$value <- NULL
  }
  args <- vapply(names(params), function(n) paste0(n, " = ", format_rule_value(params[[n]])), character(1))
  paste0("anon_", r$rule, "(", paste(args, collapse = ", "), ")")
}

# The level each quasi-identifier was released at.
deid_levels_table <- function(s, deid) {
  if (!is.null(deid) && !is.null(deid$quasi) && nrow(deid$quasi) > 0) {
    q <- deid$quasi
    return(tibble::tibble(
      quasi = q$quasi, source = q$source, level = q$level,
      step = ifelse(is.na(q$step), "as is", q$step), constant = q$constant
    ))
  }
  NULL
}

# Short, factual notes for the agent, generated from the step's log and
# settings. All the wording is here.
deid_caveats <- function(step, deid, fidelity = NULL) {
  s <- step$settings
  pick <- function(kind) s[names(s) == kind | startsWith(names(s), paste0(kind, "_"))]
  out <- character(0)

  for (x in pick("subject_dates")) {
    out <- c(out, paste0(
      "Dates are shifted by a per-subject offset of ", x$min_days, " to ", x$max_days, " days, earlier or later: ",
      "intervals within a subject are exact; calendar dates, days of week and comparisons across subjects are not. ",
      "Study days (--DY) are unchanged."
    ))
    if (identical(x$partial, "remove")) {
      out <- c(out, "Partial dates (year and month, or year only) are blank.")
    }
    if (identical(x$partial, "shift")) {
      out <- c(out, paste0(
        "Year-and-month dates moved from the middle of the month and year-only dates by whole years, ",
        "so they can fall out of order with the subject's full dates."
      ))
    }
  }
  ids <- unique(unlist(lapply(pick("subject_ids"), function(x) c(x$key, x$also, x$key_cols)), use.names = FALSE))
  # Only the columns the data holds (the steps' defaults name RSUBJID).
  data <- if (is.data.frame(step$data)) list(step$data) else step$data
  present <- unique(unlist(lapply(data, names), use.names = FALSE))
  ids <- ids[ids %in% present]
  if (length(ids) > 0) {
    out <- c(out, paste0("Pseudonyms replace ", deid_and(ids), "; they are the same in every dataset."))
  }
  # What the keys and dates steps found and left (their `findings`), by
  # reason, with the places named; never values.
  findings <- do.call(rbind, lapply(c(pick("subject_ids"), pick("subject_dates")), `[[`, "findings"))
  if (!is.null(findings) && nrow(findings) > 0) {
    reason <- unname(step_finding_reason[findings$kind])
    reason[is.na(reason)] <- findings$kind[is.na(reason)]
    where <- ifelse(is.na(findings$column), findings$dataset, paste0(findings$dataset, ".", findings$column))
    places <- vapply(split(where, factor(reason, levels = unique(reason))), function(w) {
      w <- unique(w)
      paste0(paste(utils::head(w, 10), collapse = ", "), if (length(w) > 10) paste0(" and ", length(w) - 10, " more"))
    }, character(1))
    out <- c(out, paste0(
      "Left as found by the keys and dates steps (see their warnings): ",
      paste0(names(places), " (", places, ")", collapse = "; "), "."
    ))
  }
  removed <- unique(unlist(lapply(pick("remove"), `[[`, "columns"), use.names = FALSE))
  if (length(removed) > 0) {
    out <- c(out, paste0("Direct identifiers are blank in every row: ", paste(removed, collapse = ", "), "."))
  }

  # Generalization: one note per quasi-identifier and rule.
  notes <- character(0)
  for (x in pick("generalize")) {
    for (r in x$rules) {
      name <- if (!is.null(r$column)) r$column else r$test
      notes <- c(notes, deid_rule_caveat(name, r))
    }
  }
  if (!is.null(deid) && !is.null(deid$quasi)) {
    gone <- deid$quasi$quasi[!is.na(deid$quasi$step) & deid$quasi$step == "remove"]
    notes <- c(notes, if (length(gone) > 0) paste0(deid_and(gone), " blank in every row (removed as a quasi-identifier)."))
  }
  out <- c(out, unique(notes))

  # Suppression: subjects with a blanked cell, by quasi-identifier.
  rec <- step$suppressed
  if (!is.null(rec) && nrow(rec) > 0) {
    label <- ifelse(is.na(rec$column), rec$test, rec$column)
    counts <- tapply(rec$key, label, function(k) length(unique(k)))
    counts <- sort(counts, decreasing = TRUE)
    out <- c(out, paste0(
      "Blank for some subjects (suppressed to meet the risk target): ",
      paste0(names(counts), " for ", counts, " subject(s)", collapse = ", "), "."
    ))
  }
  n_dropped <- if (!is.null(deid)) deid$n_dropped else sum(vapply(pick("suppress"), function(x) as.integer(x$n_dropped %||% 0L), integer(1)))
  if (isTRUE(n_dropped > 0)) {
    out <- c(out, paste0(n_dropped, " subject(s) were dropped from every dataset."))
  }
  log <- step$log
  derived <- log$step == "deidentify" & !is.na(log$dataset) & !is.na(log$variable) &
    grepl("blanked", log$detail) & !is.na(log$n_changed) & log$n_changed > 0 & startsWith(log$detail, "derived from")
  if (any(derived)) {
    out <- c(out, paste0(
      "Derived variables are blank where their source changed: ",
      paste(unique(paste0(log$dataset[derived], ".", log$variable[derived])), collapse = ", "), "."
    ))
  }
  if (!is.null(deid)) {
    if (!isTRUE(deid$meets_target)) {
      out <- c(out, paste0("The risk target is not met: ", paste(deid$not_met, collapse = "; "), "."))
    }
    if (any(deid$signals$n_below_k_with_quasi > 0)) {
      out <- c(out, paste0(
        "Residual signals (rows per subject, visits, study days, tests, durations) put subjects in groups below ",
        "the target: the copy is not anonymous against someone who knows them."
      ))
    }
  }
  if (!is.null(fidelity)) {
    failed <- sum(fidelity$status == "fail")
    out <- c(out, if (failed > 0) {
      paste0(
        "The fidelity check against the original failed ", failed, " check(s): code tested on the copy may not ",
        "behave the same on the original (see the fidelity table)."
      )
    } else if (!isTRUE(attr(fidelity, "complete"))) {
      paste0(
        "The fidelity check against the original found no failure but is incomplete: ",
        sum(fidelity$status == "warn"), " warning(s), ", sum(fidelity$status == "not applicable"),
        " check(s) not applicable; some rows or dates couldn't be compared (see the fidelity table)."
      )
    } else {
      paste0(
        "The fidelity check against the original passed (", sum(fidelity$status == "warn"), " warning(s)): ",
        "structure, keys, categories, date formats and intervals, study days and numeric relations hold as in ",
        "the original."
      )
    })
  }
  c(out, "The copy is for testing code, not for estimating results: means, counts and other statistics differ from the original.")
}

# What a generalization rule does to a quasi-identifier, from its parameters.
deid_rule_caveat <- function(name, r) {
  switch(r$rule,
    band = paste0(
      name, " is in bands of ", format_rule_value(r$width), " (",
      if (identical(r$value, "midpoint")) "midpoint" else "lower bound", " shown",
      if (!is.null(r$top)) paste0("; ", format_rule_value(r$top), " and above shown as ", format_rule_value(r$top)),
      if (!is.null(r$bottom)) paste0("; ", format_rule_value(r$bottom), " and below shown as ", format_rule_value(r$bottom)),
      ")."
    ),
    round = paste0(name, " is rounded to ", deid_round_text(r$digits), "."),
    fold = paste0(name, ": categories of fewer than ", r$min_n, " subjects are shown as ", format_rule_value(r$to), "."),
    paste0(name, " is generalized: ", deid_rule_text(r), ".")
  )
}

deid_round_text <- function(digits) {
  if (digits == 0) {
    "whole numbers"
  } else if (digits < 0) {
    paste0("multiples of ", format(10^-digits, big.mark = ",", scientific = FALSE))
  } else {
    paste0(digits, " decimal place(s)")
  }
}

deid_and <- function(x) {
  if (length(x) <= 1) {
    return(x)
  }
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

# The section as text for the prompt bundle.
format_deid_section_text <- function(x) {
  block <- function(title, table) {
    if (is.null(table) || nrow(table) == 0) {
      return(NULL)
    }
    c("", title, format_table_block(table))
  }
  lines <- c("Caveats for the agent:", paste0("- ", x$caveats))
  if (!is.null(x$risk)) {
    lines <- c(lines, block("Risk", x$risk))
    lines <- c(lines, paste0(
      "Meets target: ", if (isTRUE(x$meets_target)) "yes" else paste0("no (", paste(x$not_met, collapse = "; "), ")")
    ))
  }
  c(
    lines,
    block("Fidelity", x$fidelity),
    block("Settings", x$settings),
    block("Quasi-identifier levels", x$levels),
    block("Residual signals", x$signals),
    block("Profile rules", x$profile_rules),
    block("Steps", x$steps)
  ) |> paste(collapse = "\n")
}

# A report's section, or its named list of sections (one per step).
is_deid_section <- function(x) !is.null(x$caveats)

format_deid_sections_text <- function(x) {
  if (is_deid_section(x)) {
    return(format_deid_section_text(x))
  }
  paste(vapply(names(x), function(name) {
    paste0("Step: ", name, "\n", format_deid_section_text(x[[name]]))
  }, character(1)), collapse = "\n\n")
}

serialize_deid_sections <- function(x, path = "$") {
  if (!is_deid_section(x)) {
    out <- lapply(names(x), function(name) serialize_deid_sections(x[[name]], path = paste0(path, "$", name)))
    names(out) <- names(x)
    return(out)
  }
  out <- lapply(names(x), function(name) serialize_supported_value(x[[name]], path = paste0(path, "$", name)))
  names(out) <- names(x)
  out
}
