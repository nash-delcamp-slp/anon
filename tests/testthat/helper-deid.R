# Shared fixture and registries for the de-identification invariants in
# test-deid-invariants.R.
#
# The fixture plants sentinel identifiers, real dates and user-pattern terms
# (such as a drug name in a label) in every place they can hide. Each
# pipeline in `deid_pipelines` is run on it, and every way data can leave the
# package (`deid_exits`) is checked for the sentinels. Rows are checked
# against the step's secrets: each row keeps its own subject's pseudonym and
# date offset.
#
# To register new work:
# - a de-identification function: add a pipeline record to `deid_pipelines`;
# - a way to print, report, serialize or share an object: add an exit to
#   `deid_exits`;
# - a hiding place the package can't handle yet: add it to `deid_known_gaps`
#   (the test fails when a gap is fixed, so it can move into the fixture).

# Original subject keys, related IDs and per-subject real dates. Subjects'
# dates are ten years apart, so a shifted date (at most 3650 days) can't land
# on another subject's real date, and each subject's full dates lie within 21
# days of each other, so a shift of more than 21 days can't land on the same
# subject's other real dates. Pipelines must use `min_days` above 21.
deid_keys <- c("ZQ-7701-0001", "ZQ-7701-0002", "ZQ-7701-0003", "ZQ-7701-0004", "ZQ-7701-0005")
deid_subjids <- c("ZQ0001", "ZQ0002", "ZQ0003", "ZQ0004", "ZQ0005")
deid_start <- as.Date(c("2031-03-10", "2041-08-22", "2051-11-05", "2061-05-17", "2071-01-28"))
deid_min_spacing <- 21

# Terms that only anon() with `deid_patterns` removes, such as a drug or study
# name in a variable or dataset label.
deid_patterns <- list("[DRUG]" = "DRUGQX", "[STUDY]" = "STUDYQX")

# Columns or datasets that can't be de-identified automatically. Each must be
# named in a warning that gives the reason; `pattern` is matched against each
# warning on its own. The invariants then drop them, as a user acting on the
# warnings would, before checking that nothing else leaks.
deid_flagged <- list(
  list(dataset = "dm", column = "COVAL", pattern = "original `USUBJID` values.*dm\\.COVAL([,.;]|$)"),
  list(dataset = "dm", column = "COVAL", pattern = "dates that were not shifted.*dm\\.COVAL([,.;]|$)"),
  list(dataset = "vs", column = "usubjid", pattern = "another case.*vs\\.usubjid([,.;]|$)"),
  list(dataset = "ad", column = "TRTSDT", pattern = "SAS date.*ad\\.TRTSDT([,.;]|$)"),
  list(dataset = "notes", column = NULL, pattern = "Datasets without `USUBJID`.*notes \\(usubjid, NOTEDTC\\)")
)

deid_study_day <- function(date, ref) as.integer(date - ref) + (date >= ref)

deid_fixture <- function() {
  n <- length(deid_keys)

  dm <- data.frame(
    USUBJID = deid_keys,
    SUBJID = deid_subjids,
    AGE = c(34, 51, 47, 62, 29),
    RFSTDTC = format(deid_start),
    # A factor date column with a lower-case name, and an unused level that
    # still holds a real date.
    rficdtc = factor(format(deid_start - 14), levels = c(format(deid_start - 14), "2081-01-01")),
    RFSTDT = deid_start,
    COVAL = paste("Patient", deid_keys, "seen on", format(deid_start + 3)),
    row.names = deid_keys,
    stringsAsFactors = FALSE
  )
  attr(dm$USUBJID, "label") <- "Unique Subject Identifier"
  attr(dm$RFSTDTC, "label") <- "Subject Reference Start Date/Time"
  attr(dm$rficdtc, "label") <- "Date/Time of Informed Consent"

  vs_dates <- rep(deid_start, each = 2) + c(0, 7)
  vs <- data.frame(
    USUBJID = factor(rep(deid_keys, each = 2), levels = c(deid_keys, "ZQ-7701-0009")),
    VSDTC = paste0(format(vs_dates), "T08:30"),
    VSDY = deid_study_day(vs_dates, rep(deid_start, each = 2)),
    VSSTRESN = seq_len(2 * n) + 60,
    VSDTM = as.POSIXct(paste(format(vs_dates), "08:30"), tz = "UTC"),
    usubjid = rep(deid_keys, each = 2),
    stringsAsFactors = FALSE
  )
  attr(vs$USUBJID, "label") <- "Unique Subject Identifier"
  attr(vs$VSSTRESN, "label") <- "DRUGQX Concentration"
  attr(vs, "label") <- "STUDYQX Vital Signs"

  ex <- data.frame(EXSTDTC = format(deid_start), EXENDTC = substr(format(deid_start + 20), 1, 7))
  ex$USUBJID <- structure(deid_keys, label = "Unique Subject Identifier")
  ex$EXSTDY <- deid_study_day(deid_start, deid_start)

  suppdm <- data.frame(
    USUBJID = deid_keys, QNAM = "ENRLDT", QVAL = format(deid_start - 10),
    stringsAsFactors = FALSE
  )
  relsub <- data.frame(USUBJID = deid_keys, RSUBJID = deid_keys[c(2:n, 1)])

  # A numeric SAS date (days since 1960-01-01), which can't be shifted.
  ad <- data.frame(USUBJID = deid_keys)
  ad$TRTSDT <- as.numeric(deid_start - as.Date("1960-01-01"))
  attr(ad$TRTSDT, "format.sas") <- "DATE9"

  # A dataset without the key that still holds keys and dates.
  notes <- data.frame(usubjid = deid_keys[1], NOTEDTC = format(deid_start[1] + 2))

  list(dm = dm, vs = vs, ex = ex, suppdm = suppdm, relsub = relsub, ad = ad, notes = notes)
}

# Hiding places the package doesn't handle yet. Each `plant` adds one to the
# fixture, and `sentinel` must then leak through some exit (only exits that
# apply `deid_patterns` when `pattern_exits_only` is TRUE). When a fix makes a
# gap stop leaking, its test fails: move the planting into deid_fixture()
# (with the expected handling) and remove the gap.
deid_known_gaps <- list(
  variable_label_key = list(
    sentinel = "ZQ-7701-0003",
    plant = function(d) {
      attr(d$vs$VSDY, "label") <- "Study day for ZQ-7701-0003"
      d
    }
  ),
  variable_label_date = list(
    sentinel = "2031-03-10",
    plant = function(d) {
      attr(d$ex$EXSTDY, "label") <- "Study day from 2031-03-10"
      d
    }
  ),
  dataset_attribute_key = list(
    sentinel = "ZQ-7701-0002",
    plant = function(d) {
      attr(d$vs, "note") <- "from ZQ-7701-0002"
      d
    }
  ),
  comment_attribute_key = list(
    sentinel = "ZQ-7701-0004",
    plant = function(d) {
      comment(d$vs$VSDY) <- "ZQ-7701-0004"
      d
    }
  ),
  value_labels_key = list(
    sentinel = "ZQ-7701-0001",
    plant = function(d) {
      d$dm$GRP <- haven::labelled(c(1, 2, 2, 2, 2), labels = c("ZQ-7701-0001" = 1, "Other" = 2))
      d
    }
  ),
  value_label_names_pattern = list(
    sentinel = "DRUGQX arm",
    pattern_exits_only = TRUE,
    plant = function(d) {
      d$dm$ARMCD <- haven::labelled(c(1, 2, 1, 2, 1), labels = c("DRUGQX arm" = 1, "Placebo" = 2))
      d
    }
  ),
  na_values_key = list(
    sentinel = "ZQ-7701-0005",
    plant = function(d) {
      d$dm$SITE <- haven::labelled_spss(c("S1", "S1", "S2", "S2", "S1"), na_values = "ZQ-7701-0005")
      d
    }
  ),
  list_column_key = list(
    sentinel = "ZQ-7701-0001",
    plant = function(d) {
      d$dm$LINKS <- I(as.list(c("ZQ-7701-0001", "x", "y", "z", "w")))
      d
    }
  ),
  column_named_key = list(
    sentinel = "ZQ-7701-0003",
    plant = function(d) {
      d$vs[["ZQ-7701-0003"]] <- 1
      d
    }
  ),
  row_names_unkeyed = list(
    sentinel = "ZQ-7701-0002",
    plant = function(d) {
      d$ta <- data.frame(ARM = c("A", "B"), row.names = deid_keys[2:3])
      d
    }
  ),
  dataset_named_key = list(
    sentinel = "ZQ-7701-0004",
    plant = function(d) {
      d[["ZQ-7701-0004"]] <- data.frame(X = 1)
      d
    }
  ),
  subjid_in_text = list(
    sentinel = "ZQ0002",
    plant = function(d) {
      d$dm$COMM <- c("see ZQ0002", "", "", "", "")
      d
    }
  )
)

# Strings that must not appear in anything that leaves the package: keys,
# related IDs, every planted real full date, and the numbers behind real Date
# and POSIXct values (matched as whole numbers). Partial dates (EXENDTC) are
# checked against the secrets instead, because a shifted full date may fall in
# the same month. With `patterns = TRUE`, the user-pattern terms too.
deid_sentinels <- function(patterns = FALSE) {
  dates <- c(deid_start, deid_start - 14, deid_start - 10, deid_start + 3, deid_start + 7)
  c(
    deid_keys,
    deid_subjids,
    # Unused factor levels planted in vs$USUBJID and dm$rficdtc.
    "ZQ-7701-0009",
    "2081-01-01",
    format(dates),
    as.character(as.numeric(c(deid_start, deid_start + 7))),
    as.character(as.numeric(as.POSIXct(paste(format(c(deid_start, deid_start + 7)), "08:30"), tz = "UTC"))),
    if (patterns) unlist(deid_patterns, use.names = FALSE)
  )
}

# Each pipeline record has `run` (takes the fixture, returns a step),
# `keep_secrets`, `prefix` (of character pseudonyms) and `min_days`. A
# pipeline without secrets names, in `same_as`, the pipeline with secrets it
# must match. Later pipelines that change or remove columns on purpose should
# add fields for those (and adjust the shape and oracle tests) rather than
# loosen the checks.
deid_pipelines <- list(
  ids_then_dates = list(
    run = function(data) {
      anon_subject_dates(anon_subject_ids(data, keep_secrets = TRUE, seed = 1), keep_secrets = TRUE, seed = 2)
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30
  ),
  dates_then_ids = list(
    run = function(data) {
      anon_subject_ids(anon_subject_dates(data, keep_secrets = TRUE, seed = 3), keep_secrets = TRUE, seed = 4)
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30
  ),
  ids_then_dates_no_secrets = list(
    run = function(data) anon_subject_dates(anon_subject_ids(data, seed = 1), seed = 2),
    keep_secrets = FALSE, prefix = "SUBJ-", min_days = 30, same_as = "ids_then_dates"
  )
)

# Each exit has `fn` (takes a step, returns text: what a user could print,
# save, paste into a prompt or see in the app), `patterns` (TRUE if it applies
# `deid_patterns`, so user-pattern terms must be gone too) and `errors` (TRUE,
# or a function of the step, if it is expected to error; its error message is
# then checked). Messages
# and warnings raised by an exit are checked as well. `anon_secrets()` and
# serialize() of a step with secrets are left out on purpose: they are
# documented to hold the secrets.
deid_dput <- function(x) {
  capture.output(dput(x, control = c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17")))
}

deid_env_with <- function(step) {
  e <- new.env()
  assign("step", step, envir = e)
  e
}

deid_exits <- list(
  print = list(fn = function(step) capture.output(print(step))),
  str = list(fn = function(step) capture.output(str(step))),
  summary = list(fn = function(step) capture.output(summary(step))),
  dput_data = list(fn = function(step) deid_dput(step$data)),
  dput_log_settings = list(fn = function(step) deid_dput(unclass(step)[c("log", "settings")])),
  print_secrets = list(fn = function(step) c(capture.output(print(step$secrets)), capture.output(str(step$secrets)))),
  json_secrets = list(
    fn = function(step) as_anon_json(list(secrets = step$secrets)),
    errors = function(step) !is.null(step$secrets)
  ),
  json = list(fn = function(step) as_anon_json(step)),
  json_data = list(fn = function(step) as_anon_json(step$data)),
  json_nested = list(fn = function(step) as_anon_json(list(a = list(step = step)))),
  anon = list(
    fn = function(step) {
      a <- anon(step, pattern_list = deid_patterns)
      c(capture.output(print(a)), deid_dput(unclass(a)))
    },
    patterns = TRUE
  ),
  anon_nested = list(
    fn = function(step) deid_dput(unclass(anon(list(a = step), pattern_list = deid_patterns))),
    patterns = TRUE
  ),
  serialize_anon = list(
    fn = function(step) rawToChar(serialize(anon(step, pattern_list = deid_patterns), NULL, ascii = TRUE)),
    patterns = TRUE
  ),
  inventory = list(
    fn = function(step) capture.output(print(anon_inventory(list(step = step), pattern_list = deid_patterns))),
    patterns = TRUE
  ),
  data_summary = list(
    fn = function(step) {
      capture.output(print(anon_data_summary(step, pattern_list = deid_patterns, example_values_n = 5, example_rows = 3)))
    },
    patterns = TRUE
  ),
  data_summary_keyed_rows = list(
    fn = function(step) {
      rows <- anon_example_rows(n = 3, key = "USUBJID")
      deid_dput(anon_data_summary(step, pattern_list = deid_patterns, example_values_n = 5, example_rows = rows))
    },
    patterns = TRUE
  ),
  report = list(
    fn = function(step) {
      report <- anon_report(deid_env_with(step), pattern_list = deid_patterns, example_values_n = 5, example_rows = 3)
      c(capture.output(print(report)), as_anon_json(report))
    },
    patterns = TRUE
  ),
  prompt_bundles = list(
    fn = function(step) {
      report <- anon_report(deid_env_with(step), pattern_list = deid_patterns, example_values_n = 5, example_rows = 3)
      unlist(lapply(c("markdown", "text", "json"), function(f) {
        as.character(anon_prompt_bundle(report = report, format = f))
      }))
    },
    patterns = TRUE
  ),
  # What the app shows: its inventory and object choices. The state also holds
  # the objects themselves, unredacted, for building reports.
  app_state = list(
    fn = function(step) {
      state <- anon_app_initial_state(deid_env_with(step), pattern_list = deid_patterns, initial_selection = "step")
      deid_dput(state[c("inventory", "raw_names", "choices", "selected")])
    },
    patterns = TRUE
  ),
  risk = list(
    fn = function(step) {
      risk <- anon_risk(step, quasi = list(dm = "AGE"))
      c(capture.output(print(risk)), as_anon_json(risk), deid_dput(anon_risk_subjects(step, quasi = list(dm = "AGE"))))
    }
  )
)

# Run a pipeline, collecting its warnings instead of printing them.
deid_run <- function(pipeline, data) {
  messages <- character(0)
  step <- withCallingHandlers(
    pipeline$run(data),
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(step = step, warnings = messages)
}

# Drop the flagged columns and datasets from a step's data.
deid_drop_flagged <- function(step) {
  for (f in deid_flagged) {
    if (is.null(f$column)) {
      step$data[[f$dataset]] <- NULL
    } else {
      step$data[[f$dataset]][[f$column]] <- NULL
    }
  }
  step
}

# Run every exit on a step: a list with each exit's text (including messages,
# warnings and any error), whether it errored, and whether it applies
# `deid_patterns`.
deid_run_exits <- function(step) {
  lapply(deid_exits, function(exit) {
    conditions <- character(0)
    errored <- FALSE
    text <- withCallingHandlers(
      tryCatch(exit$fn(step), error = function(e) {
        errored <<- TRUE
        conditionMessage(e)
      }),
      warning = function(w) {
        conditions <<- c(conditions, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        conditions <<- c(conditions, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    list(text = c(as.character(text), conditions), errored = errored, patterns = isTRUE(exit$patterns))
  })
}

# Sentinels found in each exit's text, as "exit: sentinel" strings. Numeric
# sentinels match only as whole numbers. By default each exit is checked
# against deid_sentinels(), with the user-pattern terms for exits that apply
# them.
deid_leaks <- function(results, sentinels = NULL) {
  found <- character(0)
  for (name in names(results)) {
    text <- paste(results[[name]]$text, collapse = "\n")
    s <- if (is.null(sentinels)) deid_sentinels(patterns = results[[name]]$patterns) else sentinels
    hit <- vapply(s, function(x) {
      if (grepl("^[0-9]+$", x)) {
        grepl(paste0("(?<![0-9.])", x, "(?![0-9])"), text, perl = TRUE)
      } else {
        grepl(x, text, fixed = TRUE)
      }
    }, logical(1))
    if (any(hit)) {
      found <- c(found, paste0(name, ": ", paste(s[hit], collapse = ", ")))
    }
  }
  found
}

# From a step's secrets: per original key, the new key and the date offset in
# days.
deid_oracle <- function(step) {
  secrets <- anon_secrets(step)
  ids <- secrets$subject_ids
  offsets <- secrets$date_offsets
  new_key <- stats::setNames(as.character(ids$pseudonym), as.character(ids$key))
  # The offsets are keyed by the key as it was when the dates step ran: the
  # original key, or the pseudonym if the IDs step ran first.
  offset_key <- as.character(offsets$key)
  if (!all(offset_key %in% names(new_key))) {
    offset_key <- names(new_key)[match(offset_key, new_key)]
  }
  list(new_key = new_key, offset = stats::setNames(offsets$offset_days, offset_key))
}
