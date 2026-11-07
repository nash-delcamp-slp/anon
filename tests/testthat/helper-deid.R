# Shared fixture and registries for the de-identification invariants in
# test-deid-invariants.R.
#
# The fixture plants sentinel identifiers and real dates in every place they
# can hide. Each de-identification pipeline in `deid_pipelines` is run on it,
# and every way data can leave the package (`deid_exits`) is checked for the
# sentinels. When you add a de-identification function, add a pipeline that
# uses it; when you add a way to print, report, serialize or share an object,
# add an exit.

# Original subject keys, related IDs and per-subject real dates. Subjects'
# dates are years apart, so a shifted date (at most 3650 days) can't land on
# another subject's real date by chance, and each subject's full dates lie
# within 21 days of each other, so a shift of at least 30 days (the default
# minimum) can't land on the same subject's other real dates.
deid_keys <- c("ZQ-7701-0001", "ZQ-7701-0002", "ZQ-7701-0003")
deid_subjids <- c("ZQ0001", "ZQ0002", "ZQ0003")
deid_start <- as.Date(c("2031-03-10", "2041-08-22", "2051-11-05"))

# Columns or datasets that can't be de-identified automatically. Each must be
# named in a warning that gives the reason (`pattern` is matched against each
# warning on its own); the invariants then drop them, as a user acting on the
# warnings would, before checking that nothing else leaks.
deid_flagged <- list(
  list(dataset = "dm", column = "COVAL", pattern = "original `USUBJID` values.*dm\\.COVAL"),
  list(dataset = "dm", column = "COVAL", pattern = "dates that were not shifted.*dm\\.COVAL"),
  list(dataset = "vs", column = "usubjid", pattern = "another case.*vs\\.usubjid"),
  list(dataset = "ad", column = "TRTSDT", pattern = "SAS date.*ad\\.TRTSDT"),
  list(dataset = "notes", column = NULL, pattern = "Datasets without `USUBJID`.*notes \\(usubjid, NOTEDTC\\)")
)

deid_fixture <- function() {
  n <- length(deid_keys)
  study_day <- function(date, ref) as.integer(date - ref) + (date >= ref)

  dm <- data.frame(
    USUBJID = deid_keys,
    SUBJID = deid_subjids,
    RFSTDTC = format(deid_start),
    # A factor date column with a lower-case name, and an unused level that
    # still holds a real date.
    rficdtc = factor(format(deid_start - 14), levels = c(format(deid_start - 14), "2061-01-01")),
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
    VSDY = study_day(vs_dates, rep(deid_start, each = 2)),
    VSDTM = as.POSIXct(paste(format(vs_dates), "08:30"), tz = "UTC"),
    usubjid = rep(deid_keys, each = 2),
    stringsAsFactors = FALSE
  )
  attr(vs$USUBJID, "label") <- "Unique Subject Identifier"

  ex <- data.frame(EXSTDTC = format(deid_start), EXENDTC = substr(format(deid_start + 20), 1, 7))
  ex$USUBJID <- structure(deid_keys, label = "Unique Subject Identifier")
  ex$EXSTDY <- study_day(deid_start, deid_start)

  suppdm <- data.frame(
    USUBJID = deid_keys, QNAM = "ENRLDT", QVAL = format(deid_start - 10),
    stringsAsFactors = FALSE
  )
  relsub <- data.frame(USUBJID = deid_keys, RSUBJID = deid_keys[c(2, 3, 1)])

  # A numeric SAS date (days since 1960-01-01), which can't be shifted.
  ad <- data.frame(USUBJID = deid_keys)
  ad$TRTSDT <- as.numeric(deid_start - as.Date("1960-01-01"))
  attr(ad$TRTSDT, "format.sas") <- "DATE9"

  # A dataset without the key that still holds keys and dates.
  notes <- data.frame(usubjid = deid_keys[1], NOTEDTC = format(deid_start[1] + 2))

  list(dm = dm, vs = vs, ex = ex, suppdm = suppdm, relsub = relsub, ad = ad, notes = notes)
}

# Strings that must not appear in anything that leaves the package: keys,
# related IDs, every planted real full date, and the numbers behind real Date
# and POSIXct values. Partial dates (EXENDTC) are checked in the fidelity
# tests instead, because a shifted full date may fall in the same month.
deid_sentinels <- function() {
  dates <- c(deid_start, deid_start - 14, deid_start - 10, deid_start + 3, deid_start + 7)
  c(
    deid_keys,
    deid_subjids,
    # Unused factor levels planted in vs$USUBJID and dm$rficdtc.
    "ZQ-7701-0009",
    "2061-01-01",
    format(dates),
    as.character(as.numeric(c(deid_start, deid_start + 7))),
    as.character(as.numeric(as.POSIXct(paste(format(c(deid_start, deid_start + 7)), "08:30"), tz = "UTC")))
  )
}

# Each pipeline takes the fixture and returns a de-identification step, with
# secrets kept so that the exits are tested against a step that holds them.
deid_pipelines <- list(
  ids_then_dates = function(data) {
    anon_subject_dates(anon_subject_ids(data, keep_secrets = TRUE, seed = 1), keep_secrets = TRUE, seed = 2)
  },
  dates_then_ids = function(data) {
    anon_subject_ids(anon_subject_dates(data, keep_secrets = TRUE, seed = 3), keep_secrets = TRUE, seed = 4)
  }
)

# Each exit takes a step and returns text: what a user could print, save as
# JSON, paste into a prompt or see in the app. An exit that errors is checked
# on its error message. `anon_secrets()` and
# saveRDS() are left out on purpose: they are documented to hold the secrets.
deid_exits <- list(
  print = function(step) capture.output(print(step)),
  print_secrets = function(step) c(capture.output(print(step$secrets)), capture.output(str(step$secrets))),
  json_secrets = function(step) as_anon_json(list(secrets = step$secrets)),
  str = function(step) capture.output(str(step)),
  summary = function(step) capture.output(summary(step)),
  dput_data = function(step) capture.output(dput(step$data)),
  json = function(step) as_anon_json(step),
  json_nested = function(step) as_anon_json(list(a = list(step = step))),
  anon = function(step) c(capture.output(print(anon(step))), capture.output(dput(unclass(anon(step))))),
  anon_nested = function(step) capture.output(str(anon(list(a = step)))),
  inventory = function(step) capture.output(print(anon_inventory(list(step = step)))),
  data_summary = function(step) {
    capture.output(print(anon_data_summary(step, example_values_n = 5, example_rows = 3)))
  },
  report_env = function(step) {
    e <- new.env()
    assign("step", step, envir = e)
    capture.output(print(anon_report(e, example_values_n = 5, example_rows = 3)))
  },
  prompt_bundles = function(step) {
    e <- new.env()
    assign("step", step, envir = e)
    report <- anon_report(e, example_values_n = 5, example_rows = 3)
    unlist(lapply(c("markdown", "text", "json"), function(f) {
      as.character(anon_prompt_bundle(report = report, format = f))
    }))
  },
  app_state = function(step) {
    e <- new.env()
    assign("step", step, envir = e)
    capture.output(str(anon_app_initial_state(e, initial_selection = "step")))
  }
)

# Run a pipeline, collecting its warnings instead of printing them.
deid_run <- function(pipeline, data) {
  messages <- character(0)
  step <- withCallingHandlers(
    pipeline(data),
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

# Sentinels found in each piece of text, as "exit: sentinel" strings.
deid_leaks <- function(texts, sentinels = deid_sentinels()) {
  found <- character(0)
  for (name in names(texts)) {
    text <- paste(texts[[name]], collapse = "\n")
    hits <- sentinels[vapply(sentinels, grepl, logical(1), x = text, fixed = TRUE)]
    if (length(hits) > 0) {
      found <- c(found, paste0(name, ": ", paste(hits, collapse = ", ")))
    }
  }
  found
}
