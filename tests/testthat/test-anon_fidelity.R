# anon_check_fidelity() and the De-identification section of reports, on the
# small datasets of helper-fidelity.R. The invariants harness runs the check
# on every pipeline's output as well (test-deid-invariants.R).

test_that("an anon_deidentify() result passes against its original", {
  data <- fid_data()
  res <- fid_run(data)
  f <- anon_check_fidelity(data, res)
  expect_s3_class(f, "anon_fidelity")
  expect_named(f, c("dataset", "check", "status", "n_checked", "n_failed", "detail"))
  expect_true(attr(f, "passed"))
  expect_true(all(f$status == "pass"))
  expected_checks <- c(
    "datasets", "pseudonyms", "columns", "types", "attributes", "rows", "row order", "keys in dm",
    "distinct keys", "categorical values", "ISO 8601", "date intervals", "study days", "numeric relations"
  )
  expect_setequal(unique(f$check), expected_checks)
  # Subjects are paired by their rows, without the pseudonym map.
  expect_match(fid_row(f, "(all)", "pseudonyms")$detail, "12 of 12 subject(s) paired by the rows", fixed = TRUE)
  expect_match(fid_row(f, "vs", "categorical values")$detail, "rewritten from generalized numbers")
  expect_match(fid_row(f, "vs", "numeric relations")$detail, "changed on purpose")
  expect_identical(fid_row(f, "adlb", "study days")$n_checked, 36L)
  expect_identical(fid_row(f, "ae", "study days")$n_checked, 3L)
})

test_that("a step's kept pseudonym map pairs the subjects", {
  data <- fid_data()
  f <- anon_check_fidelity(data, fid_run(data, keep_secrets = TRUE))
  expect_true(attr(f, "passed"))
  expect_match(fid_row(f, "(all)", "pseudonyms")$detail, "by the step's kept pseudonym map", fixed = TRUE)
})

# Each break must fail its own check.
fid_break <- function(change, secrets = FALSE) {
  data <- fid_data()
  res <- fid_run(data, keep_secrets = secrets)
  res$data <- change(res$data)
  anon_check_fidelity(data, res)
}

test_that("a date shifted in one row only fails the date intervals", {
  for (secrets in c(FALSE, TRUE)) {
    f <- fid_break(function(d) {
      d$vs$VSDTC[3] <- format(as.Date(d$vs$VSDTC[3]) + 1)
      d
    }, secrets)
    row <- fid_row(f, "vs", "date intervals")
    expect_identical(row$status, "fail", info = paste("secrets:", secrets))
    expect_identical(row$n_failed, 1L, info = paste("secrets:", secrets))
    expect_identical(fid_row(f, "dm", "date intervals")$status, "pass", info = paste("secrets:", secrets))
    expect_false(attr(f, "passed"))
  }
})

test_that("a study day off by one fails the study days", {
  for (secrets in c(FALSE, TRUE)) {
    f <- fid_break(function(d) {
      d$vs$VSDY[2] <- d$vs$VSDY[2] + 1L
      d
    }, secrets)
    row <- fid_row(f, "vs", "study days")
    expect_identical(row$status, "fail", info = paste("secrets:", secrets))
    expect_identical(row$n_failed, 1L, info = paste("secrets:", secrets))
    expect_identical(fid_row(f, "(all)", "pseudonyms")$status, "pass", info = paste("secrets:", secrets))
  }
  f <- fid_break(function(d) {
    d$adlb$ADY[5] <- d$adlb$ADY[5] - 1L
    d
  })
  expect_identical(fid_row(f, "adlb", "study days")$status, "fail")
})

test_that("values moved between subjects fail the rows check, the subjects still paired", {
  for (secrets in c(FALSE, TRUE)) {
    # VSSEQ 1 of the first subject and 2 of the second swap: the column's
    # values are the original's, but two rows changed.
    f <- fid_break(function(d) {
      d$vs$VSSEQ[c(1, 4)] <- d$vs$VSSEQ[c(4, 1)]
      d
    }, secrets)
    row <- fid_row(f, "vs", "rows")
    expect_identical(row$status, "fail", info = paste("secrets:", secrets))
    expect_identical(row$n_failed, 2L, info = paste("secrets:", secrets))
    expect_identical(fid_row(f, "(all)", "pseudonyms")$status, "pass", info = paste("secrets:", secrets))
  }
})

test_that("a dataset or a column missing fails, without showing the original's names", {
  f <- fid_break(function(d) {
    d$ae <- NULL
    d$vs$VSBLFL <- NULL
    d
  })
  expect_identical(fid_row(f, "(all)", "datasets")$status, "fail")
  expect_match(fid_row(f, "(all)", "datasets")$detail, "1 missing from the copy", fixed = TRUE)
  expect_identical(fid_row(f, "vs", "columns")$status, "fail")
  expect_false(grepl("VSBLFL", fid_row(f, "vs", "columns")$detail, fixed = TRUE))
  f <- fid_break(function(d) {
    d$vs <- d$vs[c(2, 1, 3:ncol(d$vs))]
    d
  })
  expect_identical(fid_row(f, "vs", "columns")$status, "fail")
})

test_that("a column of another type fails the types", {
  f <- fid_break(function(d) {
    d$vs$VSSEQ <- as.character(d$vs$VSSEQ)
    d
  })
  row <- fid_row(f, "vs", "types")
  expect_identical(row$status, "fail")
  expect_match(row$detail, "VSSEQ")
})

test_that("a dropped subject left in one dataset fails the key checks", {
  for (secrets in c(FALSE, TRUE)) {
    f <- fid_break(function(d) {
      gone <- d$ae$USUBJID[1]
      for (ds in c("dm", "vs", "adlb")) d[[ds]] <- vctrs::vec_slice(d[[ds]], d[[ds]]$USUBJID != gone)
      d
    }, secrets)
    expect_identical(fid_row(f, "ae", "keys in dm")$status, "fail", info = paste("secrets:", secrets))
    # Without the map the subject counts as dropped, and ae has one subject
    # too many; with it, dm has one too few.
    expect_true("fail" %in% f$status[f$check == "distinct keys"], info = paste("secrets:", secrets))
    expect_identical(fid_row(f, "(all)", "pseudonyms")$status, "fail", info = paste("secrets:", secrets))
    # The step dropped no subject: the missing rows are not intended.
    expect_identical(fid_row(f, "dm", "rows")$status, "fail", info = paste("secrets:", secrets))
  }
})

test_that("a category not in the original nor in the step's fold rules fails", {
  f <- fid_break(function(d) {
    d$ae$AETERM[2] <- "NOT A TERM"
    d
  })
  row <- fid_row(f, "ae", "categorical values")
  expect_identical(row$status, "fail")
  expect_identical(row$n_failed, 1L)

  # A fold category is intended when the step's rules name it, and fails
  # when they don't.
  data <- fid_data()
  step <- anon_generalize(data, rules = list(dm = list(RACE = anon_fold(4, "OTHER"))))
  expect_true("OTHER" %in% step$data$dm$RACE)
  f <- anon_check_fidelity(data, step)
  expect_identical(fid_row(f, "dm", "categorical values")$status, "pass")
  expect_match(fid_row(f, "dm", "categorical values")$detail, "fold value")
  step$settings$generalize$rules[[1]]$to <- "SOMETHING ELSE"
  expect_identical(fid_row(anon_check_fidelity(data, step), "dm", "categorical values")$status, "fail")
})

test_that("format, order, relations and labels are checked", {
  f <- fid_break(function(d) {
    d$vs$VSDTC[1] <- substr(d$vs$VSDTC[1], 1, 7)
    d
  })
  expect_identical(fid_row(f, "vs", "ISO 8601")$status, "fail")

  f <- fid_break(function(d) {
    d$adlb[1:2, ] <- d$adlb[2:1, ]
    d
  })
  expect_identical(fid_row(f, "adlb", "row order")$status, "fail")

  f <- fid_break(function(d) {
    d$adlb$CHG[2] <- d$adlb$CHG[2] + 5
    d
  })
  expect_identical(fid_row(f, "adlb", "numeric relations")$status, "fail")

  f <- fid_break(function(d) {
    attr(d$dm$AGE, "label") <- NULL
    d
  })
  expect_identical(fid_row(f, "dm", "attributes")$status, "fail")

  f <- fid_break(function(d) {
    d$ae$AESTDTC[1] <- ""
    d
  })
  expect_identical(fid_row(f, "ae", "ISO 8601")$status, "fail")
})

test_that("properties already broken in the original are not applicable", {
  data <- fid_data()
  data$ae$USUBJID[5] <- "ZZSTUDY-999"
  data$adlb$CHG <- data$adlb$CHG + 1
  data$adlb$PCHG <- data$adlb$PCHG + 1
  f <- anon_check_fidelity(data, fid_run(data))
  row <- fid_row(f, "ae", "keys in dm")
  expect_identical(row$status, "not applicable")
  expect_match(row$detail, "1 row(s) of the original", fixed = TRUE)
  expect_identical(fid_row(f, "adlb", "numeric relations")$status, "not applicable")
})

test_that("without a step, intended changes are warnings and dropped subjects are counted", {
  data <- fid_data()
  res <- suppressWarnings(anon_deidentify(
    data, anon_profile_sdtm(min_n = 3), max_risk = 1/3, unresolved = "drop", seed = 7
  ))
  # Drop two subjects, as the step would, and say so in its settings.
  gone <- res$data$dm$USUBJID[1:2]
  for (ds in names(res$data)) res$data[[ds]] <- vctrs::vec_slice(res$data[[ds]], !res$data[[ds]]$USUBJID %in% gone)
  res$settings$suppress$n_dropped <- 2L
  f <- anon_check_fidelity(data, res)
  expect_true(attr(f, "passed"))
  expect_identical(fid_row(f, "dm", "rows")$status, "warn")
  expect_match(fid_row(f, "dm", "rows")$detail, "of 2 subject(s) missing from the copy", fixed = TRUE)
  expect_identical(fid_row(f, "dm", "distinct keys")$status, "pass")

  plain <- anon_check_fidelity(data, res$data)
  expect_identical(fid_row(plain, "vs", "categorical values")$status, "warn")
  expect_true(attr(plain, "passed"))
})

test_that("anon_check_fidelity() takes single data frames and checks its arguments", {
  data <- fid_data()
  res <- anon_subject_dates(data$dm, seed = 1)
  f <- anon_check_fidelity(data$dm, res)
  expect_identical(unique(f$dataset[f$dataset != "(all)"]), "data")
  expect_true(attr(f, "passed"))
  expect_error(anon_check_fidelity(list(data$dm), res), "named list")
  expect_error(anon_check_fidelity(data, res, dm = NA), "`dm`")
})

test_that("the result prints and serializes with counts only", {
  data <- fid_data()
  f <- anon_check_fidelity(data, fid_run(data, keep_secrets = TRUE))
  out <- capture.output(print(f))
  expect_match(out[1], "passed")
  payload <- as_anon_payload(f)
  expect_identical(payload$kind, "anon_fidelity")
  expect_true(payload$passed)
  json <- as.character(as_anon_json(list(fidelity = f)))
  expect_match(json, "\"kind\": \"anon_fidelity\"", fixed = TRUE)
  for (k in fid_keys) expect_false(grepl(k, paste(c(out, json), collapse = "\n"), fixed = TRUE))
})

test_that("anon_report() and anon_prompt_bundle() describe the step first", {
  data <- fid_data()
  res <- fid_run(data)
  f <- anon_check_fidelity(data, res)
  report <- anon_report(res, fidelity = f, nlp_auto = FALSE)
  section <- report$deidentification
  expect_named(section, c(
    "caveats", "risk", "meets_target", "not_met", "settings", "levels", "profile_rules", "signals", "steps", "fidelity"
  ))
  expect_true(any(grepl("per-subject offset of 30 to 365 days", section$caveats)))
  expect_true(any(grepl("AGE is in bands of 20", section$caveats)))
  expect_true(any(grepl("Pseudonyms replace USUBJID and SUBJID", section$caveats)))
  expect_true(any(grepl("^Blank for some subjects.*SEX for 12 subject", section$caveats)))
  expect_true(any(grepl("not for estimating results", section$caveats)))
  expect_true(any(grepl("fidelity check against the original passed", section$caveats)))
  expect_identical(section$risk$measured, c("before", "after"))
  expect_identical(nrow(section$fidelity), nrow(f))

  for (format in c("markdown", "text")) {
    bundle <- as.character(anon_prompt_bundle(report, format = format))
    expect_lt(regexpr("De-identification", bundle), regexpr("Inventory", bundle))
    expect_match(bundle, "Caveats for the agent", fixed = TRUE)
    without <- as.character(anon_prompt_bundle(report, format = format, include_deidentification = FALSE))
    expect_false(grepl("De-identification", without, fixed = TRUE))
  }
  json <- jsonlite::fromJSON(as.character(anon_prompt_bundle(report, format = "json")), simplifyVector = FALSE)
  expect_identical(names(json$report)[1], "deidentification")
  expect_identical(as_anon_payload(report)$deidentification$caveats, section$caveats)

  # Without a step there is no section; a plain report is unchanged.
  expect_null(anon_report(res$data, nlp_auto = FALSE)$deidentification)
})

test_that("the section takes the report's rules and one section per step", {
  data <- fid_data()
  res <- fid_run(data)
  report <- anon_report(list(res = res), pattern_list = list("[TRIAL]" = "adlb"), nlp_auto = FALSE)
  steps <- report$deidentification$steps
  expect_false(any(grepl("adlb", unlist(report$deidentification), fixed = TRUE)))

  two <- anon_report(list(a = res, b = res), nlp_auto = FALSE)
  expect_named(two$deidentification, c("a", "b"))
  expect_match(as.character(anon_prompt_bundle(two)), "Step: a", fixed = TRUE)
  expect_error(anon_report(list(a = res, b = res), fidelity = anon_check_fidelity(data, res)), "exactly one")
  expect_error(anon_report(res, fidelity = list()), "anon_check_fidelity")
  # A selection picks the step through one of its datasets.
  one <- anon_report(list(a = res, b = res), selection = "b$dm", nlp_auto = FALSE)
  expect_true(is_deid_section(one$deidentification))
})

test_that("reports and bundles of a step that kept its secrets hold none of them", {
  data <- fid_data()
  res <- fid_run(data, keep_secrets = TRUE)
  secrets <- anon_secrets(res)
  expect_true(all(fid_keys %in% secrets$subject_ids$key))
  f <- anon_check_fidelity(data, res)
  report <- anon_report(list(res = res), fidelity = f, example_values_n = 3, example_rows = 2, nlp_auto = FALSE)
  bundles <- lapply(c("markdown", "text", "json"), function(format) anon_prompt_bundle(report, format = format))
  raw <- c(
    serialize(report, NULL), serialize(bundles, NULL), serialize(f, NULL),
    charToRaw(as.character(as_anon_json(report)))
  )
  # No original key, no secret table or its columns anywhere; no date at all
  # in the section (the data summary shows shifted dates, which can equal
  # another subject's real ones).
  for (x in fid_keys) expect_length(grepRaw(x, raw, fixed = TRUE), 0)
  for (x in c("offset_days", "year_direction", "anon_secrets", "generalize_folded")) {
    expect_length(grepRaw(x, raw, fixed = TRUE), 0)
  }
  section <- serialize(report$deidentification, NULL)
  expect_length(grepRaw("[0-9]{4}-[0-9]{2}", section), 0)
  # The test can find what it looks for.
  expect_gt(length(grepRaw(fid_keys[1], serialize(data, NULL), fixed = TRUE)), 0)
  expect_gt(length(grepRaw("offset_days", serialize(secrets, NULL), fixed = TRUE)), 0)
})
