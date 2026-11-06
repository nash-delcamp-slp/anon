subject_data <- function() {
  list(
    dm = data.frame(
      USUBJID = c("01-701-1015", "01-701-1023", "01-702-1001"),
      SUBJID = c("1015", "1023", "1001"),
      RFSTDTC = c("2014-01-02", "2012-08-05", "2013-07-19"),
      AGE = c(63, 64, 71),
      stringsAsFactors = FALSE
    ),
    pc = data.frame(
      USUBJID = c("01-701-1015", "01-701-1015", "01-702-1001"),
      PCDTC = c("2014-01-02T08:00", "2014-01-03T09:30", "2013-07-19T08:00"),
      PCDY = c(1, 2, 1),
      stringsAsFactors = FALSE
    ),
    ts = data.frame(TSPARMCD = "SSTDTC", TSVAL = "2012-07-01", stringsAsFactors = FALSE)
  )
}

test_that("anon_subject_ids() gives each subject one pseudonym in every dataset", {
  data <- subject_data()
  result <- anon_subject_ids(data, seed = 1)

  expect_s3_class(result, "anon_deid_step")
  out <- result$data
  expect_named(out, names(data))
  expect_false(any(out$dm$USUBJID %in% data$dm$USUBJID))
  expect_match(out$dm$USUBJID, "^SUBJ-[0-9]$")
  expect_equal(anyDuplicated(out$dm$USUBJID), 0)

  # Same subject, same pseudonym across datasets
  map <- stats::setNames(out$dm$USUBJID, data$dm$USUBJID)
  expect_equal(out$pc$USUBJID, unname(map[data$pc$USUBJID]))
  # SUBJID follows the pseudonym number
  expect_equal(out$dm$SUBJID, sub("^SUBJ-", "", out$dm$USUBJID))
  # Other columns and datasets without the key are unchanged
  expect_equal(out$dm$AGE, data$dm$AGE)
  expect_equal(out$ts, data$ts)
  expect_null(result$secrets)
})

test_that("anon_subject_ids() keeps column types and missing values", {
  data <- list(
    dm = data.frame(ID = c(101, 102, NA), SUBJID = c(1L, 2L, NA), X = 1:3),
    vs = data.frame(ID = c(102, 102, 101), Y = 1:3)
  )
  out <- anon_subject_ids(data, key = "ID", seed = 2)$data
  expect_type(out$dm$ID, "double")
  expect_type(out$dm$SUBJID, "integer")
  # Rows are sorted by the new key, with missing keys last
  expect_true(is.na(out$dm$ID[3]))
  expect_true(is.na(out$dm$SUBJID[3]))
  expect_equal(out$dm$ID[1:2], c(1, 2))
  expect_equal(out$dm$SUBJID[1:2], as.integer(out$dm$ID[1:2]))
  # X follows its row: dm row with X = 2 had ID 102
  id_102 <- out$dm$ID[out$dm$X == 2]
  expect_equal(sum(out$vs$ID == id_102), 2)
  expect_equal(out$vs$Y[out$vs$ID == id_102], 1:2)
})

test_that("anon_subject_ids() treats blank keys as missing and accepts a data frame", {
  df <- data.frame(USUBJID = c("a", "", "b"), SUBJID = c("1", "", "2"))
  out <- anon_subject_ids(df, seed = 3)$data
  expect_s3_class(out, "data.frame")
  expect_equal(out$USUBJID, c("SUBJ-1", "SUBJ-2", ""))
  expect_equal(out$SUBJID, c("1", "2", ""))
})

test_that("anon_subject_ids() errors when a related ID is not one per subject", {
  df <- data.frame(USUBJID = c("a", "a"), SUBJID = c("1", "2"))
  expect_error(anon_subject_ids(df), "`SUBJID` has more than one value for some subjects")
})

test_that("anon_subject_ids() keeps secrets only when asked, out of print and JSON", {
  data <- subject_data()
  result <- anon_subject_ids(data, keep_secrets = TRUE, seed = 4)
  secrets <- anon_secrets(result)
  expect_equal(nrow(secrets$subject_ids), 3)
  expect_setequal(secrets$subject_ids$key, data$dm$USUBJID)

  shown <- c(capture.output(print(result)), capture.output(print(result$secrets)),
             capture.output(print(anon(result))))
  expect_false(any(grepl("01-701", shown, fixed = TRUE)))
  json <- as_anon_json(result)
  expect_match(json, '"kind": "anon_deid_step"', fixed = TRUE)
  expect_false(grepl("01-701", json, fixed = TRUE))
  expect_error(anon_secrets(anon_subject_ids(data, seed = 4)), "No secrets were kept")
})

test_that("the log records when a seed made the result reproducible", {
  expect_true(any(grepl("reproducible", anon_subject_ids(subject_data(), seed = 1)$log$detail)))
  expect_false(any(grepl("reproducible", anon_subject_ids(subject_data())$log$detail)))
})

test_that("anon_subject_dates() shifts each subject's dates by one offset everywhere", {
  data <- subject_data()
  expect_warning(
    result <- anon_subject_dates(data, keep_secrets = TRUE, seed = 5),
    "ts \\(TSVAL\\)"
  )
  out <- result$data
  offsets <- anon_secrets(result)$date_offsets
  off <- stats::setNames(offsets$offset_days, offsets$key)

  expect_true(all(abs(off) >= 30 & abs(off) <= 365))
  expect_equal(
    as.numeric(as.Date(out$dm$RFSTDTC) - as.Date(data$dm$RFSTDTC)),
    unname(off[data$dm$USUBJID])
  )
  expect_equal(
    as.numeric(as.Date(substr(out$pc$PCDTC, 1, 10)) - as.Date(substr(data$pc$PCDTC, 1, 10))),
    unname(off[data$pc$USUBJID])
  )
  # Times and study days are unchanged
  expect_equal(substring(out$pc$PCDTC, 11), substring(data$pc$PCDTC, 11))
  expect_equal(out$pc$PCDY, data$pc$PCDY)
  # Datasets without the key are left alone and logged
  expect_equal(out$ts, data$ts)
  expect_true(any(result$log$dataset == "ts" & grepl("no `USUBJID`", result$log$detail)))
})

test_that("anon_subject_dates() handles Date and POSIXct columns and keeps their types", {
  data <- list(
    dm = data.frame(USUBJID = c("a", "b"), D = as.Date(c("2020-01-01", "2020-06-01")),
                    T = as.POSIXct(c("2020-01-01 08:00:00", "2020-06-01 10:30:00"), tz = "UTC"))
  )
  result <- anon_subject_dates(data, vars = c("D", "T"), keep_secrets = TRUE, seed = 6)
  out <- result$data$dm
  off <- anon_secrets(result)$date_offsets$offset_days
  expect_s3_class(out$D, "Date")
  expect_s3_class(out$T, "POSIXct")
  expect_equal(as.numeric(out$D - data$dm$D), off)
  expect_equal(format(out$T, "%H:%M:%S"), c("08:00:00", "10:30:00"))
})

test_that("anon_subject_dates() removes dates it cannot tie to a subject", {
  data <- list(ae = data.frame(USUBJID = c("a", NA), AESTDTC = c("2020-01-01", "2020-02-01")))
  expect_warning(
    out <- anon_subject_dates(data, seed = 7)$data,
    "1 date value\\(s\\) in rows without a `USUBJID` were removed"
  )
  expect_true(is.na(out$ae$AESTDTC[2]))
})

test_that("anon_subject_dates() validates its arguments", {
  data <- subject_data()
  expect_error(anon_subject_dates(data, max_days = 10, min_days = 30), "`min_days`")
  expect_error(anon_subject_dates(data, max_days = -1), "`max_days`")
  expect_error(anon_subject_dates(data, vars = "NOPE"), "None of `vars`")
})

test_that("subject IDs and dates keep SDTM study days consistent in pharmaversesdtm", {
  skip_if_not_installed("pharmaversesdtm")
  sdtm <- list(dm = pharmaversesdtm::dm, pc = pharmaversesdtm::pc, ex = pharmaversesdtm::ex)
  dated <- anon_subject_dates(sdtm, seed = 8)$data
  out <- anon_subject_ids(dated, seed = 9)$data

  joined <- merge(out$pc[c("USUBJID", "PCDTC", "PCDY")], out$dm[c("USUBJID", "RFSTDTC")])
  days <- as.numeric(as.Date(substr(joined$PCDTC, 1, 10)) - as.Date(joined$RFSTDTC))
  study_day <- ifelse(days >= 0, days + 1, days)
  expect_equal(mean(study_day == joined$PCDY, na.rm = TRUE), 1)

  expect_false(any(out$pc$USUBJID %in% pharmaversesdtm::dm$USUBJID))
  expect_true(all(out$pc$USUBJID %in% out$dm$USUBJID))
  expect_equal(nrow(out$pc), nrow(sdtm$pc))
})

test_that("year-only dates always change year; month dates keep month precision", {
  d <- data.frame(USUBJID = sprintf("S%03d", 1:300), BRTHDTC = "1950", MHSTDTC = "2014-03")
  result <- anon_subject_dates(d, keep_secrets = TRUE, seed = 10)
  out <- result$data
  off <- anon_secrets(result)$date_offsets$offset_days
  expect_true(all(out$BRTHDTC != "1950"))
  expect_true(all((as.integer(out$BRTHDTC) > 1950) == (off > 0)))
  expect_match(out$MHSTDTC, "^[0-9]{4}-[0-9]{2}$")
  expect_true(all(out$MHSTDTC != "2014-03"))
  expect_true(all(is.na(anon_subject_dates(d, partial = "remove", seed = 10)$data$BRTHDTC)))
})

test_that("anon_date_shift() keeps its year-only behaviour", {
  expect_equal(anon_date_shift("2014", offset = 30), "2014")
})

test_that("factor and lowercase DTC columns are shifted and keep their type", {
  d <- data.frame(USUBJID = c("a", "b"), aestdtc = c("2020-01-01", "2020-02-01"))
  d$AEENDTC <- factor(c("2020-01-05", "2020-02-05"))
  out <- anon_subject_dates(d, seed = 11)$data
  expect_false(any(out$aestdtc == d$aestdtc))
  expect_s3_class(out$AEENDTC, "factor")
  expect_false(any(as.character(out$AEENDTC) == as.character(d$AEENDTC)))
})

test_that("ISO dates in SUPP QVAL are shifted with the subject's offset", {
  supp <- data.frame(USUBJID = c("a", "a"), QNAM = c("NACTDT", "GRADE"), QVAL = c("2014-02-10", "2"))
  dm <- data.frame(USUBJID = "a", RFSTDTC = "2014-02-01")
  result <- anon_subject_dates(list(dm = dm, suppx = supp), keep_secrets = TRUE, seed = 12)
  off <- anon_secrets(result)$date_offsets$offset_days
  out <- result$data$suppx
  expect_equal(as.numeric(as.Date(out$QVAL[1]) - as.Date("2014-02-10")), off)
  expect_equal(out$QVAL[2], "2")
})

test_that("unshifted ISO dates and unkeyed datasets with dates or IDs give warnings", {
  d <- list(
    ae = data.frame(USUBJID = "a", AESTDTC = "2020-01-01", AESTTPT = "2020-01-01"),
    raw = data.frame(SUBJID = "1015", VISDTC = "2020-01-01", usubjid = "a")
  )
  expect_warning(
    expect_warning(anon_subject_dates(d, seed = 13), "contain ISO 8601 dates but were not shifted: ae.AESTTPT"),
    "Datasets without `USUBJID` were left unchanged but contain identifiers or dates: raw \\(usubjid, VISDTC\\)"
  )
  expect_warning(anon_subject_ids(d, seed = 13), "raw \\(SUBJID, usubjid\\)")
})

test_that("RSUBJID is mapped through the pseudonyms; other key columns are flagged", {
  rel <- list(
    dm = data.frame(USUBJID = c("S-1", "S-2")),
    relsub = data.frame(USUBJID = c("S-1", "S-2"), RSUBJID = c("S-2", "S-1"), NOTE = c("S-1", "x"))
  )
  expect_warning(result <- anon_subject_ids(rel, seed = 14), "still contain original `USUBJID` values: relsub.NOTE")
  out <- result$data
  expect_equal(out$relsub$RSUBJID, rev(out$relsub$USUBJID))
  expect_warning(
    anon_subject_ids(list(relsub = data.frame(USUBJID = "S-1", RSUBJID = "S-9")), seed = 1),
    "1 value\\(s\\) of `RSUBJID` are not known `USUBJID` values and were removed"
  )
})

test_that("labels and other attributes are kept on replaced columns", {
  dm <- data.frame(USUBJID = c("a", "b"), SUBJID = c("1", "2"), RFSTDTC = c("2020-01-01", "2020-02-01"),
                   D = as.Date(c("2020-01-01", "2020-02-01")))
  for (col in names(dm)) attr(dm[[col]], "label") <- paste("Label", col)
  dm$F <- factor(c("a", "b"))
  attr(dm$F, "label") <- "Label F"
  step <- anon_subject_dates(anon_subject_ids(dm, key_cols = "F", seed = 15), seed = 16)
  out <- step$data
  for (col in c(names(dm))) expect_equal(attr(out[[col]], "label"), paste("Label", col))
})

test_that("POSIXct keeps clock time across a daylight saving change", {
  d <- data.frame(USUBJID = "a", T = as.POSIXct("2020-01-15 08:00:00", tz = "America/New_York"))
  expect_warning(
    out <- anon_subject_dates(d, vars = "T", min_days = 150, max_days = 150, seed = 17),
    "possible offsets"
  )
  expect_equal(format(out$data$T, "%H:%M"), "08:00")
  expect_equal(attr(out$data$T, "tzone"), "America/New_York")
})

test_that("vars must be date columns", {
  d <- data.frame(USUBJID = "a", AGE = 60)
  expect_error(anon_subject_dates(d, vars = "AGE"), "`AGE` in `data` is not a date column")
})

test_that("narrow or zero-including offset ranges warn", {
  d <- data.frame(USUBJID = "a", X = as.Date("2020-01-01"))
  expect_warning(anon_subject_dates(d, min_days = 0, max_days = 400, seed = 1), "keep their real dates")
  expect_warning(anon_subject_dates(d, min_days = 100, max_days = 100, seed = 1), "Only 2 possible offsets")
})

test_that("invalid dates are logged as removed, not shifted", {
  d <- data.frame(USUBJID = c("a", "b"), XDTC = c("2020-02-30", "UNK"))
  expect_warning(result <- anon_subject_dates(d, seed = 18), "2 value\\(s\\) are not recognised ISO 8601 dates")
  expect_match(result$log$detail[result$log$variable %in% "XDTC"], "0 value\\(s\\) shifted, 2 removed \\(not a recognised date, or year out of range\\)")
  expect_true(all(is.na(result$data$XDTC)))
})

test_that("steps chain, keeping earlier logs and secrets", {
  data <- subject_data()
  step <- anon_subject_ids(
    suppressWarnings(anon_subject_dates(data, keep_secrets = TRUE, seed = 19)),
    keep_secrets = TRUE, seed = 20
  )
  expect_setequal(unique(step$log$step), c("subject_dates", "subject_ids"))
  expect_named(anon_secrets(step), c("date_offsets", "subject_ids"))
  expect_setequal(anon_secrets(step)$subject_ids$key, data$dm$USUBJID)
})

test_that("the global random state is restored after a seeded step", {
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  suppressWarnings(anon_subject_dates(subject_data(), seed = 1))
  expect_equal(stats::runif(1), before)
})

test_that("rows are sorted by the new key, keeping order within subjects", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:20), SEQ = 1:20)
  out <- anon_subject_ids(dm, seed = 21)$data
  expect_equal(out$USUBJID, sort(out$USUBJID))
  expect_false(identical(out$SEQ, 1:20))
  expect_equal(rownames(out), as.character(1:20))

  lb <- data.frame(USUBJID = rep(c("A", "B"), each = 3), LBSEQ = c(3, 1, 2, 1, 2, 3))
  out <- anon_subject_ids(lb, seed = 22)$data
  by_subject <- unname(split(out$LBSEQ, out$USUBJID))
  expect_setequal(by_subject, list(c(3, 1, 2), c(1, 2, 3)))
})

test_that("value labels holding original values are dropped", {
  skip_if_not_installed("haven")
  dm <- data.frame(row = 1:2)
  dm$USUBJID <- haven::labelled(c("01-1015", "01-1023"), labels = c(Smith = "01-1015"))
  dm$SUBJID <- haven::labelled(c(1015, 1023), labels = c("Pat Smith" = 1015))
  dm$AESTDTC <- haven::labelled(c("2014-01-01", "2014-02-01"), labels = c(First = "2014-01-01"))
  out <- anon_subject_dates(anon_subject_ids(dm, seed = 23), seed = 24)$data
  expect_null(attr(out$USUBJID, "labels"))
  expect_null(attr(out$SUBJID, "labels"))
  expect_null(attr(out$AESTDTC, "labels"))
  expect_false(any(as.character(out$AESTDTC) %in% c("2014-01-01", "2014-02-01")))
})

test_that("SDTM partial dates and intervals are shifted, not removed", {
  d <- data.frame(USUBJID = "a", XDTC = c("2014---15", "2014-01--T08:00", "2014-01-02/2014-01-05"))
  result <- suppressWarnings(anon_subject_dates(d, keep_secrets = TRUE, min_days = 40, max_days = 40, seed = 25))
  off <- anon_secrets(result)$date_offsets$offset_days
  out <- result$data$XDTC
  expect_match(out[1], "^[0-9]{4}---15$")
  expect_match(out[2], "^[0-9]{4}-[0-9]{2}--T08:00$")
  ends <- strsplit(out[3], "/")[[1]]
  expect_equal(as.numeric(as.Date(ends) - as.Date(c("2014-01-02", "2014-01-05"))), c(off, off))
})

test_that("large max_days is capped and years stay within 0001-9999", {
  d <- data.frame(USUBJID = "a", XDTC = "2014-01-01")
  expect_error(anon_subject_dates(d, max_days = 1e7), "from 1 to 3650")
  msgs <- character(0)
  withCallingHandlers(
    anon_subject_dates(data.frame(USUBJID = c("a", "b"), YDTC = c("9999", "9999")), min_days = 400, max_days = 400, seed = 1),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("outside years 0001-9999", msgs)))
})

test_that("offsets cover both directions uniformly and 0 only once", {
  off <- draw_offsets(20000, max_days = 3, min_days = 0, seed = 1)
  expect_setequal(unique(off), -3:3)
  expect_true(all(abs(table(off) / 20000 - 1 / 7) < 0.02))
  off <- draw_offsets(5000, max_days = 365, min_days = 30, seed = 2)
  expect_true(all(abs(off) >= 30 & abs(off) <= 365))
  expect_true(abs(mean(off > 0) - 0.5) < 0.03)
})

test_that("QVAL dates are flagged when vars skips them, and month dates shift", {
  supp <- data.frame(USUBJID = c("a", "a"), QNAM = c("A", "B"), QVAL = c("2014-03-05", "2014-03"), XDTC = "2014-01-01")
  expect_warning(anon_subject_dates(list(suppx = supp), vars = "XDTC", seed = 26), "suppx.QVAL")
  out <- anon_subject_dates(list(suppx = supp), seed = 26)$data$suppx
  expect_false(any(out$QVAL == supp$QVAL))
  expect_match(out$QVAL[2], "^[0-9]{4}-[0-9]{2}$")
})

test_that("text vars without any dates are rejected", {
  expect_error(
    anon_subject_dates(data.frame(USUBJID = "a", AETERM = "headache"), vars = "AETERM"),
    "`AETERM` in `data` is not a date column"
  )
})

test_that("invalid UTF-8 text does not break the steps or leak into warnings", {
  ae <- data.frame(USUBJID = c("a", "b"), AESTDTC = c("2020-01-01", "2020-02-01"))
  ae$AETERM <- c("Mr Smith\x92s rash", "ok")
  msgs <- character(0)
  withCallingHandlers({
    anon_subject_ids(list(ae = ae), seed = 27)
    anon_subject_dates(list(ae = ae), seed = 28)
  }, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_false(any(grepl("Smith", msgs, useBytes = TRUE)))
})

test_that("key, also and key_cols must not overlap", {
  d <- data.frame(USUBJID = c("A", "B"), SUBJID = c("1", "2"), RSUBJID = c("B", "A"))
  expect_error(anon_subject_ids(d, also = c("USUBJID", "SUBJID")), "must not overlap")
  expect_error(anon_subject_ids(d, also = "RSUBJID", key_cols = "RSUBJID"), "must not overlap")
})

test_that("chaining the same step twice keeps both secrets", {
  d <- data.frame(USUBJID = c("a", "b"))
  step <- anon_subject_ids(anon_subject_ids(d, keep_secrets = TRUE, seed = 1), keep_secrets = TRUE, seed = 2)
  expect_named(anon_secrets(step), c("subject_ids", "subject_ids_1"))
  expect_setequal(anon_secrets(step)$subject_ids$key, c("a", "b"))
})

test_that("numeric keys do not trigger the leftover-key warning; factor levels do", {
  d <- data.frame(USUBJID = c(1, 2, 3), ARMCD = c("1", "2", "3"))
  expect_no_warning(anon_subject_ids(d, seed = 1))
  f <- data.frame(USUBJID = c("S1", "S2"), OTHER = factor("x", levels = c("x", "S1")))
  expect_warning(anon_subject_ids(f, seed = 1), "f|data.OTHER")
})

test_that("unkeyed datasets with ISO dates in text columns are flagged", {
  d <- list(dm = data.frame(USUBJID = "a"), ts = data.frame(TSPARMCD = "SSTDTC", TSVAL = "2013-06-01"))
  expect_warning(anon_subject_dates(d, seed = 1), "ts \\(TSVAL\\)")
})

test_that("log has numeric counts and records settings", {
  d <- data.frame(USUBJID = c("a", NA), XDTC = c("2020-01-01", "2020-02-01"))
  log <- suppressWarnings(anon_subject_dates(d, seed = 1))$log
  row <- log[log$variable %in% "XDTC", ]
  expect_equal(row$n_changed, 1L)
  expect_equal(row$n_removed, 1L)
  expect_match(log$detail[1], "min_days = 30, max_days = 365", fixed = TRUE)
})

test_that("anon_risk() accepts a step result", {
  step <- anon_subject_ids(list(dm = data.frame(USUBJID = c("a", "b"), SEX = "F")), seed = 1)
  expect_equal(anon_risk(step, quasi = list(dm = "SEX"), max_risk = 1)$summary$n_subjects, 2)
})

test_that("invalid UTF-8 in a date column does not crash the dates step", {
  d <- data.frame(USUBJID = c("a", "b"), XDTC = c("2020-01-01", "caf\xe9"))
  expect_warning(out <- anon_subject_dates(d, seed = 1)$data, "not recognised ISO 8601 dates")
  expect_true(is.na(out$XDTC[2]))
})

test_that("years before 1000 keep four digits", {
  expect_equal(anon_date_shift("0001-01-05", offset = 40), "0001-02-14")
  expect_equal(anon_date_shift("0500-01", offset = 40), "0500-02")
})

test_that("mixed result columns in vars keep their non-date values", {
  fa <- data.frame(USUBJID = "a", FAORRES = c("2014-01-02", "MILD", "2014-03", "SEVERE"))
  result <- anon_subject_dates(fa, vars = "FAORRES", keep_secrets = TRUE, seed = 1)
  out <- result$data$FAORRES
  expect_equal(out[c(2, 4)], c("MILD", "SEVERE"))
  expect_false(any(out[c(1, 3)] == fa$FAORRES[c(1, 3)]))
})

test_that("SDTM times with unknown parts, time-only values and durations are handled", {
  d <- data.frame(USUBJID = "a", XDTC = c("2003-12-15T-:15", "2003-12-15T13:-:17", "-----T07:15", "--12-15", "2014-01-01/P1D"))
  result <- suppressWarnings(anon_subject_dates(d, keep_secrets = TRUE, min_days = 40, max_days = 40, seed = 1))
  off <- anon_secrets(result)$date_offsets$offset_days
  out <- result$data$XDTC
  expect_equal(substring(out[1:2], 11), c("T-:15", "T13:-:17"))
  expect_equal(as.numeric(as.Date(substr(out[1], 1, 10)) - as.Date("2003-12-15")), off)
  expect_equal(out[3], "-----T07:15")
  expect_true(is.na(out[4]))
  expect_match(out[5], "/P1D$")
})

test_that("year-and-day QVAL dates are shifted", {
  supp <- data.frame(USUBJID = "a", QNAM = "X", QVAL = "2014---15")
  out <- anon_subject_dates(list(suppae = supp), seed = 1)$data$suppae
  expect_false(out$QVAL == "2014---15")
  expect_match(out$QVAL, "^[0-9]{4}---15$")
})

test_that("partial = \"remove\" uses the column's missing value and counts partials", {
  d <- data.frame(USUBJID = "a", AESTDTC = c("2014-01-02", "2014-03", "2014", "2014-01/2014-02-01", NA))
  result <- anon_subject_dates(d, partial = "remove", seed = 1)
  out <- result$data$AESTDTC
  expect_false(is.na(out[1]))
  expect_true(all(is.na(out[2:5])))
  row <- result$log[result$log$variable %in% "AESTDTC", ]
  expect_equal(row$n_changed, 1L)
  expect_match(row$detail, "3 removed \\(partial\\)")
})

test_that("row names are reset so they can't hold keys", {
  dm <- data.frame(USUBJID = c("S1", "S2"))
  rownames(dm) <- dm$USUBJID
  expect_equal(rownames(anon_subject_ids(dm, seed = 1)$data), c("1", "2"))
  one <- data.frame(USUBJID = "S1")
  rownames(one) <- "S1"
  expect_equal(rownames(anon_subject_ids(one, seed = 1)$data), "1")
})

test_that("SPSS missing-value codes are dropped with value labels", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("a", "b"))
  d$SUBJID <- haven::labelled_spss(c(1015, 1028), na_values = c(-9, 1028))
  out <- anon_subject_ids(d, seed = 1)$data$SUBJID
  expect_null(attr(out, "na_values"))
  expect_null(attr(out, "na_range"))
})

test_that("narrow ranges that include 0 give both warnings", {
  d <- data.frame(USUBJID = "a", X = as.Date("2020-01-01"))
  msgs <- character(0)
  withCallingHandlers(anon_subject_dates(d, min_days = 0, max_days = 1, seed = 1),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("keep their real dates", msgs)))
  expect_true(any(grepl("possible offsets", msgs)))
})

test_that("steps record structured settings without the seed value", {
  step <- anon_subject_ids(suppressWarnings(anon_subject_dates(subject_data(), seed = 123)), seed = 456)
  expect_named(step$settings, c("subject_dates", "subject_ids"))
  expect_equal(step$settings$subject_dates$max_days, 365)
  expect_true(step$settings$subject_ids$seeded)
  expect_false(any(grepl("123|456", unlist(step$settings))))
})

test_that("anon() on a step redacts its data and keeps the log-only presentation", {
  d <- data.frame(USUBJID = c("01-701-1015", "01-701-1023"), XDTC = c("2020-01-01", "2020-02-01"),
                  NOTE = c("Alice", "Bob"))
  step <- anon_subject_dates(d, seed = 1)
  redacted <- anon(step, pattern_list = list("[NAME]" = "Alice"))
  expect_s3_class(redacted, "anon_deid_step")
  expect_equal(redacted$data$NOTE, c("[NAME]", "Bob"))
  shown <- c(capture.output(print(redacted)), as_anon_json(redacted))
  expect_false(any(grepl("01-701", shown, fixed = TRUE)))
  nested <- anon(list(step = step))
  expect_s3_class(nested$step, "anon_deid_step")
})

test_that("duplicate dataset names are rejected", {
  two <- list(dm = data.frame(USUBJID = "a"), dm = data.frame(USUBJID = "b"))
  expect_error(anon_subject_ids(two), "must be unique")
  expect_error(anon_subject_dates(two), "must be unique")
  expect_error(anon_risk(list(dm = data.frame(USUBJID = "a", X = 1), dm = data.frame(USUBJID = "b", X = 1)), list(dm = "X")), "must be unique")
})

test_that("value labels are dropped from shifted QVAL", {
  skip_if_not_installed("haven")
  q <- data.frame(USUBJID = "a", QNAM = "X")
  q$QVAL <- haven::labelled("2014-02-10", labels = c(D = "2014-02-10"))
  out <- anon_subject_dates(list(supp = q), seed = 1)$data$supp$QVAL
  expect_null(attr(out, "labels"))
  expect_false(as.character(out) == "2014-02-10")
})

test_that("a step nested in a list serializes as its log", {
  step <- anon_subject_ids(data.frame(USUBJID = c("01-701-1015", "01-701-1023")), seed = 1)
  json <- as_anon_json(list(step = step))
  expect_match(json, '"kind": "anon_deid_step"', fixed = TRUE)
  expect_false(grepl("01-701", json, fixed = TRUE))
})

test_that("native dates shifted outside years 0001-9999 are removed and counted", {
  # Whichever way the 40-day offset goes, one of the two dates leaves the range
  d <- data.frame(USUBJID = c("a", "a"), D = as.Date(c("9999-12-31", "0001-01-01")))
  d$T <- as.POSIXct(c("9999-12-31 08:00:00", "0001-01-01 08:00:00"), tz = "UTC")
  msgs <- character(0)
  result <- withCallingHandlers(
    anon_subject_dates(d, vars = c("D", "T"), min_days = 40, max_days = 40, seed = 3),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(sum(is.na(result$data$D)), 1)
  expect_equal(sum(is.na(result$data$T)), 1)
  expect_true(any(grepl("outside years 0001-9999", msgs)))
  expect_equal(result$log$n_removed[result$log$variable %in% "D"], 1L)
})

test_that("QVAL dates that can't be shifted are removed and logged as removed", {
  qv <- data.frame(USUBJID = c("a", "b"), QNAM = "X", QVAL = c("9999-12-31", "0001-01-01"))
  msgs <- character(0)
  result <- withCallingHandlers(
    anon_subject_dates(list(supp = qv), min_days = 40, max_days = 40, seed = 3),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  row <- result$log[result$log$variable %in% "QVAL", ]
  # One of the two always falls outside 0001-9999 whichever way it moves
  expect_equal(row$n_changed + row$n_removed, 2L)
  expect_gte(row$n_removed, 1L)
  expect_true(any(grepl("outside years 0001-9999", msgs)))
  expect_true(any(is.na(result$data$supp$QVAL)))
})

test_that("anon() on a step applies its patterns to the log and settings", {
  step <- anon_subject_dates(list(Alice = data.frame(USUBJID = c("a", "b"), XDTC = c("2020-01-01", "2020-02-01"))),
                             vars = "XDTC", seed = 1)
  redacted <- anon(step, pattern_list = list("[NAME]" = "Alice"))
  expect_named(redacted$data, "[NAME]")
  expect_false(any(grepl("Alice", unlist(redacted$log[c("dataset", "variable", "detail")]), fixed = TRUE)))
  expect_equal(redacted$log$n_changed, step$log$n_changed)
  expect_false(grepl("Alice", as_anon_json(redacted), fixed = TRUE))
  expect_false(any(grepl("Alice", capture.output(print(redacted)), fixed = TRUE)))
})

test_that("duplicate column names are rejected", {
  d <- data.frame(USUBJID = "a", AESTDTC = "2020-01-01", AESTDTC = "2020-02-01", check.names = FALSE)
  expect_error(anon_subject_dates(d), "`AESTDTC` repeats")
  expect_error(anon_subject_ids(list(ae = d)), "Column names in `ae` must be unique")
  expect_error(anon_risk(d, quasi = "USUBJID", key = "AESTDTC"), "must be unique")
})
