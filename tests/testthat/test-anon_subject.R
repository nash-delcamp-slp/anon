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
  expect_error(anon_secrets(anon_subject_ids(data, seed = 4)), "holds no secrets")
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
    expect_warning(anon_subject_dates(d, seed = 13), "contain dates that were not shifted: ae.AESTTPT"),
    "Datasets without `USUBJID` kept their values unchanged but contain identifiers or dates: raw \\(usubjid, VISDTC\\)"
  )
  expect_warning(anon_subject_ids(d, seed = 13), "raw \\(SUBJID, usubjid\\)")
})

test_that("RSUBJID is mapped through the pseudonyms; other key columns are flagged", {
  rel <- list(
    dm = data.frame(USUBJID = c("S-1", "S-2")),
    relsub = data.frame(USUBJID = c("S-1", "S-2"), RSUBJID = c("S-2", "S-1"), NOTE = c("S-1", "x"))
  )
  expect_warning(result <- anon_subject_ids(rel, seed = 14), "still contain original `USUBJID` values, possibly inside text: relsub.NOTE")
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

test_that("SPSS value labels and user-defined missing values are dropped from ID columns", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("a", "b", "c"))
  d$SUBJID <- haven::labelled_spss(
    c(1015, 1028, -9), labels = c("Pat 1015" = 1015), na_values = c(-9, 1028), na_range = c(-99, -10)
  )
  out <- anon_subject_ids(d, seed = 1)$data$SUBJID
  expect_null(attr(out, "labels"))
  expect_null(attr(out, "na_values"))
  expect_null(attr(out, "na_range"))
  expect_false(any(c(1015, 1028, -9) %in% c(vctrs::vec_data(out), unlist(attributes(out)))))
  expect_equal(sum(is.na(out)), 2)
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
  # Names can hold keys, so the error gives positions.
  expect_error(anon_subject_dates(d), "Column names in dataset 1 of `data` must be unique; column 3 repeats")
  expect_error(anon_subject_ids(list(x = d[1], ae = d)), "dataset 2 of `data` must be unique")
  expect_error(anon_risk(d, quasi = "USUBJID", key = "AESTDTC"), "must be unique")
})

test_that("four-digit numbers in mixed columns are not shifted as years", {
  fa <- data.frame(USUBJID = "a", FAORRES = c("2014-03-05", "1500", "2000", "POSITIVE"))
  expect_warning(
    out <- anon_subject_dates(fa, vars = "FAORRES", seed = 1)$data$FAORRES,
    "bare four-digit numbers that were kept"
  )
  expect_equal(out[2:4], c("1500", "2000", "POSITIVE"))
  expect_false(out[1] == "2014-03-05")

  supp <- data.frame(USUBJID = "a", QNAM = "X", QVAL = c("1000", "1000/2000", "2014-03-05"))
  expect_warning(out <- anon_subject_dates(list(suppx = supp), seed = 1)$data$suppx$QVAL, "suppx.QVAL")
  expect_equal(out[1:2], c("1000", "1000/2000"))
  expect_warning(
    out <- anon_subject_dates(list(suppx = supp), vars = "QVAL", seed = 1)$data$suppx$QVAL,
    "suppx.QVAL"
  )
  expect_equal(out[1:2], c("1000", "1000/2000"))
})

test_that("date columns left out of vars, SAS-formatted dates and dates in text are flagged", {
  ad <- data.frame(USUBJID = "a", RFSTDTC = "2014-01-01", TRTSDT = as.Date("2014-01-02"),
                   COVAL = "Seen on 2014-01-05")
  ad$NUMDT <- 19000
  attr(ad$NUMDT, "format.sas") <- "DATE9"
  expect_warning(
    expect_warning(
      anon_subject_dates(ad, vars = "RFSTDTC", seed = 1),
      "not shifted: data.TRTSDT, data.COVAL\\."
    ),
    "SAS date or datetime format and were not shifted: data.NUMDT\\. Convert"
  )
  attr(ad$NUMDT, "format.sas") <- "WORDDATE18"
  expect_warning(
    expect_warning(anon_subject_dates(ad[c("USUBJID", "RFSTDTC", "NUMDT")], seed = 1), "data.NUMDT"),
    NA
  )
})

test_that("dates kept in mixed columns and QVAL are flagged", {
  fa <- data.frame(USUBJID = "a", FAORRES = c("2014-03-05", "05JAN2014", "Visit 2014-01-05", "POSITIVE"))
  expect_warning(anon_subject_dates(fa, vars = "FAORRES", seed = 1), "not shifted: data.FAORRES")
  ok <- data.frame(USUBJID = "a", FAORRES = c("2014-03-05", "POSITIVE"))
  expect_no_warning(anon_subject_dates(ok, vars = "FAORRES", seed = 1))
  text <- data.frame(USUBJID = "a", RFSTDTC = "2014-01-01",
                     COM = c("seen 5 Jan 2014", "Jan 5, 2014", "dob 1/2/1980", "MAY 2 TABLETS"))
  expect_warning(anon_subject_dates(text, seed = 1), "not shifted: data.COM")
  text$COM <- "MAY 2 TABLETS, 10 mg"
  expect_no_warning(anon_subject_dates(text, seed = 1))

  supp <- data.frame(USUBJID = "a", QNAM = c("X", "Y"), QVAL = c("Diagnosed 2009-03-04", "2014-03-05"))
  expect_warning(anon_subject_dates(list(suppx = supp), seed = 1), "not shifted: suppx.QVAL")
})

test_that("a vars column of bare years is shifted by whole years", {
  dm <- data.frame(USUBJID = c("a", "b"), BRTHYR = c("1956", "1971"))
  result <- anon_subject_dates(dm, vars = "BRTHYR", keep_secrets = TRUE, seed = 1)
  off <- anon_secrets(result)$date_offsets
  years <- as.integer(result$data$BRTHYR) - c(1956L, 1971L)
  expect_true(all(years != 0))
  expect_equal(sign(years), sign(off$offset_days[match(dm$USUBJID, off$key)]))
})

test_that("leftover-key check: invalid UTF-8 keys, latin1 text, unkeyed datasets, word boundaries", {
  bad <- data.frame(USUBJID = c("ab\xe9", "01-702"), X = "x")
  expect_no_error(anon_subject_ids(bad, seed = 1))

  co <- data.frame(USUBJID = c("01-701", "01-702"), COM = c(iconv("Patient 01-701 caf\u00e9", "UTF-8", "latin1"), "z"))
  expect_warning(anon_subject_ids(co, seed = 1), "inside text: data.COM")

  d <- list(
    dm = data.frame(USUBJID = c("01-701-1015", "01-701-1023")),
    notes = data.frame(NOTE = "Patient 01-701-1015 withdrew")
  )
  expect_warning(
    expect_warning(anon_subject_ids(d, seed = 1), "inside text: notes.NOTE"),
    NA
  )

  # Replaced also columns, and keys that only appear inside longer numbers, are not flagged.
  num <- list(
    dm = data.frame(USUBJID = as.character(101:120), SUBJID = as.character(101:120)),
    lb = data.frame(USUBJID = as.character(101:120), LBORRES = "5101.5", LBSPID = "SPEC-1010")
  )
  expect_no_warning(anon_subject_ids(num, seed = 1))
})

test_that("original keys inside text are flagged", {
  co <- data.frame(USUBJID = "01-701-1015", COVAL = "Ref subject 01-701-1015")
  expect_warning(anon_subject_ids(co, seed = 1), "possibly inside text: data.COVAL")
})

test_that("NLP on a list holding a step with secrets works", {
  st <- anon_subject_ids(data.frame(USUBJID = c("a", "b")), keep_secrets = TRUE, seed = 1)
  local_mocked_bindings(nlp_get_entities = function(x, entity_types, return_list = FALSE) {
    stats::setNames(rep(list(character(0)), length(entity_types)), entity_types)
  })
  expect_no_error(anon(list(st = st), nlp_auto = nlp_auto(person = TRUE)))
})

test_that("steps can't be combined with c(), with a clear message", {
  st <- anon(anon_subject_ids(data.frame(USUBJID = "a"), seed = 1))
  expect_error(c(st, anon("x")), "combine `step\\$data` instead")
  expect_error(c(anon("x"), st), "combine `step\\$data` instead")
})

test_that("summaries and reports describe a step's data", {
  data <- list(dm = data.frame(USUBJID = c("a", "b"), AGE = c(60, 70)), vs = data.frame(USUBJID = "a", X = 1))
  st <- anon_subject_ids(data, seed = 1)
  expect_setequal(anon_data_summary(st)$data_frames$structure$name, c("dm", "vs"))
  expect_setequal(anon_report(st)$object_names, c("dm", "vs"))
  expect_equal(anon_data_summary(list(st = anon_subject_ids(data$dm, seed = 1)))$data_frames$structure$name, "st")
})

test_that("missing values in also columns stay missing", {
  out <- anon_subject_ids(data.frame(USUBJID = c("a", "b", "c"), SUBJID = c("1", NA, "")), seed = 1)
  expect_equal(sum(is.na(out$data$SUBJID) | out$data$SUBJID == ""), 2)
  expect_equal(out$log$n_changed[out$log$variable %in% "SUBJID"], 1L)
})

test_that("anon(step, check_names = FALSE) leaves log names alone", {
  st <- anon_subject_dates(list(Boston = data.frame(USUBJID = "a", XDTC = "2020-01-01")), seed = 1)
  a <- anon(st, pattern_list = list("[CITY]" = "Boston"), check_names = FALSE)
  expect_named(a$data, "Boston")
  expect_true("Boston" %in% a$log$dataset)
})

test_that("user-defined missing values on keys become missing, including real keys", {
  skip_if_not_installed("haven")
  d <- data.frame(row = 1:4)
  d$USUBJID <- haven::labelled_spss(c(1015, 1023, -9, 1031), na_values = c(-9, 1031))
  out <- anon_subject_ids(d, seed = 1)$data
  expect_null(attr(out$USUBJID, "na_values"))
  expect_false(any(c(1015, 1023, -9, 1031) %in% vctrs::vec_data(out$USUBJID)))
  expect_equal(sum(is.na(out$USUBJID)), 2)
})

test_that("anon() on a step drops its secrets", {
  st <- anon_subject_ids(data.frame(USUBJID = c("a", "b")), keep_secrets = TRUE, seed = 1)
  a <- anon(st, pattern_list = list("a"))
  expect_null(a$secrets)
  expect_error(anon_secrets(a), "holds no secrets")
  expect_s3_class(anon_secrets(st)$subject_ids, "tbl_df")
})

test_that("steps inside an environment or list are described dataset by dataset", {
  d <- list(
    dm = data.frame(USUBJID = c("a", "b"), AGE = c(30, 40)),
    vs = data.frame(USUBJID = c("a", "b"), VSORRES = c("1", "2"))
  )
  e <- new.env()
  assign("step", anon_subject_ids(d, keep_secrets = TRUE, seed = 1), envir = e)
  assign("one", anon_subject_ids(d$dm, seed = 1), envir = e)
  expect_setequal(anon_inventory(e)$name, c("one", "step$dm", "step$vs"))
  expect_named(anon_data_summary(e)$data_frames$variables, c("one", "step$dm", "step$vs"), ignore.order = TRUE)
  expect_named(anon_data_summary(e, selection = "step")$data_frames$variables, c("step$dm", "step$vs"))
  expect_named(anon_data_summary(e, selection = "step$vs")$data_frames$variables, "step$vs")
  expect_error(anon_data_summary(e, selection = "nope"), "Unknown objects")
  state <- anon_app_initial_state(e)
  expect_setequal(state$raw_names, c("one", "step$dm", "step$vs"))
})

test_that("bare years in a vars column are shifted only when the column holds only years", {
  dm <- data.frame(USUBJID = c("a", "b", "c"), BRTHYR = c("1980", "1975", "UNK"))
  expect_warning(
    out <- anon_subject_dates(dm, vars = "BRTHYR", seed = 1)$data$BRTHYR,
    "bare four-digit numbers that were kept.*data.BRTHYR"
  )
  expect_equal(out, dm$BRTHYR)

  # The rule is decided across all datasets with the column. (Dataset names
  # that equal a key would be shown as "[KEY]" in the warning.)
  d <- list(
    dm1 = data.frame(USUBJID = "a", BRTHYR = "1980"),
    dm2 = data.frame(USUBJID = "b", BRTHYR = c("1975", "UNK"))
  )
  expect_warning(out <- anon_subject_dates(d, vars = "BRTHYR", seed = 1)$data, "dm1.BRTHYR, dm2.BRTHYR")
  expect_equal(out$dm1$BRTHYR, "1980")

  d$dm2$BRTHYR <- c("1975", NA)
  out <- anon_subject_dates(d, vars = "BRTHYR", seed = 1)$data
  expect_false(out$dm1$BRTHYR == "1980")
  expect_false(out$dm2$BRTHYR[1] == "1975")
})

test_that("keys made only of digits are not searched for in text", {
  d <- list(
    dm = data.frame(USUBJID = as.character(1:30), VISIT = "WEEK 2", DTC = "2014-01-15"),
    ts = data.frame(TSVAL = "15")
  )
  expect_no_warning(anon_subject_ids(d, seed = 1))
})

test_that("anon_subject_dates() treats user-defined missing keys and dates as missing", {
  skip_if_not_installed("haven")
  d <- data.frame(XXDTC = c("2020-01-01", "2020-01-02", "2020-01-03"))
  d$USUBJID <- haven::labelled_spss(c("A1", "A2", "-9"), na_values = "-9")
  d$AEDTC <- haven::labelled_spss(c("2020-01-01", "UNKN", "2020-01-03"), na_values = "UNKN")
  expect_warning(
    result <- anon_subject_dates(d, keep_secrets = TRUE, seed = 1),
    "date value\\(s\\) in rows without a `USUBJID` were removed"
  )
  expect_setequal(anon_secrets(result)$date_offsets$key, c("A1", "A2"))
  expect_true(is.na(vctrs::vec_data(result$data$AEDTC)[2]))
  expect_s3_class(result$data$USUBJID, "haven_labelled_spss")
})

test_that("SAS formats for parts of a date are flagged, and dotted dataset names are kept", {
  ad <- data.frame(USUBJID = "a", RFSTDTC = "2014-01-01", YR = 19000, WK = 19000)
  attr(ad$YR, "format.sas") <- "YEAR4"
  attr(ad$WK, "format.sas") <- "WEEKDATX"
  expect_warning(anon_subject_dates(ad, seed = 1), "data.YR, data.WK")

  d <- list(dm = data.frame(USUBJID = "a"), my.ts = data.frame(X = "2014-01-01"))
  expect_warning(anon_subject_dates(d, seed = 1), "my.ts \\(X\\)")
})

test_that("step datasets can't clash with other object names", {
  st <- anon_subject_ids(list(dm = data.frame(USUBJID = "a"), vs = data.frame(USUBJID = "a")), seed = 1)
  e <- new.env()
  assign("step", st, envir = e)
  assign("step$dm", data.frame(USUBJID = "01-701-1015"), envir = e)
  expect_error(anon_inventory(e), "clash with datasets of a de-identification step: step\\$dm")

  dup <- list(a = data.frame(x = 1), a = data.frame(y = 2))
  expect_equal(nrow(anon_inventory(dup)), 2)

  e2 <- new.env()
  assign("step", st, envir = e2)
  assign("other", data.frame(x = 1), envir = e2)
  expect_equal(anon_app_initial_state(e2, initial_selection = "step")$selected, c("step$dm", "step$vs"))
})

test_that("the years-only rule handles factors, blank datasets and Date columns", {
  dm <- data.frame(USUBJID = c("a", "b"))
  dm$BRTHYR <- factor(c("1980", "1975"))
  out <- anon_subject_dates(dm, vars = "BRTHYR", seed = 1)$data$BRTHYR
  expect_false(any(as.character(out) %in% c("1980", "1975")))

  d <- list(
    a = data.frame(USUBJID = c("a", "b"), BRTHYR = c("1980", "1975")),
    b = data.frame(USUBJID = "c", BRTHYR = ""),
    c = data.frame(USUBJID = "d", BRTHYR = as.Date("2014-01-01"))
  )
  expect_no_warning(out <- anon_subject_dates(d, vars = "BRTHYR", seed = 1)$data)
  expect_false(any(out$a$BRTHYR %in% c("1980", "1975")))
})

test_that("a column named like the key in another case is checked in keyed datasets", {
  d <- list(dm = data.frame(USUBJID = c("01-701-1015", "01-701-1023"), usubjid = c("01-701-1015", "01-701-1023")))
  expect_warning(anon_subject_ids(d, seed = 1), "another case and were not replaced: dm.usubjid")
})

test_that("bare years kept in QVAL are warned about", {
  supp <- data.frame(USUBJID = "a", QNAM = c("BRTHYR", "X"), QVAL = c("1980", "2014-01-05"))
  expect_warning(
    out <- anon_subject_dates(list(suppdm = supp), seed = 1)$data$suppdm$QVAL,
    "QVAL columns hold bare four-digit numbers.*suppdm.QVAL"
  )
  expect_equal(out[1], "1980")
})

test_that("a vars column of user-missing codes is accepted", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("a", "b"), RFSTDTC = "2014-01-01")
  d$XDT <- haven::labelled_spss(c("UNK", "UNK"), na_values = "UNK")
  expect_no_error(anon_subject_dates(d, vars = c("RFSTDTC", "XDT"), seed = 1))
})

test_that("drug names and other text don't look like month-name dates", {
  co <- data.frame(USUBJID = "a", RFSTDTC = "2014-01-01",
                   CMTRT = c("Take 1 AUGMENTIN 50 mg", "10 NOVOLOG 10 units", "Grade 3 DECREASED 25%"))
  expect_no_warning(anon_subject_dates(co, seed = 1))
  co$CMTRT <- "given 06-MAR-2014T10:00"
  expect_warning(anon_subject_dates(co, seed = 1), "data.CMTRT")
})

test_that("custom SAS formats with date-like names aren't flagged", {
  ad <- data.frame(USUBJID = "a", RFSTDTC = "2014-01-01", GRP = 1, DT = 19000)
  attr(ad$GRP, "format.sas") <- "DAYGRP"
  attr(ad$DT, "format.sas") <- "MDYAMPM"
  expect_warning(anon_subject_dates(ad, seed = 1), "not shifted: data.DT\\.")
})

test_that("step selection uses where names came from; empty and duplicate steps", {
  st <- anon_subject_ids(list(dm = data.frame(USUBJID = "a"), vs = data.frame(USUBJID = "a")), seed = 1)
  e <- new.env()
  assign("a$b", data.frame(x = 1), envir = e)
  assign("step", st, envir = e)
  expect_equal(anon_app_initial_state(e, initial_selection = "a")$selected, character(0))
  expect_equal(anon_app_initial_state(e, initial_selection = "step")$selected, c("step$dm", "step$vs"))

  empty <- st
  empty$data <- list()
  expect_equal(nrow(anon_inventory(list(x = data.frame(y = 1), empty = empty))), 1)
  expect_error(anon_inventory(list(s = st, s = st)), "same name: s")
})

test_that("partial = \"remove\" keeps bare numbers in mixed columns", {
  fa <- data.frame(USUBJID = c("A", "B"), FAORRES = c("1000", "2014-03-02"))
  expect_warning(
    out <- anon_subject_dates(fa, vars = "FAORRES", partial = "remove", seed = 1)$data$FAORRES,
    "bare four-digit numbers that were kept"
  )
  expect_equal(out[1], "1000")
  expect_false(out[2] == "2014-03-02")
})

test_that("identifier columns named in another case are named in a warning", {
  digits <- data.frame(USUBJID = c("1001", "1002"), usubjid = c("1001", "1002"))
  expect_warning(anon_subject_ids(digits, seed = 1), "another case and were not replaced: data.usubjid")
  num <- data.frame(USUBJID = c(1001, 1002), rsubjid = c("1002", "1001"))
  expect_warning(anon_subject_ids(num, seed = 1), "data.rsubjid")
  sub <- data.frame(USUBJID = c("a", "b"), SUBJID = c("1", "2"), subjid = c("1", "2"))
  expect_warning(anon_subject_ids(sub, seed = 1), "data.subjid")
  unkeyed <- list(dm = data.frame(USUBJID = "a"), raw = data.frame(subjid = "1"))
  expect_warning(anon_subject_ids(unkeyed, seed = 1), "raw \\(subjid\\)")
})

test_that("bare years in QVAL get their own warning", {
  supp <- data.frame(USUBJID = "a", QNAM = "BRTHYR", QVAL = "1980")
  expect_warning(anon_subject_dates(list(suppdm = supp), seed = 1), "QVAL columns hold bare four-digit numbers.*suppdm.QVAL")
})

test_that("more date forms in text are flagged, without common dosing text", {
  forms <- c("2014/01/05", "01.05.2014", "05/JAN/2014", "UNK-JAN-2014", "2014JAN05", "Jan 2014",
             "5th January 2014", "1/5/14")
  for (f in forms) {
    co <- data.frame(USUBJID = "a", RFSTDTC = "2014-01-01", COM = paste("seen", f))
    expect_warning(anon_subject_dates(co, seed = 1), "data.COM", info = f)
  }
  co <- data.frame(USUBJID = "a", RFSTDTC = "2014-01-01",
                   COM = c("1 MAY 12 HRS", "Take 2 mar 10", "1/2 tablet", "10-20 mg", "BP 120/80", "1.5-2.0"))
  expect_no_warning(anon_subject_dates(co, seed = 1))

  ad <- data.frame(USUBJID = "a", RFSTDTC = "2014-01-01", MO = 19000)
  attr(ad$MO, "format.sas") <- "MONTH2"
  expect_warning(anon_subject_dates(ad, seed = 1), "data.MO")
})

test_that("anon_subject_ids() replaces keys in attributes and names with pseudonyms", {
  dm <- data.frame(USUBJID = c("S-01", "S-02"), AGE = c(30, 40), ROWID = 1:2)
  attr(dm$AGE, "label") <- "Age of S-01 (not S-010)"
  comment(dm$AGE) <- "S-02"
  attr(dm$AGE, "note") <- c("first S-01", NA)
  attr(dm$AGE, "format.sas") <- "S-01"
  attr(dm, "label") <- "Demographics with S-02"
  dm[["S-02"]] <- 1
  ta <- data.frame(ARM = c("A", "B"), row.names = c("S-01", "S-02"))
  step <- anon_subject_ids(list(dm = dm, ta = ta, "S-01" = data.frame(X = 1)), keep_secrets = TRUE, seed = 3)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  out <- step$data

  expect_named(out, c("dm", "ta", new[["S-01"]]))
  expect_equal(attr(out$dm$AGE, "label"), paste0("Age of ", new[["S-01"]], " (not S-010)"))
  expect_equal(comment(out$dm$AGE), new[["S-02"]])
  expect_equal(attr(out$dm$AGE, "note"), c(paste("first", new[["S-01"]]), NA))
  # Formats are not text about the data.
  expect_equal(attr(out$dm$AGE, "format.sas"), "S-01")
  expect_equal(attr(out$dm, "label"), paste("Demographics with", new[["S-02"]]))
  expect_named(out$dm, c("USUBJID", "AGE", "ROWID", new[["S-02"]]))
  expect_equal(rownames(out$ta), unname(new[c("S-01", "S-02")]))

  # The log counts the rewrites by dataset, without the original keys.
  rows <- step$log[step$log$dataset %in% names(out) & grepl("in attributes and names", step$log$detail), ]
  expect_equal(rows$dataset, c("dm", "ta", new[["S-01"]]))
  expect_equal(rows$n_changed, c(5L, 2L, 1L))
  expect_match(rows$detail[1], "original keys replaced with pseudonyms in attributes and names: 4 attribute value\\(s\\), 1 name\\(s\\)")
  expect_false(any(grepl("S-0[12]", unlist(step$log[c("dataset", "variable", "detail")]))))
})

test_that("anon_subject_ids() replaces keys in value labels and missing-value codes of other columns", {
  skip_if_not_installed("haven")
  dm <- data.frame(USUBJID = c("S-01", "S-02"))
  dm$GRP <- haven::labelled(c(1, 2), labels = c("S-01" = 1, Other = 2))
  dm$ARM <- haven::labelled(c("S-02", "x"), labels = c("Group of S-02" = "S-02", "S-01 arm" = "S-01"))
  dm$SITE <- haven::labelled_spss(c("A", "B"), na_values = "S-01")
  # Values are data: the key left in ARM is flagged, not rewritten, and so is
  # the code that labels it.
  expect_warning(
    expect_warning(
      step <- anon_subject_ids(dm, keep_secrets = TRUE, seed = 4),
      "still contain original `USUBJID` values.*: data.ARM"
    ),
    "keep value-label or missing-value codes .*: data.ARM\\."
  )
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  out <- step$data[match(new[c("S-01", "S-02")], step$data$USUBJID), ]

  expect_equal(attr(out$GRP, "labels"), stats::setNames(c(1, 2), c(new[["S-01"]], "Other")))
  expect_equal(
    attr(out$ARM, "labels"),
    stats::setNames(c("S-02", new[["S-01"]]), c(paste("Group of", new[["S-02"]]), paste(new[["S-01"]], "arm")))
  )
  expect_equal(attr(out$SITE, "na_values"), new[["S-01"]])
  expect_equal(vctrs::vec_data(out$ARM), c("S-02", "x"))
})

test_that("keys made only of digits, and numeric keys, are not rewritten in attributes", {
  digits <- data.frame(USUBJID = c("1001", "1002"), AGE = 1:2)
  attr(digits$AGE, "label") <- "Dose 1001 mg"
  expect_equal(attr(anon_subject_ids(digits, seed = 1)$data$AGE, "label"), "Dose 1001 mg")
  num <- data.frame(USUBJID = c(1001, 1002), AGE = 1:2)
  attr(num$AGE, "label") <- "Dose 1001 mg"
  expect_equal(attr(anon_subject_ids(num, seed = 1)$data$AGE, "label"), "Dose 1001 mg")
})

test_that("keys in list columns are flagged, not rewritten", {
  dm <- data.frame(USUBJID = c("S-01", "S-02"))
  dm$LINKS <- I(list(c("x", "S-02"), list(factor("y"))))
  expect_warning(out <- anon_subject_ids(dm, seed = 1)$data, "still contain original `USUBJID` values.*: data.LINKS")
  expect_true("S-02" %in% unlist(out$LINKS))
  dm$LINKS <- I(list("x", list(1)))
  expect_no_warning(anon_subject_ids(dm, seed = 1))
})

test_that("anon_subject_dates() replaces dates in attributes and names with [DATE]", {
  dm <- data.frame(USUBJID = c("a", "b"), RFSTDTC = c("2014-01-02", "2013-07-19"), AGE = c(30, 40))
  attr(dm$AGE, "label") <- "Age at 2014-01-02, seen 05JAN2014 and 1/5/14"
  attr(dm, "note") <- "Cut on Jan 5, 2014"
  dm[["2014-02"]] <- 1
  dm[["2014-03"]] <- 2
  dm[["[DATE]"]] <- 3
  ts <- data.frame(TSVAL = "x", row.names = "2012-07-01")
  attr(ts$TSVAL, "label") <- "Planned 2012-07-01"
  step <- anon_subject_dates(list(dm = dm, ts = ts), seed = 5)
  out <- step$data

  expect_equal(attr(out$dm$AGE, "label"), "Age at [DATE], seen [DATE] and [DATE]")
  expect_equal(attr(out$dm, "note"), "Cut on [DATE]")
  # Rewritten names that clash are made unique; the others keep their names.
  expect_named(out$dm, c("USUBJID", "RFSTDTC", "AGE", "[DATE]_1", "[DATE]_2", "[DATE]"))
  # Datasets without the key keep their values, but not dates in attributes.
  expect_equal(rownames(out$ts), "[DATE]")
  expect_equal(attr(out$ts$TSVAL, "label"), "Planned [DATE]")
  expect_equal(out$ts$TSVAL, structure("x", label = "Planned [DATE]"))

  rows <- step$log[grepl("in attributes and names", step$log$detail), ]
  expect_equal(rows$dataset, c("dm", "ts"))
  expect_equal(rows$n_changed, c(4L, 2L))
  expect_match(rows$detail, "^dates replaced with \\[DATE\\] in attributes and names")
})

test_that("a later step renames the datasets and columns named in earlier steps' logs", {
  data <- list(dm = data.frame(USUBJID = c("S-01", "S-02"), RFSTDTC = c("2014-01-02", "2013-07-19")))
  data[["S-01"]] <- data.frame(X = 1)
  step <- anon_subject_ids(anon_subject_dates(data, seed = 1), seed = 2)
  expect_false(any(grepl("S-01", step$log$dataset)))
  expect_equal(sum(step$log$detail == "no `USUBJID` column; values left unchanged"), 2)
})

test_that("a key that starts another is not taken for it", {
  d <- data.frame(USUBJID = c("S-1", "S-1-2"), V = 1:2)
  attr(d$V, "label") <- "S-1-2 and S-1"
  step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 3)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  expect_equal(attr(step$data$V, "label"), paste(new[["S-1-2"]], "and", new[["S-1"]]))
  expect_warning(anon_subject_ids(data.frame(USUBJID = c("S-1", "S-1-2"), N = c("S-1-2", "x")), seed = 1), "data.N")
})

test_that("grouped tibbles keep their label, attributes and grouping", {
  g <- dplyr::group_by(tibble::tibble(USUBJID = c("S-01", "S-02", "S-01"), `S-02` = 1:3, A = 1), USUBJID, `S-02`)
  attr(g, "label") <- "Vital Signs"
  attr(g, "note") <- "S-01"
  step <- anon_subject_ids(list(vs = g), keep_secrets = TRUE, seed = 1)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  out <- step$data$vs
  expect_s3_class(out, "grouped_df")
  expect_equal(attr(out, "label"), "Vital Signs")
  expect_equal(attr(out, "note"), new[["S-01"]])
  expect_equal(dplyr::group_vars(out), c("USUBJID", new[["S-02"]]))
  expect_equal(names(dplyr::group_data(out)), c("USUBJID", new[["S-02"]], ".rows"))
  expect_false(any(c("S-01", "S-02") %in% unlist(dplyr::group_data(out)[1])))
  dates <- anon_subject_dates(list(vs = dplyr::group_by(tibble::tibble(USUBJID = "a", X = 1), X)), seed = 1)$data$vs
  expect_s3_class(dates, "grouped_df")
  # Nothing to rewrite: the dataset is returned as it was.
  te <- dplyr::group_by(tibble::tibble(ARMCD = c("A", "B")), ARMCD)
  attr(te, "label") <- "Trial Elements"
  out <- anon_subject_dates(list(dm = data.frame(USUBJID = "a"), te = te), seed = 1)$data$te
  expect_identical(out, te)
})

test_that("keys are rewritten at any depth of attributes, in vector names and dimnames", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"), W = c("S-01" = 1, "S-02" = 2))
  d$M <- matrix(1:4, 2, dimnames = list(c("S-01", "x"), c(S = "S-02", "y")))
  attr(d$W, "meta") <- list(by = structure(c(who = "S-01"), note = "S-02"), f = factor("S-02"))
  attr(d$W, "S-01") <- "an attribute named by a key"
  data <- list(dm = d)
  attr(data, "note") <- c(n = "made from S-02")
  step <- anon_subject_ids(data, keep_secrets = TRUE, seed = 2)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  out <- step$data$dm
  s1 <- new[["S-01"]]
  s2 <- new[["S-02"]]
  expect_equal(names(out$W), unname(new[c("S-01", "S-02")]))
  expect_equal(dimnames(out$M), list(c(s1, "x"), c(S = s2, "y")))
  meta <- attr(out$W, "meta")
  expect_equal(meta$by, structure(c(who = s1), note = s2))
  expect_equal(levels(meta$f), s2)
  expect_equal(attr(out$W, s1), "an attribute named by a key")
  expect_null(attr(out$W, "S-01"))
  expect_equal(attr(step$data, "note"), c(n = paste("made from", s2)))
  expect_true(any(is.na(step$log$dataset) & grepl("in attributes and names: 1 attribute", step$log$detail)))
})

test_that("keys and dates inside list-column elements are flagged, not rewritten", {
  d <- tibble::tibble(
    USUBJID = c("S-01", "S-02"), RFSTDTC = c("2014-01-01", "2014-02-01"),
    L = list(list(`S-02` = 1), structure(2, note = "x")),
    D = list(1, structure(2, note = "seen 2014-01-05"))
  )
  expect_warning(out <- anon_subject_ids(d, seed = 1)$data, "still contain original `USUBJID` values.*: data.L\\.")
  expect_true("S-02" %in% unlist(lapply(out$L, names)))
  expect_warning(anon_subject_dates(d, seed = 1), "contain dates that were not shifted: data.D\\.")
})

test_that("text that isn't valid UTF-8 is searched through a repaired copy; latin1 text is searched", {
  bad <- "Caf\xe9 S-01"
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1:2, T = c(bad, "ok"), RFSTDTC = "2014-01-01")
  attr(d$V, "label") <- bad
  expect_warning(
    step <- anon_subject_ids(list(dm = d), keep_secrets = TRUE, seed = 1),
    "^These columns still contain original `USUBJID` values, possibly inside text: dm.T\\."
  )
  # The rewritten label is the repaired text.
  expect_identical(attr(step$data$dm$V, "label"), paste0("Caf<e9> ", anon_secrets(step)$subject_ids$pseudonym[1]))
  # Text with nothing to rewrite keeps its bytes.
  attr(d$V, "label") <- "Caf\xe9"
  expect_identical(attr(suppressWarnings(anon_subject_ids(d, seed = 1))$data$V, "label"), "Caf\xe9")
  expect_no_warning(anon_subject_dates(data.frame(USUBJID = "a", RFSTDTC = "2014-01-01", T = "Caf\xe9"), seed = 1))
  latin <- "Caf\xe9 S-01"
  Encoding(latin) <- "latin1"
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1:2)
  attr(d$V, "label") <- latin
  step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 1)
  expect_equal(attr(step$data$V, "label"), paste0("Caf\u00e9 ", anon_secrets(step)$subject_ids$pseudonym[1]))
})

test_that("settings and log text naming renamed columns are rewritten", {
  d <- data.frame(USUBJID = c("AB-1", "AB-2"), `X2014-01-05DTC` = c("2014-01-05", "2014-02-01"), check.names = FALSE)
  d$`AB-1` <- 1:2
  step <- anon_subject_ids(anon_subject_dates(list(dm = d), vars = "X2014-01-05DTC", seed = 1), also = "AB-1", seed = 2)
  text <- c(capture.output(print(step)), as_anon_json(step), unlist(step$settings))
  expect_false(any(grepl("2014-01-05|AB-1", text)))
  expect_equal(step$settings$subject_dates$vars, "X[DATE]DTC")
  expect_equal(step$settings$subject_ids$also, "SUBJ-1")
  # The renamed date column was shifted and is named by its new name in the log.
  expect_named(step$data$dm, c("USUBJID", "X[DATE]DTC", "SUBJ-1"))
  expect_false(any(step$data$dm[["X[DATE]DTC"]] %in% d[["X2014-01-05DTC"]]))
  expect_true("X[DATE]DTC" %in% step$log$variable[step$log$step == "subject_dates"])
  expect_true("SUBJ-1" %in% step$log$variable[step$log$step == "subject_ids"])
})

test_that("a column renamed by a later step is renamed in earlier steps' logs", {
  d <- data.frame(USUBJID = c("S-01", "S-02"), `S-01_DTC` = c("2014-01-05", "2014-02-01"), check.names = FALSE)
  step <- anon_subject_ids(anon_subject_dates(d, seed = 1), seed = 2)
  shifted <- step$log[step$log$step == "subject_dates" & !is.na(step$log$variable), ]
  expect_equal(shifted$variable, setdiff(names(step$data), "USUBJID"))
  expect_false(any(grepl("S-0", unlist(step$log[c("variable", "detail")]))))
})

test_that("SDTM partial dates in attributes are replaced", {
  d <- data.frame(USUBJID = "a", V = 1)
  attr(d$V, "label") <- "from 2014-01--T08:00 and 2014---15; not 20140105 or 2014-1-5"
  out <- anon_subject_dates(d, seed = 1)$data
  expect_equal(attr(out$V, "label"), "from [DATE]T08:00 and [DATE]; not 20140105 or 2014-1-5")
})

test_that("value-label codes and missing values equal to a value are not rewritten", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("AB-1", "AB-2", "AB-3"))
  d$REF <- haven::labelled_spss(c("AB-1", "X", "AB-2"), labels = c("first subj" = "AB-1", other = "X"), na_values = "AB-2")
  d$RNG <- haven::labelled_spss(c("AB-15", "B", "C"), na_range = c("AB-1", "AB-3"))
  d$FREE <- haven::labelled_spss(c("Z", "Z", "Z"), labels = c(z = "AB-3"), na_range = c("AB-1", "AB-2"))
  expect_warning(
    expect_warning(step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 1), "still contain .*: data.REF\\."),
    "keep value-label or missing-value codes .*: data.REF, data.RNG\\."
  )
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  out <- step$data
  expect_equal(is.na(out$REF), is.na(d$REF))
  expect_equal(attr(out$REF, "labels"), attr(d$REF, "labels"))
  expect_equal(attr(out$REF, "na_values"), "AB-2")
  # "AB-15" lies in the range AB-1 to AB-3, so the range is kept.
  expect_equal(attr(out$RNG, "na_range"), c("AB-1", "AB-3"))
  expect_equal(attr(out$FREE, "labels"), c(z = new[["AB-3"]]))
  expect_equal(attr(out$FREE, "na_range"), unname(new[c("AB-1", "AB-2")]))
})

test_that("dates in value labels, missing values and ranges are replaced; structural attributes are not", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("a", "b"), RFSTDTC = c("2014-01-02", "2013-07-19"))
  d$GRP <- haven::labelled(c(1, 2), labels = c("cut 2014-01-02" = 1, other = 2))
  d$SITE <- haven::labelled_spss(c("S1", "S2"), labels = c(first = "2014-03-01"), na_values = "2014-01-05",
                                 na_range = c("2015-01-01", "2015-12-31"))
  d$N <- 1:2
  attr(d$N, "format.sas") <- "2014-01-01"
  # `units` is text on a number, and structural on a difftime.
  attr(d$N, "units") <- "2014-01-01"
  d$DT <- as.difftime(c(1, 2), units = "days")
  d$TM <- as.POSIXct(c("2014-01-02 10:00", "2013-07-19 10:00"), tz = "UTC")
  step <- anon_subject_dates(d, seed = 1)
  out <- step$data
  expect_equal(names(attr(out$GRP, "labels")), c("cut [DATE]", "other"))
  expect_equal(attr(out$SITE, "labels"), c(first = "[DATE]"))
  expect_equal(attr(out$SITE, "na_values"), "[DATE]")
  # Both ends become "[DATE]", so "S1" and "S2" are still outside the range.
  expect_equal(attr(out$SITE, "na_range"), c("[DATE]", "[DATE]"))
  expect_equal(attr(out$N, "format.sas"), "2014-01-01")
  expect_equal(attr(out$N, "units"), "[DATE]")
  expect_equal(attr(out$DT, "units"), "days")
  expect_equal(attr(out$TM, "tzone"), "UTC")
  expect_match(step$log$detail[step$log$dataset %in% "data" & is.na(step$log$variable)], "5 value label\\(s\\) or missing-value code\\(s\\)")
})

test_that("na_range codes that are keys are rewritten when no value falls in either range", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("AB-1", "AB-2"))
  d$C <- haven::labelled_spss(c("Z", "Z"), na_range = c("AB-1", "AB-2"))
  step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 1)
  expect_equal(attr(step$data$C, "na_range"), anon_secrets(step)$subject_ids$pseudonym)
})

test_that("rewritten names that clash are made unique in the ids step", {
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1)
  d[["S-01"]] <- 1
  d[["SUBJ-1"]] <- 2
  data <- list(dm = d, "SUBJ-2" = data.frame(X = 1), "S-02" = data.frame(Y = 1))
  step <- anon_subject_ids(data, keep_secrets = TRUE, seed = 1)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  # The seed gives S-01 SUBJ-1 and S-02 SUBJ-2, clashing with existing names.
  expect_equal(unname(new), c("SUBJ-1", "SUBJ-2"))
  expect_named(step$data$dm, c("USUBJID", "V", "SUBJ-1_1", "SUBJ-1"))
  expect_named(step$data, c("dm", "SUBJ-2", "SUBJ-2_1"))
  expect_equal(step$data[["SUBJ-2_1"]], data.frame(Y = 1))
})

test_that("packed data frame columns are data: searched and flagged, not rewritten", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"), RFSTDTC = c("2014-01-01", "2014-02-01"))
  d$P <- tibble::tibble(REF = c("S-02", "x"), SEEN = c("2014-01-05", "y"), Q = structure(1:2, label = "for S-01"))
  d$P$IN <- tibble::tibble(Z = as.Date("2014-01-05"))
  d$H <- tibble::tibble(`S-01` = 1)
  expect_warning(
    out <- anon_subject_ids(d, seed = 1)$data,
    "still contain original `USUBJID` values, possibly inside text: data.P\\[REF\\], data.P\\[Q\\], data.H\\. Add text or factor columns to `key_cols`"
  )
  expect_identical(out$P, d$P[match(out$USUBJID, c("SUBJ-1", "SUBJ-2")), ])
  expect_named(out$H, "S-01")
  # The dates step names columns inside packed columns by position.
  expect_warning(anon_subject_dates(d, seed = 1), "contain dates that were not shifted: data.P\\[2\\], data.P\\[4\\]\\[1\\]\\.")
})

test_that("dates held in attributes as Date or date-time values become [DATE]", {
  d <- data.frame(USUBJID = "a", V = 1)
  attr(d$V, "cut") <- as.Date(c(x = "2014-01-05", y = NA))
  attr(d$V, "at") <- as.POSIXct("2014-01-05 10:00", tz = "UTC")
  attr(d$V, "lt") <- list(as.POSIXlt("2014-01-05 10:00", tz = "UTC"))
  step <- anon_subject_dates(d, seed = 1)
  out <- step$data$V
  expect_identical(attr(out, "cut"), c(x = "[DATE]", y = NA))
  expect_identical(attr(out, "at"), "[DATE]")
  expect_identical(attr(out, "lt"), list("[DATE]"))
  expect_match(step$log$detail[step$log$dataset %in% "data" & is.na(step$log$variable)], "3 attribute value")
  # The IDs step leaves them alone.
  expect_identical(attr(anon_subject_ids(d, seed = 1)$data$V, "cut"), attr(d$V, "cut"))
})

test_that("data frames held in attributes have their values rewritten; grouping is left to the data", {
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1:2)
  attr(d$V, "map") <- data.frame(who = c("S-01", "S-02"), when = factor(c("2014-01-05", "x")), n = 1:2)
  step <- anon_subject_dates(anon_subject_ids(d, keep_secrets = TRUE, seed = 1), seed = 2)
  new <- anon_secrets(step)$subject_ids
  map <- attr(step$data$V, "map")
  expect_equal(map$who, new$pseudonym[match(c("S-01", "S-02"), new$key)])
  expect_equal(as.character(map$when), c("[DATE]", "x"))
  expect_equal(map$n, 1:2)
  te <- dplyr::group_by(tibble::tibble(ARM = c("S-01", "B")), ARM)
  expect_warning(
    out <- anon_subject_ids(list(dm = data.frame(USUBJID = c("S-01", "S-02")), te = te), seed = 1),
    "still contain original `USUBJID` values.*: te.ARM\\."
  )
  expect_identical(dplyr::group_data(out$data$te), dplyr::group_data(te))
})

test_that("columns referred to by grouping and data.table keys are renamed with the columns, after a clash", {
  g <- dplyr::group_by(tibble::tibble(USUBJID = c("S-01", "S-02"), `S-01` = 1:2, `SUBJ-1` = 3:4), `S-01`)
  out <- anon_subject_ids(list(g = g), seed = 1)$data$g
  expect_named(out, c("USUBJID", "SUBJ-1_1", "SUBJ-1"))
  expect_equal(dplyr::group_vars(out), "SUBJ-1_1")
  # The attributes data.table uses for a key and an index, built without
  # data.table (not a suggested package).
  dt <- data.frame(USUBJID = c("S-01", "S-02"), `S-01` = 1:2, `SUBJ-1` = 2:1, check.names = FALSE)
  attr(dt, "sorted") <- "S-01"
  attr(dt, "index") <- structure(integer(0), "__SUBJ-1__S-01" = 2:1)
  class(dt) <- c("data.table", "data.frame")
  out <- anon_subject_ids(list(dt = dt), seed = 1)$data$dt
  expect_equal(attr(out, "sorted"), "SUBJ-1_1")
  expect_equal(names(attributes(attr(out, "index"))), "__SUBJ-1__SUBJ-1_1")
})

test_that("settings and log details name the column a clash renamed", {
  d <- data.frame(USUBJID = c("AB-1", "AB-2"), `AB-1_DTC` = c("2014-01-05", "2014-02-01"),
                  `SUBJ-1_DTC` = "2014-01-01", check.names = FALSE)
  step <- anon_subject_ids(anon_subject_dates(d, vars = c("AB-1_DTC", "SUBJ-1_DTC"), seed = 1), seed = 2)
  expect_named(step$data, c("USUBJID", "SUBJ-1_DTC_1", "SUBJ-1_DTC"))
  expect_equal(step$settings$subject_dates$vars, c("SUBJ-1_DTC_1", "SUBJ-1_DTC"))
  expect_match(step$log$detail[1], "vars = c(\"SUBJ-1_DTC_1\", \"SUBJ-1_DTC\")", fixed = TRUE)
})

test_that("value-label codes that the rewrite makes equal are merged, and named in a warning", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("a", "b"))
  d$V <- haven::labelled(c("x", "y"), labels = c(first = "2014-01-01", second = "2015-02-02", third = "on 2016-03-03"))
  expect_warning(
    out <- anon_subject_dates(d, seed = 1)$data$V,
    "^Value labels of these columns were merged because the rewrite made their codes equal: data.V\\.$"
  )
  expect_equal(attr(out, "labels"), c("first / second" = "[DATE]", third = "on [DATE]"))
  # The joined label is rewritten too, and duplicate label text is allowed.
  d$V <- haven::labelled(c("x", "y"), labels = c("cut 2014-01-01" = "2014-01-01", "cut 2014-01-01" = "2015-02-02"))
  expect_warning(out <- anon_subject_dates(d, seed = 1)$data$V, "were merged")
  expect_equal(attr(out, "labels"), c("cut [DATE] / cut [DATE]" = "[DATE]"))
  d$V <- haven::labelled(c("x", "y"), labels = c("same" = "a", "same" = "b"))
  expect_no_warning(out <- anon_subject_dates(d, seed = 1)$data$V)
  expect_equal(attr(out, "labels"), c("same" = "a", "same" = "b"))
  # Missing-value codes made equal are deduplicated.
  d$V <- haven::labelled_spss(c("x", "y"), na_values = c("2014-01-01", "2015-02-02"))
  expect_warning(
    out <- anon_subject_dates(d, seed = 1)$data$V,
    "^User-defined missing values of these columns were deduplicated because the rewrite made them equal: data.V\\.$"
  )
  expect_equal(attr(out, "na_values"), "[DATE]")
  # A key code kept because a value equals the pseudonym is named too.
  k <- data.frame(USUBJID = c("S-01", "S-02"))
  k$C <- haven::labelled(c("SUBJ-1", "x"), labels = c(k = "S-01"))
  expect_warning(
    out <- anon_subject_ids(k, seed = 1)$data$C,
    "keep value-label or missing-value codes that hold original `USUBJID` values.*: data.C\\."
  )
  expect_equal(attr(out, "labels"), c(k = "S-01"))
})

test_that("text in list-level attributes or dataset names, or marked as bytes, that can't be searched is named", {
  d <- list(dm = data.frame(USUBJID = c("S-01", "S-02")))
  note <- "caf\u00e9"
  Encoding(note) <- "bytes"
  attr(d, "note") <- note
  expect_warning(anon_subject_ids(d, seed = 1), "^1 distinct text\\(s\\) can't be searched .*: \\(list\\)\\.")
  b <- "S-01 caf\u00e9"
  Encoding(b) <- "bytes"
  d <- data.frame(USUBJID = c("S-01", "S-02"), T = c(b, "x"), V = 1:2, RFSTDTC = "2014-01-01")
  attr(d$V, "label") <- b
  expect_warning(anon_subject_ids(d, seed = 1), "^1 distinct text\\(s\\) .*: data.T, data.V\\.")
  expect_warning(out <- anon_subject_dates(d, seed = 1), "^1 distinct text\\(s\\) .*: data.T, data.V\\.")
  expect_identical(attr(out$data$V, "label"), b)
})

test_that("environments and functions held in attributes are left alone", {
  e <- new.env()
  attr(e, "note") <- "S-02"
  assign("self", e, envir = e)
  f <- function() 1
  attr(f, "note") <- "S-01"
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1:2)
  attr(d$V, "env") <- e
  attr(d$V, "fn") <- f
  out <- anon_subject_ids(d, seed = 1)$data$V
  expect_identical(attr(e, "note"), "S-02")
  expect_identical(attr(out, "env"), e)
  expect_identical(attr(attr(out, "fn"), "note"), "S-01")
})

test_that("a factor held in an attribute whose levels now read the same becomes text", {
  d <- data.frame(USUBJID = "a", V = 1)
  attr(d$V, "f") <- factor(c("2014-01-01", "2015-01-01", "2014-01-01", "x"))
  expect_identical(attr(anon_subject_dates(d, seed = 1)$data$V, "f"), c("[DATE]", "[DATE]", "[DATE]", "x"))
})

test_that("text held in list columns is searched quickly", {
  n <- 20000
  d <- tibble::tibble(USUBJID = sprintf("S-%05d", seq_len(n)))
  d$L <- lapply(seq_len(n), function(i) list(a = "x", b = structure(i, note = "n")))
  expect_lt(system.time(anon_subject_ids(d, seed = 1))[["elapsed"]], 5)
})

test_that("other_attributes = \"drop\" drops attributes beyond the core ones, and counts them", {
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1:2, D = as.Date(c("2014-01-02", "2014-02-03")))
  attr(d$V, "label") <- "Value for S-01"
  comment(d$V) <- "S-02"
  attr(d$V, "note") <- "S-01"
  attr(d$V, "cut") <- as.Date("2014-01-05")
  attr(d$V, "format.sas") <- "BEST12"
  attr(d, "label") <- "Data"
  attr(d, "source") <- list(by = "S-02")
  data <- list(dm = d)
  attr(data, "note") <- "S-01"
  for (step_fn in list(
    function(x) anon_subject_ids(x, keep_secrets = TRUE, seed = 1, other_attributes = "drop"),
    function(x) anon_subject_dates(anon_subject_ids(x, keep_secrets = TRUE, seed = 1), seed = 2, other_attributes = "drop")
  )) {
    step <- step_fn(data)
    out <- step$data$dm
    expect_null(attr(out$V, "note"))
    expect_null(attr(out$V, "cut"))
    expect_null(attr(out, "source"))
    expect_null(attr(step$data, "note"))
    expect_equal(attr(out$V, "format.sas"), "BEST12")
    expect_match(attr(out$V, "label"), "^Value for SUBJ-[12]$")
    expect_match(comment(out$V), "^SUBJ-[12]$")
    expect_equal(attr(out, "label"), "Data")
    dropped <- step$log[grepl("other attribute", step$log$detail), ]
    expect_equal(dropped$dataset, c("dm", NA))
    expect_equal(dropped$detail, c("3 other attribute(s) dropped", "1 other attribute(s) dropped"))
    expect_false(any(grepl("note|cut|source|S-0", step$log$detail)))
  }
  expect_equal(step$settings$subject_dates$other_attributes, "drop")
  expect_equal(step$settings$subject_ids$other_attributes, "rewrite")
  expect_error(anon_subject_ids(d, other_attributes = "keep"), "should be one of")
  # Numeric keys aren't searched in text, but other attributes are still dropped.
  num <- data.frame(USUBJID = c(1, 2), V = 1:2)
  attr(num$V, "note") <- "x"
  expect_null(attr(anon_subject_ids(num, seed = 1, other_attributes = "drop")$data$V, "note"))
})

test_that("the grouping of a grouped tibble held in an attribute is rewritten with its values", {
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1:2)
  attr(d$V, "g") <- dplyr::group_by(tibble::tibble(who = c("S-01", "S-02"), when = c("2014-01-05", "x")), who, when)
  step <- anon_subject_dates(anon_subject_ids(d, keep_secrets = TRUE, seed = 1), seed = 2)
  new <- anon_secrets(step)$subject_ids
  g <- attr(step$data$V, "g")
  groups <- dplyr::group_data(g)
  expect_setequal(groups$who, new$pseudonym)
  expect_setequal(groups$when, c("[DATE]", "x"))
  expect_equal(lengths(groups$.rows), c(1L, 1L))
  expect_equal(g$who[unlist(groups$.rows)], groups$who)
})

test_that("zero-length dates and vctrs prototypes in attributes are left alone", {
  d <- tibble::tibble(USUBJID = c("a", "b"), V = 1:2)
  attr(d$V, "z") <- as.Date(character(0))
  attr(d$V, "p") <- vctrs::list_of(as.Date("2014-01-05"))
  out <- anon_subject_dates(d, seed = 1)$data$V
  expect_identical(attr(out, "z"), as.Date(character(0)))
  expect_identical(attr(attr(out, "p"), "ptype"), as.Date(character(0)))
  expect_identical(attr(out, "p")[[1]], "[DATE]")
})

test_that("S4 objects are not entered; S4 columns are named as data that can't be checked", {
  methods::setClass("AnonTestLab", contains = "character", representation(note = "character"), where = environment())
  s4 <- methods::new("AnonTestLab", "S-01 x", note = "S-02")
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1:2)
  attr(d$V, "obj") <- s4
  # Kept as they are under "rewrite", and named in a warning.
  expect_warning(
    out <- anon_subject_ids(d, seed = 1)$data$V,
    "^These columns or datasets hold S4 objects in attributes, .* kept as they are: data.V\\. Use `other_attributes = \"drop\"`"
  )
  expect_identical(attr(out, "obj"), s4)
  expect_no_warning(out <- anon_subject_ids(d, seed = 1, other_attributes = "drop")$data$V)
  expect_null(attr(out, "obj"))
  d$V <- NULL
  d$C <- methods::new("AnonTestLab", c("S-01", "y"), note = "S-02")
  expect_true(isS4(d$C))
  expect_warning(out <- anon_subject_ids(d, seed = 1)$data, "hold S4 objects, which can't be checked for original `USUBJID` values: data.C\\.")
  expect_true(isS4(out$C))
  expect_warning(anon_subject_dates(d, seed = 1), "hold S4 objects, which can't be checked for dates: data.C\\.")
})

test_that("attributes named groups, sorted or index on other objects are ordinary attributes", {
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1:2)
  attr(d, "groups") <- "S-01 group"
  attr(d$V, "index") <- "S-02"
  attr(d, "sorted") <- "S-02"
  out <- anon_subject_ids(d, keep_secrets = TRUE, seed = 1)
  new <- stats::setNames(anon_secrets(out)$subject_ids$pseudonym, anon_secrets(out)$subject_ids$key)
  expect_equal(attr(out$data, "groups"), paste(new[["S-01"]], "group"))
  expect_equal(attr(out$data$V, "index"), new[["S-02"]])
  expect_equal(attr(out$data, "sorted"), new[["S-02"]])
  expect_null(attr(anon_subject_ids(d, seed = 1, other_attributes = "drop")$data, "sorted"))
})

test_that("data.table index names whose columns hold \"__\" are renamed through the columns", {
  d <- data.frame(USUBJID = c("S-01", "S-02"), `a__S-01` = 1:2, `S-01` = 1:2, check.names = FALSE)
  attr(d, "index") <- structure(integer(0), "__a__S-01__S-01" = 1:2, "__unknown" = 1:2)
  class(d) <- c("data.table", "data.frame")
  out <- anon_subject_ids(d, keep_secrets = TRUE, seed = 1)
  new <- anon_secrets(out)$subject_ids$pseudonym[1]
  expect_named(out$data, c("USUBJID", paste0("a__", new), new))
  expect_equal(names(attributes(attr(out$data, "index"))), c(paste0("__a__", new, "__", new), "__unknown"))
})

test_that("column and dataset names marked as bytes don't stop the steps", {
  # Not valid UTF-8: searched through a repaired copy, so a key in it is
  # rewritten (the name becomes the repaired text); without a key it is kept.
  b <- "S-01 caf\xe9"
  Encoding(b) <- "bytes"
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1, RFSTDTC = "2014-01-01")
  names(d)[2] <- b
  out <- anon_subject_ids(list(dm = d), seed = 1)
  expect_identical(names(out$data$dm)[2], "SUBJ-1 caf<e9>")
  plain <- "caf\xe9"
  Encoding(plain) <- "bytes"
  names(d)[2] <- plain
  out <- anon_subject_dates(list(dm = d), seed = 1)
  expect_identical(names(out$data$dm)[2], plain)
  expect_identical(Encoding(names(out$data$dm)[2]), "bytes")
  expect_false(any(.subset2(out$data$dm, 3) == "2014-01-01"))
  data <- list(dm = data.frame(USUBJID = "S-01"), x = data.frame(Y = 1))
  names(data)[2] <- b
  out <- anon_subject_ids(data, seed = 1)
  expect_identical(names(out$data)[2], "SUBJ-1 caf<e9>")
  # Valid UTF-8 marked as bytes is searched, and stays marked as bytes.
  v <- "S-01 caf\u00e9"
  Encoding(v) <- "bytes"
  d <- data.frame(USUBJID = c("S-01", "S-02"), V = 1)
  names(d)[2] <- v
  step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 1)
  expect_identical(Encoding(names(step$data)[2]), "bytes")
  expect_false(grepl("S-01", names(step$data)[2], useBytes = TRUE))
})

test_that("locations inside packed columns are printed with keys and dates rewritten", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"), RFSTDTC = c("2014-01-01", "2014-02-01"))
  d$P <- tibble::tibble(`S-02` = c("S-01", "x"), `2014-03-03` = c("2014-01-05", "y"))
  step_warnings <- function(expr) {
    w <- character(0)
    withCallingHandlers(expr, warning = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    })
    w
  }
  w <- step_warnings(out <- anon_subject_ids(d, keep_secrets = TRUE, seed = 1))
  s2 <- anon_secrets(out)$subject_ids$pseudonym[2]
  expect_match(w, paste0("data.P, data.P\\[", s2, "\\]\\. Add text or factor columns"), all = FALSE)
  expect_false(any(grepl("S-0", w)))
  w <- step_warnings(anon_subject_dates(d, seed = 1))
  expect_match(w, "not shifted: data.P, data.P\\[2\\]\\.", all = FALSE)
  expect_false(any(grepl("2014-03-03", w)))
})

test_that("attributes are structural only on the classes that use them", {
  d <- data.frame(USUBJID = c("S-01", "S-02"))
  d$N <- c(1, 2)
  attr(d$N, "units") <- "mg for S-01 on 2014-01-05"
  attr(d$N, "tzone") <- "S-02"
  attr(d$N, "ptype") <- "S-01"
  attr(d$N, "display_width") <- "S-02"
  attr(d$N, "format.sas") <- "BEST12"
  d$C <- c("a", "b")
  attr(d$C, "levels") <- c("S-01", "2014-01-05")
  d$DT <- as.difftime(c(1, 2), units = "days")
  d$TM <- as.POSIXct(c("2014-01-02 10:00", "2014-01-03 10:00"), tz = "UTC")
  d$F <- factor(c("x", "y"))
  stats::contrasts(d$F) <- stats::contr.sum(2)
  step <- anon_subject_dates(anon_subject_ids(d, keep_secrets = TRUE, seed = 1), seed = 2)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  out <- step$data
  expect_equal(attr(out$N, "units"), paste("mg for", new[["S-01"]], "on [DATE]"))
  expect_equal(attr(out$N, "tzone"), new[["S-02"]])
  expect_equal(attr(out$N, "ptype"), new[["S-01"]])
  expect_equal(attr(out$N, "display_width"), new[["S-02"]])
  expect_equal(attr(out$N, "format.sas"), "BEST12")
  expect_equal(attr(out$C, "levels"), c(new[["S-01"]], "[DATE]"))
  expect_equal(attr(out$DT, "units"), "days")
  expect_equal(attr(out$TM, "tzone"), "UTC")
  expect_equal(attr(out$F, "contrasts"), attr(d$F, "contrasts"))
  # With "drop", the text attributes go and the structural ones stay, uncounted.
  step <- anon_subject_ids(d, seed = 1, other_attributes = "drop")
  out <- step$data
  for (a in c("units", "tzone", "ptype", "display_width")) expect_null(attr(out$N, a))
  expect_null(attr(out$C, "levels"))
  expect_equal(attr(out$N, "format.sas"), "BEST12")
  expect_equal(attr(out$DT, "units"), "days")
  expect_equal(attr(out$TM, "tzone"), "UTC")
  expect_equal(attr(out$F, "contrasts"), attr(d$F, "contrasts"))
  expect_equal(step$log$detail[grepl("dropped", step$log$detail)], "5 other attribute(s) dropped")
})

test_that("a vctrs prototype is kept, with text in it rewritten", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"))
  d$L <- vctrs::list_of(factor("a"), factor("a"), .ptype = factor(levels = c("a", "S-01")))
  for (other in c("rewrite", "drop")) {
    # The elements are cast to the prototype, so their levels hold the key
    # too: they are data, and flagged.
    expect_warning(
      step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 1, other_attributes = other),
      "still contain original `USUBJID` values.*: data.L\\."
    )
    new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
    expect_equal(levels(attr(step$data$L, "ptype")), c("a", new[["S-01"]]), info = other)
    expect_s3_class(step$data$L, "vctrs_list_of")
  }
})

test_that("time series, rowwise tibbles and data.table internals keep what they need, also with \"drop\"", {
  rw <- dplyr::rowwise(tibble::tibble(A = c("x", "y"), N = 1:2))
  dt <- data.frame(A = 1:2)
  # The attributes data.table uses, built without data.table.
  attr(dt, ".internal.selfref") <- new("externalptr")
  class(dt) <- c("data.table", "data.frame")
  tsd <- data.frame(A = 1:2)
  tsd$TS <- stats::ts(c(1, 2), start = 2000)
  data <- list(dm = data.frame(USUBJID = c("S-01", "S-02")), rw = rw, dt = dt, tsd = tsd)
  for (other in c("rewrite", "drop")) {
    step <- anon_subject_ids(data, seed = 1, other_attributes = other)
    out <- step$data
    expect_identical(out$rw, rw, info = other)
    expect_true(!is.null(attr(out$dt, ".internal.selfref")), info = other)
    expect_identical(out$tsd, tsd, info = other)
    expect_false(any(grepl("dropped", step$log$detail)), info = other)
  }
  # A rowwise tibble grouped by a column named with a key keeps its grouping.
  rk <- dplyr::rowwise(tibble::tibble(A = 1:2, `S-01` = 1:2), `S-01`)
  out <- anon_subject_ids(list(dm = data.frame(USUBJID = c("S-01", "S-02")), rk = rk), keep_secrets = TRUE,
                          seed = 1, other_attributes = "drop")
  expect_equal(dplyr::group_vars(out$data$rk), anon_secrets(out)$subject_ids$pseudonym[1])
})

test_that("joined labels of merged value labels are rewritten once, from the original labels", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("S-1", "S-2", "S-9"))
  # Pseudonyms that are themselves keys of other subjects: rewriting twice
  # would turn the S-1 of a label into the pseudonym of S-2's pseudonym.
  d$REL <- haven::labelled(c("x", "x", "x"), labels = c("brother of S-1" = "S-9", other = "S-3"))
  expect_warning(step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 5, prefix = "S-"), "were merged")
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  expect_equal(new[["S-9"]], "S-3")
  expect_equal(attr(step$data$REL, "labels"), stats::setNames("S-3", paste0("brother of ", new[["S-1"]], " / other")))
})

test_that("a missing-value range whose rewritten ends would descend is kept and named", {
  skip_if_not_installed("haven")
  d <- data.frame(USUBJID = c("a", "b"))
  d$Z <- haven::labelled_spss(c("B", "C"), na_range = c("1", "2020-01-01"))
  expect_warning(out <- anon_subject_dates(d, seed = 1)$data$Z, "keep value-label or missing-value codes that hold dates.*data.Z\\.")
  expect_equal(attr(out, "na_range"), c("1", "2020-01-01"))
})

test_that("warnings print locations with keys and dates hidden, and text that can't be printed escaped", {
  d <- tibble::tibble(USUBJID = c("AB-001", "AB-002"), DMDTC = c("2020-01-01", "2020-01-02"))
  d$PK <- tibble::tibble(x = 1:2)
  d$PK[["AB-002"]] <- c("2020-02-03", "y")
  d$PK[["2020-03-04"]] <- c("AB-001", "z")
  warnings_of <- function(expr) {
    w <- character(0)
    withCallingHandlers(expr, warning = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    })
    w
  }
  dates_first <- warnings_of(step <- anon_subject_dates(d, seed = 1))
  # The packed column itself is named too: its column names hold a date. The
  # dates step names columns inside packed columns by position, also when
  # given the data of an IDs step rather than the step.
  expect_match(dates_first, "not shifted: data.PK, data.PK\\[2\\]\\.", all = FALSE)
  expect_match(
    warnings_of(anon_subject_dates(anon_subject_ids(d, seed = 2)$data, seed = 1)),
    "not shifted: data.PK, data.PK\\[2\\]\\.", all = FALSE
  )
  ids_after <- warnings_of(anon_subject_ids(step, seed = 2))
  ids_first <- warnings_of(step <- anon_subject_ids(d, seed = 2))
  # After the IDs step, columns inside packed columns are shown by position.
  dates_after <- warnings_of(anon_subject_dates(step, seed = 1))
  expect_match(dates_after, "not shifted: data.PK, data.PK\\[2\\]\\.", all = FALSE)
  all <- c(dates_first, ids_after, ids_first, dates_after)
  expect_false(any(grepl("AB-00|2020-0[123]", all)))
  # A sub-column name marked "bytes" that isn't valid UTF-8 is escaped.
  b <- "caf\xe9 AB-001"
  Encoding(b) <- "bytes"
  p <- data.frame(a = 1:2, c = c("AB-001", "x"))
  attr(p, "names") <- c("a", b)
  d2 <- data.frame(USUBJID = c("AB-001", "AB-002"))
  d2$PK <- p
  w <- warnings_of(anon_subject_ids(d2, seed = 1))
  expect_match(w, "data.PK\\[caf<e9> SUBJ-[12]\\]", all = FALSE)
})

test_that("factors whose values are replaced or shifted drop a contrasts matrix; others keep it, renamed", {
  d <- data.frame(USUBJID = factor(c("S-01", "S-02")), DTC = factor(c("2014-01-02", "2014-02-03")))
  stats::contrasts(d$USUBJID) <- stats::contr.sum(2)
  stats::contrasts(d$DTC) <- stats::contr.treatment(2)
  # Contrasts named by levels that hold a key and a date.
  d$KEPT <- factor(c("S-01 group", "2014-01-02 group"))
  stats::contrasts(d$KEPT) <- stats::contr.sum(2)
  d$NAMED <- factor(c("x", "y"))
  attr(d$NAMED, "contrasts") <- "contr.sum"
  # KEPT's levels are values, and flagged.
  step <- suppressWarnings(anon_subject_dates(anon_subject_ids(d, keep_secrets = TRUE, seed = 1), seed = 2))
  out <- step$data
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  expect_null(attr(out$USUBJID, "contrasts"))
  expect_null(attr(out$DTC, "contrasts"))
  expect_equal(rownames(attr(out$KEPT, "contrasts")), c("[DATE] group", paste(new[["S-01"]], "group")))
  expect_equal(attr(out$NAMED, "contrasts"), "contr.sum")
  # A contrasts function's name on a replaced factor stays too.
  k <- data.frame(USUBJID = factor(c("S-01", "S-02")))
  attr(k$USUBJID, "contrasts") <- "contr.sum"
  expect_equal(attr(anon_subject_ids(k, seed = 1)$data$USUBJID, "contrasts"), "contr.sum")
})

test_that("a survival::Surv column keeps its type and states, also with \"drop\"", {
  # A Surv object as survival::Surv() builds it (survival isn't a suggested
  # package).
  surv <- structure(
    matrix(c(5, 8, 3, 1, 0, 1), ncol = 2, dimnames = list(NULL, c("time", "status"))),
    type = "right", states = c("event"), class = "Surv"
  )
  d <- data.frame(USUBJID = c("S-01", "S-02", "S-03"))
  d$S <- surv
  attr(d$S, "inputAttributes") <- list(time = list(note = "x"))
  out <- anon_subject_ids(d, seed = 1, other_attributes = "drop")$data$S
  expect_equal(attr(out, "type"), "right")
  expect_equal(attr(out, "states"), "event")
  expect_null(attr(out, "inputAttributes"))
  expect_s3_class(out, "Surv")
  expect_equal(nrow(unclass(out)), 3)
})

test_that("errors name datasets with keys and dates hidden", {
  d <- list(dm = data.frame(USUBJID = "AB-1001", X = 1))
  names(d) <- "AB-1001 2014-05-06"
  expect_error(anon_subject_dates(d, vars = "X"), "`X` in `\\[KEY\\] \\[DATE\\]` is not a date column")
})
