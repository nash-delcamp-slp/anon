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
  expect_true(is.na(out$dm$ID[3]))
  expect_true(is.na(out$dm$SUBJID[3]))
  expect_setequal(out$dm$ID[1:2], c(1, 2))
  expect_equal(out$dm$SUBJID[1:2], as.integer(out$dm$ID[1:2]))
  expect_equal(out$vs$ID[1], out$vs$ID[2])
  expect_equal(out$vs$ID[1], out$dm$ID[2])
})

test_that("anon_subject_ids() treats blank keys as missing and accepts a data frame", {
  df <- data.frame(USUBJID = c("a", "", "b"), SUBJID = c("1", "", "2"))
  out <- anon_subject_ids(df, seed = 3)$data
  expect_s3_class(out, "data.frame")
  expect_equal(out$USUBJID[2], "")
  expect_equal(out$SUBJID[2], "")
  expect_setequal(out$USUBJID[c(1, 3)], c("SUBJ-1", "SUBJ-2"))
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
  result <- anon_subject_dates(data, keep_secrets = TRUE, seed = 5)
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
  expect_equal(anon_subject_dates(d, partial = "remove", seed = 10)$data$BRTHDTC, rep("", 300))
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
  expect_warning(result <- anon_subject_dates(d, seed = 18), "2 value\\(s\\) are not ISO 8601 dates and were removed")
  expect_match(result$log$detail[result$log$variable %in% "XDTC"], "0 value\\(s\\) shifted, 2 removed \\(not ISO 8601\\)")
  expect_true(all(is.na(result$data$XDTC)))
})

test_that("steps chain, keeping earlier logs and secrets", {
  data <- subject_data()
  step <- anon_subject_ids(anon_subject_dates(data, keep_secrets = TRUE, seed = 19), keep_secrets = TRUE, seed = 20)
  expect_setequal(unique(step$log$step), c("subject_dates", "subject_ids"))
  expect_named(anon_secrets(step), c("date_offsets", "subject_ids"))
  expect_setequal(anon_secrets(step)$subject_ids$key, data$dm$USUBJID)
})

test_that("the global random state is restored after a seeded step", {
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  anon_subject_dates(subject_data(), seed = 1)
  expect_equal(stats::runif(1), before)
})
