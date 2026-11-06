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
  expect_setequal(secrets$subject_ids$original, data$dm$USUBJID)

  shown <- c(capture.output(print(result)), capture.output(print(result$secrets)),
             capture.output(print(anon(result))))
  expect_false(any(grepl("01-701", shown, fixed = TRUE)))
  expect_error(as_anon_json(result), "Unsupported class")
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
  off <- stats::setNames(offsets$offset_days, offsets$original)

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
  expect_equal(out$ae$AESTDTC[2], "")
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
