sup_dm <- function() {
  data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    AGE = c(30, 30, 30, 40, 40, 50),
    SEX = c("F", "F", "M", "M", "M", "F"),
    stringsAsFactors = FALSE
  )
}

# The attacker's count, written apart from the package: for each subject,
# the released records that show its true value or a blanked cell (missing
# in `released` but not in `truth`) on every quasi-identifier. Real missing
# values (and blank strings) are values. Rows of both are the same subjects.
sup_attacker_k <- function(truth, released, qs) {
  missing <- function(x) is.na(x) | (is.character(x) & !is.na(x) & trimws(x) == "")
  vapply(seq_len(nrow(truth)), function(i) {
    ok <- rep(TRUE, nrow(released))
    for (q in qs) {
      rel <- as.character(released[[q]])
      tru <- as.character(truth[[q]])
      blank <- missing(rel) & !missing(tru)
      same <- (missing(rel) & missing(tru[i])) | (!missing(rel) & !missing(tru[i]) & rel == tru[i])
      ok <- ok & (blank | same)
    }
    sum(ok)
  }, numeric(1))
}

test_that("anon_suppress() blanks other records until each subject at risk has enough consistent ones", {
  result <- anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), max_risk = 1/2)
  out <- result$data
  # S03 (30, M) and S06 (50, F) are unique. Blanking their own cells
  # wouldn't hide them from an attacker who knows their values: S04's AGE
  # (one cell, AGE before SEX in `quasi`) covers S03, and S01's AGE covers
  # S06.
  expect_identical(out$AGE, c(NA, 30, 30, NA, 40, 50))
  expect_identical(out$SEX, sup_dm()$SEX)
  expect_true(all(sup_attacker_k(sup_dm(), out, c("AGE", "SEX")) >= 2))
  exact <- anon_risk(result, quasi = c("AGE", "SEX"), max_risk = 1/2, original = sup_dm())
  expect_true(exact$summary$meets_target)
  expect_identical(exact$summary$min_k, 2L)

  # With SEX less useful, S01's SEX covers S03; S01's AGE then covers S06.
  out <- anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), max_risk = 1/2, order = c("SEX", "AGE"))$data
  expect_identical(out$AGE, c(NA, 30, 30, 40, 40, 50))
  # Blank text is "" unless the column uses NA for missing text.
  expect_identical(out$SEX, c("", "F", "M", "M", "M", "F"))

  log <- result$log
  expect_identical(log$step, rep("suppress", 2))
  expect_identical(log$variable[2], "AGE")
  expect_identical(log$n_changed[2], 2L)
  expect_match(log$detail[2], "blanked for 2 subject\\(s\\)")
  expect_match(log$detail[1], "before: min k 1, 2 subject\\(s\\) below 2; after: min k 2, 0 subject\\(s\\) below 2")
  expect_true(result$settings$suppress$meets_target)
  expect_identical(result$settings$suppress$min_k, 2L)
  expect_identical(result$settings$suppress$order, c("AGE", "SEX"))
})

test_that("a rare subject's own blanked cells don't count: the reviewer's example reaches the target", {
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:24),
    AGE = c(rep(30, 12), rep(30, 11), 80),
    SEX = c(rep("M", 12), rep("F", 11), "F")
  )
  out <- anon_suppress(dm, c("AGE", "SEX"))
  expect_true(all(sup_attacker_k(dm, out$data, c("AGE", "SEX")) >= 12))
  expect_true(out$settings$suppress$meets_target)
  # The symmetric wildcard measure called the same data safe after blanking
  # S24's own AGE, which the attacker sees through.
  naive <- dm
  naive$AGE[24] <- NA
  expect_true(anon_risk(naive, c("AGE", "SEX"), na = "wildcard")$summary$meets_target)
  expect_identical(min(sup_attacker_k(dm, naive, c("AGE", "SEX"))), 1)
})

test_that("real missing values are values, not wildcards", {
  set.seed(1)
  dm <- data.frame(
    USUBJID = sprintf("S%03d", 1:51),
    AGE = c(sample(20:80, 40), rep(NA, 11)),
    SEX = c(sample(c("M", "F"), 40, TRUE), rep(NA, 11))
  )
  expect_false(anon_risk(dm, c("AGE", "SEX"))$summary$meets_target)
  out <- anon_suppress(dm, c("AGE", "SEX"))
  expect_gt(sum(out$log$n_changed[out$log$step == "suppress"], na.rm = TRUE), 0)
  expect_true(all(sup_attacker_k(dm, out$data, c("AGE", "SEX")) >= 12))
})

test_that("the records needing the fewest cells, then the least useful, cover a subject", {
  dm <- data.frame(USUBJID = c("X", "Y", "Z"), A = c(1, 1, 2), B = c(1, 1, 2), C = c(9, 5, 9))
  # X: Y needs one cell (C), Z two (A, B): Y's C. Z: X and Y need two each
  # (A, B), X first. Y: X now needs C only.
  out <- anon_suppress(dm, quasi = c("A", "B", "C"), max_risk = 1/2)$data
  expect_identical(out$A, c(NA, 1, 2))
  expect_identical(out$B, c(NA, 1, 2))
  expect_identical(out$C, c(NA, NA, 9))
  expect_true(all(sup_attacker_k(dm, out, c("A", "B", "C")) >= 2))
})

test_that("anon_suppress() blanks every row of a test with its text results", {
  dm <- sup_dm()
  vs <- data.frame(
    USUBJID = c(rep(dm$USUBJID, each = 2), "S01"),
    VSTESTCD = c(rep(c("WEIGHT", "HEIGHT"), 6), "WEIGHT"),
    VSORRES = c("70", "", "70", "160", "70", "160", "70", "170", "70", "170", "95", "160", "71"),
    VSSTRESC = c("70", "160", "70", "160", "70", "160", "70", "170", "70", "170", "95", "160", "71"),
    VSSTRESN = c(70, 160, 70, 160, 70, 160, 70, 170, 70, 170, 95, 160, 71),
    VSBLFL = c(rep("Y", 12), ""),
    stringsAsFactors = FALSE
  )
  vs$VSSTRESC[2] <- NA
  quasi <- list(dm = "SEX", anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN", filter = ~ VSBLFL == "Y"))
  result <- anon_suppress(list(dm = dm, vs = vs), quasi = quasi, max_risk = 1/2)
  out <- result$data$vs
  # S06 (F, 95) is the only one at risk; S01 (F, 70) covers it, so S01's
  # WEIGHT is blanked in both of its rows, not only the baseline one.
  s01 <- out$USUBJID == "S01" & out$VSTESTCD == "WEIGHT"
  expect_identical(out$VSSTRESN[s01], c(NA_real_, NA_real_))
  # Blank text is "" unless the column uses NA for missing text.
  expect_identical(out$VSORRES[s01], c("", ""))
  expect_identical(out$VSSTRESC[s01], c(NA_character_, NA_character_))
  expect_identical(out[!s01, ], vs[!s01, ])
  expect_identical(result$data$dm, dm)
  log <- result$log
  expect_identical(log$variable[-1], c("VSORRES (VSTESTCD = WEIGHT)", "VSSTRESC (VSTESTCD = WEIGHT)", "VSSTRESN (VSTESTCD = WEIGHT)"))
  expect_identical(log$n_changed[-1], c(2L, 2L, 2L))
  expect_true(result$settings$suppress$meets_target)
  # The step records the blanked cell by subject, never by value.
  expect_identical(result$suppressed$key, "S01")
  expect_identical(result$suppressed$test, "WEIGHT")
})

test_that("blanked factors keep their class; levels held only by blanked values go", {
  dm <- sup_dm()
  dm$SEX <- factor(dm$SEX)
  dm$AGE <- factor(c(30, 30, 30, 45, 40, 50))
  out <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/2)$data
  expect_s3_class(out$AGE, "factor")
  expect_true(all(sup_attacker_k(dm, out, c("AGE", "SEX")) >= 2))
  expect_false(anyNA(out$SEX))
  expect_true(all(levels(out$AGE) %in% as.character(out$AGE)))
})

test_that("anon_risk() measures a suppressed step from its record of blanked cells", {
  result <- anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), max_risk = 1/2)
  r <- anon_risk(result, quasi = c("AGE", "SEX"), max_risk = 1/2)
  expect_identical(r$n_blanked, 2L)
  expect_true(r$lower_bound)
  # Without the true values, S01 (blanked AGE, F) is measured against a value
  # no record shows: a lower bound.
  expect_identical(anon_risk_subjects(result, quasi = c("AGE", "SEX"))$k, c(1L, 2L, 2L, 1L, 2L, 2L))
  expect_identical(anon_risk_subjects(result, quasi = c("AGE", "SEX"), original = sup_dm())$k, c(2L, 2L, 2L, 2L, 2L, 2L))
  expect_output(print(r), "Cells blanked by suppression: 2")
  expect_output(print(r), "lower bound")
  # Measured without the step, blanked cells are missing values.
  expect_false(anon_risk(result$data, quasi = c("AGE", "SEX"), max_risk = 1/2)$summary$meets_target)

  # Later steps carry the record on, with the new keys.
  ids <- anon_subject_ids(result, seed = 3)
  expect_identical(
    anon_risk(ids, quasi = c("AGE", "SEX"), max_risk = 1/2)$summary,
    r$summary
  )
  expect_false(any(ids$suppressed$key %in% sup_dm()$USUBJID))
  expect_true(all(ids$suppressed$key %in% ids$data$USUBJID))
  dated <- anon_subject_dates(ids, seed = 4)
  expect_identical(dated$suppressed, ids$suppressed)

  # A second suppression counts the first one's cells as blanked.
  again <- anon_suppress(result, quasi = c("AGE", "SEX"), max_risk = 1/3)
  expect_identical(nrow(again$suppressed), nrow(result$suppressed) + sum(again$log$n_changed[-(1:2)], na.rm = TRUE))
  expect_true(anon_risk(again, quasi = c("AGE", "SEX"), max_risk = 1/3)$summary$meets_target)
})

test_that("method = \"drop\" removes subjects at risk at any depth, repeating as groups shrink", {
  dm <- sup_dm()
  dm$HIST <- I(lapply(dm$USUBJID, function(k) data.frame(USUBJID = k, X = 1)))
  ae <- data.frame(USUBJID = c("S03", "S01", "S06", "S06"), AETERM = "x")
  relsub <- data.frame(USUBJID = c("S01", "S02"), RSUBJID = c("S06", "S01"))
  ts <- data.frame(TSPARMCD = "AGEMIN", TSVAL = "18")
  # Datasets without the key whose rows belong to subjects through a packed
  # column, or a list column of one subject per row.
  pk <- tibble::tibble(V = 1:3)
  pk$ID <- tibble::tibble(USUBJID = c("S06", "S01", "S03"))
  spl <- tibble::tibble(V = 1:2)
  spl$data <- list(data.frame(USUBJID = "S03"), data.frame(USUBJID = "S02"))
  lab <- tibble::tibble(TEST = "ALB")
  lab$data <- list(data.frame(USUBJID = c("S01", "S03", "S06"), LBSTRESN = 1:3))
  data <- list(dm = dm, ae = ae, relsub = relsub, ts = ts, pk = pk, spl = spl, lab = lab)
  expect_no_warning(
    result <- anon_suppress(data, quasi = list(dm = c("AGE", "SEX")), max_risk = 1/2, method = "drop")
  )
  out <- result$data
  expect_identical(out$dm$USUBJID, c("S01", "S02", "S04", "S05"))
  expect_identical(out$ae$USUBJID, "S01")
  expect_identical(out$relsub$RSUBJID, "S01")
  expect_identical(out$ts, ts)
  expect_identical(out$pk$ID$USUBJID, "S01")
  expect_identical(nrow(out$spl), 1L)
  expect_identical(out$lab$data[[1]]$USUBJID, "S01")
  expect_true(result$settings$suppress$meets_target)
  log <- result$log
  expect_identical(log$n_removed[log$dataset %in% c("dm", "ae", "relsub")], c(2L, 3L, 1L))
  expect_match(log$detail, "2 subject\\(s\\) at risk removed", all = FALSE)

  # A dataset without the key that still holds removed subjects' keys or
  # related IDs: a warning, and the target isn't met.
  dm2 <- sup_dm()
  dm2$SUBJID <- sprintf("10%03d", 1:6)
  lut <- data.frame(SUBJ = dm2$SUBJID, NOTE = "x")
  expect_warning(
    r2 <- anon_suppress(list(dm = dm2, lut = lut), quasi = list(dm = c("AGE", "SEX")), max_risk = 1/2, method = "drop"),
    "Rows or values of removed subjects remain in these datasets: lut"
  )
  expect_false(r2$settings$suppress$meets_target)
  expect_match(r2$settings$suppress$not_met, "values of removed subjects remain", all = FALSE)
})

test_that("method = \"drop\" repeats when removing subjects leaves others at risk", {
  dm <- rbind(sup_dm(), data.frame(USUBJID = c("S07", "S08"), AGE = c(30, 40), SEX = c("F", "M")))
  # Cells first: S04's AGE covers S03 (30, M) and S01's covers S06 (50, F).
  first <- anon_suppress(dm, c("AGE", "SEX"), max_risk = 1/2)
  expect_identical(is.na(first$data$AGE), dm$USUBJID %in% c("S01", "S04"))
  # Measured from the released data, S01 and S04 are at risk (their true AGE
  # is no longer known). Removing them leaves S03 and S06 alone: they go in
  # a second round.
  out <- anon_suppress(first, c("AGE", "SEX"), max_risk = 1/2, method = "drop")
  expect_identical(out$data$USUBJID, c("S02", "S05", "S07", "S08"))
  expect_true(out$settings$suppress$meets_target)
  expect_true(anon_risk(out, c("AGE", "SEX"), max_risk = 1/2)$summary$meets_target)
})

test_that("subjects that can't reach the target are dropped or kept, with a warning", {
  dm <- sup_dm()[1:3, ]
  expect_warning(
    result <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/4),
    "3 subject\\(s\\) could not reach groups of at least 4"
  )
  expect_identical(nrow(result$data), 0L)
  expect_identical(result$settings$suppress$n_dropped, 3L)
  expect_match(result$log$detail[1], "after: no subjects left")
  expect_warning(
    kept <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/4, unresolved = "keep"),
    "3 subject\\(s\\) remain in groups smaller than 4"
  )
  expect_identical(kept$data, dm)
  expect_false(kept$settings$suppress$meets_target)
})

test_that("anon_suppress() reaches the target on random data, with 13 quasi-identifiers too", {
  set.seed(7)
  n <- 400
  dm <- data.frame(
    USUBJID = sprintf("S%03d", seq_len(n)),
    AGE = sample(seq(20, 80, by = 10), n, replace = TRUE),
    SEX = sample(c("F", "M"), n, replace = TRUE),
    RACE = sample(c("A", "B", "C", "D", NA), n, replace = TRUE, prob = c(0.65, 0.2, 0.07, 0.03, 0.05)),
    SITE = sample(sprintf("%02d", 1:8), n, replace = TRUE)
  )
  quasi <- c("AGE", "SEX", "RACE", "SITE")
  out <- anon_suppress(dm, quasi = quasi, max_risk = 0.2)$data
  expect_true(all(sup_attacker_k(dm, out, quasi) >= 5))
  untouched <- rowSums(is.na(out[quasi]) & !is.na(dm[quasi])) == 0
  expect_identical(out[untouched, ], dm[untouched, ])

  wide <- data.frame(USUBJID = sprintf("W%03d", 1:60))
  for (j in 1:13) wide[[paste0("Q", j)]] <- sample(1:3, 60, TRUE)
  qs <- paste0("Q", 1:13)
  res <- anon_suppress(wide, qs, max_risk = 1/3)
  expect_true(all(sup_attacker_k(wide, res$data, qs) >= 3))
  expect_true(res$settings$suppress$meets_target)
})

test_that("`everywhere` blanks a quasi-identifier's copies in other datasets and nested data", {
  dm <- sup_dm()
  dm$HIST <- I(lapply(seq_len(6), function(i) data.frame(USUBJID = dm$USUBJID[i], AGE = dm$AGE[i])))
  adsl <- sup_dm()
  adae <- data.frame(USUBJID = rep(dm$USUBJID, 2), AGE = rep(dm$AGE, 2))
  data <- list(dm = dm, adsl = adsl, adae = adae)
  out <- anon_suppress(data, list(dm = c("AGE", "SEX")), max_risk = 1/2)
  blanked <- c("S01", "S04")
  expect_identical(is.na(out$data$adsl$AGE), adsl$USUBJID %in% blanked)
  expect_identical(is.na(out$data$adae$AGE), adae$USUBJID %in% blanked)
  expect_identical(vapply(out$data$dm$HIST, function(h) is.na(h$AGE), logical(1)), dm$USUBJID %in% blanked)
  expect_true(out$settings$suppress$meets_target)
  expect_match(out$log$detail, "copy of `AGE` in `dm`", all = FALSE)

  expect_warning(
    only <- anon_suppress(data, list(dm = c("AGE", "SEX")), max_risk = 1/2, everywhere = FALSE),
    "`AGE` was blanked in `dm` only; these datasets hold it too: adsl, adae"
  )
  expect_identical(only$data$adsl, adsl)
  expect_false(only$settings$suppress$meets_target)
})

test_that("birth dates more precise than a year next to AGE: a warning, and the target isn't met", {
  dm <- sup_dm()
  dm$BRTHDTC <- c("1990-01-02", "1990", "1990-05", "", "1980", "1970")
  expect_warning(
    out <- anon_suppress(dm, c("AGE", "SEX"), max_risk = 1/2),
    "Birth dates more precise than a year are next to a generalized or suppressed `AGE`: data.BRTHDTC"
  )
  expect_false(out$settings$suppress$meets_target)
  expect_match(out$settings$suppress$not_met, "birth dates", all = FALSE)
  dm$BRTHDTC <- c("1990", "1990", "1990", "", "1980", "1970")
  expect_no_warning(out <- anon_suppress(dm, c("AGE", "SEX"), max_risk = 1/2))
  expect_true(out$settings$suppress$meets_target)
})

test_that("anon_suppress() validates `order` and chains with other steps", {
  expect_error(anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), order = "XX"), "`order` names `XX`")
  expect_error(anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), order = c("AGE", "AGE")), "without duplicates")
  expect_error(anon_suppress(sup_dm(), quasi = "AGE", method = "x"), "should be one of")
  # A partial order: those listed first, then the rest in `quasi` order.
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:6), A = c(1, 1, 1, 2, 2, 3), B = c("x", "x", "y", "y", "y", "x"), C = c(1, 1, 1, 1, 1, 2))
  partial <- anon_suppress(dm, quasi = c("A", "B", "C"), max_risk = 1/2, order = "C")
  expect_identical(partial$settings$suppress$order, c("C", "A", "B"))
  expect_true(all(sup_attacker_k(dm, partial$data, c("A", "B", "C")) >= 2))

  gen <- anon_generalize(sup_dm(), rules = list(AGE = anon_band(20)))
  step <- anon_suppress(gen, quasi = c("AGE", "SEX"), max_risk = 1/2)
  expect_identical(unique(step$log$step), c("generalize", "suppress"))
  expect_true(step$settings$suppress$meets_target)
  expect_true(anon_risk(step, c("AGE", "SEX"), max_risk = 1/2, original = gen)$summary$meets_target)
  ids <- anon_subject_ids(step, seed = 3)
  expect_named(ids$settings, c("generalize", "suppress", "subject_ids"))
})

test_that("the log and settings hold no values", {
  dm <- sup_dm()
  dm$SEX[6] <- "QQRARE"
  result <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/2)
  text <- c(capture.output(print(result)), deparse(unclass(result)[c("log", "settings")]), as_anon_json(result))
  expect_false(any(grepl("QQRARE|S0[1-6]", text)))
})
