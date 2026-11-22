sup_dm <- function() {
  data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    AGE = c(30, 30, 30, 40, 40, 50),
    SEX = c("F", "F", "M", "M", "M", "F"),
    stringsAsFactors = FALSE
  )
}

test_that("anon_suppress() blanks the fewest quasi-identifiers, least useful first", {
  result <- anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), max_risk = 1/2)
  out <- result$data
  # S03 (30, M) and S06 (50, F) are unique; blanking AGE alone is enough for
  # each, and AGE comes first in `quasi`.
  expect_identical(out$AGE, c(30, 30, NA, 40, 40, NA))
  expect_identical(out$SEX, sup_dm()$SEX)
  risk <- anon_risk(out, quasi = c("AGE", "SEX"), max_risk = 1/2, na = "wildcard")
  expect_true(risk$summary$meets_target)
  expect_identical(risk$summary$min_k, 3L)

  # With SEX less useful, S03 loses SEX; S06 then needs AGE blanked.
  out <- anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), max_risk = 1/2, order = c("SEX", "AGE"))$data
  expect_identical(out$AGE, c(30, 30, 30, 40, 40, NA))
  expect_identical(out$SEX, c("F", "F", NA, "M", "M", "F"))

  log <- result$log
  expect_identical(log$step, rep("suppress", 2))
  expect_identical(log$variable[2], "AGE")
  expect_identical(log$n_changed[2], 2L)
  expect_match(log$detail[2], "blanked for 2 subject\\(s\\)")
  expect_match(log$detail[1], "before: min k 1, 2 subject\\(s\\) below 2; after: min k 3, 0 subject\\(s\\) below 2")
  expect_true(result$settings$suppress$meets_target)
  expect_identical(result$settings$suppress$order, c("AGE", "SEX"))
})

test_that("anon_suppress() prefers one more useful cell to two less useful ones", {
  dm <- data.frame(USUBJID = c("X", "Y", "Z"), A = c(1, 1, 2), B = c(1, 1, 2), C = c(9, 5, 9))
  # X shares A and B with Y, and C with Z: blanking C (the most useful) is
  # one cell, blanking A and B two. Z then needs A and B blanked.
  out <- anon_suppress(dm, quasi = c("A", "B", "C"), max_risk = 1/2)$data
  expect_identical(out$A, c(1, 1, NA))
  expect_identical(out$B, c(1, 1, NA))
  expect_identical(out$C, c(NA, 5, 9))
})

test_that("anon_suppress() blanks every row of a test with its text results", {
  dm <- sup_dm()
  vs <- data.frame(
    USUBJID = rep(dm$USUBJID, each = 2),
    VSTESTCD = rep(c("WEIGHT", "HEIGHT"), 6),
    VSORRES = c("70", "160", "70", "160", "70", "160", "70", "170", "70", "170", "95", "160"),
    VSSTRESC = c("70", "160", "70", "160", "70", "160", "70", "170", "70", "170", "95", "160"),
    VSSTRESN = c(70, 160, 70, 160, 70, 160, 70, 170, 70, 170, 95, 160),
    VSBLFL = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", ""),
    stringsAsFactors = FALSE
  )
  # A second WEIGHT row for S06, not the baseline one.
  vs <- rbind(vs, data.frame(USUBJID = "S06", VSTESTCD = "WEIGHT", VSORRES = "96", VSSTRESC = "96", VSSTRESN = 96, VSBLFL = ""))
  vs$VSORRES[2] <- ""
  quasi <- list(dm = "SEX", anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN", filter = ~ VSBLFL == "Y"))
  result <- anon_suppress(list(dm = dm, vs = vs), quasi = quasi, max_risk = 1/2)
  out <- result$data$vs
  s06 <- out$USUBJID == "S06" & out$VSTESTCD == "WEIGHT"
  # S06 (F, 95) is the only one at risk: WEIGHT is blanked in both its rows.
  expect_identical(out$VSSTRESN[s06], c(NA_real_, NA_real_))
  # Text uses "" where the column already has blanks, otherwise NA.
  expect_identical(out$VSORRES[s06], c("", ""))
  expect_identical(out$VSSTRESC[s06], c(NA_character_, NA_character_))
  expect_identical(out[!s06, ], vs[!s06, ])
  expect_identical(result$data$dm, dm)
  log <- result$log
  expect_identical(log$variable[-1], c("VSORRES (VSTESTCD = WEIGHT)", "VSSTRESC (VSTESTCD = WEIGHT)", "VSSTRESN (VSTESTCD = WEIGHT)"))
  expect_identical(log$n_changed[-1], c(2L, 2L, 2L))
  expect_true(anon_risk(result, quasi = quasi, max_risk = 1/2, na = "wildcard")$summary$meets_target)
})

test_that("blanked factors keep their class; levels held only by blanked values go", {
  dm <- sup_dm()
  dm$SEX <- factor(dm$SEX)
  dm$AGE <- factor(dm$AGE)
  out <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/2)$data
  expect_s3_class(out$AGE, "factor")
  expect_identical(as.character(out$AGE), c("30", "30", NA, "40", "40", NA))
  expect_identical(levels(out$AGE), c("30", "40"))
  expect_identical(out$SEX, dm$SEX)
})

test_that("method = \"drop\" removes subjects at risk from every dataset, repeating as groups shrink", {
  dm <- sup_dm()
  ae <- data.frame(USUBJID = c("S03", "S01", "S06", "S06"), AETERM = "x")
  relsub <- data.frame(USUBJID = c("S01", "S02"), RSUBJID = c("S06", "S01"))
  ts <- data.frame(TSPARMCD = "AGEMIN", TSVAL = "18")
  result <- anon_suppress(list(dm = dm, ae = ae, relsub = relsub, ts = ts), quasi = list(dm = c("AGE", "SEX")), max_risk = 1/2, method = "drop")
  out <- result$data
  expect_identical(out$dm$USUBJID, c("S01", "S02", "S04", "S05"))
  expect_identical(out$ae$USUBJID, "S01")
  # The row referring to a removed subject goes too.
  expect_identical(out$relsub$RSUBJID, "S01")
  expect_identical(out$ts, ts)
  log <- result$log
  expect_identical(log$n_removed[log$dataset %in% c("dm", "ae", "relsub")], c(2L, 3L, 1L))
  expect_match(log$detail, "2 subject\\(s\\) at risk removed", all = FALSE)

  # a (with H missing) matches b and c, which don't match each other:
  # removing them leaves a alone, so it goes in a second round.
  dm2 <- data.frame(USUBJID = letters[1:6], G = c("x", "x", "x", "p", "p", "p"), H = c(NA, 1, 2, 5, 5, 5))
  out2 <- anon_suppress(dm2, quasi = c("G", "H"), max_risk = 1/3, method = "drop")$data
  expect_identical(out2$USUBJID, c("d", "e", "f"))
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
})

test_that("anon_suppress() reaches the target on random data and changes only subjects at risk", {
  set.seed(7)
  n <- 400
  dm <- data.frame(
    USUBJID = sprintf("S%03d", seq_len(n)),
    AGE = sample(seq(20, 80, by = 10), n, replace = TRUE),
    SEX = sample(c("F", "M"), n, replace = TRUE),
    RACE = sample(c("A", "B", "C", "D"), n, replace = TRUE, prob = c(0.7, 0.2, 0.07, 0.03)),
    SITE = sample(sprintf("%02d", 1:8), n, replace = TRUE)
  )
  quasi <- c("AGE", "SEX", "RACE", "SITE")
  before <- anon_risk_subjects(dm, quasi = quasi, na = "wildcard")
  out <- anon_suppress(dm, quasi = quasi, max_risk = 0.2)$data
  after <- anon_risk_subjects(out, quasi = quasi, na = "wildcard")
  expect_true(all(after$k >= 5))
  changed <- rowSums(is.na(out[quasi])) > 0
  expect_true(all(before$k[changed] < 5))
  expect_identical(out[!changed, ], dm[!changed, ])
})

test_that("anon_suppress() validates `order` and chains with other steps", {
  expect_error(anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), order = "XX"), "`order` names `XX`")
  expect_error(anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), order = c("AGE", "AGE")), "without duplicates")
  expect_error(anon_suppress(sup_dm(), quasi = "AGE", method = "x"), "should be one of")
  step <- anon_generalize(sup_dm(), rules = list(AGE = anon_band(20)))
  step <- anon_suppress(step, quasi = c("AGE", "SEX"), max_risk = 1/2)
  expect_identical(unique(step$log$step), c("generalize", "suppress"))
  ids <- anon_subject_ids(step, seed = 3)
  expect_named(ids$settings, c("generalize", "suppress", "subject_ids"))
  expect_true(anon_risk(ids, quasi = c("AGE", "SEX"), max_risk = 1/2, na = "wildcard")$summary$meets_target)
})

test_that("dropping subjects names datasets whose nested data frames hold the key", {
  dm <- sup_dm()
  dm$SUB <- I(lapply(dm$USUBJID, function(k) data.frame(USUBJID = k)))
  expect_warning(
    out <- anon_suppress(list(dm = dm), quasi = list(dm = c("AGE", "SEX")), max_risk = 1/2, method = "drop"),
    "data frames nested in these datasets still hold rows with `USUBJID`[^:]*: dm\\."
  )
  expect_identical(nrow(out$data$dm), 4L)
  expect_false(holds_nested_key(sup_dm(), "USUBJID"))
})

test_that("the log and settings hold no values", {
  dm <- sup_dm()
  dm$SEX[6] <- "QQRARE"
  result <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/2)
  text <- c(capture.output(print(result)), deparse(unclass(result)[c("log", "settings")]), as_anon_json(result))
  expect_false(any(grepl("QQRARE|S0[1-6]", text)))
})
