sup_dm <- function() {
  data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    AGE = c(30, 30, 30, 40, 40, 50),
    SEX = c("F", "F", "M", "M", "M", "F"),
    stringsAsFactors = FALSE
  )
}

# k-anonymity of the released table, written apart from the package: each
# released record's pattern is its values, with a cell blanked (missing in
# `released` but not in `truth`) as "<blank>" and a real missing value as
# "<NA>"; k is the number of records with the same pattern. `truth` is
# matched to `released` by USUBJID.
sup_release_k <- function(truth, released, qs) {
  truth <- truth[match(released$USUBJID, truth$USUBJID), , drop = FALSE]
  missing <- function(x) is.na(x) | (!is.na(x) & trimws(x) == "")
  pattern <- vapply(qs, function(q) {
    rel <- as.character(released[[q]])
    tru <- as.character(truth[[q]])
    ifelse(missing(rel) & !missing(tru), "<blank>", ifelse(missing(rel), "<NA>", rel))
  }, character(nrow(released)))
  key <- apply(matrix(pattern, nrow(released)), 1, paste, collapse = "\r")
  as.vector(table(key)[key])
}

test_that("anon_suppress() blanks cells until every released pattern is shared by enough records", {
  result <- anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), max_risk = 1/2)
  out <- result$data
  # S03 (30, M) and S06 (50, F) are alone. Blanking AGE or SEX on both
  # brings neither into a group, nor merges them: AGE goes first (first in
  # `quasi`); then blanking SEX on both makes them one pattern of two.
  expect_identical(out$AGE, c(30, 30, NA, 40, 40, NA))
  # Blank text is "" unless the column uses NA for missing text.
  expect_identical(out$SEX, c("F", "F", "", "M", "M", ""))
  expect_true(all(sup_release_k(sup_dm(), out, c("AGE", "SEX")) >= 2))
  risk <- anon_risk(result, quasi = c("AGE", "SEX"), max_risk = 1/2)
  expect_true(risk$summary$meets_target)
  expect_identical(risk$summary$min_k, 2L)
  expect_identical(risk$n_blanked, 4L)

  # With SEX less useful, SEX goes first; the result is the same here.
  out2 <- anon_suppress(sup_dm(), quasi = c("AGE", "SEX"), max_risk = 1/2, order = c("SEX", "AGE"))
  expect_identical(out2$data, out)

  log <- result$log
  expect_identical(log$step, rep("suppress", 3))
  expect_identical(log$variable[-1], c("AGE", "SEX"))
  expect_identical(log$n_changed[-1], c(2L, 2L))
  expect_match(log$detail[2], "blanked for 2 subject\\(s\\)")
  expect_match(log$detail[1], "before: min k 1, 2 subject\\(s\\) below 2; after: min k 2, 0 subject\\(s\\) below 2")
  expect_true(result$settings$suppress$meets_target)
  expect_identical(result$settings$suppress$min_k, 2L)
  expect_identical(result$settings$suppress$order, c("AGE", "SEX"))
})

test_that("the reviewer's example: a rare subject is hidden in a group, not behind blank records", {
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:24),
    AGE = c(rep(30, 12), rep(30, 11), 80),
    SEX = c(rep("M", 12), rep("F", 11), "F")
  )
  out <- anon_suppress(dm, c("AGE", "SEX"))
  # The 11 women of 30 and S24 (80, F) are all short: AGE blanked on the 12
  # of them makes one pattern of 12.
  expect_identical(is.na(out$data$AGE), dm$SEX == "F")
  expect_true(all(sup_release_k(dm, out$data, c("AGE", "SEX")) >= 12))
  expect_true(out$settings$suppress$meets_target)
})

test_that("records blanked everywhere hide only among each other", {
  set.seed(3)
  dm <- data.frame(USUBJID = sprintf("S%03d", 1:300))
  for (j in 1:8) dm[[paste0("Q", j)]] <- sample(1:3, 300, TRUE)
  qs <- paste0("Q", 1:8)
  out <- suppressWarnings(anon_suppress(dm, qs, max_risk = 1/5))
  k <- sup_release_k(dm, out$data, qs)
  expect_true(all(k >= 5))
  # A record that keeps every value is in a group of such records.
  shown <- rowSums(is.na(out$data[qs])) == 0
  expect_true(all(k[shown] >= 5))
  expect_lt(sum(shown), 300)
  expect_identical(anon_risk(out, qs, max_risk = 1/5)$summary$min_k >= 5L, TRUE)
})

test_that("real missing values are values, not wildcards", {
  set.seed(1)
  dm <- data.frame(
    USUBJID = sprintf("S%03d", 1:51),
    AGE = c(sample(20:80, 40), rep(NA, 11)),
    SEX = c(sample(c("M", "F"), 40, TRUE), rep(NA, 11))
  )
  expect_false(anon_risk(dm, c("AGE", "SEX"))$summary$meets_target)
  # The 11 subjects with nothing recorded form a group of 11, and missing
  # values can't be blanked: they are dropped.
  expect_warning(out <- anon_suppress(dm, c("AGE", "SEX")), "11 subject\\(s\\) could not reach groups of at least 12")
  expect_false(any(out$data$USUBJID %in% dm$USUBJID[41:51]))
  expect_gt(sum(out$log$n_changed[out$log$step == "suppress"], na.rm = TRUE), 0)
  expect_true(all(sup_release_k(dm, out$data, c("AGE", "SEX")) >= 12))
})

test_that("the blank that brings the most records into groups wins; what's left short is dropped", {
  dm <- data.frame(USUBJID = c("X", "Y", "Z"), A = c(1, 1, 2), B = c(1, 1, 2), C = c(9, 5, 9))
  # Blanking C brings X and Y together; Z, alone even with everything
  # blanked, is dropped.
  expect_warning(
    res <- anon_suppress(dm, quasi = c("A", "B", "C"), max_risk = 1/2),
    "1 subject\\(s\\) could not reach groups of at least 2"
  )
  expect_identical(res$data$USUBJID, c("X", "Y"))
  expect_identical(res$data$C, c(NA_real_, NA_real_))
  expect_identical(res$data$A, c(1, 1))
  expect_true(res$settings$suppress$meets_target)
})

test_that("anon_suppress() blanks every row of a test with its text results", {
  dm <- sup_dm()
  dm$SEX[5] <- "F"
  vs <- data.frame(
    USUBJID = c(rep(dm$USUBJID, each = 2), "S06"),
    VSTESTCD = c(rep(c("WEIGHT", "HEIGHT"), 6), "WEIGHT"),
    VSORRES = c("70", "", "70", "160", "70", "160", "70", "170", "90", "170", "95", "160", "96"),
    VSSTRESC = c("70", "160", "70", "160", "70", "160", "70", "170", "90", "170", "95", "160", "96"),
    VSSTRESN = c(70, 160, 70, 160, 70, 160, 70, 170, 90, 170, 95, 160, 96),
    VSBLFL = c(rep("Y", 12), ""),
    stringsAsFactors = FALSE
  )
  vs$VSSTRESC[2] <- NA
  quasi <- list(dm = "SEX", anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN", filter = ~ VSBLFL == "Y"))
  result <- anon_suppress(list(dm = dm, vs = vs), quasi = quasi, max_risk = 1/2)
  out <- result$data$vs
  # S05 (F, 90) and S06 (F, 95) are alone; blanking WEIGHT brings them
  # together (blanking SEX wouldn't), in every row of the test, the
  # non-baseline row of S06 too.
  hit <- out$USUBJID %in% c("S05", "S06") & out$VSTESTCD == "WEIGHT"
  expect_identical(sum(hit), 3L)
  expect_identical(out$VSSTRESN[hit], rep(NA_real_, 3))
  # Blank text is "" unless the column uses NA for missing text.
  expect_identical(out$VSORRES[hit], rep("", 3))
  expect_identical(out$VSSTRESC[hit], rep(NA_character_, 3))
  expect_identical(out[!hit, ], vs[!hit, ])
  expect_identical(result$data$dm, dm)
  log <- result$log
  expect_identical(log$variable[-1], c("VSORRES (VSTESTCD = WEIGHT)", "VSSTRESC (VSTESTCD = WEIGHT)", "VSSTRESN (VSTESTCD = WEIGHT)"))
  expect_identical(log$n_changed[-1], c(3L, 3L, 3L))
  expect_true(result$settings$suppress$meets_target)
  # The step records the blanked cells by subject, never by value.
  expect_identical(result$suppressed$key, c("S05", "S06"))
  expect_identical(unique(result$suppressed$test), "WEIGHT")
  expect_true(anon_risk(result, quasi, max_risk = 1/2)$summary$meets_target)
})

test_that("blanked factors keep their class; levels held only by blanked values go", {
  dm <- sup_dm()
  dm$SEX <- factor(dm$SEX)
  dm$AGE <- factor(c(30, 30, 30, 45, 40, 50))
  # The record left alone takes a donor from a group of three.
  expect_no_warning(res <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/2))
  out <- res$data
  expect_s3_class(out$AGE, "factor")
  expect_true(all(sup_release_k(dm, out, c("AGE", "SEX")) >= 2))
  expect_true(all(levels(out$AGE) %in% as.character(out$AGE)))
})

test_that("anon_risk() measures a suppressed step from its record of blanked cells", {
  dm <- rbind(sup_dm(), data.frame(USUBJID = c("S07", "S08"), AGE = 40, SEX = NA))
  # S07 and S08 (40, missing) share a pattern; S03 (30, M) and S06 (50, F)
  # are blanked into one. A real missing value isn't a blank: the two pairs
  # stay apart.
  result <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/2)
  expect_identical(anon_risk_subjects(result, quasi = c("AGE", "SEX"))$k, rep(2L, 8))
  r <- anon_risk(result, quasi = c("AGE", "SEX"), max_risk = 1/2)
  expect_identical(r$n_blanked, 4L)
  expect_output(print(r), "Cells blanked by suppression: 4")
  # The symmetric wildcard count is kept for comparison: the blanked pair
  # (both blank) matches everyone there.
  expect_identical(anon_risk_subjects(result, quasi = c("AGE", "SEX"), na = "wildcard")$k[c(3, 6)], c(8L, 8L))

  # Later steps carry the record on, with the new keys.
  ids <- anon_subject_ids(result, seed = 3)
  expect_identical(anon_risk(ids, quasi = c("AGE", "SEX"), max_risk = 1/2)$summary, r$summary)
  expect_false(any(ids$suppressed$key %in% dm$USUBJID))
  expect_true(all(ids$suppressed$key %in% ids$data$USUBJID))
  dated <- anon_subject_dates(ids, seed = 4)
  expect_identical(dated$suppressed, ids$suppressed)

  # A second suppression counts the first one's cells as blanked.
  again <- suppressWarnings(anon_suppress(result, quasi = c("AGE", "SEX"), max_risk = 1/3))
  expect_true(anon_risk(again, quasi = c("AGE", "SEX"), max_risk = 1/3)$summary$meets_target)
  expect_true(all(result$suppressed$key %in% again$suppressed$key | !result$suppressed$key %in% again$data$USUBJID))
})

test_that("method = \"drop\" removes subjects in small groups at any depth", {
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
  expect_no_warning(res <- anon_suppress(dm, quasi = quasi, max_risk = 0.2))
  out <- res$data
  expect_true(all(sup_release_k(dm, out, quasi) >= 5))
  same <- dm[match(out$USUBJID, dm$USUBJID), ]
  untouched <- rowSums(is.na(out[quasi]) != is.na(same[quasi]) | (!is.na(out[quasi]) & out[quasi] == "")) == 0
  expect_identical(out[untouched, quasi], same[untouched, quasi], ignore_attr = TRUE)

  wide <- data.frame(USUBJID = sprintf("W%03d", 1:60))
  for (j in 1:13) wide[[paste0("Q", j)]] <- sample(1:3, 60, TRUE)
  qs <- paste0("Q", 1:13)
  expect_no_warning(res <- anon_suppress(wide, qs, max_risk = 1/3))
  expect_true(all(sup_release_k(wide, res$data, qs) >= 3))
  expect_true(res$settings$suppress$meets_target)
})

test_that("`everywhere` blanks a quasi-identifier's copies in other datasets and nested data", {
  dm <- sup_dm()
  dm$HIST <- I(lapply(seq_len(6), function(i) data.frame(USUBJID = dm$USUBJID[i], AGE = dm$AGE[i])))
  adsl <- sup_dm()[c("USUBJID", "AGE")]
  adae <- data.frame(USUBJID = rep(dm$USUBJID, 2), AGE = rep(dm$AGE, 2))
  data <- list(dm = dm, adsl = adsl, adae = adae)
  out <- anon_suppress(data, list(dm = c("AGE", "SEX")), max_risk = 1/2)
  blanked <- c("S03", "S06")
  expect_identical(is.na(out$data$adsl$AGE), adsl$USUBJID %in% blanked)
  expect_identical(is.na(out$data$adae$AGE), adae$USUBJID %in% blanked)
  expect_identical(vapply(out$data$dm$HIST, function(h) is.na(h$AGE), logical(1)), dm$USUBJID %in% blanked)
  expect_true(out$settings$suppress$meets_target)
  expect_match(out$log$detail, "copy of `AGE` in `dm`", all = FALSE)

  warnings <- character(0)
  only <- withCallingHandlers(
    anon_suppress(data, list(dm = c("AGE", "SEX")), max_risk = 1/2, everywhere = FALSE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 2)
  expect_match(warnings[1], "`AGE` was blanked in `dm` only; these datasets hold it too: adsl, adae")
  expect_match(warnings[2], "`AGE` in these datasets differs from `dm` for some subjects [^:]*: adsl, adae")
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
  expect_true(all(sup_release_k(dm, partial$data, c("A", "B", "C")) >= 2))

  gen <- anon_generalize(sup_dm(), rules = list(AGE = anon_band(20)))
  step <- anon_suppress(gen, quasi = c("AGE", "SEX"), max_risk = 1/2)
  expect_identical(unique(step$log$step), c("generalize", "suppress"))
  expect_true(step$settings$suppress$meets_target)
  expect_true(anon_risk(step, c("AGE", "SEX"), max_risk = 1/2)$summary$meets_target)
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

test_that("a blanked cell never matches a real missing value", {
  dm <- rbind(sup_dm(), data.frame(USUBJID = "S07", AGE = NA, SEX = NA))
  # S03 and S06 are blanked into one pattern (blank, blank); S07, missing
  # both values for real, shows the same thing but isn't blanked: it stays
  # alone and is dropped.
  expect_warning(
    res <- anon_suppress(dm, c("AGE", "SEX"), max_risk = 1/2),
    "1 subject\\(s\\) could not reach groups of at least 2"
  )
  expect_false("S07" %in% res$data$USUBJID)
  expect_identical(nrow(res$suppressed), 4L)
  expect_true(all(sup_release_k(dm, res$data, c("AGE", "SEX")) >= 2))
  # Kept, it is measured alone from the step too.
  expect_warning(kept <- anon_suppress(dm, c("AGE", "SEX"), max_risk = 1/2, unresolved = "keep"), "remain in groups")
  expect_identical(anon_risk_subjects(kept, c("AGE", "SEX"))$k, c(2L, 2L, 2L, 2L, 2L, 2L, 1L))
})

test_that("a group still too small takes donors from a group that can spare them", {
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:24),
    AGE = c(rep(30, 22), 71, 85),
    SEX = c(rep("F", 22), "F", "M")
  )
  # S23 and S24, blanked everywhere, form a group of 2; ten of the 22 (30, F)
  # records give AGE and SEX to join them, leaving 12.
  expect_no_warning(res <- anon_suppress(dm, c("AGE", "SEX")))
  expect_identical(nrow(res$data), 24L)
  blank <- is.na(res$data$AGE)
  expect_identical(sum(blank), 12L)
  expect_true(all(res$data$SEX[blank] == ""))
  expect_true(all(sup_release_k(dm, res$data, c("AGE", "SEX")) >= 12))
  expect_true(res$settings$suppress$meets_target)
})

test_that("copies of a quasi-identifier that differ from its own dataset fail the target", {
  dm <- data.frame(USUBJID = sprintf("K%02d", 1:24), AGE = rep(c(31, 47), 12), SEX = "F")
  adsl <- dm[c("USUBJID", "AGE")]
  gen <- suppressWarnings(anon_generalize(list(dm = dm, adsl = adsl), list(dm = list(AGE = anon_band(20)))))
  expect_warning(
    res <- anon_suppress(gen, list(dm = c("AGE", "SEX"))),
    "`AGE` in these datasets differs from `dm` for some subjects \\(not generalized or blanked the same way\\): adsl"
  )
  expect_false(res$settings$suppress$meets_target)
  expect_true("copies of `AGE` differ from `dm`" %in% res$settings$suppress$not_met)
  # Generalized in both: the same values, no warning.
  gen2 <- anon_generalize(list(dm = dm, adsl = adsl), list(dm = list(AGE = anon_band(20)), adsl = list(AGE = anon_band(20))))
  expect_no_warning(res2 <- anon_suppress(gen2, list(dm = c("AGE", "SEX"))))
  expect_true(res2$settings$suppress$meets_target)
})

test_that("anon_risk() reads blanked cells through a result's siblings and through copies", {
  k <- sprintf("K%02d", 1:12)
  vs <- data.frame(USUBJID = k, VSTESTCD = "WEIGHT", VSSTRESN = c(rep(NA, 6), 61:66))
  vs$VSSTRESC <- ifelse(is.na(vs$VSSTRESN), "", as.character(vs$VSSTRESN))
  vs$VSORRES <- vs$VSSTRESC
  expect_warning(
    s <- anon_suppress(list(vs = vs), anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN"), unresolved = "keep"),
    "remain in groups"
  )
  for (rc in c("VSSTRESN", "VSSTRESC", "VSORRES")) {
    r <- suppressWarnings(anon_risk(s, anon_quasi_values("vs", "VSTESTCD", "WEIGHT", rc)))
    expect_identical(r$n_blanked, 6L, info = rc)
    expect_false(r$summary$meets_target, info = rc)
  }

  dm <- sup_dm()
  adsl <- sup_dm()[c("USUBJID", "AGE")]
  res <- anon_suppress(list(dm = dm, adsl = adsl), list(dm = c("AGE", "SEX")), max_risk = 1/2)
  r <- anon_risk(res, list(adsl = "AGE", dm = "SEX"), max_risk = 1/2)
  # AGE of S03 and S06 in the adsl copy, and their SEX.
  expect_identical(r$n_blanked, 4L)
  expect_true(r$summary$meets_target)
})

test_that("min_k and meets_target are measured on the released data", {
  # K012 refers to K013 in RSUBJID: removing K013 removes K012's row too,
  # leaving 11 of age 30.
  dm <- data.frame(USUBJID = sprintf("K%03d", 1:14), AGE = c(rep(30, 12), 71, 85), RSUBJID = c(rep("", 11), "K013", "", ""))
  res <- anon_suppress(dm, "AGE", method = "drop")
  expect_identical(nrow(res$data), 11L)
  expect_identical(res$settings$suppress$min_k, 11L)
  expect_false(res$settings$suppress$meets_target)
  expect_identical(anon_risk(res, "AGE")$summary$min_k, 11L)
})

test_that("removed subjects leave the record of blanked cells and their factor levels", {
  dm <- data.frame(USUBJID = sprintf("ORIG-%03d", 1:16), AGE = c(rep(30, 12), rep(40, 3), 50), SEX = c(rep("F", 12), "F", "F", "M", "M"))
  s1 <- anon_suppress(dm, c("AGE", "SEX"), max_risk = 1/2)
  expect_gt(nrow(s1$suppressed), 0)
  s2 <- anon_suppress(s1, c("AGE", "SEX"), max_risk = 0.09, method = "drop")
  dropped <- setdiff(dm$USUBJID, s2$data$USUBJID)
  expect_gt(length(dropped), 0)
  expect_false(any(s2$suppressed$key %in% dropped))

  fk <- data.frame(USUBJID = factor(sprintf("ORIG-%03d", 1:14)), AGE = c(rep(30, 12), 71, 85))
  fk$SITE <- factor(c(rep("S01", 12), "S99", "S77"), levels = c("S01", "S77", "S99", "UNUSED"))
  out <- anon_suppress(fk, "AGE", method = "drop")$data
  expect_identical(levels(out$USUBJID), sprintf("ORIG-%03d", 1:12))
  expect_identical(levels(out$SITE), c("S01", "UNUSED"))
})

test_that("a risk measured without a step says so", {
  expect_output(print(anon_risk(sup_dm(), c("AGE", "SEX"))), "Measured without a step's record of suppressed cells")
  step <- anon_suppress(sup_dm(), c("AGE", "SEX"), max_risk = 1/2)
  out <- capture.output(print(anon_risk(step, c("AGE", "SEX"))))
  expect_false(any(grepl("Measured without", out)))
})
