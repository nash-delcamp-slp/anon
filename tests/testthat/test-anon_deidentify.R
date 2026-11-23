# A profile with the key and the given rules.
deid_test_profile <- function(...) anon_profile(anon_profile_rule("USUBJID", "key"), ..., name = "test")

# Twelve subjects in SDTM and ADaM form: dm and adsl (with copies of AGE,
# SEX and RACE, derived AGEGR1, AGEGR1N, RACEN, baseline weight WEIGHTBL and
# BMIBL), vs (WEIGHT twice per subject, the first flagged as baseline;
# HEIGHT once, unflagged) and advs (WEIGHT and BMI rows, with ADSL copies).
deid_test_data <- function() {
  keys <- sprintf("S%02d", 1:12)
  age <- c(31, 33, 36, 38, 41, 43, 46, 48, 51, 53, 56, 58)
  sex <- rep(c("F", "M"), 6)
  race <- c(rep("WHITE", 9), "ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")
  weight <- c(61.2, 64.8, 66.1, 70.4, 71.9, 74.3, 76.8, 80.2, 82.5, 85.1, 88.7, 93.6)
  height <- c(158, 161, 163, 166, 168, 170, 172, 175, 177, 180, 183, 186)
  start <- as.Date("2020-01-06") + 7 * (0:11)
  dm <- data.frame(
    USUBJID = keys, SUBJID = sprintf("%04d", 1:12), AGE = age, AGEU = "YEARS", SEX = sex, RACE = race,
    BRTHDTC = format(start - 365 * age), INVNAM = "Dr Who", RFSTDTC = format(start),
    RFENDTC = format(start + c(rep(90, 11), 30)), stringsAsFactors = FALSE
  )
  adsl <- data.frame(
    USUBJID = keys, AGE = age, AGEGR1 = ifelse(age < 45, "<45", ">=45"), AGEGR1N = ifelse(age < 45, 1, 2),
    SEX = sex, RACE = race, RACEN = match(race, c("WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN", "OTHER")),
    WEIGHTBL = weight, BMIBL = round(weight / (height / 100)^2, 1), BRTHDT = start - 365 * age,
    TRTSDT = start, TRTEDT = start + 90, stringsAsFactors = FALSE
  )
  vs <- data.frame(
    USUBJID = rep(keys, each = 3), VSTESTCD = rep(c("HEIGHT", "WEIGHT", "WEIGHT"), 12),
    VSSTRESN = as.vector(rbind(height, weight, weight + 0.5)), VSBLFL = rep(c("", "Y", ""), 12),
    VSDTC = format(rep(start, each = 3) + c(-7, 0, 28)), stringsAsFactors = FALSE
  )
  vs$VSSTRESC <- as.character(vs$VSSTRESN)
  vs$VSORRES <- vs$VSSTRESC
  advs <- data.frame(
    USUBJID = rep(keys, each = 2), AGE = rep(age, each = 2), SEX = rep(sex, each = 2), RACE = rep(race, each = 2),
    PARAMCD = rep(c("WEIGHT", "BMI"), 12), AVAL = as.vector(rbind(weight, round(weight / (height / 100)^2, 1))),
    ABLFL = "Y", ADT = rep(start, each = 2), stringsAsFactors = FALSE
  )
  list(dm = dm, adsl = adsl, vs = vs, advs = advs)
}

deid_quiet <- function(expr) suppressWarnings(expr)

test_that("direct identifiers are blanked in every row, keeping the column and its type", {
  data <- deid_test_data()
  attr(data$dm$INVNAM, "label") <- "Investigator Name"
  r <- deid_quiet(anon_deidentify(data, c(anon_profile_sdtm(3), anon_profile_adam(3)), max_risk = 1/3, seed = 1))
  expect_true(all(r$data$dm$BRTHDTC == ""))
  expect_true(all(r$data$dm$INVNAM == ""))
  expect_identical(attr(r$data$dm$INVNAM, "label"), "Investigator Name")
  expect_s3_class(r$data$adsl$BRTHDT, "Date")
  expect_true(all(is.na(r$data$adsl$BRTHDT)))
  removed <- r$log[r$log$step == "remove" & !is.na(r$log$dataset), ]
  expect_setequal(paste(removed$dataset, removed$variable), c("dm BRTHDTC", "dm INVNAM", "adsl BRTHDT"))
  expect_identical(removed$n_changed[removed$variable == "INVNAM"], 12L)
  expect_identical(r$settings$remove$columns, c("dm.BRTHDTC", "dm.INVNAM", "adsl.BRTHDT"))
})

test_that("subjects get one pseudonym and one offset in every dataset, from the seed", {
  data <- deid_test_data()
  profile <- c(anon_profile_sdtm(3), anon_profile_adam(3))
  r <- deid_quiet(anon_deidentify(data, profile, max_risk = 1/3, seed = 7, keep_secrets = TRUE))
  ids <- anon_secrets(r)$subject_ids
  offsets <- anon_secrets(r)$date_offsets
  expect_setequal(ids$key, data$dm$USUBJID)
  new <- stats::setNames(ids$pseudonym, ids$key)
  # dm and adsl agree on each subject's offset.
  off <- stats::setNames(offsets$offset_days, offsets$key)
  dm_start <- as.Date(r$data$dm$RFSTDTC)
  expect_identical(as.numeric(dm_start - as.Date(data$dm$RFSTDTC[match(r$data$dm$USUBJID, new)])), as.numeric(off[r$data$dm$USUBJID]))
  expect_identical(r$data$adsl$TRTSDT[match(r$data$dm$USUBJID, r$data$adsl$USUBJID)], dm_start)
  expect_identical(r$data$dm$SUBJID, sub("SUBJ-", "", r$data$dm$USUBJID))
  again <- deid_quiet(anon_deidentify(data, profile, max_risk = 1/3, seed = 7))
  expect_identical(again$data, r$data)
  expect_null(again$secrets)
  expect_true(r$settings$deidentify$seeded)
})

test_that("the ladder search takes the step that leaves the least to suppress, and stops within the budget", {
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:12), AGE = rep(c(30, 40, 50, 60), each = 3),
    WT = c(61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93)
  )
  profile <- deid_test_profile(
    anon_profile_rule("AGE", "quasi", ladder = list(anon_band(20))),
    anon_profile_rule("WT", "quasi", ladder = list(anon_round(-1)))
  )
  r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, max_suppressed = 0, seed = 1)
  q <- r$settings$deidentify$quasi
  expect_identical(q$level, c(0L, 1L))
  expect_identical(q$step, c(NA, "anon_round(digits = -1)"))
  expect_identical(r$settings$deidentify$moves$quasi, "WT")
  expect_equal(r$settings$deidentify$moves$cells_after, 0)
  expect_identical(sort(r$data$dm$WT), rep(c(60, 70, 80, 90), each = 3))
  expect_true(r$settings$deidentify$meets_target)
  expect_false(r$settings$deidentify$exhausted)
  # Nothing to suppress from the start: nothing is generalized.
  dm$WT <- rep(c(60, 70, 80, 90), each = 3)
  r0 <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, max_suppressed = 0, seed = 1)
  expect_identical(r0$settings$deidentify$quasi$level, c(0L, 0L))
  expect_identical(nrow(r0$settings$deidentify$moves), 0L)
})

test_that("ties between steps go to the one that loses the fewest distinct values", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:6), Q2 = c(100, 100, 200, 200, 300, 300), Q1 = c(10, 11, 10, 11, 10, 11))
  profile <- deid_test_profile(
    anon_profile_rule("Q2", "quasi", ladder = list(anon_round(-3))),
    anon_profile_rule("Q1", "quasi", ladder = list(anon_round(-1)))
  )
  r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/2, max_suppressed = 0, seed = 1)
  # Either step alone leaves nothing to suppress; Q1's loses 1 distinct value,
  # Q2's loses 2.
  expect_identical(r$settings$deidentify$quasi$level, c(0L, 1L))
})

test_that("steps that turn out unneeded are undone, and exhausted ladders keep the least suppression", {
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:12), A = rep(c(30, 40, 50, 60), each = 3), B = rep(c(1, 1, 2), 4),
    W = c(61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93)
  )
  profile <- deid_test_profile(
    anon_profile_rule("A", "quasi"),
    anon_profile_rule("B", "quasi", ladder = list(anon_fold(4, to = 9))),
    anon_profile_rule("W", "quasi", ladder = list(anon_round(0), anon_round(-1)))
  )
  r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, max_suppressed = 0, seed = 1)
  moves <- r$settings$deidentify$moves
  # The fold changes nothing (every category holds 4 subjects or more): taken
  # on a plateau, then undone.
  expect_identical(moves$quasi, c("B", "W", "W", "B"))
  expect_identical(moves$level, c(1L, 1L, 2L, 0L))
  expect_identical(r$settings$deidentify$quasi$level, c(0L, 0L, 2L))
  expect_true(r$settings$deidentify$exhausted)
  expect_true(any(grepl("B back to its original values, not needed", r$log$detail)))
  expect_true(r$settings$deidentify$meets_target)
})

test_that("when every ladder is exhausted, the search goes back to the state needing the least suppression", {
  # The profile pipelines' subjects (see deid_profile_expected()): WEIGHT in
  # bands of 20, the last step, needs more cells than rounding to tens.
  dm <- data.frame(
    USUBJID = sprintf("S%d", 1:8), AGE = c(34, 37, 51, 58, 43, 47, 62, 66),
    SEX = c("F", "F", "M", "M", "F", "M", "F", "F"), RACE = c(rep("WHITE", 4), "ASIAN", rep("WHITE", 3)),
    WT = c(71.3, 69.6, 70.8, 73.2, 61.4, 58.6, 79.5, 88.7)
  )
  profile <- deid_test_profile(
    anon_profile_rule("AGE", "quasi", ladder = list(anon_band(5, top = 90), anon_band(10, top = 90), anon_band(20, top = 90))),
    anon_profile_rule("SEX", "quasi"),
    anon_profile_rule("RACE", "quasi", ladder = list(anon_fold(2, "OTHER"))),
    anon_profile_rule("WT", "quasi", ladder = list(anon_round(0), anon_round(-1), anon_band(20)))
  )
  r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/2, seed = 1)
  moves <- r$settings$deidentify$moves
  back <- which(is.na(moves$quasi))
  expect_length(back, 1)
  # It goes back to the fewest cells the climb reached.
  climb <- moves[seq_len(back - 1), ]
  expect_lt(moves$cells_after[back], moves$cells_before[back])
  expect_equal(moves$cells_after[back], min(c(climb$cells_before[1], climb$cells_after)))
  expect_true(any(grepl("ladders exhausted above max_suppressed; back to the levels after move [0-9]+", r$log$detail)))
  expect_identical(r$settings$deidentify$quasi$step[4], "anon_round(digits = -1)")
})

test_that("one generalization applies to every copy, and suppression blanks every copy", {
  data <- deid_test_data()
  r <- deid_quiet(anon_deidentify(data, c(anon_profile_sdtm(3), anon_profile_adam(3)), max_risk = 1/3, seed = 2, keep_secrets = TRUE))
  d <- r$data
  q <- r$settings$deidentify$quasi
  expect_identical(q$source[q$quasi == "AGE"], "dm.AGE")
  expect_identical(q$source[q$quasi == "WEIGHT"], "vs.VSSTRESN (VSTESTCD = WEIGHT)")
  expect_identical(q$copies[q$quasi == "WEIGHT"], "vs.VSSTRESN (VSTESTCD = WEIGHT), adsl.WEIGHTBL, advs.AVAL (PARAMCD = WEIGHT)")
  by_key <- function(df, col) df[[col]][match(d$dm$USUBJID, df$USUBJID)]
  for (col in c("AGE", "SEX", "RACE")) {
    expect_identical(by_key(d$adsl, col), d$dm[[col]], info = col)
    expect_identical(by_key(d$advs, col), d$dm[[col]], info = col)
  }
  # Baseline weight: the vs baseline row, WEIGHTBL and the advs WEIGHT row
  # show the same released value (generalized alike, blanked for the same
  # subjects).
  base_vs <- d$vs[d$vs$VSTESTCD == "WEIGHT" & d$vs$VSBLFL == "Y", ]
  expect_identical(by_key(d$adsl, "WEIGHTBL"), by_key(base_vs, "VSSTRESN"))
  expect_identical(by_key(d$advs[d$advs$PARAMCD == "WEIGHT", ], "AVAL"), by_key(base_vs, "VSSTRESN"))
  # Every WEIGHT row of a subject is blanked where its baseline is.
  blank <- d$dm$USUBJID[is.na(by_key(base_vs, "VSSTRESN"))]
  weights <- d$vs[d$vs$VSTESTCD == "WEIGHT", ]
  expect_true(all(is.na(weights$VSSTRESN[weights$USUBJID %in% blank])))
  expect_true(all(weights$VSSTRESC[weights$USUBJID %in% blank] == ""))
  # The suppressed copies are in the record of blanked cells.
  if (length(blank) > 0) {
    rec <- r$suppressed
    expect_setequal(rec$key[rec$dataset == "adsl" & rec$column %in% "WEIGHTBL"], blank)
  }
  # The released data meets the target, measured from the step.
  expect_true(r$settings$deidentify$meets_target)
  expect_true(r$risk_after$summary$meets_target)
  expect_identical(r$risk_after$summary$k_target, 3)
})

test_that("a quasi-identifier removed by its ladder is blanked in every copy", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:12), AGE = rep(c(30, 40, 50, 60), each = 3), SITEID = sprintf("%02d", 1:12))
  adsl <- dm
  profile <- deid_test_profile(
    anon_profile_rule("AGE", "quasi"),
    anon_profile_rule("SITEID", "quasi", ladder = list("remove"))
  )
  r <- anon_deidentify(list(dm = dm, adsl = adsl), profile, max_risk = 1/3, max_suppressed = 0, seed = 2)
  # Removing SITEID costs as many cells as blanking it for everyone: taken
  # above the budget, then undone as it needs no fewer cells; suppression
  # blanks it for everyone.
  expect_identical(r$settings$deidentify$moves$step, c("remove", NA))
  expect_true(all(r$data$dm$SITEID == ""))
  expect_true(all(r$data$adsl$SITEID == ""))
  # Removed when every subject is unresolved without it.
  r2 <- anon_deidentify(list(dm = dm[1:2, ], adsl = adsl[1:2, ]), profile, max_risk = 1/2, max_suppressed = 0, seed = 2)
  expect_true(all(r2$data$dm$SITEID == ""))
})

test_that("removing a quasi-identifier counts every one of its cells", {
  # Three large sites and two small ones: folding the small ones costs far
  # fewer cells than removing SITEID.
  n <- 60
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:n), SEX = rep(c("F", "M"), n / 2),
    SITEID = c(rep("101", 20), rep("102", 18), rep("103", 16), rep("104", 4), rep("105", 2))
  )
  r <- anon_deidentify(list(dm = dm), anon_profile_sdtm(min_n = 5), max_risk = 1/5, seed = 1)
  q <- r$settings$deidentify$quasi
  expect_identical(q$step[q$quasi == "SITEID"], "anon_fold(min_n = 5, to = \"OTHER\")")
  expect_setequal(unique(r$data$dm$SITEID[r$data$dm$SITEID != ""]), c("101", "102", "103", "OTHER"))
})

test_that("derived text values follow the released value where the original data maps it", {
  old <- c("WHITE", "WHITE", "ASIAN", "BLACK", "OTHER", "WHITE")
  new <- c("WHITE", "WHITE", "OTHER", "OTHER", "OTHER", "")
  derived <- c(1, 1, 2, 3, 9, 1)
  hit <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  m <- map_derived(old, new, derived, hit, anon_fold(2, "OTHER"))
  # Folded rows take OTHER's code (from row 5); the blanked row is blanked.
  expect_identical(m$from, c(0L, 0L, 5L, 5L, 0L, 0L))
  expect_identical(m$blank, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  # Without OTHER in the original data there is nothing to map to.
  m2 <- map_derived(old[-5], new[-5], derived[-5], hit[-5], anon_fold(2, "OTHER"))
  expect_identical(m2$blank, c(FALSE, FALSE, TRUE, TRUE, TRUE))
  # A derived value that isn't a function of the original value: blanked.
  m3 <- map_derived(c("A", "A", "B"), c("A", "A", "C"), c(1, 2, 3), c(TRUE, TRUE, TRUE), anon_fold(2, "C"))
  expect_true(all(m3$blank))
})

test_that("derived categories of a number follow a band only when it lies within one category", {
  age <- c(31, 33, 36, 44, 45, 48, 52, 58)
  group <- ifelse(age < 45, "<45", ">=45")
  hit <- rep(TRUE, 8)
  band5 <- anon_band(5)
  m <- map_derived(age, band5(age), group, hit, band5)
  # 30-34, 35-39 and 40-44 lie in "<45" (up to 44, open below); 45-49,
  # 50-54 and 55-59 in ">=45" (from 45, open above).
  expect_false(any(m$blank))
  expect_identical(group[m$from], group)
  band10 <- anon_band(10)
  m10 <- map_derived(age, band10(age), group, hit, band10)
  # 40-49 straddles the categories.
  expect_identical(m10$blank, age >= 40 & age < 50)
  # Ages that aren't whole numbers: the cut lies somewhere between 44 and
  # 45.5, so neither 40-44.99 nor 45-49.99 can be placed.
  frac <- c(31.5, 44, 45.5, 50)
  mf <- map_derived(frac, band5(frac), ifelse(frac < 45, "<45", ">=45"), rep(TRUE, 4), band5)
  expect_identical(mf$blank, c(FALSE, TRUE, TRUE, FALSE))
  # Categories that overlap aren't intervals: blanked.
  mo <- map_derived(c(30, 40, 35), c(30, 40, 35), c("a", "a", "b"), rep(TRUE, 3), band5)
  expect_true(all(mo$blank))
  # Top coding: 90 stands for 90 and over, beyond any observed range.
  expect_identical(released_range(90, anon_band(5, top = 90), TRUE), c(90, Inf))
  expect_identical(released_range(70, anon_round(-1), TRUE), c(65, 75))
  expect_identical(released_range(72.5, anon_band(5, value = "midpoint"), FALSE), c(70, 75))
})

test_that("derived variables are blanked where their quasi-identifiers changed, or follow them", {
  data <- deid_test_data()
  r <- deid_quiet(anon_deidentify(data, c(anon_profile_sdtm(3), anon_profile_adam(3)), max_risk = 1/3, seed = 2, keep_secrets = TRUE))
  d <- r$data
  adsl <- d$adsl[match(d$dm$USUBJID, d$adsl$USUBJID), ]
  ids <- anon_secrets(r)$subject_ids
  orig <- data$adsl[match(ids$key[match(d$dm$USUBJID, ids$pseudonym)], data$adsl$USUBJID), ]
  # AGEGR1 is kept only where the released AGE determines it.
  shown <- adsl$AGEGR1 != ""
  expect_true(all(is.na(adsl$AGE[shown]) == FALSE))
  expect_identical(adsl$AGEGR1[shown], orig$AGEGR1[shown])
  expect_identical(adsl$AGEGR1N[shown], orig$AGEGR1N[shown])
  expect_true(all(is.na(adsl$AGEGR1N[!shown])))
  # RACEN is blank where RACE is, and otherwise the code of the released RACE.
  codes <- c(WHITE = 1, ASIAN = 2, "BLACK OR AFRICAN AMERICAN" = 3, OTHER = 4)
  expect_true(all(is.na(adsl$RACEN[adsl$RACE == ""])))
  expect_equal(adsl$RACEN[adsl$RACE != "" & !is.na(adsl$RACEN)], unname(codes[adsl$RACE[adsl$RACE != "" & !is.na(adsl$RACEN)]]))
  # BMIBL (from HEIGHT and WEIGHT, not in its rows) is blanked for every
  # subject whose height or weight changed: all of them, as weights are
  # rounded.
  expect_true(all(is.na(adsl$BMIBL)))
  # So is the BMI parameter of advs.
  expect_true(all(is.na(d$advs$AVAL[d$advs$PARAMCD == "BMI"])))
  derived_log <- r$log[r$log$step == "deidentify" & grepl("^derived from", r$log$detail), ]
  expect_true(all(c("BMIBL", "AVAL (PARAMCD = BMI)") %in% derived_log$variable))
})

test_that("residual signals count subjects singled out by duration, row counts and tests", {
  data <- deid_test_data()
  # One subject has an extra vs row and an extra test.
  data$vs <- rbind(data$vs, data.frame(
    USUBJID = "S01", VSTESTCD = "PULSE", VSSTRESN = 70, VSBLFL = "", VSDTC = "2020-01-07", VSSTRESC = "70", VSORRES = "70"
  ))
  r <- deid_quiet(anon_deidentify(data, anon_profile_sdtm(3), max_risk = 1/3, seed = 2))
  s <- r$signals
  expect_identical(names(s), c("signal", "dataset", "n_subjects", "n_unique", "n_below_k", "n_below_k_with_quasi"))
  row <- function(signal, ds) s[startsWith(s$signal, signal) & s$dataset == ds, ]
  # RFENDTC - RFSTDTC: 11 subjects at 90 days, one at 30.
  expect_identical(row("duration RFSTDTC to RFENDTC (bands of 7 days)", "dm")$n_unique, 1L)
  expect_identical(row("duration RFSTDTC to RFENDTC (days)", "dm")$n_unique, 1L)
  expect_identical(row("rows per subject", "vs")$n_unique, 1L)
  expect_identical(row("set of VSTESTCD", "vs")$n_below_k, 1L)
  expect_gte(row("set of VSTESTCD", "vs")$n_below_k_with_quasi, 1L)
  expect_identical(r$settings$deidentify$signals, s)
  expect_true(any(grepl("residual signals single out subjects", r$log$detail)))
  expect_true(any(grepl("not anonymous against someone who knows them", capture.output(print(r)))))
})

test_that("a quasi-identifier with one value is measured but not generalized or suppressed", {
  data <- deid_test_data()
  data$dm$COUNTRY <- "USA"
  r <- deid_quiet(anon_deidentify(data[c("dm", "vs")], anon_profile_sdtm(3), max_risk = 1/3, seed = 2))
  q <- r$settings$deidentify$quasi
  expect_true(q$constant[q$quasi == "COUNTRY"])
  expect_identical(q$level[q$quasi == "COUNTRY"], 0L)
  expect_true(all(r$data$dm$COUNTRY == "USA"))
  expect_true("COUNTRY" %in% r$risk_after$quasi)
  expect_false("COUNTRY" %in% r$settings$suppress$order)
})

test_that("the target isn't met without quasi-identifiers or when the measured risk misses it", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:4), X = 1:4)
  r <- anon_deidentify(list(dm = dm), deid_test_profile(), seed = 1)
  expect_false(r$settings$deidentify$meets_target)
  expect_identical(r$settings$deidentify$not_met, "no quasi-identifiers found in the data")
  # Subjects kept although they can't reach the target.
  dm$AGE <- c(20, 30, 40, 50)
  expect_warning(
    r2 <- anon_deidentify(list(dm = dm), deid_test_profile(anon_profile_rule("AGE", "quasi")), unresolved = "keep", seed = 1),
    "remain in groups smaller than 12"
  )
  expect_false(r2$settings$deidentify$meets_target)
  expect_true("measured risk above the target" %in% r2$settings$deidentify$not_met)
  # Dropped instead: no subject left.
  expect_warning(
    r3 <- anon_deidentify(list(dm = dm), deid_test_profile(anon_profile_rule("AGE", "quasi")), seed = 1),
    "removed from every dataset"
  )
  expect_identical(nrow(r3$data$dm), 0L)
  expect_identical(r3$settings$deidentify$n_dropped, 4L)
})

test_that("a birth date left next to AGE is a reason the target isn't met", {
  data <- deid_test_data()["dm"]
  expect_warning(
    profile <- c(anon_profile_sdtm(3), anon_profile(anon_profile_rule("BRTHDTC", "keep"), name = "keep birth")),
    "remove \\(sdtm\\) replaced by keep"
  )
  w <- capture_warnings(r <- anon_deidentify(data, profile, max_risk = 1/3, seed = 2))
  expect_true(all(grepl("^Birth dates more precise than a year are next to [^:]*: dm.BRTHDTC\\.", w)))
  expect_false(r$settings$deidentify$meets_target)
  expect_true("birth dates more precise than a year next to AGE" %in% r$settings$deidentify$not_met)
})

test_that("a single data frame comes back as a data frame", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:6), AGE = c(30, 31, 32, 40, 41, 42), RFSTDTC = "2020-01-01")
  r <- anon_deidentify(dm, deid_test_profile(anon_profile_rule("AGE", "quasi", ladder = list(anon_band(10)))), max_risk = 1/3, seed = 1)
  expect_s3_class(r$data, "data.frame")
  expect_identical(sort(r$data$AGE), rep(c(30, 40), each = 3))
  expect_identical(r$settings$deidentify$quasi$source, "data.AGE")
})

test_that("arguments are checked", {
  dm <- data.frame(USUBJID = "a", AGE = 1)
  expect_error(anon_deidentify(dm, profile = list()), "must be an `anon_profile\\(\\)`")
  expect_error(anon_deidentify(dm, key = "ID"), "The profile's key is `USUBJID`, not `ID`")
  expect_error(anon_deidentify(dm, max_suppressed = 2), "`max_suppressed` must be")
  expect_error(anon_deidentify(dm, seed = "a"), "`seed` must be NULL")
})

test_that("ladder steps that don't apply to a quasi-identifier's values are left out, with a warning", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:6), SITEID = c(1, 1, 1, 2, 2, 3))
  profile <- deid_test_profile(anon_profile_rule("SITEID", "quasi", ladder = list(anon_fold(3, "OTHER"), "remove")))
  expect_warning(
    r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, max_suppressed = 0, seed = 1),
    "SITEID: anon_fold\\(min_n = 3, to = \"OTHER\"\\)"
  )
  expect_false(any(grepl("anon_fold", r$settings$deidentify$moves$step)))
  expect_true(r$settings$deidentify$meets_target)
})

test_that("a profile's date that can't be offset is named in a warning", {
  # A number of days, without a SAS date format.
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:3), VISITDT = c(18000, 18100, NA), RFSTDTC = "2020-01-01")
  profile <- deid_test_profile(anon_profile_rule("*DT", "date"))
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm), profile, seed = 1))
  expect_true(any(grepl("were not offset[^:]*: dm.VISITDT\\.", w)))
  expect_true("profile date columns were not offset" %in% r$settings$deidentify$not_met)
})

test_that("baseline values come from the flagged row, or the first row without one", {
  key <- c("a", "a", "b", "b", "c")
  test <- c("W", "W", "W", "W", "H")
  flag <- c("", "Y", "", "", NA)
  expect_identical(baseline_rows(flag, key, test), c(FALSE, TRUE, TRUE, TRUE, TRUE))
})

test_that("durations are measured in bands of days from full dates", {
  dm <- data.frame(
    USUBJID = c("a", "b", "c", "d"),
    RFSTDTC = c("2014-01-02", "2014-02-10", "2014-03", "2014-03-01T08:00"),
    RFENDTC = c("2014-01-15", "2014-02-24", "2014-04-02", "2014-03-14")
  )
  spec <- anon_quasi_duration("data", "RFSTDTC", "RFENDTC", width = 7)
  expect_output(print(spec), "DURATION = days from RFSTDTC to RFENDTC, in bands of 7")
  k <- anon_risk_subjects(dm, quasi = spec)
  expect_identical(k$k, c(2L, 1L, 1L, 2L))
  table <- risk_quasi_table(dm, spec, "USUBJID")
  # 13, 14 and 13 days in bands of 7; a partial date gives no duration.
  expect_identical(table$DURATION, c(7, 14, NA, 7))
  dates <- data.frame(USUBJID = c("a", "b"), S = as.Date(c("2020-01-01", "2020-01-01")), E = as.POSIXct(c("2020-01-03 10:00", "2020-01-31 10:00"), tz = "UTC"))
  expect_identical(risk_quasi_table(dates, anon_quasi_duration("data", "S", "E", width = 1), "USUBJID")$DURATION, c(2, 30))
  expect_error(anon_suppress(dm, quasi = spec), "can be measured but not suppressed")
  expect_error(anon_quasi_duration("dm", "A", "B", width = 0), "greater than 0")
})

test_that("pharmaverse SDTM and ADaM data de-identified together meet the target with consistent copies", {
  skip_if_not_installed("pharmaversesdtm")
  skip_if_not_installed("pharmaverseadam")
  keep <- pharmaversesdtm::dm$USUBJID[1:80]
  pick <- function(df) df[df$USUBJID %in% keep, , drop = FALSE]
  data <- list(
    dm = pick(pharmaversesdtm::dm), vs = pick(pharmaversesdtm::vs),
    adsl = pick(pharmaverseadam::adsl), advs = pick(pharmaverseadam::advs)
  )
  r <- deid_quiet(anon_deidentify(data, c(anon_profile_sdtm(), anon_profile_adam()), seed = 3))
  d <- r$data
  expect_true(r$settings$deidentify$meets_target)
  expect_true(all(d$dm$BRTHDTC == ""))
  by_key <- function(df, col) df[[col]][match(d$dm$USUBJID, df$USUBJID)]
  for (col in c("AGE", "SEX", "RACE", "ETHNIC", "SITEID")) {
    expect_identical(as.character(by_key(d$adsl, col)), as.character(d$dm[[col]]), info = col)
    expect_identical(as.character(by_key(d$advs, col)), as.character(d$dm[[col]]), info = col)
  }
  # Weight is generalized alike in vs and advs, and blanked for the same subjects.
  vs_w <- d$vs[d$vs$VSTESTCD == "WEIGHT" & d$vs$VSBLFL %in% "Y", ]
  advs_w <- d$advs[d$advs$PARAMCD == "WEIGHT" & d$advs$VSBLFL %in% "Y" & is.na(d$advs$DTYPE), ]
  expect_identical(by_key(advs_w, "VSSTRESN"), by_key(vs_w, "VSSTRESN"))
  # adsl: AGEGR1 is shown only where the released AGE band lies within its
  # category, and is then the original one.
  shown <- d$adsl$AGEGR1 != ""
  band <- d$adsl$AGE[shown]
  expect_true(all((d$adsl$AGEGR1[shown] == "18-64" & band + 19 <= 64) | (d$adsl$AGEGR1[shown] == ">64" & band >= 65)))
  # RACEGR1 follows the released RACE where shown.
  shown_race <- d$adsl$RACEGR1 != "" & !is.na(d$adsl$RACEGR1)
  expect_true(all(ifelse(d$adsl$RACE[shown_race] == "WHITE", "White", "Non-white") == d$adsl$RACEGR1[shown_race]))
  expect_identical(types <- lapply(d, function(df) vapply(df, typeof, "")), lapply(data, function(df) vapply(df, typeof, "")))
})

test_that("a copy that shows some subject another value is measured and suppressed on its own", {
  keys <- sprintf("S%02d", 1:24)
  age <- rep(c(30, 40, 50, 60), each = 6)
  sex <- rep(c("F", "F", "F", "M", "M", "M"), 4)
  w <- rep(c(60, 60, 60, 70, 70, 70), 4)
  dm <- data.frame(USUBJID = keys, AGE = age, SEX = sex)
  vs <- data.frame(USUBJID = keys, VSTESTCD = "WEIGHT", VSSTRESN = w, VSBLFL = "Y")
  # Baseline weight from another visit in adsl: a value of its own per subject.
  adsl <- data.frame(USUBJID = keys, AGE = age, SEX = sex, WEIGHTBL = w + seq_len(24) * 0.37)
  r <- deid_quiet(anon_deidentify(list(dm = dm, vs = vs, adsl = adsl), c(anon_profile_sdtm(3), anon_profile_adam(3)), max_risk = 1/3, seed = 1))
  q <- r$settings$deidentify$quasi
  expect_identical(q$measured[q$quasi == "WEIGHT"], "adsl.WEIGHTBL")
  expect_true("WEIGHTBL" %in% r$risk_after$quasi)
  expect_true(r$settings$deidentify$meets_target)
  # The released adsl copy is k-anonymous with the other quasi-identifiers.
  k <- anon_risk(r$data, quasi = list(adsl = c("AGE", "SEX", "WEIGHTBL")), max_risk = 1/3)
  expect_gte(k$summary$min_k, 3)
  # A copy equal to its source for every subject is not measured.
  adsl$WEIGHTBL <- w
  r2 <- deid_quiet(anon_deidentify(list(dm = dm, vs = vs, adsl = adsl), c(anon_profile_sdtm(3), anon_profile_adam(3)), max_risk = 1/3, seed = 1))
  q2 <- r2$settings$deidentify$quasi
  expect_identical(q2$measured[q2$quasi == "WEIGHT"], "")
})

test_that("subjects missing from the source are measured from the copies that hold them", {
  n <- 40
  dm <- data.frame(USUBJID = sprintf("Q%02d", 1:n), SITEID = rep(c("101", "102", "103", "104"), each = 10), SEX = rep(c("F", "M"), 20))
  # adsl lacks half of site 104, and comes first.
  adsl <- dm[!(dm$SITEID == "104" & seq_len(n) %% 2 == 0), ]
  profile <- deid_test_profile(anon_profile_rule("SITEID", "quasi"), anon_profile_rule("SEX", "quasi"))
  r <- anon_deidentify(list(adsl = adsl, dm = dm), profile, max_risk = 1/5, seed = 1)
  # The source is the copy that holds the most subjects.
  expect_identical(r$settings$deidentify$quasi$source[1], "dm.SITEID")
  b <- function(v) ifelse(is.na(v) | v == "", "<b>", v)
  pattern <- paste(b(r$data$dm$SITEID), b(r$data$dm$SEX))
  expect_gte(min(table(pattern)), 5)
  # A source blank for everyone, with a copy that isn't, is not constant.
  dm2 <- dm
  dm2$SITEID <- ""
  adsl2 <- dm
  w <- capture_warnings(r2 <- anon_deidentify(list(dm = dm2, adsl = adsl2), profile, max_risk = 1/5, seed = 1))
  q2 <- r2$settings$deidentify$quasi
  expect_false(q2$constant[q2$quasi == "SITEID"])
  expect_identical(q2$source[q2$quasi == "SITEID"], "adsl.SITEID")
  # The same-name copies differ in the data itself: PR 3's check says so.
  expect_true(any(grepl("`SITEID` in these datasets differs", w)))
  expect_false(r2$settings$deidentify$meets_target)
})

test_that("a copy with several values per subject is generalized but reported as not measured", {
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:6), AGE = c(30, 31, 32, 40, 41, 42))
  adae <- data.frame(USUBJID = rep(adsl$USUBJID, each = 2), AAGE = rep(adsl$AGE, each = 2) + c(0, 1))
  w <- capture_warnings(r <- anon_deidentify(list(adsl = adsl, adae = adae), anon_profile_adam(3), max_risk = 1/3, seed = 1))
  expect_true(any(grepl("hold more than one value for a subject and can't be measured: adae.AAGE", w, fixed = TRUE)))
  expect_false(r$settings$deidentify$meets_target)
  expect_true("copies of quasi-identifiers that vary within a subject aren't measured" %in% r$settings$deidentify$not_met)
})

test_that("derived values are mapped for every row of a generalized quasi-identifier, bounds included", {
  # Ages exactly on a band's lower bound keep their value under the band, but
  # their group is decided by the band: 60-79 straddles the cut at 65.
  n <- 24
  age <- c(rep(60, 4), rep(c(61, 63, 66, 70, 72, 75, 77, 79), 2), 62, 64, 68, 74)
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = age, AGEGR1 = ifelse(age < 65, "<65", ">=65"))
  profile <- deid_test_profile(
    anon_profile_rule("AGE", "quasi", ladder = list(anon_band(20))),
    anon_profile_rule("AGEGR1", "derived", from = "AGE")
  )
  r <- anon_deidentify(list(adsl = adsl), profile, max_risk = 1/3, max_suppressed = 0, seed = 1)
  expect_identical(r$settings$deidentify$quasi$step, "anon_band(width = 20)")
  expect_true(all(r$data$adsl$AGEGR1 == ""))
})

test_that("a quasi-identifier at level 0 still blanks its derived variables where it was suppressed", {
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:7), SEX = c(rep("F", 3), rep("M", 3), "U"), SEXN = c(rep(2, 3), rep(1, 3), 3))
  profile <- deid_test_profile(anon_profile_rule("SEX", "quasi"), anon_profile_rule("SEXN", "derived", from = "SEX"))
  r <- suppressWarnings(anon_deidentify(list(adsl = adsl), profile, max_risk = 1/3, seed = 1, unresolved = "keep"))
  shown <- r$data$adsl$SEX != ""
  expect_identical(is.na(r$data$adsl$SEXN), !shown)
})

test_that("the ADaM profile covers AAGE, EDUCLVL, HGTBL and WGTBL, and derived groups of them", {
  n <- 24
  age <- rep(c(31, 36, 42, 47, 52, 58), 4)
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:n), AGE = age, AAGE = age, AAGEGR1 = ifelse(age < 45, "<45", ">=45"),
    EDUCLVL = rep(c("HS", "COLLEGE"), 12), SITEID = rep(c("01", "02"), each = 12), SITEGR1 = rep(c("A", "B"), each = 12),
    HGTBL = 160 + seq_len(n) * 0.7, WGTBL = 60 + seq_len(n) * 1.1
  )
  adsl$BMIBLGR1 <- ifelse(adsl$WGTBL / (adsl$HGTBL / 100)^2 < 25, "<25", ">=25")
  r <- deid_quiet(anon_deidentify(list(adsl = adsl), anon_profile_adam(3), max_risk = 1/3, seed = 1))
  q <- r$settings$deidentify$quasi
  expect_true(all(c("AGE", "EDUCLVL", "HEIGHT", "WEIGHT") %in% q$quasi))
  expect_match(q$copies[q$quasi == "AGE"], "adsl.AAGE")
  expect_identical(q$source[q$quasi == "HEIGHT"], "adsl.HGTBL")
  # AAGE is generalized with AGE (a copy equal to it).
  expect_identical(r$data$adsl$AAGE, r$data$adsl$AGE)
  # BMIBLGR1 is blanked wherever height or weight was generalized.
  expect_true(all(r$data$adsl$BMIBLGR1 == ""))
  derived <- r$log$variable[grepl("^derived from", r$log$detail)]
  expect_true(all(c("AAGEGR1", "BMIBLGR1") %in% derived))
})

test_that("columns that look like quasi-identifiers without a rule, and keep rules over quasi rules, are named", {
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:6), AGE = 30, BIRTHYR = 1990:1995, SITENO = 1:2, WT = 70,
    PROJIDN = 7, CRCLBLU = c("mL/min", "mL/min/1.73m2"), COUNTRYL = rep(c("Canada", "Iceland"), 3), RACE2 = c("ASIAN", rep("", 5))
  )
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi"), anon_profile_rule("WT", "quasi"), anon_profile_rule("WT", "keep", datasets = "dm"))
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, seed = 1))
  # A unit (*U), and a column with one value in every row, aren't named.
  expect_true(any(grepl(
    "look like quasi-identifiers or birth dates but no profile rule matches them: dm.BIRTHYR, dm.SITENO, dm.COUNTRYL, dm.RACE2\\.", w
  )))
  expect_true(any(grepl("kept by a `keep` rule although a quasi-identifier rule matches them too: dm.WT\\.", w)))
  # They are released as they are: the target isn't met.
  expect_false(r$settings$deidentify$meets_target)
  expect_true("columns that look like quasi-identifiers have no profile rule" %in% r$settings$deidentify$not_met)
})

test_that("ADaM companions of the IDs get the pseudonym's number, and those of quasi-identifiers follow them", {
  n <- 24
  site <- c(rep("701", 12), rep("702", 11), "703")
  country <- c(rep("USA", 23), "ISL")
  adsl <- data.frame(
    USUBJID = sprintf("01-%03d", 1:n), USUBJIDN = 1000 + 1:n, SUBJID = sprintf("%03d", 1:n), SUBJIDN = 1:n,
    AGE = rep(c(30, 50), 12), SITEID = site, SITEIDN = as.numeric(site), COUNTRY = country,
    COUNTRYL = c(USA = "United States", ISL = "Iceland")[country], COUNTRYN = match(country, c("USA", "ISL")),
    stringsAsFactors = FALSE
  )
  adpk <- data.frame(USUBJID = rep(adsl$USUBJID, each = 2), USUBJIDN = rep(adsl$USUBJIDN, each = 2), SITEIDN = rep(adsl$SITEIDN, each = 2))
  w <- capture_warnings(r <- anon_deidentify(list(adsl = adsl, adpk = adpk), anon_profile_adam(3), max_risk = 1/3, seed = 1, unresolved = "keep"))
  expect_false(any(grepl("look like quasi-identifiers", w)))
  a <- r$data$adsl
  # The number of the subject's pseudonym, numeric, the same in every dataset.
  expect_type(a$USUBJIDN, "double")
  expect_identical(a$USUBJIDN, as.numeric(sub("^SUBJ-", "", a$USUBJID)))
  expect_identical(as.numeric(a$SUBJIDN), a$USUBJIDN)
  p <- r$data$adpk
  expect_identical(p$USUBJIDN, as.numeric(sub("^SUBJ-", "", p$USUBJID)))
  # SITEIDN shows the code of the released SITEID, and is blank where it is;
  # COUNTRYL and COUNTRYN likewise for COUNTRY.
  expect_true(all(is.na(a$SITEIDN) == (a$SITEID == "")))
  shown <- a$SITEID != "" & a$SITEID != "OTHER"
  expect_identical(a$SITEIDN[shown], as.numeric(a$SITEID[shown]))
  expect_true(all(is.na(a$SITEIDN[a$SITEID == "OTHER"])))
  expect_true(all(is.na(p$SITEIDN) | p$SITEIDN %in% a$SITEIDN))
  expect_true(all((a$COUNTRYL == "") == (a$COUNTRY == "")))
  expect_false(any(a$COUNTRYL == "Iceland"))
  expect_true(all(is.na(a$COUNTRYN) == (a$COUNTRY == "")))
  expect_true(all(c("SITEIDN", "COUNTRYL", "COUNTRYN") %in% r$log$variable[grepl("^derived from", r$log$detail)]))
  # A rule that matches a companion wins.
  r2 <- deid_quiet(anon_deidentify(
    list(adsl = adsl), c(anon_profile_adam(3), anon_profile(anon_profile_rule("COUNTRYL", "keep"))), max_risk = 1/3, seed = 1
  ))
  expect_identical(r2$data$adsl$COUNTRYL, adsl$COUNTRYL)
})

test_that("baseline body surface area and renal function are derived from height and weight, units kept", {
  n <- 12
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 6), SEX = rep(c("F", "M"), each = 6),
    WEIGHTBL = 60 + 1:n * 1.3, HEIGHTBL = 160 + 1:n, BSABL = 1.6 + 1:n / 100, BSABLU = "m2",
    CRCLBL = 80 + 1:n, EGFRBL = 70 + 1:n, CRCLBLU = "mL/min"
  )
  w <- capture_warnings(r <- anon_deidentify(list(adsl = adsl), anon_profile_adam(3), max_risk = 1/3, seed = 1))
  expect_false(any(grepl("look like quasi-identifiers", w)))
  a <- r$data$adsl
  # Height and weight were generalized: the derived values are blanked.
  expect_true(all(is.na(a$BSABL) & is.na(a$CRCLBL) & is.na(a$EGFRBL)))
  expect_identical(a$BSABLU, adsl$BSABLU)
  expect_identical(a$CRCLBLU, adsl$CRCLBLU)
})

test_that("nested copies of a quasi-identifier are generalized, suppressed and measured with it", {
  n <- 12
  age <- c(31, 32, 33, 34, 41, 42, 43, 44, 51, 52, 53, 79)
  dm <- tibble::tibble(USUBJID = sprintf("S%02d", 1:n), AGE = age, RFSTDTC = "2021-01-01")
  dm$packed <- data.frame(AGE = age)
  dm$lst <- lapply(seq_len(n), function(i) data.frame(USUBJID = dm$USUBJID[i], AGE = age[i]))
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi", ladder = list(anon_band(10))))
  r <- deid_quiet(anon_deidentify(list(dm = dm), profile, max_risk = 1/3, seed = 1, unresolved = "keep"))
  d <- r$data$dm
  expect_identical(r$settings$deidentify$quasi$step[1], "anon_band(width = 10)")
  expect_true(any(is.na(d$AGE)))
  expect_identical(d$packed$AGE, d$AGE)
  expect_identical(vapply(d$lst, function(x) x$AGE, numeric(1)), d$AGE)
  expect_match(r$settings$deidentify$quasi$copies[1], "dm.packed.AGE, dm.lst[[]].AGE", fixed = TRUE)
  expect_false("dm.packed" %in% names(r$data))
  # A nested copy that shows a subject another value is measured on its own.
  dm2 <- dm
  dm2$packed$AGE[1] <- 79
  r2 <- deid_quiet(anon_deidentify(list(dm = dm2), profile, max_risk = 1/3, seed = 1, unresolved = "keep"))
  expect_identical(r2$settings$deidentify$quasi$measured[1], "dm.packed.AGE")
  d2 <- r2$data$dm
  released <- paste(d2$AGE, d2$packed$AGE)
  expect_true(all(table(released) >= 3))
  # Rows that no subject owns (no key) are blanked in the nested copies.
  dm3 <- dm
  dm3$USUBJID[12] <- NA
  dm3$lst[[12]]$USUBJID <- NA_character_
  r3 <- deid_quiet(anon_deidentify(list(dm = dm3), profile, max_risk = 1/3, seed = 1, unresolved = "keep"))
  d3 <- r3$data$dm
  expect_true(is.na(d3$packed$AGE[is.na(d3$USUBJID)]))
  expect_true(any(grepl("no subject owns", r3$log$detail)))
  # Data frames whose own key is missing in every row: no subject owns them.
  dm5 <- dm
  dm5$lst <- lapply(age, function(a) data.frame(USUBJID = NA_character_, AGE = a))
  r5 <- deid_quiet(anon_deidentify(list(dm = dm5), profile, max_risk = 1/3, seed = 1, unresolved = "keep"))
  expect_true(all(is.na(vapply(r5$data$dm$lst, function(x) x$AGE, numeric(1)))))
  # A numeric key.
  dm4 <- dm[c("USUBJID", "AGE", "packed")]
  dm4$USUBJID <- 100 + seq_len(n)
  r4 <- deid_quiet(anon_deidentify(list(dm = dm4), profile, max_risk = 1/3, seed = 1, unresolved = "keep"))
  expect_identical(r4$data$dm$packed$AGE, r4$data$dm$AGE)
})

test_that("profile text dates lose what isn't an ISO 8601 date, as *DTC columns do", {
  n <- 12
  v <- c("2021-02-01", "10MAR2021", "2021", "--03-10", "2021-03-10 visit", "03/10/2021", rep("2021-02-01", 6))
  dm <- data.frame(USUBJID = sprintf("P%02d", 1:n), AGE = rep(c(30, 50), 6), VISIT_DATE = v, CMPDTC = v, ROWNO = 1:n)
  p <- c(anon_profile_sdtm(), anon_profile(anon_profile_rule("VISIT_DATE", "date")))
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm), p, max_risk = 1/6, seed = 1, unresolved = "keep"))
  o <- r$data$dm
  o <- o[order(o$ROWNO), ]
  blank <- function(x) ifelse(is.na(x), "", x)
  expect_identical(blank(o$VISIT_DATE), blank(o$CMPDTC))
  expect_true(all(blank(o$VISIT_DATE)[!v %in% c("2021-02-01", "2021")] == ""))
  expect_false(any(o$VISIT_DATE %in% v))
  row <- r$log[r$log$step == "subject_dates" & r$log$variable %in% "VISIT_DATE", ]
  expect_identical(row$n_removed, 4L)
  expect_true(any(grepl("date columns in the profile are not recognised ISO 8601 dates", w)))
})

test_that("a profile date column doesn't drop secrets kept by earlier steps", {
  reg <- data.frame(ID = sprintf("P%02d", 1:6), VISIT_DATE = format(as.Date("2021-03-01") + 1:6), AGE = 30)
  p <- anon_profile(anon_profile_rule("ID", "key"), anon_profile_rule("VISIT_DATE", "date"), anon_profile_rule("AGE", "quasi"))
  ids <- anon_subject_ids(list(reg = reg), key = "ID", also = character(0), keep_secrets = TRUE, seed = 1)
  r <- deid_quiet(anon_deidentify(ids, p, key = "ID", max_risk = 1/3, seed = 2))
  expect_identical(names(anon_secrets(r)), "subject_ids")
})

test_that("derived values missing in the data stay missing", {
  n <- 12
  age <- rep(c(30, 39, 50, 59), 3)
  race <- c(rep("WHITE", 9), "ASIAN", "ASIAN", "BLACK OR AFRICAN AMERICAN")
  adsl <- data.frame(
    USUBJID = sprintf("S%02d", 1:n), AGE = age, AGEGR1 = ifelse(age < 45, "<45", ">=45"),
    RACE = race, RACEN = match(race, c("WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN")), ROWNO = 1:n, stringsAsFactors = FALSE
  )
  adsl$AGEGR1[1] <- ""
  adsl$RACEN[2] <- NA
  profile <- deid_test_profile(
    anon_profile_rule("AGE", "quasi", ladder = list(anon_band(10))), anon_profile_rule("AGEGR1", "derived", from = "AGE"),
    anon_profile_rule("RACE", "quasi", ladder = list(anon_fold(3, "OTHER"))), anon_profile_rule("RACEN", "derived", from = "RACE")
  )
  r <- anon_deidentify(list(adsl = adsl), profile, max_risk = 1/3, max_suppressed = 1, seed = 1)
  a <- r$data$adsl
  a <- a[order(a$ROWNO), ]
  expect_identical(r$settings$deidentify$quasi$level, c(1L, 1L))
  # AGEGR1 follows each band where AGE is shown, but stays blank where it was.
  shown <- !is.na(a$AGE)
  expect_true(shown[1])
  expect_identical(a$AGEGR1[shown], adsl$AGEGR1[shown])
  expect_identical(a$AGEGR1[1], "")
  # RACEN keeps WHITE's code, except where it was missing; OTHER has none.
  expect_identical(a$RACEN, c(1L, NA, rep(1L, 7), NA, NA, NA))
})

test_that("dataset scopes match whatever the case of the dataset names", {
  n <- 24
  ids <- sprintf("P%02d", 1:n)
  DM <- data.frame(USUBJID = ids, AGE = rep(c(30, 50), 12), RACE = c(rep("WHITE", 22), "MULTIPLE", "MULTIPLE"))
  SUPPDM <- data.frame(USUBJID = rep(ids[23:24], each = 2), QNAM = rep(c("RACE1", "RACE2"), 2), QVAL = c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN", "WHITE"))
  r <- deid_quiet(anon_deidentify(list(DM = DM, SUPPDM = SUPPDM), anon_profile_sdtm(2), max_risk = 1/4, seed = 1))
  q <- r$settings$deidentify$quasi
  expect_match(q$copies[q$quasi == "RACE"], "SUPPDM.QVAL (QNAM = RACE1)", fixed = TRUE)
  expect_false(any(r$data$SUPPDM$QVAL %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN")))
  # A qualifier that looks like a quasi-identifier without a rule: the target isn't met.
  SUPPDM2 <- rbind(SUPPDM, data.frame(USUBJID = ids[1], QNAM = "HEIGHTCM", QVAL = "171"))
  r2 <- deid_quiet(anon_deidentify(list(DM = DM, SUPPDM = SUPPDM2), anon_profile_sdtm(2), max_risk = 1/4, seed = 1))
  expect_true("supplemental qualifiers that look like quasi-identifiers have no profile rule" %in% r2$settings$deidentify$not_met)
})

test_that("a derived variable whose quasi-identifiers aren't in the data is blanked, with a warning", {
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:6), AGE = rep(c(30, 50), 3), BMIBL = 20:25 + 0.5)
  w <- capture_warnings(r <- anon_deidentify(list(adsl = adsl), anon_profile_adam(3), max_risk = 1/3, seed = 1))
  expect_true(all(is.na(r$data$adsl$BMIBL)))
  expect_true(any(grepl("derived variables come from quasi-identifiers that aren't measured[^:]*: adsl.BMIBL\\.", w)))
  expect_true(any(r$log$variable %in% "BMIBL" & grepl("not measured: blanked in every row", r$log$detail)))
})

test_that("a quasi-identifier whose only copy varies within a subject is reported, not an error", {
  n <- 12
  adae <- data.frame(USUBJID = rep(sprintf("S%02d", 1:n), each = 2), AGE = rep(20 + 1:n, each = 2) + c(0, 1), SEX = rep(c("F", "M"), n))
  w <- capture_warnings(r <- anon_deidentify(list(adae = adae), anon_profile_adam(3), max_risk = 1/3, seed = 1))
  expect_true(any(grepl("no copy with one value per subject and can't be measured: AGE (adae.AGE)", w, fixed = TRUE)))
  expect_false(r$settings$deidentify$meets_target)
  expect_false("AGE" %in% r$settings$deidentify$quasi$quasi)
})

test_that("a removed factor keeps no levels, and a second run has settings of its own", {
  data <- deid_test_data()
  data$dm$INVNAM <- factor(data$dm$INVNAM, levels = c("Dr Who", "Dr Unused"))
  r <- deid_quiet(anon_deidentify(data[c("dm", "vs")], anon_profile_sdtm(3), max_risk = 1/3, seed = 1))
  expect_identical(levels(r$data$dm$INVNAM), character(0))
  expect_true(all(is.na(r$data$dm$INVNAM)))
  r2 <- deid_quiet(anon_deidentify(r, anon_profile_sdtm(3), max_risk = 1/3, seed = 2))
  expect_true(all(c("deidentify", "deidentify_1") %in% names(r2$settings)))
  expect_identical(r2$settings$deidentify, r$settings$deidentify)
})

test_that("a derived variable follows the copy named as its quasi-identifier", {
  n <- 12
  age <- c(41, 42, 43, 44, 46, 47, 48, 31, 32, 33, 34, 35)
  # AAGE's bands (20-29 for ages 41-44) lie within one AGEGR1 category; AGE's
  # (40-49) don't.
  aage <- c(25, 26, 27, 28, 55, 56, 57, 20, 21, 22, 23, 24)
  adae <- data.frame(USUBJID = sprintf("S%02d", 1:n), AAGE = aage, AGE = age, AGEGR1 = ifelse(age < 45, "<45", ">=45"))
  # AAGE's rule first: its copy comes first.
  profile <- deid_test_profile(
    anon_profile_rule("AAGE", "quasi", quasi_name = "AGE", ladder = list(anon_band(10))),
    anon_profile_rule("AGE", "quasi", ladder = list(anon_band(10))),
    anon_profile_rule("AGEGR1", "derived", from = "AGE")
  )
  r <- anon_deidentify(list(adae = adae), profile, max_risk = 1/3, seed = 1)
  a <- r$data$adae
  expect_identical(r$settings$deidentify$quasi$level, 1L)
  expect_false(anyNA(a$AGE))
  # 40-49 straddles the cut at 45: blanked, whatever AAGE's band.
  expect_true(all(a$AGEGR1[a$AGE == 40] == ""))
  expect_true(all(a$AGEGR1[a$AGE == 30] == "<45"))
})

test_that("the profile's text date columns are offset with the subject's offset", {
  reg <- data.frame(ID = sprintf("P%02d", 1:6), VISIT_DATE = format(as.Date("2021-03-01") + 1:6), V2DTC = format(as.Date("2021-03-01") + 1:6), AGE = 30)
  p <- anon_profile(anon_profile_rule("ID", "key"), anon_profile_rule("VISIT_DATE", "date"), anon_profile_rule("AGE", "quasi"))
  w <- capture_warnings(r <- anon_deidentify(list(reg = reg), p, key = "ID", max_risk = 1/3, seed = 1, keep_secrets = TRUE))
  expect_false(any(grepl("VISIT_DATE", w)))
  # Moved like the *DTC column of the same row, which the dates step shifts.
  expect_identical(r$data$reg$VISIT_DATE, r$data$reg$V2DTC)
  expect_false(any(r$data$reg$VISIT_DATE %in% reg$VISIT_DATE[1]) && identical(sort(r$data$reg$VISIT_DATE), sort(reg$VISIT_DATE)))
  expect_identical(r$settings$subject_dates$profile_dates, "reg.VISIT_DATE")
  expect_true(any(r$log$step == "subject_dates" & r$log$variable %in% "VISIT_DATE"))
  # Without keep_secrets, the offsets read for it aren't kept.
  r2 <- suppressWarnings(anon_deidentify(list(reg = reg), p, key = "ID", max_risk = 1/3, seed = 1))
  expect_null(r2$secrets)
  expect_identical(r2$data$reg$VISIT_DATE, r$data$reg$VISIT_DATE)
})

test_that("direct identifiers are removed at every depth, with their value labels; names not found are logged", {
  dm <- tibble::tibble(
    USUBJID = sprintf("N%02d", 1:6), AGE = rep(c(30, 50), 3), RFSTDTC = "2020-01-01",
    INVNAM = haven::labelled(rep("Dr Quux", 6), c("Dr Quux" = "Dr Quux")),
    extra = lapply(sprintf("N%02d", 1:6), function(i) data.frame(USUBJID = i, INVNAM = "Dr Quux", BRTHDTC = "1950-03-04"))
  )
  dm$PACKED <- tibble::tibble(BRTHDTC = rep("1950-03-04", 6))
  r <- deid_quiet(anon_deidentify(list(dm = dm), max_risk = 1/3, seed = 1))
  d <- r$data$dm
  expect_true(all(d$INVNAM == ""))
  expect_null(attr(d$INVNAM, "labels"))
  expect_true(all(vapply(d$extra, function(e) all(e$INVNAM == "" & e$BRTHDTC == ""), logical(1))))
  expect_true(all(d$PACKED$BRTHDTC == ""))
  expect_length(grepRaw("Quux", serialize(r$data, NULL), all = TRUE), 0)
  rm <- r$log[r$log$step == "remove", ]
  expect_true(all(c("extra[[]][INVNAM]", "extra[[]][BRTHDTC]", "PACKED[BRTHDTC]") %in% rm$variable))
  expect_true(any(rm$variable %in% "INVID" & rm$detail == "direct identifier in the profile, not found in the data"))
  expect_identical(r$settings$remove$not_found, "INVID")
})

test_that("value labels of a generalized quasi-identifier keep only values still released", {
  dm <- tibble::tibble(USUBJID = sprintf("S%02d", 1:6), AGE = haven::labelled(c(40, 41, 42, 45, 46, 47), c("Forty five" = 45, "Fifty" = 50)))
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi", ladder = list(anon_band(10))))
  r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, max_suppressed = 0, seed = 1)
  expect_length(attr(r$data$dm$AGE, "labels"), 0)
})

test_that("race qualifiers in SUPPDM are copies of RACE, and other quasi-identifier qualifiers are named", {
  n <- 24
  race <- c(rep("WHITE", 18), rep("ASIAN", 4), "MULTIPLE", "MULTIPLE")
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:n), RACE = race, SEX = rep(c("F", "M"), 12))
  suppdm <- data.frame(
    USUBJID = dm$USUBJID[23:24], RDOMAIN = "DM", QNAM = c("RACE1", "RACE1"),
    QVAL = c("AMERICAN INDIAN OR ALASKA NATIVE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")
  )
  suppdm <- rbind(suppdm, data.frame(USUBJID = dm$USUBJID[1], RDOMAIN = "DM", QNAM = "HEIGHTCM", QVAL = "171"))
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm, suppdm = suppdm), anon_profile_sdtm(5), max_risk = 1/5, seed = 1))
  q <- r$settings$deidentify$quasi
  expect_match(q$copies[q$quasi == "RACE"], "suppdm.QVAL (QNAM = RACE1)", fixed = TRUE)
  # The qualifiers hold one subject each: folded or blanked, never released as they were.
  released <- r$data$suppdm$QVAL[r$data$suppdm$QNAM == "RACE1"]
  expect_true(all(released %in% c("", "OTHER")))
  expect_true(any(grepl("supplemental qualifiers look like quasi-identifiers[^:]*: suppdm \\(HEIGHTCM\\)", w)))
})

test_that("residual signals include each generalized test's values across rows and the visits and study days", {
  data <- deid_test_data()
  data$vs$VSDY <- rep(c(-7, 1, 29), 12)
  data$vs$VSDY[3] <- 30
  r <- deid_quiet(anon_deidentify(data[c("dm", "vs")], anon_profile_sdtm(3), max_risk = 1/3, seed = 2))
  s <- r$signals
  expect_true(any(startsWith(s$signal, "set of WEIGHT values across rows (VSSTRESN where VSTESTCD = WEIGHT)")))
  dy <- s[s$signal == "set of VSDY", ]
  expect_identical(dy$n_unique, 1L)
})

test_that("a larger budget doesn't stop the search while a step still lowers the cells needed", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:12), AGE = rep(c(30, 40, 50, 60), each = 3), WT = c(61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93))
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi"), anon_profile_rule("WT", "quasi", ladder = list(anon_round(-1))))
  # Within the budget from the start (12 of 24 cells), yet rounding WT needs none.
  r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, max_suppressed = 0.6, seed = 1)
  expect_identical(r$settings$deidentify$quasi$level, c(0L, 1L))
  expect_false(any(is.na(r$data$dm$WT)))
})

test_that("columns of nested data that look like quasi-identifiers without a rule are named", {
  n <- 6
  dm <- tibble::tibble(USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 3))
  dm$extra <- data.frame(HEIGHTCM = 170 + 1:n)
  dm$WEIGHTS <- lapply(1:n, function(i) c(70, 71) + i)
  dm$visits <- lapply(1:n, function(i) data.frame(VISIT = 1:2, GENDER = "F"))
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi"))
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, seed = 1))
  # A list column of data frames is walked; one of numbers is named, and so
  # are the nested columns, except a value that every subject shares.
  expect_true(any(grepl("no profile rule matches them: dm.WEIGHTS, dm.extra\\[HEIGHTCM\\]\\.", w)))
  expect_false(r$settings$deidentify$meets_target)
  expect_true("columns that look like quasi-identifiers have no profile rule" %in% r$settings$deidentify$not_met)
})

test_that("a look-alike with one value is released only when enough subjects share it", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:6), AGE = rep(c(30, 50), 3), BIRTHYR = 1990)
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi"))
  # Six subjects share it, at least the target's 3.
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, seed = 1))
  expect_false(any(grepl("look like quasi-identifiers", w)))
  expect_true(r$settings$deidentify$meets_target)
  # In a dataset holding one subject, the same value singles it out.
  one <- data.frame(USUBJID = "S01", HEIGHTCM = 203.5, BIRTHYR = 1990, VISIT = c("SCREENING", "WEEK 2"))
  w2 <- capture_warnings(r2 <- anon_deidentify(list(dm = dm[1:2], one = one), profile, max_risk = 1/3, seed = 1))
  expect_true(any(grepl("no profile rule matches them: one.HEIGHTCM, one.BIRTHYR\\.", w2)))
  expect_false(r2$settings$deidentify$meets_target)
  # Fewer subjects than the target's group size share it.
  w3 <- capture_warnings(r3 <- anon_deidentify(list(dm = dm), profile, max_risk = 1/12, seed = 1, unresolved = "keep"))
  expect_true(any(grepl("no profile rule matches them: dm.BIRTHYR\\.", w3)))
})

test_that("nested profile text dates are offset with their subject's offset, and numbers aren't dates", {
  n <- 6
  adsl <- tibble::tibble(USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 3), TRTSDT = as.Date("2021-03-01") + 1:n)
  adsl$visits <- lapply(1:n, function(i) data.frame(AVISITN = 1:2, VISDT = format(as.Date("2021-03-01") + i + c(0, 30))))
  p <- c(deid_test_profile(anon_profile_rule("AGE", "quasi")), anon_profile(anon_profile_rule("*DT", "date")))
  w <- capture_warnings(r <- anon_deidentify(list(adsl = adsl), p, max_risk = 1/3, seed = 1))
  a <- r$data$adsl
  # Each nested date moved with the subject's TRTSDT.
  moved <- vapply(seq_len(n), function(i) as.numeric(as.Date(a$visits[[i]]$VISDT[1]) - a$TRTSDT[i]), numeric(1))
  expect_identical(moved, rep(0, n))
  expect_false(any(unlist(lapply(a$visits, `[[`, "VISDT")) %in% unlist(lapply(adsl$visits, `[[`, "VISDT"))))
  expect_false(any(grepl("not shifted|not offset", w)))
  expect_true(any(r$log$variable %in% "visits[[]][VISDT]" & r$log$step == "subject_dates"))
  expect_true(r$settings$deidentify$meets_target)
  # A number of days is left as it is, and the target isn't met.
  adsl2 <- adsl[c("USUBJID", "AGE")]
  adsl2$TRTSDT <- as.numeric(as.Date("2021-03-01") + 1:n)
  w2 <- capture_warnings(r2 <- anon_deidentify(list(adsl = adsl2), p, max_risk = 1/3, seed = 1))
  expect_true(any(grepl("were not offset[^:]*: adsl.TRTSDT\\.", w2)))
  expect_false(r2$settings$deidentify$meets_target)
  expect_true("profile date columns were not offset" %in% r2$settings$deidentify$not_met)
})

test_that("a dataset without the key is processed through an `also` column, or fails the target", {
  n <- 12
  adsl <- data.frame(
    USUBJID = sprintf("01-%03d", 1:n), SUBJID = sprintf("%03d", 1:n), AGE = c(31:36, 51:56), SEX = rep(c("F", "M"), 6)
  )
  ex <- data.frame(SUBJID = rev(adsl$SUBJID), AGE = rev(adsl$AGE), EXDOSE = 10)
  profile <- anon_profile_adam(3)
  w <- capture_warnings(r <- anon_deidentify(list(adsl = adsl, ex = ex), profile, max_risk = 1/3, seed = 1))
  e <- r$data$ex
  a <- r$data$adsl
  # The rows took their subjects' pseudonymized SUBJID and released AGE, and
  # the dataset no key column.
  expect_identical(names(e), names(ex))
  expect_false(any(e$SUBJID %in% ex$SUBJID))
  expect_identical(e$AGE, a$AGE[match(e$SUBJID, a$SUBJID)])
  expect_true(r$settings$deidentify$meets_target)
  expect_true(any(r$log$dataset %in% "ex" & grepl("tied to subjects through `SUBJID`", r$log$detail)))
  # A value that ties to no subject: left as it is, and the target isn't met.
  ex2 <- rbind(ex, data.frame(SUBJID = "999", AGE = 40, EXDOSE = 10))
  w2 <- capture_warnings(r2 <- anon_deidentify(list(adsl = adsl, ex = ex2), profile, max_risk = 1/3, seed = 1))
  expect_identical(r2$data$ex, ex2)
  expect_true(any(grepl("no `USUBJID` column[^:]*: ex \\(SUBJID, AGE\\)", w2)))
  expect_true("datasets without the key hold identifiers, quasi-identifiers or dates" %in% r2$settings$deidentify$not_met)
  # NONMEM-style data with a numeric ID companion the keyed datasets lack.
  nm <- data.frame(ID = 1:n, USUBJIDN = 1:n, WTBL = 60 + 1:n)
  w3 <- capture_warnings(r3 <- anon_deidentify(list(adsl = adsl, nm = nm), profile, max_risk = 1/3, seed = 1))
  expect_true(any(grepl("no `USUBJID` column[^:]*: nm \\(USUBJIDN, WTBL\\)", w3)))
  expect_false(r3$settings$deidentify$meets_target)
})

test_that("a rule limited to other datasets doesn't cancel a companion", {
  n <- 24
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 12), RACE = c(rep("WHITE", 22), "ASIAN", "ASIAN"))
  adsl$RACEL <- tolower(adsl$RACE)
  p <- c(anon_profile_adam(3), anon_profile(anon_profile_rule("RACEL", "keep", datasets = "adae")))
  r <- deid_quiet(anon_deidentify(list(adsl = adsl, adae = adsl), p, max_risk = 1/3, seed = 1))
  a <- r$data$adsl
  # In adsl, RACEL follows RACE: blanked where RACE was folded or blanked.
  expect_false(any(a$RACEL %in% "asian"))
  expect_true(all(a$RACEL[a$RACE == "WHITE"] == "white"))
  # In adae, the keep rule wins.
  expect_identical(sort(r$data$adae$RACEL), sort(adsl$RACEL))
})

test_that("variable names match rules ignoring case, and demographic variants are look-alikes", {
  n <- 24
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 12), RACE = c(rep("WHITE", 22), "ASIAN", "ASIAN"))
  pk <- data.frame(
    USUBJID = dm$USUBJID, race = dm$RACE, Sex = rep(c("F", "M"), 12), GENDER = "F", RACEOTH = c(rep("", 22), "ASIAN", "ASIAN"),
    ETHNICITY = rep(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"), 12)
  )
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm, pk = pk), anon_profile_sdtm(3), max_risk = 1/3, seed = 1))
  # race is a copy of RACE, folded alike; Sex a copy of SEX.
  expect_identical(r$data$pk$race, r$data$dm$RACE[match(r$data$pk$USUBJID, r$data$dm$USUBJID)])
  expect_false(any(r$data$pk$race %in% "ASIAN"))
  expect_match(r$settings$deidentify$quasi$copies[r$settings$deidentify$quasi$quasi == "SEX"], "pk.Sex", fixed = TRUE)
  # GENDER is the same for every subject: not named.
  expect_true(any(grepl("no profile rule matches them: pk.RACEOTH, pk.ETHNICITY\\.", w)))
  expect_false(r$settings$deidentify$meets_target)
})

test_that("a call made while another runs doesn't change the other's result", {
  n <- 48
  set.seed(4)
  x <- data.frame(USUBJID = sprintf("P%02d", 1:n), AGE = sample(20:80, n, TRUE), SEX = rep(c("F", "M"), n / 2))
  x$pk <- data.frame(AGE = x$AGE, AGEGR1 = ifelse(x$AGE < 50, "<50", ">=50"))
  x$multi <- lapply(seq_len(n), function(i) data.frame(AGE = c(x$AGE[i], x$AGE[i] + 1)))
  # Rules limited to adsl: the nested data frames take them through adsl.
  p <- anon_profile(
    anon_profile_rule("USUBJID", "key"),
    anon_profile_rule("AGE", "quasi", datasets = "adsl", ladder = list(anon_band(10), anon_band(20))),
    anon_profile_rule("AGEGR1", "derived", datasets = "adsl", from = "AGE"),
    anon_profile_rule("SEX", "quasi", datasets = "adsl")
  )
  run <- function(reenter) {
    done <- FALSE
    withCallingHandlers(anon_deidentify(list(adsl = x), p, max_risk = 1/6, seed = 1), warning = function(w) {
      if (reenter && !done) {
        done <<- TRUE
        suppressWarnings(anon_deidentify(list(adsl = x[1:3]), p, seed = 9))
      }
      invokeRestart("muffleWarning")
    })
  }
  a <- run(FALSE)
  expect_identical(run(TRUE)$data, a$data)
  # The nested AGEGR1 was derived from the released AGE.
  d <- a$data$adsl
  expect_true(all(d$pk$AGEGR1[d$AGE %in% c(20, 30)] %in% "<50"))
})

test_that("renal function parameters of a BDS dataset are derived from height, weight, age and sex", {
  n <- 12
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = 30 + 1:n, SEX = rep(c("F", "M"), 6), WEIGHTBL = 60 + 1:n * 1.3)
  adlb <- data.frame(USUBJID = rep(adsl$USUBJID, 2), PARAMCD = rep(c("CRCL", "ALT"), each = n), AVAL = c(80 + 1:n, 20 + 1:n))
  r <- deid_quiet(anon_deidentify(list(adsl = adsl, adlb = adlb), anon_profile_adam(3), max_risk = 1/3, seed = 1))
  l <- r$data$adlb
  expect_true(all(is.na(l$AVAL[l$PARAMCD == "CRCL"])))
  expect_identical(sort(l$AVAL[l$PARAMCD == "ALT"]), 20 + 1:n)
})

test_that("what the subject steps leave in the data is a reason the target isn't met", {
  n <- 6
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 3), RFSTDTC = format(as.Date("2021-03-01") + 1:n))
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi"))
  met <- function(...) {
    w <- capture_warnings(r <- anon_deidentify(list(...), profile, max_risk = 1/3, seed = 1))
    list(w = w, s = r$settings$deidentify, steps = r$settings)
  }
  # Original keys in free text.
  x <- dm
  x$COMMENT <- paste("same patient as", x$USUBJID)
  r <- met(dm = x)
  expect_true(any(grepl("still contain original `USUBJID` values[^:]*: dm.COMMENT", r$w)))
  expect_identical(r$s$not_met, "columns still hold original subject keys")
  # The step records it, without values.
  f <- r$steps$subject_ids$findings
  expect_identical(as.list(f[1, ]), list(kind = "keys_left", dataset = "dm", column = "COMMENT", n_values = 6L, single_value = FALSE))
  # Dates inside free text, and a SAS datetime held as text in a result.
  x <- dm
  x$NOTE <- paste("seen on", x$RFSTDTC)
  x$AVALC <- c("09FEB2014:23:30:00", rep("", n - 1))
  r <- met(dm = x)
  expect_identical(r$s$not_met, "columns hold dates that were not offset")
  f <- r$steps$subject_dates$findings
  expect_identical(f$column[f$kind == "dates_unshifted"], c("NOTE", "AVALC"))
  # Date columns in a dataset without the key, and a list column of models.
  lots <- data.frame(RUN = 1:3, RUNDATE = as.Date("2021-01-01") + 1:3)
  x <- tibble::as_tibble(dm)
  x$FIT <- lapply(1:n, function(i) structure(list(coefficients = c(AGE = i)), class = "lm"))
  r <- met(dm = x, lots = lots)
  expect_setequal(r$s$not_met, c(
    "datasets without the key hold identifiers, quasi-identifiers or dates", "columns hold objects that can't be de-identified"
  ))
  # A date with one value in every row (a dictionary version) can't tell
  # subjects apart: not a reason.
  r <- met(dm = dm, dict = data.frame(version = rep("2019-09", 3), TERM = c("a", "b", "c")))
  expect_true(any(grepl("Datasets without `USUBJID`[^:]*: dict \\(version\\)", r$w)))
  expect_true(r$s$meets_target)
  # An identifier with one value in every row is still one.
  r <- met(dm = dm, dict = data.frame(PATIENT = rep(dm$USUBJID[1], 3)))
  expect_identical(r$s$not_met, "columns still hold original subject keys")
})

test_that("attributes holding data, and data frames held in attributes, are reasons the target isn't met", {
  n <- 6
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 3), SEX = rep(c("F", "M"), 3))
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi"), anon_profile_rule("SEX", "quasi"))
  x <- dm
  attr(x$AGE, "label") <- "Age"
  attr(x$AGE, "raw") <- 30 + 1:n
  attr(x$SEX, "who") <- stats::setNames(30 + 1:n, x$USUBJID)
  w <- capture_warnings(r <- anon_deidentify(list(dm = x), profile, max_risk = 1/3, seed = 1))
  expect_true(any(grepl('left as they are and can hold their original values: attr\\(dm.AGE, "raw"\\), attr\\(dm.SEX, "who"\\)\\.', w)))
  expect_identical(r$settings$deidentify$not_met, "attributes of quasi-identifier or derived columns were left as they are")
  # A label alone is fine.
  attr(dm$AGE, "label") <- "Age"
  expect_true(deid_quiet(anon_deidentify(list(dm = dm), profile, max_risk = 1/3, seed = 1))$settings$deidentify$meets_target)
  # A data frame held in a dataset's attribute, holding quasi-identifiers.
  x <- dm
  attr(x, "source") <- data.frame(SUBJ = 1:n, AGE = 30 + 1:n, HEIGHTCM = 170 + 1:n, NOTE = "x")
  w <- capture_warnings(r <- anon_deidentify(list(dm = x), profile, max_risk = 1/3, seed = 1))
  expect_true(any(grepl('held in attributes[^:]*: attr\\(dm, "source"\\)\\$AGE, attr\\(dm, "source"\\)\\$HEIGHTCM\\.', w)))
  expect_identical(r$settings$deidentify$not_met, "data frames held in attributes hold identifiers, quasi-identifiers or dates")
})

test_that("elements of list columns named like quasi-identifiers or by a rule are reasons the target isn't met", {
  n <- 6
  dm <- tibble::tibble(USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 3))
  dm$INFO <- lapply(1:n, function(i) list(AGE = 30 + i, more = list(HEIGHT = 170 + i, NOTE = "x")))
  dm$MEAS <- lapply(1:n, function(i) c(VISIT = 1, WEIGHT = 70 + i))
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi"))
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, seed = 1))
  expect_true(any(grepl("no profile rule matches them: dm.INFO\\[\\[\\]\\]\\$HEIGHT, dm.MEAS\\[\\[\\]\\]\\$WEIGHT\\.", w)))
  expect_true(any(grepl("left as they are \\([^)]*\\): dm.INFO\\[\\[\\]\\]\\$AGE\\.", w)))
  expect_setequal(r$settings$deidentify$not_met, c(
    "columns that look like quasi-identifiers have no profile rule", "columns that match a profile rule were left as they are"
  ))
})

test_that("a dataset without the key is mapped only when every `also` value in a row points to one subject", {
  n <- 12
  adsl <- data.frame(USUBJID = sprintf("01-%03d", 1:n), SUBJID = sprintf("%03d", 1:n), USUBJIDN = 100 + 1:n, AGE = c(31:36, 51:56))
  nm <- data.frame(SUBJID = adsl$SUBJID, USUBJIDN = adsl$USUBJIDN, AGE = adsl$AGE)
  run <- function(nm) {
    w <- capture_warnings(r <- anon_deidentify(list(adsl = adsl, nm = nm), anon_profile_adam(3), max_risk = 1/3, seed = 1))
    list(r = r, mapped = !any(r$data$nm$SUBJID %in% adsl$SUBJID))
  }
  # A missing value is no information: the other column ties the row.
  x <- nm
  x$USUBJIDN[2] <- NA
  expect_true(run(x)$mapped)
  # A value pointing to another subject than the row's other column, even
  # with a missing value elsewhere: not mapped, and the target isn't met.
  x$USUBJIDN[3] <- adsl$USUBJIDN[4]
  out <- run(x)
  expect_false(out$mapped)
  expect_true("datasets without the key hold identifiers, quasi-identifiers or dates" %in% out$r$settings$deidentify$not_met)
  # A value no subject holds, in a column the other ties completely.
  x <- nm
  x$SUBJID[1] <- "999"
  out <- run(x)
  expect_false(out$mapped)
  expect_false(out$r$settings$deidentify$meets_target)
  # The log says how the rows were tied, under the IDs step, and names no
  # key column the released dataset doesn't have.
  out <- run(nm)
  log <- out$r$log[out$r$log$dataset %in% "nm", ]
  expect_false(any(log$variable %in% "USUBJID"))
  expect_identical(log$step[grepl("tied to subjects through `SUBJID`", log$detail)], "subject_ids")
})

test_that("derived test codes follow the rule's pattern, ignoring case", {
  n <- 12
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = 30 + 1:n, SEX = rep(c("F", "M"), 6), WEIGHTBL = 60 + 1:n * 1.3)
  adlb <- data.frame(USUBJID = rep(adsl$USUBJID, 3), PARAMCD = rep(c("crcl", "EGFRCKD", "ALT"), each = n), AVAL = c(80 + 1:n, 90 + 1:n, 20 + 1:n))
  p <- c(anon_profile_adam(3), anon_profile(anon_profile_rule("AVAL", "derived", test_col = "PARAMCD", test = "EGFR*", from = "AGE"), name = "x"))
  r <- deid_quiet(anon_deidentify(list(adsl = adsl, adlb = adlb), p, max_risk = 1/3, seed = 1))
  l <- r$data$adlb
  expect_true(all(is.na(l$AVAL[l$PARAMCD %in% c("crcl", "EGFRCKD")])))
  expect_identical(sort(l$AVAL[l$PARAMCD == "ALT"]), 20 + 1:n)
})

test_that("birth dates, body weights, countries and postal codes under other names are look-alikes", {
  n <- 6
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 3), DOB = format(as.Date("1990-01-01") + 1:n),
    BW = 60 + 1:n, BWT = 61 + 1:n, CNTRY = rep(c("CAN", "ISL"), 3), ZIP = sprintf("9%04d", 1:n), POSTALCD = sprintf("H%dA", 1:n)
  )
  profile <- deid_test_profile(anon_profile_rule("AGE", "quasi", ladder = list(anon_band(10))))
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, seed = 1))
  expect_true(any(grepl("no profile rule matches them: dm.DOB, dm.BW, dm.BWT, dm.CNTRY, dm.ZIP, dm.POSTALCD\\.", w)))
  # A birth date under another name, next to a generalized AGE.
  expect_true(any(grepl("Birth dates more precise than a year[^:]*: dm.DOB\\.", w)))
  expect_false(r$settings$deidentify$meets_target)
  # A look-alike whose one value hides others in its factor levels.
  x <- dm[c("USUBJID", "AGE")]
  x$WEIGHTX <- factor(rep("NA", n), levels = c("NA", "71.2", "80.4"))
  w <- capture_warnings(anon_deidentify(list(dm = x), profile, max_risk = 1/3, seed = 1))
  expect_true(any(grepl("no profile rule matches them: dm.WEIGHTX\\.", w)))
})

test_that("a factor quasi-identifier that no ladder step applies to is suppressed only", {
  n <- 12
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = factor(rep(c(30, 31, 50, 52), 3)), SEX = rep(c("F", "M"), 6))
  w <- capture_warnings(r <- anon_deidentify(list(dm = dm), anon_profile_sdtm(3), max_risk = 1/3, seed = 1))
  expect_true(any(grepl("ladder steps don't apply[^:]*: AGE: ", w)))
  expect_s3_class(r$data$dm$AGE, "factor")
  expect_true(r$settings$deidentify$meets_target)
})

test_that("a codelist without the key can be kept with a rule limited to it", {
  n <- 12
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:n), AGE = rep(c(30, 50), 6), SEX = rep(c("F", "M"), 6))
  fmt <- data.frame(SEX = c("F", "M"), SEXN = 1:2)
  r <- deid_quiet(anon_deidentify(list(adsl = adsl, fmt = fmt), anon_profile_adam(3), max_risk = 1/3, seed = 1))
  expect_false(r$settings$deidentify$meets_target)
  p <- c(anon_profile_adam(3), anon_profile(anon_profile_rule(c("SEX", "SEXN"), "keep", datasets = "fmt"), name = "codelists"))
  r <- deid_quiet(anon_deidentify(list(adsl = adsl, fmt = fmt), p, max_risk = 1/3, seed = 1))
  expect_true(r$settings$deidentify$meets_target)
  expect_identical(r$data$fmt, fmt)
})
