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
  removed <- r$log[r$log$step == "remove" & !is.na(r$log$variable), ]
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
  # Within the budget from the start: nothing is generalized.
  r0 <- anon_deidentify(list(dm = dm), profile, max_risk = 1/3, max_suppressed = 0.5, seed = 1)
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
  data <- deid_test_data()
  data$dm$SITEID <- sprintf("%02d", 1:12)
  data$adsl$SITEID <- data$dm$SITEID
  profile <- c(anon_profile_sdtm(3), anon_profile_adam(3))
  r <- deid_quiet(anon_deidentify(data, profile, max_risk = 1/3, seed = 2))
  q <- r$settings$deidentify$quasi
  expect_identical(q$step[q$quasi == "SITEID"], "remove")
  expect_true(all(r$data$dm$SITEID == ""))
  expect_true(all(r$data$adsl$SITEID == ""))
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
  expect_identical(row("duration RFSTDTC to RFENDTC", "dm")$n_unique, 1L)
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
  expect_identical(r$settings$deidentify$quasi$step, "remove")
  expect_true(all(is.na(r$data$dm$SITEID)))
})

test_that("a profile's date that isn't offset is named in a warning", {
  dm <- data.frame(USUBJID = sprintf("S%02d", 1:3), VISITDT = c("2020-01-01", "2020-02-01", ""), RFSTDTC = "2020-01-01")
  profile <- deid_test_profile(anon_profile_rule("*DT", "date"))
  w <- capture_warnings(anon_deidentify(list(dm = dm), profile, seed = 1))
  expect_true(any(grepl("were not offset[^:]*: dm.VISITDT\\.", w)))
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
  expect_identical(is.na(by_key(advs_w, "VSSTRESN")), is.na(by_key(vs_w, "VSSTRESN")))
  expect_identical(types <- lapply(d, function(df) vapply(df, typeof, "")), lapply(data, function(df) vapply(df, typeof, "")))
})
