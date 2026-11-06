risk_dm <- function() {
  data.frame(
    USUBJID = sprintf("S%02d", 1:8),
    AGE = c(60, 60, 60, 60, 70, 70, 70, 85),
    SEX = c("F", "F", "F", "F", "M", "M", "F", "M"),
    RACE = c("WHITE", "WHITE", "WHITE", "WHITE", "WHITE", "WHITE", "ASIAN", "WHITE"),
    stringsAsFactors = FALSE
  )
}

test_that("anon_risk() measures group sizes and risk per subject", {
  risk <- anon_risk(risk_dm(), quasi = c("AGE", "SEX"), max_risk = 1/3)

  expect_s3_class(risk, "anon_risk")
  subjects <- anon_risk_subjects(risk_dm(), quasi = c("AGE", "SEX"))
  expect_equal(subjects$USUBJID, sprintf("S%02d", 1:8))
  expect_equal(subjects$k, c(4, 4, 4, 4, 2, 2, 1, 1))
  expect_equal(subjects$risk, 1 / c(4, 4, 4, 4, 2, 2, 1, 1))

  s <- risk$summary
  expect_equal(s$n_subjects, 8)
  expect_equal(s$n_unique, 2)
  expect_equal(s$min_k, 1)
  expect_equal(s$max_risk, 1)
  expect_equal(s$avg_risk, mean(1 / c(4, 4, 4, 4, 2, 2, 1, 1)))
  expect_equal(s$k_target, 3)
  expect_equal(s$max_risk_target, 1/3)
  expect_equal(s$n_below_k, 4)
  expect_equal(s$prop_below_k, 0.5)
  expect_false(s$meets_target)
})

test_that("anon_risk() reports which variables drive the risk", {
  risk <- anon_risk(risk_dm(), quasi = c("AGE", "SEX", "RACE"), max_risk = 1/3)
  drivers <- risk$drivers

  expect_equal(drivers$variable[1], "AGE")
  expect_setequal(drivers$variable, c("AGE", "SEX", "RACE"))
  expect_equal(drivers$n_values[drivers$variable == "AGE"], 3)
  # Without AGE: groups F/WHITE (4), M/WHITE (3), F/ASIAN (1)
  expect_equal(drivers$n_below_k_without[drivers$variable == "AGE"], 1)
  expect_equal(drivers$min_k_without[drivers$variable == "AGE"], 1)
  expect_equal(drivers$n_unique_without[drivers$variable == "AGE"], 1)
  expect_equal(drivers$n_missing, c(0, 0, 0))
})

test_that("anon_risk() ranks drivers by remaining unique subjects when all stay below k", {
  dm <- data.frame(
    USUBJID = 1:4,
    AGE = c(60, 61, 62, 63),
    SEX = c("F", "F", "M", "M")
  )
  drivers <- anon_risk(dm, quasi = c("AGE", "SEX"), key = "USUBJID", max_risk = 1/5)$drivers
  expect_equal(drivers$variable, c("AGE", "SEX"))
  expect_equal(drivers$n_unique_without, c(0, 4))
})

test_that("anon_quasi_values() warns when a test has no values", {
  vs <- data.frame(USUBJID = c("a", "b"), VSTESTCD = c("HEIGHT", "WEIGHT"),
                   VSSTRESN = c(160, 70), VSBLFL = c(NA, "Y"))
  expect_warning(
    anon_risk(
      list(vs = vs),
      quasi = list(anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN",
                                     filter = ~ VSBLFL == "Y")),
      max_risk = 1
    ),
    "No values for `HEIGHT` in `vs` after `filter`"
  )
})

test_that("anon_risk() meets the target when every group is large enough", {
  dm <- risk_dm()[1:4, ]
  risk <- anon_risk(dm, quasi = c("AGE", "SEX"), max_risk = 1/4)
  expect_true(risk$summary$meets_target)
  expect_equal(risk$summary$max_risk, 0.25)
})

test_that("anon_risk() treats missing values as a value of their own", {
  dm <- data.frame(USUBJID = c("a", "b", "c"), RACE = c(NA, NA, "WHITE"))
  expect_equal(anon_risk_subjects(dm, quasi = "RACE")$k, c(2, 2, 1))
})

test_that("anon_risk() combines quasi-identifiers from several datasets", {
  dm <- risk_dm()
  vs <- data.frame(
    USUBJID = rep(dm$USUBJID, each = 2),
    VSTESTCD = rep(c("HEIGHT", "WEIGHT"), times = 8),
    VSSTRESN = c(160, 60, 160, 60, 160, 60, 160, 60, 180, 80, 180, 80, 155, 50, 180, 80),
    VSBLFL = "Y",
    stringsAsFactors = FALSE
  )
  vs <- rbind(vs, data.frame(USUBJID = "S01", VSTESTCD = "WEIGHT", VSSTRESN = 65, VSBLFL = ""))

  quasi <- list(
    dm = "SEX",
    anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN", filter = ~ VSBLFL == "Y")
  )
  risk <- anon_risk(list(dm = dm, vs = vs), quasi = quasi, max_risk = 1/2)

  expect_equal(risk$quasi, c("SEX", "HEIGHT", "WEIGHT"))
  expect_equal(anon_risk_subjects(list(dm = dm, vs = vs), quasi = quasi)$k, c(4, 4, 4, 4, 3, 3, 1, 3))
})

test_that("anon_quasi_values() uses the first value per subject and names columns by test", {
  vs <- data.frame(
    USUBJID = c("a", "a", "b"),
    VSTESTCD = "WEIGHT",
    VSSTRESN = c(NA, 70, 80)
  )
  risk <- anon_risk(
    list(vs = vs),
    quasi = list(anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN", names = "BL_WEIGHT")),
    max_risk = 1
  )
  expect_equal(risk$quasi, "BL_WEIGHT")
  expect_equal(risk$summary$n_subjects, 2)
})

test_that("anon_risk() counts subjects missing from a quasi dataset", {
  dm <- data.frame(USUBJID = c("a", "b", "c"), SEX = c("F", "F", "M"))
  vs <- data.frame(USUBJID = c("a", "b"), VSTESTCD = "HEIGHT", VSSTRESN = c(160, 160))
  subjects <- anon_risk_subjects(
    list(dm = dm, vs = vs),
    quasi = list(dm = "SEX", anon_quasi_values("vs", "VSTESTCD", "HEIGHT", "VSSTRESN"))
  )
  expect_equal(subjects$USUBJID, c("a", "b", "c"))
  expect_equal(subjects$k, c(2, 2, 1))
})

test_that("anon_risk() prefixes column names that appear in more than one dataset", {
  a <- data.frame(USUBJID = c("x", "y"), SITE = c("1", "2"))
  b <- data.frame(USUBJID = c("x", "y"), SITE = c("A", "A"))
  risk <- anon_risk(list(a = a, b = b), quasi = list(a = "SITE", b = "SITE"), max_risk = 1)
  expect_equal(risk$quasi, c("a.SITE", "b.SITE"))
})

test_that("anon_risk() validates its inputs", {
  dm <- risk_dm()
  expect_error(anon_risk(dm, quasi = "AGE", max_risk = 0), "`max_risk` must be a single number greater than 0 and at most 1")
  expect_error(anon_risk(dm, quasi = "AGE", max_risk = 1.5), "`max_risk` must be")
  expect_error(anon_risk(dm, quasi = "AGE", max_risk = NA), "`max_risk` must be")
  expect_error(anon_risk(dm, quasi = "HEIGHT"), "Column `HEIGHT` not found in `data`")
  expect_error(anon_risk(dm, quasi = "AGE", key = "ID"), "Key column `ID` not found in `data`")
  expect_error(anon_risk(list(dm = dm), quasi = list(vs = "X")), "Dataset `vs` not found")
  expect_error(anon_risk(list(dm = dm), quasi = "AGE"), "`quasi` must name the dataset")
  expect_error(anon_risk(dm, quasi = character(0)), "at least one quasi-identifier")

  long <- data.frame(USUBJID = c("a", "a"), AGE = c(60, 61))
  expect_error(anon_risk(long, quasi = "AGE"), "more than one value per subject")
})

test_that("anon_risk() print and JSON payload omit subject-level data", {
  risk <- anon_risk(risk_dm(), quasi = c("AGE", "SEX"), max_risk = 1/3)

  output <- paste(capture.output(print(risk)), collapse = "\n")
  expect_match(output, "Re-identification risk", fixed = TRUE)
  expect_match(output, "Target: maximum risk 0.3333333 (groups of at least 3)", fixed = TRUE)
  expect_match(output, "Subjects in groups smaller than 3: 4 (50%)", fixed = TRUE)
  expect_match(output, "Maximum risk: 1 (smallest group: 1)", fixed = TRUE)
  expect_false(grepl("S01", output, fixed = TRUE))

  payload <- as_anon_payload(risk)
  expect_equal(payload$kind, "anon_risk")
  expect_equal(payload$quasi, c("AGE", "SEX"))
  expect_null(payload$subjects)
  json <- as_anon_json(risk)
  expect_false(grepl("S01", json, fixed = TRUE))

  # anon() rebuilds lists without their class; nothing subject-level is left to show
  redacted <- anon(risk)
  expect_false(grepl("S01", as_anon_json(redacted), fixed = TRUE))
  expect_false(grepl("S01", paste(capture.output(print(redacted)), collapse = "\n"), fixed = TRUE))
  expect_false(grepl("S01", as_anon_json(anon(list(r = risk))), fixed = TRUE))

  nested <- serialize_supported_value(list(risk = risk))
  expect_equal(nested$risk$kind, "anon_risk")
})

test_that("anon_risk() reproduces uniqueness in pharmaversesdtm dm", {
  skip_if_not_installed("pharmaversesdtm")
  dm <- pharmaversesdtm::dm
  risk <- anon_risk(dm, quasi = c("AGE", "SEX", "RACE", "ETHNIC", "SITEID"))
  expect_equal(risk$summary$n_subjects, 306)
  expect_equal(risk$summary$n_unique, 202)
  expect_equal(risk$summary$n_below_k, 306)
  expect_false(risk$summary$meets_target)
  expect_equal(risk$drivers$variable[1], "AGE")
  expect_equal(risk$drivers$n_unique_without[1], 20)

  risk <- anon_risk(
    list(dm = dm, vs = pharmaversesdtm::vs),
    quasi = list(
      dm = c("AGE", "SEX", "RACE", "ETHNIC", "SITEID"),
      anon_quasi_values("vs", "VSTESTCD", "HEIGHT", "VSSTRESN"),
      anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN", filter = ~ VSBLFL == "Y")
    )
  )
  expect_equal(risk$summary$n_unique, 304)
})

test_that("anon_risk() evaluates filter with variables from where it was written", {
  vs <- data.frame(USUBJID = c("a", "b", "c"), VSDY = c(1, 5, 9), VSTESTCD = "W", VSSTRESN = c(70, 80, 90))
  by_day <- function(cut) {
    anon_risk_subjects(
      list(vs = vs),
      quasi = list(anon_quasi_values("vs", "VSTESTCD", "W", "VSSTRESN", filter = ~ VSDY <= cut))
    )
  }
  expect_equal(by_day(5)$k, c(1, 1, 1))
  expect_error(
    anon_risk(list(vs = vs), quasi = list(anon_quasi_values("vs", "VSTESTCD", "W", "VSSTRESN", filter = ~ VSDY))),
    "must return TRUE or FALSE for each row"
  )
})

test_that("anon_risk() requires the key to have the same type in every dataset", {
  dm <- data.frame(USUBJID = c(1e5, 2e5), AGE = c(60, 70))
  vs <- data.frame(USUBJID = c("100000", "200000"), VSTESTCD = "W", VSSTRESN = c(70, 80))
  expect_error(
    anon_risk(list(dm = dm, vs = vs), quasi = list(dm = "AGE", anon_quasi_values("vs", "VSTESTCD", "W", "VSSTRESN"))),
    "different types across datasets"
  )

  vs$USUBJID <- c(1e5, 2e5)
  subjects <- anon_risk_subjects(
    list(dm = dm, vs = vs),
    quasi = list(dm = "AGE", anon_quasi_values("vs", "VSTESTCD", "W", "VSSTRESN"))
  )
  expect_equal(subjects$USUBJID, c("100000", "200000"))

  dm$USUBJID <- factor(c("a", "b"))
  vs$USUBJID <- c("a", "b")
  expect_equal(nrow(anon_risk_subjects(
    list(dm = dm, vs = vs),
    quasi = list(dm = "AGE", anon_quasi_values("vs", "VSTESTCD", "W", "VSSTRESN"))
  )), 2)
})

test_that("anon_risk() groups by exact values without separator collisions", {
  d <- data.frame(USUBJID = 1:4, A = c("x\u001fy", "x", "\u0001NA", NA), B = c("z", "y\u001fz", "q", "q"))
  expect_equal(anon_risk_subjects(d, quasi = c("A", "B"))$k, c(1, 1, 1, 1))

  n <- data.frame(USUBJID = 1:2, X = c(0.1 + 0.2, 0.3))
  risk <- anon_risk(n, quasi = "X", max_risk = 1)
  expect_equal(risk$summary$n_unique, 2)
  expect_equal(risk$drivers$n_values, 2)

  f <- data.frame(USUBJID = 1:3, SEX = factor(c("F", "F", "M")))
  expect_equal(anon_risk_subjects(f, quasi = "SEX")$k, c(2, 2, 1))
})

test_that("anon_risk() warns about missing keys and rejects empty data and name clashes", {
  dm <- data.frame(USUBJID = c("a", NA, "b", NA), AGE = c(60, 61, 62, 63))
  expect_warning(risk <- anon_risk(dm, quasi = "AGE", max_risk = 1), "2 row\\(s\\) with a missing `USUBJID` were dropped")
  expect_equal(risk$summary$n_subjects, 2)

  expect_error(anon_risk(dm[0, ], quasi = "AGE"), "No subjects found")
  expect_error(anon_risk(risk_dm(), quasi = c("USUBJID", "AGE")), "must not be named like the key")
  expect_error(anon_risk(risk_dm(), quasi = c("AGE", "AGE")), "must not contain duplicates")
  expect_error(anon_quasi_values("vs", "VSTESTCD", c("W", "W"), "VSSTRESN"), "`tests` must not contain duplicates")
  expect_error(anon_quasi_values(c("vs", "dm"), "VSTESTCD", "W", "VSSTRESN"), "`dataset` must be a single string")
  expect_error(anon_quasi_values("vs", "VSTESTCD", "W", "VSSTRESN", names = "USUBJID") |>
    (\(spec) anon_risk(list(vs = data.frame(USUBJID = "a", VSTESTCD = "W", VSSTRESN = 1)), quasi = list(spec)))(),
    "must not be named like the key")
})

test_that("anon_risk() drivers handle a single quasi-identifier and report average risk", {
  risk <- anon_risk(risk_dm(), quasi = "AGE", max_risk = 1/3)
  expect_equal(risk$drivers$min_k_without, 8)
  expect_equal(risk$drivers$avg_risk_without, 1 / 8)
})

test_that("anon_risk() default 0.09 requires groups of 12; 1/11 allows 11", {
  dm <- data.frame(USUBJID = 1:11, SEX = "F")
  risk <- anon_risk(dm, quasi = "SEX")
  expect_equal(risk$summary$k_target, 12)
  expect_equal(risk$summary$max_risk_target, 0.09)
  expect_false(risk$summary$meets_target)
  expect_equal(risk$summary$max_risk, 1 / 11)

  risk <- anon_risk(dm, quasi = "SEX", max_risk = 1/11)
  expect_equal(risk$summary$k_target, 11)
  expect_true(risk$summary$meets_target)

  expect_equal(anon_risk(dm, quasi = "SEX", max_risk = 0.1)$summary$k_target, 10)
  expect_equal(anon_risk(dm, quasi = "SEX", max_risk = 0.2)$summary$k_target, 5)
  expect_equal(anon_risk(dm, quasi = "SEX", max_risk = 1)$summary$k_target, 1)
})

test_that("anon_risk() output separates 1/11 from 0.0909 at the boundary", {
  dm <- data.frame(USUBJID = 1:11, SEX = "F")
  output <- capture.output(print(anon_risk(dm, quasi = "SEX", max_risk = 0.0909)))
  expect_true(any(grepl("Target: maximum risk 0.0909 (groups of at least 12)", output, fixed = TRUE)))
  expect_true(any(grepl("Maximum risk: 0.09090909 (smallest group: 11)", output, fixed = TRUE)))

  json <- as_anon_json(anon_risk(dm, quasi = "SEX", max_risk = 0.0909))
  expect_match(json, '"max_risk": 0.0909090909', fixed = TRUE)
})

test_that("numeric keys keep every digit and match across datasets", {
  dm <- data.frame(ID = c(1, 2, 1000000.1, 1000000.2), AGE = c(60, 61, 62, 63))
  vs <- data.frame(ID = c(1, 2.5), VSTESTCD = "W", VSSTRESN = c(70, 80))
  subjects <- anon_risk_subjects(
    list(dm = dm, vs = vs),
    quasi = list(dm = "AGE", anon_quasi_values("vs", "VSTESTCD", "W", "VSSTRESN")),
    key = "ID"
  )
  expect_equal(subjects$ID, c("1", "2", "1000000.1", "1000000.2", "2.5"))
})

test_that("anon_quasi_values() prints what it selects", {
  spec <- anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN", filter = ~ VSBLFL == "Y")
  output <- paste(capture.output(print(spec)), collapse = "\n")
  expect_match(output, "HEIGHT = first non-missing VSSTRESN where VSTESTCD == \"HEIGHT\"", fixed = TRUE)
  expect_match(output, "Filter: VSBLFL == \"Y\"", fixed = TRUE)
})
