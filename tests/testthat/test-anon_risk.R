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
  risk <- anon_risk(risk_dm(), quasi = c("AGE", "SEX"), k = 3)

  expect_s3_class(risk, "anon_risk")
  expect_equal(risk$subjects$USUBJID, sprintf("S%02d", 1:8))
  expect_equal(risk$subjects$k, c(4, 4, 4, 4, 2, 2, 1, 1))
  expect_equal(risk$subjects$risk, 1 / c(4, 4, 4, 4, 2, 2, 1, 1))

  s <- risk$summary
  expect_equal(s$n_subjects, 8)
  expect_equal(s$n_unique, 2)
  expect_equal(s$min_k, 1)
  expect_equal(s$max_risk, 1)
  expect_equal(s$avg_risk, mean(1 / c(4, 4, 4, 4, 2, 2, 1, 1)))
  expect_equal(s$k_target, 3)
  expect_equal(s$n_below_k, 4)
  expect_equal(s$prop_below_k, 0.5)
  expect_false(s$meets_target)
})

test_that("anon_risk() reports which variables drive the risk", {
  risk <- anon_risk(risk_dm(), quasi = c("AGE", "SEX", "RACE"), k = 3)
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
  drivers <- anon_risk(dm, quasi = c("AGE", "SEX"), key = "USUBJID", k = 5)$drivers
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
      k = 1
    ),
    "No values for `HEIGHT` in `vs` after `filter`"
  )
})

test_that("anon_risk() meets the target when every group is large enough", {
  dm <- risk_dm()[1:4, ]
  risk <- anon_risk(dm, quasi = c("AGE", "SEX"), k = 4)
  expect_true(risk$summary$meets_target)
  expect_equal(risk$summary$max_risk, 0.25)
})

test_that("anon_risk() treats missing values as a value of their own", {
  dm <- data.frame(USUBJID = c("a", "b", "c"), RACE = c(NA, NA, "WHITE"))
  risk <- anon_risk(dm, quasi = "RACE", k = 2)
  expect_equal(risk$subjects$k, c(2, 2, 1))
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

  risk <- anon_risk(
    list(dm = dm, vs = vs),
    quasi = list(
      dm = "SEX",
      anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN", filter = ~ VSBLFL == "Y")
    ),
    k = 2
  )

  expect_equal(risk$quasi, c("SEX", "HEIGHT", "WEIGHT"))
  expect_equal(risk$subjects$k, c(4, 4, 4, 4, 3, 3, 1, 3))
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
    k = 1
  )
  expect_equal(risk$quasi, "BL_WEIGHT")
  expect_equal(risk$summary$n_subjects, 2)
})

test_that("anon_risk() counts subjects missing from a quasi dataset", {
  dm <- data.frame(USUBJID = c("a", "b", "c"), SEX = c("F", "F", "M"))
  vs <- data.frame(USUBJID = c("a", "b"), VSTESTCD = "HEIGHT", VSSTRESN = c(160, 160))
  risk <- anon_risk(
    list(dm = dm, vs = vs),
    quasi = list(dm = "SEX", anon_quasi_values("vs", "VSTESTCD", "HEIGHT", "VSSTRESN")),
    k = 1
  )
  expect_equal(risk$subjects$USUBJID, c("a", "b", "c"))
  expect_equal(risk$subjects$k, c(2, 2, 1))
})

test_that("anon_risk() prefixes column names that appear in more than one dataset", {
  a <- data.frame(USUBJID = c("x", "y"), SITE = c("1", "2"))
  b <- data.frame(USUBJID = c("x", "y"), SITE = c("A", "A"))
  risk <- anon_risk(list(a = a, b = b), quasi = list(a = "SITE", b = "SITE"), k = 1)
  expect_equal(risk$quasi, c("a.SITE", "b.SITE"))
})

test_that("anon_risk() validates its inputs", {
  dm <- risk_dm()
  expect_error(anon_risk(dm, quasi = "AGE", k = 0), "`k` must be a single whole number of at least 1")
  expect_error(anon_risk(dm, quasi = "AGE", k = 2.5), "`k` must be")
  expect_error(anon_risk(dm, quasi = "HEIGHT"), "Column `HEIGHT` not found in `data`")
  expect_error(anon_risk(dm, quasi = "AGE", key = "ID"), "Key column `ID` not found in `data`")
  expect_error(anon_risk(list(dm = dm), quasi = list(vs = "X")), "Dataset `vs` not found")
  expect_error(anon_risk(list(dm = dm), quasi = "AGE"), "`quasi` must name the dataset")
  expect_error(anon_risk(dm, quasi = character(0)), "at least one quasi-identifier")

  long <- data.frame(USUBJID = c("a", "a"), AGE = c(60, 61))
  expect_error(anon_risk(long, quasi = "AGE"), "more than one value per subject")
})

test_that("anon_risk() print and JSON payload omit subject-level data", {
  risk <- anon_risk(risk_dm(), quasi = c("AGE", "SEX"), k = 3)

  output <- paste(capture.output(print(risk)), collapse = "\n")
  expect_match(output, "Re-identification risk", fixed = TRUE)
  expect_match(output, "Smallest group (k): 1", fixed = TRUE)
  expect_false(grepl("S01", output, fixed = TRUE))

  payload <- as_anon_payload(risk)
  expect_equal(payload$kind, "anon_risk")
  expect_equal(payload$quasi, c("AGE", "SEX"))
  expect_null(payload$subjects)
  json <- as_anon_json(risk)
  expect_false(grepl("S01", json, fixed = TRUE))

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
})
