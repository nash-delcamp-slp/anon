test_that("anon_band() bands numbers from 0, with top and bottom coding", {
  ages <- c(23, 37, 41, 58, 64, 89, 90, 97, NA)
  expect_equal(anon_band(5)(ages), c(20, 35, 40, 55, 60, 85, 90, 95, NA))
  expect_equal(anon_band(5, top = 90)(ages), c(20, 35, 40, 55, 60, 85, 90, 90, NA))
  expect_equal(anon_band(10, bottom = 40, top = 90)(ages), c(40, 40, 40, 50, 60, 80, 90, 90, NA))
  expect_equal(anon_band(10, value = "midpoint")(c(23, 37, 40)), c(25, 35, 45))
  # Fractions of a width stay in their band.
  expect_equal(anon_band(0.1)(c(0.3, 0.29, 1.05)), c(0.3, 0.2, 1.0))
  expect_equal(anon_band(5)(c(-3, -5)), c(-5, -5))
})

test_that("anon_band() and anon_round() keep the type and attributes", {
  x <- structure(c(23L, 37L, NA), label = "Age")
  out <- anon_band(5)(x)
  expect_identical(out, structure(c(20L, 35L, NA), label = "Age"))
  expect_error(anon_band(5, value = "midpoint")(x), "integer column, numbers that aren't whole")
  expect_identical(anon_band(10, value = "midpoint")(c(23L, 37L)), c(25L, 35L))
  expect_identical(anon_round(-1)(c(153L, 178L)), c(150L, 180L))
  expect_equal(anon_round(1)(c(70.44, 70.46)), c(70.4, 70.5))
  expect_error(anon_band(5)(factor("a")), "must be numeric")
  expect_error(anon_band(5)(Sys.Date()), "must be numeric")
  expect_error(anon_round(0)("1"), "must be numeric")
})

test_that("rule constructors validate their arguments and print readably", {
  expect_error(anon_band(0), "greater than 0")
  expect_error(anon_band("5"), "single number")
  expect_error(anon_band(5, top = 10, bottom = 20), "below `top`")
  expect_error(anon_round(1.5), "whole number")
  expect_error(anon_fold(12), "`to` is required")
  expect_error(anon_fold(0, "OTHER"), "at least 1")
  expect_error(anon_fold(2, c("A", "B")), "single string or number")
  expect_output(print(anon_band(5, top = 90)), "<anon rule> anon_band\\(width = 5, top = 90\\)")
  expect_output(print(anon_band(5, value = "midpoint")), "value = \"midpoint\"")
  expect_output(print(anon_round(0)), "anon_round\\(digits = 0\\)")
  expect_output(print(anon_fold(12, "OTHER")), "anon_fold\\(min_n = 12, to = \"OTHER\"\\)")
  rule <- anon_rule(anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN"), anon_round(0))
  expect_output(print(rule), "anon_round\\(digits = 0\\) on `vs`: VSSTRESN where VSTESTCD is \"HEIGHT\", \"WEIGHT\"")
  expect_error(anon_rule("vs", anon_round(0)), "anon_quasi_values")
  expect_error(anon_rule(anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN"), round), "made by")
})

test_that("anon_fold() folds categories held by fewer than min_n subjects", {
  race <- c("WHITE", "WHITE", "ASIAN", "BLACK", "WHITE", "", NA)
  expect_identical(anon_fold(2, "OTHER")(race), c("WHITE", "WHITE", "OTHER", "OTHER", "WHITE", "", NA))
  # Counted by subject, not by row.
  expect_identical(anon_fold(2, "OTHER")(c("A", "A", "B", "B"), subjects = c(1, 1, 2, 3)), c("OTHER", "OTHER", "B", "B"))
  # `to` itself is never folded.
  expect_identical(anon_fold(2, "OTHER")(c("OTHER", "A", "A")), c("OTHER", "A", "A"))
  expect_identical(anon_fold(2, 99)(c(1, 1, 2)), c(1, 1, 99))
  expect_error(anon_fold(2, "X")(c(1, 2)), "`to` must be a number")
  expect_identical(anon_fold(2, 99)(c(1L, 1L, 2L)), c(1L, 1L, 99L))
})

test_that("anon_fold() keeps a factor a factor with updated levels", {
  f <- factor(c("WHITE", "WHITE", "ASIAN", "BLACK"), levels = c("ASIAN", "BLACK", "WHITE", "UNUSED"))
  attr(f, "label") <- "Race"
  out <- anon_fold(2, "OTHER")(f)
  expect_s3_class(out, "factor")
  expect_identical(levels(out), c("WHITE", "OTHER"))
  expect_identical(as.character(out), c("WHITE", "WHITE", "OTHER", "OTHER"))
  expect_identical(attr(out, "label"), "Race")
  expect_identical(anon_fold(1, "OTHER")(factor(c("a", "b"))), factor(c("a", "b")))
})

test_that("haven value labels are kept only where they still apply", {
  skip_if_not_installed("haven")
  race <- haven::labelled(c("W", "W", "A", "B"), labels = c(White = "W", Asian = "A", Other = "O"))
  out <- anon_fold(2, "O")(race)
  expect_s3_class(out, "haven_labelled")
  expect_identical(as.character(vctrs::vec_data(out)), c("W", "W", "O", "O"))
  expect_identical(attr(out, "labels"), c(White = "W", Other = "O"))

  age <- haven::labelled(c(20, 23, 90, 95), labels = c("Twenty" = 20, "90 or older" = 90))
  out <- anon_band(5, top = 90)(age)
  expect_identical(as.numeric(vctrs::vec_data(out)), c(20, 20, 90, 90))
  # 23 moved into 20 and 95 into 90: neither label describes its values now.
  expect_length(attr(out, "labels"), 0)
  out <- anon_band(5)(haven::labelled(c(20, 90), labels = c("Twenty" = 20, "Ninety" = 90)))
  expect_identical(attr(out, "labels"), c("Twenty" = 20, "Ninety" = 90))

  # User-defined missing values are left alone.
  w <- haven::labelled_spss(c(71, 999), na_values = 999)
  expect_identical(as.numeric(vctrs::vec_data(anon_round(-1)(w))), c(70, 999))
})

gen_data <- function() {
  dm <- data.frame(
    USUBJID = sprintf("S%02d", 1:6),
    AGE = c(34L, 37L, 52L, 58L, 91L, 93L),
    RACE = c("WHITE", "WHITE", "WHITE", "ASIAN", "WHITE", "BLACK"),
    stringsAsFactors = FALSE
  )
  attr(dm$AGE, "label") <- "Age"
  adsl <- tibble::tibble(USUBJID = dm$USUBJID, RACE = factor(c("WHITE", "WHITE", "WHITE", "ASIAN", "WHITE", "BLACK")))
  vs <- data.frame(
    USUBJID = rep(dm$USUBJID, each = 2),
    VSTESTCD = rep(c("WEIGHT", "HEIGHT"), 6),
    VSORRES = c("70.4", "64.0", "65.2", "170", "180.9", "69.5", "68.13", "171.2", "60.3", "ND", "55.5", "<150"),
    VSORRESU = c("kg", "in", "kg", "cm", "LB", "in", "kg", "cm", "kg", "cm", "kg", "cm"),
    VSSTRESC = c("70.4", "162.56", "65.2", "170", "82.06", "176.53", "68.13", "171.2", "60.3", "", "55.5", "<150"),
    VSSTRESN = c(70.4, 162.56, 65.2, 170, 82.06, 176.53, 68.13, 171.2, 60.3, NA, 55.5, NA),
    VSSTRESU = c("kg", "cm", "kg", "cm", "kg", "cm", "kg", "cm", "kg", "cm", "kg", "cm"),
    VSBLFL = rep(c("Y", ""), 6),
    stringsAsFactors = FALSE
  )
  list(dm = dm, adsl = adsl, vs = vs)
}

test_that("anon_generalize() applies column rules and keeps type, attributes and rows", {
  data <- gen_data()
  result <- anon_generalize(data, rules = list(dm = list(AGE = anon_band(10, top = 90))))
  expect_s3_class(result, "anon_deid_step")
  out <- result$data
  expect_identical(out$dm$AGE, structure(c(30L, 30L, 50L, 50L, 90L, 90L), label = "Age"))
  expect_identical(out$dm[c("USUBJID", "RACE")], data$dm[c("USUBJID", "RACE")])
  expect_identical(out[c("adsl", "vs")], data[c("adsl", "vs")])
  log <- result$log
  expect_identical(log$step, rep("generalize", 2))
  expect_identical(log$n_changed[2], 6L)
  expect_match(log$detail[1], "dm AGE: anon_band\\(width = 10, top = 90\\)")
  expect_match(log$detail[2], "banded: width 10, at or above 90 set to it, lower bound as the value")
  expect_identical(result$settings$generalize$rules[[1]], list(dataset = "dm", column = "AGE", rule = "band", width = 10, top = 90, value = "lower"))
  expect_null(result$secrets)
})

test_that("anon_fold() in anon_generalize() counts subjects across the datasets that list the column", {
  data <- gen_data()
  # ASIAN and BLACK have one subject each, in both datasets.
  rules <- list(dm = list(RACE = anon_fold(2, "OTHER")), adsl = list(RACE = anon_fold(2, "OTHER")))
  result <- anon_generalize(data, rules = rules, keep_secrets = TRUE)
  expect_identical(result$data$dm$RACE, c("WHITE", "WHITE", "WHITE", "OTHER", "WHITE", "OTHER"))
  expect_identical(as.character(result$data$adsl$RACE), result$data$dm$RACE)
  expect_identical(levels(result$data$adsl$RACE), c("WHITE", "OTHER"))
  folded <- anon_secrets(result)$generalize_folded
  expect_setequal(folded$category, c("ASIAN", "BLACK"))
  expect_identical(unique(folded$dataset), c("dm", "adsl"))
  # Values appear only in the secrets, never in the log or settings.
  expect_false(any(grepl("ASIAN|BLACK", c(result$log$detail, unlist(result$settings)))))
  expect_output(print(result), "generalize")

  # Subjects are counted across the datasets: ASIAN has one subject in dm
  # and another only in adsl, so it is kept in both.
  adsl <- tibble::tibble(USUBJID = c(data$dm$USUBJID, "S07"), RACE = c(data$dm$RACE, "ASIAN"))
  out <- anon_generalize(list(dm = data$dm, adsl = adsl), rules = list(
    dm = list(RACE = anon_fold(2, "OTHER")), adsl = list(RACE = anon_fold(2, "OTHER"))
  ))$data
  expect_identical(out$dm$RACE, c("WHITE", "WHITE", "WHITE", "ASIAN", "WHITE", "OTHER"))
  expect_identical(out$adsl$RACE, c(out$dm$RACE, "ASIAN"))

  # Rows are counted by subject: a category on two rows of one subject is
  # small.
  vs <- data.frame(USUBJID = c("a", "a", "b", "c"), POS = c("X", "X", "Y", "Y"))
  out <- anon_generalize(vs, rules = list(POS = anon_fold(2, "OTHER")))$data
  expect_identical(out$POS, c("OTHER", "OTHER", "Y", "Y"))
  expect_error(anon_generalize(list(a = data.frame(X = "a")), list(a = list(X = anon_fold(2, "B")))), "counts subjects by `USUBJID`")
})

test_that("anon_rule() generalizes every row of a test and keeps the text results consistent", {
  data <- gen_data()
  spec <- anon_quasi_values("vs", "VSTESTCD", c("WEIGHT", "HEIGHT"), "VSSTRESN", filter = ~ VSBLFL == "Y")
  result <- anon_generalize(data, rules = list(anon_rule(spec, anon_round(0))))
  vs <- result$data$vs
  # Every row of each test, not only the baseline rows the filter selects.
  expect_identical(vs$VSSTRESN, c(70, 163, 65, 170, 82, 177, 68, 171, 60, NA, 56, NA))
  # --STRESC from --STRESN at its own decimals; non-numbers unchanged.
  expect_identical(vs$VSSTRESC, c("70.0", "163.00", "65.0", "170", "82.00", "177.00", "68.00", "171.0", "60.0", "", "56.0", "<150"))
  # --ORRES from --STRESN in the same units; on its own in others (LB, in).
  expect_identical(vs$VSORRES, c("70.0", "64.0", "65.0", "170", "181.0", "70.0", "68.00", "171.0", "60.0", "ND", "56.0", "<150"))
  expect_identical(vs[setdiff(names(vs), c("VSSTRESN", "VSSTRESC", "VSORRES"))], data$vs[setdiff(names(vs), c("VSSTRESN", "VSSTRESC", "VSORRES"))])
  log <- result$log
  expect_identical(log$variable[-1], c(
    "VSSTRESN (VSTESTCD = WEIGHT)", "VSORRES (VSTESTCD = WEIGHT)", "VSSTRESC (VSTESTCD = WEIGHT)",
    "VSSTRESN (VSTESTCD = HEIGHT)", "VSORRES (VSTESTCD = HEIGHT)", "VSSTRESC (VSTESTCD = HEIGHT)"
  ))
  expect_match(log$detail[log$variable %in% "VSORRES (VSTESTCD = WEIGHT)"], "5 rewritten from the generalized `VSSTRESN`, 1 generalized on their own")
  expect_match(log$detail[log$variable %in% "VSORRES (VSTESTCD = HEIGHT)"], "2 value\\(s\\) that aren't plain numbers left unchanged")

  # A midpoint needs a decimal that the original text didn't have.
  vs2 <- data.frame(USUBJID = "a", VSTESTCD = "WEIGHT", VSORRES = "64", VSSTRESC = "64", VSSTRESN = 64)
  out <- anon_generalize(list(vs = vs2), list(anon_rule(anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN"), anon_band(5, value = "midpoint"))))$data$vs
  expect_identical(c(out$VSORRES, out$VSSTRESC, as.character(out$VSSTRESN)), c("62.5", "62.5", "62.5"))
})

test_that("anon_rule() with anon_fold() folds a text result and its other results", {
  sc <- data.frame(
    USUBJID = c("a", "b", "c", "d"),
    SCTESTCD = "EDULEVEL",
    SCORRES = c("College", "College", "PhD", "High school"),
    SCSTRESC = c("COLLEGE", "COLLEGE", "PHD", "HIGH SCHOOL"),
    SCSTRESN = c(NA, NA, 3, NA)
  )
  out <- anon_generalize(
    list(sc = sc), list(anon_rule(anon_quasi_values("sc", "SCTESTCD", "EDULEVEL", "SCSTRESC"), anon_fold(2, "OTHER")))
  )$data$sc
  expect_identical(out$SCSTRESC, c("COLLEGE", "COLLEGE", "OTHER", "OTHER"))
  expect_identical(out$SCORRES, c("College", "College", "OTHER", "OTHER"))
  expect_identical(out$SCSTRESN, c(NA, NA, NA_real_, NA))
  expect_error(
    anon_generalize(list(sc = sc), list(anon_rule(anon_quasi_values("sc", "SCTESTCD", "EDULEVEL", "SCSTRESN"), anon_fold(2, 9)))),
    "needs a text result column"
  )
  expect_error(
    anon_generalize(list(sc = sc), list(anon_rule(anon_quasi_values("sc", "SCTESTCD", "EDULEVEL", "SCSTRESC"), anon_round(0)))),
    "needs a numeric result column"
  )
})

test_that("anon_generalize() validates rules and chains with the other steps", {
  data <- gen_data()
  expect_error(anon_generalize(data, rules = list()), "non-empty list")
  expect_error(anon_generalize(data, rules = list(xx = list(AGE = anon_band(5)))), "Dataset `xx`")
  expect_error(anon_generalize(data, rules = list(dm = list(XX = anon_band(5)))), "Column `XX` not found in `dm`")
  expect_error(anon_generalize(data, rules = list(dm = list(AGE = 5))), "named by a dataset")
  expect_error(
    anon_generalize(data, rules = list(dm = list(AGE = anon_band(5)), dm = list(AGE = anon_band(10)))),
    "`AGE` in `dm` has more than one rule"
  )
  expect_error(anon_generalize(data, rules = list(dm = list(RACE = anon_band(5)))), "`RACE` must be numeric")

  ids <- anon_subject_ids(data, seed = 1)
  step <- anon_generalize(ids, rules = list(dm = list(AGE = anon_band(10))))
  expect_identical(unique(step$log$step), c("subject_ids", "generalize"))
  expect_named(step$settings, c("subject_ids", "generalize"))
  dated <- anon_subject_dates(step, seed = 2)
  expect_identical(dated$data$dm$AGE, step$data$dm$AGE)
})
