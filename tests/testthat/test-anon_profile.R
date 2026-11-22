test_that("variables match exactly, by SDTM domain wildcard or by pattern", {
  names <- c("VSDTC", "LBDTC", "RFSTDTC", "DTC", "VSXDTC", "TRTSDT", "ADTM", "AGE")
  expect_identical(names[profile_match("--DTC", names)], c("VSDTC", "LBDTC"))
  expect_identical(names[profile_match("--STDTC", names)], "RFSTDTC")
  expect_identical(names[profile_match("*DTC", names)], c("VSDTC", "LBDTC", "RFSTDTC", "DTC", "VSXDTC"))
  expect_identical(names[profile_match("*DT", names)], "TRTSDT")
  expect_identical(names[profile_match("AGE", names)], "AGE")
  expect_identical(names[profile_match("AG?", names)], "AGE")
})

test_that("rules are checked when made", {
  expect_error(anon_profile_rule("AGE", "secret"), "`role` must be one of")
  expect_error(anon_profile_rule("AVAL", "quasi", test_col = "PARAMCD"), "both `test_col` and `test`")
  expect_error(anon_profile_rule("AGE", "quasi", ladder = list("remove", anon_band(5))), "last step")
  expect_error(anon_profile_rule("AGE", "quasi", ladder = list(5)), "Each step of `ladder`")
  expect_error(anon_profile_rule("AGE", "keep", ladder = list(anon_band(5))), "for quasi-identifiers")
  expect_error(anon_profile_rule("AGEGR1", "derived"), "needs `from`")
  expect_error(anon_profile_rule("RFENDTC", "duration", from = "RFSTDTC"), "a `ladder` of one `anon_band\\(\\)`")
  expect_error(anon_profile_rule("AGE", "keep", priority = 1), "for quasi-identifiers")
  expect_error(anon_profile_rule("SEX", "quasi", flag = "VSBLFL"), "held in a test")
})

test_that("a quasi-identifier is named by its variable or test, or by quasi_name for copies", {
  p <- anon_profile(
    anon_profile_rule("AGE", "quasi"),
    anon_profile_rule("AVAL", "quasi", test_col = "PARAMCD", test = "WEIGHT"),
    anon_profile_rule(c("WEIGHTBL", "WTBL"), "quasi", quasi_name = "WEIGHT")
  )
  expect_identical(p$quasi_name, c("AGE", "WEIGHT", "WEIGHT", "WEIGHT"))
  expect_true(all(is.na(anon_profile_rule("BRTHDTC", "remove")$quasi_name)))
})

test_that("a profile can't hold two rules for the same variable, datasets and test", {
  expect_error(
    anon_profile(anon_profile_rule("AGE", "quasi"), anon_profile_rule("AGE", "keep")),
    "More than one rule for `AGE`"
  )
  # Different datasets, or different tests, are different rules.
  expect_s3_class(
    anon_profile(anon_profile_rule("AGE", "quasi"), anon_profile_rule("AGE", "keep", datasets = "dm")),
    "anon_profile"
  )
})

test_that("a column takes the most specific rule: limited to datasets, then exact, then later", {
  p <- anon_profile(
    anon_profile_rule("*DTC", "date"),
    anon_profile_rule("BRTHDTC", "remove"),
    anon_profile_rule("AGE", "keep", datasets = "ad*"),
    anon_profile_rule("AGE", "quasi"),
    anon_profile_rule("--DTC", "keep")
  )
  data <- list(
    dm = data.frame(BRTHDTC = "", RFSTDTC = "", VSDTC = "", AGE = 1),
    adsl = data.frame(AGE = 1, VSDTC = "")
  )
  cols <- profile_columns(p, data)
  role <- function(ds, col) cols$role[cols$dataset == ds & cols$column == col]
  expect_identical(role("dm", "BRTHDTC"), "remove")
  expect_identical(role("dm", "RFSTDTC"), "date")
  # Two patterns: the later wins.
  expect_identical(role("dm", "VSDTC"), "keep")
  expect_identical(role("dm", "AGE"), "quasi")
  # Limited to datasets: wins although it comes first.
  expect_identical(role("adsl", "AGE"), "keep")
})

test_that("combining profiles replaces rules in place and warns about other roles", {
  sdtm <- anon_profile_sdtm()
  mine <- anon_profile(
    anon_profile_rule("AGE", "keep"),
    anon_profile_rule("SITEID", "quasi", ladder = list("remove")),
    anon_profile_rule("WEIGHTKG", "quasi"),
    name = "mine"
  )
  expect_warning(both <- c(sdtm, mine), "`AGE`: quasi \\(sdtm\\) replaced by keep \\(mine\\)")
  expect_s3_class(both, "anon_profile")
  expect_identical(attr(both, "profile_name"), "sdtm + mine")
  expect_identical(nrow(both), nrow(sdtm) + 1L)
  at <- match("AGE", sdtm$variable)
  expect_identical(both$role[at], "keep")
  expect_identical(both$profile[at], "mine")
  # Same role: replaced without a warning.
  expect_identical(both$ladder[[match("SITEID", both$variable)]], list("remove"))
  expect_identical(utils::tail(both$variable, 1), "WEIGHTKG")
  expect_identical(anon_profile_combine(sdtm), sdtm)
  expect_error(anon_profile_combine(sdtm, list()), "Give one or more profiles")
})

test_that("SDTM and ADaM profiles combine without conflicts, one rule per shared variable", {
  expect_silent(both <- c(anon_profile_sdtm(), anon_profile_adam()))
  expect_false(anyDuplicated(profile_rule_ids(both)) > 0)
  expect_identical(sum(both$variable == "AGE"), 1L)
  expect_identical(both$profile[both$variable == "AGE"], "adam")
})

test_that("the standard profiles hold the roles the spec gives them", {
  p <- anon_profile_sdtm(min_n = 5)
  role <- function(v) p$role[p$variable == v & is.na(p$test)]
  expect_identical(role("BRTHDTC"), "remove")
  expect_identical(role("INVNAM"), "remove")
  expect_identical(role("USUBJID"), "key")
  expect_identical(role("SUBJID"), "also")
  expect_identical(role("RSUBJID"), "key_cols")
  expect_identical(role("AGEU"), "keep")
  ladder <- function(v) vapply(p$ladder[[which(p$quasi_name == v)[1]]], describe_ladder_step, character(1))
  expect_identical(ladder("AGE"), c(
    "anon_band(width = 5, top = 90)", "anon_band(width = 10, top = 90)", "anon_band(width = 20, top = 90)"
  ))
  expect_identical(ladder("RACE"), "anon_fold(min_n = 5, to = \"OTHER\")")
  expect_identical(ladder("SITEID"), c("anon_fold(min_n = 5, to = \"OTHER\")", "remove"))
  expect_length(p$ladder[[which(p$quasi_name == "ETHNIC")]], 0)
  expect_length(p$ladder[[which(p$quasi_name == "SEX")]], 0)
  expect_identical(ladder("WEIGHT"), c("anon_round(digits = 0)", "anon_round(digits = -1)", "anon_band(width = 20)"))
  expect_identical(p$flag[which(p$quasi_name == "HEIGHT")], "VSBLFL")

  a <- anon_profile_adam()
  expect_setequal(a$variable[a$role == "remove"], c("INVNAM", "INVID", "BRTHDT", "BRTHDTM", "BRTHDTC"))
  expect_setequal(a$variable[a$role == "quasi" & a$quasi_name == "WEIGHT"], c("WEIGHTBL", "WTBL", "AVAL", "VSSTRESN"))
  expect_identical(a$from[[match("AGEGR*", a$variable)]], "AGE")
  expect_identical(a$from[[match("BMIBL", a$variable)]], c("HEIGHT", "WEIGHT"))
  expect_error(anon_profile_sdtm(min_n = 0), "at least 1")
})

test_that("a profile prints by role and goes to and from a data frame", {
  p <- anon_profile_sdtm()
  out <- capture.output(print(p))
  expect_match(out[1], "<anon_profile> sdtm: 25 rule\\(s\\)")
  expect_true(any(grepl("^  BRTHDTC$", out)))
  expect_true(any(grepl("AGE, ladder: anon_band\\(width = 5, top = 90\\) > ", out)))
  expect_true(any(grepl("SITEID, ladder: .* > remove, priority 1", out)))
  df <- as.data.frame(p)
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "anon_profile"))
  # An edited data frame becomes a profile again.
  df$ladder[[match("AGE", df$variable)]] <- list(anon_band(10, top = 90))
  edited <- anon_profile(df, name = "edited")
  expect_identical(describe_ladder_step(edited$ladder[[match("AGE", edited$variable)]][[1]]), "anon_band(width = 10, top = 90)")
  expect_identical(edited$profile, rep("sdtm", nrow(edited)))
  expect_error(anon_profile(df[, -1]), "need the columns `variable`")
})
