test_that("anon_options() lists and sets supported anon options", {
  old <- anon_options()
  on.exit(options(old), add = TRUE)

  expect_true(all(c(
    "anon.default_replacement",
    "anon.pattern_list",
    "anon.df_variable_names",
    "anon.df_classes",
    "anon.nlp_auto",
    "anon.nlp_default_replacements",
    "anon.example_values_n",
    "anon.example_rows"
  ) %in% names(old)))

  previous <- anon_options(
    default_replacement = "[MASKED]",
    example_values_n = 2,
    example_rows = anon_example_rows(n = 3, method = "last")
  )

  expect_true(all(c(
    "anon.default_replacement",
    "anon.example_values_n",
    "anon.example_rows"
  ) %in% names(previous)))
  expect_equal(getOption("anon.default_replacement"), "[MASKED]")
  expect_equal(getOption("anon.example_values_n"), 2)
  expect_equal(getOption("anon.example_rows"), anon_example_rows(n = 3, method = "last"))
})

test_that("anon_data_summary() and runtime defaults honor package options", {
  old <- anon_options()
  on.exit(options(old), add = TRUE)

  anon_options(
    example_values_n = 1,
    example_rows = anon_example_rows(n = 1, key = "USUBJID", method = "last", n_key_values = 2, seed = 13)
  )

  data <- list(
    adsl = data.frame(USUBJID = c("01", "02"), ARM = c("A", "B")),
    labs = data.frame(USUBJID = c("01", "02"), PARAM = c("ALT", "AST"))
  )

  summary <- anon_data_summary(data)
  expect_equal(length(summary$examples$scenarios), 2)
  expect_true("example_values" %in% names(summary$data_frames$variables$adsl))

  shiny_defaults <- default_data_summary_options()
  expect_equal(shiny_defaults$example_values_n, 1L)
  expect_equal(shiny_defaults$example_rows_n, 1L)
  expect_equal(shiny_defaults$example_rows_key, "USUBJID")
  expect_equal(shiny_defaults$example_rows_method, "last")
  expect_equal(shiny_defaults$example_rows_n_key_values, 2L)
  expect_equal(shiny_defaults$example_rows_seed, "13")
})


test_that("anon_options() preserves NULL assignments when clearing options", {
  old <- anon_options()
  on.exit(options(old), add = TRUE)

  anon_options(pattern_list = list(TEST = "x"))
  previous <- anon_options(pattern_list = NULL)

  expect_true("anon.pattern_list" %in% names(previous))
  expect_equal(previous[["anon.pattern_list"]], list(TEST = "x"))
  expect_null(getOption("anon.pattern_list"))
})

test_that("anon_options() sets and clears df_variable_names", {
  old <- anon_options()
  on.exit(options(old), add = TRUE)

  anon_options(df_variable_names = list("name" = "[REDACTED]"))
  expect_equal(getOption("anon.df_variable_names"), list("name" = "[REDACTED]"))

  anon_options(df_variable_names = NULL)
  expect_null(getOption("anon.df_variable_names"))
})

test_that("anon_options() sets and clears df_classes", {
  old <- anon_options()
  on.exit(options(old), add = TRUE)

  anon_options(df_classes = list(character = "[TEXT]"))
  expect_equal(getOption("anon.df_classes"), list(character = "[TEXT]"))

  anon_options(df_classes = NULL)
  expect_null(getOption("anon.df_classes"))
})

test_that("anon_options() sets and clears nlp_default_replacements", {
  old <- anon_options()
  on.exit(options(old), add = TRUE)

  replacements <- nlp_default_replacements(person = "[NAME]")
  anon_options(nlp_default_replacements = replacements)
  expect_equal(getOption("anon.nlp_default_replacements"), replacements)

  anon_options(nlp_default_replacements = NULL)
  expect_null(getOption("anon.nlp_default_replacements"))
})

test_that("anon_options() sets and clears nlp_auto", {
  old <- anon_options()
  on.exit(options(old), add = TRUE)

  anon_options(nlp_auto = nlp_auto(person = TRUE, org = FALSE))
  expect_false(is.null(getOption("anon.nlp_auto")))

  anon_options(nlp_auto = NULL)
  expect_null(getOption("anon.nlp_auto"))
})

test_that("get_anon_options() returns all 8 option names", {
  result <- anon:::get_anon_options()

  expect_length(result, 8)
  expect_true(all(anon:::anon_option_names() %in% names(result)))
})

test_that("append_option_value() adds to option map", {
  map <- list()
  result <- anon:::append_option_value(map, "anon.test", "value")

  expect_equal(result[["anon.test"]], "value")
})

test_that("append_option_value() handles NULL values", {
  map <- list()
  result <- anon:::append_option_value(map, "anon.test", NULL)

  expect_true("anon.test" %in% names(result))
  expect_null(result[["anon.test"]])
})
