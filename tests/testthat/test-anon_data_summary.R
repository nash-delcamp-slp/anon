test_that("anon_data_summary() works with basic data frames", {
  df <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    email = c("alice@test.com", "bob@test.com", "charlie@test.com"),
    age = c(25, 30, 35),
    score = c(95.5, 87.2, 92.8)
  )

  env_list <- list(study_data = df)
  result <- anon_data_summary(env_list)

  expect_s3_class(result, "anon_data_summary")
  expect_true("summary" %in% names(result))
  expect_true("data_frames" %in% names(result))

  # Check summary structure
  expect_equal(result$summary$total_objects, 1)
  expect_equal(result$summary$data_frames, 1)
  expect_equal(result$summary$other_objects, 0)

  # Check data frame structure info
  expect_equal(nrow(result$data_frames$structure), 1)
  expect_equal(result$data_frames$structure$name, "study_data")
  expect_equal(result$data_frames$structure$n_rows, 3)
  expect_equal(result$data_frames$structure$n_cols, 4)

  # Check variable details
  expect_true("variables" %in% names(result$data_frames))
  expect_true(is.list(result$data_frames$variables))
  expect_true("study_data" %in% names(result$data_frames$variables))

  study_data_vars <- result$data_frames$variables$study_data
  expect_equal(nrow(study_data_vars), 4)
  expect_true(all(c("name", "email", "age", "score") %in% study_data_vars$variable))
  expect_true("data_type" %in% names(study_data_vars))
  expect_true("n_missing" %in% names(study_data_vars))
})

test_that("anon_data_summary() works with pattern anonymization", {
  study_results <- data.frame(
    participant_id = c("P001", "P002", "P003"),
    ABC123_RESULT = c(85.2, 92.1, 78.5),
    ABC123_BASELINE = c(80.0, 88.3, 75.2),
    age = c(45, 32, 67)
  )

  study_metadata <- list(
    primary_study = "ABC123",
    principal_investigator = "Dr. Smith",
    site_location = "Boston Medical Center"
  )

  env_list <- list(study_results = study_results, metadata = study_metadata)

  result <- anon_data_summary(
    env_list,
    pattern_list = list(
      "STUDY_A" = "ABC123",
      "INVESTIGATOR" = "Dr. Smith",
      "MEDICAL_CENTER" = "Boston Medical Center"
    )
  )

  # Check that patterns were anonymized in variable names
  study_results_vars <- result$data_frames$variables$study_results
  anonymized_vars <- study_results_vars$variable
  expect_true(any(grepl("STUDY_A", anonymized_vars)))
  expect_false(any(grepl("ABC123", anonymized_vars)))

  # Check that patterns were anonymized in metadata values
  # This would be visible in the print output or if we had access to the raw list data
  expect_s3_class(result, "anon_data_summary")
})

test_that("anon_data_summary() works with mixed object types", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  vec <- c("test", "data", "values")
  num_vec <- c(1.5, 2.7, 3.9)

  env_list <- list(my_df = df, my_vec = vec, my_nums = num_vec)
  result <- anon_data_summary(env_list)

  expect_equal(result$summary$total_objects, 3)
  expect_equal(result$summary$data_frames, 1)
  expect_equal(result$summary$other_objects, 2)

  # Should have data frame details
  expect_true("data_frames" %in% names(result))
  expect_equal(nrow(result$data_frames$structure), 1)
  expect_equal(result$data_frames$structure$name, "my_df")
})

test_that("anon_data_summary() works with environments", {
  test_env <- new.env(parent = emptyenv())
  assign("test_df", data.frame(a = 1:2, b = c("x", "y")), envir = test_env)
  assign("test_vec", c("hello", "world"), envir = test_env)

  result <- anon_data_summary(test_env)

  expect_s3_class(result, "anon_data_summary")
  expect_equal(result$summary$total_objects, 2)
  expect_equal(result$summary$data_frames, 1)
  expect_equal(result$summary$other_objects, 1)
})

test_that("anon_data_summary() handles empty environments", {
  empty_list <- list()
  result <- anon_data_summary(empty_list)

  expect_s3_class(result, "anon_data_summary")
  expect_equal(result$summary$total_objects, 0)
  expect_equal(result$summary$data_frames, 0)
  expect_equal(result$summary$other_objects, 0)
  expect_false("data_frames" %in% names(result))
})

test_that("anon_data_summary() handles objects with labels", {
  df <- data.frame(
    patient_id = c("P001", "P002"),
    treatment_ABC = c("A", "B")
  )

  # Add labels
  attr(df, "label") <- "Patient ABC study data"
  attr(df$patient_id, "label") <- "Patient identifier ABC"
  attr(df$treatment_ABC, "label") <- "ABC treatment arm"

  env_list <- list(labeled_data = df)
  result <- anon_data_summary(
    env_list,
    pattern_list = list("STUDY" = "ABC")
  )

  # Labels should be present and potentially anonymized
  labeled_data_vars <- result$data_frames$variables$labeled_data
  expect_true("label" %in% names(labeled_data_vars))
  expect_s3_class(result, "anon_data_summary")

  # Check that some labels contain the anonymized pattern
  labels <- labeled_data_vars$label
  anonymized_labels <- labels[!is.na(labels)]
  expect_true(any(grepl("STUDY", anonymized_labels)))
})

test_that("anon_data_summary() handles missing values correctly", {
  df <- data.frame(
    complete_col = c(1, 2, 3),
    partial_col = c("A", NA, "C"),
    mostly_missing = c(NA, NA, "X")
  )

  env_list <- list(missing_data = df)
  result <- anon_data_summary(env_list)

  variables <- result$data_frames$variables$missing_data

  # Check missing value calculations
  complete_var <- variables[variables$variable == "complete_col", ]
  expect_equal(complete_var$n_missing, 0)
  expect_equal(complete_var$pct_missing, 0)

  partial_var <- variables[variables$variable == "partial_col", ]
  expect_equal(partial_var$n_missing, 1)
  expect_equal(partial_var$pct_missing, 33.33)

  mostly_missing_var <- variables[variables$variable == "mostly_missing", ]
  expect_equal(mostly_missing_var$n_missing, 2)
  expect_equal(mostly_missing_var$pct_missing, 66.67)
})

test_that("anon_data_summary() handles unnamed list elements", {
  df1 <- data.frame(x = 1:2)
  df2 <- data.frame(y = 3:4)
  vec1 <- c("a", "b")

  # Create list with some unnamed elements
  mixed_list <- list(named_df = df1, df2, vec1)
  result <- anon_data_summary(mixed_list)

  expect_s3_class(result, "anon_data_summary")
  expect_equal(result$summary$total_objects, 3)
  expect_equal(result$summary$data_frames, 2)  # df1 and df2
  expect_equal(result$summary$other_objects, 1)  # vec1
})

test_that("anon_data_summary() works with different data types", {
  df <- data.frame(
    char_col = c("text1", "text2"),
    num_col = c(1.5, 2.5),
    int_col = c(1L, 2L),
    lgl_col = c(TRUE, FALSE),
    factor_col = factor(c("A", "B"))
  )

  env_list <- list(typed_data = df)
  result <- anon_data_summary(env_list)

  variables <- result$data_frames$variables$typed_data

  expect_equal(nrow(variables), 5)
  expect_true("character" %in% variables$data_type)
  expect_true("numeric" %in% variables$data_type)
  expect_true("integer" %in% variables$data_type)
  expect_true("logical" %in% variables$data_type)
  expect_true("factor" %in% variables$data_type)
})

test_that("anon_data_summary() print method works", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(95, 87))
  lst <- list(numbers = 1:10, letters = letters)
  fn <- function(x) x
  env_list <- list(test_data = df, test_list = lst, test_fn = fn)
  result <- anon_data_summary(env_list)

  # Test that print method doesn't error
  expect_output(print(result), "Environment Data Summary")
  expect_output(print(result), "Data Frames:")
  expect_output(print(result), "Variable Details.*:")
  expect_output(print(result), "Other Objects:")
})

test_that("anon_data_summary() handles check_approximate parameter", {
  df <- data.frame(
    Alice = 1:3,
    Alise = 4:6
  )

  env_list <- list(study = df)

  # With approximate checking enabled, should warn about potential matches
  expect_warning(
    result <- anon_data_summary(
      env_list,
      pattern_list = list("PERSON" = "Alice"),
      check_approximate = TRUE,
      max_distance = 2
    ),
    "Potential approximate match.*Alise.*similar to pattern.*Alice"
  )

  # With approximate checking disabled, should not warn
  expect_silent(
    result2 <- anon_data_summary(
      env_list,
      pattern_list = list("PERSON" = "Alice"),
      check_approximate = FALSE
    )
  )

  expect_s3_class(result, "anon_data_summary")
  expect_s3_class(result2, "anon_data_summary")
})

test_that("anon_data_summary() handles custom default_replacement", {
  df <- data.frame(sensitive = c("secret1", "secret2"))
  env_list <- list(data = df)

  result <- anon_data_summary(
    env_list,
    pattern_list = c("secret", "sensitive"),
    default_replacement = "[HIDDEN]"
  )

  expect_s3_class(result, "anon_data_summary")
})


test_that("anon_example_rows() defaults to random selection and builds a reusable example_rows spec", {
  expect_equal(anon_example_rows(n = 2)$method, "random")

  spec <- anon_example_rows(
    n = 7,
    key = "USUBJID",
    method = "random",
    value = "01-701-1015",
    n_key_values = 1,
    seed = 42
  )

  expect_s3_class(spec, "anon_example_rows_spec")
  expect_equal(
    unclass(spec),
    list(
      n = 7,
      key = "USUBJID",
      method = "random",
      value = "01-701-1015",
      n_key_values = 1L,
      seed = 42L
    )
  )
})


test_that("anon_data_summary() includes example values and accepts numeric example_rows", {
  df <- data.frame(
    char_col = c("alpha", "beta", "alpha", NA),
    num_col = c(1, 2, 3, 4),
    lgl_col = c(TRUE, FALSE, TRUE, NA),
    factor_col = factor(c("low", "high", "low", "medium"))
  )

  result <- anon_data_summary(
    list(typed_data = df),
    example_values_n = 2,
    example_rows = 2
  )

  variables <- result$data_frames$variables$typed_data
  expect_true("example_values" %in% names(variables))

  expect_equal(
    variables$example_values[[match("char_col", variables$variable)]],
    c("alpha", "beta")
  )
  expect_equal(
    variables$example_values[[match("num_col", variables$variable)]],
    character(0)
  )
  expect_equal(
    variables$example_values[[match("lgl_col", variables$variable)]],
    c("TRUE", "FALSE")
  )

  expect_equal(nrow(result$examples$rows$typed_data), 2)
})

test_that("anon_data_summary() builds keyed scenarios with configurable methods", {
  adsl <- data.frame(
    USUBJID = c("01", "02", "03"),
    SEX = c("F", "M", "F")
  )
  labs <- data.frame(
    USUBJID = c("01", "01", "02", "03", "03"),
    PARAM = c("ALT", "AST", "ALT", "ALT", "AST"),
    AVAL = c(10, 20, 25, 30, 40)
  )
  visit <- data.frame(VISIT = c("SCREEN", "DAY 1"))

  last_result <- anon_data_summary(
    list(adsl = adsl, labs = labs, visit = visit),
    example_rows = anon_example_rows(n = 20, key = "USUBJID", method = "last")
  )

  expect_equal(last_result$examples$scenario$key, "USUBJID")
  expect_equal(as.character(last_result$examples$scenario$value), "03")
  expect_equal(last_result$examples$scenario$method, "last")
  expect_equal(names(last_result$examples$scenario$tables), c("adsl", "labs"))
  expect_equal(nrow(last_result$examples$scenario$tables$adsl), 1)
  expect_equal(nrow(last_result$examples$scenario$tables$labs), 2)

  multi_result <- anon_data_summary(
    list(adsl = adsl, labs = labs, visit = visit),
    example_rows = anon_example_rows(n = 20, key = "USUBJID", method = "first", n_key_values = 2)
  )

  expect_equal(length(multi_result$examples$scenarios), 2)
  expect_equal(vapply(multi_result$examples$scenarios, `[[`, character(1), "value"), c("01", "02"))

  explicit_result <- anon_data_summary(
    list(adsl = adsl, labs = labs),
    example_rows = anon_example_rows(n = 20, key = "USUBJID", value = "01", n_key_values = 3)
  )

  expect_equal(as.character(explicit_result$examples$scenario$value), "01")
  expect_equal(length(explicit_result$examples$scenarios), 1)
})

test_that("anon_data_summary() anonymizes example payloads", {
  adsl <- data.frame(
    USUBJID = c("SUBJ001", "SUBJ002"),
    name = c("Alice", "Bob"),
    flag = c(TRUE, FALSE)
  )
  detail <- data.frame(
    USUBJID = c("SUBJ001", "SUBJ001"),
    note = c("Alice visit", "Alice follow-up")
  )

  result <- anon_data_summary(
    list(adsl = adsl, detail = detail),
    pattern_list = list("PERSON" = "Alice", "SUBJECT" = "SUBJ001"),
    example_values_n = 1,
    example_rows = anon_example_rows(n = 5, key = "USUBJID", method = "first")
  )

  variables <- result$data_frames$variables$adsl
  expect_equal(
    variables$example_values[[match("name", variables$variable)]],
    "PERSON"
  )
  expect_equal(as.character(result$examples$scenario$value), "SUBJECT")
  expect_true(any(grepl("PERSON", result$examples$scenario$tables$detail$note)))
  expect_false(any(grepl("Alice", result$examples$scenario$tables$detail$note)))
  expect_output(print(result), "Example Scenario")
})


test_that("anon_data_summary() supports non-first row selection methods", {
  df <- data.frame(
    id = 1:5,
    label = paste0("row", 1:5)
  )

  last_result <- anon_data_summary(
    list(study = df),
    example_rows = anon_example_rows(n = 2, method = "last")
  )
  expect_equal(last_result$examples$row_method, "last")
  expect_equal(last_result$examples$rows$study$id, c(4L, 5L))

  random_a <- anon_data_summary(
    list(study = df),
    example_rows = anon_example_rows(n = 2, method = "random", seed = 11)
  )
  random_b <- anon_data_summary(
    list(study = df),
    example_rows = anon_example_rows(n = 2, method = "random", seed = 11)
  )

  expect_equal(random_a$examples$rows$study, random_b$examples$rows$study)
})

test_that("anon_data_summary() combines pattern_list with the anon.pattern_list option", {
  old <- options(anon.pattern_list = list("[SITE]" = "Boston"))
  on.exit(options(old), add = TRUE)
  summary <- anon_data_summary(
    list(sites = data.frame(city = "Boston", owner = "Alice")),
    pattern_list = list("[PERSON]" = "Alice"),
    example_values_n = 1
  )
  values <- unlist(summary$data_frames$variables$sites$example_values)
  expect_setequal(values, c("[SITE]", "[PERSON]"))
})

test_that("normalize_example_values_n() rejects invalid values", {
  expect_error(normalize_example_values_n(c(1, 2)), "single non-negative number")
  expect_error(normalize_example_values_n("a"), "single non-negative number")
  expect_error(normalize_example_values_n(-1), "single non-negative number")
  expect_identical(normalize_example_values_n(2), 2L)
})

test_that("normalize_example_rows_spec() validates each field", {
  expect_null(normalize_example_rows_spec(NULL))
  expect_null(normalize_example_rows_spec(FALSE))
  expect_null(normalize_example_rows_spec(0))
  expect_equal(normalize_example_rows_spec(3)$n, 3L)

  expect_error(normalize_example_rows_spec("a"), "NULL, a single number, or a list")
  expect_error(normalize_example_rows_spec(list(n = "a")), "example_rows\\$n")
  expect_error(normalize_example_rows_spec(list(n = -1)), "example_rows\\$n")
  expect_error(normalize_example_rows_spec(list(n = 1, key = "")), "example_rows\\$key")
  expect_error(normalize_example_rows_spec(list(n = 1, method = NA_character_)), "example_rows\\$method")
  expect_error(normalize_example_rows_spec(list(n = 1, value = "01")), "requires `example_rows\\$key`")
  expect_error(
    normalize_example_rows_spec(list(n = 1, key = "ID", value = c("01", "02"))),
    "single value"
  )
  expect_error(
    normalize_example_rows_spec(list(n = 1, key = "ID", n_key_values = "a")),
    "n_key_values"
  )
  expect_error(
    normalize_example_rows_spec(list(n = 1, key = "ID", n_key_values = 0)),
    "n_key_values"
  )
  expect_error(normalize_example_rows_spec(list(n = 1, seed = "a")), "example_rows\\$seed")

  spec <- normalize_example_rows_spec(list(n = 2, key = "ID", value = "01", n_key_values = 5, seed = 3.7))
  expect_equal(spec$n_key_values, 1L)
  expect_identical(spec$seed, 3L)
})

test_that("get_example_values() returns nothing for unsupported or all-missing input", {
  expect_equal(get_example_values(c("a", "b"), n = 0), character(0))
  expect_equal(get_example_values(1:3, n = 2), character(0))
  expect_equal(get_example_values(c(NA_character_, NA_character_), n = 2), character(0))
  expect_equal(get_example_values(c("a", "a", "b", "c"), n = 2), c("a", "b"))
})

test_that("example payload helpers return NULL when there is nothing to show", {
  rows <- anon_example_rows(n = 2, method = "first")
  keyed <- anon_example_rows(n = 2, key = "ID", method = "first")

  expect_null(build_example_payload(list(x = 1:3), rows))
  expect_null(build_example_payload(list(df = data.frame(a = integer(0))), rows))
  expect_null(build_example_payload(list(df = data.frame(a = 1)), keyed))
  expect_null(build_example_payload(list(df = data.frame(ID = NA_character_)), keyed))

  expect_null(build_single_scenario(list(df = data.frame(ID = "01")), "ID", "99", n = 1))
  expect_null(build_example_payload(
    list(df = data.frame(ID = "01")),
    anon_example_rows(n = 1, key = "ID", value = "99")
  ))
  expect_equal(nrow(select_matching_rows(data.frame(ID = "01"), "ID", "99", n = 1)), 0)
  expect_equal(nrow(select_example_rows(data.frame(a = 1:3), n = 0)), 0)
  expect_equal(select_example_indices(5, n = 2, method = "last"), 4:5)
})

test_that("sample_int_with_seed() restores the global RNG state", {
  set.seed(1)
  before <- .Random.seed
  first <- sample_int_with_seed(10, 3, seed = 42)
  expect_identical(.Random.seed, before)
  expect_equal(sample_int_with_seed(10, 3, seed = 42), first)

  saved <- .Random.seed
  on.exit(assign(".Random.seed", saved, envir = .GlobalEnv), add = TRUE)
  rm(".Random.seed", envir = .GlobalEnv)
  sample_int_with_seed(10, 3, seed = 42)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("get_example_scenarios() handles NULL, single and multiple scenarios", {
  expect_equal(get_example_scenarios(NULL), list())
  expect_equal(get_example_scenarios(list()), list())
  expect_equal(get_example_scenarios(list(scenario = "s")), list("s"))
  expect_equal(get_example_scenarios(list(scenarios = list("a", "b"))), list("a", "b"))
})

test_that("print.anon_data_summary() shows example rows and multiple scenarios", {
  dm <- data.frame(USUBJID = c("01", "02", "03"), AGE = c(30, 40, 50))

  summary <- anon_data_summary(
    list(dm = dm),
    example_rows = anon_example_rows(n = 2, method = "first")
  )
  output <- paste(capture.output(print(summary)), collapse = "\n")
  expect_match(output, "Example Rows (dm):", fixed = TRUE)

  summary <- anon_data_summary(
    list(dm = dm),
    example_rows = anon_example_rows(n = 1, key = "USUBJID", method = "first", n_key_values = 2)
  )
  output <- paste(capture.output(print(summary)), collapse = "\n")
  expect_match(output, "Example Scenario 1 (USUBJID = 01):", fixed = TRUE)
  expect_match(output, "Example Scenario 2 (USUBJID = 02):", fixed = TRUE)
})

test_that("anon_data_summary() applies column rules to example values and rows", {
  dm <- data.frame(
    USUBJID = c("01-701-1015", "01-701-1023"),
    BRTHDTC = c("1950-12-26", "1948-07-22"),
    AGE = c(63, 64),
    stringsAsFactors = FALSE
  )
  vs <- data.frame(USUBJID = c("01-701-1015", "01-701-1015"), VSORRES = c("64", "83"))
  key <- c("01-701-1015" = "SUBJ-1", "01-701-1023" = "SUBJ-2")

  summary <- anon_data_summary(
    list(dm = dm, vs = vs),
    df_variable_names = list(USUBJID = ~ unname(key[.x]), BRTHDTC = "[DATE]"),
    df_classes = list(numeric = ~ .x * 0),
    example_values_n = 2,
    example_rows = anon_example_rows(n = 2, key = "USUBJID", method = "first")
  )

  shared <- paste(capture.output(print(summary)), collapse = "\n")
  expect_false(grepl("01-701-", shared, fixed = TRUE))
  expect_false(grepl("1950-12-26", shared, fixed = TRUE))

  variables <- summary$data_frames$variables$dm
  expect_equal(variables$example_values[[1]], c("SUBJ-1", "SUBJ-2"))
  expect_equal(variables$example_values[[2]], "[DATE]")
  # Structure statistics describe the source data
  expect_equal(variables$n_distinct[variables$variable == "BRTHDTC"], 2)
  expect_equal(variables$data_type[variables$variable == "AGE"], "numeric")

  scenario <- summary$examples$scenario
  expect_equal(scenario$value, "SUBJ-1")
  expect_equal(scenario$tables$dm$USUBJID, "SUBJ-1")
  expect_equal(scenario$tables$dm$AGE, 0)
  expect_equal(scenario$tables$vs$USUBJID, c("SUBJ-1", "SUBJ-1"))
})

test_that("anon_data_summary() selects example rows on source values", {
  df <- data.frame(ID = c("a", "b", "c"), X = c("1", "2", "3"))
  key <- c(a = "P1", b = "P2", c = "P3")

  summary <- anon_data_summary(
    list(df = df),
    df_variable_names = list(ID = ~ unname(key[.x])),
    example_rows = anon_example_rows(n = 1, key = "ID", value = "b")
  )
  expect_equal(summary$examples$scenario$value, "P2")
  expect_equal(summary$examples$scenario$tables$df$X, "2")

  summary <- anon_data_summary(
    list(df = df),
    df_variable_names = list(ID = ~ unname(key[.x])),
    example_rows = anon_example_rows(n = 2, method = "last")
  )
  expect_equal(summary$examples$rows$df$ID, c("P2", "P3"))
})

test_that("anon_data_summary() and anon_report() use global column rules", {
  old <- options(
    anon.df_variable_names = list(ID = "[ID]"),
    anon.df_classes = "integer"
  )
  on.exit(options(old), add = TRUE)

  objects <- list(df = data.frame(ID = c("a", "b"), N = 1:2))
  summary <- anon_data_summary(objects, example_values_n = 2)
  expect_equal(summary$data_frames$variables$df$example_values[[1]], "[ID]")

  report <- anon_report(
    objects,
    example_rows = anon_example_rows(n = 1, method = "first"),
    df_variable_names = list(N = "[N]")
  )
  expect_equal(report$data_summary$examples$rows$df$ID, "[ID]")
  expect_equal(report$data_summary$examples$rows$df$N, "[N]")
})
