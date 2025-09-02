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

test_that("anon_data_summary() works with globalenv()", {
  # Clean up any existing variables first
  rm(list = ls(envir = globalenv()), envir = globalenv())

  # Create test data in globalenv
  assign("test_df", data.frame(a = 1:2, b = c("x", "y")), envir = globalenv())
  assign("test_vec", c("hello", "world"), envir = globalenv())

  result <- anon_data_summary()

  expect_s3_class(result, "anon_data_summary")
  expect_equal(result$summary$total_objects, 2)
  expect_equal(result$summary$data_frames, 1)
  expect_equal(result$summary$other_objects, 1)

  # Clean up
  rm("test_df", "test_vec", envir = globalenv())
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
