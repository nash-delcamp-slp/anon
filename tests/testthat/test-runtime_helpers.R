test_that("anon_inventory() summarizes objects and respects selection", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(1, 2))
  vec <- c("alpha", "beta")
  attr(df$name, "label") <- "Person name"

  inventory <- anon_inventory(list(study = df, notes = vec), selection = "study")

  expect_s3_class(inventory, "anon_inventory")
  expect_equal(nrow(inventory), 1)
  expect_equal(inventory$name, "study")
  expect_equal(inventory$kind, "data.frame")
  expect_true(inventory$has_text)
  expect_true(inventory$has_labels)
})

test_that("anon_data_summary() supports selection", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(1, 2))
  vec <- c("alpha", "beta")

  result <- anon_data_summary(list(study = df, notes = vec), selection = "notes")

  expect_equal(result$summary$total_objects, 1)
  expect_equal(result$summary$data_frames, 0)
  expect_equal(result$summary$other_objects, 1)
  expect_false("data_frames" %in% names(result))
})

test_that("anon_report() combines inventory and data summary", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(1, 2))
  notes <- c("Alice reviewed the report.")

  report <- anon_report(
    list(study = df, notes = notes),
    pattern_list = list("PERSON" = "Alice")
  )

  expect_s3_class(report, "anon_report")
  expect_true(all(c("inventory", "data_summary") %in% names(report)))
  expect_true(any(grepl("PERSON", report$inventory$name)) || identical(report$inventory$name, c("study", "notes")))
  expect_equal(report$data_summary$summary$total_objects, 2)
})

test_that("anon_compare_text() reports changed lines", {
  comparison <- anon_compare_text(
    before = c("Alice", "Bob"),
    after = c("PERSON", "Bob")
  )

  expect_equal(comparison$summary$changed_lines, 1)
  expect_equal(comparison$summary$unchanged_lines, 1)
  expect_true(comparison$details$changed[1])
  expect_false(comparison$details$changed[2])
})

test_that("anon_prompt_bundle() formats report and text", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(1, 2))
  report <- anon_report(list(study = df), pattern_list = list("PERSON" = "Alice"))
  comparison <- anon_compare_text("Alice", "PERSON")

  bundle <- anon_prompt_bundle(
    report = report,
    text = "PERSON visited the site.",
    comparison = comparison,
    format = "markdown"
  )

  expect_s3_class(bundle, "anon_prompt_bundle")
  expect_true(grepl("# Anonymized Prompt Context", bundle, fixed = TRUE))
  expect_true(grepl("## Inventory", bundle, fixed = TRUE))
  expect_true(grepl("## Redacted Text", bundle, fixed = TRUE))
  expect_true(grepl("PERSON visited the site.", bundle, fixed = TRUE))
})
