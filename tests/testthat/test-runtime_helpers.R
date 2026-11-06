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

test_that("as_anon_json() serializes anon_report objects", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(1, 2))
  report <- anon_report(list(study = df, notes = c("alpha", "beta")))

  payload <- jsonlite::fromJSON(as_anon_json(report), simplifyVector = FALSE)

  expect_equal(payload$schema_version, 1)
  expect_equal(payload$kind, "anon_report")
  expect_equal(payload$inventory$rows[[1]]$name, "study")
  expect_equal(payload$inventory$rows[[2]]$name, "notes")
  expect_equal(payload$data_summary$summary$rows[[1]]$total_objects, 2)
  expect_equal(payload$data_summary$data_frames$structure$rows[[1]]$name, "study")
})

test_that("as_anon_payload() exposes structured report payloads", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(1, 2))
  report <- anon_report(list(study = df, notes = c("alpha", "beta")))

  payload <- as_anon_payload(report)

  expect_equal(payload$schema_version, 1)
  expect_equal(payload$kind, "anon_report")
  expect_equal(payload$inventory$rows[[1]]$name, "study")
  expect_equal(payload$data_summary$summary$rows[[1]]$total_objects, 2)
})

test_that("anon_prompt_bundle() can emit JSON payloads", {
  df <- data.frame(name = c("Alice", "Bob"), score = c(1, 2))
  report <- anon_report(list(study = df))
  comparison <- anon_compare_text("Alice", "PERSON")

  bundle <- anon_prompt_bundle(
    report = report,
    text = "PERSON visited the site.",
    comparison = comparison,
    format = "json"
  )
  payload <- jsonlite::fromJSON(bundle, simplifyVector = FALSE)

  expect_s3_class(bundle, "anon_prompt_bundle")
  expect_equal(attr(bundle, "output_format"), "json")
  expect_equal(payload$kind, "anon_prompt_bundle")
  expect_equal(payload$title, "Anonymized Prompt Context")
  expect_equal(payload$report$inventory$rows[[1]]$name, "study")
  expect_equal(payload$text, "PERSON visited the site.")
  expect_equal(payload$comparison$summary$rows[[1]]$changed_lines, 1)
})

test_that("as_anon_json() errors on unsupported custom classes", {
  unsupported <- new_anon_context(structure(list(coefficients = 1), class = "lm"))

  expect_error(
    as_anon_json(unsupported),
    "Unsupported class for JSON serialization"
  )
})

test_that("anon_report() forwards example options, nlp_auto and column rules to anon_data_summary()", {
  summary_call <- new.env(parent = emptyenv())

  local_mocked_bindings(
    anon_data_summary = function(
      envir,
      selection = NULL,
      pattern_list = list(),
      default_replacement = "[REDACTED]",
      example_values_n = 0,
      example_rows = NULL,
      check_approximate = FALSE,
      max_distance = 2,
      nlp_auto = NULL,
      df_variable_names = NULL,
      df_classes = NULL
    ) {
      summary_call$df_variable_names <- df_variable_names
      summary_call$df_classes <- df_classes
      summary_call$example_values_n <- example_values_n
      summary_call$example_rows <- example_rows
      summary_call$nlp_auto <- nlp_auto
      structure(
        list(summary = tibble::tibble(
          total_objects = 1,
          data_frames = 0,
          other_objects = 1,
          total_memory = "1 Kb"
        )),
        class = c("anon_data_summary", "anon_context", "list")
      )
    }
  )

  spec <- anon_example_rows(n = 5, key = "USUBJID", method = "random", seed = 9)

  report <- anon_report(
    list(study = "Alice"),
    example_values_n = 2,
    example_rows = spec,
    nlp_auto = list(person = TRUE, org = TRUE),
    df_variable_names = list(ID = "[ID]"),
    df_classes = "Date"
  )

  expect_s3_class(report, "anon_report")
  expect_equal(summary_call$df_variable_names, list(ID = "[ID]"))
  expect_equal(summary_call$df_classes, "Date")
  expect_equal(summary_call$example_values_n, 2)
  expect_equal(summary_call$example_rows, spec)
  expect_equal(summary_call$nlp_auto, list(person = TRUE, org = TRUE))
})


test_that("anon_prompt_bundle() includes example sections from report summaries", {
  df <- data.frame(name = c("Alice", "Bob"), flag = c(TRUE, FALSE))
  report <- anon_report(
    list(study = df),
    pattern_list = list("PERSON" = "Alice"),
    example_values_n = 1,
    example_rows = 1
  )

  bundle <- anon_prompt_bundle(
    report = report,
    format = "text",
    include_text = FALSE,
    include_comparison = FALSE
  )

  expect_match(bundle, "Example Rows \\(study\\)")
  expect_match(bundle, "example_values", fixed = TRUE)
})

test_that("normalize_object_source() rejects bad input and unknown selections", {
  expect_error(normalize_object_source(1), "must be an environment or a list")
  expect_error(
    normalize_object_source(list(a = 1), selection = c("a", "zz")),
    "Unknown objects in `selection`: zz"
  )
})

test_that("anon_inventory() classifies object kinds, text and labels", {
  labelled <- c("a", "b")
  attr(labelled, "label") <- "Letters"
  labelled_df <- data.frame(x = 1:2)
  attr(labelled_df$x, "label") <- "Count"

  objects <- list(
    fn = function(x) x,
    lst = list(note = "text", n = 1),
    nums = list(1, 2),
    vec = c("a", "b"),
    labelled = labelled,
    df = labelled_df,
    nested = list(inner = labelled_df),
    env = new.env()
  )
  inventory <- anon_inventory(objects)
  row <- function(name) inventory[inventory$name == name, ]

  expect_equal(row("fn")$kind, "function")
  expect_equal(row("lst")$kind, "list")
  expect_equal(row("vec")$kind, "vector")
  expect_equal(row("env")$kind, "environment")
  expect_true(row("lst")$has_text)
  expect_false(row("nums")$has_text)
  expect_true(row("labelled")$has_labels)
  expect_true(row("df")$has_labels)
  expect_true(row("nested")$has_labels)
  expect_false(row("nums")$has_labels)
})

test_that("format_list_cell() formats empty, data frame, list and scalar cells", {
  expect_equal(format_list_cell(NULL), "")
  expect_equal(format_list_cell(character(0)), "")
  expect_match(format_list_cell(data.frame(a = 1)), "a \\|\\s+1")
  expect_equal(format_list_cell(list("a", NA)), "a | NA")
  expect_equal(format_list_cell(c("x", "y")), "x | y")
  expect_equal(format_example_scalar(NULL), "")
})

test_that("format_anon_data_summary_text() includes scenarios and other objects", {
  dm <- data.frame(USUBJID = c("01", "02"), AGE = c(30, 40))
  summary <- anon_data_summary(
    list(dm = dm, notes = "free text"),
    example_rows = anon_example_rows(n = 1, key = "USUBJID", method = "first")
  )
  text <- format_anon_data_summary_text(summary)
  expect_match(text, "Example Scenario (USUBJID = 01)", fixed = TRUE)
  expect_match(text, "Other Objects", fixed = TRUE)

  summary <- anon_data_summary(
    list(dm = dm),
    example_rows = anon_example_rows(n = 1, key = "USUBJID", method = "first", n_key_values = 2)
  )
  text <- format_anon_data_summary_text(summary)
  expect_match(text, "Example Scenario 2 (USUBJID = 02)", fixed = TRUE)
})

test_that("anon_inventory() handles POSIXlt objects", {
  lt <- as.POSIXlt(c("2021-03-14 08:00:00", "2021-03-15 09:00:00"), tz = "UTC")
  inventory <- anon_inventory(list(t = lt, nested = list(t = lt)))
  expect_equal(inventory$kind, c("POSIXlt", "list"))
  expect_equal(inventory$has_text, c(FALSE, FALSE))
  expect_equal(inventory$has_labels, c(FALSE, FALSE))
})

test_that("anon_inventory() keeps its columns when there are no objects", {
  inventory <- anon_inventory(list())
  expect_equal(nrow(inventory), 0)
  expect_named(inventory, names(anon_inventory(list(a = 1))))

  state <- expect_no_warning(anon_app_initial_state(
    envir = list(),
    pattern_list = list(),
    default_replacement = "[REDACTED]"
  ))
  expect_equal(nrow(state$inventory), 0)
})
