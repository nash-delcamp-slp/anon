test_that("anon_runtime_defaults() reflects anon options", {
  old <- options(
    anon.default_replacement = "[HIDDEN]",
    anon.pattern_list = list("MASK" = c("Alice", "Bob"), "secret phrase"),
    anon.nlp_auto = nlp_auto(person = TRUE, org = TRUE),
    anon.nlp_default_replacements = nlp_default_replacements(person = "[NAME]")
  )
  on.exit(options(old), add = TRUE)

  defaults <- anon_runtime_defaults()

  expect_equal(defaults$default_replacement, "[HIDDEN]")
  expect_true(defaults$enable_nlp)
  expect_setequal(defaults$nlp_entity_types, c("PERSON", "ORG"))
  expect_match(defaults$pattern_rules_text, "MASK = Alice | Bob", fixed = TRUE)
  expect_match(defaults$pattern_rules_text, "secret phrase", fixed = TRUE)
})

test_that("anon_app_initial_state() selects all objects by default", {
  state <- anon_app_initial_state(list(study = 1:2, notes = "a"))

  expect_equal(state$selected, c("study", "notes"))
  expect_equal(unname(state$choices), c("study", "notes"))
})

test_that("resolve_report_selection() defaults to all objects on startup", {
  expect_equal(
    resolve_report_selection(c("study", "notes")),
    c("study", "notes")
  )
  expect_equal(
    resolve_report_selection(
      raw_names = c("study", "notes"),
      initial_selection = "study"
    ),
    "study"
  )
  expect_equal(
    resolve_report_selection(
      raw_names = c("study", "notes"),
      current_selected = "notes",
      selection_initialized = TRUE
    ),
    "notes"
  )
})

test_that("anon_app_server loads environment on startup and generates a report", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  env_list <- list(
    study = data.frame(Alice = c(1, 2), score = c(1, 2)),
    notes = c("alpha", "beta")
  )

  shiny::testServer(
    app = anon_app_server(envir = env_list, initial_selection = "study"),
    {
      session$flushReact()

      expect_equal(nrow(inventory_data()), 2)
      expect_match(scan_message(), "Loaded 2 objects", fixed = TRUE)
      expect_match(output$nlp_status, "NLP", fixed = TRUE)

      session$setInputs(pattern_rules = "PERSON = Alice")
      session$setInputs(selected_objects = "study")
      session$setInputs(generate_report = 1)
      session$flushReact()

      expect_s3_class(report_data(), "anon_report")
      expect_equal(report_data()$data_summary$summary$total_objects, 1)
      expect_match(output$report_preview, "PERSON", fixed = TRUE)
    }
  )
})

test_that("anon_app_server honors option defaults for pattern rules and replacement", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  old <- options(
    anon.default_replacement = "[HIDDEN]",
    anon.pattern_list = list("MASK" = "Alpha", "Beta")
  )
  on.exit(options(old), add = TRUE)

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(source_text = "Alpha Beta")
      session$setInputs(apply_text_tools = 1)
      session$flushReact()

      expect_equal(cleaned_text_data(), "MASK [HIDDEN]")
    }
  )
})

test_that("anon_app_server cleans text and assembles a prompt bundle", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(pattern_rules = "PERSON = Alice | Bob")
      session$setInputs(source_text = " Alice   met  Bob ")
      session$setInputs(apply_text_tools = 1)
      session$flushReact()

      expect_equal(cleaned_text_data(), "PERSON met PERSON")
      expect_equal(comparison_data()$summary$changed_lines, 1)
      expect_match(output$comparison_preview, "Line-level details", fixed = TRUE)
      expect_match(output$prompt_bundle_preview, "Redacted Text", fixed = TRUE)
      expect_match(output$prompt_bundle_preview, "PERSON met PERSON", fixed = TRUE)
      expect_match(output$prompt_bundle_json_preview, '"kind": "anon_prompt_bundle"', fixed = TRUE)
      expect_match(output$prompt_bundle_json_preview, '"text": "PERSON met PERSON"', fixed = TRUE)
    }
  )
})

test_that("anon_app_server reports unavailable NLP state without changing text", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  local_mocked_bindings(
    get_nlp_runtime_status = function() {
      list(available = FALSE, message = "mock unavailable")
    }
  )

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      expect_match(output$nlp_status, "mock unavailable", fixed = TRUE)

      session$setInputs(enable_nlp = TRUE)
      session$setInputs(source_text = "Alice met Bob")
      session$setInputs(apply_text_tools = 1)
      session$flushReact()

      expect_equal(cleaned_text_data(), "Alice met Bob")
    }
  )
})

test_that("anon_app_server uses option-driven NLP defaults when available", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  nlp_call <- new.env(parent = emptyenv())
  old <- options(
    anon.default_replacement = "[HIDDEN]",
    anon.nlp_auto = nlp_auto(person = TRUE),
    anon.nlp_default_replacements = nlp_default_replacements(person = "[NAME]")
  )
  on.exit(options(old), add = TRUE)

  local_mocked_bindings(
    get_nlp_runtime_status = function() {
      list(available = TRUE, message = "mock ready")
    },
    anon_nlp_entities = function(x, entity_types, default_replacement, check_approximate) {
      nlp_call$default_missing <- missing(default_replacement)
      nlp_call$entity_types <- entity_types
      "[NAME] met [NAME]"
    }
  )

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(source_text = "John met Mary")
      session$setInputs(apply_text_tools = 1)
      session$flushReact()

      expect_true(isTRUE(nlp_call$default_missing))
      expect_equal(nlp_call$entity_types, "PERSON")
      expect_equal(cleaned_text_data(), "[NAME] met [NAME]")
    }
  )
})

test_that("anon_app_server passes explicit NLP replacement override from the UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  nlp_call <- new.env(parent = emptyenv())
  old <- options(
    anon.default_replacement = "[HIDDEN]",
    anon.nlp_auto = nlp_auto(person = TRUE),
    anon.nlp_default_replacements = nlp_default_replacements(person = "[NAME]")
  )
  on.exit(options(old), add = TRUE)

  local_mocked_bindings(
    get_nlp_runtime_status = function() {
      list(available = TRUE, message = "mock ready")
    },
    anon_nlp_entities = function(x, entity_types, default_replacement, check_approximate) {
      nlp_call$default_missing <- missing(default_replacement)
      nlp_call$default_replacement <- default_replacement
      "[ENTITY] met [ENTITY]"
    }
  )

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(default_replacement = "[ENTITY]")
      session$setInputs(source_text = "John met Mary")
      session$setInputs(apply_text_tools = 1)
      session$flushReact()

      expect_false(isTRUE(nlp_call$default_missing))
      expect_equal(nlp_call$default_replacement, "[ENTITY]")
      expect_equal(cleaned_text_data(), "[ENTITY] met [ENTITY]")
    }
  )
})

test_that("anon_app_server uses raw object names and modal summary options for reports", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  report_call <- new.env(parent = emptyenv())
  old <- options(anon.nlp_auto = nlp_auto(default = TRUE))
  on.exit(options(old), add = TRUE)

  local_mocked_bindings(
    anon_report = function(
      envir,
      selection,
      pattern_list,
      default_replacement,
      example_values_n,
      example_rows,
      check_approximate = FALSE,
      nlp_auto
    ) {
      report_call$selection <- selection
      report_call$example_values_n <- example_values_n
      report_call$example_rows <- example_rows
      report_call$nlp_auto <- nlp_auto

      structure(
        list(
          inventory = tibble::tibble(name = "[PERSON]"),
          data_summary = structure(
            list(summary = tibble::tibble(
              total_objects = 1,
              data_frames = 1,
              other_objects = 0,
              total_memory = "1 Kb"
            )),
            class = c("anon_data_summary", "anon_context", "list")
          )
        ),
        class = c("anon_report", "anon_context", "list")
      )
    }
  )

  shiny::testServer(
    app = anon_app_server(envir = list(Alice = data.frame(x = 1:2))),
    {
      session$flushReact()
      session$setInputs(pattern_rules = "PERSON = Alice")
      session$setInputs(nlp_entity_types = c("PERSON", "ORG"))
      session$setInputs(data_summary_example_values_n = 2)
      session$setInputs(data_summary_example_rows_n = 5)
      session$setInputs(data_summary_example_rows_key = "USUBJID")
      session$setInputs(data_summary_example_rows_method = "random")
      session$setInputs(data_summary_example_rows_value = "01")
      session$setInputs(data_summary_example_rows_n_key_values = 2)
      session$setInputs(data_summary_example_rows_seed = "17")
      session$setInputs(save_data_summary_options = 1)
      session$setInputs(selected_objects = "Alice")
      session$setInputs(generate_report = 1)
      session$flushReact()

      expect_equal(report_call$selection, "Alice")
      expect_equal(report_call$example_values_n, 2)
      expect_equal(
        report_call$example_rows,
        anon_example_rows(
          n = 5,
          key = "USUBJID",
          method = "random",
          value = "01",
          n_key_values = 1,
          seed = 17
        )
      )
      expect_equal(report_call$nlp_auto, list(person = TRUE, org = TRUE))
    }
  )
})

test_that("anon_app_server allows clearing all NLP entity types", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  nlp_call <- new.env(parent = emptyenv())
  old <- options(anon.nlp_auto = nlp_auto(default = TRUE))
  on.exit(options(old), add = TRUE)

  local_mocked_bindings(
    get_nlp_runtime_status = function() {
      list(available = TRUE, message = "mock ready")
    },
    anon_nlp_entities = function(x, entity_types, default_replacement, check_approximate) {
      nlp_call$called <- TRUE
      x
    }
  )

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(nlp_entity_types = character(0))
      session$setInputs(source_text = "John met Acme")
      session$setInputs(apply_text_tools = 1)
      session$flushReact()

      expect_false(isTRUE(nlp_call$called))
      expect_equal(cleaned_text_data(), "John met Acme")
    }
  )
})


test_that("append_uploaded_content() formats imported file blocks", {
  result <- append_uploaded_content(
    "Existing text",
    c("notes.txt" = "alpha", "book.xlsx" = "beta")
  )

  expect_equal(
    result,
    paste(
      "Existing text",
      "--- notes.txt ---\nalpha\n\n--- book.xlsx ---\nbeta",
      sep = "\n\n"
    )
  )
})

test_that("collect_excel_upload_sheet_choices() includes every uploaded sheet", {
  files <- data.frame(
    name = c("book.xlsx", "tables.xls"),
    datapath = c("/tmp/book.xlsx", "/tmp/tables.xls"),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    list_excel_file_sheets = function(path) {
      if (grepl("book", path, fixed = TRUE)) {
        c("Raw", "Derived")
      } else {
        "Listing"
      }
    }
  )

  choice_data <- collect_excel_upload_sheet_choices(files)

  expect_equal(
    names(choice_data$choices),
    c(
      "book.xlsx :: Raw",
      "book.xlsx :: Derived",
      "tables.xls :: Listing"
    )
  )
  expect_equal(
    unname(choice_data$choices),
    c("1::Raw", "1::Derived", "2::Listing")
  )
  expect_equal(choice_data$selected, unname(choice_data$choices))
})

test_that("import_uploaded_excel_files() reads only the selected sheets", {
  files <- data.frame(
    name = c("book.xlsx", "tables.xls"),
    datapath = c("/tmp/book.xlsx", "/tmp/tables.xls"),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    read_content_xlsx = function(path, sheet = NULL) {
      paste(sheet, collapse = " | ")
    }
  )

  result <- import_uploaded_excel_files(
    files,
    c("1::Raw", "1::Derived", "2::Listing")
  )

  expect_equal(names(result), c("book.xlsx", "tables.xls"))
  expect_equal(unname(result), c("Raw | Derived", "Listing"))
})

test_that("split_uploaded_files() separates Excel uploads from other files", {
  files <- data.frame(
    name = c("book.xlsx", "notes.txt", "tables.xls", "report.pdf"),
    datapath = c("/tmp/book.xlsx", "/tmp/notes.txt", "/tmp/tables.xls", "/tmp/report.pdf"),
    stringsAsFactors = FALSE
  )

  split_files <- split_uploaded_files(files)

  expect_equal(split_files$excel$name, c("book.xlsx", "tables.xls"))
  expect_equal(split_files$other$name, c("notes.txt", "report.pdf"))
})

test_that("ensure_shiny_runtime_packages() succeeds when runtime packages are present", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  expect_true(ensure_shiny_runtime_packages())
})


test_that("data summary option helpers default to random row selection", {
  expect_equal(default_data_summary_options()$example_rows_method, "random")
})

test_that("data summary option helpers parse and build modal settings", {
  parsed <- parse_data_summary_options_input(list(
    data_summary_example_values_n = 3,
    data_summary_example_rows_n = 4,
    data_summary_example_rows_key = " USUBJID ",
    data_summary_example_rows_method = "random",
    data_summary_example_rows_value = " 01 ",
    data_summary_example_rows_n_key_values = 1,
    data_summary_example_rows_seed = "11"
  ))

  expect_equal(
    parsed,
    list(
      example_values_n = 3L,
      example_rows_n = 4L,
      example_rows_key = "USUBJID",
      example_rows_method = "random",
      example_rows_value = "01",
      example_rows_n_key_values = 1L,
      example_rows_seed = "11"
    )
  )
  expect_equal(
    build_data_summary_example_rows(parsed),
    anon_example_rows(n = 4, key = "USUBJID", method = "random", value = "01", n_key_values = 1, seed = 11)
  )
})


test_that("data summary option helpers keep n_key_values for multiple scenarios", {
  parsed <- parse_data_summary_options_input(list(
    data_summary_example_values_n = 0,
    data_summary_example_rows_n = 3,
    data_summary_example_rows_key = "USUBJID",
    data_summary_example_rows_method = "last",
    data_summary_example_rows_value = "",
    data_summary_example_rows_n_key_values = 2,
    data_summary_example_rows_seed = ""
  ))

  expect_equal(parsed$example_rows_n_key_values, 2L)
  expect_equal(
    build_data_summary_example_rows(parsed),
    anon_example_rows(n = 3, key = "USUBJID", method = "last", n_key_values = 2)
  )
})

# Records calls to Shiny UI side-effect functions so server tests can check
# what the app would show or update.
local_shiny_recorder <- function(env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$text <- list()
  calls$notifications <- list()
  calls$modals <- 0L
  calls$removed_modals <- 0L

  local_mocked_bindings(
    updateTextAreaInput = function(session, inputId, value = NULL, ...) {
      calls$text[[length(calls$text) + 1]] <- value
    },
    showNotification = function(ui, ..., type = "default") {
      calls$notifications[[length(calls$notifications) + 1]] <- list(ui = ui, type = type)
    },
    showModal = function(ui, ...) {
      force(ui)
      calls$modals <- calls$modals + 1L
    },
    removeModal = function(...) {
      calls$removed_modals <- calls$removed_modals + 1L
    },
    .package = "shiny",
    .env = env
  )

  calls
}

test_that("run_anon_app() builds the app and hands it to shiny::runApp()", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  local_mocked_bindings(
    runApp = function(appDir, launch.browser, ...) {
      list(app = appDir, launch.browser = launch.browser)
    },
    .package = "shiny"
  )
  local_mocked_bindings(
    get_nlp_runtime_status = function() list(available = FALSE, message = "mock")
  )

  result <- run_anon_app(envir = list(study = data.frame(x = 1)), launch.browser = FALSE)
  expect_s3_class(result$app, "shiny.appobj")
  expect_false(result$launch.browser)
})

test_that("anon_app_ui() renders the main controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  html <- as.character(anon_app_ui(
    initial_state = anon_app_initial_state(
      envir = list(study = data.frame(x = 1)),
      pattern_list = list(PERSON = "Alice"),
      default_replacement = "[REDACTED]"
    )
  ))
  for (id in c("scan_environment", "selected_objects", "generate_report",
               "upload_files", "source_text", "apply_text_tools",
               "copy_prompt_bundle_json", "download_prompt_bundle_json")) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }
  expect_match(html, "PERSON = Alice", fixed = TRUE)

  expect_s3_class(anon_app_ui(), "shiny.tag.list")
})

test_that("anon_app_server saves valid data summary options and reports invalid ones", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  calls <- local_shiny_recorder()

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$flushReact()
      session$setInputs(configure_data_summary = 1)
      expect_equal(calls$modals, 1L)

      session$setInputs(
        data_summary_example_values_n = 2,
        data_summary_example_rows_n = 1,
        data_summary_example_rows_key = "",
        data_summary_example_rows_method = "first",
        data_summary_example_rows_value = "01",
        data_summary_example_rows_n_key_values = 1,
        data_summary_example_rows_seed = ""
      )
      session$setInputs(save_data_summary_options = 1)
      expect_equal(calls$notifications[[1]]$type, "error")
      expect_match(calls$notifications[[1]]$ui, "requires a scenario key")
      expect_equal(calls$removed_modals, 0L)

      session$setInputs(data_summary_example_rows_value = "")
      session$setInputs(save_data_summary_options = 2)
      expect_equal(active_data_summary_options()$example_values_n, 2L)
      expect_equal(calls$removed_modals, 1L)
    }
  )
})

test_that("anon_app_server rescans the environment on request", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  local_mocked_bindings(updateSelectizeInput = function(...) NULL, .package = "shiny")

  env <- new.env()
  env$study <- data.frame(x = 1:2)

  shiny::testServer(
    app = anon_app_server(envir = env),
    {
      session$flushReact()
      expect_match(scan_message(), "Loaded 1 object from environment.", fixed = TRUE)

      assign("notes", "free text", envir = env)
      session$setInputs(scan_environment = 1)
      expect_match(scan_message(), "Loaded 2 objects", fixed = TRUE)
      expect_null(report_data())
    }
  )
})

test_that("anon_app_server imports uploaded text files and reports read errors", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  calls <- local_shiny_recorder()

  notes <- tempfile(fileext = ".txt")
  bad <- tempfile(fileext = ".docx")
  on.exit(unlink(c(notes, bad)))
  writeLines("Alice met Bob", notes)
  writeLines("not a docx", bad)

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(source_text = "Existing")
      session$setInputs(upload_files = data.frame(
        name = "notes.txt", size = 1, type = "text/plain", datapath = notes
      ))
      expect_equal(
        calls$text[[1]],
        "Existing\n\n--- notes.txt ---\nAlice met Bob"
      )

      session$setInputs(upload_files = data.frame(
        name = "bad.docx", size = 1, type = "application/octet-stream", datapath = bad
      ))
      expect_length(calls$text, 1)
      expect_equal(calls$notifications[[1]]$type, "error")
    }
  )
})

test_that("anon_app_server imports selected Excel sheets after confirmation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("readxl")
  calls <- local_shiny_recorder()
  book <- test_path("fixtures", "book.xlsx")
  upload <- data.frame(name = "book.xlsx", size = 1, type = "xlsx", datapath = book)

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(source_text = "")
      session$setInputs(upload_files = upload)
      expect_equal(calls$modals, 1L)
      expect_false(is.null(pending_excel_upload_data()))

      session$setInputs(selected_excel_sheets = "1::Raw")
      session$setInputs(confirm_excel_sheet_selection = 1)
      expect_null(pending_excel_upload_data())
      expect_match(calls$text[[1]], "^--- book.xlsx ---\n--- Sheet: Raw ---")
      expect_false(grepl("Sheet: Empty", calls$text[[1]]))

      session$setInputs(source_text = "Kept")
      session$setInputs(upload_files = upload[1, , drop = FALSE])
      session$setInputs(cancel_excel_sheet_selection = 1)
      expect_null(pending_excel_upload_data())
      expect_equal(calls$text[[2]], "Kept")
    }
  )
})

test_that("anon_app_server handles Excel uploads without sheets or with read errors", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  calls <- local_shiny_recorder()
  upload <- data.frame(name = "book.xlsx", size = 1, type = "xlsx", datapath = "unused")

  local_mocked_bindings(list_excel_file_sheets = function(path) character(0))
  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(source_text = "Kept")
      session$setInputs(upload_files = upload)
      expect_equal(calls$modals, 0L)
      expect_equal(calls$text[[1]], "Kept")
    }
  )

  local_mocked_bindings(
    list_excel_file_sheets = function(path) "Raw",
    read_content_xlsx = function(path, sheet = NULL) stop("corrupt workbook")
  )
  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(source_text = "")
      session$setInputs(upload_files = upload)
      session$setInputs(selected_excel_sheets = "1::Raw")
      session$setInputs(confirm_excel_sheet_selection = 1)
      expect_match(calls$notifications[[1]]$ui, "corrupt workbook")
      expect_false(is.null(pending_excel_upload_data()))
    }
  )

  local_mocked_bindings(list_excel_file_sheets = function(path) stop("not a workbook"))
  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(source_text = "Kept")
      session$setInputs(upload_files = upload)
      expect_match(calls$notifications[[2]]$ui, "not a workbook")
      expect_equal(calls$text[[2]], "Kept")
    }
  )
})

test_that("anon_app_server copies and downloads the prompt bundle JSON", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  calls <- local_shiny_recorder()

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$flushReact()
      session$setInputs(copy_prompt_bundle_json = 1)
      expect_equal(calls$notifications[[1]]$type, "warning")
      expect_match(output$prompt_bundle_json_preview, "Generate a report", fixed = TRUE)
      expect_error(output$download_prompt_bundle_json, "Generate a report")

      session$setInputs(source_text = "Alice")
      session$setInputs(apply_text_tools = 1)
      session$setInputs(copy_prompt_bundle_json = 2)
      expect_length(calls$notifications, 1)

      path <- output$download_prompt_bundle_json
      payload <- jsonlite::fromJSON(path)
      expect_equal(payload$kind, "anon_prompt_bundle")

      session$setInputs(copy_prompt_bundle_json_result = "success")
      expect_equal(calls$notifications[[2]]$type, "message")
      session$setInputs(copy_prompt_bundle_json_result = "failed")
      expect_equal(calls$notifications[[3]]$type, "error")
    }
  )
})

test_that("get_nlp_runtime_status() reports a missing backend or failed initialization", {
  local_mocked_bindings(py_module_available = function(...) FALSE, .package = "reticulate")
  expect_match(get_nlp_runtime_status()$message, "spaCy backend is not available")

  local_mocked_bindings(py_module_available = function(...) TRUE, .package = "reticulate")
  local_mocked_bindings(cnlp_init_spacy = function(...) stop("no model"), .package = "cleanNLP")
  status <- get_nlp_runtime_status()
  expect_false(status$available)
  expect_match(status$message, "spaCy initialization failed: no model")
})

test_that("data_summary_options_modal() shows the current settings", {
  skip_if_not_installed("shiny")
  html <- as.character(data_summary_options_modal(list(example_rows_key = "USUBJID")))
  expect_match(html, 'id="data_summary_example_rows_key"', fixed = TRUE)
  expect_match(html, "USUBJID", fixed = TRUE)
})

test_that("parse_data_summary_options_input() rejects inconsistent settings", {
  base <- list(
    data_summary_example_values_n = 0,
    data_summary_example_rows_n = 2,
    data_summary_example_rows_key = "",
    data_summary_example_rows_method = "random",
    data_summary_example_rows_value = "",
    data_summary_example_rows_n_key_values = 1,
    data_summary_example_rows_seed = ""
  )
  with <- function(...) utils::modifyList(base, list(...))

  expect_error(
    parse_data_summary_options_input(with(data_summary_example_rows_n_key_values = 2)),
    "Multiple scenario key values require a scenario key"
  )
  expect_error(
    parse_data_summary_options_input(with(
      data_summary_example_rows_method = "first",
      data_summary_example_rows_seed = "5"
    )),
    "random seed can only be used"
  )
  expect_error(
    parse_data_summary_options_input(with(data_summary_example_rows_seed = "abc")),
    "whole number"
  )
  expect_error(
    parse_data_summary_options_input(with(data_summary_example_values_n = -1)),
    "Example values must be a non-negative whole number"
  )
  expect_error(
    parse_data_summary_options_input(with(data_summary_example_values_n = "a")),
    "Example values must be a non-negative whole number"
  )
  expect_error(
    parse_data_summary_options_input(with(data_summary_example_rows_n_key_values = 0)),
    "Scenario key values must be at least 1"
  )
  expect_null(build_data_summary_example_rows(with(example_rows_n = 0)))
})

test_that("anon_app_server falls back to [REDACTED] for a blank default replacement", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  shiny::testServer(
    app = anon_app_server(envir = list(study = data.frame(x = 1:2))),
    {
      session$setInputs(default_replacement = "  ")
      expect_equal(active_default_replacement(), "[REDACTED]")
    }
  )
})

test_that("nlp_auto_from_entity_types() maps entity types to nlp_auto flags", {
  expect_false(nlp_auto_from_entity_types(NULL))
  expect_false(nlp_auto_from_entity_types(character(0)))
  expect_equal(nlp_auto_from_entity_types(c("PERSON", "DATE")), list(person = TRUE, date = TRUE))
})

test_that("import helpers handle empty selections and results", {
  expect_equal(append_uploaded_content("Kept", character(0)), "Kept")
  expect_equal(append_uploaded_content("  ", c(a.txt = "x")), "--- a.txt ---\nx")
  expect_equal(parse_excel_sheet_selection(character(0)), list())

  files <- data.frame(name = c("a.xlsx", "b.xlsx"), datapath = c("a", "b"))
  expect_equal(import_uploaded_excel_files(files, character(0)), character(0))

  local_mocked_bindings(read_content_xlsx = function(path, sheet = NULL) "content")
  expect_equal(import_uploaded_excel_files(files, "2::Sheet1"), c(b.xlsx = "content"))

  local_mocked_bindings(list_excel_file_sheets = function(path) if (path == "a") character(0) else "S")
  expect_equal(unname(collect_excel_upload_sheet_choices(files)$choices), "2::S")
})
