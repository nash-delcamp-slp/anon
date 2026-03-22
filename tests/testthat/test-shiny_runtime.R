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

test_that("anon_app_server uses raw object names for selection and UI NLP settings for reports", {
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
      check_approximate,
      nlp_auto
    ) {
      report_call$selection <- selection
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
      session$setInputs(selected_objects = "Alice")
      session$setInputs(generate_report = 1)
      session$flushReact()

      expect_equal(report_call$selection, "Alice")
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

test_that("ensure_shiny_runtime_packages() succeeds when runtime packages are present", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  expect_true(ensure_shiny_runtime_packages())
})
