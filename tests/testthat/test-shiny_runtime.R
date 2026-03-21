test_that("anon_app_server scans environment and generates a report", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  env_list <- list(
    study = data.frame(Alice = c(1, 2), score = c(1, 2)),
    notes = c("alpha", "beta")
  )

  shiny::testServer(
    app = anon_app_server(envir = env_list, initial_selection = "study"),
    {
      expect_equal(scan_message(), "Environment not scanned yet.")

      session$setInputs(scan_environment = 1)
      session$flushReact()

      expect_equal(nrow(inventory_data()), 2)
      expect_match(scan_message(), "Scanned 2 objects", fixed = TRUE)

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

test_that("ensure_shiny_runtime_packages() succeeds when runtime packages are present", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  expect_true(ensure_shiny_runtime_packages())
})
