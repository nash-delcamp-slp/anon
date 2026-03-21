#' Launch the local `anon` Shiny runtime
#'
#' The Shiny runtime provides a local, session-oriented workflow for scanning an
#' environment, selecting objects, cleaning text, and generating anonymized
#' structural reports suitable for prompt building.
#'
#' @param envir Environment or list to inspect. Defaults to `globalenv()`.
#' @param launch.browser Passed to [shiny::runApp()]. Defaults to `interactive()`.
#' @param initial_selection Optional character vector of object names to preselect
#'   after a scan.
#'
#' @return Invisibly returns the running Shiny app object.
#'
#' @export
run_anon_app <- function(
  envir = globalenv(),
  launch.browser = interactive(),
  initial_selection = NULL
) {
  ensure_shiny_runtime_packages()

  app <- shiny::shinyApp(
    ui = anon_app_ui(),
    server = anon_app_server(
      envir = envir,
      initial_selection = initial_selection
    )
  )

  shiny::runApp(app, launch.browser = launch.browser)
}

anon_app_ui <- function() {
  bslib::page_sidebar(
    title = "anon runtime",
    sidebar = bslib::sidebar(
      width = 340,
      shiny::tags$p(
        class = "text-muted",
        "1. Scan environment  2. Configure rules  3. Generate report / text outputs"
      ),
      shiny::actionButton(
        inputId = "scan_environment",
        label = "Scan environment",
        class = "btn-primary"
      ),
      shiny::selectizeInput(
        inputId = "selected_objects",
        label = "Objects for report",
        choices = character(0),
        multiple = TRUE,
        options = list(placeholder = "Scan first, then choose objects")
      ),
      shiny::textInput(
        inputId = "default_replacement",
        label = "Default replacement",
        value = "[REDACTED]"
      ),
      shiny::checkboxInput(
        inputId = "check_approximate",
        label = "Check approximate matches",
        value = FALSE
      ),
      shiny::textAreaInput(
        inputId = "pattern_rules",
        label = "Pattern rules",
        rows = 8,
        placeholder = paste(
          "One rule per line.",
          "Example: PERSON = Alice | Bob",
          "Example: SITE = Boston Medical Center",
          sep = "\n"
        )
      ),
      shiny::actionButton(
        inputId = "generate_report",
        label = "Generate report"
      ),
      shiny::hr(),
      shiny::verbatimTextOutput("scan_status")
    ),
    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "row",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Inventory"),
        shiny::tableOutput("inventory_table")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Workspace"),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Report",
            shiny::verbatimTextOutput("report_preview")
          ),
          shiny::tabPanel(
            title = "Text Tools",
            shiny::textAreaInput(
              inputId = "source_text",
              label = "Source text",
              rows = 10,
              width = "100%",
              placeholder = "Paste text to clean and redact."
            ),
            shiny::checkboxInput(
              inputId = "trim_text",
              label = "Trim line whitespace",
              value = TRUE
            ),
            shiny::checkboxInput(
              inputId = "squish_whitespace",
              label = "Collapse repeated whitespace",
              value = TRUE
            ),
            shiny::checkboxInput(
              inputId = "squash_blank_lines",
              label = "Compress repeated blank lines",
              value = TRUE
            ),
            shiny::actionButton(
              inputId = "apply_text_tools",
              label = "Clean and redact text"
            ),
            shiny::hr(),
            shiny::verbatimTextOutput("cleaned_text_preview"),
            shiny::verbatimTextOutput("comparison_preview")
          ),
          shiny::tabPanel(
            title = "Prompt Bundle",
            shiny::verbatimTextOutput("prompt_bundle_preview")
          )
        )
      )
    )
  )
}

anon_app_server <- function(
  envir = globalenv(),
  initial_selection = NULL
) {
  force(envir)
  force(initial_selection)

  function(input, output, session) {
    inventory_data <- shiny::reactiveVal(NULL)
    report_data <- shiny::reactiveVal(NULL)
    cleaned_text_data <- shiny::reactiveVal(NULL)
    comparison_data <- shiny::reactiveVal(NULL)

    active_rules <- shiny::reactive({
      parse_pattern_rule_text(input$pattern_rules)
    })

    active_default_replacement <- shiny::reactive({
      value <- trimws(input$default_replacement %||% "")
      if (identical(value, "")) "[REDACTED]" else value
    })

    scan_message <- shiny::reactive({
      inventory <- inventory_data()

      if (is.null(inventory)) {
        return("Environment not scanned yet.")
      }

      paste0(
        "Scanned ",
        nrow(inventory),
        " object",
        if (nrow(inventory) == 1) "" else "s",
        "."
      )
    })

    shiny::observeEvent(input$scan_environment, {
      inventory <- anon_inventory(envir, check_approximate = FALSE)
      inventory_data(inventory)
      report_data(NULL)

      available_names <- inventory$name
      selected <- intersect(initial_selection %||% available_names, available_names)

      shiny::updateSelectizeInput(
        session = session,
        inputId = "selected_objects",
        choices = available_names,
        selected = selected,
        server = TRUE
      )
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$generate_report, {
      shiny::req(input$selected_objects)

      report <- anon_report(
        envir = envir,
        selection = input$selected_objects,
        pattern_list = active_rules(),
        default_replacement = active_default_replacement(),
        check_approximate = isTRUE(input$check_approximate)
      )

      report_data(report)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$apply_text_tools, {
      source_text <- input$source_text %||% ""

      cleaned_text <- anon_clean_text(
        source_text,
        trim = !identical(input$trim_text, FALSE),
        squish_whitespace = !identical(input$squish_whitespace, FALSE),
        squash_blank_lines = !identical(input$squash_blank_lines, FALSE)
      )

      redacted_text <- anon(
        cleaned_text,
        pattern_list = active_rules(),
        default_replacement = active_default_replacement(),
        check_approximate = isTRUE(input$check_approximate),
        nlp_auto = FALSE
      )

      cleaned_text_data(as.character(redacted_text))
      comparison_data(anon_compare_text(source_text, as.character(redacted_text)))
    }, ignoreNULL = TRUE)

    output$scan_status <- shiny::renderText({
      scan_message()
    })

    output$inventory_table <- shiny::renderTable({
      inventory <- inventory_data()
      shiny::req(inventory)
      as.data.frame(inventory)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$report_preview <- shiny::renderText({
      report <- report_data()

      if (is.null(report)) {
        return("Generate a report to preview anonymized context.")
      }

      anon_prompt_bundle(
        report = report,
        format = "text",
        include_text = FALSE,
        include_comparison = FALSE
      )
    })

    output$cleaned_text_preview <- shiny::renderText({
      cleaned_text <- cleaned_text_data()

      if (is.null(cleaned_text)) {
        return("Apply text tools to preview cleaned and redacted text.")
      }

      cleaned_text
    })

    output$comparison_preview <- shiny::renderText({
      comparison <- comparison_data()

      if (is.null(comparison)) {
        return("Comparison summary will appear here after text processing.")
      }

      format_text_comparison(comparison)
    })

    output$prompt_bundle_preview <- shiny::renderText({
      report <- report_data()
      cleaned_text <- cleaned_text_data()
      comparison <- comparison_data()

      if (is.null(report) && is.null(cleaned_text)) {
        return("Generate a report or process text to assemble a prompt bundle.")
      }

      anon_prompt_bundle(
        report = report,
        text = cleaned_text,
        comparison = comparison,
        format = "text",
        include_inventory = !is.null(report),
        include_data_summary = !is.null(report),
        include_text = !is.null(cleaned_text),
        include_comparison = !is.null(comparison)
      )
    })
  }
}

ensure_shiny_runtime_packages <- function() {
  required <- c("shiny", "bslib")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    stop(
      "The anon Shiny runtime requires these packages: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
