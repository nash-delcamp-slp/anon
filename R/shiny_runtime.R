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
  runtime_defaults <- anon_runtime_defaults()

  app <- shiny::shinyApp(
    ui = anon_app_ui(runtime_defaults = runtime_defaults),
    server = anon_app_server(
      envir = envir,
      initial_selection = initial_selection,
      runtime_defaults = runtime_defaults
    )
  )

  shiny::runApp(app, launch.browser = launch.browser)
}

anon_app_ui <- function(runtime_defaults = anon_runtime_defaults()) {
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
        value = runtime_defaults$default_replacement
      ),
      shiny::checkboxInput(
        inputId = "check_approximate",
        label = "Check approximate matches",
        value = runtime_defaults$check_approximate
      ),
      shiny::textAreaInput(
        inputId = "pattern_rules",
        label = "Pattern rules",
        rows = 8,
        value = runtime_defaults$pattern_rules_text,
        placeholder = paste(
          "One rule per line.",
          "Example: PERSON = Alice | Bob",
          "Example: SITE = Boston Medical Center",
          "Unnamed lines use the default replacement.",
          sep = "\n"
        )
      ),
      shiny::checkboxInput(
        inputId = "enable_nlp",
        label = "Enable NLP entity redaction",
        value = runtime_defaults$enable_nlp
      ),
      shiny::selectizeInput(
        inputId = "nlp_entity_types",
        label = "NLP entity types",
        choices = nlp_entity_sets$all,
        selected = runtime_defaults$nlp_entity_types,
        multiple = TRUE,
        options = list(placeholder = "Choose entity types for NLP redaction")
      ),
      shiny::actionButton(
        inputId = "generate_report",
        label = "Generate report"
      ),
      shiny::hr(),
      shiny::verbatimTextOutput("scan_status"),
      shiny::verbatimTextOutput("nlp_status")
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
  initial_selection = NULL,
  runtime_defaults = anon_runtime_defaults()
) {
  force(envir)
  force(initial_selection)
  force(runtime_defaults)

  function(input, output, session) {
    inventory_data <- shiny::reactiveVal(NULL)
    report_data <- shiny::reactiveVal(NULL)
    cleaned_text_data <- shiny::reactiveVal(NULL)
    comparison_data <- shiny::reactiveVal(NULL)
    nlp_status_data <- shiny::reactiveVal(get_nlp_runtime_status())

    active_rules <- shiny::reactive({
      if (is.null(input$pattern_rules)) {
        return(runtime_defaults$pattern_list)
      }
      parse_pattern_rule_text(input$pattern_rules)
    })

    active_default_replacement <- shiny::reactive({
      if (is.null(input$default_replacement)) {
        return(runtime_defaults$default_replacement)
      }
      value <- trimws(input$default_replacement %||% "")
      if (identical(value, "")) "[REDACTED]" else value
    })

    active_nlp_enabled <- shiny::reactive({
      if (is.null(input$enable_nlp)) {
        return(runtime_defaults$enable_nlp)
      }
      isTRUE(input$enable_nlp)
    })

    active_nlp_entity_types <- shiny::reactive({
      if (is.null(input$nlp_entity_types)) {
        return(runtime_defaults$nlp_entity_types)
      }
      input$nlp_entity_types %||% character(0)
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
      inventory <- anon_inventory(
        envir,
        pattern_list = active_rules(),
        default_replacement = active_default_replacement(),
        check_approximate = FALSE
      )
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

      if (isTRUE(active_nlp_enabled())) {
        nlp_status <- nlp_status_data()
        entity_types <- active_nlp_entity_types()

        if (!isTRUE(nlp_status$available)) {
          shiny::showNotification(
            paste("NLP redaction unavailable:", nlp_status$message),
            type = "warning"
          )
        } else if (length(entity_types) > 0) {
          nlp_args <- list(
            x = redacted_text,
            entity_types = entity_types,
            check_approximate = isTRUE(input$check_approximate)
          )

          if (!identical(
            active_default_replacement(),
            runtime_defaults$default_replacement
          )) {
            nlp_args$default_replacement <- active_default_replacement()
          }

          redacted_text <- do.call(anon_nlp_entities, nlp_args)
        }
      }

      cleaned_text_data(as.character(redacted_text))
      comparison_data(anon_compare_text(source_text, as.character(redacted_text)))
    }, ignoreNULL = TRUE)

    output$scan_status <- shiny::renderText({
      scan_message()
    })

    output$nlp_status <- shiny::renderText({
      status <- nlp_status_data()
      prefix <- if (isTRUE(status$available)) "NLP ready" else "NLP unavailable"
      paste0(prefix, ": ", status$message)
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

anon_runtime_defaults <- function() {
  option_pattern_list <- getOption("anon.pattern_list", default = list())
  enabled_nlp_entities <- get_enabled_nlp_entities(getOption("anon.nlp_auto"))

  list(
    default_replacement = getOption(
      "anon.default_replacement",
      default = "[REDACTED]"
    ),
    check_approximate = FALSE,
    pattern_list = option_pattern_list,
    pattern_rules_text = format_pattern_rule_text(option_pattern_list),
    enable_nlp = length(enabled_nlp_entities) > 0,
    nlp_entity_types = if (length(enabled_nlp_entities) > 0) {
      enabled_nlp_entities
    } else {
      nlp_entity_sets$named
    }
  )
}

get_nlp_runtime_status <- function() {
  if (!requireNamespace("cleanNLP", quietly = TRUE)) {
    return(list(available = FALSE, message = "cleanNLP is not installed."))
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(list(available = FALSE, message = "reticulate is not installed."))
  }

  spacy_available <- tryCatch(
    suppressWarnings(reticulate::py_module_available("cleannlp")),
    error = function(e) FALSE
  )

  if (!isTRUE(spacy_available)) {
    return(list(
      available = FALSE,
      message = "spaCy backend is not available for cleanNLP."
    ))
  }

  init_error <- tryCatch(
    {
      suppressWarnings(suppressMessages(cleanNLP::cnlp_init_spacy()))
      NULL
    },
    error = function(e) e
  )

  if (inherits(init_error, "error")) {
    return(list(
      available = FALSE,
      message = paste("spaCy initialization failed:", conditionMessage(init_error))
    ))
  }

  list(
    available = TRUE,
    message = "cleanNLP and spaCy are available for entity redaction."
  )
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
