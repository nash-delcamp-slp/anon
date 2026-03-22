#' Launch the local `anon` Shiny app
#'
#' The `anon` Shiny app provides a local, session-oriented workflow for reviewing an
#' environment, selecting objects, cleaning text, and generating anonymized
#' structural reports suitable for prompt building.
#'
#' @param envir Environment or list to inspect. Defaults to `globalenv()`.
#' @param launch.browser Passed to [shiny::runApp()]. Defaults to `interactive()`.
#' @param initial_selection Optional character vector of object names to preselect
#'   after the environment is loaded.
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
  initial_state <- anon_app_initial_state(
    envir = envir,
    pattern_list = runtime_defaults$pattern_list,
    default_replacement = runtime_defaults$default_replacement,
    initial_selection = initial_selection
  )

  app <- shiny::shinyApp(
    ui = anon_app_ui(
      runtime_defaults = runtime_defaults,
      initial_state = initial_state
    ),
    server = anon_app_server(
      envir = envir,
      initial_selection = initial_selection,
      runtime_defaults = runtime_defaults,
      initial_state = initial_state
    )
  )

  shiny::runApp(app, launch.browser = launch.browser)
}

anon_app_ui <- function(
  runtime_defaults = anon_runtime_defaults(),
  initial_state = NULL
) {
  if (is.null(initial_state)) {
    initial_state <- anon_app_initial_state(
      envir = globalenv(),
      pattern_list = runtime_defaults$pattern_list,
      default_replacement = runtime_defaults$default_replacement
    )
  }

  bslib::page_sidebar(
    title = "anon",
    sidebar = bslib::sidebar(
      width = 340,
      shiny::actionButton(
        inputId = "scan_environment",
        label = "Refresh environment",
        class = "btn-primary"
      ),
      shiny::selectizeInput(
        inputId = "selected_objects",
        label = "Objects for report",
        choices = initial_state$choices,
        selected = initial_state$selected,
        multiple = TRUE,
        options = list(placeholder = "Choose objects from the current environment")
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
            shiny::fileInput(
              inputId = "upload_files",
              label = "Upload files (.docx, .pptx, .xlsx, .pdf, .txt, .csv, ...)",
              multiple = TRUE,
              accept = c(
                ".docx", ".pptx", ".xlsx", ".xls", ".txt", ".csv",
                ".log", ".md", ".json", ".xml", ".pdf"
              )
            ),
            shiny::textAreaInput(
              inputId = "source_text",
              label = "Source text",
              rows = 10,
              width = "100%",
              placeholder = "Paste text here, or upload files above."
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
  runtime_defaults = anon_runtime_defaults(),
  initial_state = NULL
) {
  force(envir)
  force(initial_selection)
  force(runtime_defaults)

  if (is.null(initial_state)) {
    initial_state <- anon_app_initial_state(
      envir = envir,
      pattern_list = runtime_defaults$pattern_list,
      default_replacement = runtime_defaults$default_replacement,
      initial_selection = initial_selection
    )
  }

  force(initial_state)

  function(input, output, session) {
    current_objects_data <- shiny::reactiveVal(initial_state$objects)
    inventory_data <- shiny::reactiveVal(initial_state$inventory)
    report_data <- shiny::reactiveVal(NULL)
    cleaned_text_data <- shiny::reactiveVal(NULL)
    comparison_data <- shiny::reactiveVal(NULL)
    nlp_status_data <- shiny::reactiveVal(get_nlp_runtime_status())
    nlp_entity_types_touched <- shiny::reactiveVal(FALSE)

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

    shiny::observeEvent(input$nlp_entity_types, {
      nlp_entity_types_touched(TRUE)
    }, ignoreInit = TRUE)

    active_nlp_entity_types <- shiny::reactive({
      if (!isTRUE(nlp_entity_types_touched()) && is.null(input$nlp_entity_types)) {
        return(runtime_defaults$nlp_entity_types)
      }
      input$nlp_entity_types %||% character(0)
    })

    active_report_nlp_auto <- shiny::reactive({
      if (!isTRUE(active_nlp_enabled())) {
        return(FALSE)
      }

      nlp_auto_from_entity_types(active_nlp_entity_types())
    })

    scan_message <- shiny::reactive({
      inventory <- inventory_data()

      paste0(
        "Loaded ",
        nrow(inventory),
        " object",
        if (nrow(inventory) == 1) "" else "s",
        " from environment."
      )
    })

    shiny::observeEvent(input$scan_environment, {
      refreshed_state <- anon_app_initial_state(
        envir = envir,
        pattern_list = active_rules(),
        default_replacement = active_default_replacement(),
        initial_selection = initial_selection,
        current_selected = input$selected_objects,
        selection_initialized = TRUE
      )

      current_objects_data(refreshed_state$objects)
      inventory_data(refreshed_state$inventory)
      report_data(NULL)

      shiny::updateSelectizeInput(
        session = session,
        inputId = "selected_objects",
        choices = refreshed_state$choices,
        selected = refreshed_state$selected,
        server = FALSE
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$generate_report, {
      shiny::req(length(input$selected_objects %||% character(0)) > 0)

      report <- anon_report(
        envir = envir,
        selection = input$selected_objects,
        pattern_list = active_rules(),
        default_replacement = active_default_replacement(),
        check_approximate = isTRUE(input$check_approximate),
        nlp_auto = active_report_nlp_auto()
      )

      report_data(report)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$upload_files, {
      files <- input$upload_files
      shiny::req(nrow(files) > 0)

      result <- tryCatch(
        read_content(files$datapath, name = files$name),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      shiny::req(result)

      combined <- paste(
        paste0("--- ", names(result), " ---\n", result),
        collapse = "\n\n"
      )

      current <- input$source_text %||% ""
      new_text <- if (nzchar(trimws(current))) {
        paste(current, combined, sep = "\n\n")
      } else {
        combined
      }

      shiny::updateTextAreaInput(session, "source_text", value = new_text)
    }, ignoreInit = TRUE)

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
    }, ignoreInit = TRUE)

    output$scan_status <- shiny::renderText({
      scan_message()
    })

    output$nlp_status <- shiny::renderText({
      status <- nlp_status_data()
      prefix <- if (isTRUE(status$available)) "NLP ready" else "NLP unavailable"
      paste0(prefix, ": ", status$message)
    })

    output$inventory_table <- shiny::renderTable({
      as.data.frame(inventory_data())
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

anon_app_initial_state <- function(
  envir,
  pattern_list = list(),
  default_replacement = getOption(
    "anon.default_replacement",
    default = "[REDACTED]"
  ),
  initial_selection = NULL,
  current_selected = NULL,
  selection_initialized = FALSE
) {
  objects <- normalize_object_source(envir)
  inventory <- anon_inventory(
    objects,
    pattern_list = pattern_list,
    default_replacement = default_replacement,
    check_approximate = FALSE
  )
  raw_names <- names(objects)
  selected <- resolve_report_selection(
    raw_names = raw_names,
    initial_selection = initial_selection,
    current_selected = current_selected,
    selection_initialized = selection_initialized
  )

  list(
    objects = objects,
    inventory = inventory,
    raw_names = raw_names,
    choices = stats::setNames(raw_names, inventory$name),
    selected = selected
  )
}

nlp_auto_from_entity_types <- function(entity_types) {
  if (is.null(entity_types) || length(entity_types) == 0) {
    return(FALSE)
  }

  stats::setNames(as.list(rep(TRUE, length(entity_types))), tolower(entity_types))
}

resolve_report_selection <- function(
  raw_names,
  initial_selection = NULL,
  current_selected = NULL,
  selection_initialized = FALSE
) {
  if (!isTRUE(selection_initialized)) {
    if (is.null(initial_selection)) {
      return(raw_names)
    }

    return(intersect(initial_selection, raw_names))
  }

  intersect(current_selected %||% character(0), raw_names)
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
      "The anon Shiny app requires these packages: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
