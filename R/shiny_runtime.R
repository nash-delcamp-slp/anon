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
        inputId = "configure_data_summary",
        label = "Data Summary Options"
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
    data_summary_options_data <- shiny::reactiveVal(runtime_defaults$data_summary_options)
    pending_excel_upload_data <- shiny::reactiveVal(NULL)

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


    active_data_summary_options <- shiny::reactive({
      data_summary_options_data()
    })

    shiny::observeEvent(input$configure_data_summary, {
      shiny::showModal(data_summary_options_modal(active_data_summary_options()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$save_data_summary_options, {
      updated_options <- tryCatch(
        parse_data_summary_options_input(input),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      shiny::req(updated_options)

      data_summary_options_data(updated_options)
      shiny::removeModal()
    }, ignoreInit = TRUE)

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
        example_values_n = active_data_summary_options()$example_values_n,
        example_rows = build_data_summary_example_rows(active_data_summary_options()),
        check_approximate = isTRUE(input$check_approximate),
        nlp_auto = active_report_nlp_auto()
      )

      report_data(report)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$upload_files, {
      files <- input$upload_files
      shiny::req(nrow(files) > 0)

      current <- input$source_text %||% ""
      split_files <- split_uploaded_files(files)

      if (nrow(split_files$other) > 0) {
        result <- tryCatch(
          read_content(split_files$other$datapath, name = split_files$other$name),
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
            NULL
          }
        )
        shiny::req(result)
        current <- append_uploaded_content(current, result)
      }

      if (nrow(split_files$excel) == 0) {
        shiny::updateTextAreaInput(session, "source_text", value = current)
        return()
      }

      choice_data <- tryCatch(
        collect_excel_upload_sheet_choices(split_files$excel),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      if (is.null(choice_data) || length(choice_data$choices) == 0) {
        pending_excel_upload_data(NULL)
        shiny::updateTextAreaInput(session, "source_text", value = current)
        return()
      }

      pending_excel_upload_data(list(
        files = split_files$excel,
        current_text = current
      ))

      shiny::showModal(excel_sheet_selection_modal(choice_data))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_excel_sheet_selection, {
      pending <- pending_excel_upload_data()
      shiny::req(!is.null(pending))

      pending_excel_upload_data(NULL)
      shiny::removeModal()
      shiny::updateTextAreaInput(session, "source_text", value = pending$current_text)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_excel_sheet_selection, {
      pending <- pending_excel_upload_data()
      shiny::req(!is.null(pending))

      result <- tryCatch(
        import_uploaded_excel_files(
          pending$files,
          input$selected_excel_sheets %||% character(0)
        ),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      shiny::req(!is.null(result))

      pending_excel_upload_data(NULL)
      shiny::removeModal()
      shiny::updateTextAreaInput(
        session,
        "source_text",
        value = append_uploaded_content(pending$current_text, result)
      )
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
    },
    data_summary_options = default_data_summary_options()
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

split_uploaded_files <- function(files) {
  is_excel <- vapply(files$name, is_excel_upload_name, logical(1))

  list(
    excel = files[is_excel, , drop = FALSE],
    other = files[!is_excel, , drop = FALSE]
  )
}

is_excel_upload_name <- function(name) {
  tolower(tools::file_ext(name)) %in% c("xlsx", "xls")
}

append_uploaded_content <- function(current_text, result) {
  if (length(result) == 0) {
    return(current_text)
  }

  combined <- paste(
    paste0("--- ", names(result), " ---\n", result),
    collapse = "\n\n"
  )

  if (nzchar(trimws(current_text))) {
    paste(current_text, combined, sep = "\n\n")
  } else {
    combined
  }
}

collect_excel_upload_sheet_choices <- function(files) {
  choices <- character(0)

  for (i in seq_len(nrow(files))) {
    sheets <- list_excel_file_sheets(files$datapath[i])

    if (length(sheets) == 0) {
      next
    }

    choice_values <- make_excel_sheet_choice_id(i, sheets)
    choice_labels <- paste0(files$name[i], " :: ", sheets)
    choices <- c(choices, stats::setNames(choice_values, choice_labels))
  }

  list(
    choices = choices,
    selected = unname(choices)
  )
}

list_excel_file_sheets <- function(path) {
  ensure_content_package("readxl", ".xlsx")
  readxl::excel_sheets(path)
}

make_excel_sheet_choice_id <- function(file_index, sheet_name) {
  paste0(file_index, "::", sheet_name)
}

parse_excel_sheet_selection <- function(selected_ids) {
  if (length(selected_ids) == 0) {
    return(list())
  }

  file_index <- sub("::.*$", "", selected_ids)
  sheet_name <- sub("^[0-9]+::", "", selected_ids)
  split(sheet_name, file_index)
}

import_uploaded_excel_files <- function(files, selected_sheet_ids) {
  selected_sheets <- parse_excel_sheet_selection(selected_sheet_ids)

  if (length(selected_sheets) == 0) {
    return(character(0))
  }

  out <- character(0)

  for (i in seq_len(nrow(files))) {
    sheets <- selected_sheets[[as.character(i)]]

    if (is.null(sheets) || length(sheets) == 0) {
      next
    }

    out <- c(
      out,
      stats::setNames(read_content_xlsx(files$datapath[i], sheet = sheets), files$name[i])
    )
  }

  out
}

excel_sheet_selection_modal <- function(choice_data) {
  shiny::modalDialog(
    title = "Select Excel Sheets",
    shiny::checkboxGroupInput(
      inputId = "selected_excel_sheets",
      label = "Sheets to include",
      choices = choice_data$choices,
      selected = choice_data$selected
    ),
    easyClose = FALSE,
    footer = shiny::tagList(
      shiny::actionButton("cancel_excel_sheet_selection", "Cancel"),
      shiny::actionButton("confirm_excel_sheet_selection", "Import selected sheets")
    )
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


default_data_summary_options <- function() {
  example_values_n <- normalize_example_values_n(
    getOption("anon.example_values_n", default = 0)
  )
  example_rows <- normalize_example_rows_spec(getOption("anon.example_rows"))

  if (is.null(example_rows)) {
    return(list(
      example_values_n = example_values_n,
      example_rows_n = 0L,
      example_rows_key = "",
      example_rows_method = "random",
      example_rows_value = "",
      example_rows_n_key_values = 1L,
      example_rows_seed = ""
    ))
  }

  list(
    example_values_n = example_values_n,
    example_rows_n = example_rows$n,
    example_rows_key = example_rows$key %||% "",
    example_rows_method = example_rows$method,
    example_rows_value = if (is.null(example_rows$value)) "" else as.character(example_rows$value),
    example_rows_n_key_values = example_rows$n_key_values %||% 1L,
    example_rows_seed = if (is.null(example_rows$seed)) "" else as.character(example_rows$seed)
  )
}

data_summary_options_modal <- function(options = default_data_summary_options()) {
  options <- normalize_data_summary_options(options)

  shiny::modalDialog(
    title = "Data Summary Options",
    shiny::numericInput(
      inputId = "data_summary_example_values_n",
      label = "Example values per discrete column",
      value = options$example_values_n,
      min = 0,
      step = 1
    ),
    shiny::numericInput(
      inputId = "data_summary_example_rows_n",
      label = "Example rows per table",
      value = options$example_rows_n,
      min = 0,
      step = 1
    ),
    shiny::textInput(
      inputId = "data_summary_example_rows_key",
      label = "Scenario key (optional)",
      value = options$example_rows_key,
      placeholder = "USUBJID"
    ),
    shiny::selectInput(
      inputId = "data_summary_example_rows_method",
      label = "Selection method",
      choices = c("first", "last", "random"),
      selected = options$example_rows_method
    ),
    shiny::textInput(
      inputId = "data_summary_example_rows_value",
      label = "Explicit scenario value (optional)",
      value = options$example_rows_value,
      placeholder = "Overrides method when key is provided"
    ),
    shiny::numericInput(
      inputId = "data_summary_example_rows_n_key_values",
      label = "Scenario key values",
      value = options$example_rows_n_key_values,
      min = 1,
      step = 1
    ),
    shiny::textInput(
      inputId = "data_summary_example_rows_seed",
      label = "Random seed (optional)",
      value = options$example_rows_seed,
      placeholder = "Used only for random selection"
    ),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("save_data_summary_options", "Apply")
    )
  )
}

normalize_data_summary_options <- function(options) {
  defaults <- default_data_summary_options()
  utils::modifyList(defaults, options %||% list())
}

parse_data_summary_options_input <- function(input) {
  example_values_n <- normalize_modal_count(input$data_summary_example_values_n, "Example values")
  example_rows_n <- normalize_modal_count(input$data_summary_example_rows_n, "Example rows")

  key <- trimws(input$data_summary_example_rows_key %||% "")
  method <- trimws(input$data_summary_example_rows_method %||% "random")
  method <- match.arg(method, c("first", "last", "random"))
  value <- trimws(input$data_summary_example_rows_value %||% "")
  n_key_values <- normalize_modal_positive_count(
    input$data_summary_example_rows_n_key_values,
    "Scenario key values"
  )
  seed <- trimws(input$data_summary_example_rows_seed %||% "")

  if (nzchar(value) && !nzchar(key)) {
    stop("An explicit scenario value requires a scenario key.", call. = FALSE)
  }
  if (nzchar(value)) {
    n_key_values <- 1L
  }
  if (example_rows_n > 0L && n_key_values > 1L && !nzchar(key)) {
    stop("Multiple scenario key values require a scenario key.", call. = FALSE)
  }

  if (example_rows_n > 0L && nzchar(seed) && method != "random") {
    stop("A random seed can only be used with the random selection method.", call. = FALSE)
  }

  if (nzchar(seed)) {
    parsed_seed <- suppressWarnings(as.integer(seed))
    if (is.na(parsed_seed)) {
      stop("Random seed must be a whole number.", call. = FALSE)
    }
    seed <- as.character(parsed_seed)
  }

  list(
    example_values_n = example_values_n,
    example_rows_n = example_rows_n,
    example_rows_key = key,
    example_rows_method = method,
    example_rows_value = value,
    example_rows_n_key_values = n_key_values,
    example_rows_seed = seed
  )
}

normalize_modal_count <- function(x, label) {
  if (length(x) != 1 || is.na(x) || !is.numeric(x)) {
    stop(label, " must be a non-negative whole number.", call. = FALSE)
  }

  value <- as.integer(x)
  if (value < 0L) {
    stop(label, " must be a non-negative whole number.", call. = FALSE)
  }

  value
}

normalize_modal_positive_count <- function(x, label) {
  value <- normalize_modal_count(x, label)
  if (value <= 0L) {
    stop(label, " must be at least 1.", call. = FALSE)
  }

  value
}

build_data_summary_example_rows <- function(options) {
  options <- normalize_data_summary_options(options)

  if (options$example_rows_n <= 0L) {
    return(NULL)
  }

  key <- empty_string_to_null(options$example_rows_key)
  value <- empty_string_to_null(options$example_rows_value)
  seed <- empty_string_to_null(options$example_rows_seed)
  if (!is.null(seed)) {
    seed <- as.integer(seed)
  }

  anon_example_rows(
    n = options$example_rows_n,
    key = key,
    method = options$example_rows_method,
    value = value,
    n_key_values = if (is.null(value)) options$example_rows_n_key_values else 1L,
    seed = seed
  )
}

empty_string_to_null <- function(x) {
  value <- trimws(x %||% "")
  if (!nzchar(value)) {
    return(NULL)
  }

  value
}
