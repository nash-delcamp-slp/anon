#' Build an anonymized inventory of objects in an environment or list
#'
#' @param envir An environment or named/unnamed list of objects.
#' @param selection Optional character vector of object names to include.
#' @inheritParams anon
#'
#' @return A tibble with one row per object and anonymized metadata about the
#'   available objects.
#'
#' @export
anon_inventory <- function(
  envir = globalenv(),
  selection = NULL,
  pattern_list = list(),
  default_replacement = getOption(
    "anon.default_replacement",
    default = "[REDACTED]"
  ),
  check_approximate = TRUE,
  max_distance = 2
) {
  objects <- normalize_object_source(envir, selection = selection)

  inventory <- purrr::imap_dfr(
    objects,
    ~ get_inventory_row(.y, .x)
  )

  inventory <- anon(
    inventory,
    pattern_list = pattern_list,
    default_replacement = default_replacement,
    check_approximate = check_approximate,
    max_distance = max_distance,
    df_variable_names = FALSE,
    df_classes = FALSE,
    check_names = TRUE,
    check_labels = FALSE,
    nlp_auto = FALSE
  )

  class(inventory) <- unique(c("anon_inventory", class(inventory)))
  inventory
}

#' Build an anonymized report for selected objects
#'
#' @param envir An environment or named/unnamed list of objects.
#' @param selection Optional character vector of object names to include.
#' @inheritParams anon
#'
#' @return An object of class `"anon_report"` containing an anonymized object
#'   inventory and environment summary for the selected objects.
#'
#' @export
anon_report <- function(
  envir = globalenv(),
  selection = NULL,
  pattern_list = list(),
  default_replacement = getOption(
    "anon.default_replacement",
    default = "[REDACTED]"
  ),
  example_values_n = getOption("anon.example_values_n", default = 0),
  example_rows = getOption("anon.example_rows"),
  check_approximate = TRUE,
  max_distance = 2,
  df_variable_names = NULL,
  df_classes = NULL,
  check_names = TRUE,
  check_labels = TRUE,
  nlp_auto = getOption("anon.nlp_auto")
) {
  objects <- normalize_object_source(envir, selection = selection)

  report <- list(
    object_names = names(objects),
    inventory = anon_inventory(
      objects,
      pattern_list = pattern_list,
      default_replacement = default_replacement,
      check_approximate = check_approximate,
      max_distance = max_distance
    ),
    data_summary = anon_data_summary(
      objects,
      pattern_list = pattern_list,
      default_replacement = default_replacement,
      example_values_n = example_values_n,
      example_rows = example_rows,
      check_approximate = check_approximate,
      max_distance = max_distance,
      nlp_auto = nlp_auto
    )
  )

  structure(report, class = c("anon_report", "anon_context", "list"))
}

#' Compare text before and after cleaning or redaction
#'
#' @param before Character vector containing the original text.
#' @param after Character vector containing the transformed text.
#'
#' @return A list with overall summary statistics and line-level details.
#'
#' @export
anon_compare_text <- function(before, after) {
  before <- as.character(before)
  after <- as.character(after)

  n_rows <- max(length(before), length(after))
  before <- c(before, rep(NA_character_, n_rows - length(before)))
  after <- c(after, rep(NA_character_, n_rows - length(after)))

  details <- tibble::tibble(
    index = seq_len(n_rows),
    before = before,
    after = after,
    changed = dplyr::coalesce(before != after, TRUE)
  )

  summary <- tibble::tibble(
    lines_before = sum(!is.na(before)),
    lines_after = sum(!is.na(after)),
    changed_lines = sum(details$changed),
    unchanged_lines = sum(!details$changed),
    chars_before = sum(nchar(before), na.rm = TRUE),
    chars_after = sum(nchar(after), na.rm = TRUE)
  )

  structure(
    list(summary = summary, details = details),
    class = c("anon_text_compare", "list")
  )
}

#' Build prompt-ready text from anonymized report objects
#'
#' @param report Optional object created by [anon_report()].
#' @param text Optional character vector of redacted text to include.
#' @param comparison Optional object created by [anon_compare_text()].
#' @param format Output format. `"markdown"` uses markdown headings and fenced
#'   blocks, while `"text"` uses plain-text headings.
#' @param title Top-level title for the generated bundle.
#' @param include_inventory Logical indicating whether to include the inventory
#'   section when `report` is provided.
#' @param include_data_summary Logical indicating whether to include the summary
#'   section when `report` is provided.
#' @param include_text Logical indicating whether to include `text`.
#' @param include_comparison Logical indicating whether to include `comparison`.
#'
#' @return A length-one character vector of class `"anon_prompt_bundle"`.
#'
#' @export
anon_prompt_bundle <- function(
  report = NULL,
  text = NULL,
  comparison = NULL,
  format = c("markdown", "text", "json"),
  title = "Anonymized Prompt Context",
  include_inventory = TRUE,
  include_data_summary = TRUE,
  include_text = TRUE,
  include_comparison = TRUE
) {
  format <- match.arg(format)

  payload <- build_anon_prompt_bundle_payload(
    report = report,
    text = text,
    comparison = comparison,
    title = title,
    include_inventory = include_inventory,
    include_data_summary = include_data_summary,
    include_text = include_text,
    include_comparison = include_comparison
  )

  if (format == "json") {
    bundle <- jsonlite::toJSON(
      payload,
      pretty = TRUE,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      force = TRUE
    )
  } else {
    sections <- character(0)

    if (format == "markdown") {
      sections <- c(sections, paste0("# ", title))
    } else {
      sections <- c(
        sections,
        title,
        strrep("=", nchar(title))
      )
    }

    if (!is.null(report)) {
      if (isTRUE(include_inventory) && !is.null(report$inventory)) {
        sections <- c(
          sections,
          format_section(
            "Inventory",
            format_table_block(report$inventory),
            format = format
          )
        )
      }

      if (isTRUE(include_data_summary) && !is.null(report$data_summary)) {
        sections <- c(
          sections,
          format_section(
            "Data Summary",
            format_anon_data_summary_text(report$data_summary),
            format = format
          )
        )
      }
    }

    if (isTRUE(include_text) && !is.null(text)) {
      sections <- c(
        sections,
        format_section(
          "Redacted Text",
          paste(text, collapse = "
"),
          format = format
        )
      )
    }

    if (isTRUE(include_comparison) && !is.null(comparison)) {
      sections <- c(
        sections,
        format_section(
          "Comparison Summary",
          paste(
            c(
              format_table_block(comparison$summary),
              "",
              "Line-level details:",
              format_table_block(comparison$details)
            ),
            collapse = "
"
          ),
          format = format
        )
      )
    }

    bundle <- paste(sections, collapse = "

")
  }

  attr(bundle, "payload") <- payload
  attr(bundle, "output_format") <- format
  class(bundle) <- c("anon_prompt_bundle", "anon_context", "character")
  bundle
}

# helpers -----------------------------------------------------------------

normalize_object_source <- function(envir, selection = NULL) {
  if (is.environment(envir)) {
    object_names <- ls(envir = envir)
    objects <- stats::setNames(
      lapply(object_names, function(name) {
        get(name, envir = envir, inherits = FALSE)
      }),
      object_names
    )
  } else if (is.list(envir)) {
    objects <- envir
    object_names <- names(objects)

    if (is.null(object_names)) {
      object_names <- if (length(objects) == 0) character(0) else paste0("x", seq_along(objects))
    } else {
      missing_names <- is.na(object_names) | object_names == ""
      object_names[missing_names] <- paste0("x", which(missing_names))
    }

    names(objects) <- object_names
  } else {
    stop("`envir` must be an environment or a list.", call. = FALSE)
  }

  if (!is.null(selection)) {
    missing_selection <- setdiff(selection, names(objects))
    if (length(missing_selection) > 0) {
      stop(
        "Unknown objects in `selection`: ",
        paste(missing_selection, collapse = ", "),
        call. = FALSE
      )
    }
    objects <- objects[selection]
  }

  objects
}

get_inventory_row <- function(name, obj) {
  tibble::tibble(
    name = name,
    kind = get_object_kind(obj),
    type = class(obj)[1],
    n_rows = get_n_rows(obj),
    n_cols = get_n_cols(obj),
    length = length(obj),
    has_text = object_has_text(obj),
    has_labels = object_has_labels(obj),
    memory_size = format(utils::object.size(obj), units = "auto")
  )
}

get_object_kind <- function(obj) {
  if (is.data.frame(obj)) {
    "data.frame"
  } else if (is.function(obj)) {
    "function"
  } else if (is.list(obj)) {
    "list"
  } else if (is.vector(obj) || is.factor(obj)) {
    "vector"
  } else {
    class(obj)[1]
  }
}

get_n_rows <- function(obj) {
  if (is.data.frame(obj)) {
    nrow(obj)
  } else {
    NA_integer_
  }
}

get_n_cols <- function(obj) {
  if (is.data.frame(obj)) {
    ncol(obj)
  } else {
    NA_integer_
  }
}

object_has_text <- function(obj) {
  if (is.character(obj) || is.factor(obj)) {
    return(length(obj) > 0)
  }

  if (is.data.frame(obj)) {
    return(any(vapply(obj, function(col) is.character(col) || is.factor(col), logical(1))))
  }

  if (is.list(obj)) {
    return(any(vapply(obj, object_has_text, logical(1))))
  }

  FALSE
}

object_has_labels <- function(obj) {
  if (!is.null(attr(obj, "label", exact = TRUE))) {
    return(TRUE)
  }

  if (is.data.frame(obj)) {
    return(any(vapply(
      obj,
      function(col) !is.null(attr(col, "label", exact = TRUE)),
      logical(1)
    )))
  }

  if (is.list(obj)) {
    return(any(vapply(obj, object_has_labels, logical(1))))
  }

  FALSE
}

format_section <- function(title, body, format = c("markdown", "text")) {
  format <- match.arg(format)

  if (format == "markdown") {
    paste0("## ", title, "\n\n```text\n", body, "\n```")
  } else {
    paste(title, strrep("-", nchar(title)), body, sep = "\n")
  }
}

format_table_for_display <- function(x) {
  x <- as.data.frame(x)

  list_cols <- vapply(x, is.list, logical(1))
  if (any(list_cols)) {
    x[list_cols] <- lapply(x[list_cols], function(col) {
      vapply(col, format_list_cell, character(1))
    })
  }

  x
}

format_list_cell <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }

  if (is.data.frame(x)) {
    return(paste(utils::capture.output(print(x, row.names = FALSE)), collapse = " | "))
  }

  if (is.list(x)) {
    return(paste(vapply(x, format_example_scalar, character(1)), collapse = " | "))
  }

  format_example_scalar(x)
}

format_example_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }

  values <- as.character(x)
  values[is.na(values)] <- "NA"
  paste(values, collapse = " | ")
}

format_table_block <- function(x) {
  paste(
    utils::capture.output(print(format_table_for_display(x), row.names = FALSE)),
    collapse = "\n"
  )
}

format_anon_data_summary_text <- function(x) {
  sections <- character(0)

  if (!is.null(x$summary)) {
    sections <- c(
      sections,
      "Environment Data Summary",
      format_table_block(x$summary)
    )
  }

  if (!is.null(x$data_frames$structure)) {
    sections <- c(
      sections,
      "",
      "Data Frames",
      format_table_block(x$data_frames$structure)
    )
  }

  if (!is.null(x$data_frames$variables)) {
    variable_blocks <- purrr::imap_chr(
      x$data_frames$variables,
      ~ paste(
        paste0("Variable Details (", .y, ")"),
        format_table_block(.x),
        sep = "\n"
      )
    )
    sections <- c(sections, "", variable_blocks)
  }

  if (!is.null(x$examples$rows)) {
    row_blocks <- purrr::imap_chr(
      x$examples$rows,
      ~ paste(
        paste0("Example Rows (", .y, ")"),
        format_table_block(.x),
        sep = "\n"
      )
    )
    sections <- c(sections, "", row_blocks)
  }

  scenarios <- get_example_scenarios(x$examples)
  if (length(scenarios) > 0L) {
    scenario_sections <- purrr::imap_chr(
      scenarios,
      ~ {
        heading <- if (length(scenarios) == 1L) {
          paste0(
            "Example Scenario (",
            .x$key,
            " = ",
            format_example_scalar(.x$value),
            ")"
          )
        } else {
          paste0(
            "Example Scenario ",
            .y,
            " (",
            .x$key,
            " = ",
            format_example_scalar(.x$value),
            ")"
          )
        }

        scenario_blocks <- purrr::imap_chr(
          .x$tables,
          ~ paste(
            .y,
            format_table_block(.x),
            sep = "\n"
          )
        )

        paste(c(heading, scenario_blocks), collapse = "\n")
      }
    )
    sections <- c(sections, "", scenario_sections)
  }

  if (!is.null(x$other_objects)) {
    sections <- c(
      sections,
      "",
      "Other Objects",
      format_table_block(x$other_objects)
    )
  }

  paste(sections, collapse = "\n")
}