#' Build structured anon payloads
#'
#' Normalize supported anon objects into a structured payload suitable for
#' downstream AI/tooling use or for conversion with \code{as_anon_json()}.
#' Unsupported custom classes error with the object path to make failures easy
#' to diagnose.
#'
#' @param x Object to normalize. Supports report/prompt-bundle objects and
#'   common anon_context payloads such as data frames, lists, and atomic
#'   vectors.
#' @param path Internal recursion path used for serializer error reporting.
#'   External callers should usually leave the default.
#'
#' @return A structured R object composed of lists, vectors, and table payloads
#'   ready for JSON serialization.
#'
#' @export
as_anon_payload <- function(x, path = "$") {
  UseMethod("as_anon_payload")
}

#' Serialize anon objects as JSON
#'
#' Convert supported anon objects and common anon_context payloads into a JSON
#' representation suitable for downstream AI/tooling use.
#'
#' @inheritParams as_anon_payload
#' @param pretty Logical indicating whether to pretty-print the JSON output.
#'   Defaults to \code{TRUE}.
#' @param auto_unbox Logical passed to \code{jsonlite::toJSON()}. Defaults to
#'   \code{TRUE}.
#'
#' @return A length-one character vector containing JSON.
#'
#' @export
as_anon_json <- function(x, pretty = TRUE, auto_unbox = TRUE) {
  jsonlite::toJSON(
    as_anon_payload(x),
    pretty = pretty,
    auto_unbox = auto_unbox,
    null = "null",
    na = "null",
    force = TRUE
  )
}

#' @exportS3Method
as_anon_payload.default <- function(x, path = "$") {
  serialize_supported_value(x, path = path)
}

#' @exportS3Method
as_anon_payload.anon_context <- function(x, path = "$") {
  underlying <- x
  class(underlying) <- setdiff(class(x), "anon_context")

  if (length(class(underlying)) == 0L) {
    attr(underlying, "class") <- NULL
  }

  serialize_supported_value(underlying, path = path)
}

#' @exportS3Method
as_anon_payload.anon_context_collection <- function(x, path = "$") {
  items <- lapply(seq_along(x), function(i) {
    serialize_supported_value(
      x[[i]],
      path = json_child_path(path, names(x)[i], i)
    )
  })

  item_names <- names(x)
  if (!is.null(item_names) && all(!is.na(item_names) & nzchar(item_names))) {
    names(items) <- item_names
  }

  list(
    schema_version = anon_json_schema_version(),
    kind = "anon_context_collection",
    items = items
  )
}

#' @exportS3Method
as_anon_payload.anon_inventory <- function(x, path = "$") {
  c(
    list(
      schema_version = anon_json_schema_version(),
      kind = "anon_inventory"
    ),
    serialize_table_payload(x, path = path)
  )
}

#' @exportS3Method
as_anon_payload.anon_data_summary <- function(x, path = "$") {
  payload <- list(
    schema_version = anon_json_schema_version(),
    kind = "anon_data_summary"
  )

  summary_payload <- serialize_data_summary_payload(x, path = path)
  payload[names(summary_payload)] <- summary_payload
  payload
}

#' @exportS3Method
as_anon_payload.anon_report <- function(x, path = "$") {
  payload <- list(
    schema_version = anon_json_schema_version(),
    kind = "anon_report"
  )

  if (!is.null(x$inventory)) {
    payload$inventory <- serialize_table_payload(
      x$inventory,
      path = paste0(path, "$inventory")
    )
  }

  if (!is.null(x$data_summary)) {
    payload$data_summary <- serialize_data_summary_payload(
      x$data_summary,
      path = paste0(path, "$data_summary")
    )
  }

  payload
}

#' @exportS3Method
as_anon_payload.anon_text_compare <- function(x, path = "$") {
  payload <- list(
    schema_version = anon_json_schema_version(),
    kind = "anon_text_compare"
  )

  if (!is.null(x$summary)) {
    payload$summary <- serialize_table_payload(
      x$summary,
      path = paste0(path, "$summary")
    )
  }

  if (!is.null(x$details)) {
    payload$details <- serialize_table_payload(
      x$details,
      path = paste0(path, "$details")
    )
  }

  payload
}

#' @exportS3Method
as_anon_payload.anon_prompt_bundle <- function(x, path = "$") {
  payload <- attr(x, "payload", exact = TRUE)

  if (!is.null(payload)) {
    return(payload)
  }

  list(
    schema_version = anon_json_schema_version(),
    kind = "anon_prompt_bundle",
    output_format = attr(x, "output_format", exact = TRUE),
    content = as.character(unclass(x))
  )
}

anon_json_schema_version <- function() {
  1L
}

build_anon_prompt_bundle_payload <- function(
  report = NULL,
  text = NULL,
  comparison = NULL,
  title = "Anonymized Prompt Context",
  include_inventory = TRUE,
  include_data_summary = TRUE,
  include_text = TRUE,
  include_comparison = TRUE
) {
  payload <- list(
    schema_version = anon_json_schema_version(),
    kind = "anon_prompt_bundle",
    title = title
  )

  if (!is.null(report)) {
    report_payload <- list()

    if (isTRUE(include_inventory) && !is.null(report$inventory)) {
      report_payload$inventory <- serialize_table_payload(
        report$inventory,
        path = "$.report$inventory"
      )
    }

    if (isTRUE(include_data_summary) && !is.null(report$data_summary)) {
      report_payload$data_summary <- serialize_data_summary_payload(
        report$data_summary,
        path = "$.report$data_summary"
      )
    }

    if (length(report_payload) > 0L) {
      payload$report <- report_payload
    }
  }

  if (isTRUE(include_text) && !is.null(text)) {
    payload$text <- serialize_supported_value(as.character(text), path = "$.text")
  }

  if (isTRUE(include_comparison) && !is.null(comparison)) {
    payload$comparison <- as_anon_payload(comparison, path = "$.comparison")
    payload$comparison$schema_version <- NULL
    payload$comparison$kind <- NULL
  }

  payload
}

serialize_data_summary_payload <- function(x, path = "$") {
  payload <- list()

  if (!is.null(x$summary)) {
    payload$summary <- serialize_table_payload(
      x$summary,
      path = paste0(path, "$summary")
    )
  }

  if (!is.null(x$data_frames)) {
    data_frames_payload <- list()

    if (!is.null(x$data_frames$structure)) {
      data_frames_payload$structure <- serialize_table_payload(
        x$data_frames$structure,
        path = paste0(path, "$data_frames$structure")
      )
    }

    if (!is.null(x$data_frames$variables)) {
      data_frames_payload$variables <- serialize_named_tables(
        x$data_frames$variables,
        path = paste0(path, "$data_frames$variables")
      )
    }

    if (length(data_frames_payload) > 0L) {
      payload$data_frames <- data_frames_payload
    }
  }

  if (!is.null(x$examples)) {
    examples_payload <- list()

    if (!is.null(x$examples$row_method)) {
      examples_payload$row_method <- x$examples$row_method
    }

    if (!is.null(x$examples$rows)) {
      examples_payload$rows <- serialize_named_tables(
        x$examples$rows,
        path = paste0(path, "$examples$rows")
      )
    }

    scenarios <- get_example_scenarios(x$examples)
    if (length(scenarios) > 0L) {
      examples_payload$scenarios <- lapply(seq_along(scenarios), function(i) {
        scenario <- scenarios[[i]]
        scenario_payload <- list(
          key = serialize_supported_value(
            scenario$key,
            path = paste0(path, "$examples$scenarios[", i, "]$key")
          ),
          value = serialize_supported_value(
            scenario$value,
            path = paste0(path, "$examples$scenarios[", i, "]$value")
          ),
          method = serialize_supported_value(
            scenario$method,
            path = paste0(path, "$examples$scenarios[", i, "]$method")
          ),
          tables = serialize_named_tables(
            scenario$tables,
            path = paste0(path, "$examples$scenarios[", i, "]$tables")
          )
        )
        scenario_payload
      })
    }

    if (length(examples_payload) > 0L) {
      payload$examples <- examples_payload
    }
  }

  if (!is.null(x$other_objects)) {
    payload$other_objects <- serialize_table_payload(
      x$other_objects,
      path = paste0(path, "$other_objects")
    )
  }

  payload
}

serialize_named_tables <- function(x, path = "$") {
  out <- lapply(seq_along(x), function(i) {
    serialize_table_payload(
      x[[i]],
      path = json_child_path(path, names(x)[i], i)
    )
  })

  names(out) <- names(x)
  out
}

serialize_table_payload <- function(x, path = "$") {
  df <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  columns <- names(df)
  rows <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    row <- vector("list", length(df))
    names(row) <- columns

    for (j in seq_along(df)) {
      column <- df[[j]]
      value <- if (is.list(column) && !is.data.frame(column)) {
        column[[i]]
      } else {
        column[i]
      }

      row[[j]] <- serialize_supported_value(
        value,
        path = paste0(path, "$rows[", i, "]$", columns[j])
      )
    }

    rows[[i]] <- row
  }

  list(
    columns = columns,
    rows = rows
  )
}

serialize_supported_value <- function(x, path = "$") {
  if (is.null(x)) {
    return(NULL)
  }

  if (inherits(x, "anon_report")) {
    return(as_anon_payload.anon_report(x, path = path))
  }

  if (inherits(x, "anon_data_summary")) {
    return(as_anon_payload.anon_data_summary(x, path = path))
  }

  if (inherits(x, "anon_inventory")) {
    return(as_anon_payload.anon_inventory(x, path = path))
  }

  if (inherits(x, "anon_text_compare")) {
    return(as_anon_payload.anon_text_compare(x, path = path))
  }

  if (inherits(x, "anon_prompt_bundle")) {
    return(as_anon_payload.anon_prompt_bundle(x, path = path))
  }

  if (inherits(x, "anon_context_collection")) {
    return(as_anon_payload.anon_context_collection(x, path = path))
  }

  if (inherits(x, "anon_context")) {
    return(as_anon_payload.anon_context(x, path = path))
  }

  if (inherits(x, "Date") || inherits(x, "POSIXt") || inherits(x, "difftime")) {
    return(as.character(x))
  }

  if (is.factor(x)) {
    return(as.character(x))
  }

  if (is.matrix(x)) {
    return(serialize_table_payload(as.data.frame(x, stringsAsFactors = FALSE), path = path))
  }

  if (is.data.frame(x)) {
    return(serialize_table_payload(x, path = path))
  }

  if (is.object(x)) {
    unsupported_json_class_error(x, path = path)
  }

  if (is.list(x)) {
    return(serialize_list_payload(x, path = path))
  }

  if (is.atomic(x)) {
    return(serialize_atomic_payload(x, path = path))
  }

  unsupported_json_class_error(x, path = path)
}

serialize_list_payload <- function(x, path = "$") {
  out <- lapply(seq_along(x), function(i) {
    serialize_supported_value(
      x[[i]],
      path = json_child_path(path, names(x)[i], i)
    )
  })

  item_names <- names(x)
  if (!is.null(item_names) && all(!is.na(item_names) & nzchar(item_names))) {
    names(out) <- item_names
  }

  out
}

serialize_atomic_payload <- function(x, path = "$") {
  if (is.complex(x) || is.raw(x)) {
    unsupported_json_class_error(x, path = path)
  }

  out <- unname(x)
  item_names <- names(x)

  if (!is.null(item_names) && length(x) > 0L && all(!is.na(item_names) & nzchar(item_names))) {
    named_out <- lapply(seq_along(x), function(i) {
      serialize_supported_value(
        unname(x[i]),
        path = json_child_path(path, item_names[i], i)
      )
    })
    names(named_out) <- item_names
    return(named_out)
  }

  out
}

json_child_path <- function(path, name = NULL, index = NULL) {
  if (!is.null(name) && !is.na(name) && nzchar(name)) {
    paste0(path, "$", name)
  } else if (!is.null(index)) {
    paste0(path, "[", index, "]")
  } else {
    path
  }
}

unsupported_json_class_error <- function(x, path = "$") {
  stop(
    "Unsupported class for JSON serialization at ",
    path,
    ": ",
    paste(class(x), collapse = "/"),
    call. = FALSE
  )
}
