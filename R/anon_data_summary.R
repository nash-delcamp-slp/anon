#' Generate anonymized summary of data objects in an environment
#'
#' This function creates a summary of all objects (primarily data frames) in a specified
#' environment or list, then anonymizes the results using the same pattern matching
#' approach as [anon()]. It provides structural information about data frames
#' including dimensions, variable details, and memory usage while protecting sensitive
#' information through pattern-based redaction.
#'
#' @param envir An environment or list containing the objects to summarize. When passed as a list,
#'   unnamed elements will automatically be given names (either derived from the function call
#'   or indexed as "x1", "x2", etc.). Default is `globalenv()`.
#' @param selection Optional character vector of object names to include in the summary.
#' @param example_values_n Optional number of example unique values to include for
#'   discrete/text-like data frame columns. Defaults to `0`, which disables
#'   example values.
#' @param example_rows Optional example-row specification for data frames.
#'   Use `NULL` to disable examples, a single number to request that many rows
#'   per data frame, or [anon_example_rows()] to build a spec with explicit
#'   arguments such as `n`, `key`, `method`, and `n_key_values`.
#' @inheritParams anon
#'
#' @return An object of class `"anon_data_summary"` containing:
#'   - `$summary`: A tibble with overall statistics (total objects, data frames count,
#'     other objects count, total memory usage)
#'   - `$data_frames`: A list with two elements (only present if data frames exist):
#'     - `$structure`: A tibble with structural information for each data frame
#'       (name, label, dimensions, memory size)
#'     - `$variables`: A tibble with detailed variable information including data types,
#'       missing values, distinct values, labels, and optional example values
#'   - `$examples`: Optional data frame example payloads containing either sample
#'     rows per data frame or one or more keyed cross-source scenarios
#'   - All content is anonymized according to the specified patterns
#'
#' @details
#' The function operates in a few key steps:
#' 1. Generates detailed summaries for all objects
#' 2. Creates structured output with summary statistics and detailed information about data frames
#' 3. Applies anonymization using [anon()] with the provided patterns
#'
#' For data frames, the function captures:
#' - Structural information: dimensions, memory usage, and data frame-level labels
#' - Variable details: data types, missing value counts, distinct value counts,
#'   variable labels, and optional example values
#' - Optional example payloads: either sample rows or one or more keyed
#'   cross-source scenarios when configured
#'
#' The output includes a custom print method that displays the information in a
#' readable format while maintaining the anonymization.
#'
#' @examples
#' # Create study data with sensitive study codes in variable names
#' study_results <- data.frame(
#'   participant_id = c("P001", "P002", "P003"),
#'   ABC123_RESULT = c(85.2, 92.1, 78.5),
#'   ABC123_BASELINE = c(80.0, 88.3, 75.2),
#'   CBA321_RESULT = c(45.1, 52.3, 41.8),
#'   CBA321_BASELINE = c(42.0, 49.1, 39.5),
#'   age = c(45, 32, 67)
#' )
#'
#' # Study metadata containing the same sensitive study codes as values
#' study_metadata <- list(
#'   primary_study = "ABC123",
#'   secondary_study = "CBA321",
#'   principal_investigator = "Dr. Smith",
#'   site_location = "Boston Medical Center"
#' )
#'
#' # Create environment summary with anonymization
#' env_list <- list(study_results = study_results, metadata = study_metadata)
#'
#' # Use metadata values to inform anonymization patterns
#' # This will anonymize both the variable names (ABC123_RESULT, CBA321_RESULT, etc.)
#' # and the corresponding values in the metadata
#' env_list |>
#'   anon_data_summary(
#'     pattern_list = list(
#'       "STUDY_A" = study_metadata$primary_study,    # "ABC123"
#'       "STUDY_B" = study_metadata$secondary_study,  # "CBA321"
#'       "MEDICAL_CENTER" = "Boston Medical Center"
#'     ),
#'     example_values_n = 2,
#'     example_rows = anon_example_rows(n = 2, method = "random", seed = 42)
#'   )
#'
#' @seealso [anon()] for the underlying anonymization function
#' @export
anon_data_summary <- function(
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
  nlp_auto = getOption("anon.nlp_auto")
) {
  name <- type <- NULL

  # Combine user arguments with global options
  option_pattern_list <- getOption("anon.pattern_list", default = list())

  # Combine pattern_list with option
  if (length(option_pattern_list) > 0) {
    pattern_list <- c(pattern_list, option_pattern_list)
  }

  example_values_n <- normalize_example_values_n(example_values_n)
  example_rows <- normalize_example_rows_spec(example_rows)

  objects <- normalize_object_source(envir, selection = selection)
  obj_names <- names(objects)

  summaries <- purrr::map(
    obj_names,
    ~ {
      obj <- objects[[.x]]
      get_object_summary(
        .x,
        obj,
        example_values_n = example_values_n
      )
    }
  ) |>
    purrr::set_names(obj_names)

  data_frames <- summaries |>
    purrr::keep(~ is.list(.x) && "structure" %in% names(.x))

  other_objects <- summaries |>
    purrr::keep(~ tibble::is_tibble(.x))

  result <- list()

  if (length(data_frames) > 0) {
    result$data_frames <- list(
      structure = purrr::map_dfr(data_frames, "structure"),
      variables = purrr::map(data_frames, "variables")
    )
  }

  examples <- build_example_payload(
    objects,
    example_rows = example_rows
  )
  if (!is.null(examples)) {
    result$examples <- examples
  }

  if (length(other_objects) > 0) {
    result$other_objects <- dplyr::bind_rows(other_objects) |>
      dplyr::arrange(type, name)
  }

  result$summary <- tibble::tibble(
    total_objects = length(summaries),
    data_frames = length(data_frames),
    other_objects = length(other_objects),
    total_memory = format(
      sum(purrr::map_dbl(obj_names, ~ utils::object.size(objects[[.x]]))),
      units = "auto"
    )
  )

  result <- anon(
    result,
    pattern_list = pattern_list,
    default_replacement = default_replacement,
    check_approximate = check_approximate,
    max_distance = max_distance,
    df_variable_names = FALSE,
    df_classes = FALSE,
    check_names = FALSE,
    nlp_auto = nlp_auto
  )

  class(result) <- c("anon_data_summary", "anon_context", "list")
  result
}


# helpers -----------------------------------------------------------------

get_object_summary <- function(
  obj_name,
  obj,
  example_values_n = 0L
) {

  if (is.data.frame(obj)) {
    var_info <- obj |>
      purrr::map_dfr(~ {
        tibble::tibble(
          data_type = class(.x)[1],
          n_distinct = dplyr::n_distinct(.x, na.rm = TRUE),
          n_missing = sum(is.na(.x)),
          n_total = length(.x),
          pct_missing = round(sum(is.na(.x)) / length(.x) * 100, 2)
        )
      }, .id = "variable") |>
      dplyr::mutate(
        label = {
          labels <- purrr::map_chr(obj, ~ {
            lbl <- attr(.x, "label", exact = TRUE)
            if (!is.null(lbl)) as.character(lbl) else NA_character_
          })
          labels
        }
      )

    if (example_values_n > 0L) {
      var_info$example_values <- purrr::map(
        obj,
        ~ get_example_values(.x, n = example_values_n)
      )
    }

    df_label <- attr(obj, "label", exact = TRUE)

    structure_info <- tibble::tibble(
      name = obj_name,
      label = df_label,
      type = "data.frame",
      n_rows = nrow(obj),
      n_cols = ncol(obj),
      memory_size = format(utils::object.size(obj), units = "auto")
    )

    return(list(
      structure = structure_info,
      variables = var_info
    ))
  } else if (!is.list(obj) && is.vector(obj) || is.factor(obj)) {

    return(tibble::tibble(
      name = obj_name,
      type = class(obj)[1],
      length = length(obj),
      n_distinct = dplyr::n_distinct(obj, na.rm = TRUE),
      n_missing = sum(is.na(obj)),
      pct_missing = round(sum(is.na(obj)) / length(obj) * 100, 2),
      memory_size = format(utils::object.size(obj), units = "auto")
    ))

  } else if (is.list(obj) && !is.data.frame(obj)) {

    return(tibble::tibble(
      name = obj_name,
      type = "list",
      length = length(obj),
      element_types = paste(unique(purrr::map_chr(obj, ~ class(.x)[1])), collapse = ", "),
      memory_size = format(utils::object.size(obj), units = "auto")
    ))

  } else {

    return(tibble::tibble(
      name = obj_name,
      type = class(obj)[1],
      memory_size = format(utils::object.size(obj), units = "auto")
    ))
  }
}


#' Build an example-row specification for [anon_data_summary()]
#'
#' @param n Number of rows to include in each example payload.
#' @param key Optional shared grouping key for cross-source scenarios.
#' @param method Selection method. `"random"` samples rows or key values,
#'   `"first"` uses the first encountered rows or key values, and `"last"`
#'   uses the last encountered rows or key values.
#' @param value Optional explicit key value for scenario mode. Requires `key` and
#'   takes precedence over `method` when supplied.
#' @param n_key_values Number of keyed scenario values to include when `key` is
#'   provided and `value` is not. Defaults to `1`.
#' @param seed Optional integer seed used when `method = "random"`.
#'
#' @return A list of class `"anon_example_rows_spec"`.
#'
#' @export
anon_example_rows <- function(
  n,
  key = NULL,
  method = c("random", "first", "last"),
  value = NULL,
  n_key_values = 1,
  seed = NULL
) {
  method <- match.arg(method)

  spec <- list(
    n = n,
    key = key,
    method = method,
    value = value,
    n_key_values = n_key_values,
    seed = seed
  )

  structure(spec, class = c("anon_example_rows_spec", "list"))
}

normalize_example_values_n <- function(example_values_n) {
  if (length(example_values_n) != 1 || is.na(example_values_n) || !is.numeric(example_values_n)) {
    stop("`example_values_n` must be a single non-negative number.", call. = FALSE)
  }

  example_values_n <- as.integer(example_values_n)
  if (example_values_n < 0L) {
    stop("`example_values_n` must be a single non-negative number.", call. = FALSE)
  }

  example_values_n
}

normalize_example_rows_spec <- function(example_rows) {
  if (is.null(example_rows) || identical(example_rows, FALSE)) {
    return(NULL)
  }

  if (is.numeric(example_rows) && length(example_rows) == 1 && !is.na(example_rows)) {
    example_rows <- anon_example_rows(n = as.integer(example_rows))
  }

  if (!is.list(example_rows)) {
    stop("`example_rows` must be NULL, a single number, or a list.", call. = FALSE)
  }

  n <- example_rows$n
  if (length(n) != 1 || is.na(n) || !is.numeric(n)) {
    stop("`example_rows$n` must be a single non-negative number.", call. = FALSE)
  }

  n <- as.integer(n)
  if (n < 0L) {
    stop("`example_rows$n` must be a single non-negative number.", call. = FALSE)
  }

  if (n == 0L) {
    return(NULL)
  }

  key <- example_rows$key %||% NULL
  if (!is.null(key)) {
    if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
      stop("`example_rows$key` must be a single non-empty string.", call. = FALSE)
    }
  }

  method <- example_rows$method %||% "random"
  if (!is.character(method) || length(method) != 1 || is.na(method)) {
    stop("`example_rows$method` must be one of: first, last, random.", call. = FALSE)
  }
  method <- match.arg(method, c("first", "last", "random"))

  value <- example_rows$value %||% NULL
  if (!is.null(value) && is.null(key)) {
    stop("`example_rows$value` requires `example_rows$key`.", call. = FALSE)
  }
  if (!is.null(value) && length(value) != 1) {
    stop("`example_rows$value` must be a single value.", call. = FALSE)
  }

  n_key_values <- example_rows$n_key_values %||% 1L
  if (length(n_key_values) != 1 || is.na(n_key_values) || !is.numeric(n_key_values)) {
    stop("`example_rows$n_key_values` must be a single positive whole number.", call. = FALSE)
  }
  n_key_values <- as.integer(n_key_values)
  if (n_key_values <= 0L) {
    stop("`example_rows$n_key_values` must be a single positive whole number.", call. = FALSE)
  }
  if (!is.null(value)) {
    n_key_values <- 1L
  }

  seed <- example_rows$seed %||% NULL
  if (!is.null(seed)) {
    if (length(seed) != 1 || is.na(seed) || !is.numeric(seed)) {
      stop("`example_rows$seed` must be a single number or NULL.", call. = FALSE)
    }
    seed <- as.integer(seed)
  }

  structure(
    list(
      n = n,
      key = key,
      method = method,
      value = value,
      n_key_values = n_key_values,
      seed = seed
    ),
    class = c("anon_example_rows_spec", "list")
  )
}

get_example_values <- function(x, n = 0L) {
  if (n <= 0L || !supports_example_values(x)) {
    return(character(0))
  }

  values <- as.character(x)
  values <- values[!is.na(values)]

  if (length(values) == 0L) {
    return(character(0))
  }

  unique_values <- unique(values)
  unique_values[seq_len(min(length(unique_values), n))]
}

supports_example_values <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x)
}

build_example_payload <- function(objects, example_rows = NULL) {
  if (is.null(example_rows)) {
    return(NULL)
  }

  data_frames <- objects[vapply(objects, is.data.frame, logical(1))]
  if (length(data_frames) == 0L) {
    return(NULL)
  }

  if (is.null(example_rows$key)) {
    rows <- purrr::map2(
      data_frames,
      seq_along(data_frames),
      ~ select_example_rows(
        .x,
        n = example_rows$n,
        method = example_rows$method,
        seed = derive_example_seed(example_rows$seed, .y - 1L)
      )
    )
    names(rows) <- names(data_frames)
    rows <- rows[vapply(rows, nrow, integer(1)) > 0L]

    if (length(rows) == 0L) {
      return(NULL)
    }

    return(list(rows = rows, row_method = example_rows$method))
  }

  build_keyed_examples(
    data_frames = data_frames,
    key = example_rows$key,
    n = example_rows$n,
    method = example_rows$method,
    value = example_rows$value,
    n_key_values = example_rows$n_key_values,
    seed = example_rows$seed
  )
}

build_keyed_examples <- function(
  data_frames,
  key,
  n,
  method = "first",
  value = NULL,
  n_key_values = 1L,
  seed = NULL
) {
  keyed_tables <- data_frames[vapply(data_frames, function(x) key %in% names(x), logical(1))]
  if (length(keyed_tables) == 0L) {
    return(NULL)
  }

  scenario_values <- select_example_key_values(
    keyed_tables = keyed_tables,
    key = key,
    method = method,
    value = value,
    n_key_values = n_key_values,
    seed = seed
  )

  if (length(scenario_values) == 0L) {
    return(NULL)
  }

  scenarios <- purrr::imap(
    scenario_values,
    ~ build_single_scenario(
      keyed_tables = keyed_tables,
      key = key,
      value = .x,
      method = method,
      n = n,
      seed = derive_example_seed(seed, .y * 1000L)
    )
  )
  scenarios <- purrr::compact(scenarios)

  if (length(scenarios) == 0L) {
    return(NULL)
  }

  result <- list(scenarios = scenarios)
  if (length(scenarios) == 1L) {
    result$scenario <- scenarios[[1]]
  }

  result
}

build_single_scenario <- function(keyed_tables, key, value, method = "first", n, seed = NULL) {
  scenario_tables <- purrr::map2(
    keyed_tables,
    seq_along(keyed_tables),
    ~ select_matching_rows(
      .x,
      key = key,
      value = value,
      n = n,
      method = method,
      seed = derive_example_seed(seed, .y - 1L)
    )
  )
  names(scenario_tables) <- names(keyed_tables)
  scenario_tables <- scenario_tables[vapply(scenario_tables, nrow, integer(1)) > 0L]

  if (length(scenario_tables) == 0L) {
    return(NULL)
  }

  list(
    key = key,
    value = value,
    method = method,
    tables = scenario_tables
  )
}

select_example_key_values <- function(
  keyed_tables,
  key,
  method = "first",
  value = NULL,
  n_key_values = 1L,
  seed = NULL
) {
  if (!is.null(value)) {
    return(list(value))
  }

  candidates <- collect_key_value_candidates(keyed_tables, key)
  if (length(candidates) == 0L) {
    return(list())
  }

  indices <- select_example_indices(
    length(candidates),
    n = n_key_values,
    method = method,
    seed = seed
  )
  as.list(candidates[indices])
}

collect_key_value_candidates <- function(keyed_tables, key) {
  values <- unlist(
    lapply(keyed_tables, function(df) as.character(df[[key]])),
    use.names = FALSE
  )
  values <- values[!is.na(values)]
  unique(values)
}

select_example_rows <- function(df, n, method = "first", seed = NULL) {
  index <- select_example_indices(nrow(df), n = n, method = method, seed = seed)
  if (length(index) == 0L) {
    return(df[0, , drop = FALSE])
  }

  df[index, , drop = FALSE]
}

select_example_indices <- function(n_total, n, method = "first", seed = NULL) {
  n_select <- min(n_total, n)
  if (n_select <= 0L) {
    return(integer(0))
  }

  if (method == "first") {
    return(seq_len(n_select))
  }

  if (method == "last") {
    return(seq.int(n_total - n_select + 1L, n_total))
  }

  sort(sample_int_with_seed(n_total, n_select, seed = seed))
}

choose_example_index <- function(n_total, method = "first", seed = NULL) {
  if (n_total <= 0L) {
    return(integer(0))
  }

  if (method == "first") {
    return(1L)
  }

  if (method == "last") {
    return(as.integer(n_total))
  }

  sample_int_with_seed(n_total, 1L, seed = seed)
}

sample_int_with_seed <- function(n_total, size, seed = NULL) {
  if (is.null(seed)) {
    return(sample.int(n_total, size = size, replace = FALSE))
  }

  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  }

  set.seed(seed)
  sample.int(n_total, size = size, replace = FALSE)
}

derive_example_seed <- function(seed, offset = 0L) {
  if (is.null(seed)) {
    return(NULL)
  }

  as.integer(seed + offset)
}

select_matching_rows <- function(df, key, value, n, method = "first", seed = NULL) {
  matches <- !is.na(df[[key]]) & values_match(df[[key]], value)
  matched_df <- df[matches, , drop = FALSE]
  if (nrow(matched_df) == 0L) {
    return(matched_df)
  }

  select_example_rows(matched_df, n = n, method = method, seed = seed)
}

values_match <- function(x, value) {
  as.character(x) == as.character(value)
}


get_example_scenarios <- function(examples) {
  if (is.null(examples)) {
    return(list())
  }

  if (!is.null(examples$scenarios)) {
    return(examples$scenarios)
  }

  if (!is.null(examples$scenario)) {
    return(list(examples$scenario))
  }

  list()
}


# print method ------------------------------------------------------------

#' Print method for anonymized environment summary
#'
#' Displays an anonymized environment summary in a structured, readable format.
#' The output includes overall statistics, data frame structural information,
#' variable details, optional example payloads, and information about other
#' objects (if present).
#'
#' @param x An object of class `"anon_data_summary"` created by [anon_data_summary()]
#' @param ... Additional arguments passed to print methods (currently unused)
#'
#' @return Invisibly returns the input object `x`
#'
#' @details
#' The print method displays information in the following order:
#' 1. **Environment Data Summary**: Overall statistics including total objects,
#'    data frame count, other object count, and total memory usage
#' 2. **Data Frames**: Structural information for each data frame
#' 3. **Variable Details**: Detailed information about variables within data frames
#' 4. **Examples**: Optional sample rows or keyed scenarios when configured
#' 5. **Other Objects**: Information about non-data frame objects
#'
#' All displayed content respects the anonymization patterns applied during
#' the creation of the summary object.
#'
#' @examples
#' \dontrun{
#' # Create and print an anonymized summary
#' summary_result <- anon_data_summary(
#'   list(data = mtcars),
#'   pattern_list = list("CAR" = "Mazda|Merc")
#' )
#' print(summary_result)
#' }
#'
#' @export
print.anon_data_summary <- function(x, ...) {
  cat("Environment Data Summary\n")
  cat("========================\n\n")

  if (!is.null(x$summary)) {
    print(format_table_for_display(x$summary))
    cat("\n")
  }

  if (!is.null(x$data_frames)) {
    if (!is.null(x$data_frames$structure)) {
      cat("Data Frames:\n")
      cat("------------\n")
      print(format_table_for_display(x$data_frames$structure))
      cat("\n")
    }
    if (!is.null(x$data_frames$variables)) {
      purrr::iwalk(x$data_frames$variables, ~ {
        cat(sprintf("\nVariable Details (%s):\n\n", .y))
        cat(strrep("-", nchar(.y) + 18), "\n")
        print(format_table_for_display(.x))
        cat("\n")
      })
    }
    cat("\n")
  }

  if (!is.null(x$examples$rows)) {
    purrr::iwalk(x$examples$rows, ~ {
      cat(sprintf("Example Rows (%s):\n\n", .y))
      cat(strrep("-", nchar(.y) + 16), "\n")
      print(format_table_for_display(.x))
      cat("\n")
    })
  }

  scenarios <- get_example_scenarios(x$examples)
  if (length(scenarios) > 0L) {
    purrr::iwalk(scenarios, ~ {
      scenario_value <- format_example_scalar(.x$value)
      heading <- if (length(scenarios) == 1L) {
        sprintf("Example Scenario (%s = %s):", .x$key, scenario_value)
      } else {
        sprintf("Example Scenario %s (%s = %s):", .y, .x$key, scenario_value)
      }
      cat(heading, "\n")
      cat(strrep("-", nchar(heading)), "\n\n")
      purrr::iwalk(.x$tables, ~ {
        cat(sprintf("%s:\n", .y))
        print(format_table_for_display(.x))
        cat("\n")
      })
    })
  }

  if (!is.null(x$other_objects)) {
    cat("Other Objects:\n")
    cat("--------------\n")
    print(format_table_for_display(x$other_objects))
  }

  invisible(x)
}
