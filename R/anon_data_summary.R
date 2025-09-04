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
#' @inheritParams anon
#'
#' @return An object of class `"anon_data_summary"` containing:
#'   - `$summary`: A tibble with overall statistics (total objects, data frames count,
#'     other objects count, total memory usage)
#'   - `$data_frames`: A list with two elements (only present if data frames exist):
#'     - `$structure`: A tibble with structural information for each data frame
#'       (name, label, dimensions, memory size)
#'     - `$variables`: A tibble with detailed variable information including data types,
#'       missing values, distinct values, and labels
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
#' - Variable details: data types, missing value counts, distinct value counts, and variable labels
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
#'     )
#'   )
#'
#' @seealso [anon()] for the underlying anonymization function
#' @export
anon_data_summary <- function(
  envir = globalenv(),
  pattern_list = list(),
  default_replacement = getOption(
    "anon.default_replacement",
    default = "[REDACTED]"
  ),
  check_approximate = TRUE,
  max_distance = 2
) {
  name <- type <- NULL

  # Combine user arguments with global options
  option_pattern_list <- getOption("anon.pattern_list", default = list())

  # Combine pattern_list with option
  if (length(option_pattern_list) > 0) {
    pattern_list <- c(pattern_list, option_pattern_list)
  }

  pattern_replacements <- with_default_replacements(
    pattern_list,
    default_replacement = default_replacement
  )

  # when envir is passed as a list, name elements that are unnamed.
  if (!is.environment(envir)) {
    if (is.list(envir)) {
      # ideally, the content of the envir argument as a language object will
      # resolve to the length of the envir list. otherwise, will use indexed names.
      names_for_nameless <- as.character(sys.calls()[[1]]["envir"][[1]])[-1]
      if (length(names_for_nameless) != length(envir)) {
        names_for_nameless <- paste0("x", 1:length(envir))
      }

      # name elements that are not named.
      if (is.null(names(envir))) {
        names(envir) <- names_for_nameless
      } else {
        missing_names_lgl <- names(envir) == ""
        names(envir)[missing_names_lgl] <- names_for_nameless[missing_names_lgl]
      }
    }
  }

  obj_names <- objects(envir)

  summaries <- purrr::map(
    obj_names,
    ~ {
      obj <- envir[[.x]]
      get_object_summary(.x, obj)
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
      variables = purrr::map(data_frames, "variables") # Keep as separate list elements
    )
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
      sum(purrr::map_dbl(obj_names, ~ utils::object.size(envir[[.x]]))),
      units = "auto"
    )
  )

  # anonymize summaries.
  result <- anon(
    result,
    pattern_list = pattern_list,
    default_replacement = default_replacement,
    check_approximate = check_approximate,
    max_distance = max_distance,
    df_variable_names = FALSE,
    df_classes = FALSE,
    check_names = FALSE
  )

  class(result) <- c("anon_data_summary", "anon_context", "list")
  result
}


# helpers -----------------------------------------------------------------

get_object_summary <- function(obj_name, obj) {

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

    # Get data frame level label
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


# print method ------------------------------------------------------------

#' Print method for anonymized environment summary
#'
#' Displays an anonymized environment summary in a structured, readable format.
#' The output includes overall statistics, data frame structural information,
#' variable details, and information about other objects (if present).
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
#' 4. **Other Objects**: Information about non-data frame objects
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
    print(as.data.frame(x$summary))
    cat("\n")
  }

  if (!is.null(x$data_frames)) {
    if (!is.null(x$data_frames$structure)) {
      cat("Data Frames:\n")
      cat("------------\n")
      print(as.data.frame(x$data_frames$structure))
      cat("\n")
    }
    if (!is.null(x$data_frames$variables)) {
      purrr::iwalk(x$data_frames$variables, ~ {
        cat(sprintf("\nVariable Details (%s):\n\n", .y))
        cat(strrep("-", nchar(.y) + 18), "\n")
        print(as.data.frame(.x))
        cat("\n")
      })
    }
    cat("\n")
  }

  if (!is.null(x$other_objects)) {
    cat("Other Objects:\n")
    cat("--------------\n")
    print(as.data.frame(x$other_objects))
  }

  invisible(x)
}
