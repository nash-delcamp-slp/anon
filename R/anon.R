#' Anonymize sensitive information in R objects
#'
#' This function anonymizes or redacts sensitive information from various R objects
#' including character vectors, factors, data frames, and lists. It uses pattern
#' matching to find and replace sensitive content, with options for targeted
#' anonymization based on variable names or classes and warnings about approximate matches.
#'
#' @param x The object to anonymize. Can be a character vector, factor, data frame, or list.
#' @param pattern_list A list of patterns to search for and replace. Can include:
#'   - Named elements where names are replacement values and values are one or more patterns to match
#'   - Unnamed elements where one or more patterns are replaced with `default_replacement`
#'   This parameter is combined with the global option `getOption("anon.pattern_list")`.
#' @param default_replacement Value to use as the default replacement value when no specific replacement
#' is provided. Default is `getOption("anon.default_replacement", default = "\[REDACTED\]")`.
#' @param check_approximate Logical indicating whether to check for approximate
#'   matches using string distance. Default is `TRUE`.
#' @param max_distance Maximum string distance for approximate matching when
#'   `check_approximate` is `TRUE`. Default is `2`.
#' @param df_variable_names For data frames, a character vector or named list
#'   specifying which variable names should be anonymized:
#'   - Unnamed elements: variables are replaced with `default_replacement`
#'   - Named elements: variable names are keys, value can be either a replacement value or a function
#'   This parameter is combined with the global option `getOption("anon.df_variable_names")`.
#' @param df_classes For data frames, a character vector or named list specifying
#'   which variable classes should be anonymized:
#'   - Unnamed elements: variables with matching classes are replaced with `default_replacement`
#'   - Named elements: class names are keys, value can be either a replacement value or a function
#'   This parameter is combined with the global option `getOption("anon.df_classes")`.
#' @param check_names Logical indicating whether to anonymize object names
#'   (column names, row names, list names). Default is `TRUE`.
#' @param check_labels Logical indicating whether to anonymize labels (attributes).
#'   Default is `TRUE`.
#' @param nlp_auto List of logical values with names corresponding to entity names. Can be
#'   generated with [`nlp_auto()`] and can be set as the `anon.nlp_auto` global option.
#'   This argument overrides the global option.
#' @param .self Logical for internal use only. Used in recursive calls. Default is `FALSE`.
#'   When `TRUE`, warnings are collected as attributes instead of being issued immediately
#'   and global options are ignored and only explicitly provided parameters are used.
#'
#' @return An object of class `anon_context` with the same structure as `x` but with sensitive
#'   information replaced. If approximate matches are found and `.self` is `FALSE`, warnings are issued.
#'   If `.self` is `TRUE`, warnings are attached as an attribute.
#'
#' @details
#' `anon()` operates recursively on nested structures. For data frames:
#' - Individual columns are processed based on their content
#' - Entire columns can be replaced if specified in `df_variable_names` or `df_classes`
#' - Column names, row names, and labels are anonymized when enabled
#'
#' Pattern matching is case-insensitive. When `check_approximate` is enabled,
#' `anon()` will warn about remaining potential matches that are similar but not exact.
#'
#' Replacement functions can be provided in `df_variable_names` and `df_classes` as:
#' - R functions that take the column/variable as input
#' - Formula notation (e.g., `~ .x + rnorm(length(.x), mean = 1)`)
#'
#' The returned object has class `anon_context` which allows it to be combined with other
#' anonymized objects using `c()`.
#'
#' @section Global Options:
#' The following global options affect function behavior:
#'
#' \describe{
#'   \item{`anon.default_replacement`}{Default replacement text (default: "\[REDACTED\]").}
#'   \item{`anon.pattern_list`}{Global patterns to combine with (after) `pattern_list` parameter.}
#'   \item{`anon.df_variable_names`}{Global variable name specifications to combine with (after)
#'         `df_variable_names` parameter.}
#'   \item{`anon.df_classes`}{Global class specifications to combine with (after) `df_classes` parameter.}
#'   \item{`anon.nlp_auto`}{List of logical values indicating which NLP entity types should be
#'         automatically anonymized. Use [`nlp_auto()`] to generate this list. Override the option by setting the `nlp_auto` argument.}
#' }
#'
#' To set global options:
#' ```r
#' options(anon.pattern_list = list("EMAIL" = "@\\S+"))
#' options(anon.df_variable_names = c("name", "email"))
#' options(anon.default_replacement = "[HIDDEN]")
#' options(anon.nlp_auto = nlp_auto(person = TRUE))
#' ```
#'
#' @examples
#' # Basic string anonymization
#' text <- c("John Smith", "jane.doe@email.com", "Call 555-1234")
#' anon(text, pattern_list = c("John Smith", "@\\S+", "\\d{3}-\\d{4}"))
#'
#' # Using named patterns for specific replacements
#' anon(text, pattern_list = list("PERSON" = "John Smith",
#'                                "EMAIL" = "@\\S+",
#'                                "PHONE" = "\\d{3}-\\d{4}"))
#'
#' # Data frame anonymization
#' df <- data.frame(
#'   name = c("Alice", "Bob"),
#'   email = c("alice@test.com", "bob@test.com"),
#'   score = c(95, 87)
#' )
#'
#' # Anonymize specific columns by name
#' anon(df, df_variable_names = c("name", "email"))
#'
#' # Anonymize columns by class with custom replacements
#' anon(df, df_classes = list("character" = "HIDDEN"))
#'
#' # Using functions for dynamic replacement
#' anon(df, df_variable_names = list("name" = ~ paste("Person", seq_along(.x))))
#'
#' anon_df <- df |>
#'   anon(
#'     df_variable_names = list(
#'       "name" = ~ paste("Person", seq_along(.x)),
#'       "email"
#'     )
#'   )
#'
#' # Using global options
#' options(anon.pattern_list = list("EMAIL" = "@\\S+"))
#' options(anon.df_variable_names = "name")
#' anon(df)  # Will anonymize emails and names using global settings
#'
#' # Combine anonymized objects
#' anon_summary <- anon_data_summary(list(df = df))
#' combined <- c(anon_df, anon_summary)
#' combined
#'
#' @export
anon <- function(
  x,
  pattern_list = list(),
  default_replacement = getOption(
    "anon.default_replacement",
    default = "[REDACTED]"
  ),
  check_approximate = TRUE,
  max_distance = 2,
  df_variable_names = NULL,
  df_classes = NULL,
  check_names = TRUE,
  check_labels = TRUE,
  nlp_auto = getOption("anon.nlp_auto"),
  .self = FALSE
) {
  # Combine user arguments with global options
  if (!.self) {
    option_pattern_list <- getOption("anon.pattern_list", default = list())
    option_df_variable_names <- getOption(
      "anon.df_variable_names",
      default = NULL
    )
    option_df_classes <- getOption("anon.df_classes", default = NULL)

    # Combine pattern_list with option
    if (length(option_pattern_list) > 0) {
      pattern_list <- c(pattern_list, option_pattern_list)
    }

    # Combine df_variable_names with option
    # Internally, df_variable_names might be passed as FALSE to ignore the option.
    if (!is.null(option_df_variable_names) && !isFALSE(df_variable_names)) {
      if (is.null(df_variable_names)) {
        df_variable_names <- option_df_variable_names
      } else {
        df_variable_names <- c(df_variable_names, option_df_variable_names)
      }
    }

    # Combine df_classes with option
    # Internally, df_classes might be passed as FALSE to ignore the option.
    if (!is.null(option_df_classes) && !isFALSE(df_classes)) {
      if (is.null(df_classes)) {
        df_classes <- option_df_classes
      } else {
        df_classes <- c(df_classes, option_df_classes)
      }
    }
  }

  # Track warnings at the anon() level
  approximate_warnings <- character(0)

  apply_patterns <- function(
    text,
    pattern_replacements,
    check_approximate,
    max_distance
  ) {
    protected_replacements <- function(text, patterns, replacement) {
      # Collect existing protected mappings
      text_protected_mappings <- attr(text, "protected_mappings")

      # Use a format that's very unlikely to include patterns that are being replaced
      protected_token <- digest::digest(replacement)

      # Do the replacement with protected token
      result <- tryCatch(
        {
          stringr::str_replace_all(text, patterns, protected_token)
        },
        error = function(e) {
          # If regex fails, try with fixed string matching
          stringr::str_replace_all(
            text,
            stringr::fixed(as.character(patterns)),
            protected_token
          )
        }
      )

      # Store mapping for final cleanup
      attr(result, "protected_mappings") <- c(
        text_protected_mappings,
        setNames(replacement, protected_token)
      )

      return(result)
    }

    finalize_replacements <- function(text) {
      mappings <- attr(text, "protected_mappings")
      if (!is.null(mappings)) {
        for (i in seq_along(mappings)) {
          text <- stringr::str_replace_all(
            text,
            stringr::fixed(names(mappings)[i]),
            mappings[i]
          )
        }
      }
      return(text)
    }

    result <- text

    for (i in seq_along(pattern_replacements)) {
      result <- protected_replacements(
        result,
        patterns = stringr::regex(
          pattern_replacements[[i]][[1]],
          ignore_case = TRUE
        ),
        replacement = pattern_replacements[[i]][[2]]
      )
    }
    result <- finalize_replacements(result)

    # Check for approximate matches if enabled
    if (isTRUE(check_approximate)) {
      for (i in seq_along(pattern_replacements)) {
        pattern <- pattern_replacements[[i]][[1]]

        # Use the helper function for approximate matching
        match_results <- compute_approximate_distances(
          result,
          pattern,
          max_distance
        )

        if (length(match_results$matching_strings) > 0) {
          warning_msgs <- paste0(
            "Potential approximate match: '",
            match_results$matching_strings,
            "' is similar to pattern '",
            pattern,
            "'"
          )
          # Add to parent scope warnings instead of warning immediately
          approximate_warnings <<- c(approximate_warnings, warning_msgs)
        }
      }
    }

    result
  }

  # Helper function to apply replacement (formula, function, or constant)
  apply_replacement <- function(replacement, data) {
    if (rlang::is_formula(replacement)) {
      # Convert formula to function and apply to data
      func <- rlang::as_function(replacement)
      return(func(data))
    } else if (is.function(replacement)) {
      # Apply function directly to data
      return(replacement(data))
    } else {
      # Return constant value
      return(replacement)
    }
  }

  # Helper function to get replacement value for a variable name
  get_variable_replacement <- function(
    col_name,
    col_data,
    df_variable_names,
    default_replacement
  ) {
    if (is.null(df_variable_names)) {
      return(NULL)
    }

    if (is.null(names(df_variable_names))) {
      # Unnamed vector - check if variable name is in the list
      if (col_name %in% df_variable_names) {
        return(default_replacement)
      }
    } else {
      # Named vector - check for exact match and return the assigned value
      if (col_name %in% names(df_variable_names)) {
        replacement <- df_variable_names[[col_name]]
        return(apply_replacement(replacement, col_data))
      }
      # Also check unnamed elements
      unnamed_elements <- df_variable_names[names(df_variable_names) == ""]
      if (length(unnamed_elements) > 0 && col_name %in% unnamed_elements) {
        return(default_replacement)
      }
    }
    return(NULL)
  }

  # Helper function to get replacement value for a variable class
  get_class_replacement <- function(var, df_classes, default_replacement) {
    if (is.null(df_classes)) {
      return(NULL)
    }

    var_classes <- class(var)

    if (is.null(names(df_classes))) {
      # Unnamed vector - check if any class matches
      if (any(tolower(var_classes) %in% tolower(df_classes))) {
        return(default_replacement)
      }
    } else {
      # Named vector - check for exact match and return the assigned value
      for (class_name in names(df_classes)) {
        if (tolower(class_name) %in% tolower(var_classes)) {
          replacement <- df_classes[[class_name]]
          return(apply_replacement(replacement, var))
        }
      }
      # Also check unnamed elements
      unnamed_elements <- df_classes[names(df_classes) == ""]
      if (
        length(unnamed_elements) > 0 &&
          any(tolower(var_classes) %in% tolower(unnamed_elements))
      ) {
        return(default_replacement)
      }
    }
    return(NULL)
  }

  # Handle automatic/provided NLP anonymization
  enabled_nlp_entities <- get_enabled_nlp_entities(nlp_auto)
  if (length(enabled_nlp_entities) > 0) {
    # Extract NLP entities and add to pattern_list for processing
    nlp_pattern_list <- extract_nlp_patterns(x, enabled_nlp_entities)
    if (length(nlp_pattern_list) > 0) {
      pattern_list <- c(pattern_list, nlp_pattern_list)
    }
  }

  pattern_replacements <- with_default_replacements(
    pattern_list,
    default_replacement = default_replacement
  )

  # Dispatch based on object type (using the inner apply_patterns function)
  if (is.character(x) || is.factor(x)) {
    if (is.factor(x)) {
      levels(x) <- apply_patterns(
        levels(x),
        pattern_replacements,
        check_approximate,
        max_distance
      )
      result <- x
    } else {
      result <- apply_patterns(
        x,
        pattern_replacements,
        check_approximate,
        max_distance
      )
    }
  } else if (is.data.frame(x)) {
    result <- x

    # Process each column
    for (col_name in names(result)) {
      # Check if this variable should be anonymized and get replacement values
      var_name_replacement <- get_variable_replacement(
        col_name,
        result[[col_name]],
        df_variable_names,
        default_replacement
      )
      var_class_replacement <- get_class_replacement(
        result[[col_name]],
        df_classes,
        default_replacement
      )

      # Anonymize column-level label only if check_labels is TRUE
      if (isTRUE(check_labels)) {
        col_label <- attr(result[[col_name]], "label", exact = TRUE)
        if (!is.null(col_label)) {
          attr(result[[col_name]], "label") <- apply_patterns(
            col_label,
            pattern_replacements,
            check_approximate,
            max_distance
          )
        }
      }

      # Replace entire column with constant value if specified
      if (!is.null(var_name_replacement)) {
        result[[col_name]] <- var_name_replacement
      } else if (!is.null(var_class_replacement)) {
        result[[col_name]] <- var_class_replacement
      } else {
        # Use .self = TRUE for recursive calls to collect warnings
        recursive_result <- anon(
          result[[col_name]],
          pattern_list = pattern_list,
          default_replacement = default_replacement,
          check_approximate = check_approximate,
          max_distance = max_distance,
          df_variable_names = NULL, # Don't pass these down to avoid recursion
          df_classes = NULL,
          check_names = check_names,
          check_labels = check_labels,
          .self = TRUE
        )

        # If recursive call has warnings, collect them
        if (!is.null(attr(recursive_result, "approximate_warnings"))) {
          approximate_warnings <- c(
            approximate_warnings,
            attr(recursive_result, "approximate_warnings")
          )
          attr(recursive_result, "approximate_warnings") <- NULL # Remove the attribute
        }

        result[[col_name]] <- recursive_result
      }
    }

    # Anonymize column names only if check_names is TRUE
    if (isTRUE(check_names) && !is.null(names(result))) {
      names(result) <- apply_patterns(
        names(result),
        pattern_replacements,
        check_approximate,
        max_distance
      )
    }

    # Anonymize row names only if check_names is TRUE
    if (
      isTRUE(check_names) &&
        !is.null(rownames(result)) &&
        !identical(rownames(result), as.character(seq_len(nrow(result))))
    ) {
      rownames(result) <- apply_patterns(
        rownames(result),
        pattern_replacements,
        check_approximate,
        max_distance
      )
    }

    # Anonymize data frame level label only if check_labels is TRUE
    if (isTRUE(check_labels)) {
      df_label <- attr(result, "label", exact = TRUE)
      if (!is.null(df_label)) {
        attr(result, "label") <- apply_patterns(
          df_label,
          pattern_replacements,
          check_approximate,
          max_distance
        )
      }
    }
  } else if (is.list(x)) {
    result <- purrr::map(
      x,
      ~ {
        # Use .self = TRUE for recursive calls to collect warnings
        recursive_result <- anon(
          .x,
          pattern_list = pattern_list,
          default_replacement = default_replacement,
          check_approximate = check_approximate,
          max_distance = max_distance,
          df_variable_names = df_variable_names,
          df_classes = df_classes,
          check_names = check_names,
          check_labels = check_labels,
          .self = TRUE
        )

        # If recursive call has warnings, collect them
        if (!is.null(attr(recursive_result, "approximate_warnings"))) {
          approximate_warnings <<- c(
            approximate_warnings,
            attr(recursive_result, "approximate_warnings")
          )
          attr(recursive_result, "approximate_warnings") <- NULL # Remove the attribute
        }

        recursive_result
      }
    )

    # Anonymize list names only if check_names is TRUE
    if (isTRUE(check_names) && !is.null(names(result))) {
      names(result) <- apply_patterns(
        names(result),
        pattern_replacements,
        check_approximate,
        max_distance
      )
    }
  } else {
    result <- x
  }

  # Handle warnings based on .self parameter
  if (length(approximate_warnings) > 0) {
    unique_warnings <- unique(approximate_warnings)

    if (isTRUE(.self)) {
      # When called recursively, attach warnings as attribute instead of issuing them
      attr(result, "approximate_warnings") <- unique_warnings
    } else {
      # When called from top level, issue warnings normally
      rlang::warn(unique_warnings)
    }
  }

  if (isFALSE(.self)) {
    result <- new_anon_context(result)
  }
  result
}

# helpers -----------------------------------------------------------------

# Helper function for approximate distance matching
compute_approximate_distances <- function(text, pattern, max_distance = 2) {
  if (nchar(pattern) <= 3) {
    return(list(
      distances = integer(0),
      matches = integer(0),
      matching_strings = character(0)
    ))
  }

  text_lengths <- nchar(text)
  pattern_length <- nchar(pattern)

  # First check with fixed (non-partial) where the candidates are x and the pattern is y and insertions are
  # more costly.
  distances1 <- utils::adist(
    tolower(text),
    tolower(pattern),
    fixed = TRUE,
    ignore.case = TRUE,
    costs = c(insertions = 2, deletions = 1, substitutions = 1)
  )

  # Second check with partial matches where x and y are reverse of above and insertions are less
  # costly.
  distances2 <- utils::adist(
    tolower(pattern),
    tolower(text),
    partial = TRUE,
    ignore.case = TRUE,
    costs = c(insertions = 1, deletions = 5, substitutions = 5)
  )

  # Take minimum distance for each pattern (transpose distances2 since matrix is flipped)
  distances <- pmin(distances1, t(distances2))

  matches <- which(distances <= max_distance)

  # Extract the actual matching strings
  matching_strings <- if (length(matches) > 0) {
    text[matches]
  } else {
    character(0)
  }

  list(
    distances = distances,
    matches = matches,
    matching_strings = matching_strings
  )
}

with_default_replacements <- function(
  pattern_list,
  default_replacement
) {
  if (!is.list(pattern_list)) {
    pattern_list <- list(pattern_list)
  }

  result <- list()

  for (i in seq_along(pattern_list)) {
    element_name <- names(pattern_list)[i]
    element_value <- pattern_list[[i]]

    if (!is.null(element_name) && element_name != "") {
      # Named element: use structure (replacement_value -> pattern_key)
      if (length(element_value) == 1) {
        result[[length(result) + 1]] <- c(element_value, element_name)
      } else {
        # Multiple values map to same pattern key
        for (value in element_value) {
          result[[length(result) + 1]] <- c(value, element_name)
        }
      }
    } else {
      # Unnamed element: each value gets replaced with default_replacement
      for (value in element_value) {
        result[[length(result) + 1]] <- c(value, default_replacement)
      }
    }
  }

  # Validate all elements are proper pairs
  for (i in seq_along(result)) {
    stopifnot(length(result[[i]]) == 2)
    stopifnot(is.character(result[[i]]))
  }

  result
}
