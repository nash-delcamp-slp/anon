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
#'   matches using string distance. Default is
#'   `getOption("anon.check_approximate", default = FALSE)`.
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
#'   Class names are matched case-insensitively against `class()` of each column. `"double"`
#'   also matches plain double columns (not `Date` or `POSIXct`). Character columns also match
#'   `"double"`/`"numeric"`, `"integer"`, `"Date"` or `"POSIXct"` when every non-missing,
#'   non-blank value parses as that type (dates as ISO 8601); a replacement function then
#'   receives the parsed values.
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
#' @param .pattern_replacements List for internal use only. Pre-computed pattern replacement
#'   pairs passed down during recursive calls to avoid recomputing them for each column or
#'   list element. Default is `NULL`, which triggers normal computation.
#' @param .compiled List for internal use only. Pre-compiled pattern groups and the
#'   combined single-pass regex. Passed down during
#'   recursive calls to avoid recompiling patterns. Default is `NULL`.
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
#'   \item{`anon.nlp_default_replacements`}{Default NLP replacement labels. Use
#'         [`nlp_default_replacements()`] to generate this list.}
#'   \item{`anon.example_values_n`}{Default `example_values_n` used by [`anon_data_summary()`]
#'         and [`anon_report()`].}
#'   \item{`anon.example_rows`}{Default `example_rows` specification used by
#'         [`anon_data_summary()`] and [`anon_report()`]. Use [`anon_example_rows()`]
#'         to generate this value.}
#' }
#'
#' See [`anon_options()`] for a central helper that lists and sets all supported
#' `anon.*` options.
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
  check_approximate = getOption("anon.check_approximate", default = FALSE),
  max_distance = 2,
  df_variable_names = NULL,
  df_classes = NULL,
  check_names = TRUE,
  check_labels = TRUE,
  nlp_auto = getOption("anon.nlp_auto"),
  .self = FALSE,
  .pattern_replacements = NULL,
  .compiled = NULL
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

  apply_patterns <- function(text, compiled, check_approximate, max_distance) {
    if (length(compiled$groups) == 0) return(text)

    result <- replace_compiled(text, compiled)

    # Check for approximate matches if enabled
    if (isTRUE(check_approximate)) {
      approximate_text <- unique(result[!is.na(result)])

      batch_results <- compute_approximate_distances_batch(
        approximate_text,
        compiled$all_patterns,
        max_distance
      )

      if (length(batch_results) > 0) {
        approximate_warnings <<- c(approximate_warnings, batch_results)
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
    rule <- find_df_class_rule(var, df_classes)
    if (is.null(rule)) {
      return(NULL)
    }
    if (is.null(rule$replacement)) {
      return(default_replacement)
    }
    apply_replacement(rule$replacement, rule$value)
  }

  # Use pre-computed pattern_replacements if available (recursive calls)
  if (!is.null(.pattern_replacements)) {
    pattern_replacements <- .pattern_replacements
  } else {
    # Handle automatic/provided NLP anonymization
    enabled_nlp_entities <- get_enabled_nlp_entities(nlp_auto)
    if (length(enabled_nlp_entities) > 0) {
      nlp_pattern_list <- extract_nlp_patterns(
        nlp_text_source(x, df_variable_names, df_classes),
        enabled_nlp_entities
      )
      if (length(nlp_pattern_list) > 0) {
        pattern_list <- c(pattern_list, nlp_pattern_list)
      }
    }

    pattern_replacements <- with_default_replacements(
      pattern_list,
      default_replacement = default_replacement
    )
  }

  # Reuse pre-compiled patterns from parent call, or compile once
  compiled <- if (!is.null(.compiled)) .compiled else compile_patterns(pattern_replacements)

  # Dispatch based on object type (using the inner apply_patterns function)
  if (is.character(x) || is.factor(x)) {
    if (is.factor(x)) {
      levels(x) <- apply_patterns(
        levels(x),
        compiled,
        check_approximate,
        max_distance
      )
      result <- x
    } else {
      result <- apply_patterns(
        x,
        compiled,
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
            compiled,
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
      } else if (is.character(result[[col_name]])) {
        # Apply patterns directly for character columns (avoid full anon() overhead)
        result[[col_name]] <- apply_patterns(
          result[[col_name]],
          compiled,
          check_approximate,
          max_distance
        )
      } else if (is.factor(result[[col_name]])) {
        # Apply patterns to factor levels directly
        levels(result[[col_name]]) <- apply_patterns(
          levels(result[[col_name]]),
          compiled,
          check_approximate,
          max_distance
        )
      } else if (is.list(result[[col_name]])) {
        # Only recurse into anon() for list columns (need recursive handling)
        recursive_result <- anon(
          result[[col_name]],
          pattern_list = pattern_list,
          default_replacement = default_replacement,
          check_approximate = check_approximate,
          max_distance = max_distance,
          df_variable_names = NULL,
          df_classes = NULL,
          check_names = check_names,
          check_labels = check_labels,
          .self = TRUE,
          .pattern_replacements = pattern_replacements,
          .compiled = compiled
        )

        if (!is.null(attr(recursive_result, "approximate_warnings"))) {
          approximate_warnings <- c(
            approximate_warnings,
            attr(recursive_result, "approximate_warnings")
          )
          attr(recursive_result, "approximate_warnings") <- NULL
        }

        result[[col_name]] <- recursive_result
      }
    }

    # Anonymize column names only if check_names is TRUE
    if (isTRUE(check_names) && !is.null(names(result))) {
      names(result) <- apply_patterns(
        names(result),
        compiled,
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
        compiled,
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
          compiled,
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
          .self = TRUE,
          .pattern_replacements = pattern_replacements,
          .compiled = compiled
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
        compiled,
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

# Find the df_classes rule that applies to a vector. Named rules are checked
# first, in order, then unnamed ones. Returns NULL when nothing matches, or a
# list with the rule's `replacement` (NULL for unnamed rules, meaning the
# default replacement) and the `value` to pass to a replacement function.
find_df_class_rule <- function(var, df_classes) {
  if (is.null(df_classes) || isFALSE(df_classes) || length(df_classes) == 0) {
    return(NULL)
  }

  rule_names <- names(df_classes)
  if (is.null(rule_names)) {
    rule_names <- rep("", length(df_classes))
  }

  for (i in which(rule_names != "")) {
    value <- match_df_class(var, rule_names[i])
    if (!is.null(value)) {
      return(list(replacement = df_classes[[i]], value = value$value))
    }
  }

  for (i in which(rule_names == "")) {
    for (class_name in as.character(unlist(df_classes[[i]]))) {
      value <- match_df_class(var, class_name)
      if (!is.null(value)) {
        return(list(replacement = NULL, value = value$value))
      }
    }
  }

  NULL
}

# Check whether a vector matches a df_classes class name (case-insensitive).
# A vector matches its own classes. Plain double vectors (not Date, POSIXct or
# other classed doubles) also match "double". A character vector matches
# "double"/"numeric", "integer", "Date" or "POSIXct"/"POSIXt" when every
# non-missing, non-blank value parses as that type. Returns NULL for no match,
# or list(value = ...) holding the vector, parsed when it was character.
match_df_class <- function(var, class_name) {
  cls <- tolower(class_name)

  if (cls %in% tolower(class(var))) {
    return(list(value = var))
  }
  if (cls == "double" && is.double(var) && is.numeric(var)) {
    return(list(value = var))
  }
  if (is.character(var)) {
    parsed <- parse_character_as(var, cls)
    if (!is.null(parsed)) {
      return(list(value = parsed))
    }
  }
  NULL
}

# Parse a character vector as the given class when every non-missing,
# non-blank value converts. Missing and blank values become NA. Returns NULL
# when any value does not convert or when there are no values to check.
parse_character_as <- function(x, cls) {
  present <- !is.na(x) & trimws(x) != ""
  values <- trimws(x[present])
  if (length(values) == 0) {
    return(NULL)
  }

  iso_date <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  iso_datetime <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?",
    "(Z|[+-][0-9]{2}(:?[0-9]{2})?)?$"
  )

  parsed <- switch(
    cls,
    double = ,
    numeric = {
      num <- suppressWarnings(as.numeric(values))
      if (all(is.finite(num))) num else NULL
    },
    integer = {
      num <- suppressWarnings(as.numeric(values))
      if (all(is.finite(num)) && all(num == round(num)) &&
        all(abs(num) <= .Machine$integer.max)) {
        as.integer(num)
      } else {
        NULL
      }
    },
    date = {
      dates <- if (all(grepl(iso_date, values))) {
        as.Date(values, format = "%Y-%m-%d", optional = TRUE)
      }
      if (!is.null(dates) && !anyNA(dates)) dates else NULL
    },
    posixct = ,
    posixt = {
      is_date <- grepl(iso_date, values)
      is_datetime <- grepl(iso_datetime, values)
      if (all(is_date | is_datetime) && any(is_datetime)) {
        times <- parse_iso8601_datetime(values)
        if (!anyNA(times)) times else NULL
      }
    },
    NULL
  )

  if (is.null(parsed)) {
    return(NULL)
  }

  out <- rep(parsed[NA_integer_], length(x))
  out[present] <- parsed
  out
}

# Parse ISO 8601 dates and date-times (optional seconds, fractional seconds and
# UTC offset) to POSIXct in UTC, keeping the instant each value represents.
# Values without an offset are read as UTC. Unparseable values become NA.
parse_iso8601_datetime <- function(x) {
  x <- sub("^([0-9]{4}-[0-9]{2}-[0-9]{2})$", "\\1T00:00:00", x)
  x <- sub("(T[0-9]{2}:[0-9]{2})(?=$|[Z+-])", "\\1:00", x, perl = TRUE)
  x <- sub("Z$", "+0000", x)
  x <- sub("([+-][0-9]{2}):?([0-9]{2})$", "\\1\\2", x)
  x <- sub("(T[0-9:.]+)([+-][0-9]{2})$", "\\1\\200", x)
  x <- ifelse(grepl("[+-][0-9]{4}$", x), x, paste0(x, "+0000"))
  as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"))
}

# Whether a data frame column is replaced wholesale by df_variable_names or
# df_classes, in which case its text is not used to find NLP entities.
column_replaced_wholesale <- function(col_name, col, df_variable_names, df_classes) {
  if (!is.null(df_variable_names) && !isFALSE(df_variable_names)) {
    rule_names <- names(df_variable_names)
    if (is.null(rule_names)) {
      rule_names <- rep("", length(df_variable_names))
    }
    listed <- c(
      rule_names[rule_names != ""],
      as.character(unlist(df_variable_names[rule_names == ""]))
    )
    if (col_name %in% listed) {
      return(TRUE)
    }
  }
  !is.null(find_df_class_rule(col, df_classes))
}

# The part of `x` whose text is searched for NLP entities: data frame columns
# that are replaced wholesale are dropped, recursively through lists.
nlp_text_source <- function(x, df_variable_names, df_classes) {
  if (is.data.frame(x)) {
    keep <- !vapply(
      names(x),
      function(col_name) {
        column_replaced_wholesale(col_name, x[[col_name]], df_variable_names, df_classes)
      },
      logical(1)
    )
    return(x[keep])
  }
  if (is.list(x)) {
    return(lapply(x, nlp_text_source, df_variable_names, df_classes))
  }
  x
}

# Compile all pattern groups into one regex with a named capture group per
# replacement value. Replacing in a single pass means a later group can never
# match inside text produced by an earlier one. At any position, groups are
# tried in order, and patterns within a group in order. Patterns that are not
# valid regular expressions are matched literally.
compile_patterns <- function(pattern_replacements) {
  if (length(pattern_replacements) == 0) {
    return(list(
      groups = list(),
      regex = NULL,
      group_names = character(0),
      replacements = character(0),
      all_patterns = character(0)
    ))
  }

  # Group patterns by replacement value
  replacement_groups <- list()
  all_patterns <- character(0)
  for (i in seq_along(pattern_replacements)) {
    pat <- pattern_replacements[[i]][[1]]
    repl <- pattern_replacements[[i]][[2]]
    replacement_groups[[repl]] <- c(replacement_groups[[repl]], pat)
    all_patterns <- c(all_patterns, pat)
  }

  groups <- vector("list", length(replacement_groups))
  group_names <- paste0("anonGroup", seq_along(replacement_groups))

  for (i in seq_along(replacement_groups)) {
    patterns <- vapply(
      replacement_groups[[i]],
      as_valid_regex,
      character(1),
      USE.NAMES = FALSE
    )
    groups[[i]] <- list(
      replacement = names(replacement_groups)[i],
      patterns = patterns
    )
  }

  # Each named wrapper is itself a numbered capture group, so numbered
  # backreferences in user patterns are shifted by the number of capture
  # groups that open before that pattern in the combined regex.
  opened <- 0L
  for (i in seq_along(groups)) {
    opened <- opened + 1L
    for (j in seq_along(groups[[i]]$patterns)) {
      pattern <- groups[[i]]$patterns[[j]]
      groups[[i]]$patterns[[j]] <- shift_backreferences(pattern, opened)
      opened <- opened + count_capture_groups(pattern)
    }
  }

  regex <- paste0(
    "(?<", group_names, ">",
    vapply(groups, function(g) combine_group_patterns(g$patterns), character(1)),
    ")",
    collapse = "|"
  )

  list(
    groups = groups,
    regex = regex,
    group_names = group_names,
    replacements = names(replacement_groups),
    all_patterns = all_patterns
  )
}

# Join a group's patterns into one alternation. Consecutive whole-value
# patterns (see whole_value_patterns()) share a single pair of boundary
# assertions: "(?<!B)(?:a|b)(?!B)" matches the same text as
# "(?<!B)a(?!B)|(?<!B)b(?!B)", and compiles far faster because ICU builds each
# case-insensitive [[:alnum:]] set once instead of once per entity.
combine_group_patterns <- function(patterns) {
  prefix <- whole_value_prefix
  suffix <- whole_value_suffix
  is_whole <- startsWith(patterns, prefix) & endsWith(patterns, suffix) &
    nchar(patterns) > nchar(prefix) + nchar(suffix)
  runs <- rle(is_whole)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1

  pieces <- character(length(runs$lengths))
  for (k in seq_along(pieces)) {
    run <- patterns[starts[k]:ends[k]]
    if (runs$values[k] && length(run) > 1) {
      inner <- substr(run, nchar(prefix) + 1, nchar(run) - nchar(suffix))
      pieces[k] <- paste0(prefix, "(?:", paste(inner, collapse = "|"), ")", suffix)
    } else {
      pieces[k] <- paste(run, collapse = "|")
    }
  }
  paste(pieces, collapse = "|")
}

# Number of capture groups in a valid regex, as counted by ICU.
count_capture_groups <- function(pattern) {
  ncol(stringi::stri_match_first_regex("", pattern)) - 1L
}

# Add `shift` to every numbered backreference (\\1, \\2, ...) in a regex.
# A backslash that is itself escaped (\\\\1) is not a backreference.
shift_backreferences <- function(pattern, shift) {
  if (shift == 0L || !grepl("\\\\[1-9]", pattern)) {
    return(pattern)
  }
  locs <- stringi::stri_locate_all_regex(pattern, "(?<!\\\\)(?:\\\\\\\\)*\\\\[1-9][0-9]*")[[1]]
  pieces <- stringi::stri_sub(pattern, locs[, "start"], locs[, "end"])
  shifted <- vapply(
    pieces,
    function(piece) {
      digits <- sub("^.*\\\\", "", piece)
      paste0(substr(piece, 1, nchar(piece) - nchar(digits)), as.integer(digits) + shift)
    },
    character(1),
    USE.NAMES = FALSE
  )
  stringi::stri_sub_replace_all(pattern, locs[, "start"], locs[, "end"], replacement = shifted)
}

# Return a pattern unchanged if it compiles as a regular expression, otherwise
# an escaped version that matches it literally.
as_valid_regex <- function(pattern) {
  valid <- tryCatch(
    {
      stringi::stri_detect_regex("", pattern)
      TRUE
    },
    error = function(e) FALSE
  )
  if (valid) pattern else escape_regex_chars(pattern)
}

# Replace every match of the compiled regex with the replacement value of the
# group that matched. Each distinct string is processed once.
replace_compiled <- function(text, compiled) {
  if (length(compiled$groups) == 0 || length(text) == 0) {
    return(text)
  }

  opts <- stringi::stri_opts_regex(case_insensitive = TRUE)
  keys <- unique(text[!is.na(text)])
  if (length(keys) == 0) {
    return(text)
  }

  if (length(compiled$groups) == 1) {
    # One replacement value: no need to find out which group matched.
    literal <- gsub("([\\\\$])", "\\\\\\1", compiled$replacements)
    replaced <- stringi::stri_replace_all_regex(
      keys,
      compiled$regex,
      literal,
      opts_regex = opts
    )
    return(replaced[match(text, keys)])
  }

  hit <- stringi::stri_detect_regex(keys, compiled$regex, opts_regex = opts)
  if (!any(hit)) {
    return(text)
  }

  hit_text <- keys[hit]
  matches <- stringi::stri_match_all_regex(
    hit_text,
    compiled$regex,
    opts_regex = opts,
    cg_missing = NA_character_
  )
  locations <- stringi::stri_locate_all_regex(
    hit_text,
    compiled$regex,
    opts_regex = opts
  )

  # Which named group matched each match, across all strings
  group_cols <- do.call(
    rbind,
    lapply(matches, function(m) m[, compiled$group_names, drop = FALSE])
  )
  group_idx <- max.col(!is.na(group_cols), ties.method = "first")
  n_matches <- vapply(locations, nrow, integer(1))
  owner <- factor(rep(seq_along(hit_text), n_matches), levels = seq_along(hit_text))

  replaced <- keys
  replaced[hit] <- stringi::stri_sub_all_replace(
    hit_text,
    from = lapply(locations, function(loc) loc[, "start"]),
    to = lapply(locations, function(loc) loc[, "end"]),
    replacement = unname(split(compiled$replacements[group_idx], owner))
  )
  replaced[match(text, keys)]
}

# Helper function for approximate distance matching
compute_approximate_distances <- function(text, pattern, max_distance = 2) {
  if (length(text) == 0 || is.na(pattern) || nchar(pattern) <= 3) {
    return(list(
      distances = integer(0),
      matches = integer(0),
      matching_strings = character(0)
    ))
  }

  candidate_matches <- agrepl(
    pattern,
    text,
    max.distance = max_distance,
    ignore.case = TRUE,
    fixed = TRUE
  )

  matches <- which(candidate_matches)

  if (length(matches) == 0) {
    return(list(
      distances = integer(0),
      matches = integer(0),
      matching_strings = character(0)
    ))
  }

  candidate_text <- text[matches]

  # First check with fixed (non-partial) where the candidates are x and the pattern is y and insertions are
  # more costly.
  distances1 <- utils::adist(
    candidate_text,
    pattern,
    fixed = TRUE,
    ignore.case = TRUE,
    costs = c(insertions = 2, deletions = 1, substitutions = 1)
  )

  # Second check with partial matches where x and y are reverse of above and insertions are less
  # costly.
  distances2 <- utils::adist(
    pattern,
    candidate_text,
    partial = TRUE,
    ignore.case = TRUE,
    costs = c(insertions = 1, deletions = 5, substitutions = 5)
  )

  # Take minimum distance for each pattern (transpose distances2 since matrix is flipped)
  distances <- pmin(as.vector(distances1), as.vector(distances2))

  keep <- which(distances <= max_distance)
  matches <- matches[keep]

  # Extract the actual matching strings
  matching_strings <- if (length(matches) > 0) {
    text[matches]
  } else {
    character(0)
  }

  list(
    distances = distances[keep],
    matches = matches,
    matching_strings = matching_strings
  )
}

# Batched approximate distance matching across all patterns at once
compute_approximate_distances_batch <- function(
  text,
  patterns,
  max_distance = 2
) {
  # Filter to usable patterns (non-NA, length > 3)
  keep_pat <- !is.na(patterns) & nchar(patterns) > 3
  patterns <- patterns[keep_pat]

  if (length(text) == 0 || length(patterns) == 0) {
    return(character(0))
  }

  # Pre-filter: find which text elements are candidates for ANY pattern
  # Use agrepl per pattern but collect a single logical mask
  candidate_mask <- logical(length(text))
  for (pat in patterns) {
    candidate_mask <- candidate_mask | agrepl(
      pat,
      text,
      max.distance = max_distance,
      ignore.case = TRUE,
      fixed = TRUE
    )
  }

  candidates_idx <- which(candidate_mask)
  if (length(candidates_idx) == 0) {
    return(character(0))
  }

  candidate_text <- text[candidates_idx]

  # Compute distance matrices for all candidates x all patterns in one call each
  dist1 <- utils::adist(
    candidate_text,
    patterns,
    fixed = TRUE,
    ignore.case = TRUE,
    costs = c(insertions = 2, deletions = 1, substitutions = 1)
  )

  dist2 <- utils::adist(
    patterns,
    candidate_text,
    partial = TRUE,
    ignore.case = TRUE,
    costs = c(insertions = 1, deletions = 5, substitutions = 5)
  )

  # dist1 is [candidates x patterns], dist2 is [patterns x candidates]
  # Take element-wise minimum after transposing dist2
  distances <- pmin(dist1, t(dist2))

  # Collect warnings for any (candidate, pattern) pair within max_distance
  warning_msgs <- character(0)
  for (j in seq_along(patterns)) {
    close <- which(distances[, j] <= max_distance)
    if (length(close) > 0) {
      warning_msgs <- c(
        warning_msgs,
        paste0(
          "Potential approximate match: '",
          candidate_text[close],
          "' is similar to pattern '",
          patterns[j],
          "'"
        )
      )
    }
  }

  warning_msgs
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
