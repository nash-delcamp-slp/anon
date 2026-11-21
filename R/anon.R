#' Redact and replace sensitive information in R objects
#'
#' This function redacts or replaces sensitive information in various R objects
#' including character vectors, factors, data frames, and lists. It uses pattern
#' matching to find and replace sensitive content, with options for targeted
#' anonymization based on variable names or classes and warnings about approximate matches.
#'
#' @param x The object to anonymize. Can be a character vector, factor, data
#'   frame, list, de-identification step or environment.
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
#'   specifying which variables should be replaced:
#'   - Unnamed elements: variables are replaced with `default_replacement`
#'   - Named elements: variable names are keys, value can be either a replacement value or a function
#'   This parameter is combined with the global option `getOption("anon.df_variable_names")`.
#' @param df_classes For data frames, a character vector or named list specifying
#'   which variable classes should be replaced:
#'   - Unnamed elements: variables with matching classes are replaced with `default_replacement`
#'   - Named elements: class names are keys, value can be either a replacement value or a function
#'   Class names are matched case-insensitively against `class()` of each column. `"double"`
#'   also matches plain double columns (not `Date` or `POSIXct`). Character columns also match
#'   `"double"`/`"numeric"`, `"integer"`, `"Date"` or `"POSIXct"` when every non-missing,
#'   non-blank value parses as that type (dates as ISO 8601); a replacement function then
#'   receives the parsed values.
#'   This parameter is combined with the global option `getOption("anon.df_classes")`.
#' @param check_names Logical indicating whether to anonymize object names
#'   (column names, row names, list names, and the names and dimnames of
#'   vectors and columns; a data.table's key and indices follow its column
#'   names). Default is `TRUE`.
#' @param check_labels Logical indicating whether to anonymize labels
#'   (attributes) of columns, lists and data frames: variable and data frame
#'   labels, comments and other attributes that aren't structural (text at
#'   any depth in them, such as lists like SPSS `variable.labels` or readr's
#'   `spec`, factors, data frames, their names and their own attributes, and
#'   the names of these attributes), haven
#'   value labels (the label names, and the label values when they are text)
#'   and text user-defined missing values (`na_values`, and the ends of a
#'   text `na_range`), on vectors and columns wherever `anon()` reaches them
#'   (in data frames, lists and list columns). Attributes that are structural
#'   for the object's class (see the `other_attributes` argument of
#'   [anon_subject_ids()]) are not redacted, except the names of a factor's
#'   contrasts matrix, which follow its levels. Value labels whose codes the
#'   redaction made equal are merged into one entry, their original labels
#'   joined with `" / "` and then redacted, with a warning naming the column as
#'   it is named in the output (or a list element, as `col[2]` or `col$name`); equal
#'   missing-value codes are deduplicated without a warning, as which values
#'   are missing doesn't change. A text `na_range` whose redacted ends would be
#'   in descending order is kept, with a warning. Whatever `check_labels`
#'   says, a warning names a column whose redaction changed which values are
#'   user-defined missing values. Default is `TRUE`.
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
#' @param .where Character for internal use only. Where `x` is (such as a
#'   list column's element, `"col[2]"`), for warnings. Default is `NULL`.
#'
#' @return An object of class `anon_context` with the same structure as `x` but with sensitive
#'   information replaced. For a de-identification step (see [anon_subject_ids()]), the step
#'   itself is returned with its data redacted, the patterns also applied to its log and
#'   settings, and its secrets dropped (read them from the original step); combine `step$data` rather than the step with `c()`.
#'   For an environment, a new environment holding redacted copies of its
#'   objects (see "Environments" below). If approximate matches are found and `.self` is `FALSE`, warnings are issued.
#'   If `.self` is `TRUE`, warnings are attached as an attribute.
#'
#' @details
#' `anon()` operates recursively on nested structures. For data frames:
#' - Individual columns are processed based on their content
#' - Entire columns can be replaced if specified in `df_variable_names` or `df_classes`
#' - Column names, row names, and labels (including haven value labels) are redacted when enabled
#' - List columns are redacted element by element, nested data frames included, and keep
#'   their class (`AsIs`, vctrs `list_of`, whose prototype is redacted like the elements)
#'
#' Lists, list columns and the classed lists they hold (such as fitted
#' models) keep their attributes and class, with their elements and
#' attributes redacted. Records (vctrs `rcrd`) and `POSIXlt` date-times have
#' their text fields and attributes redacted. Objects `anon()` can't search
#' for text (functions, calls and formulas, such as a model's `call` or a
#' model frame's `terms` attribute, environments held inside other objects,
#' S4 objects and pointers) are left as they are, and one warning lists where
#' they are (by element name where there is one, as `fit$call`; the first
#' ten, and how many more): remove them, or rebuild them from redacted data,
#' before sharing.
#'
#' Secrets read from a de-identification step with [anon_secrets()], and a
#' step's secrets environment, are dropped wherever `anon()` finds them
#' (attributes included, where a step's secrets are dropped too), with a
#' warning (a step's own secrets are dropped as described under Value).
#' External pointers and weak references are kept without a warning: they
#' hold no text, and are saved as null.
#'
#' Text that isn't valid UTF-8 (such as a stray Windows-1252 byte) is searched
#' through a repaired copy with the invalid bytes shown as hex (e.g.
#' `"<92>"`), which is written back where a pattern matched; a pattern that
#' matches digits can match inside such a marker. Text marked as `"bytes"`
#' that isn't valid UTF-8 is left as it is, with a warning.
#'
#' Pattern matching is case-insensitive. When `check_approximate` is enabled,
#' `anon()` will warn about remaining potential matches that are similar but not exact.
#'
#' Replacement functions can be provided in `df_variable_names` and `df_classes` as:
#' - R functions that take the column/variable as input
#' - Formula notation (e.g., `~ .x + rnorm(length(.x), mean = 1)`)
#'
#' The returned object has class `anon_context` which allows it to be combined with other
#' objects returned by `anon()` using `c()`.
#'
#' @section Environments:
#' For an environment, `anon()` returns a new environment holding a redacted
#' copy of each of its objects (hidden ones included), named as `check_names`
#' says (with NLP entities found in the names too); the environment given and
#' its objects are not changed. Its class and other attributes are not kept:
#' the copy is a plain environment (of class `anon_context`).
#'
#' The copy's parent is the environment's parent when R saves that parent by
#' reference (the global, base or empty environment, a package or a
#' namespace). Any other parent, such as a function's frame, would be saved
#' with the copy, and the objects it holds with it, so the copy's parent is
#' then the global environment, with a warning.
#'
#' A de-identification step in it is redacted as above, its secrets dropped.
#' Environments held in it are not copied: they are left as they are, as are
#' functions, and named in the warning. A closure, formula, terms object or
#' quosure whose environment is the environment given gets the copy as its
#' environment (so it runs on the redacted objects), and one whose
#' environment is another one R doesn't save by reference (such as a
#' function's frame) gets the global environment, wherever `anon()` finds it
#' (in environments, lists, list columns and attributes, such as a model's
#' `terms`), with a warning naming them, so that saving the copy doesn't save
#' the original's data. Reading the objects forces promises.
#' Active bindings (reading one would run its function) and objects that
#' can't be read (a missing argument, or a promise whose code fails) are not
#' copied, with a warning. The copy is not locked, whether or not the
#' environment given is. A name that redacts to nothing is replaced by
#' `"[unnamed 1]"` and so on. Names that redact alike are made unique
#' (`"[DRUG]"`, `"[DRUG] 1"`), as in reports.
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
#'         automatically redacted. Use [`nlp_auto()`] to generate this list. Override the option by setting the `nlp_auto` argument.}
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
#' # Combine processed objects
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
  .compiled = NULL,
  .where = NULL
) {
  # Combine user arguments with global options
  if (!.self) {
    option_pattern_list <- getOption("anon.pattern_list", default = list())

    # Combine pattern_list with option
    if (length(option_pattern_list) > 0) {
      pattern_list <- c(pattern_list, option_pattern_list)
    }

    # Internally, df_variable_names and df_classes may be FALSE to ignore the options.
    df_variable_names <- merge_with_option(df_variable_names, "anon.df_variable_names")
    df_classes <- merge_with_option(df_classes, "anon.df_classes")
  }

  # Track warnings at the anon() level: messages, and the locations (as the
  # warning shows them) of objects anon() can't redact (see
  # is_unredactable()), of objects whose environment it replaced (see
  # rehome_environment()) and of secrets it dropped. `foreign` marks a result
  # that is the object given (left as it is), on which no attribute is set.
  approximate_warnings <- character(0)
  unredactable <- character(0)
  rehomed <- character(0)
  secrets_dropped <- character(0)
  foreign <- FALSE
  recorded <- c("approximate_warnings", "anon_unredactable", "anon_rehomed", "anon_secrets_dropped")

  # Collect what a recursive call recorded, and strip it from its result.
  take <- function(child) {
    approximate_warnings <<- c(approximate_warnings, attr(child, "approximate_warnings"))
    unredactable <<- c(unredactable, attr(child, "anon_unredactable"))
    rehomed <<- c(rehomed, attr(child, "anon_rehomed"))
    secrets_dropped <<- c(secrets_dropped, attr(child, "anon_secrets_dropped"))
    for (name in recorded) {
      if (!is.null(attr(child, name, exact = TRUE))) attr(child, name) <- NULL
    }
    child
  }

  # An element anon() doesn't enter (secrets, which are dropped, or an object
  # it can't redact, which is kept, its environment replaced if need be, and
  # named in the warning), handled where it is found so that nothing is set
  # on it. NULL for other elements.
  skip_element <- function(element, where) {
    if (is_secrets(element)) {
      secrets_dropped <<- c(secrets_dropped, shown_location(where))
      return(list(value = NULL))
    }
    if (is_unredactable(element)) {
      moved <- rehome_environment(element)
      if (!is.null(moved)) {
        rehomed <<- c(rehomed, shown_location(where))
        element <- moved
      }
      unredactable <<- c(unredactable, shown_location(where))
      return(list(value = element))
    }
    NULL
  }

  # Record what clean_attributes() found in the attribute `name` of an object
  # at `where`, the attribute named as redacted.
  found_in_attribute <- function(where) {
    function(name, kind) {
      shown_name <- if (name %in% core_attributes) name else apply_patterns(name, compiled, FALSE, max_distance)
      location <- if (is.null(where)) {
        paste0("attribute `", shown_name, "`")
      } else {
        paste0("`", where, "` (attribute `", shown_name, "`)")
      }
      switch(kind,
        unredactable = unredactable <<- c(unredactable, location),
        rehomed = rehomed <<- c(rehomed, location),
        secrets = secrets_dropped <<- c(secrets_dropped, location)
      )
    }
  }

  # Clean then (as `check_labels` says) redact the attributes of an object at
  # `where` (see clean_attributes() and redact_text_attributes()).
  redact_attributes <- function(v, where, redact = NULL) {
    if (!has_text_attributes(v)) {
      return(v)
    }
    if (is.null(redact)) {
      redact <- function(text) apply_patterns(text, compiled, check_approximate, max_distance)
    }
    v <- clean_attributes(v, found_in_attribute(where))
    if (isTRUE(check_labels) && has_text_attributes(v)) {
      v <- redact_text_attributes(v, redact, where = where)
    }
    v
  }

  apply_patterns <- function(text, compiled, check_approximate, max_distance) {
    if (length(compiled$groups) == 0) return(text)

    result <- replace_compiled(text, compiled)
    if (is.character(text) && any(unconvertible_bytes(unique(text)))) {
      approximate_warnings <<- c(approximate_warnings, unsearchable_warning)
    }

    # Check for approximate matches if enabled
    if (isTRUE(check_approximate)) {
      approximate_text <- searchable_copy(unique(result[!is.na(result)]))

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

  # Redact a vector: its values (text, or a factor's levels), and, as
  # `check_labels` and `check_names` say, its labels and other text
  # attributes and its names. `where` names it in warnings.
  redact_atomic <- function(v, where, redact = NULL) {
    if (is.null(redact)) {
      redact <- function(text) apply_patterns(text, compiled, check_approximate, max_distance)
    }
    result <- v
    if (is.factor(v)) {
      levels(result) <- redact(levels(v))
      result <- redact_contrasts(result, redact)
    } else if (is.character(v)) {
      result <- redact(v)
    }
    result <- redact_attributes(result, where, redact)
    warn_user_missing_changed(user_missing_of(v), result, where)
    if (isTRUE(check_names) && has_names(result)) {
      result <- redact_names(result, redact)
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

  # An environment's objects are read once, for NLP and for the copy.
  env_read <- if (is.environment(x) && !isTRUE(.self) && !is_secrets(x)) read_environment(x)

  # Use pre-computed pattern_replacements if available (recursive calls)
  if (!is.null(.pattern_replacements)) {
    pattern_replacements <- .pattern_replacements
  } else {
    # Handle automatic/provided NLP anonymization
    enabled_nlp_entities <- get_enabled_nlp_entities(nlp_auto)
    if (length(enabled_nlp_entities) > 0) {
      nlp_pattern_list <- extract_nlp_patterns(
        nlp_text_source(
          if (inherits(x, "anon_deid_step")) x$data else if (!is.null(env_read)) env_read else x,
          df_variable_names,
          df_classes
        ),
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
  if (is_secrets(x)) {
    # Secrets read from a step (or its secrets environment) are dropped.
    secrets_dropped <- shown_location(.where)
    result <- NULL
    foreign <- TRUE
  } else if (inherits(x, "anon_deid_step")) {
    # Redact the step's data but keep the step, so print() and JSON still show
    # only its log.
    step_data <- anon(
      x$data,
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
    step_data <- take(step_data)
    result <- x
    result$data <- step_data
    # The redacted step is meant to be shared, so it doesn't carry the secrets.
    result["secrets"] <- list(NULL)
    # The log and settings name datasets and columns; apply the patterns to
    # their text too. Counts are left alone.
    log_cols <- c(if (isTRUE(check_names)) c("dataset", "variable"), "detail")
    for (col in log_cols) {
      result$log[[col]] <- apply_patterns(result$log[[col]], compiled, FALSE, max_distance)
    }
    result$settings <- rapply(
      result$settings,
      function(v) apply_patterns(v, compiled, FALSE, max_distance),
      classes = "character",
      how = "replace"
    )
    if (isTRUE(check_names) && length(result$settings) > 0) {
      names(result$settings) <- apply_patterns(names(result$settings), compiled, FALSE, max_distance)
    }
  } else if (is.environment(x) && !isTRUE(.self)) {
    # A new environment holding redacted copies of the objects, named as
    # `check_names` says; the environment given is left as it is (see
    # "Environments" in the documentation).
    object_names <- env_read$names
    shown <- if (isTRUE(check_names)) {
      apply_patterns(object_names, compiled, check_approximate, max_distance)
    } else {
      object_names
    }
    # A name redacted to nothing can't name an object: it gets a placeholder.
    blank <- !nzchar(shown)
    shown[blank] <- paste0("[unnamed ", seq_len(sum(blank)), "]")
    active <- env_read$active
    if (any(active)) {
      approximate_warnings <- c(approximate_warnings, paste0(
        "Active bindings are not copied, as reading them would run their function: ",
        paste0("`", shown[active], "`", collapse = ", "), "."
      ))
    }
    unreadable <- env_read$unreadable
    if (any(unreadable)) {
      approximate_warnings <- c(approximate_warnings, paste0(
        "These objects couldn't be read (a missing argument, or code that failed when their value was ",
        "computed) and were not copied: ", paste0("`", shown[unreadable], "`", collapse = ", "), "."
      ))
    }
    values <- env_read$values
    keep <- !active & !unreadable
    # Names that redact alike are made unique ("[DRUG]", "[DRUG] 1"), as in
    # reports.
    shown[keep] <- make.unique(shown[keep], sep = " ")
    # Saving an environment saves its parent too, unless the parent is one R
    # saves by reference (the global, base or empty environment, a package or
    # a namespace); another parent (such as a function's frame) could hold
    # unredacted objects, so the copy's parent is then the global environment.
    parent <- if (identical(x, emptyenv())) emptyenv() else parent.env(x)
    if (!is_shared_environment(parent)) {
      approximate_warnings <- c(approximate_warnings, paste0(
        "The environment's parent wasn't kept, as saving the copy would save it and the objects it holds: ",
        "the copy's parent is the global environment."
      ))
      parent <- globalenv()
    }
    result <- new.env(parent = parent)
    # While the objects are copied, closures and formulas that refer to the
    # environment given are given the copy instead (see rehome_environment()).
    previous <- list(from = copying$from, to = copying$to)
    copying$from <- x
    copying$to <- result
    on.exit({
      copying$from <- previous$from
      copying$to <- previous$to
    }, add = TRUE)
    for (i in which(keep)) {
      skipped <- skip_element(values[[i]], shown[i])
      if (!is.null(skipped)) {
        if (!is.null(skipped$value)) assign(shown[i], skipped$value, envir = result)
        next
      }
      value <- take(anon(
        values[[i]],
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
        .compiled = compiled,
        .where = shown[i]
      ))
      assign(shown[i], value, envir = result)
    }
  } else if (inherits(x, c("vctrs_rcrd", "POSIXlt"))) {
    # Records and POSIXlt date-times are lists of fields: text fields are
    # redacted like vectors, list and data frame fields like any list, and
    # their own attributes like a vector's. A record's field names, and a
    # POSIXlt's names (held by its `year` field), are names.
    result <- unclass(x)
    field_where <- function(i) paste0(if (is.null(.where)) "" else .where, "$", names(result)[i])
    for (i in seq_along(result)) {
      field <- result[[i]]
      if (is.character(field) || is.factor(field)) {
        result[[i]] <- redact_atomic(field, field_where(i))
      } else if (is.list(field)) {
        skipped <- skip_element(field, field_where(i))
        result[i] <- list(if (!is.null(skipped)) skipped$value else take(anon(
          field,
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
          .compiled = compiled,
          .where = field_where(i)
        )))
      } else if (inherits(x, "POSIXlt") && isTRUE(check_names) && !is.null(names(field))) {
        names(result[[i]]) <- apply_patterns(names(field), compiled, check_approximate, max_distance)
      }
    }
    if (inherits(x, "vctrs_rcrd") && isTRUE(check_names)) {
      names(result) <- apply_patterns(names(result), compiled, check_approximate, max_distance)
    }
    class(result) <- oldClass(x)
    result <- redact_attributes(result, .where)
  } else if (is.atomic(x) && !is.null(x)) {
    result <- redact_atomic(x, .where)
  } else if (is.data.frame(x)) {
    # Column names marked "bytes" can't be looked up by name; they are marked
    # "unknown" while the columns are processed, and "bytes" again after.
    unmarked <- unmark_bytes_names(list(x))
    result <- unmarked$data[[1]]
    frame_attributes <- attributes(result)

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

      # The column as named in the redacted output, for warnings.
      shown_name <- if (isTRUE(check_names)) {
        apply_patterns(col_name, compiled, FALSE, max_distance)
      } else {
        col_name
      }
      user_missing <- user_missing_of(result[[col_name]])

      # Clean the column's attributes, and redact its labels and other text
      # attributes if check_labels is TRUE
      result[[col_name]] <- redact_attributes(result[[col_name]], shown_name)

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
        result[[col_name]] <- redact_contrasts(
          result[[col_name]],
          function(text) apply_patterns(text, compiled, check_approximate, max_distance)
        )
      } else if (is_unredactable(result[[col_name]])) {
        unredactable <- c(unredactable, shown_location(shown_name))
      } else if (is_container_list(result[[col_name]]) || inherits(result[[col_name]], c("vctrs_rcrd", "POSIXlt"))) {
        # Only recurse into anon() for list columns (need recursive handling)
        recursive_result <- anon(
          .where = shown_name,
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
        result[[col_name]] <- take(recursive_result)
      }

      # Anonymize the names and dimnames of a column only if check_names is
      # TRUE
      if (isTRUE(check_names) && !is.data.frame(result[[col_name]]) && has_names(result[[col_name]])) {
        result[[col_name]] <- redact_names(
          result[[col_name]],
          function(text) apply_patterns(text, compiled, check_approximate, max_distance)
        )
      }
      warn_user_missing_changed(user_missing, result[[col_name]], shown_name)
    }

    # Anonymize column names only if check_names is TRUE, renaming the
    # columns in a data.table's key and indices too
    if (isTRUE(check_names) && !is.null(names(result))) {
      old_names <- names(result)
      names(result) <- apply_patterns(
        names(result),
        compiled,
        check_approximate,
        max_distance
      )
      result <- rename_table_references(result, stats::setNames(names(result), old_names))
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

    # Assigning columns or names to some classes (such as a grouped tibble)
    # rebuilds the data frame without its other attributes (such as its
    # label): those are put back, and redacted below.
    for (name in setdiff(names(frame_attributes), names(attributes(result)))) {
      attr(result, name) <- frame_attributes[[name]]
    }

    # Clean the data frame's attributes, and redact its labels and other text
    # attributes if check_labels is TRUE
    result <- redact_attributes(result, .where)
    result <- unmarked$restore(list(result))[[1]]
  } else if (is_container_list(x)) {
    # An element by its name as named in the output (`where$name`, or
    # `where[2]` when it has none).
    element_where <- function(i) {
      name <- if (!is.null(names(x)) && !is.na(names(x)[i]) && nzchar(names(x)[i])) {
        if (isTRUE(check_names)) apply_patterns(names(x)[i], compiled, FALSE, max_distance) else names(x)[i]
      }
      if (!is.null(.where)) {
        if (is.null(name)) paste0(.where, "[", i, "]") else paste0(.where, "$", name)
      } else if (!is.null(name)) {
        name
      } else {
        paste0("[[", i, "]]")
      }
    }
    # The text of the list's vectors is redacted in one pass, then looked up.
    redact_vectors <- batch_redactor(
      x,
      function(text) apply_patterns(text, compiled, check_approximate, max_distance),
      with_labels = isTRUE(check_labels), with_names = isTRUE(check_names)
    )
    result <- lapply(seq_along(x), function(i) {
        element <- x[[i]]
        # Elements with nothing to redact are returned as they are, and
        # vectors are redacted here rather than by a call to anon() each.
        if (nothing_to_redact(element)) {
          return(element)
        }
        if (is.atomic(element) && !is.null(element) && !is_container_list(element)) {
          return(redact_atomic(element, element_where(i), redact_vectors))
        }
        skipped <- skip_element(element, element_where(i))
        if (!is.null(skipped)) {
          return(skipped$value)
        }
        # Use .self = TRUE for recursive calls to collect warnings
        recursive_result <- anon(
          element,
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
          .compiled = compiled,
          .where = element_where(i)
        )
        take(recursive_result)
      })
    names(result) <- names(x)

    # Anonymize list names only if check_names is TRUE
    if (isTRUE(check_names) && !is.null(names(result))) {
      names(result) <- apply_patterns(
        names(result),
        compiled,
        check_approximate,
        max_distance
      )
    }

    # The list keeps its own attributes, its class included (a model or
    # other classed list stays one): those that aren't structural are
    # redacted as `check_labels` says, and dimnames as `check_names` says.
    own <- attributes(x)
    own$names <- NULL
    if (length(own) > 0) {
      if (isTRUE(check_names) && !is.null(own$dimnames)) {
        own$dimnames <- redact_names(
          structure(list(), dimnames = own$dimnames),
          function(text) apply_patterns(text, compiled, check_approximate, max_distance)
        )
        own$dimnames <- attr(own$dimnames, "dimnames", exact = TRUE)
      }
      attributes(result) <- c(attributes(result), own)
      result <- redact_attributes(result, .where)
    }

    # A list_of's prototype is redacted like its elements, so that they still
    # match it.
    if (is_plain_list(x) && !is.null(oldClass(x))) {
      ptype <- attr(x, "ptype", exact = TRUE)
      if (inherits(x, "vctrs_list_of") && !is.null(ptype)) {
        ptype <- anon(
          ptype,
          pattern_list = pattern_list,
          default_replacement = default_replacement,
          check_approximate = FALSE,
          max_distance = max_distance,
          df_variable_names = df_variable_names,
          df_classes = df_classes,
          check_names = check_names,
          check_labels = check_labels,
          .self = TRUE,
          .pattern_replacements = pattern_replacements,
          .compiled = compiled,
          .where = .where
        )
        attr(result, "ptype") <- take(ptype)
      }
      class(result) <- oldClass(x)
    }
  } else {
    # Functions, calls, formulas, environments held inside other objects, S4
    # objects and similar can't be searched: they are left as they are, with
    # a warning.
    if (is_unredactable(x)) {
      unredactable <- c(unredactable, shown_location(.where))
    }
    result <- x
    foreign <- TRUE
  }

  # Handle warnings based on .self parameter: a recursive call attaches what
  # it recorded to its result (never to an object it was given, see
  # `foreign`), and the top-level call issues one warning.
  if (isTRUE(.self)) {
    if (!foreign) {
      if (length(approximate_warnings) > 0) attr(result, "approximate_warnings") <- unique(approximate_warnings)
      if (length(unredactable) > 0) attr(result, "anon_unredactable") <- unique(unredactable)
      if (length(rehomed) > 0) attr(result, "anon_rehomed") <- unique(rehomed)
      if (length(secrets_dropped) > 0) attr(result, "anon_secrets_dropped") <- unique(secrets_dropped)
    }
  } else {
    messages <- c(
      unique(approximate_warnings),
      if (length(unredactable) > 0) unredactable_message(unredactable),
      if (length(rehomed) > 0) rehomed_message(rehomed),
      if (length(secrets_dropped) > 0) secrets_message(secrets_dropped)
    )
    if (length(messages) > 0) {
      rlang::warn(messages)
    }
  }

  if (isFALSE(.self) && !foreign && !inherits(result, "anon_deid_step")) {
    result <- new_anon_context(result)
  }
  result
}

# helpers -----------------------------------------------------------------

# Combine a df_variable_names or df_classes argument with its global option.
# FALSE means "ignore the option" and is returned unchanged.
merge_with_option <- function(value, option_name) {
  option_value <- getOption(option_name, default = NULL)
  if (is.null(option_value) || isFALSE(value)) {
    return(value)
  }
  if (is.null(value)) option_value else c(value, option_value)
}

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

# Whether a column has attributes that redact_text_attributes() looks at:
# any that isn't structural.
has_text_attributes <- function(x) {
  any(!names(attributes(x)) %in% structural_attributes(x))
}

# Objects anon() can't search for text: functions, calls and formulas,
# environments, S4 objects and byte code. External pointers and weak
# references (such as readr's `problems`) are left out: they hold no text,
# and are saved as null.
is_unredactable <- function(x) {
  if (typeof(x) %in% c("externalptr", "weakref")) {
    return(FALSE)
  }
  is.function(x) || is.language(x) || is.environment(x) || isS4(x) || typeof(x) == "bytecode"
}

# The environment being copied by anon() and its copy, while the copy's
# objects are redacted (see rehome_environment()).
copying <- new.env(parent = emptyenv())

# A closure, or a formula, terms object or quosure (which hold their
# environment in `.Environment`), with its environment replaced when saving
# it would save an environment's data: the environment being copied by
# anon() is replaced by its copy, and any other environment R doesn't save by
# reference (see is_shared_environment()), such as a function's frame, by the
# global environment. NULL when it is kept.
rehome_environment <- function(x) {
  env <- if (is.function(x)) {
    if (is.primitive(x)) NULL else environment(x)
  } else if (is.language(x)) {
    attr(x, ".Environment", exact = TRUE)
  }
  if (!is.environment(env)) {
    return(NULL)
  }
  new <- if (!is.null(copying$from) && identical(env, copying$from)) {
    copying$to
  } else if (is_shared_environment(env)) {
    return(NULL)
  } else {
    globalenv()
  }
  if (is.function(x)) environment(x) <- new else attr(x, ".Environment") <- new
  x
}

# Clean the attributes that aren't structural of `x`, at any depth through
# lists (entered unclassed, so records and POSIXlt values are walked by their
# fields) and attributes: secrets read from a step are dropped, and a step's
# secrets; closures and formulas get their environment replaced (see
# rehome_environment()). `found(name, kind)` is told, by attribute, what was
# found: "secrets", "rehomed", or "unredactable" (see is_unredactable(),
# which are not entered).
clean_attributes <- function(x, found) {
  attrs <- attributes(x)
  for (name in setdiff(names(attrs), structural_attributes(x))) {
    state <- new.env(parent = emptyenv())
    state$secrets <- state$rehomed <- state$unredactable <- FALSE
    new <- clean_value(attrs[[name]], state)
    if (!identical(new, attrs[[name]])) {
      attr(x, name) <- new
    }
    for (kind in c("secrets", "rehomed", "unredactable")) {
      if (state[[kind]]) found(name, kind)
    }
  }
  x
}

clean_value <- function(v, state) {
  if (is_secrets(v)) {
    state$secrets <- TRUE
    return(NULL)
  }
  if (inherits(v, "anon_deid_step") && !is.null(v$secrets)) {
    state$secrets <- TRUE
    v["secrets"] <- list(NULL)
  }
  moved <- rehome_environment(v)
  if (!is.null(moved)) {
    state$rehomed <- TRUE
    v <- moved
  }
  if (is_unredactable(v)) {
    state$unredactable <- TRUE
    return(v)
  }
  if (is.list(v)) {
    y <- unclass(v)
    changed <- FALSE
    for (i in seq_along(y)) {
      if (is.null(y[[i]])) next
      new <- clean_value(y[[i]], state)
      if (!identical(new, y[[i]])) {
        y[i] <- list(new)
        changed <- TRUE
      }
    }
    if (changed) {
      class(y) <- oldClass(v)
      v <- y
    }
  }
  attrs <- attributes(v)
  for (name in setdiff(names(attrs), structural_attributes(v))) {
    new <- clean_value(attrs[[name]], state)
    if (!identical(new, attrs[[name]])) {
      attr(v, name) <- new
    }
  }
  v
}

# An environment's objects as anon() reads them, once: their names, which
# are active bindings (not read, as reading one runs its function), the
# values (forcing promises), and which couldn't be read (a missing argument,
# or a promise whose code fails).
read_environment <- function(x) {
  object_names <- ls(x, all.names = TRUE, sorted = TRUE)
  active <- vapply(object_names, bindingIsActive, logical(1), env = x, USE.NAMES = FALSE)
  unread <- structure(list(), class = "anon_unread")
  values <- lapply(seq_along(object_names), function(i) {
    if (active[i]) {
      return(unread)
    }
    tryCatch(get(object_names[i], envir = x, inherits = FALSE), error = function(e) unread)
  })
  unreadable <- !active & vapply(values, inherits, logical(1), "anon_unread")
  structure(
    list(names = object_names, active = active, values = values, unreadable = unreadable),
    class = "anon_env_read"
  )
}

# Secrets read from a de-identification step with anon_secrets(), or its
# secrets environment.
is_secrets <- function(x) {
  inherits(x, c("anon_secrets", "anon_secrets_list"))
}

# Environments R saves by reference rather than by their contents.
is_shared_environment <- function(env) {
  identical(env, globalenv()) || identical(env, baseenv()) || identical(env, emptyenv()) ||
    isNamespace(env) || startsWith(attr(env, "name", exact = TRUE) %||% "", "package:")
}

# A location as warnings show it.
shown_location <- function(where) {
  if (is.null(where)) "the object itself" else paste0("`", where, "`")
}

# The first ten locations, and how many more.
shown_locations <- function(locations, n = 10) {
  locations <- unique(locations)
  more <- length(locations) - n
  paste0(paste(utils::head(locations, n), collapse = ", "), if (more > 0) paste0(", and ", more, " more"))
}

unredactable_message <- function(locations) {
  paste0(
    "anon() can't redact functions, calls, formulas, environments, S4 objects or similar, and left these as ",
    "they are: ", shown_locations(locations), ". Remove them, or rebuild them from redacted data, before sharing."
  )
}

rehomed_message <- function(locations) {
  paste0(
    "These functions, formulas or quosures had their environment replaced, so that saving the result doesn't ",
    "save the original's data: ", shown_locations(locations), "."
  )
}

secrets_message <- function(locations) {
  paste0(
    "anon() dropped the secrets of de-identification steps found in ", shown_locations(locations),
    ": read them from the original step, and keep them apart from what you share."
  )
}

# A redactor for the vectors held in list `x`: their distinct texts (values,
# factor levels, names, dimnames and text attributes, value labels included)
# are redacted with `redact` in one call, and looked up. Text not found is
# redacted directly, so the lookup never leaves text unredacted. NULL when
# the list holds fewer than two vectors.
batch_redactor <- function(x, redact, with_labels = TRUE, with_names = TRUE) {
  vectors <- Filter(function(e) is.atomic(e) && !is.null(e), x)
  if (length(vectors) < 2) {
    return(NULL)
  }
  # Only the texts that will be redacted, so that approximate-match warnings
  # stay as they would be element by element.
  texts <- unlist(lapply(vectors, function(e) {
    values <- if (is.factor(e)) levels(e) else if (is.character(e)) as.character(unclass(e))
    attrs <- attributes(e)
    text_attrs <- attrs[!names(attrs) %in% structural_attributes(e)]
    c(
      values,
      if (with_labels) unlist(lapply(text_attrs[vapply(text_attrs, is.character, logical(1))], unname), use.names = FALSE),
      if (with_labels) names(attrs$labels),
      if (with_names) c(names(e), unlist(dimnames(e), use.names = FALSE))
    )
  }), use.names = FALSE)
  texts <- as.character(texts)
  # Looked up by name in a hashed environment, which (unlike match()) is
  # built once. Text that can't be a name there (empty, not valid UTF-8,
  # marked "bytes", or 10,000 bytes or longer) is left to `redact`.
  in_table <- function(t) {
    !is.na(t) & nzchar(t) & Encoding(t) != "bytes" & validUTF8(t) & nchar(t, type = "bytes") < 10000
  }
  texts <- unique(texts[in_table(texts)])
  table <- list2env(stats::setNames(as.list(redact(texts)), texts), hash = TRUE, parent = emptyenv())
  function(text) {
    plain <- unclass(text)
    attributes(plain) <- NULL
    out <- plain
    usable <- in_table(plain)
    found <- rep(FALSE, length(plain))
    if (any(usable)) {
      hits <- mget(plain[usable], envir = table, ifnotfound = list(NULL))
      known <- !vapply(hits, is.null, logical(1))
      found[which(usable)[known]] <- TRUE
      out[found] <- unlist(hits[known], use.names = FALSE)
    }
    missing <- !found & !is.na(plain)
    if (any(missing)) {
      out[missing] <- redact(plain[missing])
    }
    keep_attributes(text, out)
  }
}

# A factor's contrasts matrix is named by its levels, which are redacted as
# values, so its names are redacted too; when redaction merged levels, the
# matrix no longer fits and is dropped.
redact_contrasts <- function(x, redact) {
  contrasts <- attr(x, "contrasts", exact = TRUE)
  if (is.matrix(contrasts) && nrow(contrasts) != nlevels(x)) {
    # Redaction merged levels: the matrix no longer fits, as in the steps.
    attr(x, "contrasts") <- NULL
  } else if (!is.null(dimnames(contrasts))) {
    dimnames(contrasts) <- lapply(dimnames(contrasts), function(d) if (!is.null(d)) redact(d))
    attr(x, "contrasts") <- contrasts
  }
  x
}

# Whether a list element can be returned as it is: no text values, no names
# and no attributes beyond structural ones.
nothing_to_redact <- function(x) {
  (is.numeric(x) || is.logical(x) || is.null(x)) && !is.object(x) && is.null(attributes(x))
}

# Apply `redact` to the text attributes of a vector, list, column or data
# frame: labels, comments and other attributes that aren't structural for its
# class (see structural_attributes()), haven value labels (their names,
# and their values when they are text) and text user-defined missing values
# (`na_values`, and the ends of `na_range`), as the values are redacted.
# Names are left to `check_names`. Value labels whose codes the redaction made
# equal are merged (see merge_equal_codes()), with a warning naming the column
# (`where`, as named in the output); equal missing-value codes are
# deduplicated, which doesn't change which values are missing. A text
# `na_range` whose redacted ends would be in descending order is kept, with a
# warning.
redact_text_attributes <- function(x, redact, where = NULL) {
  attrs <- attributes(x)
  shown <- if (is.null(where)) "a vector" else paste0("`", where, "`")
  names_attrs <- names(attrs)
  structural <- structural_attributes(x)
  own_terms <- if (is.data.frame(x)) character(0) else c("labels", "na_range")
  # Text at any depth in the other attributes (character values and their
  # names, factor levels, lists such as SPSS `variable.labels` or readr's
  # `spec`, data frames, and the attributes of these values) is redacted by
  # the walk the de-identification steps use (see rewrite_value()), which
  # leaves environments, functions and S4 objects alone.
  ctx <- NULL
  for (name in names_attrs[!names_attrs %in% c(structural, own_terms)]) {
    value <- attrs[[name]]
    if (is.null(ctx)) ctx <- new_rewrite_context(redact)
    new <- rewrite_value(ctx, value)
    if (!identical(new, value)) {
      attr(x, name) <- new
    }
  }
  # The names of those attributes are text too, except structural and core
  # ones (labels, comments, value labels, missing values), whose names R and
  # the packages that read them rely on.
  old <- names(attributes(x))
  renamable <- !old %in% c(structural, core_attributes)
  if (any(renamable)) {
    new_names <- old
    new_names[renamable] <- redact(old[renamable])
    if (!identical(new_names, old)) {
      new_names <- unique_rewritten(new_names, old)
      for (k in which(new_names != old)) {
        value <- attr(x, old[k], exact = TRUE)
        attr(x, old[k]) <- NULL
        attr(x, new_names[k]) <- value
      }
    }
  }
  if (is.data.frame(x)) {
    return(x)
  }
  na_range <- attrs$na_range
  if (is.character(na_range)) {
    redacted <- redact(na_range)
    if (length(redacted) == 2 && !isTRUE(redacted[[1]] <= redacted[[2]])) {
      warning(
        "The missing-value range of ", shown, " was not redacted, because its redacted ends would be ",
        "in descending order: redact or remove it.",
        call. = FALSE
      )
    } else {
      attr(x, "na_range") <- redacted
    }
  }
  na_values <- attr(x, "na_values", exact = TRUE)
  if (!is.null(na_values) && !anyDuplicated(attrs$na_values)) {
    attr(x, "na_values") <- merge_equal_codes(na_values)$codes
  }
  labels <- attrs$labels
  if (!is.null(labels)) {
    if (!is.null(names(labels))) {
      names(labels) <- redact(names(labels))
    }
    if (is.character(labels)) {
      labels[] <- redact(unname(labels))
    }
    if (!anyDuplicated(unname(attrs$labels))) {
      merged <- merge_equal_codes(labels, redact, names(attrs$labels))
      labels <- merged$codes
      if (merged$merged) {
        warning(
          "Value labels of ", shown, " were merged because redaction made their codes equal.",
          call. = FALSE
        )
      }
    }
    attr(x, "labels") <- labels
  }
  x
}

# Which values of a haven column with user-defined missing values are
# missing; NULL for other columns.
user_missing_of <- function(x) {
  if (inherits(x, "haven_labelled_spss")) is.na(x)
}

# Warn when redaction changed which values of a column (`where`, as named in
# the output) are user-defined missing values: a value or a code in
# `na_values` or `na_range` was redacted and no longer matches the other.
warn_user_missing_changed <- function(before, x, where) {
  if (!is.null(before) && inherits(x, "haven_labelled_spss") && !identical(before, is.na(x))) {
    warning(
      "Redaction changed which values of ", if (is.null(where)) "a vector" else paste0("`", where, "`"),
      " are user-defined missing values (`na_values` or `na_range`): check them.",
      call. = FALSE
    )
  }
}

# A data.table's key and indices, renamed with its columns (`columns` maps old
# names to new ones).
rename_table_references <- function(x, columns) {
  if (!inherits(x, "data.table")) {
    return(x)
  }
  for (name in c("sorted", "index")) {
    value <- attr(x, name, exact = TRUE)
    if (!is.null(value)) {
      attr(x, name) <- rename_column_references(name, value, columns)
    }
  }
  x
}

has_names <- function(x) {
  !is.null(attr(x, "names", exact = TRUE)) || !is.null(attr(x, "dimnames", exact = TRUE))
}

# Apply `redact` to the names and dimnames of a vector or column.
redact_names <- function(x, redact) {
  if (!is.null(attr(x, "names", exact = TRUE))) {
    attr(x, "names") <- redact(attr(x, "names", exact = TRUE))
  }
  dimnames <- attr(x, "dimnames", exact = TRUE)
  if (!is.null(dimnames)) {
    for (i in seq_along(dimnames)) {
      if (!is.null(dimnames[[i]])) dimnames[[i]] <- redact(dimnames[[i]])
    }
    if (!is.null(names(dimnames))) names(dimnames) <- redact(names(dimnames))
    attr(x, "dimnames") <- dimnames
  }
  x
}

# The part of `x` whose text is searched for NLP entities: data frame columns
# that are replaced wholesale are dropped, recursively through lists. For an
# environment (`top`, the one given to anon()), the names and the objects
# anon() copies from it: active bindings, objects that can't be read, secrets
# and objects anon() can't redact (environments held in it included) are
# left out, so nothing else is entered.
nlp_text_source <- function(x, df_variable_names, df_classes, top = TRUE) {
  if (inherits(x, "anon_deid_step")) {
    return(nlp_text_source(x$data, df_variable_names, df_classes, top = FALSE))
  }
  if (is.environment(x)) {
    if (!top) {
      return(NULL)
    }
    x <- read_environment(x)
  }
  if (inherits(x, "anon_env_read")) {
    read <- !x$active & !x$unreadable
    objects <- lapply(x$values[read], function(value) {
      if (is_secrets(value) || is_unredactable(value)) NULL else value
    })
    return(c(
      list(x$names),
      lapply(objects, nlp_text_source, df_variable_names, df_classes, top = FALSE)
    ))
  }
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
  if (is_container_list(x)) {
    return(lapply(x, nlp_text_source, df_variable_names, df_classes, top = FALSE))
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
    # The replacements with \\ and $ escaped, for stri_replace_all_regex().
    literal = gsub("([\\\\$])", "\\\\\\1", names(replacement_groups)),
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
  locs <- stringi::stri_locate_all_regex(
    pattern,
    "(?<!\\\\)(?:\\\\\\\\)*\\\\[1-9][0-9]*",
    omit_no_match = TRUE
  )[[1]]
  if (nrow(locs) == 0) {
    return(pattern)
  }
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
# group that matched, keeping the attributes of `text`. Each distinct string is
# processed once.
replace_compiled <- function(text, compiled) {
  if (length(compiled$groups) == 0 || length(text) == 0) {
    return(text)
  }

  opts <- stringi::stri_opts_regex(case_insensitive = TRUE)
  # The underlying values: for haven columns, user-defined missing values are
  # missing to is.na() but are values to redact.
  original <- text
  text <- unclass(text)
  attributes(text) <- NULL
  # Text is searched as valid UTF-8: text marked "bytes" is unmarked, and
  # invalid bytes are written as "<92>" (the repaired text is written back
  # where a pattern matched). Text marked "bytes" that isn't valid UTF-8 is
  # left as it is; apply_patterns() reports it.
  # These checks run on the distinct values only, which are then matched back.
  values <- unique(text[!is.na(text)])
  skipped <- unconvertible_bytes(values)
  values <- values[!skipped]
  search <- values
  bytes <- Encoding(search) == "bytes"
  if (any(bytes)) {
    unmarked <- search[bytes]
    Encoding(unmarked) <- "unknown"
    search[bytes] <- unmarked
  }
  search <- repair_utf8(enc2utf8(search))
  keys <- unique(search)
  if (length(keys) == 0) {
    return(original)
  }

  if (length(compiled$groups) == 1) {
    # One replacement value: no need to find out which group matched.
    literal <- compiled$literal %||% gsub("([\\\\$])", "\\\\\\1", compiled$replacements)
    replaced <- stringi::stri_replace_all_regex(
      keys,
      compiled$regex,
      literal,
      opts_regex = opts
    )
    return(keep_attributes(original, replace_matched(text, values, search, keys, replaced, any(skipped))))
  }

  hit <- stringi::stri_detect_regex(keys, compiled$regex, opts_regex = opts)
  if (!any(hit)) {
    return(original)
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
  keep_attributes(original, replace_matched(text, values, search, keys, replaced, any(skipped)))
}

# `text` with each of the distinct `values` (searched as `search`) replaced
# where the redaction changed it; other values are kept as they are.
replace_matched <- function(text, values, search, keys, replaced, skipped = FALSE) {
  i <- match(search, keys)
  changed <- replaced[i] != keys[i]
  if (!any(changed)) {
    return(text)
  }
  # Text marked "bytes" that isn't valid UTF-8 can't be matched against
  # other text, so only the other positions are.
  ok <- if (skipped) which(!unconvertible_bytes(text)) else seq_along(text)
  at <- match(text[ok], values[changed])
  hit <- !is.na(at)
  text[ok[hit]] <- replaced[i[changed]][at[hit]]
  text
}

# Distinct texts as they are searched: text marked "bytes" that isn't valid
# UTF-8 is left out, other text marked "bytes" is unmarked, and invalid bytes
# are written as "<92>".
searchable_copy <- function(text) {
  text <- text[!unconvertible_bytes(text)]
  bytes <- Encoding(text) == "bytes"
  if (any(bytes)) {
    unmarked <- text[bytes]
    Encoding(unmarked) <- "unknown"
    text[bytes] <- unmarked
  }
  unique(repair_utf8(enc2utf8(text)))
}

# Which texts are marked "bytes" and aren't valid UTF-8, so can't be searched.
unconvertible_bytes <- function(text) {
  out <- !is.na(text) & Encoding(text) == "bytes"
  if (any(out)) {
    unmarked <- text[out]
    Encoding(unmarked) <- "unknown"
    out[out] <- !validUTF8(unmarked)
  }
  out
}

unsearchable_warning <- paste0(
  "Some text is marked as \"bytes\" and isn't valid UTF-8, so it can't be searched and was left as it is: ",
  "convert it to UTF-8 (for example with iconv()) and run anon() again."
)

# The new values with the old vector's attributes (variable labels, haven
# classes and value labels, names), set directly rather than through `[<-`,
# which would check the new values against the value labels.
keep_attributes <- function(old, new) {
  attributes(new) <- attributes(old)
  new
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
