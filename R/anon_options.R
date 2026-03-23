#' Get or set package options used by anon
#'
#' This helper provides a single place to inspect and update the supported
#' `anon.*` global options. It aligns with base [options()] behavior: calling
#' it with no arguments returns the current option values, and calling it with
#' one or more arguments sets those options and invisibly returns their previous values.
#'
#' @param default_replacement Default replacement text used by [anon()] and NLP
#'   helpers when no more specific replacement is provided. Stored in
#'   `anon.default_replacement`.
#' @param pattern_list Global pattern rules combined with function-level
#'   `pattern_list` values. Stored in `anon.pattern_list`.
#' @param df_variable_names Global variable-name rules combined with
#'   function-level `df_variable_names`. Stored in `anon.df_variable_names`.
#' @param df_classes Global class-based rules combined with function-level
#'   `df_classes`. Stored in `anon.df_classes`.
#' @param nlp_auto NLP auto-redaction configuration stored in `anon.nlp_auto`.
#'   Use [nlp_auto()] to build this value.
#' @param nlp_default_replacements Default NLP replacement labels stored in
#'   `anon.nlp_default_replacements`. Use [nlp_default_replacements()] to build
#'   this value.
#' @param example_values_n Default `example_values_n` used by
#'   [anon_data_summary()] and [anon_report()]. Stored in
#'   `anon.example_values_n`.
#' @param example_rows Default `example_rows` spec used by [anon_data_summary()]
#'   and [anon_report()]. Stored in `anon.example_rows`. Use
#'   [anon_example_rows()] to build this value.
#'
#' @return If called with no arguments, a named list of current supported
#'   `anon.*` options. Otherwise, the previous values for the updated options,
#'   returned invisibly with the same values as [options()].
#'
#' @details
#' Supported options are:
#' - `anon.default_replacement`
#' - `anon.pattern_list`
#' - `anon.df_variable_names`
#' - `anon.df_classes`
#' - `anon.nlp_auto`
#' - `anon.nlp_default_replacements`
#' - `anon.example_values_n`
#' - `anon.example_rows`
#'
#' Use `NULL` to clear an option. For structured option values, these helpers
#' are available:
#' - [nlp_auto()]
#' - [nlp_default_replacements()]
#' - [anon_example_rows()]
#'
#' @examples
#' anon_options()
#'
#' old <- anon_options(
#'   default_replacement = "[HIDDEN]",
#'   example_values_n = 2,
#'   example_rows = anon_example_rows(n = 3, method = "random", seed = 11)
#' )
#'
#' options(old)
#'
#' @export
anon_options <- function(
  default_replacement,
  pattern_list,
  df_variable_names,
  df_classes,
  nlp_auto,
  nlp_default_replacements,
  example_values_n,
  example_rows
) {
  option_map <- list()

  if (!missing(default_replacement)) {
    option_map <- append_option_value(
      option_map,
      name = "anon.default_replacement",
      value = default_replacement
    )
  }
  if (!missing(pattern_list)) {
    option_map <- append_option_value(
      option_map,
      name = "anon.pattern_list",
      value = pattern_list
    )
  }
  if (!missing(df_variable_names)) {
    option_map <- append_option_value(
      option_map,
      name = "anon.df_variable_names",
      value = df_variable_names
    )
  }
  if (!missing(df_classes)) {
    option_map <- append_option_value(
      option_map,
      name = "anon.df_classes",
      value = df_classes
    )
  }
  if (!missing(nlp_auto)) {
    if (!is.null(nlp_auto)) {
      get_enabled_nlp_entities(nlp_auto)
    }
    option_map <- append_option_value(
      option_map,
      name = "anon.nlp_auto",
      value = nlp_auto
    )
  }
  if (!missing(nlp_default_replacements)) {
    option_map <- append_option_value(
      option_map,
      name = "anon.nlp_default_replacements",
      value = nlp_default_replacements
    )
  }
  if (!missing(example_values_n)) {
    if (!is.null(example_values_n)) {
      normalize_example_values_n(example_values_n)
    }
    option_map <- append_option_value(
      option_map,
      name = "anon.example_values_n",
      value = example_values_n
    )
  }
  if (!missing(example_rows)) {
    if (!is.null(example_rows)) {
      normalize_example_rows_spec(example_rows)
    }
    option_map <- append_option_value(
      option_map,
      name = "anon.example_rows",
      value = example_rows
    )
  }

  if (length(option_map) == 0L) {
    return(get_anon_options())
  }

  invisible(options(option_map))
}

anon_option_names <- function() {
  c(
    "anon.default_replacement",
    "anon.pattern_list",
    "anon.df_variable_names",
    "anon.df_classes",
    "anon.nlp_auto",
    "anon.nlp_default_replacements",
    "anon.example_values_n",
    "anon.example_rows"
  )
}

get_anon_options <- function() {
  option_names <- anon_option_names()
  stats::setNames(lapply(option_names, getOption), option_names)
}

append_option_value <- function(option_map, name, value) {
  option_map[name] <- list(value)
  option_map
}
