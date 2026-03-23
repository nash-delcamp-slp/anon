# Build an anonymized report for selected objects

Build an anonymized report for selected objects

## Usage

``` r
anon_report(
  envir = globalenv(),
  selection = NULL,
  pattern_list = list(),
  default_replacement = getOption("anon.default_replacement", default = "[REDACTED]"),
  example_values_n = getOption("anon.example_values_n", default = 0),
  example_rows = getOption("anon.example_rows"),
  check_approximate = TRUE,
  max_distance = 2,
  df_variable_names = NULL,
  df_classes = NULL,
  check_names = TRUE,
  check_labels = TRUE,
  nlp_auto = getOption("anon.nlp_auto")
)
```

## Arguments

- envir:

  An environment or named/unnamed list of objects.

- selection:

  Optional character vector of object names to include.

- pattern_list:

  A list of patterns to search for and replace. Can include:

  - Named elements where names are replacement values and values are one
    or more patterns to match

  - Unnamed elements where one or more patterns are replaced with
    `default_replacement` This parameter is combined with the global
    option `getOption("anon.pattern_list")`.

- default_replacement:

  Value to use as the default replacement value when no specific
  replacement is provided. Default is
  `getOption("anon.default_replacement", default = "\[REDACTED\]")`.

- check_approximate:

  Logical indicating whether to check for approximate matches using
  string distance. Default is `TRUE`.

- max_distance:

  Maximum string distance for approximate matching when
  `check_approximate` is `TRUE`. Default is `2`.

- df_variable_names:

  For data frames, a character vector or named list specifying which
  variable names should be anonymized:

  - Unnamed elements: variables are replaced with `default_replacement`

  - Named elements: variable names are keys, value can be either a
    replacement value or a function This parameter is combined with the
    global option `getOption("anon.df_variable_names")`.

- df_classes:

  For data frames, a character vector or named list specifying which
  variable classes should be anonymized:

  - Unnamed elements: variables with matching classes are replaced with
    `default_replacement`

  - Named elements: class names are keys, value can be either a
    replacement value or a function This parameter is combined with the
    global option `getOption("anon.df_classes")`.

- check_names:

  Logical indicating whether to anonymize object names (column names,
  row names, list names). Default is `TRUE`.

- check_labels:

  Logical indicating whether to anonymize labels (attributes). Default
  is `TRUE`.

- nlp_auto:

  List of logical values with names corresponding to entity names. Can
  be generated with [`nlp_auto()`](nlp_auto.md) and can be set as the
  `anon.nlp_auto` global option. This argument overrides the global
  option.

## Value

An object of class `"anon_report"` containing an anonymized object
inventory and environment summary for the selected objects.
