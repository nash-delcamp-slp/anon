# Get or set package options used by anon

This helper provides a single place to inspect and update the supported
`anon.*` global options. It aligns with base
[`options()`](https://rdrr.io/r/base/options.html) behavior: calling it
with no arguments returns the current option values, and calling it with
one or more arguments sets those options and invisibly returns their
previous values.

## Usage

``` r
anon_options(
  default_replacement,
  pattern_list,
  df_variable_names,
  df_classes,
  nlp_auto,
  nlp_default_replacements,
  example_values_n,
  example_rows
)
```

## Arguments

- default_replacement:

  Default replacement text used by [`anon()`](anon.md) and NLP helpers
  when no more specific replacement is provided. Stored in
  `anon.default_replacement`.

- pattern_list:

  Global pattern rules combined with function-level `pattern_list`
  values. Stored in `anon.pattern_list`.

- df_variable_names:

  Global variable-name rules combined with function-level
  `df_variable_names`. Stored in `anon.df_variable_names`.

- df_classes:

  Global class-based rules combined with function-level `df_classes`.
  Stored in `anon.df_classes`.

- nlp_auto:

  NLP auto-redaction configuration stored in `anon.nlp_auto`. Use
  [`nlp_auto()`](nlp_auto.md) to build this value.

- nlp_default_replacements:

  Default NLP replacement labels stored in
  `anon.nlp_default_replacements`. Use
  [`nlp_default_replacements()`](nlp_default_replacements.md) to build
  this value.

- example_values_n:

  Default `example_values_n` used by
  [`anon_data_summary()`](anon_data_summary.md) and
  [`anon_report()`](anon_report.md). Stored in `anon.example_values_n`.

- example_rows:

  Default `example_rows` spec used by
  [`anon_data_summary()`](anon_data_summary.md) and
  [`anon_report()`](anon_report.md). Stored in `anon.example_rows`. Use
  [`anon_example_rows()`](anon_example_rows.md) to build this value.

## Value

If called with no arguments, a named list of current supported `anon.*`
options. Otherwise, the previous values for the updated options,
returned invisibly with the same values as
[`options()`](https://rdrr.io/r/base/options.html).

## Details

Supported options are:

- `anon.default_replacement`

- `anon.pattern_list`

- `anon.df_variable_names`

- `anon.df_classes`

- `anon.nlp_auto`

- `anon.nlp_default_replacements`

- `anon.example_values_n`

- `anon.example_rows`

Use `NULL` to clear an option. For structured option values, these
helpers are available:

- [`nlp_auto()`](nlp_auto.md)

- [`nlp_default_replacements()`](nlp_default_replacements.md)

- [`anon_example_rows()`](anon_example_rows.md)

## Examples

``` r
anon_options()
#> $anon.default_replacement
#> NULL
#> 
#> $anon.pattern_list
#> $anon.pattern_list$EMAIL
#> [1] "@\\S+"
#> 
#> 
#> $anon.df_variable_names
#> [1] "name"
#> 
#> $anon.df_classes
#> NULL
#> 
#> $anon.nlp_auto
#> NULL
#> 
#> $anon.nlp_default_replacements
#> NULL
#> 
#> $anon.example_values_n
#> NULL
#> 
#> $anon.example_rows
#> NULL
#> 

old <- anon_options(
  default_replacement = "[HIDDEN]",
  example_values_n = 2,
  example_rows = anon_example_rows(n = 3, method = "random", seed = 11)
)

options(old)
```
