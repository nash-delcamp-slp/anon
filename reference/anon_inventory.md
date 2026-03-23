# Build an anonymized inventory of objects in an environment or list

Build an anonymized inventory of objects in an environment or list

## Usage

``` r
anon_inventory(
  envir = globalenv(),
  selection = NULL,
  pattern_list = list(),
  default_replacement = getOption("anon.default_replacement", default = "[REDACTED]"),
  check_approximate = getOption("anon.check_approximate", default = FALSE),
  max_distance = 2
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
  string distance. Default is
  `getOption("anon.check_approximate", default = FALSE)`.

- max_distance:

  Maximum string distance for approximate matching when
  `check_approximate` is `TRUE`. Default is `2`.

## Value

A tibble with one row per object and anonymized metadata about the
available objects.
