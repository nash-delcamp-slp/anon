# Serialize anon objects as JSON

Convert supported anon objects and common anon_context payloads into a
JSON representation suitable for downstream AI/tooling use.

## Usage

``` r
as_anon_json(x, pretty = TRUE, auto_unbox = TRUE)
```

## Arguments

- x:

  Object to normalize. Supports report/prompt-bundle objects and common
  anon_context payloads such as data frames, lists, and atomic vectors.

- pretty:

  Logical indicating whether to pretty-print the JSON output. Defaults
  to `TRUE`.

- auto_unbox:

  Logical passed to
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
  Defaults to `TRUE`.

## Value

A length-one character vector containing JSON.
