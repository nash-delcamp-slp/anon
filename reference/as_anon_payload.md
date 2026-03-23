# Build structured anon payloads

Normalize supported anon objects into a structured payload suitable for
downstream AI/tooling use or for conversion with
[`as_anon_json()`](as_anon_json.md). Unsupported custom classes error
with the object path to make failures easy to diagnose.

## Usage

``` r
as_anon_payload(x, path = "$")
```

## Arguments

- x:

  Object to normalize. Supports report/prompt-bundle objects and common
  anon_context payloads such as data frames, lists, and atomic vectors.

- path:

  Internal recursion path used for serializer error reporting. External
  callers should usually leave the default.

## Value

A structured R object composed of lists, vectors, and table payloads
ready for JSON serialization.
