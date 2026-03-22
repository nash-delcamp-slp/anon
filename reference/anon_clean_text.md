# Clean text for safer prompt input

Normalizes line endings and optionally trims whitespace, squishes
repeated spaces, and compresses consecutive blank lines.

## Usage

``` r
anon_clean_text(
  x,
  trim = TRUE,
  squish_whitespace = TRUE,
  squash_blank_lines = TRUE
)
```

## Arguments

- x:

  Character vector to clean.

- trim:

  Logical indicating whether to trim leading and trailing whitespace on
  each line.

- squish_whitespace:

  Logical indicating whether to collapse repeated internal whitespace on
  each line.

- squash_blank_lines:

  Logical indicating whether to compress repeated blank lines to a
  single blank line.

## Value

A character vector with cleaned text.
