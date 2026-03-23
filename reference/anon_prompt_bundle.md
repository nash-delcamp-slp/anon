# Build prompt-ready text from anonymized report objects

Build prompt-ready text from anonymized report objects

## Usage

``` r
anon_prompt_bundle(
  report = NULL,
  text = NULL,
  comparison = NULL,
  format = c("markdown", "text", "json"),
  title = "Anonymized Prompt Context",
  include_inventory = TRUE,
  include_data_summary = TRUE,
  include_text = TRUE,
  include_comparison = TRUE
)
```

## Arguments

- report:

  Optional object created by [`anon_report()`](anon_report.md).

- text:

  Optional character vector of redacted text to include.

- comparison:

  Optional object created by
  [`anon_compare_text()`](anon_compare_text.md).

- format:

  Output format. `"markdown"` uses markdown headings and fenced blocks,
  while `"text"` uses plain-text headings.

- title:

  Top-level title for the generated bundle.

- include_inventory:

  Logical indicating whether to include the inventory section when
  `report` is provided.

- include_data_summary:

  Logical indicating whether to include the summary section when
  `report` is provided.

- include_text:

  Logical indicating whether to include `text`.

- include_comparison:

  Logical indicating whether to include `comparison`.

## Value

A length-one character vector of class `"anon_prompt_bundle"`.
