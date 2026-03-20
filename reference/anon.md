# Anonymize sensitive information in R objects

This function anonymizes or redacts sensitive information from various R
objects including character vectors, factors, data frames, and lists. It
uses pattern matching to find and replace sensitive content, with
options for targeted anonymization based on variable names or classes
and warnings about approximate matches.

## Usage

``` r
anon(
  x,
  pattern_list = list(),
  default_replacement = getOption("anon.default_replacement", default = "[REDACTED]"),
  check_approximate = TRUE,
  max_distance = 2,
  df_variable_names = NULL,
  df_classes = NULL,
  check_names = TRUE,
  check_labels = TRUE,
  nlp_auto = getOption("anon.nlp_auto"),
  .self = FALSE
)
```

## Arguments

- x:

  The object to anonymize. Can be a character vector, factor, data
  frame, or list.

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

- .self:

  Logical for internal use only. Used in recursive calls. Default is
  `FALSE`. When `TRUE`, warnings are collected as attributes instead of
  being issued immediately and global options are ignored and only
  explicitly provided parameters are used.

## Value

An object of class `anon_context` with the same structure as `x` but
with sensitive information replaced. If approximate matches are found
and `.self` is `FALSE`, warnings are issued. If `.self` is `TRUE`,
warnings are attached as an attribute.

## Details

`anon()` operates recursively on nested structures. For data frames:

- Individual columns are processed based on their content

- Entire columns can be replaced if specified in `df_variable_names` or
  `df_classes`

- Column names, row names, and labels are anonymized when enabled

Pattern matching is case-insensitive. When `check_approximate` is
enabled, `anon()` will warn about remaining potential matches that are
similar but not exact.

Replacement functions can be provided in `df_variable_names` and
`df_classes` as:

- R functions that take the column/variable as input

- Formula notation (e.g., `~ .x + rnorm(length(.x), mean = 1)`)

The returned object has class `anon_context` which allows it to be
combined with other anonymized objects using
[`c()`](https://rdrr.io/r/base/c.html).

## Global Options

The following global options affect function behavior:

- `anon.default_replacement`:

  Default replacement text (default: "\[REDACTED\]").

- `anon.pattern_list`:

  Global patterns to combine with (after) `pattern_list` parameter.

- `anon.df_variable_names`:

  Global variable name specifications to combine with (after)
  `df_variable_names` parameter.

- `anon.df_classes`:

  Global class specifications to combine with (after) `df_classes`
  parameter.

- `anon.nlp_auto`:

  List of logical values indicating which NLP entity types should be
  automatically anonymized. Use [`nlp_auto()`](nlp_auto.md) to generate
  this list. Override the option by setting the `nlp_auto` argument.

To set global options:

    options(anon.pattern_list = list("EMAIL" = "@\\S+"))
    options(anon.df_variable_names = c("name", "email"))
    options(anon.default_replacement = "[HIDDEN]")
    options(anon.nlp_auto = nlp_auto(person = TRUE))

## Examples

``` r
# Basic string anonymization
text <- c("John Smith", "jane.doe@email.com", "Call 555-1234")
anon(text, pattern_list = c("John Smith", "@\\S+", "\\d{3}-\\d{4}"))
#> [1] "[REDACTED]"         "jane.doe[REDACTED]" "Call [REDACTED]"   

# Using named patterns for specific replacements
anon(text, pattern_list = list("PERSON" = "John Smith",
                               "EMAIL" = "@\\S+",
                               "PHONE" = "\\d{3}-\\d{4}"))
#> [1] "PERSON"        "jane.doeEMAIL" "Call PHONE"   

# Data frame anonymization
df <- data.frame(
  name = c("Alice", "Bob"),
  email = c("alice@test.com", "bob@test.com"),
  score = c(95, 87)
)

# Anonymize specific columns by name
anon(df, df_variable_names = c("name", "email"))
#>         name      email score
#> 1 [REDACTED] [REDACTED]    95
#> 2 [REDACTED] [REDACTED]    87

# Anonymize columns by class with custom replacements
anon(df, df_classes = list("character" = "HIDDEN"))
#>     name  email score
#> 1 HIDDEN HIDDEN    95
#> 2 HIDDEN HIDDEN    87

# Using functions for dynamic replacement
anon(df, df_variable_names = list("name" = ~ paste("Person", seq_along(.x))))
#>       name          email score
#> 1 Person 1 alice@test.com    95
#> 2 Person 2   bob@test.com    87

anon_df <- df |>
  anon(
    df_variable_names = list(
      "name" = ~ paste("Person", seq_along(.x)),
      "email"
    )
  )

# Using global options
options(anon.pattern_list = list("EMAIL" = "@\\S+"))
options(anon.df_variable_names = "name")
anon(df)  # Will anonymize emails and names using global settings
#>         name      email score
#> 1 [REDACTED] aliceEMAIL    95
#> 2 [REDACTED]   bobEMAIL    87

# Combine anonymized objects
anon_summary <- anon_data_summary(list(df = df))
combined <- c(anon_df, anon_summary)
combined
#> === ANONYMIZED DATA CONTEXT ===
#> 
#>       name      email score
#> 1 Person 1 [REDACTED]    95
#> 2 Person 2 [REDACTED]    87
#> 
#> Environment Data Summary
#> ========================
#> 
#>   total_objects data_frames other_objects total_memory
#> 1             1           1             0         1272
#> 
#> Data Frames:
#> ------------
#>   name       type n_rows n_cols memory_size
#> 1   df data.frame      2      3      1.2 Kb
#> 
#> 
#> Variable Details (df):
#> 
#> -------------------- 
#>   variable data_type n_distinct n_missing n_total pct_missing label
#> 1     name character          2         0       2           0  <NA>
#> 2    email character          2         0       2           0  <NA>
#> 3    score   numeric          2         0       2           0  <NA>
#> 
#> 
#> 
```
