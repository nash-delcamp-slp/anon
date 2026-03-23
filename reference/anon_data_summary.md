# Generate anonymized summary of data objects in an environment

This function creates a summary of all objects (primarily data frames)
in a specified environment or list, then anonymizes the results using
the same pattern matching approach as [`anon()`](anon.md). It provides
structural information about data frames including dimensions, variable
details, and memory usage while protecting sensitive information through
pattern-based redaction.

## Usage

``` r
anon_data_summary(
  envir = globalenv(),
  selection = NULL,
  pattern_list = list(),
  default_replacement = getOption("anon.default_replacement", default = "[REDACTED]"),
  example_values_n = getOption("anon.example_values_n", default = 0),
  example_rows = getOption("anon.example_rows"),
  check_approximate = TRUE,
  max_distance = 2,
  nlp_auto = getOption("anon.nlp_auto")
)
```

## Arguments

- envir:

  An environment or list containing the objects to summarize. When
  passed as a list, unnamed elements will automatically be given names
  (either derived from the function call or indexed as "x1", "x2",
  etc.). Default is
  [`globalenv()`](https://rdrr.io/r/base/environment.html).

- selection:

  Optional character vector of object names to include in the summary.

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

- example_values_n:

  Optional number of example unique values to include for
  discrete/text-like data frame columns. Defaults to `0`, which disables
  example values.

- example_rows:

  Optional example-row specification for data frames. Use `NULL` to
  disable examples, a single number to request that many rows per data
  frame, or [`anon_example_rows()`](anon_example_rows.md) to build a
  spec with explicit arguments such as `n`, `key`, `method`, and
  `n_key_values`.

- check_approximate:

  Logical indicating whether to check for approximate matches using
  string distance. Default is `TRUE`.

- max_distance:

  Maximum string distance for approximate matching when
  `check_approximate` is `TRUE`. Default is `2`.

- nlp_auto:

  List of logical values with names corresponding to entity names. Can
  be generated with [`nlp_auto()`](nlp_auto.md) and can be set as the
  `anon.nlp_auto` global option. This argument overrides the global
  option.

## Value

An object of class `"anon_data_summary"` containing:

- `$summary`: A tibble with overall statistics (total objects, data
  frames count, other objects count, total memory usage)

- `$data_frames`: A list with two elements (only present if data frames
  exist):

  - `$structure`: A tibble with structural information for each data
    frame (name, label, dimensions, memory size)

  - `$variables`: A tibble with detailed variable information including
    data types, missing values, distinct values, labels, and optional
    example values

- `$examples`: Optional data frame example payloads containing either
  sample rows per data frame or one or more keyed cross-source scenarios

- All content is anonymized according to the specified patterns

## Details

The function operates in a few key steps:

1.  Generates detailed summaries for all objects

2.  Creates structured output with summary statistics and detailed
    information about data frames

3.  Applies anonymization using [`anon()`](anon.md) with the provided
    patterns

For data frames, the function captures:

- Structural information: dimensions, memory usage, and data frame-level
  labels

- Variable details: data types, missing value counts, distinct value
  counts, variable labels, and optional example values

- Optional example payloads: either sample rows or one or more keyed
  cross-source scenarios when configured

The output includes a custom print method that displays the information
in a readable format while maintaining the anonymization.

## See also

[`anon()`](anon.md) for the underlying anonymization function

## Examples

``` r
# Create study data with sensitive study codes in variable names
study_results <- data.frame(
  participant_id = c("P001", "P002", "P003"),
  ABC123_RESULT = c(85.2, 92.1, 78.5),
  ABC123_BASELINE = c(80.0, 88.3, 75.2),
  CBA321_RESULT = c(45.1, 52.3, 41.8),
  CBA321_BASELINE = c(42.0, 49.1, 39.5),
  age = c(45, 32, 67)
)

# Study metadata containing the same sensitive study codes as values
study_metadata <- list(
  primary_study = "ABC123",
  secondary_study = "CBA321",
  principal_investigator = "Dr. Smith",
  site_location = "Boston Medical Center"
)

# Create environment summary with anonymization
env_list <- list(study_results = study_results, metadata = study_metadata)

# Use metadata values to inform anonymization patterns
# This will anonymize both the variable names (ABC123_RESULT, CBA321_RESULT, etc.)
# and the corresponding values in the metadata
env_list |>
  anon_data_summary(
    pattern_list = list(
      "STUDY_A" = study_metadata$primary_study,    # "ABC123"
      "STUDY_B" = study_metadata$secondary_study,  # "CBA321"
      "MEDICAL_CENTER" = "Boston Medical Center"
    ),
    example_values_n = 2,
    example_rows = anon_example_rows(n = 2, method = "random", seed = 42)
  )
#> Environment Data Summary
#> ========================
#> 
#>   total_objects data_frames other_objects total_memory
#> 1             2           1             1         2752
#> 
#> Data Frames:
#> ------------
#>            name       type n_rows n_cols memory_size
#> 1 study_results data.frame      3      6      1.7 Kb
#> 
#> 
#> Variable Details (study_results):
#> 
#> ------------------------------- 
#>           variable data_type n_distinct n_missing n_total pct_missing label
#> 1   participant_id character          3         0       3           0  <NA>
#> 2   STUDY_A_RESULT   numeric          3         0       3           0  <NA>
#> 3 STUDY_A_BASELINE   numeric          3         0       3           0  <NA>
#> 4   STUDY_B_RESULT   numeric          3         0       3           0  <NA>
#> 5 STUDY_B_BASELINE   numeric          3         0       3           0  <NA>
#> 6              age   numeric          3         0       3           0  <NA>
#>   example_values
#> 1    P001 | P002
#> 2               
#> 3               
#> 4               
#> 5               
#> 6               
#> 
#> 
#> Example Rows (study_results):
#> 
#> ----------------------------- 
#>   participant_id ABC123_RESULT ABC123_BASELINE CBA321_RESULT CBA321_BASELINE
#> 1           P001          85.2            80.0          45.1            42.0
#> 3           P003          78.5            75.2          41.8            39.5
#>   age
#> 1  45
#> 3  67
#> 
#> Other Objects:
#> --------------
#>       name type length element_types memory_size
#> 1 metadata list      4     character        1 Kb
```
