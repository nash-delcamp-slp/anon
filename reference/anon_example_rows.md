# Build an example-row specification for [`anon_data_summary()`](anon_data_summary.md)

Build an example-row specification for
[`anon_data_summary()`](anon_data_summary.md)

## Usage

``` r
anon_example_rows(
  n,
  key = NULL,
  method = c("random", "first", "last"),
  value = NULL,
  n_key_values = 1,
  seed = NULL
)
```

## Arguments

- n:

  Number of rows to include in each example payload.

- key:

  Optional shared grouping key for cross-source scenarios.

- method:

  Selection method. `"random"` samples rows or key values, `"first"`
  uses the first encountered rows or key values, and `"last"` uses the
  last encountered rows or key values.

- value:

  Optional explicit key value for scenario mode. Requires `key` and
  takes precedence over `method` when supplied.

- n_key_values:

  Number of keyed scenario values to include when `key` is provided and
  `value` is not. Defaults to `1`.

- seed:

  Optional integer seed used when `method = "random"`.

## Value

A list of class `"anon_example_rows_spec"`.
