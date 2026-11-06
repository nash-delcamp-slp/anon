# Specify quasi-identifier values that vary by row

Describes how to take one value per subject from a long dataset, such as
a findings domain, for use in
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md).
For each test code in `tests`, the first non-missing, non-blank
`result_col` value per subject (in row order, after `filter`) becomes a
quasi-identifier. Only that value is measured; other rows for the same
test identify the subject just as well, so they need the same treatment
before data is shared.

## Usage

``` r
anon_quasi_values(
  dataset,
  test_col,
  tests,
  result_col,
  filter = NULL,
  names = tests
)
```

## Arguments

- dataset:

  Name of the dataset in the list passed to
  [`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md).

- test_col:

  Name of the column holding test codes (e.g. `"VSTESTCD"`).

- tests:

  Test codes to use (e.g. `c("HEIGHT", "WEIGHT")`).

- result_col:

  Name of the column holding the result (e.g. `"VSSTRESN"`).

- filter:

  Optional one-sided formula evaluated in the dataset to select rows,
  e.g. `~ VSBLFL == "Y"` for baseline values. Variables not found in the
  dataset are looked up where the formula was created. It must return
  `TRUE` or `FALSE` for each row. It applies to every test in `tests`;
  use separate specifications when tests need different filters (for
  example, height recorded once at screening without a baseline flag). A
  warning is given when a test has no values.

- names:

  Names for the resulting quasi-identifiers. Defaults to `tests`.

## Value

An object of class `"anon_quasi_values"`.

## Examples

``` r
anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN",
                  filter = ~ VSBLFL == "Y")
#> Quasi-identifier values from `vs`: HEIGHT = first non-missing VSSTRESN where VSTESTCD == "HEIGHT"; WEIGHT = first non-missing VSSTRESN where VSTESTCD == "WEIGHT"
#> Filter: VSBLFL == "Y" 
```
