# Per-subject re-identification risk

Returns each subject's group size and risk from the same calculation as
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md).
The result contains source key values: use it to find which subjects are
at risk, and don't share it.

## Usage

``` r
anon_risk_subjects(data, quasi, key = "USUBJID", na = c("value", "wildcard"))
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of a de-identification step such as
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md).

- quasi:

  The quasi-identifiers to measure. For a single data frame, a character
  vector of column names. For a list of data frames, a list whose named
  elements are character vectors of columns in the dataset of that name
  (each subject must have one value per column, as in `dm`), and whose
  unnamed elements are
  [`anon_quasi_values()`](https://nash-delcamp-slp.github.io/anon/reference/anon_quasi_values.md)
  specifications for values that vary by row, such as baseline height
  and weight in `vs`.

- key:

  Name of the subject identifier column shared by the datasets. It must
  have the same type (character or factor, or numeric) in every dataset
  used.

- na:

  How a missing quasi-identifier value counts when forming groups.
  `"value"` (default): a missing value is a value of its own, and a cell
  blanked by
  [`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)
  is another value of its own (see "After suppression" in Details):
  groups are records with exactly the same released pattern.
  `"wildcard"`: every missing value matches any value, both ways, as
  sdcMicro's symmetric count does; a subject's `k` is then the number of
  subjects whose values agree with its own wherever both have a value.
  It counts real missing values as suppressed, and a few records blanked
  everywhere match everyone, so it can claim a target is met when it
  isn't; it is kept for comparison only.

## Value

A tibble with one row per subject: the `key` (as character), `k` (group
size) and `risk` (`1 / k`).

## Examples

``` r
dm <- data.frame(
  USUBJID = sprintf("S%02d", 1:4),
  AGE = c(60, 60, 70, 85)
)
anon_risk_subjects(dm, quasi = "AGE")
#> # A tibble: 4 × 3
#>   USUBJID     k  risk
#>   <chr>   <int> <dbl>
#> 1 S01         2   0.5
#> 2 S02         2   0.5
#> 3 S03         1   1  
#> 4 S04         1   1  
```
