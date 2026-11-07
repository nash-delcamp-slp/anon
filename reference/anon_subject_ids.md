# Pseudonymize subject identifiers consistently across datasets

Replaces each subject's key (for example `USUBJID`) with a random
pseudonym that is the same in every dataset, so the datasets can still
be joined. Related identifier columns listed in `also` (for example
`SUBJID`) are replaced with the pseudonym's number so they stay aligned
with the key, and columns listed in `key_cols` that hold other subjects'
keys (for example `RSUBJID`) are mapped through the same pseudonyms.

## Usage

``` r
anon_subject_ids(
  data,
  key = "USUBJID",
  also = "SUBJID",
  key_cols = "RSUBJID",
  prefix = "SUBJ-",
  keep_secrets = FALSE,
  seed = NULL
)
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of an earlier step such as
  [`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md).
  Datasets without the `key` column are returned unchanged.

- key:

  Name of the subject identifier column.

- also:

  Names of related identifier columns that hold one value per subject
  (default `"SUBJID"`). They are replaced with the pseudonym's number.
  Columns not present in a dataset are skipped.

- key_cols:

  Names of columns that hold subject keys, possibly of other subjects
  (default `"RSUBJID"`, as in SDTM `RELSUB`). Each value is replaced
  with that subject's pseudonym; values that are not a known key are
  removed with a warning.

- prefix:

  Prefix for character pseudonyms, followed by a zero-padded number
  (e.g. `"SUBJ-042"`). Numeric keys get the number itself.

- keep_secrets:

  Logical. If `TRUE`, keep the mapping from keys to pseudonyms; read it
  with
  [`anon_secrets()`](https://nash-delcamp-slp.github.io/anon/reference/anon_secrets.md).
  Default `FALSE`: the mapping is not kept anywhere.

- seed:

  Optional seed for the random assignment, for reproducible tests.
  Anyone with the seed and the original data can rebuild the mapping, so
  do not set it for data you share. The global random number state is
  restored afterwards.

## Value

An object of class `"anon_deid_step"`: a list with `data` (the
transformed data, in the same form as the input), `log` (a tibble with
`step`, `dataset`, `variable`, `n_changed`, `n_removed` and `detail`;
the first row of each step records its settings; earlier steps are
included when `data` was a step result), `settings` (a named list with
each step's arguments, without the seed value) and `secrets` (`NULL`
unless `keep_secrets = TRUE` here or in an earlier step). Saving the
object (for example with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html)) also saves its
secrets.

## Details

Pseudonyms are numbered in random order, and rows of each dataset are
then sorted by the new key (keeping the order of rows within each
subject), so neither the pseudonyms nor the row order reveal the order
of the original keys. Column types and attributes such as variable
labels are kept: character keys get character pseudonyms, numeric keys
get numbers, factors stay factors. Value labels (haven `labels`), which
describe the original values, are dropped from replaced columns. Missing
and blank keys are left as they are. Values of `also` columns in rows
without a key cannot be mapped and are removed (set to missing), with a
warning. `key`, `also` and `key_cols` must not overlap.

A warning lists any other character or factor column (values or factor
levels), in any dataset, that still contains original key values as
whole values or as whole words inside text (checked for character keys
that are not made only of digits, since numbers match many unrelated
codes), and any dataset without `key` that has a column named like
`key`, `also` or `key_cols` in any case. In datasets with `key`, a
column named like one of them in another case (such as `usubjid` beside
`USUBJID`) is not replaced and is named in a warning. Original values of
`also` columns (such as `SUBJID`) elsewhere are not checked, because
short numbers match many unrelated values. Missing values in `also`
columns stay missing. In `key`, `also` and `key_cols` columns, haven
user-defined missing values (`na_values`, `na_range`) become plain
missing values and the codes are dropped, because a code can't be told
apart from a real identifier.

Random numbers come from R's random number generator. If your script
calls [`set.seed()`](https://rdrr.io/r/base/Random.html) before this
step, anyone with the script and the data can reproduce the assignment.

Pseudonymized data remains personal data for anyone who holds the
mapping or can re-identify subjects by other means; measure the
remaining risk with
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md).

## See also

[`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md),
[`anon_secrets()`](https://nash-delcamp-slp.github.io/anon/reference/anon_secrets.md),
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)

## Examples

``` r
data <- list(
  dm = data.frame(USUBJID = c("01-701-1015", "01-701-1023"), SUBJID = c("1015", "1023")),
  vs = data.frame(USUBJID = c("01-701-1015", "01-701-1023"), VSSTRESN = c(70, 80))
)
result <- anon_subject_ids(data)
result$data
#> $dm
#>   USUBJID SUBJID
#> 1  SUBJ-1      1
#> 2  SUBJ-2      2
#> 
#> $vs
#>   USUBJID VSSTRESN
#> 1  SUBJ-1       70
#> 2  SUBJ-2       80
#> 
```
