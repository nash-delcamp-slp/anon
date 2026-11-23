# Specify a duration between two dates as a quasi-identifier

Describes a duration in days between two dates of each subject, banded,
such as treatment duration from `RFSTDTC` to `RFENDTC` in SDTM `dm`, for
use in
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md).
Durations are not changed by date offsetting (a subject's dates all move
together), so they can single subjects out even when the other
quasi-identifiers meet the target.

## Usage

``` r
anon_quasi_duration(dataset, start, end, width = 7, name = "DURATION")
```

## Arguments

- dataset:

  Name of the dataset in the list passed to
  [`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md).

- start, end:

  Names of the columns holding the start and end dates: Date or POSIXct
  columns, or ISO 8601 text (only full dates count; partial dates give a
  missing duration).

- width:

  Width of the bands of days, a positive number: the duration
  `end - start` in days becomes the lower bound of its band, as
  [`anon_band()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
  does with `bottom = 0`.

- name:

  Name of the resulting quasi-identifier.

## Value

An object of class `"anon_quasi_duration"`.

## Details

Each subject's duration comes from its first row (in row order) with
both dates. A subject without both dates has a missing duration, which
is a value of its own. The duration can only be measured:
[`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md)
and
[`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)
don't change dates, and changing them would break study days (`--DY`).
[`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
reports it among the residual signals.

## Examples

``` r
dm <- data.frame(
  USUBJID = c("a", "b", "c"),
  RFSTDTC = c("2014-01-02", "2014-02-10", "2014-03-01"),
  RFENDTC = c("2014-06-30", "2014-08-01", "2014-03-20")
)
anon_risk(dm, quasi = anon_quasi_duration("data", "RFSTDTC", "RFENDTC", width = 28), max_risk = 1/2)
#> Re-identification risk
#> ======================
#> 
#> Quasi-identifiers: DURATION 
#> Measured without a step's record of suppressed cells: blanked cells count as missing values
#> Subjects: 3 
#> Target: maximum risk 0.5 (groups of at least 2) 
#> Maximum risk: 1 (smallest group: 1)  Average risk: 0.6666667 
#> Unique subjects (group of 1): 1 
#> Subjects in groups smaller than 2: 1 (33.3%)
#> Meets target: no 
#> 
#> Drivers (`_without` columns: result if that variable alone were left out):
#>  variable n_values n_missing n_unique_without n_below_k_without min_k_without
#>  DURATION        2         0                0                 0             3
#>  avg_risk_without
#>         0.3333333
```
