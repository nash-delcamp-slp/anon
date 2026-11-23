# Generalize quasi-identifiers across datasets

Applies generalization rules
([`anon_band()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md),
[`anon_round()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md),
[`anon_fold()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md))
to columns and test results of linked datasets, to bring the
re-identification risk measured by
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)
down. Every column keeps its type, class and attributes, and the data
keeps its rows in the same order, so code tested on the generalized copy
still runs on the original.

## Usage

``` r
anon_generalize(data, rules, key = "USUBJID", keep_secrets = FALSE)
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of an earlier step such as
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md).

- rules:

  A list of rules. Elements named by a dataset are named lists of rules
  for that dataset's columns, as in
  `list(dm = list(AGE = anon_band(5, top = 90), RACE = anon_fold(12, "OTHER")))`;
  the rule applies to the column in every row. Unnamed elements are
  [`anon_rule()`](https://nash-delcamp-slp.github.io/anon/reference/anon_rule.md)
  pairs of an
  [`anon_quasi_values()`](https://nash-delcamp-slp.github.io/anon/reference/anon_quasi_values.md)
  specification and a rule, which apply to every row of the test and
  keep its text results consistent. For a single data frame, a named
  list of rules by column, such as `list(AGE = anon_band(5))`, is
  enough.

- key:

  Name of the subject identifier column, used by
  [`anon_fold()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
  to count subjects.

- keep_secrets:

  Logical. If `TRUE`, keep which categories were folded, with their
  numbers of subjects; read them with
  [`anon_secrets()`](https://nash-delcamp-slp.github.io/anon/reference/anon_secrets.md)
  (table `generalize_folded`). Default `FALSE`: they appear nowhere,
  because the names of small categories are themselves rare values.

## Value

An object of class `"anon_deid_step"`, as for
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md).

## Details

A column listed for several datasets (such as `AGE` in `dm` and in an
ADaM `adsl`) is generalized in each dataset where it is listed, and only
there: list every dataset that holds it.
[`anon_fold()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
rules with the same parameters on columns of the same name count
subjects across all those datasets together, so the same categories are
folded everywhere.

Rules apply to the columns of the datasets themselves. Data frames
nested in them (packed columns and list columns, see
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md))
are not generalized. A warning names the datasets with the key that hold
a generalized column under the same name without a rule for it.

A birth date more precise than a year (a column named like a birth date,
such as `BRTHDTC`, a Date `BRTHDT`, `BIRTHDATE` or `DOB`) in a dataset
whose `AGE` is generalized gives the age back with a reference date such
as `RFSTDTC`, even after the dates are offset: a warning names it.
Remove birth dates or reduce them to the year. Other dates and derived
columns can give a quasi-identifier back the same way (see
[`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)).

The log has one row per dataset, column and test, counting the values
changed; it holds no values, and the settings record the rules. The step
can be chained with
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md),
[`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)
and
[`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)
in any order: rules name columns, which those steps don't rename (unless
the names hold keys or dates).

Generalized values are representatives (a band's lower bound or
midpoint, a rounded number, a folded category), not the subjects' real
values. Tell whoever uses the data, for example with the log in a
report.

## See also

[generalize_rules](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md),
[`anon_rule()`](https://nash-delcamp-slp.github.io/anon/reference/anon_rule.md),
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md),
[`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)

## Examples

``` r
dm <- data.frame(
  USUBJID = sprintf("S%02d", 1:6),
  AGE = c(34, 37, 52, 58, 91, 93),
  RACE = c("WHITE", "WHITE", "WHITE", "ASIAN", "WHITE", "WHITE")
)
vs <- data.frame(
  USUBJID = rep(dm$USUBJID, each = 2),
  VSTESTCD = "WEIGHT",
  VSORRES = c("70.4", "71.0", "65.2", "66.0", "80.9", "79.5",
              "150.2", "151.0", "60.3", "60.0", "55.5", "56.1"),
  VSORRESU = rep(c("kg", "kg", "kg", "kg", "LB", "kg"), each = 2),
  VSSTRESC = c("70.4", "71.0", "65.2", "66.0", "80.9", "79.5",
               "68.13", "68.49", "60.3", "60.0", "55.5", "56.1"),
  VSSTRESN = c(70.4, 71.0, 65.2, 66.0, 80.9, 79.5, 68.13, 68.49, 60.3, 60.0, 55.5, 56.1),
  VSSTRESU = "kg"
)
result <- anon_generalize(
  list(dm = dm, vs = vs),
  rules = list(
    dm = list(AGE = anon_band(10, top = 90), RACE = anon_fold(2, "OTHER")),
    anon_rule(anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN"), anon_round(0))
  )
)
result$data$dm
#>   USUBJID AGE  RACE
#> 1     S01  30 WHITE
#> 2     S02  30 WHITE
#> 3     S03  50 WHITE
#> 4     S04  50 OTHER
#> 5     S05  90 WHITE
#> 6     S06  90 WHITE
result$data$vs
#>    USUBJID VSTESTCD VSORRES VSORRESU VSSTRESC VSSTRESN VSSTRESU
#> 1      S01   WEIGHT    70.0       kg     70.0       70       kg
#> 2      S01   WEIGHT    71.0       kg     71.0       71       kg
#> 3      S02   WEIGHT    65.0       kg     65.0       65       kg
#> 4      S02   WEIGHT    66.0       kg     66.0       66       kg
#> 5      S03   WEIGHT    81.0       kg     81.0       81       kg
#> 6      S03   WEIGHT    80.0       kg     80.0       80       kg
#> 7      S04   WEIGHT    68.0       kg    68.00       68       kg
#> 8      S04   WEIGHT    68.0       kg    68.00       68       kg
#> 9      S05   WEIGHT    60.0       LB     60.0       60       kg
#> 10     S05   WEIGHT    60.0       LB     60.0       60       kg
#> 11     S06   WEIGHT    56.0       kg     56.0       56       kg
#> 12     S06   WEIGHT    56.0       kg     56.0       56       kg
result$log
#> # A tibble: 6 × 6
#>   step       dataset variable                     n_changed n_removed detail    
#>   <chr>      <chr>   <chr>                            <int>     <int> <chr>     
#> 1 generalize NA      NA                                  NA        NA "6 subjec…
#> 2 generalize dm      AGE                                  6         0 "banded: …
#> 3 generalize dm      RACE                                 1         0 "categori…
#> 4 generalize vs      VSSTRESN (VSTESTCD = WEIGHT)         9         0 "rounded …
#> 5 generalize vs      VSSTRESC (VSTESTCD = WEIGHT)         9         0 "9 rewrit…
#> 6 generalize vs      VSORRES (VSTESTCD = WEIGHT)          9         0 "9 rewrit…
```
