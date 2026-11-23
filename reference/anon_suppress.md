# Suppress quasi-identifiers until every subject reaches the target

For subjects whose group is still smaller than the target after
generalization, blanks quasi-identifier cells until every released
pattern is shared by enough records (local suppression), or removes
those subjects from every dataset. Risk is k-anonymity on the released
table (see "After suppression" in
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)):
a blanked cell is a value of its own.

## Usage

``` r
anon_suppress(
  data,
  quasi,
  key = "USUBJID",
  max_risk = 0.09,
  order = NULL,
  method = c("cells", "drop"),
  unresolved = c("drop", "keep"),
  key_cols = "RSUBJID",
  also = "SUBJID",
  everywhere = TRUE
)
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of an earlier step such as
  [`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md).

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

- max_risk:

  Highest acceptable re-identification risk for any subject (the maximum
  risk, `1 / k` for the smallest group). The target group size is the
  smallest `k` with `1 / k <= max_risk`, allowing for floating-point
  error so that 0.1 and 1/3 give 10 and 3. The default, 0.09, is the
  threshold Health Canada and EMA encourage for public release of
  clinical information, and requires groups of at least 12. Health
  Canada equates 0.09 with a cell size of 11, but `1 / 11` is about
  0.0909; to use groups of 11, pass `max_risk = 1/11`. See Details.

- order:

  Names of quasi-identifiers (as
  [`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)
  names them) from least to most useful, to choose between blanking
  options that are otherwise equal: the most useful are blanked last.
  Those not listed follow, in the order of `quasi`. Default: the order
  of `quasi`, its first quasi-identifier being the first blanked.

- method:

  `"cells"` (default) blanks cells; `"drop"` removes the subjects in
  groups smaller than the target from every dataset.

- unresolved:

  With `method = "cells"`, what to do with subjects still in groups
  smaller than the target when nothing is left to blank: `"drop"`
  (default) removes them from every dataset, with a warning; `"keep"`
  keeps them, with a warning.

- key_cols:

  Names of columns holding other subjects' keys (default `"RSUBJID"`, as
  in SDTM `RELSUB`): rows that refer to a removed subject are removed
  too, so no reference is left dangling.

- also:

  Names of related identifier columns (default `"SUBJID"`): when
  subjects are removed, their values (of at least 5 characters, as
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
  searches them) are looked for in what remains.

- everywhere:

  Logical. If `TRUE` (default), a quasi-identifier given as a column
  (such as `AGE` in `dm`) is blanked for the subject in every dataset
  with the `key` that has a column of that name (such as `AGE` in an
  ADaM `adsl` or `adae`), and in data frames with the `key` nested in
  any dataset. If `FALSE`, only in its own dataset, and a warning names
  the other datasets that hold a column of that name.

## Value

An object of class `"anon_deid_step"`, as for
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md),
with `suppressed`, the record of blanked cells (dataset, column or test,
and subject key).

## Details

**The measure.** Each released record's pattern is its visible
quasi-identifier values plus the places of its blanked cells, a blanked
cell counting as a value of its own; a subject's `k` is the number of
released records with exactly its pattern, and every pattern must be
shared by at least the target group size. Real missing values are
values, distinct from blanked cells. Cells blanked by an earlier
`anon_suppress()` in the same chain count as blanked. An attacker who
knows a subject's values then finds at least `k` records that could be
it, and records blanked everywhere only hide among each other.

**Cells.** The step repeats, on the records in groups smaller than the
target: for each quasi-identifier still shown on some of them, it tries
blanking that quasi-identifier on all of them. If a try brings records
into groups of at least the target, it takes the try that brings the
most (then needs the fewest cells, then blanks the least useful by
`order`) and blanks only the records it brings in. Otherwise it takes
the try that merges the most small patterns (then the least useful) and
blanks it on all of them. Every step blanks at least one cell, so it
ends. Then each group still too small (largest first; often the one
blanked everywhere) is filled from donors: records of other groups that
can spare them (their group keeps at least the target size), which take
its pattern by blanking the cells where it is blank, the donors needing
the fewest cells (then the least useful) first. Records still short (or
with fewer subjects than the target group size) are unresolved. The
result is deterministic and doesn't depend on the order of rows, but it
isn't guaranteed to blank the fewest cells. Blanking only joins records
into groups and removing small groups leaves the others as they are, so
groups of at least the target stay so.

A quasi-identifier is blanked in **every row** of that subject: the
column in every row of the subject in its dataset (and, with
`everywhere`, in every other dataset with the key that has the column),
and for an
[`anon_quasi_values()`](https://nash-delcamp-slp.github.io/anon/reference/anon_quasi_values.md)
specification every row of the test (not only the measured one), with
its other results: for SDTM `--ORRES`, `--STRESC` and `--STRESN`; for
ADaM `AVAL`, `AVALC`, `BASE`, `BASEC`, `CHG`, `PCHG` and `R2BASE`.
Blanks are missing values: `""` for text unless the column already uses
`NA` for missing text (it has `NA` and no `""`), `NA` for numbers and
factors (a factor's levels held only by blanked values are dropped).
Columns keep their type and attributes. The step records which cells it
blanked (by subject key, as the data holds it, never values), and later
steps carry the record on, so
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)
can measure the result.

**Drop.** Subjects in groups smaller than the target are removed (the
other groups stay as they are). Removed subjects' rows go from every
dataset with the `key`, from datasets whose rows belong to subjects
through a packed column with the key or a list column of data frames
holding one subject per row, from data frames with the key nested at any
depth, and with the rows that refer to them in `key_cols`. If any
removed subject's key, or a value of an `also` column, is still found in
the text values of the data, a warning names the datasets.

The measure needs the step's record of blanked cells: measuring the data
alone (a plain list of data frames) counts blanked cells as missing
values, which can merge them with real missing values and call the
target met when it isn't. Measure the step, as `meets_target` does.

**When the target isn't met.** `settings$suppress$meets_target` is
`FALSE`, with reasons in `settings$suppress$not_met`, when the risk
measured on the released data misses the target, when rows of removed
subjects remain, when a copy of a column quasi-identifier in another
dataset with the key shows a different value than its own dataset for a
subject (for example `AGE` in `adsl` not generalized like `AGE` in
`dm`), or when a dataset holding `AGE` as a quasi-identifier also holds
a birth date more precise than a year (a column named like a birth date,
such as `BRTHDTC`, a Date `BRTHDT`, `BIRTHDATE` or `DOB`): with a
reference date such as `RFSTDTC`, it gives the age back exactly, even
after the dates were offset (both move together). Remove birth dates or
reduce them to the year first. Other dates can give a quasi-identifier
back the same way (an age at an event from `--DTC`, an ADaM `AAGE`), and
so can derived columns (`AGEGR1` from `AGE`, `RACEN` from `RACE`): list
them as quasi-identifiers or generalize them together.

The log has one row per quasi-identifier (and result column) blanked,
with the number of cells blanked and of subjects, one row per dataset
with rows removed, and the risk before and after; it holds no values.
The settings record the arguments and where each quasi-identifier comes
from (not `filter`).

Each step of the loop groups the records once per quasi-identifier still
shown on records short of the target; 10,000 subjects with 12
quasi-identifiers, all of them unique at first, take about a second.

## See also

[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md),
[`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md)

## Examples

``` r
dm <- data.frame(
  USUBJID = sprintf("S%02d", 1:6),
  AGE = c(30, 30, 30, 40, 40, 50),
  SEX = c("F", "F", "M", "M", "M", "F")
)
result <- anon_suppress(dm, quasi = c("AGE", "SEX"), max_risk = 1/2)
result$data
#>   USUBJID AGE SEX
#> 1     S01  30   F
#> 2     S02  30   F
#> 3     S03  NA    
#> 4     S04  40   M
#> 5     S05  40   M
#> 6     S06  NA    
anon_risk(result, quasi = c("AGE", "SEX"), max_risk = 1/2)
#> Re-identification risk
#> ======================
#> 
#> Quasi-identifiers: AGE, SEX 
#> Cells blanked by suppression: 4 (each a value of its own in the released pattern)
#> Subjects: 6 
#> Target: maximum risk 0.5 (groups of at least 2) 
#> Maximum risk: 0.5 (smallest group: 2)  Average risk: 0.5 
#> Unique subjects (group of 1): 0 
#> Subjects in groups smaller than 2: 0 (0%)
#> Meets target: yes 
#> 
#> Drivers (`_without` columns: result if that variable alone were left out):
#>  variable n_values n_missing n_unique_without n_below_k_without min_k_without
#>       AGE        2         2                0                 0             2
#>       SEX        2         2                0                 0             2
#>  avg_risk_without
#>               0.5
#>               0.5
```
