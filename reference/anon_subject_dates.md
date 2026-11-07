# Shift each subject's dates by a random offset

Gives every subject one random offset, in whole days, and shifts all of
that subject's dates in every dataset by it. Intervals within a subject
(time since dose, study days, visit windows) are unchanged, while
calendar dates no longer line up between subjects or with real dates.
This is the "date offsetting" technique described in EMA's Anonymisation
Report Form Instructions: a random offset, with the same offset applied
to all dates for each participant.

## Usage

``` r
anon_subject_dates(
  data,
  key = "USUBJID",
  vars = NULL,
  max_days = 365,
  min_days = 30,
  partial = c("shift", "remove"),
  keep_secrets = FALSE,
  seed = NULL
)
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of an earlier step such as `anon_subject_dates()`. Datasets
  without the `key` column are returned unchanged.

- key:

  Name of the subject identifier column.

- vars:

  Names of date columns to shift. By default, every character or factor
  column whose name ends in `DTC` (any case; ISO 8601 dates, as in SDTM)
  and every Date or POSIXct column, in each dataset that has the `key`.
  In SDTM supplemental qualifier datasets (with `QNAM` and `QVAL`),
  `QVAL` values that are ISO 8601 dates with more than a bare year are
  shifted too (year-only values are left alone because they can't be
  told apart from numbers; a warning names the columns where they were
  kept). In columns not named `*DTC`, such as `QVAL` or a result column
  given in `vars`, only the values that are dates are shifted and other
  values are kept; bare four-digit numbers are kept too (with a
  warning), unless every value of the column, in every dataset, is a
  bare year (such as a column of birth years). Columns given in `vars`
  must be Date, POSIXct, or character or factor columns of ISO 8601
  dates.

- max_days, min_days:

  Each subject's offset is drawn uniformly from the whole numbers
  between `min_days` and `max_days` days, earlier or later. Defaults 30
  and 365; `max_days` can be at most 3650. A warning is given when the
  range is so narrow, or includes 0, that offsets are easy to guess or
  some subjects keep their real dates.

- partial:

  How to handle partial ISO 8601 dates in character columns: `"shift"`
  (default) or `"remove"` (replace them with the column's missing
  value). With `"shift"`, a year-and-month date (`"2014-03"`, or SDTM
  `"2014-03--T08:00"` with the time kept) moves the middle of the month
  by the subject's offset and keeps month precision; a year-only date
  (`"2014"`, or SDTM `"2014---15"` with the day kept) moves by whole
  years, at least one, in the direction of the offset, so the real year
  is never kept. Both ends of an interval (`"2014-01-02/2014-01-05"`)
  are shifted.

- keep_secrets:

  Logical. If `TRUE`, keep each subject's offset; read it with
  [`anon_secrets()`](https://nash-delcamp-slp.github.io/anon/reference/anon_secrets.md).
  Default `FALSE`.

- seed:

  Optional seed for the random assignment, for reproducible tests.
  Anyone with the seed and the original data can rebuild the mapping, so
  do not set it for data you share. The global random number state is
  restored afterwards.

## Value

An object of class `"anon_deid_step"`, as for
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md).

## Details

Full ISO 8601 dates move by exactly the subject's offset and keep their
format and any time part. Date columns keep their type, and POSIXct
columns keep their type, time zone and clock time (the calendar date
moves, so a shift across a daylight saving change keeps 08:00 at 08:00).
Column attributes such as variable labels are kept; value labels (haven
`labels`) are dropped, and haven user-defined missing values
(`na_values`, `na_range`) in the key and date columns become plain
missing values. A clock time that doesn't exist on the new date (inside
a daylight saving gap) moves forward by the gap. Study day variables
(`--DY`) are not changed and stay consistent with the shifted full
dates, because each subject's dates all move together.

Year-and-month dates are shifted from the middle of the month, and
year-only dates by whole years, so the order between a partial date and
a full date of the same subject can change (for example a start month
can end up after an end date).

Dates in rows without a key cannot be tied to a subject's offset and are
removed, with a warning. In `*DTC` columns, values that are not
recognised ISO 8601 dates, or whose year would fall outside 0001-9999,
are removed with a warning; this includes a month and day without a year
(`"--12-15"`). Time-only values (`"-----T07:15"`) and durations
(`"P1D"`) have no date and are kept. Text columns given in `vars` must
contain at least one recognised date. Datasets without the `key` column
(for example trial-level domains such as `TS`, `TV` and `TA`) are left
unchanged and listed in the log; their real trial dates can narrow down
subjects' offsets. A warning lists any other column in a keyed dataset
that holds dates but was not shifted: Date or POSIXct columns left out
of `vars`, numeric columns with a SAS date format (in a separate
warning; convert them to Date or POSIXct to shift them), and text
columns with ISO 8601 dates as whole values, or dates inside text (which
can't be shifted) written as ISO 8601, with a month name (`05JAN2014`,
`UNK-JAN-2014`, `5 Jan 2014`, `Jan 5, 2014`, `JAN2014`) or as numbers
with separators (`01/05/2014`, `1/5/14`, `01.05.2014`, `2014/01/05`),
including the values kept in mixed columns such as `QVAL`. Other
formats, such as `20140105`, are not detected. It also lists any dataset
without `key` that has such columns or a column named like `key` in
another case.

Random numbers come from R's random number generator; see
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md).
Run this before or after
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md);
each subject keeps one offset either way. Under HIPAA, HHS does not
address date shifting directly, but Safe Harbor removes all elements of
dates except the year, so shifted dates do not meet it; see the package
README.

## Examples

``` r
data <- list(
  dm = data.frame(USUBJID = c("a", "b"), RFSTDTC = c("2014-01-02", "2013-07-19")),
  pc = data.frame(USUBJID = c("a", "a", "b"),
                  PCDTC = c("2014-01-02T08:00", "2014-01-03T09:30", "2013-07-19T08:00"))
)
anon_subject_dates(data)$data
#> $dm
#>   USUBJID    RFSTDTC
#> 1       a 2013-08-02
#> 2       b 2013-10-19
#> 
#> $pc
#>   USUBJID            PCDTC
#> 1       a 2013-08-02T08:00
#> 2       a 2013-08-03T09:30
#> 3       b 2013-10-19T08:00
#> 
```
