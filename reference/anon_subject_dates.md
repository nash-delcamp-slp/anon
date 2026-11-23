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
  seed = NULL,
  other_attributes = c("rewrite", "drop"),
  also = "SUBJID"
)
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of an earlier step such as `anon_subject_dates()`. Data frames
  nested in them are de-identified too (see "Nested data" in Details).
  In datasets without the `key` column, at the top or in nested data,
  the values are left unchanged; keys in their attributes and names are
  rewritten (see Details).

- key:

  Name of the subject identifier column.

- vars:

  Names of date columns to shift. By default, every character or factor
  column whose name ends in `DTC` (any case; ISO 8601 dates, as in SDTM)
  and every Date, POSIXct or POSIXlt column, in each dataset whose rows
  have subjects and in nested data frames with a subject (see "Nested
  data" in
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md));
  a list column holding Date, POSIXct or POSIXlt values counts as a Date
  column. `vars` names columns at every depth of nested data. In SDTM
  supplemental qualifier datasets (with `QNAM` and `QVAL`), `QVAL`
  values that are ISO 8601 dates with more than a bare year are shifted
  too (year-only values are left alone because they can't be told apart
  from numbers; a warning names the columns where they were kept). In
  columns not named `*DTC`, such as `QVAL` or a result column given in
  `vars`, only the values that are dates are shifted and other values
  are kept; bare four-digit numbers are kept too (with a warning),
  unless every value of the column, in every dataset, is a bare year
  (such as a column of birth years). Columns given in `vars` must be
  Date, POSIXct, or character or factor columns of ISO 8601 dates.

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
  years, `max(1, round(|offset| / 365.25))`, so the real year is never
  kept, in a direction (earlier or later) drawn at random for each
  subject apart from the offset, so that it doesn't reveal which way the
  subject's dates moved. Both ends of an interval
  (`"2014-01-02/2014-01-05"`) are shifted.

- keep_secrets:

  Logical. If `TRUE`, keep each subject's offset and the direction of
  its year-only dates (`offset_days` and `year_direction`, -1 or 1);
  read them with
  [`anon_secrets()`](https://nash-delcamp-slp.github.io/anon/reference/anon_secrets.md).
  Default `FALSE`.

- seed:

  Optional seed for the random assignment, for reproducible tests.
  Anyone with the seed and the original data can rebuild the mapping, so
  do not set it for data you share. The global random number state is
  restored afterwards.

- other_attributes:

  What to do with attributes beyond the core ones (variable and dataset
  labels, comments, haven value labels and user-defined missing values,
  and names, which are always rewritten): custom text attributes, list-
  and data frame-valued attributes, Date or factor values held in
  attributes, and attributes of attributes. `"rewrite"` (default)
  rewrites what it can find in them, at any depth, as a best effort.
  `"drop"` removes them, and is the strict choice: nothing can hide in
  an attribute that is gone. Structural attributes are never dropped,
  and are structural only on the classes that use them: `class`,
  `names`, `row.names`, `dim` and `dimnames` on anything; `levels` and
  `contrasts` on factors; `tzone` on date-times; `units` on difftime and
  units objects; `tsp` on time series; `type` and `states` on
  [`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html)
  objects; `ptype` on vctrs objects (its text is still rewritten);
  `groups` on grouped and rowwise tibbles; `sorted`, `index` and
  `.internal.selfref` on data.tables; SAS, SPSS and Stata formats, and a
  numeric `display_width`, on vectors. Elsewhere an attribute with one
  of these names is text (for example free-text `units` on a number).
  The log counts the attributes dropped in each dataset.

- also:

  Names of related identifier columns (default `"SUBJID"`), as in
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md).
  This step doesn't replace them; their values are masked in the
  locations its warnings and errors print, each as the column's name in
  brackets (such as `"[SUBJID]"`). When `data` is the result of
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md),
  that step's `also` is used instead (and its values are already
  replaced). Recorded in the settings.

## Value

An object of class `"anon_deid_step"`, as for
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md).

## Details

Full ISO 8601 dates move by exactly the subject's offset and keep their
format and any time part. Date columns keep their type, and POSIXct and
POSIXlt columns keep their type, time zone and clock time (the calendar
date moves, so a shift across a daylight saving change keeps 08:00 at
08:00). Column attributes such as variable labels are kept, with the
dates in them replaced by `"[DATE]"` (see below); value labels (haven
`labels`) are dropped from shifted columns, and haven user-defined
missing values (`na_values`, `na_range`) in the key and date columns
become plain missing values. A clock time that doesn't exist on the new
date (inside a daylight saving gap) moves forward by the gap. Study day
variables (`--DY`) are not changed and stay consistent with the shifted
full dates, because each subject's dates all move together.

Dates can't be tied to one subject's offset in text that describes the
data, so there they are replaced with `"[DATE]"` (the default date
replacement of
[`nlp_default_replacements()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_default_replacements.md)):
in every dataset, in the same attributes and names that
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
rewrites keys in (character attributes at any depth, vector names and
dimnames, value labels and user-defined missing values under the same
rule, column, row and dataset names), and in this and earlier steps'
logs and settings, with `other_attributes` as described there. Date and
date-time values held in attributes (Date, POSIXct, POSIXlt) become the
text `"[DATE]"` (zero-length ones, such as a vctrs prototype, hold no
date and are left alone), and a factor held in an attribute whose levels
now read the same becomes text rather than have its levels merged. Value
labels whose codes become equal are merged as described there. The forms
replaced are those found inside text by the warning described below: ISO
8601 full and year-and-month dates, SDTM partial dates
(`"2014-01--T08:00"` becomes `"[DATE]T08:00"`, and `"2014---15"`), and
dates with a month name or as numbers with separators. The log has one
row per dataset with such replacements, with counts. Values in the data
are not changed by this rule.

Dates in nested data move by the offset of the subject their row belongs
to (see "Nested data" in
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)),
with the same rules, and so do Date and date-time values held in list
columns, at any depth through plain lists. Text values in list columns
are not shifted. Objects that can't be de-identified, such as fitted
models, recipes and resampling splits, are named in a warning:
de-identify the raw data first, and build them on the de-identified
copy.

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
contain at least one recognised date. In datasets without the `key`
column (for example trial-level domains such as `TS`, `TV` and `TA`) the
values are left unchanged (dates in their attributes and names are
replaced as above), and they are listed in the log; their real trial
dates can narrow down subjects' offsets. A warning lists any other
column in a keyed dataset that holds dates but was not shifted: Date or
POSIXct columns left out of `vars`, numeric columns with a SAS date
format (in a separate warning; convert them to Date or POSIXct to shift
them), and text columns with ISO 8601 dates as whole values, or dates
inside text (which can't be shifted) written as ISO 8601, with a month
name (`05JAN2014`, `UNK-JAN-2014`, `5 Jan 2014`, `Jan 5, 2014`,
`JAN2014`) or as numbers with separators (`01/05/2014`, `1/5/14`,
`01.05.2014`, `2014/01/05`), including the values kept in mixed columns
such as `QVAL`, list columns (the text values their elements hold) and
the columns of nested data frames. Other formats, such as `20140105`,
`2014.01.05`, `2014-1-5`, `Jan-05-2014`, `12/2014` and a month and day
without a year (`--12-15`), are not detected, nor replaced in attributes
and names. It also lists any dataset without `key` that has such columns
or a column named like `key` in another case.

Text that isn't valid UTF-8 (such as a stray Windows-1252 byte) is
searched through a repaired copy with the invalid bytes shown as hex
(e.g. `"<92>"`); where something is rewritten, the repaired text is
written back. A pattern that matches digits can match inside such a
marker. Both steps warn about text marked as `"bytes"`, which can't be
searched, in values, attributes and names: they count distinct texts and
name the columns, or datasets (`"(list)"` for attributes of the list of
datasets and dataset names), holding it. Convert it to UTF-8 (for
example with [`iconv()`](https://rdrr.io/r/base/iconv.html)) and run the
step again. Every location printed in the steps' warnings and errors has
keys and dates rewritten and unprintable bytes escaped (`"<e9>"`): this
step shows original keys as `"[KEY]"` and the original values of the
`also` columns in data with subjects as the column's name in brackets
(`"[SUBJID]"`). It names the columns inside nested data by position
(`dataset.column[2]`, `dataset.column[[]][2]`), in warnings and in the
log, so that a column is named the same way whether or not
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md),
which rewrites keys in nested names, ran first.

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
