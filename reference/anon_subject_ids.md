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
  seed = NULL,
  other_attributes = c("rewrite", "drop")
)
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of an earlier step such as
  [`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md).
  In datasets without the `key` column the values are left unchanged;
  keys in their attributes and names are rewritten (see Details).

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
labels are kept (original keys in their text are rewritten, see below).
Character keys get character pseudonyms, numeric keys get numbers,
factors stay factors. Value labels (haven `labels`), which describe the
original values, are dropped from replaced columns. Missing and blank
keys are left as they are. Values of `also` columns in rows without a
key cannot be mapped and are removed (set to missing), with a warning.
`key`, `also` and `key_cols` must not overlap.

Original keys in text that describes the data are replaced with the
subject's pseudonym, in every dataset (with or without `key`), when they
appear as a whole value or a whole word: in the attributes of columns,
data frames and the list of datasets (such as `label`, `comment` or a
custom note), at any depth (text in list or factor attributes, the
values of data frames held in attributes, names of attribute vectors,
attributes of attributes, attribute names); in the names of vector
columns and the dimnames of matrix columns; in haven value labels of
columns that are not replaced (the label names, and the label values of
character columns) and in character user-defined missing values
(`na_values`, `na_range`) of those columns; and in column names,
character row names and dataset names. Structural attributes (see
`other_attributes`) are left alone, except that text in a vctrs `ptype`,
and the names of a factor's contrasts matrix, are rewritten; a factor
whose values are replaced or shifted drops a contrasts matrix (named by
its old levels, it no longer fits the new ones), and keeps a contrasts
function's name such as `"contr.sum"`. So are environments, functions
and S4 objects held in attributes (they can refer to themselves, are
shared with whoever else holds them, or must keep valid slots; S4
objects are named in a warning). The grouping of a grouped or rowwise
tibble (`groups`) and the key and indices of a data.table (`sorted`,
`index`) are renamed with the columns. A grouped tibble held in an
attribute has its grouping values rewritten with its values. With
`other_attributes = "drop"`, attributes beyond the core ones are dropped
instead of rewritten. A label value or missing-value code is rewritten
only when no value of the column equals it or its rewrite (or, for
`na_range`, falls in either range), so the mapping from values to labels
and which values are missing don't change; a code kept this way is named
in a warning, as is a missing-value range whose rewritten ends would be
in descending order. Value labels whose codes the rewrite made equal are
merged into one entry, their original labels joined with `" / "` and
then rewritten (haven requires distinct codes; label text may repeat),
and equal missing-value codes are deduplicated; a warning names each.
Ranges are compared as R compares text in the session's collation, as
haven does. A rewritten name that clashes with another gets a numeric
suffix (`"_1"`), and settings and the log name the suffixed column; a
column name in the settings that was renamed differently in different
datasets is rewritten as text instead. Column and dataset names marked
as `"bytes"` are handled like other names and stay marked as `"bytes"`
unless rewritten. A dataset with nothing to rewrite is returned as the
same object, so `:=` on a data.table changes the input too; once a later
step changes it, its `.internal.selfref` is stale (call
[`data.table::setDT()`](https://rdrr.io/pkg/data.table/man/setDT.html)).
Keys made only of digits, and numeric keys, are not rewritten, for the
same reason they are not searched for below. The log has one row per
dataset with such rewrites (and one for the list of datasets), with
counts by kind; this and earlier steps' logs and settings are renamed
and rewritten to match. Values in the data are not rewritten, including
the elements of list columns and packed data frame columns, with
everything inside them: a packed column's own attributes (even its
`label`) and names are left as they are, and handling nested data is
planned.

A warning lists any other character or factor column (values or factor
levels), list column (all the text its elements hold: values, factor
levels, names and attributes, at any depth), or column of a packed data
frame column (named as `dataset.column[subcolumn]`, and deeper as
`dataset.column[sub][subsub]`, with the text in its attributes; the
packed column itself is named for its column names and attributes), in
any dataset, that still contains original key values as whole values or
as whole words inside text (checked for character keys that are not made
only of digits, since numbers match many unrelated codes), and any
dataset without `key` that has a column named like `key`, `also` or
`key_cols` in any case. In datasets with `key`, a column named like one
of them in another case (such as `usubjid` beside `USUBJID`) is not
replaced and is named in a warning. Only text or factor columns can be
given in `key_cols`; the others must be redacted or removed. Locations
printed in warnings have keys rewritten, as names inside packed columns
can hold them. Columns of S4 objects are not searched or changed, and
are named in a warning as data that can't be checked. Original values of
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
