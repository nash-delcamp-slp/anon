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
  Data frames nested in them are de-identified too (see "Nested data" in
  Details). In datasets without the `key` column, at the top or in
  nested data, the values are left unchanged; keys in their attributes
  and names are rewritten (see Details).

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
each step's arguments, without the seed value, and `findings`: a tibble
of what its warnings name as left in the data, one row per location,
with `kind` (such as `"keys_left"`, `"dates_unshifted"` or `"objects"`),
`dataset`, `column` (as the warnings print them), `n_values` (the
distinct text values holding what was found, `NA` where that doesn't
apply) and `single_value` (whether the column holds one value in every
row); no values) and `secrets` (`NULL` unless `keep_secrets = TRUE` here
or in an earlier step). Saving the object (for example with
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
subject's pseudonym (a numeric key with its number), and original values
of `also` columns with the pseudonym's number as their column gets it
(with the same zero padding), in every dataset (with or without `key`),
when they appear as a whole value or a whole word, not next to a letter
or digit (so a number only as a whole number), compared without leading
or trailing spaces, and next to `","` or `"."` too (`"AB-1001,AB-1002"`,
`"AB-1001.1"`, `"77010001,5"`: such text is ambiguous and is rewritten).
Values made only of digits could be part of a number, so two rules apply
to them: after a digit and `"."` (the fraction of `"25.701054"` or
`"10.10005"`) they are found only when the number before the `"."` is
itself found as a key or `also` value (after a digit and `","`, as in
`"9999,10000"`, they are found); and values of `also` columns before
`"."` or `","` and a digit (`"10001.5"`) only when the number after it
is found, or, in names, before `"."` and digits at the end (as
[`make.unique()`](https://rdrr.io/r/base/make.unique.html) names
elements). In names, keys and `also` values as
[`make.names()`](https://rdrr.io/r/base/make.names.html) writes them (as
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) names columns:
`"X77010001"`, `"AB.1001"`) are rewritten too, to their replacement as
[`make.names()`](https://rdrr.io/r/base/make.names.html) writes it. They
are replaced in the attributes of columns, data frames and the list of
datasets (such as `label`, `comment` or a custom note), at any depth
(text in list or factor attributes, the values of data frames held in
attributes, names of attribute vectors, attributes of attributes,
attribute names); in the names of vector columns and the dimnames of
matrix columns; in haven value labels of columns that are not replaced
(the label names, and the label values of character columns) and in
character user-defined missing values (`na_values`, `na_range`) of those
columns; and in column names, character row names and dataset names.
Structural attributes (see `other_attributes`) are left alone, except
that text in a vctrs `ptype`, and the names of a factor's contrasts
matrix, are rewritten; a factor whose values are replaced or shifted
drops a contrasts matrix (named by its old levels, it no longer fits the
new ones), and keeps a contrasts function's name such as `"contr.sum"`.
So are environments, functions and S4 objects held in attributes (they
can refer to themselves, are shared with whoever else holds them, or
must keep valid slots; S4 objects are named in a warning). The grouping
of a grouped or rowwise tibble (`groups`) and the key and indices of a
data.table (`sorted`, `index`) are renamed with the columns. A grouped
tibble held in an attribute has its grouping values rewritten with its
values. With `other_attributes = "drop"`, attributes beyond the core
ones are dropped instead of rewritten. A label value or missing-value
code is rewritten only when no value of the column equals it or its
rewrite (or, for `na_range`, falls in either range), so the mapping from
values to labels and which values are missing don't change; a code kept
this way is named in a warning, as is a missing-value range whose
rewritten ends would be in descending order. Value labels whose codes
the rewrite made equal are merged into one entry, their original labels
joined with `" / "` and then rewritten (haven requires distinct codes;
label text may repeat), and equal missing-value codes are deduplicated;
a warning names each. Ranges are compared as R compares text in the
session's collation, as haven does. A rewritten name that clashes with
another gets a numeric suffix (`"_1"`), and settings and the log name
the suffixed column; a column name in the settings that was renamed
differently in different datasets is rewritten as text instead. Column
and dataset names marked as `"bytes"` are handled like other names and
stay marked as `"bytes"` unless rewritten. A dataset with nothing to
rewrite is returned as the same object, so `:=` on a data.table changes
the input too; once a later step changes it, its `.internal.selfref` is
stale (call
[`data.table::setDT()`](https://rdrr.io/pkg/data.table/man/setDT.html)).
Keys made only of digits, numeric keys and values of `also` columns are
searched for and rewritten only when at least 5 characters long (digits
count): shorter ones match too many unrelated numbers and codes. A value
of an `also` column that belongs to more than one subject becomes the
column's name in brackets, such as `"[SUBJID]"` (whose number to give it
isn't known), and a warning counts those found. A value found in both a
text and a numeric `also` column becomes the text column's form (the
padded number). In names (column, row and dataset names, vector and list
names and dimnames, at any depth), a key or `also` value followed by
digits at the end of the name, as
[`unlist()`](https://rdrr.io/r/base/unlist.html) names the elements of a
list named by key (`"AB-1001"` gives `"AB-10011"`, `"AB-10012"`),
becomes its replacement followed by the same digits, unless the whole
name is itself a key (the longest key wins). This can't tell such a
suffix from a name that happens to continue a key with digits (for a
numeric key, a longer number): such names are rewritten all the same.
The log has one row per dataset with such rewrites (and one for the list
of datasets), with counts by kind; this and earlier steps' logs and
settings are renamed and rewritten to match. The same text is rewritten
inside nested data (packed data frame columns and the data frames in
list columns, see below), and in list columns the attributes and names
of the vectors and plain lists the elements hold, at any depth. Text
values in the data are not rewritten, neither in columns nor in
list-column elements or nested data frames; they are searched (see
below).

**Nested data.** Data frames nested in the datasets are de-identified
like datasets: packed data frame columns (as made by
[`tidyr::pack()`](https://tidyr.tidyverse.org/reference/pack.html)) and
list columns of data frames (plain lists or vctrs `list_of`, as made by
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html),
[`dplyr::group_nest()`](https://dplyr.tidyverse.org/reference/group_nest.html)
or
[`dplyr::nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html)),
at any depth, including data frames held in plain lists inside list
columns. Each row of a packed column belongs to its outer row's subject,
and each data frame in a list column to its row's subject, unless it has
its own `key` column: then each of its rows belongs to the subject in
that column. Its `key`, `also` and `key_cols` columns are replaced as in
a dataset, subjects found only in nested data get pseudonyms too, and a
data frame with its own key has its rows sorted by the new key (row
names reset); others keep their rows in order. Row names of nested data
frames (tibbles too) are reset when their rows are sorted, with those of
their packed columns, whose rows move with them (as in a dataset sorted
by the new key); in those that belong to their outer row, only row names
that are row positions (integer, or made of digits) are reset, and other
row names, such as the terms of a table of model coefficients, are kept
and rewritten as text. A dataset (or nested data frame) without the
`key` column whose rows still get one subject each from nested data is
de-identified as if it had the key, each row having that subject: its
rows are sorted by the new key and its own dates shifted. This is so
when a packed column has the key (as
[`tidyr::pack()`](https://tidyr.tidyverse.org/reference/pack.html) makes
it), and, when its rows don't already belong to subjects, when the data
frames in one of its list columns hold exactly one subject in every row
(such as data split by subject; a row with no data frame, or none with a
key, holds none). A table where every row happens to hold one subject
(such as a per-test table where each test found one subject) is treated
the same way. When nested data gives the rows subjects in ways that
disagree (two packed columns with the key, or a packed and a list
column), a warning names the table. When they hold several subjects
(such as data nested by test, each element holding `USUBJID`), the
nested data frames are de-identified through their key, and the
dataset's own columns are left unchanged, in their order, like a dataset
without the key. Subjects are collected from every nested data frame.
Rows with no subject (a missing key, or a blank key in the outer row)
follow the rules above, the missing value being chosen for each data
frame; nested data frames with no key, in a dataset without the key, are
left unchanged like such datasets. Column types and labels are kept; a
`list_of` keeps its class, and its prototype (with its columns' labels)
follows the new elements (such as factor levels); grouped tibbles are
regrouped. List-column elements that are not data frames, vectors, plain
lists or `NULL` (fitted models such as `lm` or parsnip fits, recipes,
workflows, `rsplit` and `rset` objects, environments, functions,
formulas, S4 objects) are not entered or changed, and their columns are
named in a warning. They can hold copies of the original data:
de-identify the raw data first, and build models, recipes and resampling
splits on the de-identified copy. Changes inside nested data are logged
with counts, per dataset, with the column's path as `variable` (such as
`"data[[]][USUBJID]"`).

A warning lists any other character or factor column (values or factor
levels), list column (the text values its elements hold: character
values and factor levels, at any depth), or column of a nested data
frame (named as `dataset.column[sub]` inside a packed column,
`dataset.column[[]][sub]` for column `sub` of the data frames in list
column `column`, and deeper as `dataset.column[sub][[]][subsub]`), in
any dataset, that still contains original key values as whole values or
as whole words inside text, or followed by digits at the end of a value
(searched for as described above; values are not rewritten), and, in a
second warning, original values of `also` columns (such as `SUBJID`) the
same way; and any dataset without `key` that has a column named like
`key`, `also` or `key_cols` in any case. In datasets with `key`, and in
nested data frames with a subject, a column named like one of them in
another case (such as `usubjid` beside `USUBJID`) is not replaced and is
named in a warning. Only text or factor columns can be given in
`key_cols`; the others must be redacted or removed. Locations printed in
warnings have keys rewritten, as names can hold them. Columns of S4
objects are not searched or changed, and are named in a warning as data
that can't be checked. Missing values in `also` columns stay missing. In
`key`, `also` and `key_cols` columns, haven user-defined missing values
(`na_values`, `na_range`) become plain missing values and the codes are
dropped, because a code can't be told apart from a real identifier; a
warning names the columns where a value that became missing this way is
also a subject key elsewhere (both steps warn for the `key` column).

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
