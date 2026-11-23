# Check that code tested on a de-identified copy should run on the original

Compares a de-identified copy with the original data, property by
property, and reports what code written against the copy relies on that
no longer holds. Run it where the original data is, before sharing the
copy; the result holds counts, names and rule parameters only, so it can
be shared with the copy (see
[`anon_report()`](https://nash-delcamp-slp.github.io/anon/reference/anon_report.md)).

## Usage

``` r
anon_check_fidelity(original, deidentified, key = "USUBJID", dm = "dm")
```

## Arguments

- original:

  A data frame or a named list of data frames: the data before
  de-identification.

- deidentified:

  The same as `original` after de-identification: a data frame, a named
  list of data frames, or the step that made it (such as an
  [`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
  result). With a step, its log and settings say what was changed on
  purpose; without one, intended changes can't be told from others and
  are reported as `"warn"`.

- key:

  Name of the subject identifier column.

- dm:

  Name of the subject-level dataset whose keys every other dataset's
  keys must be found in, and whose `RFSTDTC` study days are counted from
  (`"adsl"` for ADaM data without `dm`).

## Value

A tibble of class `"anon_fidelity"` with columns `dataset`, `check`,
`status` (`"pass"`, `"fail"`, `"warn"` or `"not applicable"`),
`n_checked`, `n_failed` and `detail` (counts only), and attributes
`complete` (`TRUE` when nothing was left uncompared: no subjects that
couldn't be paired or told apart, no dates of subjects with no known
offset) and `passed` (`TRUE` when the result is complete and no check
failed). It prints, and converts with
[`as_anon_payload()`](https://nash-delcamp-slp.github.io/anon/reference/as_anon_payload.md)
and
[`as_anon_json()`](https://nash-delcamp-slp.github.io/anon/reference/as_anon_json.md).

## Details

Each check compares a property in the original with the same property in
the copy. It fails only when the property held in the original and is
broken in the copy. A property already broken in the original is
`"not applicable"`, with the original's count.

A check that compared nothing is `"not applicable"`, with the reason.

**Aligning rows.**
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
sorts rows by the new key, so the copy's rows don't line up with the
original's. Subjects are paired by their rows, coded from the columns no
step logged (and of the same class), in every dataset with `key`, in
turn: rows that match exactly; then, of the rest, subjects with as many
rows in every dataset and more than half of them equal (rows sharing a
code with more than 20 unpaired subjects don't count, which keeps this
linear; subjects with no other match stay unpaired); then rows that
match in the columns with no value more often in the copy than in the
original (a column changed throughout is left out); then, among subjects
alike in those columns, each subject's own dates (their formats and
their distances from the subject's first date, which the per-subject
offset keeps). A step's kept pseudonym map (`keep_secrets = TRUE`) is
used instead when there is one, and keys that weren't pseudonymized pair
directly. Within a subject, rows pair in order. Subjects alike in every
column no step logged and in their date pattern give the same result for
every check whichever way they pair, so when the copy holds as many of
them as the original, they are compared as a class: each copy subject is
paired, in a fixed order, with an original whose values explain the
cells a step logged (blanked, generalized or derived where the step says
so, and otherwise equal), and the checks run on them. With offset dates,
each pair's offset must be one the dates step can give (a full matching
of the class is required). If no pairing explains them all, the subjects
left fail. A very large class can't be paired this way: one whose logged
cells take more than 250,000 combinations of an original's and a copy's
values, or of more than 300 subjects that a greedy pass on their dates
can't pair; it is reported as not compared, and the result is
incomplete. Run the steps with `keep_secrets = TRUE` to pair such
subjects by the kept map. A class with a different number of subjects in
the copy can't be paired: those subjects are counted (`pseudonyms` is
`"warn"`), their rows are not compared one by one, and the result is
incomplete. A copy subject whose dates match none of the originals alike
in the other columns fails. Rows of datasets without `key` pair by
position.

**Checks** (one row per dataset and check; `"(all)"` for checks across
datasets):

- `datasets`: the same dataset names.

- `columns`: the same columns in the same order.

- `types`: the same class and type for each column, and for the columns
  of nested data frames (packed columns, and data frames in list
  columns).

- `attributes`: the same dataset and column labels, formats
  (`format.sas`, `format.spss`, `format.stata`, `display_width`), time
  zones (`tzone`), factor levels (their set and order) and value labels.
  Value labels and levels of the key, related IDs and dates may change;
  a column another step logged keeps the original's levels in their
  order, less those no longer used, plus the fold category. In a dataset
  where the log of
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
  or
  [`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)
  counts labels it rewrote, a changed label whose original holds a key,
  a related ID or a date is `"warn"`; any other change fails, and so
  does a label lost.

- `rows`: the same number of rows, or fewer only by the rows of subjects
  the step dropped (`"warn"`, with counts). Rows of subjects that
  couldn't be paired are counted, and make the result incomplete.

- `row order`: within each subject, rows in the same order.

- `keys in dm`: every key of the dataset is in `dm`.

- `distinct keys`: the original's number of subjects, less those
  dropped.

- `pseudonyms`: each subject has one pseudonym in every dataset, and
  each pseudonym one subject: every copy subject's rows across datasets
  match one original subject's. Related IDs follow the pseudonyms, row
  by row: a column the IDs step replaces (`SUBJID`) holds the
  pseudonym's number, and a key column (`RSUBJID`) the pseudonym of the
  subject the original names, or a blank where it names a subject the
  data doesn't hold; a blank where the original named one fails.

- `values`: every column no step logged keeps its values, row by row
  over the paired rows; the key, related IDs and dates have checks of
  their own. A cell a step logged may differ only by the change it
  logged: blank where a blanking step logged it; the band or rounding of
  the original value, or the fold's category where the rule folds the
  original's category (from the rule's parameters in the settings);
  numbers rewritten from a generalized result at the row's own
  conversion and decimals; recomputed values (the numeric relations
  check them); or a derived value that the original data pairs with the
  released value of its quasi-identifier. No more cells are blanked or
  changed than the log counts, test by test. A column logged for one
  test only is compared on the other rows. Copies of a column a step
  changed (`AGE` in `adsl` and in `advs`) must agree within each subject
  where they did in the original (dataset `"(all)"`). Packed data frame
  columns are compared the same way; data frames in list columns by
  their number of rows, and by their values when their rows keep their
  order (no key column, or one subject); the detail names each by its
  path (`NEST[[]]$T`).

- `categorical values`: each value of a character or factor column is a
  value of the original column, a blank, or a value a step introduced on
  purpose (a fold category of a generalization rule, numbers rewritten
  from a generalized result). Key, related ID and date columns are
  checked by the other checks.

- `ISO 8601`: each non-blank value of a `*DTC` column, or of a column a
  profile's `"date"` rule names, has the format and precision of the
  original value in its row. Values removed on purpose (not ISO 8601 in
  the original, partial dates with `partial = "remove"`, rows without a
  key, columns a step blanked) are counted apart.

- `date intervals`: within each subject, every date (text, Date and
  date-time columns, in every dataset) moves by the subject's offset, so
  the interval between any two is kept; clock times are kept. The offset
  lies within the dates step's range (`min_days` to `max_days`), or is 0
  when a step ran that doesn't offset dates. Year-and- month dates are
  compared at month precision (the month of the original's day 15 moved
  by the offset, as
  [`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)
  shifts them), and year-only dates must all move by one number of
  years, the subject's year step (`max(1, round(|offset| / 365.25))`, 0
  for no offset).

- `study days`: `--DY` recomputed from `--DTC` and `RFSTDTC` (SDTM rule,
  no day 0), and ADaM `*DY` from `*DT` and `TRTSDT`, agree with the
  copy's values wherever they agreed in the original.

- `numeric relations`: `CHG = AVAL - BASE`, `PCHG = 100 * CHG / BASE`
  and `--STRESN` equal to the number in `--STRESC`, wherever they held
  in the original. Rows where a step changed one of the values on
  purpose (and logged it) are counted apart; if the relation no longer
  holds in some of them, the check is `"warn"`.

**Not checked:** business rules beyond these; changes among subjects
alike in every unchanged column and their date pattern that another
pairing of them explains (they are compared as a class, and without the
pseudonym map which one is which can't be known); the derived values of
several quasi-identifiers beyond being blank or unchanged; values of
data frames nested in list columns whose rows hold several subjects
(their order changes), and dates and keys inside nested data; statistics
(means and counts by group change with banding and suppression). A pass
is evidence that code relying on these properties should behave the same
on the original, not proof that it will.

**Sharing the result.** It names datasets and columns as the copy does,
and never an original's name;
[`print()`](https://rdrr.io/r/base/print.html),
[`as_anon_payload()`](https://nash-delcamp-slp.github.io/anon/reference/as_anon_payload.md)
and
[`as_anon_json()`](https://nash-delcamp-slp.github.io/anon/reference/as_anon_json.md)
don't apply your `pattern_list`. To share it redacted, include it in a
report: `anon_report(step, fidelity = result, pattern_list = ...)`.

**Cost.** A few passes over every column of every dataset, plus
groupings of the rows, linear in the number of rows: a few seconds for
the pharmaversesdtm and pharmaverseadam datasets.

## See also

[`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md),
[`anon_report()`](https://nash-delcamp-slp.github.io/anon/reference/anon_report.md)

## Examples

``` r
dm <- data.frame(
  USUBJID = sprintf("S%02d", 1:12),
  AGE = c(31, 34, 38, 42, 45, 47, 52, 55, 58, 61, 66, 69),
  SEX = rep(c("F", "M"), 6),
  RFSTDTC = format(as.Date("2020-01-01") + 0:11)
)
result <- anon_deidentify(list(dm = dm), max_risk = 1/3, seed = 1)
anon_check_fidelity(list(dm = dm), result)
#> Fidelity of the de-identified copy: passed 
#> pass: 12, warn: 0, fail: 0, not applicable: 0 
#> 
#>  dataset check              status n_checked n_failed
#>  (all)   datasets           pass    1        0       
#>  (all)   pseudonyms         pass   12        0       
#>  dm      columns            pass    4        0       
#>  dm      types              pass    4        0       
#>  dm      attributes         pass    4        0       
#>  dm      rows               pass   12        0       
#>  dm      row order          pass   12        0       
#>  dm      distinct keys      pass   12        0       
#>  dm      values             pass   24        0       
#>  dm      categorical values pass   12        0       
#>  dm      ISO 8601           pass   12        0       
#>  dm      date intervals     pass   12        0       
#>  detail                                                                                                                                                 
#>  1 dataset(s)                                                                                                                                           
#>  12 of 12 subject(s) paired by the rows of the columns no step changed; 12 of them alike in every unchanged column and in their dates, paired as a class
#>  4 column(s), in the same order                                                                                                                         
#>  4 column(s) with the same class and type, nested data frames included                                                                                  
#>  4 column(s)                                                                                                                                            
#>  12 row(s) in the original, 12 in the copy                                                                                                              
#>  12 paired subject(s); 0 with rows in another order                                                                                                     
#>  12 subject(s) in the original, 12 in the copy, 0 dropped                                                                                               
#>  24 value(s) of paired rows compared; 6 blanked on purpose; 6 generalized or derived on purpose                                                         
#>  12 non-blank value(s) in 1 column(s)                                                                                                                   
#>  12 non-blank value(s) in 1 column(s) of paired rows                                                                                                    
#>  12 date(s) of 12 subject(s) compared with the subject's offset                                                                                         
```
