# De-identify linked datasets with a profile

Turns a list of SDTM and/or ADaM datasets (or other data described by a
profile) into a copy whose measured re-identification risk meets a
target, with as little damage as possible: it removes direct
identifiers, pseudonymizes subjects, offsets their dates, generalizes
quasi-identifiers up their ladders, and suppresses what remains.

## Usage

``` r
anon_deidentify(
  data,
  profile = anon_profile_sdtm(),
  key = "USUBJID",
  max_risk = 0.09,
  max_suppressed = 0.05,
  unresolved = c("drop", "keep"),
  keep_secrets = FALSE,
  seed = NULL
)
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of an earlier step.

- profile:

  An
  [`anon_profile()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md),
  such as
  [`anon_profile_sdtm()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md),
  [`anon_profile_adam()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md),
  or both combined with [`c()`](https://rdrr.io/r/base/c.html) for a
  list holding SDTM and ADaM datasets.

- key:

  Name of the subject identifier column. It must be the profile's
  `"key"` rule, if it has one.

- max_risk:

  Highest acceptable re-identification risk for any subject (the maximum
  risk, `1 / k` for the smallest group). The target group size is the
  smallest `k` with `1 / k <= max_risk`, allowing for floating-point
  error so that 0.1 and 1/3 give 10 and 3. The default, 0.09, is the
  threshold Health Canada and EMA encourage for public release of
  clinical information, and requires groups of at least 12. Health
  Canada equates 0.09 with a cell size of 11, but `1 / 11` is about
  0.0909; to use groups of 11, pass `max_risk = 1/11`. See Details.

- max_suppressed:

  Largest share of quasi-identifier cells (subjects times the measures
  of the quasi-identifiers that vary) that suppression may blank before
  the search stops generalizing; above it, the search climbs the ladders
  even when a step saves no cells. Default 0.05.

- unresolved:

  What to do with subjects still in groups smaller than the target when
  nothing is left to blank: `"drop"` (default) removes them from every
  dataset, `"keep"` keeps them (see
  [`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)).

- keep_secrets:

  Logical. If `TRUE`, keep the pseudonym mapping, the date offsets and
  the folded categories; read them with
  [`anon_secrets()`](https://nash-delcamp-slp.github.io/anon/reference/anon_secrets.md).
  Default `FALSE`: they are kept nowhere.

- seed:

  Optional seed, for reproducible tests: the pseudonyms are drawn with
  `seed` and the date offsets with `seed + 1`. Anyone with the seed and
  the original data can rebuild the mapping and the offsets, so don't
  set it for data you share.

## Value

An object of class `"anon_deid_step"` (see
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)),
chainable with the other steps, with the steps' logs and settings
(`settings$deidentify` for this function; `deidentify_1` and so on when
it runs again on its own result), the record of suppressed cells, and
also `risk_before` and `risk_after` (from
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md))
and `signals` (a tibble with `signal`, `dataset`, `n_subjects`,
`n_unique`, `n_below_k` and `n_below_k_with_quasi`). Secrets only with
`keep_secrets = TRUE`.

## Details

**Steps.**

1.  Direct identifiers (`"remove"` rules) are blanked in every row of
    the datasets and of the data frames nested in them (packed and list
    columns, at any depth); the columns are kept, with their type and
    attributes, so code that uses them still runs, but without value
    labels or missing-value codes, which can hold the identifier. An
    exact `"remove"` name found nowhere is noted in the log.

2.  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
    replaces the key, `"also"` and `"key_cols"` columns. A dataset
    without the key whose rows an `"also"` column ties to subjects (see
    [`anon_profile()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md))
    takes the key for these steps, and is released without it.

3.  [`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)
    offsets every date of each subject by one random offset (its own
    detection: Date and date-time columns, and text columns named
    `*DTC`, at any depth); text columns matched by the profile's
    `"date"` rules (such as `VISIT_DATE`, in the datasets with the key)
    are offset by the same offsets, as `*DTC` columns are: values that
    aren't ISO 8601 dates (`10MAR2021`, `03/10/2021`, `--03-10`) are
    removed, with a warning, and bare years move by whole years. So are
    those of the data frames nested in them, each row with the offset of
    the subject that owns it (rows no subject owns are blanked).

4.  The quasi-identifiers the profile finds are measured
    ([`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)).
    Each is measured from its source: the copy with one value per
    subject that holds a value for the most subjects. Every other copy
    that shows some subject another value than the source (such as
    `WEIGHTBL` from another visit than the `vs` baseline, or a copy
    holding subjects the source doesn't) is measured too, as a measure
    of its own: it is generalized at the quasi-identifier's level and
    suppressed like the others, so the released combination is what is
    measured. Copies are never overwritten with the source's values. A
    column copy with more than one value for a subject (such as `AAGE`
    across events) can't be measured: a warning names it, and the target
    is not met. A quasi-identifier with no copy that holds one value per
    subject can't be measured at all: a warning names it, it is released
    as it is, the variables derived from it are blanked, and the target
    is not met.

5.  **Ladder search** (greedy, as Datafly), on the measured values (one
    per subject and measure). The cost of a state is the cells
    suppression would blank, estimated with
    [`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)'s
    algorithm (a subject it can't resolve counts as all its cells), plus
    every cell of a removed quasi-identifier. While above
    `max_suppressed` of the cells, or while a step lowers the cost, it
    takes one step up the ladder of the quasi-identifier whose next step
    costs least (ties: the step that loses the fewest distinct values,
    then profile order). When every ladder is exhausted above the
    budget, it keeps the state that cost least (the earliest on ties).
    Last, it steps back down wherever that doesn't raise the cost (most
    useful quasi-identifiers first), so steps taken on a plateau are
    undone. Each ladder step applies to the original values. A
    quasi-identifier with one value for every subject in every measure
    can't split a group: it is measured, but neither generalized nor
    suppressed.

6.  The chosen steps are applied to every copy of each quasi-identifier
    ([`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md),
    in every dataset with the key that the rule covers; `"remove"`
    blanks every copy in every row), then
    [`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)
    blanks what remains in every measure, with `everywhere = TRUE` and
    the profile's priorities as `order` (they break ties). Cells blanked
    in a source are blanked for the same subjects in its copies that
    aren't measured on their own (copies under another name, and test
    copies such as the `WEIGHT` rows of `advs`).

7.  Derived variables are kept where they still follow from the released
    values of their quasi-identifiers, and blanked elsewhere (below).
    Value labels of generalized or suppressed columns keep only the
    values still released. The risk is measured again.

**Nested data.** Columns of the data frames nested in a dataset with the
key (packed columns and the data frames in list columns, at any depth)
take the dataset's rules. Those of quasi-identifiers and derived
variables are measured and changed like datasets of their own, each row
with the subject that owns it (see
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)):
a nested copy of `AGE` is generalized and suppressed with it, and
measured on its own where it shows some subject another value. They are
named after where they are, such as `dm.extra` for packed column `extra`
of `dm` and `dm.visits[[]]` for the data frames in list column `visits`.
Rows that no subject owns are blanked in those columns, and logged.

**Derived variables.** A derived variable is left as it is where no
ladder step applies to its quasi-identifiers and the subject's cells of
them weren't blanked. Otherwise (in every row, when a ladder step
applies; whether a value happened to stay the same, as an age on a
band's lower bound, doesn't matter), with one quasi-identifier in the
same row (such as `AGEGR1` with `AGE` in `adsl`):

- from a text quasi-identifier (`RACEN` from `RACE`), it takes the value
  that the released value had in the original data (the code of
  `"OTHER"`, say) when the original data maps each value to one derived
  value, and is blanked otherwise or where there is none;

- from a numeric one (`AGEGR1` from `AGE`), it takes a category only
  when the derived values are intervals of the quasi-identifier in the
  original data and every value the released one can stand for (all ages
  of a band, whole numbers when the original values are) lies within one
  category: between its lowest and highest observed values, the lowest
  category being open below and the highest above (between two
  categories, where the cut lies isn't known); otherwise it is blanked.

A derived variable with several quasi-identifiers (`BMIBL`), or whose
quasi-identifiers aren't in the same row (the `BMI` parameter), is
blanked in every row when a ladder step applies to one of them, and
otherwise for the subjects whose cells of one of them were blanked; for
a result column (`AVAL`, `--STRESN`), with the other results of the row.
Blanking keeps values truthful: a derived value recomputed from a
representative (such as a BMI from a rounded weight) would not be any
subject's. `from` can name the column of a copy (`AAGEGR1` from `AAGE`);
a column copy named as the quasi-identifier (`AGE` rather than `AAGE`)
is preferred. A derived value missing in the data stays missing. A
derived variable whose `from` names a quasi-identifier that isn't
measured (not in the data, or with no copy that can be measured, such as
`BMIBL` without height and weight) is blanked in every row, with a
warning.

**When the target is met.** `settings$deidentify$meets_target` is `TRUE`
only when the risk measured on the released data meets the target and
[`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)
found no other reason not to (copies of a quasi-identifier that differ,
birth dates next to `AGE`, values of removed subjects left); reasons are
in `settings$deidentify$not_met`. No quasi-identifier found in the data
is a reason too. So is each kind of identifier or date that
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
and
[`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)
name in their warnings as left in the data (their settings' `findings`):
original keys or related IDs in other columns or inside text, columns
named like an identifier in another case, datasets without the key
holding identifiers or dates, dates that weren't shifted (dates inside
free text, text dates outside `*DTC` and the profile's `"date"` rules,
numbers with a SAS date format, bare years kept), objects that can't be
de-identified, value labels holding keys or dates, and text that can't
be searched. A date column that holds one value in every row (such as a
dictionary version, `"2019-09"`, in a dataset without the key) can't
tell subjects apart and isn't a reason; identifiers always are. No date
parser is guessed: SAS datetimes held as text in a result column (such
as `AVALC` of the `FPPDTM1` parameter of ADaM's ADAB) aren't shifted,
are named in a warning, and the target is not met; remove them or
convert them first. Last, an audit walks every dataset at every depth
for what the steps left as it was, each kind named in a warning and a
reason: columns, supplemental qualifiers and elements of list columns
that look like quasi-identifiers without a rule (see
[`anon_profile()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md));
datasets without the key (and no `"also"` columns that tie their rows to
subjects) holding columns the profile names or that look like
quasi-identifiers; profile date columns that weren't offset (such as a
number of days); quasi-identifier or derived columns holding lists, and
elements of list columns that a rule matches; attributes of
quasi-identifier, derived and test result columns other than those that
describe them (label, formats, value labels and missing-value codes,
class, levels, names, time zone and the type's structural attributes),
which can hold their original values and are left as they are; and data
frames held in attributes whose columns the profile names or that look
like quasi-identifiers.

**Residual signals.** Meeting the target covers the quasi-identifiers
(measured), and the identifiers and dates the steps find (above). Other
things a person might know single subjects out too: treatment duration
(the profile's `"duration"` rules, which offsetting doesn't change; in
days and in the rule's bands), the number of rows a subject has in each
dataset, the visits (`VISITNUM`, `AVISITN`) and study days (`--DY`,
`ADY`) it has, the set of tests (`--TESTCD`, `PARAMCD`) it has, and, for
each generalized test, the set of its released values across all rows
(such as weights at every visit, not only the measured baseline).
Columns that hold a duration exactly (`TRTDURD`, `ADURN`, `--DUR`) are
kept and not measured. `signals` counts, for each, the subjects unique
on it alone and in groups smaller than the target, alone and together
with the released quasi-identifiers. They are measured, not changed
(changing dates would break study days). When subjects are in small
groups on them, the copy is not anonymous against someone who knows
them: say so wherever it is shared.

The log has rows for every step and every ladder move, with counts and
no values; the settings hold the profile (as text), the levels chosen,
the risk before and after and the residual signals.

Meeting the target is evidence about re-identification risk from the
profile's quasi-identifiers, not proof that the data is anonymous under
any regime (see
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)).
Shifted dates do not meet HIPAA Safe Harbor.

## See also

[`anon_profile()`](https://nash-delcamp-slp.github.io/anon/reference/anon_profile.md),
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md),
[`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md),
[`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)

## Examples

``` r
dm <- data.frame(
  USUBJID = sprintf("S%02d", 1:12),
  AGE = c(31, 34, 38, 42, 45, 47, 52, 55, 58, 61, 66, 69),
  SEX = rep(c("F", "M"), 6),
  RFSTDTC = format(as.Date("2020-01-01") + 0:11)
)
result <- anon_deidentify(list(dm = dm), max_risk = 1/3, seed = 1)
result$data$dm
#>    USUBJID AGE SEX    RFSTDTC
#> 1  SUBJ-01  40   M 2019-09-17
#> 2  SUBJ-02  40   F 2019-10-12
#> 3  SUBJ-03  40   F 2020-11-04
#> 4  SUBJ-04  NA   M 2019-11-21
#> 5  SUBJ-05  40   M 2020-08-26
#> 6  SUBJ-06  40   F 2019-10-27
#> 7  SUBJ-07  NA   F 2020-11-24
#> 8  SUBJ-08  40   M 2019-04-03
#> 9  SUBJ-09  NA   F 2019-01-30
#> 10 SUBJ-10  NA   M 2019-07-01
#> 11 SUBJ-11  NA   M 2019-08-06
#> 12 SUBJ-12  NA   F 2019-07-22
result$settings$deidentify$meets_target
#> [1] TRUE
```
