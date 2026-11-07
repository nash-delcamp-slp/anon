# Measure re-identification risk from quasi-identifiers

Measures how easily subjects could be singled out from the combination
of their quasi-identifiers (indirect identifiers such as age, sex, race,
ethnicity, site, height or weight). Each subject's group is the set of
subjects with exactly the same quasi-identifier values; `k` is the
group's size and the subject's risk is `1 / k`. Quasi-identifiers can
come from several datasets linked by `key`.

## Usage

``` r
anon_risk(data, quasi, key = "USUBJID", max_risk = 0.09)
```

## Arguments

- data:

  A data frame, a named list of data frames linked by `key`, or the
  result of a de-identification step such as
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md).

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

## Value

An object of class `"anon_risk"`, a list with:

- `summary`: one row with `n_subjects`, `n_quasi`, `max_risk_target`
  (the `max_risk` argument), `k_target` (the group size it requires),
  `n_unique` (subjects in a group of one), `min_k`, `max_risk` (the
  observed maximum, `1 / min_k`), `avg_risk` (mean of `1 / k` over
  subjects, equal to the number of groups divided by `n_subjects`),
  `n_below_k` (subjects in groups smaller than `k_target`),
  `prop_below_k` and `meets_target` (`min_k >= k_target`).

- `drivers`: one row per quasi-identifier with the number of distinct
  non-missing values (`n_values`), the number of missing values
  (`n_missing`), and `n_unique_without`, `n_below_k_without`,
  `min_k_without` and `avg_risk_without` (the result if that variable
  were left out). Rows are ordered so the variable whose removal helps
  most comes first: by `n_below_k_without`, then `n_unique_without`,
  then `avg_risk_without` (all ascending), then the order of `quasi`.

- `quasi`, `key`: the quasi-identifier names and key used.

- `sources`: one row per quasi-identifier giving where it comes from:
  `variable`, `dataset`, and either `column` or `test_col`, `test` and
  `result_col`, plus `filtered` (whether a `filter` was used). The
  filter itself is not stored, because it may contain identifying
  values.

The object holds counts only, no subject-level rows or key values. Use
[`anon_risk_subjects()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk_subjects.md)
for per-subject results.

## Details

Subjects are all non-missing key values found in the datasets used by
`quasi`; rows with a missing or blank key are dropped with a warning,
and a warning is given if keys differ only by leading or trailing
spaces. A subject with no value for a quasi-identifier (for example,
missing from `vs`) gets `NA` for it, and `NA` is treated as a value of
its own when forming groups. Blank strings, which is how SAS transport
(XPT) files store missing text, are treated as missing. Values are
otherwise grouped by exact equality.

Risk is measured on one value per subject for each quasi-identifier. A
copy shared with others usually contains every row of a dataset (for
example weight at every visit), and any row can identify a subject as
well as the measured one. The measurement only describes a shared copy
if every row of each quasi-identifier is generalized or removed in the
same way.

Risk is measured against the subjects in `data` only. Health Canada and
EMA allow or encourage measuring risk against a larger reference
population, which gives lower risk; measuring within the data, the
smallest population, is the strictest reading.

The default target comes from Health Canada's *Public Release of
Clinical Information* guidance. Its "Risk Threshold" section "encourages
adopting a 9% re-identification risk threshold (risk=0.09)", and its
section on measuring risk states "Adopting a risk threshold of risk=0.09
equates to a target cell size of 11 patients"
(<https://www.canada.ca/en/health-canada/services/drug-health-product-review-approval/profile-public-release-clinical-information-guidance/document.html>).
EMA's Anonymisation Report Form Instructions (EMA/482639/2023) state
that "Health Canada PRCI and EMA Policy 0070 guidance encourages a 9%
re-identification risk threshold (risk=0.09)" and describe the target as
a maximum "that should not be exceeded"
(<https://www.ema.europa.eu/en/documents/other/anonymisation-report-form-instructions_en.pdf>).
Because `1 / 11` (about 0.0909) exceeds 0.09, the default requires
groups of at least 12 so that the threshold is never exceeded;
`max_risk = 1/11` follows Health Canada's stated cell size instead. Both
set the threshold for public release of clinical documents; using it for
other sharing is a deliberately conservative choice. GDPR and HIPAA set
no numerical re-identification risk threshold. Meeting the target is
evidence about re-identification risk from the chosen quasi-identifiers,
not proof that data is anonymous or de-identified under any regime.

The `drivers` table shows what leaving out each quasi-identifier on its
own would achieve. It cannot see variables that identify the same
subjects together (for example height and weight), it shows the effect
of removing a variable rather than generalizing it, and it does not
measure information loss. Use it to decide what to try next, then
measure again.

## See also

[`anon_risk_subjects()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk_subjects.md)
for per-subject group sizes.

## Examples

``` r
dm <- data.frame(
  USUBJID = sprintf("S%02d", 1:6),
  AGE = c(60, 60, 60, 70, 70, 85),
  SEX = c("F", "F", "F", "M", "M", "M")
)
anon_risk(dm, quasi = c("AGE", "SEX"), max_risk = 1/3)
#> Re-identification risk
#> ======================
#> 
#> Quasi-identifiers: AGE, SEX 
#> Subjects: 6 
#> Target: maximum risk 0.3333333 (groups of at least 3) 
#> Maximum risk: 1 (smallest group: 1)  Average risk: 0.5 
#> Unique subjects (group of 1): 1 
#> Subjects in groups smaller than 3: 3 (50%)
#> Meets target: no 
#> 
#> Drivers (`_without` columns: result if that variable alone were left out):
#>  variable n_values n_missing n_unique_without n_below_k_without min_k_without
#>       AGE        3         0                0                 0             3
#>       SEX        2         0                1                 3             1
#>  avg_risk_without
#>         0.3333333
#>         0.5000000

vs <- data.frame(
  USUBJID = rep(dm$USUBJID, each = 2),
  VSTESTCD = rep(c("HEIGHT", "WEIGHT"), 6),
  VSSTRESN = c(160, 60, 160, 60, 165, 62, 180, 80, 180, 80, 175, 90),
  VSBLFL = "Y"
)
# Dropping any single variable changes nothing here: subjects 1-2 and 4-5
# match on every variable, and HEIGHT and WEIGHT single out the same
# subjects, which leave-one-out drivers cannot show.
anon_risk(
  list(dm = dm, vs = vs),
  quasi = list(
    dm = c("AGE", "SEX"),
    anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN",
                      filter = ~ VSBLFL == "Y")
  ),
  max_risk = 0.5
)
#> Re-identification risk
#> ======================
#> 
#> Quasi-identifiers: AGE, SEX, HEIGHT, WEIGHT 
#> Subjects: 6 
#> Target: maximum risk 0.5 (groups of at least 2) 
#> Maximum risk: 1 (smallest group: 1)  Average risk: 0.6666667 
#> Unique subjects (group of 1): 2 
#> Subjects in groups smaller than 2: 2 (33.3%)
#> Meets target: no 
#> 
#> Drivers (`_without` columns: result if that variable alone were left out):
#>  variable n_values n_missing n_unique_without n_below_k_without min_k_without
#>       AGE        3         0                2                 2             1
#>       SEX        2         0                2                 2             1
#>    HEIGHT        4         0                2                 2             1
#>    WEIGHT        4         0                2                 2             1
#>  avg_risk_without
#>         0.6666667
#>         0.6666667
#>         0.6666667
#>         0.6666667
```
