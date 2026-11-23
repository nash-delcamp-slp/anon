# De-identification profiles

A profile tells
[`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
what each variable is: a direct identifier to remove, the subject key
and related IDs, a date to offset, a quasi-identifier with its
generalization ladder, a variable derived from a quasi-identifier, or a
variable to keep. `anon_profile_sdtm()` and `anon_profile_adam()`
describe standard CDISC SDTM and ADaM variables; `anon_profile()` and
`anon_profile_rule()` build a profile for other data, or rules to add to
a standard one with `anon_profile_combine()`.

## Usage

``` r
anon_profile(..., name = "custom")

anon_profile_rule(
  variable,
  role,
  datasets = character(0),
  quasi_name = NULL,
  test_col = NULL,
  test = NULL,
  flag = NULL,
  ladder = list(),
  from = character(0),
  priority = NA_integer_
)

anon_profile_sdtm(min_n = 12)

anon_profile_adam(min_n = 12)

anon_profile_combine(...)

# S3 method for class 'anon_profile'
c(...)

# S3 method for class 'anon_profile'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- ...:

  For `anon_profile()`: rules made by `anon_profile_rule()`, profiles,
  or data frames with the columns of a profile (as
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  them), in order.

- name:

  A name for the profile, shown when it is printed and in warnings when
  profiles are combined.

- variable:

  Names of the variables the rule applies to: an exact name (`"AGE"`),
  an SDTM domain wildcard where `--` stands for the two-letter domain
  prefix, in capitals (`"--DTC"` matches `VSDTC` and `LBDTC`,
  `"--STDTC"` matches `AESTDTC` and `RFSTDTC`, `"--DY"` matches `VSDY`
  but not `body`), or a pattern with `*` (any characters) and `?` (one
  character), such as `"*DT"`. Several names make one rule each, sharing
  the other arguments. For a test (`test_col` given), the result column
  (`"VSSTRESN"`, `"AVAL"`).

- role:

  What the variable is:

  - `"remove"`: a direct identifier, blanked in every row (the column is
    kept, with its type and attributes, so code that uses it still
    runs);

  - `"key"`, `"also"`, `"key_cols"`: the subject key and related
    identifier columns, passed to
    [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
    as `key`, `also` and `key_cols`;

  - `"date"`: a date offset per subject by
    [`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md);

  - `"quasi"`: a quasi-identifier, measured, generalized up its `ladder`
    and suppressed;

  - `"derived"`: a variable derived from the quasi-identifiers in
    `from`, kept where it still follows from their released values and
    blanked elsewhere (see
    [`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md));

  - `"duration"`: a duration from the date in `from` to the date in
    `variable`, measured among the residual signals (see
    [`anon_quasi_duration()`](https://nash-delcamp-slp.github.io/anon/reference/anon_quasi_duration.md))
    and not changed;

  - `"keep"`: kept as it is.

- datasets:

  Names of the datasets the rule is limited to, exact or patterns with
  `*` and `?` (such as `"ad*"`). Default: every dataset.

- quasi_name:

  For `"quasi"` rules, the name of the quasi-identifier. Rules with the
  same name are copies of one quasi-identifier: such as baseline weight
  in SDTM `vs`, in ADaM `WEIGHTBL` and in `advs`. A quasi-identifier is
  measured from its source (see
  [`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)),
  generalized at the same level in every copy, and blanked for the same
  subjects in every copy. Default: `test` for a test, otherwise
  `variable`.

- test_col, test:

  For a quasi-identifier or derived variable held in the rows of a test,
  such as baseline weight in `vs`: the column holding test codes
  (`"VSTESTCD"`, `"PARAMCD"`) and the test code (`"WEIGHT"`), or a
  pattern of test codes with `*` and `?` (`"RACE*"`, each code found
  being a copy of its own). `variable` is then the result column.

- flag:

  For a test quasi-identifier, the column flagging the baseline record
  (`"VSBLFL"`, `"ABLFL"`; `"Y"` marks it). The measured value is the
  subject's first flagged result, or, for a subject without a flagged
  result (or a dataset without the column), its first result. Every row
  of the test is generalized and blanked, not only the measured one.

- ladder:

  For a quasi-identifier, its generalization ladder: a list of rules
  ([`anon_band()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md),
  [`anon_round()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md),
  [`anon_fold()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)),
  from the finest to the coarsest, each applied to the original values;
  the string `"remove"`, as the last step, blanks the quasi-identifier
  in every row. An empty list: the quasi-identifier can only be
  suppressed. For a duration, one
  [`anon_band()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
  giving the width of its bands in days.

- from:

  For a derived variable, the names of the quasi-identifiers it is
  derived from; for a duration, the start date column.

- priority:

  For a quasi-identifier, its usefulness, used to break ties when cells
  are suppressed (passed to
  [`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)
  as `order`) and in the ladder search: between options that are
  otherwise equal, a lower number is blanked first. It doesn't decide
  what is blanked when one option blanks fewer cells. Quasi-identifiers
  without one follow, in profile order.

- min_n:

  For the standard profiles, the smallest number of subjects a category
  must hold to be kept by their
  [`anon_fold()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
  steps (`RACE`, the race qualifiers of `SUPPDM`, `SITEID`). Default 12,
  the group size the default target of
  [`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
  (`max_risk = 0.09`) requires; set it to the group size of your target.

- x:

  A profile.

- row.names, optional:

  Not used.

## Value

An object of class `"anon_profile"`; for `anon_profile_rule()`, a
profile holding the rules.

## Details

**Structure.** A profile is a tibble of rules with class
`"anon_profile"` and columns `variable`, `datasets` (a list of character
vectors, empty for every dataset), `role`, `quasi_name`, `test_col`,
`test`, `flag`, `ladder` (a list of lists of rules), `from` (a list of
character vectors), `priority` and `profile` (the name of the profile
the rule came from).
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
it as a data frame to edit, and `anon_profile()` turns an edited data
frame back into a profile.

**Which rule applies.** For a column of a dataset, among the rules
(other than test rules) whose `variable` and `datasets` match it, a rule
limited to datasets wins over one that isn't, an exact name over a
pattern, and a later rule over an earlier one. Variable and dataset
names and patterns are matched ignoring case (`"RACE"` matches `race`),
except the domain prefix that `--` stands for, which is two capital
letters. Test rules apply to the rows of their test in every dataset (in
`datasets`) with the test and result columns.

**Companion variables.** ADaM names a variable's numeric code `XN` and
its decoded or long label `XL` or `XC` (`SITEIDN`, `COUNTRYL`,
`USUBJIDN`).
[`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
gives the companions of the key and of each `"also"` column the `"also"`
role (a numeric companion gets the number of the subject's pseudonym, 31
for `SUBJ-031`, the same in every dataset), and makes the companions of
each quasi-identifier column `"derived"` from it. A rule that matches a
companion's name wins, in the datasets it covers.

Columns matched by no rule are kept as they are, except that dates are
offset and subject IDs replaced wherever
[`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)
and
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
find them.
[`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
warns about columns, at any depth, whose names look like an age, a birth
date (`*BRTH*`, `*BIRTH*`, `DOB*`), a site, a numeric ID (`*IDN`), a
country (`COUNTRY*`, `CNTRY*`), a ZIP or postal code (`ZIP*`,
`POSTAL*`), a sex or gender, a race (`RACE1`, `RACEOTH`), an ethnicity,
a height (`HT`, `HTS` too), a weight (`WT`, `WTS`, `BW`, `BWT` too), a
BMI, a body surface area, a creatinine clearance or an eGFR, in any case
(units, named `*U`, aside), and that no rule matches, unless they hold
no value, or one value (and no other factor level, value label or
missing-value code) that at least as many subjects as the target's group
size share; such a column is a reason the target is not met. So is an
element of a list column (a plain list or vector held in it, at any
depth) named like one, or matched by a rule other than `"keep"`. It also
warns about `"keep"` rules that win over a quasi-identifier's rule for a
column.

**Datasets without the key.** A dataset without the key column whose
every row holds, in its `"also"` columns (such as `SUBJID`, or
`USUBJIDN`), a value that the datasets with the key give one subject,
and no value that points to no subject, to several or to another
subject, is processed as a dataset with the key (rows tied to those
subjects) and released without the key column. Any other dataset without
the key that holds a column matched by a `"key"`, `"also"`,
`"key_cols"`, `"quasi"`, `"derived"`, `"date"` or `"duration"` rule, or
that looks like a quasi-identifier, is released as it is and is a reason
the target is not met. A codelist or lookup table without the key (such
as a format table holding `SEX` and `SEXN`, or age bands with `AGEGR1`)
holds no subject's values: keep it knowingly with a `"keep"` rule
limited to that dataset, such as
`anon_profile_rule(c("SEX", "SEXN"), "keep", datasets = "fmt")` combined
with the profile. The rule then applies only there.

**Dates.**
[`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
offsets dates with
[`anon_subject_dates()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_dates.md)'s
own detection (every Date and date-time column and every text column
whose name ends in `DTC`, at any depth), and, with the same offsets, the
text columns of ISO 8601 dates matched by a `"date"` or `"duration"`
rule (such as `VISIT_DATE`), at any depth. A column matched by such a
rule that isn't a Date, a date-time or text (a number of days, with or
without a SAS date format, whose origin isn't known) is left as it is: a
warning names it, and it is a reason the target is not met.

**`anon_profile_sdtm()`.**

- remove: `BRTHDTC` (with `AGE` and an offset `RFSTDTC`, a birth date
  gives the age back exactly, even after offsetting, and a birth year
  undoes `AGE` bands, so it is removed rather than reduced to the year),
  `INVNAM`, `INVID`;

- key `USUBJID`, also `SUBJID`, key_cols `RSUBJID`;

- dates: `--DTC`, `--STDTC`, `--ENDTC` and every other `*DTC`;

- quasi-identifiers, each with its ladder: `AGE` (5-, 10- and 20-year
  bands, ages of 90 and over as 90), `SEX` (suppressed only), `RACE`
  (categories of fewer than `min_n` subjects folded into `"OTHER"`, a
  CDISC controlled term), `ETHNIC` (suppressed only: its controlled
  terminology has no neutral other value), `SITEID` (small sites merged
  into `"OTHER"`, then removed), `COUNTRY` (removed: ISO 3166 has no
  code for "other", so a small country can't be folded), the race
  qualifiers of `SUPPDM` (`QVAL` where `QNAM` is `RACE*`: copies of
  `RACE`, folded alike), and baseline `HEIGHT` and `WEIGHT` from `vs`
  (`VSSTRESN` with `VSORRES` and `VSSTRESC`; flag `VSBLFL`, else the
  first record: height is often recorded once, at screening, without the
  flag), rounded to units, then to tens, then in bands of 20;

- derived: `BMI` and `BSA` results in `vs`, from `HEIGHT` and `WEIGHT`
  (blanked where those changed); renal function results in `lb` (`CRCL`,
  `EGFR`) are not covered: add derived rules for them if needed;

- duration: `RFSTDTC` to `RFENDTC`, in bands of 7 days;

- keep: `AGEU` (the unit of `AGE`, not identifying), `--DY`, `--STDY`
  and `--ENDY` (study days, which offsetting keeps exact).

Priorities (ties are broken by blanking first): `SITEID`, `COUNTRY`,
`ETHNIC`, `RACE`, `HEIGHT`, `WEIGHT`, `AGE`, `SEX`.

The `AGE` ladder doesn't follow a study's age groups (such as `AGEGR1`
cut at 65): where a band straddles a cut, the group is blanked. To keep
them, combine the profile with an `AGE` rule whose ladder uses bands
that align with the cuts, such as `anon_band(5, bottom = 15)` for cuts
at 65 (`AAGE` in ADaM takes its own rule).

**`anon_profile_adam()`.** Rules match ADSL variables in every dataset
that carries them (ADSL, BDS and OCCDS datasets alike).

- remove: `BRTHDT`, `BRTHDTM`, `BRTHDTC`, `INVNAM`, `INVID`;

- key, also and key_cols as for SDTM;

- dates: `*DT`, `*DTM` and `*DTC` (such as `TRTSDT`, `TRTEDT`, `RANDDT`,
  `ADT`, `ADTM`);

- quasi-identifiers: `AGE` (with `AAGE`, the age at an event, as a
  copy), `SEX`, `RACE`, `ETHNIC`, `SITEID` and `COUNTRY` as for SDTM;
  `EDUCLVL` (suppressed only); `HEIGHT` and `WEIGHT` as copies in
  `HEIGHTBL`/`HTBL`/`HGTBL` and `WEIGHTBL`/`WTBL`/`WGTBL`, in the `AVAL`
  of their `PARAMCD` rows (flag `ABLFL`; `AVALC`, `BASE`, `BASEC`,
  `CHG`, `PCHG` and `R2BASE` follow, see
  [`anon_rule()`](https://nash-delcamp-slp.github.io/anon/reference/anon_rule.md))
  and in the SDTM results (`VSSTRESN`) that BDS datasets carry, with the
  SDTM ladders;

- derived: `RACEN`, `RACEGR*` from `RACE`; `SEXN` from `SEX`; `ETHNICN`
  from `ETHNIC`; `AGEGR*` (such as `AGEGR1` and `AGEGR1N`) from `AGE`;
  `AAGEGR*` from `AAGE`; `REGION*` from `COUNTRY`; `SITEGR*` from
  `SITEID`; `BMIBL` and `BMIBLGR*` from `HEIGHT` and `WEIGHT`; `BSA*`
  (such as `BSABL`) from `HEIGHT` and `WEIGHT`, and `CRCL*` and `EGFR*`
  (`CRCLBL`, `EGFRBL`) from `HEIGHT`, `WEIGHT`, `AGE` and `SEX`, their
  units (`*U`) kept; `AVALCAT*` and `AVALCA*N` of the `HEIGHT` and
  `WEIGHT` rows from them; the `BMI` and `BSA` parameters, from `HEIGHT`
  and `WEIGHT`; and the `CRCL` and `EGFR` parameters (renal function in
  a BDS dataset such as ADLB), from `HEIGHT`, `WEIGHT`, `AGE` and `SEX`;

- duration: `TRTSDT` to `TRTEDT`, in bands of 7 days.

Columns that hold a duration exactly, such as `TRTDURD`, `ADURN` or
`--DUR`, and study days (`ADY`, `--DY`) are kept: offsetting doesn't
change them, and they can single subjects out like the durations
measured among the residual signals. Remove them if that matters.

In BDS datasets, results (`AVAL`, `AVALC`, `BASE`, `BASEC`, `CHG`,
`PCHG`, `R2BASE`) are not quasi-identifiers, except for the baseline
height and weight parameters: a lab value or a blood pressure is not
something an attacker would know. Add rules for other parameters that
are.

**Combining.** `anon_profile_combine()` (or
[`c()`](https://rdrr.io/r/base/c.html)) joins profiles, such as the SDTM
and ADaM profiles for a list holding both. A rule of a later profile
replaces a rule of an earlier one with the same `variable`, `datasets`,
`test_col` and `test` (in its place); a replacement with another role is
named in a warning. Quasi-identifiers with the same name are one
quasi-identifier: one set of generalizations, applied to every copy.

## See also

[`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)

## Examples

``` r
anon_profile_sdtm()
#> <anon_profile> sdtm: 26 rule(s)
#> 
#> remove:
#>   INVNAM
#>   INVID
#>   BRTHDTC
#> 
#> key:
#>   USUBJID
#> 
#> also:
#>   SUBJID
#> 
#> key_cols:
#>   RSUBJID
#> 
#> date:
#>   --DTC
#>   --STDTC
#>   --ENDTC
#>   *DTC
#> 
#> quasi:
#>   AGE, ladder: anon_band(width = 5, top = 90) > anon_band(width = 10, top = 90) > anon_band(width = 20, top = 90), priority 7
#>   SEX, suppressed only, priority 8
#>   RACE, ladder: anon_fold(min_n = 12, to = "OTHER"), priority 4
#>   ETHNIC, suppressed only, priority 3
#>   SITEID, ladder: anon_fold(min_n = 12, to = "OTHER") > remove, priority 1
#>   COUNTRY, ladder: remove, priority 2
#>   VSSTRESN where VSTESTCD = HEIGHT, baseline flag VSBLFL, ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 5
#>   VSSTRESN where VSTESTCD = WEIGHT, baseline flag VSBLFL, ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 6
#>   QVAL where QNAM = RACE* in supp* (copy of RACE), ladder: anon_fold(min_n = 12, to = "OTHER"), priority 4
#> 
#> derived:
#>   VSSTRESN where VSTESTCD = BMI, from HEIGHT, WEIGHT
#>   VSSTRESN where VSTESTCD = BSA, from HEIGHT, WEIGHT
#> 
#> duration:
#>   RFENDTC, ladder: anon_band(width = 7), from RFSTDTC
#> 
#> keep:
#>   AGEU
#>   --DY
#>   --STDY
#>   --ENDY

# A profile for data that isn't CDISC: a key, a date and two
# quasi-identifiers.
anon_profile(
  anon_profile_rule("ID", "key"),
  anon_profile_rule("VISIT_DATE", "date"),
  anon_profile_rule("AGE", "quasi", ladder = list(anon_band(10, top = 90))),
  anon_profile_rule("SEX", "quasi"),
  name = "registry"
)
#> <anon_profile> registry: 4 rule(s)
#> 
#> key:
#>   ID
#> 
#> date:
#>   VISIT_DATE
#> 
#> quasi:
#>   AGE, ladder: anon_band(width = 10, top = 90)
#>   SEX, suppressed only

# The SDTM and ADaM profiles together, for a list holding both.
c(anon_profile_sdtm(), anon_profile_adam())
#> <anon_profile> sdtm + adam: 65 rule(s)
#> 
#> remove:
#>   INVNAM
#>   INVID
#>   BRTHDTC
#>   BRTHDT
#>   BRTHDTM
#> 
#> key:
#>   USUBJID
#> 
#> also:
#>   SUBJID
#> 
#> key_cols:
#>   RSUBJID
#> 
#> date:
#>   --DTC
#>   --STDTC
#>   --ENDTC
#>   *DTC
#>   *DT
#>   *DTM
#> 
#> quasi:
#>   AGE, ladder: anon_band(width = 5, top = 90) > anon_band(width = 10, top = 90) > anon_band(width = 20, top = 90), priority 7
#>   SEX, suppressed only, priority 8
#>   RACE, ladder: anon_fold(min_n = 12, to = "OTHER"), priority 4
#>   ETHNIC, suppressed only, priority 3
#>   SITEID, ladder: anon_fold(min_n = 12, to = "OTHER") > remove, priority 1
#>   COUNTRY, ladder: remove, priority 2
#>   VSSTRESN where VSTESTCD = HEIGHT, baseline flag VSBLFL, ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 5
#>   VSSTRESN where VSTESTCD = WEIGHT, baseline flag VSBLFL, ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 6
#>   QVAL where QNAM = RACE* in supp* (copy of RACE), ladder: anon_fold(min_n = 12, to = "OTHER"), priority 4
#>   AAGE (copy of AGE), ladder: anon_band(width = 5, top = 90) > anon_band(width = 10, top = 90) > anon_band(width = 20, top = 90), priority 7
#>   EDUCLVL, suppressed only, priority 3
#>   HEIGHTBL (copy of HEIGHT), ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 5
#>   HTBL (copy of HEIGHT), ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 5
#>   HGTBL (copy of HEIGHT), ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 5
#>   WEIGHTBL (copy of WEIGHT), ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 6
#>   WTBL (copy of WEIGHT), ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 6
#>   WGTBL (copy of WEIGHT), ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 6
#>   AVAL where PARAMCD = HEIGHT, baseline flag ABLFL, ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 5
#>   AVAL where PARAMCD = WEIGHT, baseline flag ABLFL, ladder: anon_round(digits = 0) > anon_round(digits = -1) > anon_band(width = 20), priority 6
#> 
#> derived:
#>   VSSTRESN where VSTESTCD = BMI, from HEIGHT, WEIGHT
#>   VSSTRESN where VSTESTCD = BSA, from HEIGHT, WEIGHT
#>   RACEN, from RACE
#>   RACEGR*, from RACE
#>   SEXN, from SEX
#>   ETHNICN, from ETHNIC
#>   AGEGR*, from AGE
#>   AAGEGR*, from AAGE
#>   REGION*, from COUNTRY
#>   SITEGR*, from SITEID
#>   BMIBL, from HEIGHT, WEIGHT
#>   BMIBLGR*, from HEIGHT, WEIGHT
#>   BSA*, from HEIGHT, WEIGHT
#>   CRCL*, from HEIGHT, WEIGHT, AGE, SEX
#>   EGFR*, from HEIGHT, WEIGHT, AGE, SEX
#>   AVALCAT* where PARAMCD = HEIGHT, from HEIGHT
#>   AVALCA*N where PARAMCD = HEIGHT, from HEIGHT
#>   AVALCAT* where PARAMCD = WEIGHT, from WEIGHT
#>   AVALCA*N where PARAMCD = WEIGHT, from WEIGHT
#>   AVAL where PARAMCD = BMI, from HEIGHT, WEIGHT
#>   AVAL where PARAMCD = BSA, from HEIGHT, WEIGHT
#>   AVAL where PARAMCD = CRCL, from HEIGHT, WEIGHT, AGE, SEX
#>   AVAL where PARAMCD = EGFR, from HEIGHT, WEIGHT, AGE, SEX
#> 
#> duration:
#>   RFENDTC, ladder: anon_band(width = 7), from RFSTDTC
#>   TRTEDT, ladder: anon_band(width = 7), from TRTSDT
#> 
#> keep:
#>   AGEU
#>   --DY
#>   --STDY
#>   --ENDY
#>   BSA*U
#>   CRCL*U
#>   EGFR*U
```
