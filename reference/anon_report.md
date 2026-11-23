# Build a report for selected objects, applying your redaction rules

Build a report for selected objects, applying your redaction rules

## Usage

``` r
anon_report(
  envir = globalenv(),
  selection = NULL,
  pattern_list = list(),
  default_replacement = getOption("anon.default_replacement", default = "[REDACTED]"),
  example_values_n = getOption("anon.example_values_n", default = 0),
  example_rows = getOption("anon.example_rows"),
  check_approximate = getOption("anon.check_approximate", default = FALSE),
  max_distance = 2,
  df_variable_names = NULL,
  df_classes = NULL,
  check_names = TRUE,
  check_labels = TRUE,
  nlp_auto = getOption("anon.nlp_auto"),
  fidelity = NULL
)
```

## Arguments

- envir:

  An environment or named/unnamed list of objects.

- selection:

  Optional character vector of object names to include.

- pattern_list:

  A list of patterns to search for and replace. Can include:

  - Named elements where names are replacement values and values are one
    or more patterns to match

  - Unnamed elements where one or more patterns are replaced with
    `default_replacement` This parameter is combined with the global
    option `getOption("anon.pattern_list")`.

- default_replacement:

  Value to use as the default replacement value when no specific
  replacement is provided. Default is
  `getOption("anon.default_replacement", default = "\[REDACTED\]")`.

- example_values_n:

  Optional number of example unique values to include for
  discrete/text-like data frame columns. Defaults to `0`, which disables
  example values.

- example_rows:

  Optional example-row specification for data frames. Use `NULL` to
  disable examples, a single number to request that many rows per data
  frame, or
  [`anon_example_rows()`](https://nash-delcamp-slp.github.io/anon/reference/anon_example_rows.md)
  to build a spec with explicit arguments such as `n`, `key`, `method`,
  and `n_key_values`. Rows are chosen on the source values, so `key` and
  `value` refer to the data as passed in.

- check_approximate:

  Logical indicating whether to check for approximate matches using
  string distance. Default is
  `getOption("anon.check_approximate", default = FALSE)`.

- max_distance:

  Maximum string distance for approximate matching when
  `check_approximate` is `TRUE`. Default is `2`.

- df_variable_names:

  For data frames, a character vector or named list specifying which
  variables should be replaced:

  - Unnamed elements: variables are replaced with `default_replacement`

  - Named elements: variable names are keys, value can be either a
    replacement value or a function This parameter is combined with the
    global option `getOption("anon.df_variable_names")`.

- df_classes:

  For data frames, a character vector or named list specifying which
  variable classes should be replaced:

  - Unnamed elements: variables with matching classes are replaced with
    `default_replacement`

  - Named elements: class names are keys, value can be either a
    replacement value or a function Class names are matched
    case-insensitively against
    [`class()`](https://rdrr.io/r/base/class.html) of each column.
    `"double"` also matches plain double columns (not `Date` or
    `POSIXct`). Character columns also match `"double"`/`"numeric"`,
    `"integer"`, `"Date"` or `"POSIXct"` when every non-missing,
    non-blank value parses as that type (dates as ISO 8601); a
    replacement function then receives the parsed values. This parameter
    is combined with the global option `getOption("anon.df_classes")`.

- check_names:

  Logical indicating whether to anonymize object names (column names,
  row names, list names, and the names and dimnames of vectors and
  columns; a data.table's key and indices follow its column names).
  Default is `TRUE`.

- check_labels:

  Logical indicating whether to anonymize labels (attributes) of
  columns, lists and data frames: variable and data frame labels,
  comments and other attributes that aren't structural (text at any
  depth in them, such as lists like SPSS `variable.labels` or readr's
  `spec`, factors, data frames, their names and their own attributes,
  and the names of these attributes), haven value labels (the label
  names, and the label values when they are text) and text user-defined
  missing values (`na_values`, and the ends of a text `na_range`), on
  vectors and columns wherever
  [`anon()`](https://nash-delcamp-slp.github.io/anon/reference/anon.md)
  reaches them (in data frames, lists and list columns). Attributes that
  are structural for the object's class (see the `other_attributes`
  argument of
  [`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md))
  are not redacted, except the names of a factor's contrasts matrix,
  which follow its levels. Value labels whose codes the redaction made
  equal are merged into one entry, their original labels joined with
  `" / "` and then redacted, with a warning naming the column as it is
  named in the output (or a list element, as `col[2]` or `col$name`);
  equal missing-value codes are deduplicated without a warning, as which
  values are missing doesn't change. A text `na_range` whose redacted
  ends would be in descending order is kept, with a warning. Whatever
  `check_labels` says, a warning names a column whose redaction changed
  which values are user-defined missing values. Default is `TRUE`.

- nlp_auto:

  List of logical values with names corresponding to entity names. Can
  be generated with
  [`nlp_auto()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_auto.md)
  and can be set as the `anon.nlp_auto` global option. This argument
  overrides the global option.

- fidelity:

  Optional result of
  [`anon_check_fidelity()`](https://nash-delcamp-slp.github.io/anon/reference/anon_check_fidelity.md)
  for the de-identification step the report describes, added to its
  De-identification section.

## Value

An object of class `"anon_report"` containing the names of the selected
objects (`object_names`), an object inventory and an environment summary
for them, with your rules applied to all of it, object names and a
step's dataset names (`step$dm`) included: every part shows the same
redacted names, made unique when two redact alike (`"[DRUG]"`,
`"[DRUG] 1"`). `selection` takes the objects' real names. For a
de-identification step, also `deidentification` (see Details).

## Details

**De-identification.** When `envir` is a de-identification step (such as
an
[`anon_deidentify()`](https://nash-delcamp-slp.github.io/anon/reference/anon_deidentify.md)
result), or holds one, the report gains a `deidentification` element
telling whoever reads the copy what was changed: `caveats` (short notes
generated from the step's log and settings, such as "Dates are shifted
by a per-subject offset ..."), `risk` before and after (smallest group,
subjects below the target, maximum risk), `meets_target` and `not_met`,
`settings` (rule parameters), `levels` (the level each quasi-identifier
was released at), `profile_rules`, `signals` (residual signals, counts
only), `steps` (the step log) and `fidelity`, if given. It holds counts,
names and rule parameters only, with your rules applied as to the rest
of the report; never data values, pseudonym maps, date offsets or
original keys, even when the step kept its secrets. With several steps,
`deidentification` holds one such section per step, named as the report
names the step, and `fidelity` can't be given.
