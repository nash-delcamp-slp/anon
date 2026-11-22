# Redact and replace sensitive information in R objects

This function redacts or replaces sensitive information in various R
objects including character vectors, factors, data frames, and lists. It
uses pattern matching to find and replace sensitive content, with
options for targeted anonymization based on variable names or classes
and warnings about approximate matches.

## Usage

``` r
anon(
  x,
  pattern_list = list(),
  default_replacement = getOption("anon.default_replacement", default = "[REDACTED]"),
  check_approximate = getOption("anon.check_approximate", default = FALSE),
  max_distance = 2,
  df_variable_names = NULL,
  df_classes = NULL,
  check_names = TRUE,
  check_labels = TRUE,
  nlp_auto = getOption("anon.nlp_auto"),
  closures = getOption("anon.closures", default = "copy"),
  .self = FALSE,
  .pattern_replacements = NULL,
  .compiled = NULL,
  .where = NULL
)
```

## Arguments

- x:

  The object to anonymize. Can be a character vector, factor, data
  frame, list, de-identification step or environment.

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
  vectors and columns wherever `anon()` reaches them (in data frames,
  lists and list columns). Attributes that are structural for the
  object's class (see the `other_attributes` argument of
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

- closures:

  What a closure, formula, terms object or quosure whose environment R
  doesn't save by reference (such as a function's frame) refers to in
  the result: `"copy"` (the default) gives it that environment's
  redacted copy, so it keeps working; `"detach"` gives it the global
  environment, so no data of that environment is kept, and it no longer
  finds what it needs there. See "Environments" below. Can be set as the
  `anon.closures` global option.

- .self:

  Logical for internal use only. Used in recursive calls. Default is
  `FALSE`. When `TRUE`, warnings are collected as attributes instead of
  being issued immediately and global options are ignored and only
  explicitly provided parameters are used.

- .pattern_replacements:

  List for internal use only. Pre-computed pattern replacement pairs
  passed down during recursive calls to avoid recomputing them for each
  column or list element. Default is `NULL`, which triggers normal
  computation.

- .compiled:

  List for internal use only. Pre-compiled pattern groups and the
  combined single-pass regex. Passed down during recursive calls to
  avoid recompiling patterns. Default is `NULL`.

- .where:

  Character for internal use only. Where `x` is (such as a list column's
  element, `"col[2]"`), for warnings. Default is `NULL`.

## Value

An object of class `anon_context` with the same structure as `x` but
with sensitive information replaced. For a de-identification step (see
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)),
the step itself is returned with its data redacted, the patterns also
applied to its log and settings, and its secrets dropped (read them from
the original step); combine `step$data` rather than the step with
[`c()`](https://rdrr.io/r/base/c.html). For an environment, a new
environment holding redacted copies of its objects (see "Environments"
below). If approximate matches are found and `.self` is `FALSE`,
warnings are issued. If `.self` is `TRUE`, warnings are attached as an
attribute.

## Details

`anon()` operates recursively on nested structures. For data frames:

- Individual columns are processed based on their content

- Entire columns can be replaced if specified in `df_variable_names` or
  `df_classes`

- Column names, row names, and labels (including haven value labels) are
  redacted when enabled

- List columns are redacted element by element, nested data frames
  included, and keep their class (`AsIs`, vctrs `list_of`, whose
  prototype is redacted like the elements)

Lists, list columns and the classed lists they hold (such as fitted
models) keep their attributes and class, with their elements and
attributes redacted. Records (vctrs `rcrd`) and `POSIXlt` date-times
have their text fields and attributes redacted. Environments held inside
other objects are replaced by redacted copies (see "Environments"
below). Objects `anon()` can't search for text (functions, calls and
formulas, such as a model's `call` or a model frame's `terms` attribute,
whose code and the constants in it are kept, objects embedded in that
code or in a function's formals, environments included, whatever
`closures` says, S4 and S7 objects (such as ggplot2 4 plots), raw
vectors (which may hold serialized data), and the global, base and
package environments and namespaces, which are kept by reference) are
left as they are, and one warning lists where they are (by element name
where there is one, as `fit$call`; the first ten, and how many more):
remove them, or rebuild them from redacted data, before sharing.
Functions and calls lose their source references (`srcref`, which can
hold the text of the whole file they were sourced from, comments
included), wherever `anon()` finds them: functions then print as
deparsed code (S4 functions, such as generics and methods, stay S4). A
function's own attributes are redacted like any object's, and so are a
formula's or a call's, except its environment and, for a terms object,
the attributes models rely on (`variables`, `predvars`, `term.labels`,
`factors`, `dataClasses`, `order`, `intercept`, `response`, `specials`,
`offset`). Reference class objects (made with
[`methods::setRefClass()`](https://rdrr.io/r/methods/refClass.html)) and
their generators can't be copied without breaking them, and are dropped,
with a warning. These rules apply to such an object given to `anon()`
directly too.

Secrets read from a de-identification step with
[`anon_secrets()`](https://nash-delcamp-slp.github.io/anon/reference/anon_secrets.md),
and a step's secrets environment, are dropped wherever `anon()` finds
them (attributes included, where a step's secrets are dropped too, and
as the environment of a closure or formula, or as an environment's
parent), with a warning (a step's own secrets are dropped as described
under Value). External pointers and weak references are kept without a
warning: they hold no text, and are saved as null.

Text that isn't valid UTF-8 (such as a stray Windows-1252 byte) is
searched through a repaired copy with the invalid bytes shown as hex
(e.g. `"<92>"`), which is written back where a pattern matched; a
pattern that matches digits can match inside such a marker. Text marked
as `"bytes"` that isn't valid UTF-8 is left as it is, with a warning.

Pattern matching is case-insensitive. When `check_approximate` is
enabled, `anon()` will warn about remaining potential matches that are
similar but not exact.

Replacement functions can be provided in `df_variable_names` and
`df_classes` as:

- R functions that take the column/variable as input

- Formula notation (e.g., `~ .x + rnorm(length(.x), mean = 1)`)

The returned object has class `anon_context` which allows it to be
combined with other objects returned by `anon()` using
[`c()`](https://rdrr.io/r/base/c.html).

## Environments

For an environment, `anon()` returns a new environment holding a
redacted copy of each of its objects (hidden ones included), named as
`check_names` says (with NLP entities found in the names too); the
environment given and its objects are not changed. Its class and other
attributes are not kept: the copy is a plain environment (of class
`anon_context`).

The copy's parent is the environment's parent when R saves that parent
by reference (the global, base or empty environment, a package or a
namespace). Any other parent, such as a function's frame, would be saved
with the copy, and the objects it holds with it, so, with
`closures = "copy"`, the copy's parent is that parent's redacted copy,
made the same way (and so on up the chain): the result can then hold
redacted copies of objects that weren't in the environment given, such
as those of the function whose frame is the parent
([`new.env()`](https://rdrr.io/r/base/environment.html) called inside a
function has that call's frame as its parent). With
`closures = "detach"`, the copy's parent is then the global environment,
with a warning.

A de-identification step in it is redacted as above, its secrets
dropped. Environments held in it, or anywhere `anon()` reaches (lists,
list columns, attributes), are replaced by redacted copies made the same
way, each environment copied once per call so that environments that
refer to each other, or that several objects share, map to one copy;
such a copy keeps its class and other attributes (redacted), and its
parent is the parent's copy (or the global environment, as above) when
the parent isn't kept by reference. An S4 object that contains an
environment keeps its class and slots (redacted), with its environment
replaced by a redacted copy whose parent is the global environment (the
original's is a frame of the methods package); so does one given to
`anon()` directly. A function's `...` in a copied frame is rebuilt from
its values, redacted. The global, base and empty environments, packages
and namespaces are kept by reference. A closure, formula, terms object
or quosure whose environment R doesn't save by reference (such as a
function's frame) is handled as `closures` says, wherever `anon()` finds
it (in environments, lists, list columns and attributes, such as a
model's `terms`), with a warning naming them:

- `"copy"` (the default) gives it that environment's redacted copy (with
  that environment's parents, as above): it keeps working, on the
  redacted objects, and saving the copy doesn't save the original's
  data. But the copy then holds the environment the closure was created
  in (such as the frame of the function call that made it) and, in turn,
  the environments that one was created in, up to the first one R saves
  by reference (the global environment, a package or a namespace). Their
  objects are redacted by your patterns only: values that match no
  pattern are kept.

- `"detach"` gives it the global environment: none of that environment's
  objects is kept, at the cost of closures, formulas and models that
  need them (they may fail, or find other objects of the same name in
  the global environment). It is the strict choice: the parents of the
  environments `anon()` copies become the global environment too.

The environment given to `anon()`, and environments held in it as data,
are copied whatever `closures` says. Objects that rely on reference
semantics (such as R6 objects, their generators and inheritance, and
closures from function factories such as memoised functions) work on
their copy, which is no longer linked to the original; their active
bindings are not copied, and nothing in the copy is locked. Objects
nested several hundred levels deep (such as a long chain of
environments) are an error. Reading the objects forces promises. Active
bindings (reading one would run its function) and objects that can't be
read (a missing argument, or a promise whose code fails) are not copied,
with a warning. The copy is not locked, whether or not the environment
given is. A name that redacts to nothing is replaced by `"[unnamed 1]"`
and so on. Names that redact alike are made unique (`"[DRUG]"`,
`"[DRUG] 1"`), as in reports.

## Global Options

The following global options affect function behavior:

- `anon.default_replacement`:

  Default replacement text (default: "\[REDACTED\]").

- `anon.pattern_list`:

  Global patterns to combine with (after) `pattern_list` parameter.

- `anon.df_variable_names`:

  Global variable name specifications to combine with (after)
  `df_variable_names` parameter.

- `anon.df_classes`:

  Global class specifications to combine with (after) `df_classes`
  parameter.

- `anon.closures`:

  Default `closures` (`"copy"` or `"detach"`).

- `anon.nlp_auto`:

  List of logical values indicating which NLP entity types should be
  automatically redacted. Use
  [`nlp_auto()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_auto.md)
  to generate this list. Override the option by setting the `nlp_auto`
  argument.

- `anon.nlp_default_replacements`:

  Default NLP replacement labels. Use
  [`nlp_default_replacements()`](https://nash-delcamp-slp.github.io/anon/reference/nlp_default_replacements.md)
  to generate this list.

- `anon.example_values_n`:

  Default `example_values_n` used by
  [`anon_data_summary()`](https://nash-delcamp-slp.github.io/anon/reference/anon_data_summary.md)
  and
  [`anon_report()`](https://nash-delcamp-slp.github.io/anon/reference/anon_report.md).

- `anon.example_rows`:

  Default `example_rows` specification used by
  [`anon_data_summary()`](https://nash-delcamp-slp.github.io/anon/reference/anon_data_summary.md)
  and
  [`anon_report()`](https://nash-delcamp-slp.github.io/anon/reference/anon_report.md).
  Use
  [`anon_example_rows()`](https://nash-delcamp-slp.github.io/anon/reference/anon_example_rows.md)
  to generate this value.

See
[`anon_options()`](https://nash-delcamp-slp.github.io/anon/reference/anon_options.md)
for a central helper that lists and sets all supported `anon.*` options.

To set global options:

    options(anon.pattern_list = list("EMAIL" = "@\\S+"))
    options(anon.df_variable_names = c("name", "email"))
    options(anon.default_replacement = "[HIDDEN]")
    options(anon.nlp_auto = nlp_auto(person = TRUE))

## Examples

``` r
# Basic string anonymization
text <- c("John Smith", "jane.doe@email.com", "Call 555-1234")
anon(text, pattern_list = c("John Smith", "@\\S+", "\\d{3}-\\d{4}"))
#> [1] "[REDACTED]"         "jane.doe[REDACTED]" "Call [REDACTED]"   

# Using named patterns for specific replacements
anon(text, pattern_list = list("PERSON" = "John Smith",
                               "EMAIL" = "@\\S+",
                               "PHONE" = "\\d{3}-\\d{4}"))
#> [1] "PERSON"        "jane.doeEMAIL" "Call PHONE"   

# Data frame anonymization
df <- data.frame(
  name = c("Alice", "Bob"),
  email = c("alice@test.com", "bob@test.com"),
  score = c(95, 87)
)

# Anonymize specific columns by name
anon(df, df_variable_names = c("name", "email"))
#>         name      email score
#> 1 [REDACTED] [REDACTED]    95
#> 2 [REDACTED] [REDACTED]    87

# Anonymize columns by class with custom replacements
anon(df, df_classes = list("character" = "HIDDEN"))
#>     name  email score
#> 1 HIDDEN HIDDEN    95
#> 2 HIDDEN HIDDEN    87

# Using functions for dynamic replacement
anon(df, df_variable_names = list("name" = ~ paste("Person", seq_along(.x))))
#>       name          email score
#> 1 Person 1 alice@test.com    95
#> 2 Person 2   bob@test.com    87

anon_df <- df |>
  anon(
    df_variable_names = list(
      "name" = ~ paste("Person", seq_along(.x)),
      "email"
    )
  )

# Using global options
options(anon.pattern_list = list("EMAIL" = "@\\S+"))
options(anon.df_variable_names = "name")
anon(df)  # Will anonymize emails and names using global settings
#>         name      email score
#> 1 [REDACTED] aliceEMAIL    95
#> 2 [REDACTED]   bobEMAIL    87

# Combine processed objects
anon_summary <- anon_data_summary(list(df = df))
combined <- c(anon_df, anon_summary)
combined
#> === DATA CONTEXT ===
#> 
#>       name      email score
#> 1 Person 1 [REDACTED]    95
#> 2 Person 2 [REDACTED]    87
#> 
#> Environment Data Summary
#> ========================
#> 
#>   total_objects data_frames other_objects total_memory
#> 1             1           1             0         1272
#> 
#> Data Frames:
#> ------------
#>   name       type n_rows n_cols memory_size
#> 1   df data.frame      2      3      1.2 Kb
#> 
#> 
#> Variable Details (df):
#> 
#> -------------------- 
#>   variable data_type n_distinct n_missing n_total pct_missing label
#> 1     name character          2         0       2           0  <NA>
#> 2    email character          2         0       2           0  <NA>
#> 3    score   numeric          2         0       2           0  <NA>
#> 
#> 
#> 
```
