# Rules that generalize quasi-identifier values

Each function returns a rule for
[`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md):
`anon_band()` puts numbers into bands, `anon_round()` rounds them, and
`anon_fold()` merges small categories into one. A rule is also a
function: call it on a vector to apply it, as in `anon_band(5)(ages)`.
Rules keep the vector's type, class and attributes, so code written for
the original values still runs.

## Usage

``` r
anon_band(width, top = NULL, bottom = NULL, value = c("lower", "midpoint"))

anon_round(digits)

anon_fold(min_n, to)
```

## Arguments

- width:

  Width of each band, a positive number (for example 5 for 5-year age
  bands). Bands start at `bottom` when given, otherwise at 0: with
  `width = 5` they are 0-4.99..., 5-9.99..., and so on, whatever the
  data's range; with `bottom = 18` they are 18-22.99..., 23-27.99...

- top, bottom:

  Optional limits: values at or above `top` become `top`, and values at
  or below `bottom` become `bottom`, instead of being banded (top and
  bottom coding). HIPAA Safe Harbor, for example, groups ages over 89
  into one category, which `top = 90` gives.

- value:

  The number that stands for a band: `"lower"` (default), its lower
  bound, or `"midpoint"`, its middle, kept within `bottom` and `top`.
  Either is a representative, not a subject's real value.

- digits:

  Number of decimal places to round to, as in
  [`round()`](https://rdrr.io/r/base/Round.html); negative values round
  to tens (`-1`), hundreds (`-2`) and so on.

- min_n:

  Smallest number of subjects a category must hold to be kept.
  Categories held by fewer subjects become `to`. Subjects are counted,
  not rows (see Details).

- to:

  The value small categories become, such as `"OTHER"`. There is no
  default: choose a value that is allowed in the column (for SDTM, a
  term in the variable's controlled terminology). A number for a numeric
  column.

## Value

An object of class `"anon_generalize_rule"`, a function
`function(x, subjects = NULL)` that returns `x` generalized. `subjects`
(used by `anon_fold()` only) gives the subject of each element of `x`.

## Details

**Bands and rounding** apply to numeric vectors (double or integer, also
haven-labelled). The result stays numeric; an integer vector stays
integer, and a rule that would give it a number that isn't whole (a
midpoint of an odd width, say) is an error, as is one too large for an
integer. Missing values, haven user-defined missing values, and values
that carry a haven value label (codes such as 99 for "not done") are
left as they are. Rounding follows
[`round()`](https://rdrr.io/r/base/Round.html).

**Folding** applies to character, factor and numeric vectors (also
haven-labelled). Missing and blank values are never folded or counted,
and `to` itself is never folded (it can stay small: measure again with
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)
and suppress what remains with
[`anon_suppress()`](https://nash-delcamp-slp.github.io/anon/reference/anon_suppress.md)).
A factor keeps its class: the folded levels are removed (with unused
levels held by fewer than `min_n` subjects) and `to` is added as the
last level if it isn't one already. Called on a vector, each element
counts as one subject unless `subjects` gives the subject of each
element. In
[`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md),
subjects are counted by the key, across every dataset where the same
column has the same rule.

haven value labels are kept where they still describe the values: a fold
drops the labels of folded categories, and a band or rounding drops the
label of any code that values moved into or out of.

## See also

[`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md)
to apply rules to datasets,
[`anon_rule()`](https://nash-delcamp-slp.github.io/anon/reference/anon_rule.md)
for test results such as weight in `vs`.

## Examples

``` r
ages <- c(23, 37, 41, 58, 64, 91)
anon_band(10, top = 90)(ages)
#> [1] 20 30 40 50 60 90
anon_band(10, value = "midpoint")(ages)
#> [1] 25 35 45 55 65 95
anon_round(-1)(c(153.2, 171.8))
#> [1] 150 170

race <- c("WHITE", "WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
anon_fold(2, to = "OTHER")(race)
#> [1] "WHITE" "WHITE" "OTHER" "OTHER" "WHITE"
anon_fold(2, to = "OTHER")
#> <anon rule> anon_fold(min_n = 2, to = "OTHER")
```
