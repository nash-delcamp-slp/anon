# Generalize the results of a test

Pairs an
[`anon_quasi_values()`](https://nash-delcamp-slp.github.io/anon/reference/anon_quasi_values.md)
specification with a rule, for
[`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md):
the rule applies to every row of each test in the specification, and the
test's other result columns are kept consistent with it.

## Usage

``` r
anon_rule(spec, rule)
```

## Arguments

- spec:

  An
  [`anon_quasi_values()`](https://nash-delcamp-slp.github.io/anon/reference/anon_quasi_values.md)
  specification, such as the one given to
  [`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md).

- rule:

  A rule made by
  [`anon_band()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md),
  [`anon_round()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md)
  or
  [`anon_fold()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md).

## Value

An object of class `"anon_rule_values"`.

## Details

The rule applies to the `result_col` of **every row** of each test, not
only the row
[`anon_risk()`](https://nash-delcamp-slp.github.io/anon/reference/anon_risk.md)
measures: any row identifies a subject as well as the baseline one.
`filter` is not used here, for that reason.

When `result_col` is a numeric SDTM result (`--STRESN`) and the rule is
a band or rounding, the text results of the same rows follow it:

- `--STRESC` is rewritten from the generalized `--STRESN`;

- `--ORRES` is rewritten from the generalized `--STRESN`: as it is where
  the units are the same (`--ORRESU` equals `--STRESU`), and otherwise
  times the row's own conversion factor (original `--ORRES` / original
  `--STRESN`), so it is never finer than `--STRESN` and unit conversion
  checks still pass. Without a factor (`--STRESN` missing or 0, or no
  unit columns and different values), it is blanked;

each in its own form (plain, with thousands separators such as
`"1,234"`, or in scientific notation such as `"2.5E2"`) and with as many
decimals as it had (in the same units, more where the generalized number
needs them, such as a midpoint of `62.5` for `"64"`). Other results,
such as `"<5"` or `"ND"`, are left unchanged and counted in the log,
with a warning where the row has a numeric result.

For ADaM, with `result_col = "AVAL"`, `AVALC` is rewritten from the
generalized `AVAL`, `BASE` is generalized by the same rule (it is the
baseline `AVAL`) with `BASEC` rewritten from it, and `CHG`
(`AVAL - BASE`), `PCHG` (`100 * CHG / BASE`) and `R2BASE`
(`AVAL / BASE`) are recomputed from the generalized values. Where the
generalized `BASE` is 0 (a band or rounding can make it so), `PCHG` and
`R2BASE` become missing. Other derived columns (such as `BASETYPE`
analyses, `CRIT` flags or `AVALCAT`) are not handled: generalize or
remove them. Other result columns are generalized alone.

With
[`anon_fold()`](https://nash-delcamp-slp.github.io/anon/reference/generalize_rules.md),
`result_col` must be text; on the rows folded, the other text results
(`--ORRES` or `--STRESC`; `AVALC`, `BASEC`) become `to` too, and numeric
ones become missing. Only the categories found in the test's rows count
(a factor's levels are shared by every test).

## See also

[`anon_generalize()`](https://nash-delcamp-slp.github.io/anon/reference/anon_generalize.md)

## Examples

``` r
anon_rule(
  anon_quasi_values("vs", "VSTESTCD", c("HEIGHT", "WEIGHT"), "VSSTRESN"),
  anon_round(0)
)
#> <anon rule> anon_round(digits = 0) on `vs`: VSSTRESN where VSTESTCD is "HEIGHT", "WEIGHT" (every row)
```
