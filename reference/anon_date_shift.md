# Shift dates to a new time period

Shifts dates to a new time period while preserving relative
relationships between dates. Supports Date and POSIXct vectors and ISO
8601 character strings such as SDTM `--DTC` variables.

## Usage

``` r
anon_date_shift(
  x,
  center_date = Sys.Date(),
  scramble = FALSE,
  offset = NULL,
  partial = c("shift", "remove")
)
```

## Arguments

- x:

  A Date, POSIXct or ISO 8601 character vector to anonymize

- center_date:

  New center point for the date range (default: current date). Ignored
  when `offset` is given.

- scramble:

  Logical, whether to randomize the assignment order of generated
  results. Not available with `offset` or character input.

- offset:

  Optional number of days (a whole number, or a `difftime`) to add to
  every value. Use the same `offset` for all columns that must stay
  aligned.

- partial:

  How to handle partial ISO 8601 dates in character input: `"shift"`
  (default) moves the middle of the month or year by the offset and
  keeps the original precision; `"remove"` replaces them with `""`.

## Value

A vector of the same class as `x` (character for character input).

## Details

By default, dates are moved so that their mean falls on `center_date`.
Each call computes its own shift, so separate columns or datasets move
by different amounts. To keep intervals between columns and datasets
(for example dose and sample times), pass the same `offset` to every
call.

Character input is read as ISO 8601 and returned as character in the
same format and precision: a full date (`2014-01-02`) is shifted and any
time part after `T` is kept as is. Partial dates (`2014-03`, `2014`) are
handled by `partial`. Empty strings and `NA` are returned unchanged, and
other values are removed with a warning.

## Examples

``` r
dates <- as.Date(c("2023-01-15", "2023-02-20", "2023-03-10"))
anon_date_shift(dates, center_date = "2024-06-01")
#> [1] "2024-05-02" "2024-06-07" "2024-06-25"

# One offset for every column keeps intervals between them
offset <- -400
anon_date_shift(c("2023-01-15T08:00", "2023-02-20", "2023-03"), offset = offset)
#> [1] "2021-12-11T08:00" "2022-01-16"       "2022-02"         
anon_date_shift(as.Date("2023-01-16"), offset = offset)
#> [1] "2021-12-12"
```
