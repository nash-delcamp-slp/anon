# Shift dates to a new time period

Shifts dates to a new time period while preserving relative
relationships between dates. Supports Date and POSIXct objects.

## Usage

``` r
anon_date_shift(x, center_date = Sys.Date(), scramble = FALSE)
```

## Arguments

- x:

  A Date or POSIXct vector to anonymize

- center_date:

  New center point for the date range (default: current date)

- scramble:

  Logical, whether to randomize the assignment order of generated
  results

## Examples

``` r
dates <- as.Date(c("2023-01-15", "2023-02-20", "2023-03-10"))
anon_date_shift(dates, center_date = "2024-06-01")
#> [1] "2024-05-02" "2024-06-07" "2024-06-25"
```
