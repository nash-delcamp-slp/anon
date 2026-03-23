# Print method for anonymized environment summary

Displays an anonymized environment summary in a structured, readable
format. The output includes overall statistics, data frame structural
information, variable details, optional example payloads, and
information about other objects (if present).

## Usage

``` r
# S3 method for class 'anon_data_summary'
print(x, ...)
```

## Arguments

- x:

  An object of class `"anon_data_summary"` created by
  [`anon_data_summary()`](anon_data_summary.md)

- ...:

  Additional arguments passed to print methods (currently unused)

## Value

Invisibly returns the input object `x`

## Details

The print method displays information in the following order:

1.  **Environment Data Summary**: Overall statistics including total
    objects, data frame count, other object count, and total memory
    usage

2.  **Data Frames**: Structural information for each data frame

3.  **Variable Details**: Detailed information about variables within
    data frames

4.  **Examples**: Optional sample rows or keyed scenarios when
    configured

5.  **Other Objects**: Information about non-data frame objects

All displayed content respects the anonymization patterns applied during
the creation of the summary object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create and print an anonymized summary
summary_result <- anon_data_summary(
  list(data = mtcars),
  pattern_list = list("CAR" = "Mazda|Merc")
)
print(summary_result)
} # }
```
