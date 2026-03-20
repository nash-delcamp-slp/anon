# Convert numeric values into range categories

Converts numeric values into range categories, either preserving actual
ranges or creating anonymized range labels.

## Usage

``` r
anon_num_range(
  x,
  n_breaks = 5,
  method = c("equal_width", "equal_count"),
  clean_breaks = TRUE,
  scramble = FALSE,
  keep_values = TRUE
)
```

## Arguments

- x:

  A vector to anonymize

- n_breaks:

  Number of range categories to create (default: 5)

- method:

  Method for creating breaks: "equal_width" or "equal_count"

- clean_breaks:

  Logical, whether to use pretty break points

- scramble:

  Logical, whether to randomize the assignment order of generated
  results

- keep_values:

  Logical, whether to keep actual range values or use generic labels

## Examples

``` r
values <- c(150, 165, 180, 175, 160, 190)
anon_num_range(values, n_breaks = 3)
#> [1] "[150,160)" "[160,170)" "[180,190]" "[170,180)" "[160,170)" "[180,190]"
```
