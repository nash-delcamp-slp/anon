# Anonymize phone numbers

Anonymizes phone numbers by replacing them with sequential fake numbers.

## Usage

``` r
anon_phone_number(x, start = "555-000")
```

## Arguments

- x:

  A character vector to anonymize

- start:

  Character prefix for generated results (default: "555-000")

## Examples

``` r
phones <- c("Call me at 123-456-7890", "1234567890", "(123) 456-7890")
anon_phone_number(phones)
#> [1] "Call me at 555-000-0001" "555-000-0003"           
#> [3] "555-000-0002"           
```
