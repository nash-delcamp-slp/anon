# Convert unique identifiers to sequential character IDs

Convert unique identifiers to sequential character IDs

## Usage

``` r
anon_id_chr_sequence(
  x,
  scramble = FALSE,
  start = "ID ",
  padding = TRUE,
  padding_chr = "0"
)
```

## Arguments

- x:

  A vector to anonymize

- scramble:

  Logical, whether to randomize the assignment order of generated
  results

- start:

  Character prefix for generated results (default: "ID ")

- padding:

  Logical, whether to zero-pad numbers for consistent width

- padding_chr:

  Character used for padding (default: "0")

## Examples

``` r
ids <- c("A123", "B456", "A123", "C789")
anon_id_chr_sequence(ids)
#> [1] "ID 1" "ID 2" "ID 1" "ID 3"
```
