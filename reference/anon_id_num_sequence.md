# Pseudonymize identifiers as sequential numeric IDs

Without `scramble = TRUE`, IDs are numbered in order of first
appearance, so if `x` is sorted by something sensitive (for example the
original ID, enrolment date or name), the new IDs keep that order.

## Usage

``` r
anon_id_num_sequence(x, scramble = FALSE)
```

## Arguments

- x:

  A vector to anonymize

- scramble:

  Logical, whether to randomize the assignment order of generated
  results
