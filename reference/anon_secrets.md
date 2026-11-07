# Read the secrets kept by a de-identification step

Returns the information needed to link de-identified data back to the
source, such as the mapping from keys to pseudonyms or each subject's
date offset. It exists only when a step was run with
`keep_secrets = TRUE`. Keep it separate from anything you share.

## Usage

``` r
anon_secrets(x)
```

## Arguments

- x:

  An object of class `"anon_deid_step"`.

## Value

A named list of tibbles, e.g. `subject_ids` or `date_offsets`.

## Details

Each table has a `key` column with the subject key as it was when that
step ran: the original key, or the pseudonym if
[`anon_subject_ids()`](https://nash-delcamp-slp.github.io/anon/reference/anon_subject_ids.md)
ran first.

## Examples

``` r
data <- data.frame(USUBJID = c("a", "b"))
result <- anon_subject_ids(data, keep_secrets = TRUE)
anon_secrets(result)
#> $subject_ids
#> # A tibble: 2 × 2
#>   key   pseudonym
#>   <chr> <chr>    
#> 1 a     SUBJ-1   
#> 2 b     SUBJ-2   
#> 
```
