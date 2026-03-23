# Combine anon_context objects

This method allows combining multiple anonymized objects created by
[`anon()`](anon.md) or [`anon_data_summary()`](anon_data_summary.md)
using [`c()`](https://rdrr.io/r/base/c.html). The result maintains the
anonymized content and provides a print method suitable for LLM context.
An additional header is included in printed output for named elements.

## Usage

``` r
# S3 method for class 'anon_context'
c(...)
```

## Arguments

- ...:

  anon_context objects to combine

## Value

An anon_context_collection object containing all input objects

## Examples

``` r
df <- data.frame(name = c("John", "Jane"), age = c(25, 30))
anon_df <- anon(df, pattern_list = c("John", "Jane"))
summary_obj <- anon_data_summary(list(df = df))
combined <- c(anon_df, summary_obj)
print(combined)
#> === ANONYMIZED DATA CONTEXT ===
#> 
#>         name age
#> 1 [REDACTED]  25
#> 2 [REDACTED]  30
#> 
#> Environment Data Summary
#> ========================
#> 
#>   total_objects data_frames other_objects total_memory
#> 1             1           1             0          992
#> 
#> Data Frames:
#> ------------
#>   name       type n_rows n_cols memory_size
#> 1   df data.frame      2      2   992 bytes
#> 
#> 
#> Variable Details (df):
#> 
#> -------------------- 
#>   variable data_type n_distinct n_missing n_total pct_missing label
#> 1     name character          2         0       2           0  <NA>
#> 2      age   numeric          2         0       2           0  <NA>
#> 
#> 
#> 
```
