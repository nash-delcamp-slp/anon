# Anonymize numeric data while preserving distribution

Anonymizes numeric data while preserving distributional properties using
various transformation methods.

## Usage

``` r
anon_num_preserve_distribution(
  x,
  method = c("rank", "noise", "quantile"),
  noise_sd = NULL,
  quantile_dist_family = c("normal", "uniform", "exponential")
)
```

## Arguments

- x:

  A numeric vector to anonymize

- method:

  Transformation method: "rank", "noise", or "quantile"

- noise_sd:

  Standard deviation for noise method (used when method = "noise")
  (default: NULL resulting in standard deviation being calculated)

- quantile_dist_family:

  Distribution family for quantile method: "normal", "uniform",
  "exponential" (used when method = "quantile")
