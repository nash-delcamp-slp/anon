# Shuffle, add noise to, or resample numeric values

Replaces numeric values while keeping their overall distribution, using
one of three methods:

- `"rank"` (default) reassigns the original values to other records
  (data swapping). Every original value, including outliers, is still
  present.

- `"noise"` adds random noise to each value (randomization).

- `"quantile"` replaces values with draws from a fitted theoretical
  distribution at shuffled quantiles.

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
