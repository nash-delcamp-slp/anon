
<!-- README.md is generated from README.Rmd. Please edit that file -->

# anon

<!-- badges: start -->

<!-- badges: end -->

The goal of anon is to anonymize sensitive information in R objects
including data frames, variable labels, lists, and
character/factor/numeric vectors.

## Installation

You can install the development version of anon from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nash-delcamp-slp/anon")
```

Load it with:

``` r
library(anon)
```

## Examples

The below examples demonstrate some of the anonymization functions that
operate on vectors.

``` r
anon_id_chr_sequence(c("John", "Paul", "John", "Keith"))
#> [1] "ID 1" "ID 2" "ID 1" "ID 3"

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data("starwars", package = "dplyr")
glimpse(starwars)
#> Rows: 87
#> Columns: 14
#> $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
#> $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
#> $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
#> $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
#> $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
#> $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
#> $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
#> $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
#> $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
#> $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
#> $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
#> $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
#> $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
#> $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…
anon_id_num_sequence(starwars$name) |> 
  head()
#> [1] 1 2 3 4 5 6
anon_num_preserve_distribution(starwars$height) |> 
  head()
#> [1] 183.0 174.0 138.0  66.0 189.5 177.5
anon_num_range(starwars$mass, n_breaks = 10) |> 
  head()
#> [1] "[0,100)"   "[0,100)"   "[0,100)"   "[100,200)" "[0,100)"   "[100,200)"
```

This example demonstrates how to anonymize a data frame with `anon()`.

``` r
anon_starwars <- starwars |> 
  anon(
    pattern_list = list(
      DARK = c("empire", "imperials?", "sith"),
      LIGHT = c("jedi", "rebels?")
    ),
    df_variable_names = list(
      name = anon_id_chr_sequence,
      homeworld = ~ anon_id_chr_sequence(.x, start = "Planet ")
    ),
    df_classes = list(
      integer = ~ anon_num_range(.x, n_breaks = 10),
      numeric = anon_num_preserve_distribution
    )
  )
glimpse(anon_starwars)
#> Rows: 87
#> Columns: 14
#> $ name       <chr> "ID 01", "ID 02", "ID 03", "ID 04", "ID 05", "ID 06", "ID 0…
#> $ height     <chr> "[160,180)", "[160,180)", "[80,100)", "[200,220)", "[140,16…
#> $ mass       <dbl> 88, 66, 57, 58, 136, 15, 66, 57, 80, 88, 80, NA, 77, 69, 90…
#> $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
#> $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
#> $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
#> $ birth_year <dbl> 35.5, 200.0, 31.5, 43.0, 35.5, 73.0, 600.0, NA, 54.0, 22.0,…
#> $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
#> $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
#> $ homeworld  <chr> "Planet 01", "Planet 01", "Planet 02", "Planet 01", "Planet…
#> $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
#> $ films      <list> <"A New Hope", "The DARK Strikes Back", "Return of the LIG…
#> $ vehicles   <list> <"Snowspeeder", "DARK Speeder Bike">, <>, <>, <>, "DARK Sp…
#> $ starships  <list> <"X-wing", "DARK shuttle">, <>, <>, "TIE Advanced x1", <>,…
```

When `check_approximate = TRUE`, warnings will be reported when an
approximate match is detected.

``` r
starwars |> 
  anon(list(
    "blonde", "bleu", "imperials"
  )) |> 
  glimpse()
#> Warning: Potential approximate match: 'blond' is similar to pattern 'blonde'
#> • Potential approximate match: 'blue' is similar to pattern 'bleu'
#> • Potential approximate match: 'Imperial Speeder Bike' is similar to pattern 'imperials'
#> • Potential approximate match: 'Imperial shuttle' is similar to pattern 'imperials'
#> Rows: 87
#> Columns: 14
#> $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
#> $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
#> $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
#> $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
#> $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
#> $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
#> $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
#> $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
#> $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
#> $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
#> $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
#> $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
#> $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
#> $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…
```

This example demonstrates the `anon_data_summary()` function which
summarizes the data in an environment.

``` r
anon_data_summary(
  pattern_list = list(
    DARK = c("empire", "imperials?", "sith"),
    LIGHT = c("jedi", "rebels?")
  )
)
#> Environment Data Summary
#> ========================
#> 
#>   total_objects data_frames other_objects total_memory
#> 1             1           1             0        57440
#> 
#> Data Frames:
#> ------------
#>       name       type n_rows n_cols memory_size
#> 1 starwars data.frame     87     14     56.1 Kb
#> 
#> Variable Details (starwars):
#> -------------------------- 
#>      variable data_type n_distinct n_missing n_total pct_missing label
#> 1        name character         87         0      87        0.00  <NA>
#> 2      height   integer         45         6      87        6.90  <NA>
#> 3        mass   numeric         38        28      87       32.18  <NA>
#> 4  hair_color character         11         5      87        5.75  <NA>
#> 5  skin_color character         31         0      87        0.00  <NA>
#> 6   eye_color character         15         0      87        0.00  <NA>
#> 7  birth_year   numeric         36        44      87       50.57  <NA>
#> 8         sex character          4         4      87        4.60  <NA>
#> 9      gender character          2         4      87        4.60  <NA>
#> 10  homeworld character         48        10      87       11.49  <NA>
#> 11    species character         37         4      87        4.60  <NA>
#> 12      films      list         24         0      87        0.00  <NA>
#> 13   vehicles      list         11         0      87        0.00  <NA>
#> 14  starships      list         16         0      87        0.00  <NA>
```

## Default Options

Default values can be provided to `options()` to automatically include
patterns and variable rules to use in `anon()` and
`anon_data_summary()`.

``` r
options(
  anon.pattern_list = list(
    DARK = c("empire", "imperials?", "sith"),
    LIGHT = c("jedi", "rebels?")
  ),
  anon.df_variable_names = list(
    name = anon_id_chr_sequence,
    homeworld = ~ anon_id_chr_sequence(.x, start = "Planet ")
  ),
  anon.df_classes = list(
    integer = ~ anon_num_range(.x, n_breaks = 10),
    numeric = anon_num_preserve_distribution
  )
)
```

``` r
c(
  starwars = anon(starwars),
  anon_data_summary()
)
#> === ANONYMIZED DATA CONTEXT ===
#> 
#> ---  starwars  ---
#> # A tibble: 87 × 14
#>    name  height     mass hair_color skin_color eye_color birth_year sex   gender
#>  * <chr> <chr>     <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
#>  1 ID 01 [160,180)   113 blond      fair       blue            53.5 male  mascu…
#>  2 ID 02 [160,180)    48 <NA>       gold       yellow         600   none  mascu…
#>  3 ID 03 [80,100)     82 <NA>       white, bl… red             52   none  mascu…
#>  4 ID 04 [200,220)    76 none       white      yellow          43   male  mascu…
#>  5 ID 05 [140,160)    40 brown      light      brown           53.5 fema… femin…
#>  6 ID 06 [160,180)    80 brown, gr… light      blue            47   male  mascu…
#>  7 ID 07 [160,180)    48 brown      light      blue           112   fema… femin…
#>  8 ID 08 [80,100)     82 <NA>       white, red red             NA   none  mascu…
#>  9 ID 09 [180,200)    85 black      light      brown           52   male  mascu…
#> 10 ID 10 [180,200)   113 auburn, w… fair       blue-gray       19   male  mascu…
#> # ℹ 77 more rows
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>
#> 
#> Environment Data Summary
#> ========================
#> 
#>   total_objects data_frames other_objects total_memory
#> 1             1           1             0        57440
#> 
#> Data Frames:
#> ------------
#>       name       type n_rows n_cols memory_size
#> 1 starwars data.frame     87     14     56.1 Kb
#> 
#> Variable Details (starwars):
#> -------------------------- 
#>      variable data_type n_distinct n_missing n_total pct_missing label
#> 1        name character         87         0      87        0.00  <NA>
#> 2      height   integer         45         6      87        6.90  <NA>
#> 3        mass   numeric         38        28      87       32.18  <NA>
#> 4  hair_color character         11         5      87        5.75  <NA>
#> 5  skin_color character         31         0      87        0.00  <NA>
#> 6   eye_color character         15         0      87        0.00  <NA>
#> 7  birth_year   numeric         36        44      87       50.57  <NA>
#> 8         sex character          4         4      87        4.60  <NA>
#> 9      gender character          2         4      87        4.60  <NA>
#> 10  homeworld character         48        10      87       11.49  <NA>
#> 11    species character         37         4      87        4.60  <NA>
#> 12      films      list         24         0      87        0.00  <NA>
#> 13   vehicles      list         11         0      87        0.00  <NA>
#> 14  starships      list         16         0      87        0.00  <NA>
```
