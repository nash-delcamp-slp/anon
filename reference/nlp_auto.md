# Generate NLP auto-redaction options list

This function creates a named list of logical values indicating which
types of NLP entities should be automatically redacted when using the
core
[`anon()`](https://nash-delcamp-slp.github.io/anon/reference/anon.md)
function. It provides a centralized way to manage automatic NLP
redaction settings when set as the global option `anon.nlp_auto`.

## Usage

``` r
nlp_auto(
  default = FALSE,
  cardinal = default,
  date = default,
  event = default,
  fac = default,
  gpe = default,
  language = default,
  law = default,
  loc = default,
  money = default,
  norp = default,
  ordinal = default,
  org = default,
  percent = default,
  person = default,
  product = default,
  quantity = default,
  time = default,
  work_of_art = default,
  propn = default
)
```

## Arguments

- default:

  Logical indicating the value to use for unset arguments. Default is
  FALSE

- cardinal:

  Logical indicating whether to automatically redact cardinal numbers

- date:

  Logical indicating whether to automatically redact dates

- event:

  Logical indicating whether to automatically redact events

- fac:

  Logical indicating whether to automatically redact facilities

- gpe:

  Logical indicating whether to automatically redact geopolitical
  entities

- language:

  Logical indicating whether to automatically redact languages

- law:

  Logical indicating whether to automatically redact laws

- loc:

  Logical indicating whether to automatically redact locations

- money:

  Logical indicating whether to automatically redact money

- norp:

  Logical indicating whether to automatically redact
  nationalities/religious/political groups

- ordinal:

  Logical indicating whether to automatically redact ordinal numbers

- org:

  Logical indicating whether to automatically redact organizations

- percent:

  Logical indicating whether to automatically redact percentages

- person:

  Logical indicating whether to automatically redact person names

- product:

  Logical indicating whether to automatically redact products

- quantity:

  Logical indicating whether to automatically redact quantities

- time:

  Logical indicating whether to automatically redact times

- work_of_art:

  Logical indicating whether to automatically redact works of art

- propn:

  Logical indicating whether to automatically redact proper nouns

## Value

A named list of logical values that can be set with
[`options()`](https://rdrr.io/r/base/options.html).

## Examples

``` r
# Get default auto-redaction options (all TRUE)
default_auto <- nlp_auto(default = TRUE)

# Customize specific entity types (default, default is FALSE)
custom_auto <- nlp_auto(
  person = TRUE,
  org = TRUE
)

# Set as global option
previous_options <- options(anon.nlp_auto = custom_auto)

# Get current global option
options("anon.nlp_auto")
#> $anon.nlp_auto
#> $anon.nlp_auto$cardinal
#> [1] FALSE
#> 
#> $anon.nlp_auto$date
#> [1] FALSE
#> 
#> $anon.nlp_auto$event
#> [1] FALSE
#> 
#> $anon.nlp_auto$fac
#> [1] FALSE
#> 
#> $anon.nlp_auto$gpe
#> [1] FALSE
#> 
#> $anon.nlp_auto$language
#> [1] FALSE
#> 
#> $anon.nlp_auto$law
#> [1] FALSE
#> 
#> $anon.nlp_auto$loc
#> [1] FALSE
#> 
#> $anon.nlp_auto$money
#> [1] FALSE
#> 
#> $anon.nlp_auto$norp
#> [1] FALSE
#> 
#> $anon.nlp_auto$ordinal
#> [1] FALSE
#> 
#> $anon.nlp_auto$org
#> [1] TRUE
#> 
#> $anon.nlp_auto$percent
#> [1] FALSE
#> 
#> $anon.nlp_auto$person
#> [1] TRUE
#> 
#> $anon.nlp_auto$product
#> [1] FALSE
#> 
#> $anon.nlp_auto$quantity
#> [1] FALSE
#> 
#> $anon.nlp_auto$time
#> [1] FALSE
#> 
#> $anon.nlp_auto$work_of_art
#> [1] FALSE
#> 
#> $anon.nlp_auto$propn
#> [1] FALSE
#> 
#> 

# Reset the previous global options
options(anon.nlp_auto = previous_options$anon.nlp_auto)
```
