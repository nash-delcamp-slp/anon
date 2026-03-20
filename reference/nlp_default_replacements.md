# Generate default replacement options list for NLP entities

This function creates a named list of default replacement values for
different types of NLP entity-based anonymization. It provides a
centralized way to manage default replacements for entities anonymized
in [`anon_nlp_entities()`](anon_nlp_entities.md) and
[`anon_nlp_proper_nouns()`](anon_nlp_entities.md) when set as the global
option `anon.nlp_default_replacements`.

## Usage

``` r
nlp_default_replacements(
  cardinal = "[CARDINAL]",
  date = "[DATE]",
  event = "[EVENT]",
  fac = "[FAC]",
  gpe = "[GPE]",
  language = "[LANGUAGE]",
  law = "[LAW]",
  loc = "[LOC]",
  money = "[MONEY]",
  norp = "[NORP]",
  ordinal = "[ORDINAL]",
  org = "[ORG]",
  percent = "[PERCENT]",
  person = "[PERSON]",
  product = "[PRODUCT]",
  quantity = "[QUANTITY]",
  time = "[TIME]",
  work_of_art = "[WORK_OF_ART]",
  propn = "[PROPN]"
)
```

## Arguments

- cardinal:

  Default replacement for cardinal numbers (default: "\[CARDINAL\]")

- date:

  Default replacement for dates (default: "\[DATE\]")

- event:

  Default replacement for events (default: "\[EVENT\]")

- fac:

  Default replacement for facilities (default: "\[FAC\]")

- gpe:

  Default replacement for geopolitical entities (default: "\[GPE\]")

- language:

  Default replacement for languages (default: "\[LANGUAGE\]")

- law:

  Default replacement for laws (default: "\[LAW\]")

- loc:

  Default replacement for locations (default: "\[LOC\]")

- money:

  Default replacement for money (default: "\[MONEY\]")

- norp:

  Default replacement for nationalities/religious/political groups
  (default: "\[NORP\]")

- ordinal:

  Default replacement for ordinal numbers (default: "\[ORDINAL\]")

- org:

  Default replacement for organizations (default: "\[ORG\]")

- percent:

  Default replacement for percentages (default: "\[PERCENT\]")

- person:

  Default replacement for person names (default: "\[PERSON\]")

- product:

  Default replacement for products (default: "\[PRODUCT\]")

- quantity:

  Default replacement for quantities (default: "\[QUANTITY\]")

- time:

  Default replacement for times (default: "\[TIME\]")

- work_of_art:

  Default replacement for works of art (default: "\[WORK_OF_ART\]")

- propn:

  Default replacement for proper nouns (default: "\[PROPN\]"). This only
  applies to [`anon_nlp_proper_nouns()`](anon_nlp_entities.md).

## Value

A named list of default replacement values that can be set with
[`options()`](https://rdrr.io/r/base/options.html).

## Examples

``` r
# Get default options
default_opts <- nlp_default_replacements()

# Customize specific replacements
custom_opts <- nlp_default_replacements(
  person = "[NAME]",
  org = "[COMPANY]"
)

# Set as global option
previous_options <- options(anon.nlp_default_replacements = custom_opts)

# Get current global option
options("anon.nlp_default_replacements")
#> $anon.nlp_default_replacements
#> $anon.nlp_default_replacements$cardinal
#> [1] "[CARDINAL]"
#> 
#> $anon.nlp_default_replacements$date
#> [1] "[DATE]"
#> 
#> $anon.nlp_default_replacements$event
#> [1] "[EVENT]"
#> 
#> $anon.nlp_default_replacements$fac
#> [1] "[FAC]"
#> 
#> $anon.nlp_default_replacements$gpe
#> [1] "[GPE]"
#> 
#> $anon.nlp_default_replacements$language
#> [1] "[LANGUAGE]"
#> 
#> $anon.nlp_default_replacements$law
#> [1] "[LAW]"
#> 
#> $anon.nlp_default_replacements$loc
#> [1] "[LOC]"
#> 
#> $anon.nlp_default_replacements$money
#> [1] "[MONEY]"
#> 
#> $anon.nlp_default_replacements$norp
#> [1] "[NORP]"
#> 
#> $anon.nlp_default_replacements$ordinal
#> [1] "[ORDINAL]"
#> 
#> $anon.nlp_default_replacements$org
#> [1] "[COMPANY]"
#> 
#> $anon.nlp_default_replacements$percent
#> [1] "[PERCENT]"
#> 
#> $anon.nlp_default_replacements$person
#> [1] "[NAME]"
#> 
#> $anon.nlp_default_replacements$product
#> [1] "[PRODUCT]"
#> 
#> $anon.nlp_default_replacements$quantity
#> [1] "[QUANTITY]"
#> 
#> $anon.nlp_default_replacements$time
#> [1] "[TIME]"
#> 
#> $anon.nlp_default_replacements$work_of_art
#> [1] "[WORK_OF_ART]"
#> 
#> $anon.nlp_default_replacements$propn
#> [1] "[PROPN]"
#> 
#> 

# Reset the previous global options
options(anon.nlp_default_replacements = previous_options$anon.nlp_default_replacements)
```
