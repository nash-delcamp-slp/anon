# Create expanded pattern vector for text matching

Accepts a character vector of patterns and returns an expanded vector
containing:

1.  Original items

2.  Original items with special characters escaped

3.  Patterns with spaces replaced with a pattern of any number of spaces
    and any one character between words.

4.  Individual words from each item, split by spaces and punctuation,
    not including stop words.

## Usage

``` r
more_patterns(
  patterns,
  original = TRUE,
  escape_regex = TRUE,
  spaces_to_flexible = TRUE,
  individual = TRUE
)
```

## Arguments

- patterns:

  Character vector of patterns to expand

- original:

  Logical. Whether to include original patterns (default: TRUE)

- escape_regex:

  Logical. Whether to include original patterns with special characters
  escaped (default: TRUE)

- spaces_to_flexible:

  Logical. Whether to include space-flexible patterns (default: TRUE)

- individual:

  Logical. Whether to include individual words (default: TRUE)

## Value

Character vector with selected pattern expansions

## Details

The intention of the order is to replace phrases with as few
replacements as possible while taking additional efforts to anonymize
all sensitive information.

## Examples

``` r
people <- c("John Smith", "Mary Jane Watson")
more_patterns(people)
#> [1] "John Smith"                       "Mary Jane Watson"                
#> [3] "John\\s*.\\s*Smith"               "Mary\\s*.\\s*Jane\\s*.\\s*Watson"
#> [5] "John"                             "Smith"                           
#> [7] "Mary"                             "Jane"                            
#> [9] "Watson"                          
more_patterns(people, individual = FALSE)
#> [1] "John Smith"                       "Mary Jane Watson"                
#> [3] "John\\s*.\\s*Smith"               "Mary\\s*.\\s*Jane\\s*.\\s*Watson"
```
