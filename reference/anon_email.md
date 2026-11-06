# Replace email addresses with pseudonymous addresses

Replaces each distinct email address with a sequential fake address.

## Usage

``` r
anon_email(x, start = "user", domain = "domain.com")
```

## Arguments

- x:

  A character vector to anonymize

- start:

  Character prefix for generated results (default: "user")

- domain:

  Domain for generated emails (default: "domain.com")

## Examples

``` r
emails <- c("john@company.com", "not_an_email", "jane at work dot org")
anon_email(emails)
#> [1] "user001@domain.com" "not_an_email"       "user002@domain.com"
```
