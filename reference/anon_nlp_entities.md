# Anonymize Named Entities in Text Using NLP

These functions use natural language processing to identify and
anonymize different types of named entities from text. Each function
combines NLP entity extraction with pattern expansion and anonymization.

## Usage

``` r
anon_nlp_entities(x, entity_types = nlp_entity_sets$all, ...)

anon_nlp_proper_nouns(x, ...)

anon_nlp_dates(x, ...)

anon_nlp_dates_and_times(x, ...)

anon_nlp_named(x, ...)

anon_nlp_numbers(x, ...)

anon_nlp_organizations(x, ...)

anon_nlp_people(x, ...)

anon_nlp_places(x, ...)
```

## Arguments

- x:

  The object to anonymize. Can be a character vector, factor, data
  frame, or list.

- entity_types:

  Character vector of entity types to extract. Entity types are:
  "PERSON", "NORP", "FAC", "ORG", "GPE", "LOC", "PRODUCT", "EVENT",
  "WORK_OF_ART", "LAW", "LANGUAGE", "DATE", "TIME", "PERCENT", "MONEY",
  "QUANTITY", "ORDINAL", "CARDINAL". See
  [nlp_entity_sets](nlp_entity_sets.md) for collections of entity types.

- ...:

  Additional arguments passed to [`anon`](anon.md)

## Value

An anonymized object of class `anon_context` with named entities
replaced according to the anonymization workflow.

## Details

These functions:

1.  Use NLP to extract potentially sensitive information of the
    specified type

2.  Extend patterns using [`more_patterns`](more_patterns.md) for
    comprehensive matching

3.  Apply [`anon`](anon.md) to anonymize the identified patterns

Available entity types:

- `anon_nlp_entities()`: Anonymize any specified entity types

- `anon_nlp_proper_nouns()`: Anonymize proper nouns using POS tagging

- `anon_nlp_dates()`: Anonymize dates

- `anon_nlp_dates_and_times()`: Anonymize dates and times

- `anon_nlp_people()`: Anonymize person names

- `anon_nlp_organizations()`: Anonymize organization names

- `anon_nlp_places()`: Anonymize place names and locations

- `anon_nlp_numbers()`: Anonymize numeric entities

- `anon_nlp_named()`: Anonymize named entities

## Global Options

The `anon.nlp_default_replacements` global option affects the default
replacement values used by these functions when no `default_replacement`
argument is explicitly provided. See
[`nlp_default_replacements()`](nlp_default_replacements.md) to generate
the content for the option.

## Examples

``` r
text <- c("John Smith works at Microsoft in Seattle.",
          "The deal was worth $1.2 million in 2023.",
          "He was the first employee to make 100% of his 3rd quarter targets.")

# Anonymize all entities
anon_nlp_entities(text)
#> [1] "[PERSON] works at [ORG] in [GPE]."                                 
#> [2] "The deal was worth [MONEY] in [DATE]."                             
#> [3] "He was the [ORDINAL] employee to make [PERCENT] of [DATE] targets."

# Anonymize person names
anon_nlp_people(text)
#> [1] "[PERSON] works at Microsoft in Seattle."                           
#> [2] "The deal was worth $1.2 million in 2023."                          
#> [3] "He was the first employee to make 100% of his 3rd quarter targets."

# Anonymize organizations with custom replacement
anon_nlp_organizations(text, default_replacement = "[COMPANY]")
#> [1] "John Smith works at [COMPANY] in Seattle."                         
#> [2] "The deal was worth $1.2 million in 2023."                          
#> [3] "He was the first employee to make 100% of his 3rd quarter targets."

# Anonymize specific entity types
anon_nlp_entities(text, entity_types = c("PERSON", "ORG"))
#> [1] "[PERSON] works at [ORG] in Seattle."                               
#> [2] "The deal was worth $1.2 million in 2023."                          
#> [3] "He was the first employee to make 100% of his 3rd quarter targets."
```
