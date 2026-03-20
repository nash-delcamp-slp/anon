# Extract Named Entities from Text

Extracts named entities using spaCy's named entity recognition. Entity
types follow the OntoNotes 5.0 annotation scheme as implemented in spaCy
models.

## Usage

``` r
nlp_get_entities(x, entity_types = nlp_entity_sets$all, return_list = FALSE)

nlp_get_dates(x)

nlp_get_dates_and_times(x)

nlp_get_named(x)

nlp_get_numbers(x)

nlp_get_organizations(x)

nlp_get_people(x)

nlp_get_places(x)

nlp_get_proper_nouns(x)
```

## Arguments

- x:

  A character vector, factor, data frame, or list containing text data

- entity_types:

  Character vector of entity types to extract. Entity types are:
  "PERSON", "NORP", "FAC", "ORG", "GPE", "LOC", "PRODUCT", "EVENT",
  "WORK_OF_ART", "LAW", "LANGUAGE", "DATE", "TIME", "PERCENT", "MONEY",
  "QUANTITY", "ORDINAL", "CARDINAL". See
  [nlp_entity_sets](nlp_entity_sets.md) for collections of entity types.

- return_list:

  Logical indicating whether to return a named list with entities
  grouped by type (`TRUE`) or a flat character vector (`FALSE`). Default
  is `FALSE`.

## Value

If `return_list = FALSE` (default): A character vector of unique
extracted entities of the specified type(s). If `return_list = TRUE`: A
named list where names are entity types and values are character vectors
of unique entities for each type.

## References

[spaCy Named Entity
Recognition](https://spacy.io/usage/linguistic-features#named-entities)
[spaCy Model Documentation](https://spacy.io/models) [OntoNotes 5.0
Annotation
Scheme](https://catalog.ldc.upenn.edu/docs/LDC2013T19/OntoNotes-Release-5.0.pdf)

## See also

The cleanNLP package provides access to spaCy and other NLP backends.

[`cnlp_annotate`](https://rdrr.io/pkg/cleanNLP/man/cnlp_annotate.html)
for spaCy-based text processing.

The entity package on GitHub (<https://github.com/trinker/entity>)
provided inspiration for these functions.

## Examples

``` r
text <- c("John Smith works at Microsoft in Seattle.",
          "The deal was worth $1.2 million in 2023.",
          "He was the first employee to make 100% of his 3rd quarter targets.")

nlp_get_entities(text)
#>  [1] "2023"            "his 3rd quarter" "Seattle"         "$1.2 million"   
#>  [5] "first"           "Microsoft"       "100%"            "John Smith"     
#>  [9] "John"            "Smith"          
nlp_get_people(text)
#> [1] "John Smith"
nlp_get_organizations(text)
#> [1] "Microsoft"
nlp_get_places(text)
#> [1] "Seattle"
nlp_get_numbers(text)
#> [1] "$1.2 million" "first"        "100%"        
nlp_get_entities(text, "MONEY")
#> [1] "$1.2 million"
nlp_get_dates(text)
#> [1] "2023"            "his 3rd quarter"
nlp_get_proper_nouns(text)
#> [1] "John"      "Smith"     "Microsoft" "Seattle"  

# Get entities grouped by type
nlp_get_entities(text, c("PERSON", "ORG", "MONEY"), return_list = TRUE)
#> $PERSON
#> [1] "John Smith"
#> 
#> $ORG
#> [1] "Microsoft"
#> 
#> $MONEY
#> [1] "$1.2 million"
#> 
```
