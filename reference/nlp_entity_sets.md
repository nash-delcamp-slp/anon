# Named Entity Type Collections

A list of predefined collections of named entity types for use with
spaCy's named entity recognition. These collections group entity types
by semantic categories, making it easier to extract related types of
entities.

## Usage

``` r
nlp_entity_sets
```

## Format

A named list with the following elements:

- all:

  All available entity types: "CARDINAL", "DATE", "EVENT", "FAC", "GPE",
  "LANGUAGE", "LAW", "LOC", "MONEY", "NORP", "ORDINAL", "ORG",
  "PERCENT", "PERSON", "PRODUCT", "QUANTITY", "TIME", "WORK_OF_ART",
  "PROPN"

- cultural_artifacts:

  Language, laws, products, and works of art: "LANGUAGE", "LAW",
  "PRODUCT", "WORK_OF_ART"

- date_and_time:

  "DATE", "TIME"

- named:

  Named entities: "EVENT", "FAC", "GPE", "LOC", "NORP", "ORG", "PERSON",
  "PRODUCT", "PROPN"

- numbers:

  Numeric and quantitative entities: "CARDINAL", "MONEY", "ORDINAL",
  "PERCENT", "QUANTITY"

- organizations:

  Organizational entities: "NORP", "ORG"

- places:

  Location and place entities: "FAC", "GPE", "LOC"

## Details

The entity types follow the OntoNotes 5.0 annotation scheme:

- **CARDINAL**: Numerals that do not fall under another type

- **DATE**: Absolute or relative dates or periods

- **EVENT**: Named hurricanes, battles, wars, sports events, etc.

- **FAC**: Buildings, airports, highways, bridges, etc.

- **GPE**: Countries, cities, states (geopolitical entities)

- **LANGUAGE**: Any named language

- **LAW**: Named documents made into laws

- **LOC**: Non-GPE locations, mountain ranges, bodies of water

- **MONEY**: Monetary values, including unit

- **NORP**: Nationalities or religious or political groups

- **ORDINAL**: "first", "second", etc.

- **ORG**: Companies, agencies, institutions, etc.

- **PERCENT**: Percentage, including "%"

- **PERSON**: People, including fictional

- **PRODUCT**: Objects, vehicles, foods, etc. (not services)

- **QUANTITY**: Measurements, as of weight or distance

- **TIME**: Times smaller than a day

- **WORK_OF_ART**: Titles of books, songs, etc.

- **PROPN**: Proper nouns

## References

OntoNotes 5.0 Annotation Scheme:
https://catalog.ldc.upenn.edu/docs/LDC2013T19/OntoNotes-Release-5.0.pdf
spaCy Named Entity Recognition:
https://spacy.io/usage/linguistic-features#named-entities

## Examples

``` r
# View all available entity sets
names(nlp_entity_sets)
#> [1] "all"                "cultural_artifacts" "date_and_time"     
#> [4] "named"              "numbers"            "organizations"     
#> [7] "places"            

# View the contents of specific sets
nlp_entity_sets$places
#> [1] "FAC" "GPE" "LOC"
nlp_entity_sets$numbers
#> [1] "CARDINAL" "MONEY"    "ORDINAL"  "PERCENT"  "QUANTITY"

# Use in entity extraction
text <- "Apple Inc. was founded in Cupertino on April 1, 1976."
nlp_get_entities(text, nlp_entity_sets$organizations)
#> [1] "Apple Inc."
nlp_get_entities(text, nlp_entity_sets$places)
#> [1] "Cupertino"
nlp_get_entities(text, nlp_entity_sets$date_and_time)
#> [1] "April 1, 1976"
```
