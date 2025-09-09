#' Named Entity Type Collections
#'
#' A list of predefined collections of named entity types for use with
#' spaCy's named entity recognition. These collections group entity types
#' by semantic categories, making it easier to extract related types of entities.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{all}{All available entity types: "CARDINAL", "DATE", "EVENT", "FAC", "GPE", "LANGUAGE", "LAW", "LOC", "MONEY", "NORP", "ORDINAL", "ORG", "PERCENT", "PERSON", "PRODUCT", "QUANTITY", "TIME", "WORK_OF_ART", "PROPN"}
#'   \item{cultural_artifacts}{Language, laws, products, and works of art: "LANGUAGE", "LAW", "PRODUCT", "WORK_OF_ART"}
#'   \item{date_and_time}{"DATE", "TIME"}
#'   \item{named}{Named entities: "EVENT", "FAC", "GPE", "LOC", "NORP", "ORG", "PERSON", "PRODUCT", "PROPN"}
#'   \item{numbers}{Numeric and quantitative entities: "CARDINAL", "MONEY", "ORDINAL", "PERCENT", "QUANTITY"}
#'   \item{organizations}{Organizational entities: "NORP", "ORG"}
#'   \item{places}{Location and place entities: "FAC", "GPE", "LOC"}
#' }
#'
#' @details
#' The entity types follow the OntoNotes 5.0 annotation scheme:
#' \itemize{
#'   \item \strong{CARDINAL}: Numerals that do not fall under another type
#'   \item \strong{DATE}: Absolute or relative dates or periods
#'   \item \strong{EVENT}: Named hurricanes, battles, wars, sports events, etc.
#'   \item \strong{FAC}: Buildings, airports, highways, bridges, etc.
#'   \item \strong{GPE}: Countries, cities, states (geopolitical entities)
#'   \item \strong{LANGUAGE}: Any named language
#'   \item \strong{LAW}: Named documents made into laws
#'   \item \strong{LOC}: Non-GPE locations, mountain ranges, bodies of water
#'   \item \strong{MONEY}: Monetary values, including unit
#'   \item \strong{NORP}: Nationalities or religious or political groups
#'   \item \strong{ORDINAL}: "first", "second", etc.
#'   \item \strong{ORG}: Companies, agencies, institutions, etc.
#'   \item \strong{PERCENT}: Percentage, including "%"
#'   \item \strong{PERSON}: People, including fictional
#'   \item \strong{PRODUCT}: Objects, vehicles, foods, etc. (not services)
#'   \item \strong{QUANTITY}: Measurements, as of weight or distance
#'   \item \strong{TIME}: Times smaller than a day
#'   \item \strong{WORK_OF_ART}: Titles of books, songs, etc.
#'   \item \strong{PROPN}: Proper nouns
#' }
#'
#' @examples
#' # View all available entity sets
#' names(nlp_entity_sets)
#'
#' # View the contents of specific sets
#' nlp_entity_sets$places
#' nlp_entity_sets$numbers
#'
#' # Use in entity extraction
#' text <- "Apple Inc. was founded in Cupertino on April 1, 1976."
#' nlp_get_entities(text, nlp_entity_sets$organizations)
#' nlp_get_entities(text, nlp_entity_sets$places)
#' nlp_get_entities(text, nlp_entity_sets$date_and_time)
#'
#' @references
#' OntoNotes 5.0 Annotation Scheme: https://catalog.ldc.upenn.edu/docs/LDC2013T19/OntoNotes-Release-5.0.pdf
#' spaCy Named Entity Recognition: https://spacy.io/usage/linguistic-features#named-entities
#'
#' @export
nlp_entity_sets <- list(
  all = c(
    "CARDINAL",
    "DATE",
    "EVENT",
    "FAC",
    "GPE",
    "LANGUAGE",
    "LAW",
    "LOC",
    "MONEY",
    "NORP",
    "ORDINAL",
    "ORG",
    "PERCENT",
    "PERSON",
    "PRODUCT",
    "QUANTITY",
    "TIME",
    "WORK_OF_ART",
    "PROPN"
  ),

  cultural_artifacts = c(
    "LANGUAGE",
    "LAW",
    "PRODUCT",
    "WORK_OF_ART"
  ),

  date_and_time = c(
    "DATE",
    "TIME"
  ),

  named = c(
    "EVENT",
    "FAC",
    "GPE",
    "LOC",
    "NORP",
    "ORG",
    "PERSON",
    "PRODUCT",
    "PROPN"
  ),

  numbers = c(
    "CARDINAL",
    "MONEY",
    "ORDINAL",
    "PERCENT",
    "QUANTITY"
  ),

  organizations = c(
    "NORP",
    "ORG"
  ),

  places = c(
    "FAC",
    "GPE",
    "LOC"
  )
)
