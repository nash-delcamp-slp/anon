#' Anonymize Named Entities in Text Using NLP
#'
#' These functions use natural language processing to identify and anonymize
#' different types of named entities from text. Each function combines NLP
#' entity extraction with pattern expansion and anonymization.
#'
#' @inheritParams anon
#' @inheritParams nlp_get_entities
#' @param ... Additional arguments passed to \code{\link{anon}}
#'
#' @return An anonymized object of class \code{anon_context} with named entities
#'   replaced according to the anonymization workflow.
#'
#' @details
#' These functions:
#' \enumerate{
#'   \item Use NLP to extract potentially sensitive information of the specified type
#'     from the distinct text values. Values without letters or digits and
#'     hexadecimal identifiers are not sent to the NLP model.
#'   \item Turn each entity into a pattern that matches only as a whole value (not
#'     preceded or followed by a letter, a digit, or a decimal separator next to
#'     a digit). For name-like entity types (\code{"PERSON"}, \code{"ORG"},
#'     \code{"NORP"}, \code{"FAC"}, \code{"GPE"}, \code{"LOC"}, \code{"PRODUCT"},
#'     \code{"EVENT"}, \code{"WORK_OF_ART"}, \code{"PROPN"}), each word of the
#'     entity is also matched as a whole word, excluding stopwords and
#'     number-only words. A name part that is also a common word (e.g.
#'     "Grace") is then redacted wherever that word appears. For \code{"DATE"} and
#'     \code{"TIME"}, ISO 8601 dates (\code{YYYY-MM}, \code{YYYY-MM-DD}, with optional
#'     time) and times (\code{HH:MM}, \code{HH:MM:SS}) are also matched by pattern.
#'   \item Apply \code{\link{anon}} to anonymize the identified patterns
#' }
#'
#' Available entity types:
#' \itemize{
#'   \item \code{anon_nlp_entities()}: Anonymize any specified entity types
#'   \item \code{anon_nlp_proper_nouns()}: Anonymize proper nouns using POS tagging
#'   \item \code{anon_nlp_dates()}: Anonymize dates
#'   \item \code{anon_nlp_dates_and_times()}: Anonymize dates and times
#'   \item \code{anon_nlp_people()}: Anonymize person names
#'   \item \code{anon_nlp_organizations()}: Anonymize organization names
#'   \item \code{anon_nlp_places()}: Anonymize place names and locations
#'   \item \code{anon_nlp_numbers()}: Anonymize numeric entities
#'   \item \code{anon_nlp_named()}: Anonymize named entities
#' }
#'
#' @section Global Options:
#' The `anon.nlp_default_replacements` global option affects the default replacement
#' values used by these functions when no `default_replacement` argument is explicitly
#' provided. See [`nlp_default_replacements()`] to generate the content for the option.
#'
#' @examples
#' text <- c("John Smith works at Microsoft in Seattle.",
#'           "The deal was worth $1.2 million in 2023.",
#'           "He was the first employee to make 100% of his 3rd quarter targets.")
#'
#' # Anonymize all entities
#' anon_nlp_entities(text)
#'
#' # Anonymize person names
#' anon_nlp_people(text)
#'
#' # Anonymize organizations with custom replacement
#' anon_nlp_organizations(text, default_replacement = "[COMPANY]")
#'
#' # Anonymize specific entity types
#' anon_nlp_entities(text, entity_types = c("PERSON", "ORG"))
#'
#' @name anon_nlp_entities
#' @export
anon_nlp_entities <- function(x, entity_types = nlp_entity_sets$all, ...) {
  entity_types <- unique(entity_types)

  # Get default replacement(s) from arguments or options
  args <- list(...)

  pattern_list <- extract_nlp_patterns(x, entity_types)

  if (!is.null(args$default_replacement)) {
    pattern_list <- list(unlist(pattern_list))
    names(pattern_list) <- args$default_replacement
  }

  # Remove the default_replacement from args since we're handling it via pattern_list
  args$default_replacement <- NULL

  # Call anon once with the complete pattern_list
  if (length(pattern_list) > 0) {
    x <- do.call(
      anon,
      c(list(x = x, pattern_list = pattern_list, nlp_auto = FALSE), args)
    )
  }

  new_anon_context(x)
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_proper_nouns <- function(x, ...) {
  anon_nlp_entities(x, "PROPN", ...)
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_dates <- function(x, ...) {
  anon_nlp_entities(x, "DATE", ...)
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_dates_and_times <- function(x, ...) {
  anon_nlp_entities(x, nlp_entity_sets$date_and_time, ...)
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_named <- function(x, ...) {
  anon_nlp_entities(x, nlp_entity_sets$named, ...)
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_numbers <- function(x, ...) {
  anon_nlp_entities(x, nlp_entity_sets$numbers, ...)
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_organizations <- function(x, ...) {
  anon_nlp_entities(x, nlp_entity_sets$organizations, ...)
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_people <- function(x, ...) {
  anon_nlp_entities(x, "PERSON", ...)
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_places <- function(x, ...) {
  anon_nlp_entities(x, nlp_entity_sets$places, ...)
}

# Helper function to extract NLP patterns for automatic anonymization.
#
# Each detected entity becomes one literal pattern that matches only as a whole
# value (see whole_value_prefix), so an entity such as "2021" does not match
# inside "ab2021cd" or a hexadecimal identifier. For name-like entity types,
# the entity's individual words are added as whole-word patterns too, so a
# surname found once in "John Smith" is also redacted where it appears alone.
# DATE and TIME also get deterministic ISO 8601 patterns, because spaCy does
# not reliably tag ISO dates in structured data. DATE and TIME groups come
# first so a full date-time is replaced before any shorter numeric entity can
# match part of it.
extract_nlp_patterns <- function(x, entity_types) {
  if (length(entity_types) == 0) {
    return(list())
  }

  entity_types <- unique(entity_types)
  date_time_first <- intersect(c("DATE", "TIME"), entity_types)
  entity_types <- c(date_time_first, setdiff(entity_types, date_time_first))

  text_to_process <- collect_text(x)

  if (length(text_to_process) == 0) {
    return(list())
  }

  all_entities <- nlp_get_entities(
    text_to_process,
    entity_types = entity_types,
    return_list = TRUE
  )

  pattern_list <- list()

  for (entity_type in entity_types) {
    entities <- all_entities[[entity_type]]
    entities <- entities[!is.na(entities) & nchar(entities) >= 2]

    words <- if (entity_type %in% name_entity_types) {
      entity_words(entities)
    } else {
      character(0)
    }

    patterns <- c(
      iso8601_patterns(entity_type),
      whole_value_patterns(c(entities, words))
    )

    if (length(patterns) > 0) {
      pattern_list[[length(pattern_list) + 1]] <- patterns
      names(pattern_list)[length(pattern_list)] <-
        get_nlp_default_replacement(entity_type)
    }
  }

  pattern_list
}

# Entity types whose individual words are also redacted.
name_entity_types <- c(
  "PERSON", "ORG", "NORP", "FAC", "GPE", "LOC", "PRODUCT", "EVENT",
  "WORK_OF_ART", "PROPN"
)

# Individual words of multi-word entities, split on whitespace and
# punctuation. Stopwords, number-only pieces and single characters are dropped.
entity_words <- function(entities) {
  words <- unlist(strsplit(entities, "[[:space:][:punct:]]+"), use.names = FALSE)
  words <- words[nchar(words) >= 2 & !grepl("^[0-9]+$", words)]
  words <- words[!tolower(words) %in% stopwords::stopwords()]
  unique(words)
}

# Literal, case-insensitive patterns that match an entity only as a whole
# value. Runs of whitespace inside an entity match any run of whitespace.
# Longer entities come first so they win over entities they contain.
whole_value_patterns <- function(entities) {
  if (length(entities) == 0) {
    return(character(0))
  }
  entities <- unique(entities)
  entities <- entities[order(nchar(entities), decreasing = TRUE)]
  escaped <- gsub("\\s+", "\\\\s+", escape_regex_chars(entities))
  paste0(whole_value_prefix, escaped, whole_value_suffix)
}

# A whole value is not preceded or followed by a letter or digit, nor by a
# decimal separator next to a digit (so "03" does not match inside "42.03").
whole_value_prefix <- "(?<![[:alnum:]])(?<![0-9][.,])"
whole_value_suffix <- "(?![[:alnum:]])(?![.,][0-9])"

# ISO 8601 date and time patterns used alongside spaCy for DATE and TIME.
# Dates need at least year and month (YYYY-MM); a bare year is not matched.
# Times follow the whole-value boundary rule, except that the time part of a
# date-time ("2021-03-14T08:30") matches after its "T" when DATE is not also
# being redacted.
iso8601_patterns <- function(entity_type) {
  switch(
    entity_type,
    DATE = paste0(
      "(?<![[:alnum:]])",
      "[0-9]{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?",
      "(?:T(?:[01][0-9]|2[0-3])(?::[0-5][0-9](?::[0-5][0-9](?:\\.[0-9]+)?)?)?",
      "(?:Z|[+-][0-9]{2}(?::?[0-9]{2})?)?)?",
      "(?![[:alnum:]])"
    ),
    TIME = paste0(
      "(?:(?<=[0-9]T)|(?<![[:alnum:]:])(?<![0-9][.,]))",
      "(?:[01]?[0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9](?:\\.[0-9]+)?)?",
      "(?:Z|[+-][0-9]{2}(?::?[0-9]{2})?)?",
      "(?![[:alnum:]:])(?![.,][0-9])"
    ),
    character(0)
  )
}
