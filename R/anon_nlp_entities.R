#' Anonymize Named Entities in Text Using NLP
#'
#' These functions use natural language processing to identify and anonymize
#' different types of named entities from text. Each function combines NLP
#' entity extraction with pattern expansion and anonymization.
#'
#' @param x A character vector to anonymize
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
#'   \item Extend patterns using \code{\link{more_patterns}} for comprehensive matching
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
  if (!is.character(x)) {
    stop("x must be a character vector")
  }
  entity_types <- unique(entity_types)

  # Get default replacement(s) from arguments or options
  args <- list(...)
  nlp_replacements <- vector("list", length = length(entity_types))
  names(nlp_replacements) <- entity_types
  if ("default_replacement" %in% names(args)) {
    nlp_replacements <- rep(
      as.list(args$default_replacement),
      times = length(nlp_replacements)
    )
    names(nlp_replacements) <- entity_types
  } else {
    for (entity in names(nlp_replacements)) {
      nlp_replacements[[entity]] <- get_nlp_default_replacement(entity)
    }
  }

  if ("PROPN" %in% entity_types) {
    propn_entities <- list(PROPN = nlp_get_proper_nouns(x))
  } else {
    propn_entities <- NULL
  }

  if (length(entity_types) > 0) {
    # Collect all entities at once, grouped by entity_type.
    all_entities <- nlp_get_entities(
      x,
      entity_types = setdiff(entity_types, "PROPN"),
      return_list = TRUE
    )
  } else {
    all_entities <- NULL
  }

  all_entities <- c(all_entities, propn_entities)[entity_types]

  # Build pattern_list for anon() where names are replacement values and values are patterns.
  pattern_list <- list()

  for (entity in entity_types) {
    i <- length(pattern_list) + 1
    entities <- all_entities[[entity]]
    replacement <- nlp_replacements[[entity]]

    if (length(entities) > 0) {
      if (is.character(replacement)) {
        if (length(replacement) > 1) {
          # Sample the replacement values as replacements for each individual entity
          entity_replacements <- lapply(entities, more_patterns)
          names(entity_replacements) <- sample(
            replacement,
            size = length(entity_replacements),
            replace = length(entities) > length(replacement)
          )
          pattern_list <- c(pattern_list, entity_replacements)
        } else {
          expanded_patterns <- more_patterns(entities)
          pattern_list[[i]] <- expanded_patterns
          names(pattern_list)[[i]] <- replacement
        }
      } else {
        stop("Issue with replacement value for ", entity)
      }
    }
  }

  # Remove the default_replacement from args since we're handling it via pattern_list
  args$default_replacement <- NULL

  # Call anon once with the complete pattern_list
  if (length(pattern_list) > 0) {
    x <- do.call(
      anon,
      c(list(x = x, pattern_list = pattern_list), args)
    )
  }

  x
}

#' @rdname anon_nlp_entities
#' @export
anon_nlp_proper_nouns <- function(x, ...) {
  anon_nlp_entities(x, "PROPN")
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
