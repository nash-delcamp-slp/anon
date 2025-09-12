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


# Helper function to extract NLP patterns for automatic anonymization
extract_nlp_patterns <- function(x, entity_types) {
  if (length(entity_types) == 0) {
    return(list())
  }

  # Only process character vectors and factors directly
  # For complex objects, the entities will be extracted during recursive processing
  text_to_process <- character(0)

  if (is.character(x)) {
    text_to_process <- x
  } else if (is.factor(x)) {
    text_to_process <- levels(x)
  } else if (is.data.frame(x)) {
    # Extract text from character and factor columns
    char_cols <- sapply(x, function(col) is.character(col) || is.factor(col))
    if (any(char_cols)) {
      text_data <- x[char_cols]
      text_to_process <- unlist(
        lapply(text_data, function(col) {
          if (is.factor(col)) {
            return(levels(col))
          } else {
            return(col)
          }
        }),
        use.names = FALSE
      )
    }
  } else if (is.list(x)) {
    # Recursively extract text from list elements
    text_to_process <- unlist(
      lapply(x, function(elem) {
        if (is.character(elem)) {
          return(elem)
        } else if (is.factor(elem)) {
          return(levels(elem))
        }
        return(character(0))
      }),
      use.names = FALSE
    )
  }

  if (length(text_to_process) == 0) {
    return(list())
  }

  # Get entities by type
  pattern_list <- list()

  # Handle NLP entities other than proper nouns
  other_entities <- setdiff(entity_types, "PROPN")
  if (length(other_entities) > 0) {
    all_entities <- nlp_get_entities(
      text_to_process,
      entity_types = other_entities,
      return_list = TRUE
    )

    for (entity_type in other_entities) {
      entities <- all_entities[[entity_type]]
      if (length(entities) > 0) {
        replacement <- get_nlp_default_replacement(entity_type)
        expanded_patterns <- more_patterns(entities, original = FALSE)
        pattern_list[[length(pattern_list) + 1]] <- expanded_patterns
        names(pattern_list)[length(pattern_list)] <- replacement
      }
    }
  }

  # Handle proper nouns separately
  if ("PROPN" %in% entity_types) {
    propn_entities <- nlp_get_proper_nouns(text_to_process)
    if (length(propn_entities) > 0) {
      replacement <- get_nlp_default_replacement("PROPN")
      expanded_patterns <- more_patterns(propn_entities, original = FALSE)
      pattern_list[[length(pattern_list) + 1]] <- expanded_patterns
      names(pattern_list)[length(pattern_list)] <- replacement
    }
  }

  # Remove entities with less than 2 characters
  if (length(pattern_list) > 0) {
    pattern_list <- lapply(pattern_list, function(patterns) {
      patterns[nchar(patterns) >= 2]
    })

    # Remove empty pattern lists
    pattern_list <- pattern_list[lengths(pattern_list) > 0]
  }

  return(pattern_list)
}
