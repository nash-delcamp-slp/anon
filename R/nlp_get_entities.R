#' Extract Named Entities from Text
#'
#' @description
#' Extracts named entities using spaCy's named entity recognition.
#' Entity types follow the OntoNotes 5.0 annotation scheme as implemented
#' in spaCy models.
#'
#' @param x A character vector
#' @param entity_types Character vector of entity types to extract.
#'   Entity types are: "PERSON", "NORP", "FAC", "ORG", "GPE", "LOC", "PRODUCT",
#'   "EVENT", "WORK_OF_ART", "LAW", "LANGUAGE", "DATE", "TIME", "PERCENT",
#'   "MONEY", "QUANTITY", "ORDINAL", "CARDINAL".
#'   See [nlp_entity_sets] for collections of entity types.
#' @param return_list Logical indicating whether to return a named list with
#'   entities grouped by type (`TRUE`) or a flat character vector (`FALSE`).
#'   Default is `FALSE`.
#'
#' @return If `return_list = FALSE` (default): A character vector of unique
#'   extracted entities of the specified type(s).
#'   If `return_list = TRUE`: A named list where names are entity types and
#'   values are character vectors of unique entities for each type.
#'
#' @format NULL
#'
#' @references
#' [spaCy Named Entity Recognition](https://spacy.io/usage/linguistic-features#named-entities)
#' [spaCy Model Documentation](https://spacy.io/models)
#' [OntoNotes 5.0 Annotation Scheme](https://catalog.ldc.upenn.edu/docs/LDC2013T19/OntoNotes-Release-5.0.pdf)
#'
#' @examples
#' text <- c("John Smith works at Microsoft in Seattle.",
#'           "The deal was worth $1.2 million in 2023.",
#'           "He was the first employee to make 100% of his 3rd quarter targets.")
#'
#' nlp_get_entities(text)
#' nlp_get_people(text)
#' nlp_get_organizations(text)
#' nlp_get_places(text)
#' nlp_get_numbers(text)
#' nlp_get_entities(text, "MONEY")
#' nlp_get_dates(text)
#' nlp_get_proper_nouns(text)
#'
#' # Get entities grouped by type
#' nlp_get_entities(text, c("PERSON", "ORG", "MONEY"), return_list = TRUE)
#'
#' @seealso
#' The \pkg{cleanNLP} package provides access to spaCy and other NLP backends.
#'
#' \code{\link[cleanNLP]{cnlp_annotate}} for spaCy-based text processing.
#'
#' The \pkg{entity} package on GitHub (\url{https://github.com/trinker/entity})
#' provided inspiration for these functions.
#'
#' @name nlp_get_entities
#' @export
nlp_get_dates <- function(x) {
  nlp_get_entities(x, "DATE")
}

#' @rdname nlp_get_entities
#' @export
nlp_get_dates_and_times <- function(x) {
  nlp_get_entities(x, nlp_entity_sets$date_and_time)
}

#' @rdname nlp_get_entities
#' @export
nlp_get_named <- function(x) {
  nlp_get_entities(x, nlp_entity_sets$named)
}

#' @rdname nlp_get_entities
#' @export
nlp_get_numbers <- function(x) {
  nlp_get_entities(x, nlp_entity_sets$numbers)
}

#' @rdname nlp_get_entities
#' @export
nlp_get_organizations <- function(x) {
  nlp_get_entities(x, nlp_entity_sets$organizations)
}

#' @rdname nlp_get_entities
#' @export
nlp_get_people <- function(x) {
  nlp_get_entities(x, "PERSON")
}

#' @rdname nlp_get_entities
#' @export
nlp_get_places <- function(x) {
  nlp_get_entities(x, nlp_entity_sets$places)
}

#' @rdname nlp_get_entities
#' @export
nlp_get_entities <- function(
  x,
  entity_types = nlp_entity_sets$all,
  return_list = FALSE
) {
  # Handle missing values and empty input
  if (length(x) == 0 || all(is.na(x))) {
    if (return_list) {
      result <- vector("list", length(entity_types))
      names(result) <- entity_types
      return(result)
    }
    return(character(0))
  }

  # Remove NA values for processing
  text_clean <- x[!is.na(x)]

  if (length(text_clean) == 0) {
    if (return_list) {
      result <- vector("list", length(entity_types))
      names(result) <- entity_types
      return(result)
    }
    return(character(0))
  }

  all_entities <- vector("list", length(entity_types))
  names(all_entities) <- entity_types

  # Check if cleanNLP is available and spacy backend is initialized
  if (!requireNamespace("cleanNLP", quietly = TRUE)) {
    warning(
      "cleanNLP package is required for entity extraction",
      call. = FALSE
    )
    if (return_list) {
      return(all_entities)
    }
    return(character(0))
  }

  tryCatch(
    {
      # Initialize spacy backend if not already done
      cleanNLP::cnlp_init_spacy()

      # Process each text element
      for (i in seq_along(text_clean)) {
        single_text <- trimws(text_clean[i])

        # Skip if text is too short
        if (nchar(single_text) < 2) {
          next
        }

        tryCatch(
          {
            # Annotate the text with cleanNLP
            doc <- cleanNLP::cnlp_annotate(single_text)

            # Extract entities
            entities <- doc$entity

            if (nrow(entities) > 0) {
              # Group entities by type
              for (entity_type in entity_types) {
                target_entities <- entities[
                  entities$entity_type == entity_type,
                ]

                if (nrow(target_entities) > 0) {
                  # Get the actual entity text
                  entity_text <- target_entities$entity
                  entity_text <- trimws(entity_text)
                  entity_text <- entity_text[
                    entity_text != "" & !is.na(entity_text)
                  ]
                  all_entities[[entity_type]] <- c(
                    all_entities[[entity_type]],
                    entity_text
                  )
                }
              }
            }
          },
          error = function(e) {
            warning(
              paste("cleanNLP processing failed for text", i, ":", e$message),
              call. = FALSE
            )
          }
        )
      }

      # Clean up results - get unique entities for each type
      for (entity_type in entity_types) {
        if (length(all_entities[[entity_type]]) > 0) {
          all_entities[[entity_type]] <- unique(all_entities[[entity_type]])
        } else {
          all_entities[[entity_type]] <- character(0)
        }
      }

      if (return_list) {
        return(all_entities)
      } else {
        # Return flat vector
        flat_entities <- unlist(all_entities, use.names = FALSE)
        if (length(flat_entities) == 0) {
          return(character(0))
        }
        return(unique(flat_entities))
      }
    },
    error = function(e) {
      warning(
        paste("cleanNLP initialization or processing failed:", e$message),
        call. = FALSE
      )
      if (return_list) {
        return(all_entities)
      }
      return(character(0))
    }
  )
}

#' @rdname nlp_get_entities
#' @export
nlp_get_proper_nouns <- function(x) {
  # Handle missing values and empty input
  if (length(x) == 0 || all(is.na(x))) {
    return(character(0))
  }

  # Remove NA values for processing
  text_clean <- x[!is.na(x)]

  if (length(text_clean) == 0) {
    return(character(0))
  }

  all_proper_nouns <- character(0)

  # Check if cleanNLP is available and spacy backend is initialized
  if (!requireNamespace("cleanNLP", quietly = TRUE)) {
    warning(
      "cleanNLP package is required for proper noun extraction",
      call. = FALSE
    )
    return(character(0))
  }

  tryCatch(
    {
      # Initialize spacy backend if not already done
      # This will check if spacy is available and set it up
      cleanNLP::cnlp_init_spacy()

      # Process each text element
      for (i in seq_along(text_clean)) {
        single_text <- trimws(text_clean[i])

        # Skip if text is too short
        if (nchar(single_text) < 2) {
          next
        }

        tryCatch(
          {
            # Annotate the text with cleanNLP
            doc <- cleanNLP::cnlp_annotate(single_text)

            # Extract tokens with POS tags
            tokens <- doc$token

            # Filter for proper nouns (PROPN in spacy, NNP/NNPS in other models)
            proper_nouns <- tokens[tokens$upos %in% c("PROPN", "NNP", "NNPS"), ]

            if (nrow(proper_nouns) > 0) {
              # Get the actual words
              words <- proper_nouns$token
              words <- trimws(words)
              words <- words[words != "" & !is.na(words)]
              all_proper_nouns <- c(all_proper_nouns, words)
            }
          },
          error = function(e) {
            warning(
              paste("cleanNLP processing failed for text", i, ":", e$message),
              call. = FALSE
            )
          }
        )
      }

      # Clean up results
      if (length(all_proper_nouns) == 0) {
        return(character(0))
      }

      # Return unique proper nouns
      unique(all_proper_nouns)
    },
    error = function(e) {
      warning(
        paste("cleanNLP initialization or processing failed:", e$message),
        call. = FALSE
      )
      return(character(0))
    }
  )
}
