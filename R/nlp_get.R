#' Extract Named Entities from Text
#'
#' These functions extract different types of named entities from text using 
#' natural language processing. Each function specializes in extracting a 
#' specific type of entity.
#'
#' @param x A character vector
#'
#' @return A character vector of unique extracted entities of the specified type.
#'   Returns an empty character vector if no entities are found or if processing fails.
#'
#' @details
#' The functions use OpenNLP's named entity recognition capabilities to identify:
#' \itemize{
#'   \item \code{nlp_get_dates()}: Date expressions
#'   \item \code{nlp_get_locations()}: Geographic locations and place names
#'   \item \code{nlp_get_money()}: Monetary amounts and currency expressions
#'   \item \code{nlp_get_organizations()}: Organization and company names
#'   \item \code{nlp_get_percentages()}: Percentage expressions
#'   \item \code{nlp_get_people()}: Person names
#' }
#'
#' The functions handle missing values and empty input gracefully. Text that is
#' too short or contains no alphabetic characters is skipped.
#'
#' @examples
#' text <- c("John Smith works at Microsoft in Seattle.", 
#'           "The deal was worth $1.2 million in 2023.")
#'           
#' nlp_get_people(text)
#' nlp_get_organizations(text)
#' nlp_get_locations(text)
#' nlp_get_money(text)
#' nlp_get_dates(text)
#'
#' @seealso 
#' \code{\link[NLP]{annotate}} for text annotation functionality,
#' \code{\link[openNLP]{Maxent_Entity_Annotator}} for named entity recognition,
#' \code{\link[openNLP]{Maxent_Sent_Token_Annotator}} and 
#' \code{\link[openNLP]{Maxent_Word_Token_Annotator}} for text tokenization.
#' 
#' The \pkg{openNLPmodels.en} package provides the Apache OpenNLP models for 
#' the English language that are required for these functions to work.
#' 
#' The \pkg{entity} package on GitHub (\url{https://github.com/trinker/entity}) 
#' provided inspiration for these functions.
#'
#' @name nlp_get
#' @export
nlp_get_dates <- function(x) {
  nlp_get_kind(x, "date")
}

#' @rdname nlp_get
#' @export
nlp_get_locations <- function(x) {
  nlp_get_kind(x, "location")
}

#' @rdname nlp_get
#' @export
nlp_get_money <- function(x) {
  nlp_get_kind(x, "money")
}

#' @rdname nlp_get
#' @export
nlp_get_organizations <- function(x) {
  nlp_get_kind(x, "organization")
}

#' @rdname nlp_get
#' @export
nlp_get_percentages <- function(x) {
  nlp_get_kind(x, "percentage")
}

#' @rdname nlp_get
#' @export
nlp_get_people <- function(x) {
  nlp_get_kind(x, "person")
}

# generalized function with NLP workflow
nlp_get_kind <- function(
  x,
  kind = c("date", "location", "money", "organization", "percentage", "person")
) {
  kind <- match.arg(kind)
  text <- x

  # Handle missing values and empty input
  if (length(text) == 0 || all(is.na(text))) {
    return(character(0))
  }

  # Remove NA values for processing
  text_clean <- text[!is.na(text)]

  if (length(text_clean) == 0) {
    return(character(0))
  }

  # Combine all text into single string for NLP processing
  combined_text <- paste(text_clean, collapse = " ")

  # Skip if text is too short
  if (nchar(combined_text) < 2) {
    return(character(0))
  }

  # Create NLP pipeline
  tryCatch(
    {
      # Convert to NLP String object
      text_string <- NLP::as.String(combined_text)

      ann <- NLP::annotate(
        text_string,
        list(
          openNLP::Maxent_Sent_Token_Annotator(),
          openNLP::Maxent_Word_Token_Annotator()
        )
      )

      # Entity annotation for the specified kind
      entity_ann <- NLP::annotate(
        text_string,
        openNLP::Maxent_Entity_Annotator(kind = kind),
        ann
      )

      # Extract entities
      if (length(entity_ann) == 0) {
        return(character(0))
      }

      # Filter for entities of the specified kind only
      target_entities <- entity_ann[
        entity_ann$type == "entity" &
          purrr::map_lgl(
            entity_ann$features,
            ~ if (!is.null(.x$kind)) .x$kind == kind else FALSE
          )
      ]

      if (length(target_entities) == 0) {
        return(character(0))
      }

      # Extract the actual text of entities
      entities <- character(length(target_entities))
      for (i in seq_along(target_entities)) {
        start_pos <- target_entities[i]$start
        end_pos <- target_entities[i]$end
        entities[i] <- substring(text_string, start_pos, end_pos)
      }

      # Clean up the extracted entities
      entities <- trimws(entities)
      entities <- entities[entities != ""]
      entities <- unique(entities)
      entities <- escape_regex_chars(entities)

      return(entities)
    },
    error = function(e) {
      # If NLP processing fails, return empty character vector
      warning(paste("NLP processing failed:", e$message), call. = FALSE)
      return(character(0))
    }
  )
}

# Helper function to escape special regex characters
escape_regex_chars <- function(x) {
  # Escape special regex characters
  # These are: \ . ^ $ * + ? { } [ ] | ( )
  for (chr in c("\\", ".", "^", "$", "*", "+", "?", "{", "}", "[", "]", "|", "(", ")")) {
     x <- gsub(chr, paste0("\\", chr), x, fixed = TRUE)
  }
  x
}