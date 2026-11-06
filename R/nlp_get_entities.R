#' Extract Named Entities from Text
#'
#' @description
#' Extracts named entities using spaCy's named entity recognition.
#' Entity types follow the OntoNotes 5.0 annotation scheme as implemented
#' in spaCy models.
#'
#' @param x A character vector, factor, data frame, or list containing text data
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
nlp_get_entities <- function(
  x,
  entity_types = nlp_entity_sets$all,
  return_list = FALSE
) {
  all_entities <- stats::setNames(
    rep(list(character(0)), length(entity_types)),
    entity_types
  )

  text_clean <- nlp_candidate_text(collect_text(x))

  if (length(text_clean) > 0) {
    found <- tryCatch(
      spacy_extract(text_clean),
      error = function(e) {
        stop(
          paste("cleanNLP initialization or processing failed:", conditionMessage(e)),
          call. = FALSE
        )
      }
    )

    for (entity_type in setdiff(entity_types, "PROPN")) {
      all_entities[[entity_type]] <- unique(
        found$entity_text[found$entity_label == entity_type]
      )
    }
    if ("PROPN" %in% entity_types) {
      all_entities[["PROPN"]] <- unique(found$propn)
    }
  }

  if (return_list) {
    return(all_entities)
  }
  unique(unlist(all_entities, use.names = FALSE)) %||% character(0)
}

# Reduce text to the distinct strings worth sending to spaCy. Strings with no
# letters or digits, and hexadecimal identifiers (8+ characters of 0-9/a-f
# containing both a digit and a letter, e.g. hashes or specimen IDs), cannot
# hold a named entity; spaCy otherwise tags many of them as DATE or CARDINAL.
nlp_candidate_text <- function(text) {
  text <- unique(trimws(text))
  text <- text[!is.na(text) & nchar(text) >= 2]
  no_alnum <- !grepl("[[:alnum:]]", text)
  hex_id <- grepl("^[0-9a-fA-F]{8,}$", text) &
    grepl("[0-9]", text) &
    grepl("[a-fA-F]", text)
  text[!no_alnum & !hex_id]
}

nlp_cache <- new.env(parent = emptyenv())

# Load the spaCy pipeline once per session. cleanNLP selects and validates the
# Python environment and model; a separate pipeline without the dependency
# parser and lemmatizer is then loaded for batched entity and POS extraction.
spacy_pipeline <- function(model_name = "en_core_web_sm") {
  if (!requireNamespace("cleanNLP", quietly = TRUE)) {
    stop("cleanNLP package is required for entity extraction", call. = FALSE)
  }
  if (identical(nlp_cache$model_name, model_name) && !is.null(nlp_cache$extract)) {
    return(nlp_cache$extract)
  }

  cleanNLP::cnlp_init_spacy(model_name = model_name)

  py <- reticulate::py_run_string(
    paste(
      "def make_extract(model_name):",
      "    import spacy",
      "    nlp = spacy.load(model_name, disable=['parser', 'lemmatizer'])",
      "    def extract(texts):",
      "        labels, ents, propn = [], [], []",
      "        for doc in nlp.pipe(texts, batch_size=256):",
      "            for e in doc.ents:",
      "                labels.append(e.label_)",
      "                ents.append(e.text.strip())",
      "            for t in doc:",
      "                if t.pos_ == 'PROPN' or t.tag_ in ('NNP', 'NNPS'):",
      "                    propn.append(t.text.strip())",
      "        return {'entity_label': labels, 'entity_text': ents, 'propn': propn}",
      "    return extract",
      sep = "\n"
    ),
    local = TRUE,
    convert = TRUE
  )

  nlp_cache$extract <- py$make_extract(model_name)
  nlp_cache$model_name <- model_name
  nlp_cache$extract
}

spacy_extract <- function(text) {
  extract <- spacy_pipeline()
  out <- extract(as.list(text))
  lapply(out, function(v) {
    v <- as.character(unlist(v))
    v[!is.na(v) & v != ""]
  })
}

#' @rdname nlp_get_entities
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
nlp_get_proper_nouns <- function(x) {
  nlp_get_entities(x, "PROPN")
}
