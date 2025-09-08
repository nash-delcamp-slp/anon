#' Generate NLP auto-anonymization options list
#'
#' This function creates a named list of logical values indicating which types of
#' NLP entities should be automatically anonymized when using the core [`anon()`]
#' function. It provides a centralized way to manage automatic NLP anonymization
#' settings when set as the global option `anon.nlp_auto`.
#'
#' @param default Logical indicating the value to use for unset arguments. Default is FALSE
#' @param cardinal Logical indicating whether to automatically anonymize cardinal numbers
#' @param date Logical indicating whether to automatically anonymize dates
#' @param event Logical indicating whether to automatically anonymize events
#' @param fac Logical indicating whether to automatically anonymize facilities
#' @param gpe Logical indicating whether to automatically anonymize geopolitical entities
#' @param language Logical indicating whether to automatically anonymize languages
#' @param law Logical indicating whether to automatically anonymize laws
#' @param loc Logical indicating whether to automatically anonymize locations
#' @param money Logical indicating whether to automatically anonymize money
#' @param norp Logical indicating whether to automatically anonymize nationalities/religious/political groups
#' @param ordinal Logical indicating whether to automatically anonymize ordinal numbers
#' @param org Logical indicating whether to automatically anonymize organizations
#' @param percent Logical indicating whether to automatically anonymize percentages
#' @param person Logical indicating whether to automatically anonymize person names
#' @param product Logical indicating whether to automatically anonymize products
#' @param quantity Logical indicating whether to automatically anonymize quantities
#' @param time Logical indicating whether to automatically anonymize times
#' @param work_of_art Logical indicating whether to automatically anonymize works of art
#' @param propn Logical indicating whether to automatically anonymize proper nouns
#'
#' @return A named list of logical values that can be set with [`options()`].
#'
#' @examples
#' # Get default auto-anonymization options (all TRUE)
#' default_auto <- nlp_auto(default = TRUE)
#'
#' # Customize specific entity types (default, default is FALSE)
#' custom_auto <- nlp_auto(
#'   person = TRUE,
#'   org = TRUE
#' )
#'
#' # Set as global option
#' previous_options <- options(anon.nlp_auto = custom_auto)
#'
#' # Get current global option
#' options("anon.nlp_auto")
#'
#' # Reset the previous global options
#' options(anon.nlp_auto = previous_options$anon.nlp_auto)
#'
#' @export
nlp_auto <- function(
  default = FALSE,
  cardinal = default,
  date = default,
  event = default,
  fac = default,
  gpe = default,
  language = default,
  law = default,
  loc = default,
  money = default,
  norp = default,
  ordinal = default,
  org = default,
  percent = default,
  person = default,
  product = default,
  quantity = default,
  time = default,
  work_of_art = default,
  propn = default
) {
  l <- list(
    cardinal = cardinal,
    date = date,
    event = event,
    fac = fac,
    gpe = gpe,
    language = language,
    law = law,
    loc = loc,
    money = money,
    norp = norp,
    ordinal = ordinal,
    org = org,
    percent = percent,
    person = person,
    product = product,
    quantity = quantity,
    time = time,
    work_of_art = work_of_art,
    propn = propn
  )
  if (!isTRUE(all(is.logical(unlist(l))))) {
    stop("All `anon.nlp_auto` options should be logical.")
  }
  l
}

# Helper function to get enabled entity types for automatic NLP anonymization.
get_enabled_nlp_entities <- function(
  nlp_auto_option = getOption("anon.nlp_auto")
) {
  if (is.null(nlp_auto_option)) {
    return(character(0))
  } else if (is.logical(nlp_auto_option)) {
    nlp_auto_option <- nlp_auto(default = nlp_auto_option)
  }

  if (!is.list(nlp_auto_option)) {
    warning(
      "anon.nlp_auto option should be a list or a logical. Use `nlp_auto()` to generate proper structure."
    )
    return(character(0))
  }

  # Get all entity types that are set to TRUE
  enabled_entities <- names(nlp_auto_option)[sapply(nlp_auto_option, isTRUE)]

  # Convert to uppercase to match expected entity type format
  enabled_entities <- toupper(enabled_entities)

  return(enabled_entities)
}
