#' Generate default replacement options list for NLP entities
#'
#' This function creates a named list of default replacement values for different
#' types of NLP entity-based anonymization. It provides a centralized way to manage
#' default replacements for entities anonymized in [`anon_nlp_entities()`] and
#' [`anon_nlp_proper_nouns()`] when set as the global option `anon.nlp_default_replacements`.
#'
#' @param cardinal Default replacement for cardinal numbers (default: "\[CARDINAL\]")
#' @param date Default replacement for dates (default: "\[DATE\]")
#' @param event Default replacement for events (default: "\[EVENT\]")
#' @param fac Default replacement for facilities (default: "\[FAC\]")
#' @param gpe Default replacement for geopolitical entities (default: "\[GPE\]")
#' @param language Default replacement for languages (default: "\[LANGUAGE\]")
#' @param law Default replacement for laws (default: "\[LAW\]")
#' @param loc Default replacement for locations (default: "\[LOC\]")
#' @param money Default replacement for money (default: "\[MONEY\]")
#' @param norp Default replacement for nationalities/religious/political groups (default: "\[NORP\]")
#' @param ordinal Default replacement for ordinal numbers (default: "\[ORDINAL\]")
#' @param org Default replacement for organizations (default: "\[ORG\]")
#' @param percent Default replacement for percentages (default: "\[PERCENT\]")
#' @param person Default replacement for person names (default: "\[PERSON\]")
#' @param product Default replacement for products (default: "\[PRODUCT\]")
#' @param quantity Default replacement for quantities (default: "\[QUANTITY\]")
#' @param time Default replacement for times (default: "\[TIME\]")
#' @param work_of_art Default replacement for works of art (default: "\[WORK_OF_ART\]")
#' @param propn Default replacement for proper nouns (default: "\[PROPN\]"). This only applies
#'   to [`anon_nlp_proper_nouns()`].
#'
#' @return A named list of default replacement values that can be set with [`options()`].
#'
#' @examples
#' # Get default options
#' default_opts <- nlp_default_replacements()
#'
#' # Customize specific replacements
#' custom_opts <- nlp_default_replacements(
#'   person = "[NAME]",
#'   org = "[COMPANY]"
#' )
#'
#' # Set as global option
#' previous_options <- options(anon.nlp_default_replacements = custom_opts)
#'
#' # Get current global option
#' options("anon.nlp_default_replacements")
#'
#' # Reset the previous global options
#' options(anon.nlp_default_replacements = previous_options$anon.nlp_default_replacements)
#'
#' @export
nlp_default_replacements <- function(
  cardinal = "[CARDINAL]",
  date = "[DATE]",
  event = "[EVENT]",
  fac = "[FAC]",
  gpe = "[GPE]",
  language = "[LANGUAGE]",
  law = "[LAW]",
  loc = "[LOC]",
  money = "[MONEY]",
  norp = "[NORP]",
  ordinal = "[ORDINAL]",
  org = "[ORG]",
  percent = "[PERCENT]",
  person = "[PERSON]",
  product = "[PRODUCT]",
  quantity = "[QUANTITY]",
  time = "[TIME]",
  work_of_art = "[WORK_OF_ART]",
  propn = "[PROPN]"
) {
  list(
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
}

# Helper function to retrieve the default replacement option for an NLP entity type.
get_nlp_default_replacement <- function(entity_type) {
  entity_type <- tolower(entity_type)
  nlp_defaults <- getOption("anon.nlp_default_replacements")
  if (!is.null(nlp_defaults)) {
    if (
      is.list(nlp_defaults) && entity_type %in% tolower(names(nlp_defaults))
    ) {
      names(nlp_defaults) <- tolower(names(nlp_defaults))
      return(nlp_defaults[[entity_type]])
    }
  }

  default_replacement <- getOption("anon.default_replacement")
  if (!is.null(default_replacement)) {
    return(default_replacement)
  }

  nlp_function_defaults <- nlp_default_replacements()
  names(nlp_function_defaults) <- tolower(names(nlp_function_defaults))
  if (entity_type %in% names(nlp_function_defaults)) {
    return(nlp_function_defaults[[entity_type]])
  }

  return("[REDACTED]")
}
