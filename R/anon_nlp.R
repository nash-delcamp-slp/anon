#' Anonymize Named Entities in Text Using NLP
#'
#' These functions use natural language processing to identify and anonymize
#' different types of named entities from text. Each function combines NLP
#' entity extraction with pattern expansion and anonymization.
#'
#' @param x A character vector to anonymize
#' @param ... Additional arguments passed to \code{\link{anon}}
#'
#' @return An anonymized object of class \code{anon_context} with named entities
#'   replaced according to the anonymization workflow.
#'
#' @details
#' The functions follow a three-step workflow:
#' \enumerate{
#'   \item Use NLP to extract potentially sensitive information of the specified type
#'   \item Extend patterns using \code{\link{more_patterns}} for comprehensive matching
#'   \item Apply \code{\link{anon}} to anonymize the identified patterns
#' }
#'
#' Available entity types:
#' \itemize{
#'   \item \code{anon_nlp_dates()}: Anonymize dates
#'   \item \code{anon_nlp_locations()}: Anonymize geographic locations and place names
#'   \item \code{anon_nlp_money()}: Anonymize monetary amounts and currency expressions
#'   \item \code{anon_nlp_organizations()}: Anonymize organization and company names
#'   \item \code{anon_nlp_percentages()}: Anonymize percentage expressions
#'   \item \code{anon_nlp_people()}: Anonymize person names
#' }
#'
#' @examples
#' text <- c("John Smith works at Microsoft in Seattle.",
#'           "The deal was worth $1.2 million in 2023.")
#'
#' # Anonymize person names
#' anon_nlp_people(text)
#'
#' # Anonymize organizations with custom replacement
#' anon_nlp_organizations(text, default_replacement = "[COMPANY]")
#'
#' @seealso
#' \code{\link{nlp_get_people}}, \code{\link{more_patterns}}, \code{\link{anon}}
#'
#' @name anon_nlp
#' @export
anon_nlp_dates <- function(x, ...) {
  anon_nlp_kind(x, "date", ...)
}

#' @rdname anon_nlp
#' @export
anon_nlp_locations <- function(x, ...) {
  anon_nlp_kind(x, "location", ...)
}

#' @rdname anon_nlp
#' @export
anon_nlp_money <- function(x, ...) {
  anon_nlp_kind(x, "money", ...)
}

#' @rdname anon_nlp
#' @export
anon_nlp_organizations <- function(x, ...) {
  anon_nlp_kind(x, "organization", ...)
}

#' @rdname anon_nlp
#' @export
anon_nlp_percentages <- function(x, ...) {
  anon_nlp_kind(x, "percentage", ...)
}

#' @rdname anon_nlp
#' @export
anon_nlp_people <- function(x, ...) {
  anon_nlp_kind(x, "person", ...)
}

# Internal worker function that handles the NLP workflow
anon_nlp_kind <- function(
  x,
  kind = c("date", "location", "money", "organization", "percentage", "person"),
  ...
) {
  # Check that x is a character vector
  if (!is.character(x)) {
    stop("x must be a character vector")
  }
  
  kind <- match.arg(kind)
  
  # Get default_replacement from arguments or options
  args <- list(...)
  if (!"default_replacement" %in% names(args)) {
    # Check for kind-specific option first
    kind_specific_option <- paste0("anon.default_replacement_", kind)
    kind_specific_replacement <- getOption(kind_specific_option, default = NULL)
    
    if (!is.null(kind_specific_replacement)) {
      args$default_replacement <- kind_specific_replacement
    } else if (kind == "person") {
      # Just in case someone sets a people option instead of person
      people_option <- getOption("anon.default_replacement_people", default = NULL)
      if (!is.null(!is.null(people_option))) {
        args$default_replacement <- people_option
      }
    } else {
      # Fall back to general default replacement option
      general_replacement <- getOption("anon.default_replacement", default = NULL)
      if (!is.null(general_replacement)) {
        args$default_replacement <- general_replacement
      }
    }
    # If none of the above options are set, use defaults based on kind.
    if (!"default_replacement" %in% names(args)) {
      args$default_replacement <- switch(kind,
        "date" = "[DATE]",
        "location" = "[PLACE]", 
        "money" = "[$]",
        "organization" = "[ORGANIZATION]",
        "percentage" = "[%]",
        "person" = "[NAME]"
      )
    }
  }

  # Use NLP to get potentially sensitive information
  patterns <- switch(kind,
    "date" = nlp_get_dates(x),
    "location" = nlp_get_locations(x),
    "money" = nlp_get_money(x),
    "organization" = nlp_get_organizations(x),
    "percentage" = nlp_get_percentages(x),
    "person" = nlp_get_people(x)
  )
  
  # Extend patterns with more_patterns()
  if (length(patterns) > 0) {
    expanded_patterns <- more_patterns(patterns)
  } else {
    expanded_patterns <- character(0)
  }
  
  # Apply anon() with the expanded patterns
  do.call(anon, c(list(x = x, pattern_list = expanded_patterns), args))
}
