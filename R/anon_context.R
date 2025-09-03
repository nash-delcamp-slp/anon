# Class constructors and methods ------------------------------------------

#' Constructor for anon_context objects
#'
#' @param x The object to wrap with anon_context class
#' @param ... Additional attributes to set
#' @return Object with anon_context class
#' @keywords internal
new_anon_context <- function(x, ...) {
  structure(x, class = c("anon_context", class(x)), ...)
}

#' Combine anon_context objects
#'
#' This method allows combining multiple anonymized objects created by `anon()`
#' or `anon_data_summary()` using `c()`. The result maintains the anonymized content
#' and provides a print method suitable for LLM context. An additional header is 
#' included in printed output for named elements.
#'
#' @param ... anon_context objects to combine
#' @return An anon_context_collection object containing all input objects
#'
#' @examples
#' df <- data.frame(name = c("John", "Jane"), age = c(25, 30))
#' anon_df <- anon(df, pattern_list = c("John", "Jane"))
#' summary_obj <- anon_data_summary(list(df = df))
#' combined <- c(anon_df, summary_obj)
#' print(combined)
#'
#' @export
c.anon_context <- function(...) {
  dots <- list(...)

  # Ensure all objects are anon_context
  if (!all(vapply(dots, inherits, logical(1), "anon_context"))) {
    stop("All objects must be anon_context objects", call. = FALSE)
  }

  # Create combined object
  combined <- structure(
    dots,
    class = c("anon_context_collection", "anon_context")
  )
  names(combined) <- names(dots)

  combined
}

# Print methods -----------------------------------------------------------

#' Print method for anon_context_collection
#'
#' Displays combined anonymized objects in a format suitable for LLM prompts.
#' Each object is clearly delineated and presented with appropriate context.
#'
#' @param x An anon_context_collection object
#' @param ... Additional arguments (currently unused)
#' @return Invisibly returns x
#'
#' @export
print.anon_context_collection <- function(x, ...) {
  cat("=== ANONYMIZED DATA CONTEXT ===\n\n")

  for (i in seq_along(x)) {
    # Add header for named elements
    if (!is.null(names(x)) && !is.na(names(x)[i]) && names(x)[i] != "") {
      cat("--- ", names(x)[i], " ---\n")
    }
    
    # Delegate to individual print methods
    print(x[[i]], ...)
    cat("\n")
  }

  invisible(x)
}

#' Print method for single anon_context objects
#'
#' @param x An anon_context object
#' @param ... Additional arguments passed to NextMethod
#' @return Invisibly returns x
#'
#' @export
print.anon_context <- function(x, ...) {
  # Remove anon_context class temporarily for clean printing
  class(x) <- setdiff(class(x), "anon_context")
  print(x, ...)
  invisible(x)
}
