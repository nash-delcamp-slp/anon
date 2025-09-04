#' Create expanded pattern vector for text matching
#'
#' Accepts a character vector of patterns and returns an expanded vector containing:
#' 1. Original items
#' 2. Patterns with spaces replaced with a pattern of any number of spaces and any 
#'    one character between words. 
#' 3. Individual words from each item, split by spaces and punctuation
#' 
#' The intention of the order is to replace phrases with as few replacements as possible 
#' while taking additional efforts to anonymize all sensitive information.
#'
#' @param patterns Character vector of patterns to expand
#' @return Character vector with original patterns, space-flexible patterns, and individual words
#' @examples
#' people <- c("John Smith", "Mary Jane Watson")
#' more_patterns(people)
#' @export
more_patterns <- function(patterns) {
  # Handle empty or NA input
  if (length(patterns) == 0 || all(is.na(patterns))) {
    return(character(0))
  }
  
  # Remove NA values
  patterns_clean <- patterns[!is.na(patterns)]
  
  if (length(patterns_clean) == 0) {
    return(character(0))
  }
  
  # 1. Original items
  original_items <- patterns_clean
  
  # 2. Patterns with spaces replaced by flexible regex
  # Replace spaces with regex that matches any single character or any amount of whitespace
  space_flexible_patterns <- gsub("\\s+", "\\\\s*.\\\\s*", patterns_clean)
  
  # 3. Individual words from each item
  individual_words <- character(0)
  for (pattern in patterns_clean) {
    # Split on whitespace and punctuation, keep only alphabetic words
    words <- unlist(strsplit(pattern, c("\\s+", "[:punct:]+")))
    # Filter out empty strings
    words <- words[words != ""]
    individual_words <- c(individual_words, words)
  }
  
  # Remove duplicates from individual words while preserving order
  individual_words <- unique(individual_words)
  
  # Combine all patterns in order: original, space-flexible, individual words
  result <- c(original_items, space_flexible_patterns, individual_words)
  
  # Remove duplicates while preserving order
  unique(result)
}
