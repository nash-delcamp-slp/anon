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
#' @param original Logical. Whether to include original patterns (default: TRUE)
#' @param spaces_to_flexible Logical. Whether to include space-flexible patterns (default: TRUE)
#' @param individual Logical. Whether to include individual words (default: TRUE)
#' @return Character vector with selected pattern expansions
#' @examples
#' people <- c("John Smith", "Mary Jane Watson")
#' more_patterns(people)
#' more_patterns(people, individual = FALSE)
#' @export
more_patterns <- function(patterns, original = TRUE, spaces_to_flexible = TRUE, individual = TRUE) {
  # Handle empty or NA input
  if (length(patterns) == 0 || all(is.na(patterns))) {
    return(character(0))
  }
  
  # Remove NA values
  patterns_clean <- patterns[!is.na(patterns)]
  
  if (length(patterns_clean) == 0) {
    return(character(0))
  }
  
  result <- character(0)
  
  # 1. Original items
  if (original) {
    original_items <- patterns_clean
    result <- c(result, original_items)
  }
  
  # 2. Patterns with spaces replaced by flexible regex
  if (spaces_to_flexible) {
    # Replace spaces with regex that matches any single character or any amount of whitespace
    space_flexible_patterns <- gsub("\\s+", "\\\\s*.\\\\s*", patterns_clean)
    result <- c(result, space_flexible_patterns)
  }
  
  # 3. Individual words from each item
  if (individual) {
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
    result <- c(result, individual_words)
  }
  
  # Remove duplicates while preserving order
  unique(result)
}
