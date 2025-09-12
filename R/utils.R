# Helper function to collect all text from an object
collect_text <- function(x) {
  # Handle missing values and empty input
  if (length(x) == 0 || all(is.na(x))) {
    return(character(0))
  }

  # Extract text from different data types
  text <- character(0)

  if (is.character(x)) {
    text <- x
  } else if (is.factor(x)) {
    text <- levels(x)
  } else if (is.data.frame(x)) {
    # Extract text from character and factor columns
    char_cols <- sapply(x, function(col) is.character(col) || is.factor(col))
    if (any(char_cols)) {
      text_data <- x[char_cols]
      text <- unlist(
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
    text <- unique(unlist(
      lapply(x, collect_text),
      use.names = FALSE
    ))
  } else {
    # Try to coerce to character if not a supported type
    text <- as.character(x)
  }

  # Remove NA values for processing
  text_clean <- text[!is.na(text)]

  return(text_clean)
}
