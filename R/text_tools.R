#' Clean text for safer prompt input
#'
#' Normalizes line endings and optionally trims whitespace, squishes repeated
#' spaces, and compresses consecutive blank lines.
#'
#' @param x Character vector to clean.
#' @param trim Logical indicating whether to trim leading and trailing whitespace
#'   on each line.
#' @param squish_whitespace Logical indicating whether to collapse repeated
#'   internal whitespace on each line.
#' @param squash_blank_lines Logical indicating whether to compress repeated
#'   blank lines to a single blank line.
#'
#' @return A character vector with cleaned text.
#'
#' @export
anon_clean_text <- function(
  x,
  trim = TRUE,
  squish_whitespace = TRUE,
  squash_blank_lines = TRUE
) {
  x <- as.character(x)

  vapply(x, clean_single_text, character(1),
    trim = trim,
    squish_whitespace = squish_whitespace,
    squash_blank_lines = squash_blank_lines,
    USE.NAMES = FALSE
  )
}

clean_single_text <- function(
  x,
  trim = TRUE,
  squish_whitespace = TRUE,
  squash_blank_lines = TRUE
) {
  if (is.na(x)) {
    return(NA_character_)
  }

  lines <- strsplit(gsub("\r\n?", "\n", x), "\n", fixed = FALSE)[[1]]

  if (isTRUE(trim)) {
    lines <- trimws(lines)
  }

  if (isTRUE(squish_whitespace)) {
    lines <- gsub("[[:space:]]+", " ", lines)
  }

  if (isTRUE(squash_blank_lines) && length(lines) > 0) {
    blank <- lines == ""
    keep <- !(blank & dplyr::lag(blank, default = FALSE))
    lines <- lines[keep]
  }

  paste(lines, collapse = "\n")
}

parse_pattern_rule_text <- function(x) {
  if (is.null(x) || length(x) == 0 || identical(trimws(x), "")) {
    return(list())
  }

  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != "" & !startsWith(lines, "#")]

  if (length(lines) == 0) {
    return(list())
  }

  rules <- list()

  for (line in lines) {
    if (!grepl("=", line, fixed = TRUE)) {
      next
    }

    pieces <- strsplit(line, "=", fixed = TRUE)[[1]]
    replacement <- trimws(pieces[1])
    patterns <- paste(pieces[-1], collapse = "=")
    patterns <- trimws(strsplit(patterns, "|", fixed = TRUE)[[1]])
    patterns <- patterns[patterns != ""]

    if (replacement == "" || length(patterns) == 0) {
      next
    }

    rules[[replacement]] <- unique(patterns)
  }

  rules
}

format_text_comparison <- function(comparison) {
  paste(
    c(
      format_table_block(comparison$summary),
      "",
      "Line-level details:",
      format_table_block(comparison$details)
    ),
    collapse = "\n"
  )
}
