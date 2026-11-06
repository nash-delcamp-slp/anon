#' Read content from files as text
#'
#' Extract text content from common document formats. The main dispatcher
#' \code{read_content()} detects the format from the file extension and
#' delegates to the appropriate helper. Format-specific functions are also
#' exported for direct use.
#'
#' @param path Character vector of file paths to read.
#' @param name Character vector of original file names, used for extension
#'   detection and output naming. Defaults to \code{basename(path)}. This is
#'   important when reading Shiny upload temp files whose paths lack meaningful
#'   extensions.
#'
#' @return A named character vector. Names are the file names and values are the
#'   extracted text content.
#'
#' @export
read_content <- function(path, name = basename(path)) {
  if (length(path) == 0) {
    return(character(0))
  }

  stopifnot(
    is.character(path),
    is.character(name),
    length(path) == length(name)
  )

  out <- vapply(
    seq_along(path),
    function(i) read_content_single(path[i], name[i]),
    character(1)
  )

  names(out) <- name
  out
}

read_content_single <- function(path, name) {
  ext <- tolower(tools::file_ext(name))

  switch(ext,
    docx = read_content_docx(path),
    pptx = read_content_pptx(path),
    xlsx = ,
    xls  = read_content_xlsx(path),
    pdf  = read_content_pdf(path),
    read_content_text(path)
  )
}


#' @describeIn read_content Read a Word document (.docx)
#'
#' @details
#' \code{read_content_docx()} requires the \pkg{officer} package. Paragraphs
#' are extracted in document order. Tables are formatted as plain text.
#'
#' @export
read_content_docx <- function(path) {
  ensure_content_package("officer", ".docx")

  doc <- officer::read_docx(path)
  summary_df <- officer::docx_summary(doc)

  if (nrow(summary_df) == 0) {
    return("")
  }

  # A table is one block. officer reports a separate doc_index for each table
  # cell and identifies the table by table_index; older versions give all cells
  # of a table the same doc_index. Blocks are kept in document order.
  is_cell <- summary_df$content_type == "table cell"
  table_id <- if ("table_index" %in% names(summary_df)) {
    summary_df$table_index
  } else {
    summary_df$doc_index
  }
  block <- ifelse(is_cell, paste0("table-", table_id), paste0("doc-", summary_df$doc_index))
  block_order <- tapply(summary_df$doc_index, block, min)
  parts <- character(0)

  for (id in names(sort(block_order))) {
    rows <- summary_df[block == id, , drop = FALSE]

    if (startsWith(id, "table-")) {
      if (all(c("row_id", "cell_id") %in% names(rows))) {
        rows <- rows[order(rows$row_id, rows$cell_id), , drop = FALSE]
        parts <- c(parts, "", format_plain_table(reshape_table_cells(rows)), "")
      }
    } else if (rows$content_type[1] == "paragraph") {
      text <- rows$text[!is.na(rows$text) & nzchar(rows$text)]
      if (length(text) > 0) {
        parts <- c(parts, text)
      }
    }
  }

  paste(parts, collapse = "\n")
}


#' @describeIn read_content Read a PowerPoint presentation (.pptx)
#'
#' @details
#' \code{read_content_pptx()} requires the \pkg{officer} package. Text is
#' extracted from each slide with slide number headers.
#'
#' @export
read_content_pptx <- function(path) {
  ensure_content_package("officer", ".pptx")

  doc <- officer::read_pptx(path)
  summary_df <- officer::pptx_summary(doc)

  # pptx_summary() returns NULL for a deck with no slide text.
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return("")
  }

  text_rows <- summary_df[
    summary_df$content_type == "paragraph" &
    !is.na(summary_df$text) &
    nzchar(summary_df$text),
    ,
    drop = FALSE
  ]

  if (nrow(text_rows) == 0) {
    return("")
  }

  slides <- split(text_rows, text_rows$slide_id)
  parts <- character(0)

  for (slide_id in sort(as.integer(names(slides)))) {
    slide_text <- slides[[as.character(slide_id)]]$text
    parts <- c(
      parts,
      paste0("--- Slide ", slide_id, " ---"),
      slide_text,
      ""
    )
  }

  paste(trimws(paste(parts, collapse = "\n")))
}


#' @describeIn read_content Read an Excel file (.xlsx, .xls)
#'
#' @param sheet Character vector of sheet names to read, or \code{NULL} (the
#'   default) to read all sheets.
#'
#' @details
#' \code{read_content_xlsx()} requires the \pkg{readxl} package. Each sheet is
#' formatted as a plain-text table with a sheet name header.
#'
#' @export
read_content_xlsx <- function(path, sheet = NULL) {
  ensure_content_package("readxl", ".xlsx")

  sheets <- sheet %||% readxl::excel_sheets(path)

  if (length(sheets) == 0) {
    return("")
  }

  parts <- character(0)

  for (s in sheets) {
    df <- tryCatch(
      readxl::read_excel(path, sheet = s),
      error = function(e) NULL
    )

    if (is.null(df) || nrow(df) == 0) {
      parts <- c(parts, paste0("--- Sheet: ", s, " ---"), "(empty)", "")
      next
    }

    parts <- c(
      parts,
      paste0("--- Sheet: ", s, " ---"),
      format_plain_table(df),
      ""
    )
  }

  paste(trimws(paste(parts, collapse = "\n")))
}


#' @describeIn read_content Read a PDF file
#'
#' @details
#' \code{read_content_pdf()} requires the \pkg{pdftools} package. Text is
#' extracted per page with page number headers.
#'
#' @export
read_content_pdf <- function(path) {
  ensure_content_package("pdftools", ".pdf")

  pages <- pdftools::pdf_text(path)

  if (length(pages) == 0) {
    return("")
  }

  if (length(pages) == 1) {
    return(trimws(pages))
  }

  parts <- character(0)

  for (i in seq_along(pages)) {
    page_text <- trimws(pages[i])
    if (nzchar(page_text)) {
      parts <- c(parts, paste0("--- Page ", i, " ---"), page_text, "")
    }
  }

  paste(trimws(paste(parts, collapse = "\n")))
}


#' @describeIn read_content Read a plain text file
#'
#' @details
#' \code{read_content_text()} reads any file as plain text using
#' \code{readLines()}. This is the fallback for unrecognized file extensions.
#'
#' @export
read_content_text <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  paste(lines, collapse = "\n")
}


# -- internal helpers ----------------------------------------------------------

ensure_content_package <- function(pkg, format) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Reading ", format, " files requires the '", pkg, "' package. ",
      "Install it with: install.packages('", pkg, "')",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

reshape_table_cells <- function(cells) {
  rows <- split(cells, cells$row_id)
  max_cols <- max(vapply(rows, nrow, integer(1)))

  mat <- matrix("", nrow = length(rows), ncol = max_cols)
  for (i in seq_along(rows)) {
    vals <- rows[[i]]$text
    vals[is.na(vals)] <- ""
    mat[i, seq_along(vals)] <- vals
  }

  as.data.frame(mat, stringsAsFactors = FALSE)
}

# Plain-text table with every row and column. Tibbles are converted first
# because their print method truncates rows and adds type annotations.
format_plain_table <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  utils::capture.output(print(df, row.names = FALSE, max = .Machine$integer.max))
}
