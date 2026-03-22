# Read content from files as text

Extract text content from common document formats. The main dispatcher
`read_content()` detects the format from the file extension and
delegates to the appropriate helper. Format-specific functions are also
exported for direct use.

## Usage

``` r
read_content(path, name = basename(path))

read_content_docx(path)

read_content_pptx(path)

read_content_xlsx(path, sheet = NULL)

read_content_pdf(path)

read_content_text(path)
```

## Arguments

- path:

  Character vector of file paths to read.

- name:

  Character vector of original file names, used for extension detection
  and output naming. Defaults to `basename(path)`. This is important
  when reading Shiny upload temp files whose paths lack meaningful
  extensions.

- sheet:

  Character vector of sheet names to read, or `NULL` (the default) to
  read all sheets.

## Value

A named character vector. Names are the file names and values are the
extracted text content.

## Details

`read_content_docx()` requires the officer package. Paragraphs are
extracted in document order. Tables are formatted as plain text.

`read_content_pptx()` requires the officer package. Text is extracted
from each slide with slide number headers.

`read_content_xlsx()` requires the readxl package. Each sheet is
formatted as a plain-text table with a sheet name header.

`read_content_pdf()` requires the pdftools package. Text is extracted
per page with page number headers.

`read_content_text()` reads any file as plain text using
[`readLines()`](https://rdrr.io/r/base/readLines.html). This is the
fallback for unrecognized file extensions.

## Functions

- `read_content_docx()`: Read a Word document (.docx)

- `read_content_pptx()`: Read a PowerPoint presentation (.pptx)

- `read_content_xlsx()`: Read an Excel file (.xlsx, .xls)

- `read_content_pdf()`: Read a PDF file

- `read_content_text()`: Read a plain text file
