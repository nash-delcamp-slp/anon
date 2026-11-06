test_that("read_content_text() reads a plain text file", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))

  writeLines(c("Hello", "World"), tmp)

  result <- read_content_text(tmp)

  expect_equal(result, "Hello\nWorld")
})

test_that("read_content() dispatches to read_content_text() for .txt", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines("test content", tmp)

  result <- read_content(tmp)

  expect_equal(names(result), basename(tmp))
  expect_equal(unname(result), "test content")
})

test_that("read_content() uses name argument for extension detection", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines("data here", tmp)

  result <- read_content(tmp, name = "report.txt")

  expect_equal(names(result), "report.txt")
  expect_equal(unname(result), "data here")
})

test_that("read_content() handles multiple files", {
  tmp1 <- tempfile(fileext = ".txt")
  tmp2 <- tempfile(fileext = ".csv")
  on.exit(unlink(c(tmp1, tmp2)))
  writeLines("file one", tmp1)
  writeLines("a,b\n1,2", tmp2)

  result <- read_content(c(tmp1, tmp2))

  expect_length(result, 2)
  expect_equal(names(result), c(basename(tmp1), basename(tmp2)))
})

test_that("read_content() returns empty vector for empty input", {
  result <- read_content(character(0))
  expect_length(result, 0)
})

test_that("read_content() falls back to text for unknown extensions", {
  tmp <- tempfile(fileext = ".log")
  on.exit(unlink(tmp))
  writeLines("log line", tmp)

  result <- read_content(tmp)

  expect_equal(unname(result), "log line")
})

test_that("ensure_content_package errors for missing package", {
  expect_error(
    ensure_content_package("nonexistent_package_xyz", ".xyz"),
    "requires the 'nonexistent_package_xyz' package"
  )
})

test_that("read_content_docx() errors when officer not installed", {
  skip_if(requireNamespace("officer", quietly = TRUE))
  expect_error(read_content_docx(tempfile()), "officer")
})

test_that("read_content_xlsx() errors when readxl not installed", {
  skip_if(requireNamespace("readxl", quietly = TRUE))
  expect_error(read_content_xlsx(tempfile()), "readxl")
})

test_that("read_content_pdf() errors when pdftools not installed", {
  skip_if(requireNamespace("pdftools", quietly = TRUE))
  expect_error(read_content_pdf(tempfile()), "pdftools")
})

test_that("read_content_pptx() errors when officer not installed", {
  skip_if(requireNamespace("officer", quietly = TRUE))
  expect_error(read_content_pptx(tempfile()), "officer")
})

test_that("read_content() reads paragraphs from .docx files", {
  skip_if_not_installed("officer")
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path))

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Hello Alice")
  doc <- officer::body_add_par(doc, "")
  doc <- officer::body_add_par(doc, "Goodbye")
  print(doc, target = path)

  expect_equal(unname(read_content(path)), "Hello Alice\nGoodbye")

  empty <- tempfile(fileext = ".docx")
  on.exit(unlink(empty), add = TRUE)
  print(officer::read_docx(), target = empty)
  expect_equal(unname(read_content(empty)), "")
})

test_that("read_content() reads slide text from .pptx files", {
  skip_if_not_installed("officer")
  path <- tempfile(fileext = ".pptx")
  on.exit(unlink(path))

  deck <- officer::read_pptx()
  deck <- officer::add_slide(deck, layout = "Title and Content", master = "Office Theme")
  deck <- officer::ph_with(deck, "First slide", location = officer::ph_location_type("title"))
  deck <- officer::add_slide(deck, layout = "Title and Content", master = "Office Theme")
  deck <- officer::ph_with(deck, "Second slide", location = officer::ph_location_type("title"))
  print(deck, target = path)

  expect_equal(
    unname(read_content(path)),
    "--- Slide 1 ---\nFirst slide\n\n--- Slide 2 ---\nSecond slide"
  )
})

test_that("read_content() reads sheets from .xlsx files", {
  skip_if_not_installed("readxl")
  # fixtures/book.xlsx: sheet "Raw" (USUBJID, AGE; 2 rows) and an empty sheet "Empty"
  path <- test_path("fixtures", "book.xlsx")

  result <- unname(read_content(path))
  expect_match(result, "^--- Sheet: Raw ---")
  expect_match(result, "USUBJID")
  expect_match(result, "--- Sheet: Empty ---\n(empty)", fixed = TRUE)

  raw_only <- read_content_xlsx(path, sheet = "Raw")
  expect_false(grepl("Empty", raw_only))

  expect_equal(list_excel_file_sheets(path), c("Raw", "Empty"))
})

test_that("read_content() reads text from .pdf files", {
  skip_if_not_installed("pdftools")
  one_page <- tempfile(fileext = ".pdf")
  pages <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(one_page, pages)))

  grDevices::pdf(one_page)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Only page")
  grDevices::dev.off()

  grDevices::pdf(pages)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Page one")
  graphics::plot.new()
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Page three")
  grDevices::dev.off()

  expect_equal(unname(read_content(one_page)), "Only page")
  expect_equal(
    unname(read_content(pages)),
    "--- Page 1 ---\nPage one\n\n--- Page 3 ---\nPage three"
  )
})

test_that("read_content_xlsx() and read_content_pdf() return empty text for empty files", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("pdftools")

  local_mocked_bindings(excel_sheets = function(path) character(0), .package = "readxl")
  expect_equal(read_content_xlsx("book.xlsx"), "")

  local_mocked_bindings(pdf_text = function(path) character(0), .package = "pdftools")
  expect_equal(read_content_pdf("empty.pdf"), "")
})

test_that("read_content() keeps every row of an Excel sheet", {
  skip_if_not_installed("readxl")
  rows <- 30
  local_mocked_bindings(
    excel_sheets = function(path) "Big",
    read_excel = function(path, sheet) {
      tibble::tibble(id = sprintf("S%03d", seq_len(rows)), value = seq_len(rows))
    },
    .package = "readxl"
  )

  result <- read_content_xlsx("big.xlsx")
  expect_match(result, "S030", fixed = TRUE)
  expect_false(grepl("more rows", result, fixed = TRUE))
  expect_false(grepl("A tibble", result, fixed = TRUE))
})

test_that("read_content() keeps Word tables intact and in document order", {
  skip_if_not_installed("officer")
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path))

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Before")
  doc <- officer::body_add_table(doc, data.frame(arm = c("A", "B"), n = c("10", "12")))
  doc <- officer::body_add_par(doc, "After")
  print(doc, target = path)

  result <- strsplit(unname(read_content(path)), "\n", fixed = TRUE)[[1]]
  result <- trimws(result[nzchar(trimws(result))])

  expect_equal(result[1], "Before")
  expect_equal(result[length(result)], "After")
  table_lines <- result[-c(1, length(result))]
  expect_length(table_lines, 4)
  expect_match(table_lines[2], "^arm\\s+n$")
  expect_match(table_lines[3], "^A\\s+10$")
  expect_match(table_lines[4], "^B\\s+12$")
})

test_that("read_content() returns empty text for .pptx files without slide text", {
  skip_if_not_installed("officer")
  no_slides <- tempfile(fileext = ".pptx")
  blank_slide <- tempfile(fileext = ".pptx")
  on.exit(unlink(c(no_slides, blank_slide)))

  print(officer::read_pptx(), target = no_slides)
  deck <- officer::add_slide(officer::read_pptx(), layout = "Title Only", master = "Office Theme")
  print(deck, target = blank_slide)

  expect_equal(unname(read_content(no_slides)), "")
  expect_equal(unname(read_content(blank_slide)), "")
})

test_that("read_content_docx() groups table cells by doc_index without table_index", {
  skip_if_not_installed("officer")
  # Layout of docx_summary() in officer versions that give every cell of a
  # table the same doc_index and have no table_index column.
  local_mocked_bindings(
    read_docx = function(path) NULL,
    docx_summary = function(x) {
      data.frame(
        doc_index = c(1, 2, 2, 2, 2, 3),
        content_type = c("paragraph", rep("table cell", 4), "paragraph"),
        text = c("Before", "arm", "n", "A", "10", "After"),
        row_id = c(NA, 1, 1, 2, 2, NA),
        cell_id = c(NA, 1, 2, 1, 2, NA),
        stringsAsFactors = FALSE
      )
    },
    .package = "officer"
  )

  lines <- strsplit(read_content_docx("old.docx"), "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines[nzchar(trimws(lines))])
  expect_equal(lines[1], "Before")
  expect_match(lines[3], "^arm\\s+n$")
  expect_match(lines[4], "^A\\s+10$")
  expect_equal(lines[5], "After")
})

test_that("read_content_pptx() returns empty text when slides only have empty paragraphs", {
  skip_if_not_installed("officer")
  local_mocked_bindings(
    read_pptx = function(path) NULL,
    pptx_summary = function(x) {
      data.frame(content_type = "paragraph", text = "", slide_id = 1, stringsAsFactors = FALSE)
    },
    .package = "officer"
  )
  expect_equal(read_content_pptx("empty.pptx"), "")
})
