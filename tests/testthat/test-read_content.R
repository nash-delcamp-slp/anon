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
