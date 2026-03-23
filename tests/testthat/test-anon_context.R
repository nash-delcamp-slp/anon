# new_anon_context ---------------------------------------------------------

test_that("new_anon_context creates correct class", {
  x <- "hello"
  result <- new_anon_context(x)

  expect_s3_class(result, "anon_context")
  expect_equal(class(result), c("anon_context", "character"))
})

test_that("new_anon_context preserves original classes", {
  x <- data.frame(a = 1)
  result <- new_anon_context(x)

  expect_s3_class(result, "anon_context")
  expect_s3_class(result, "data.frame")
  expect_equal(class(result), c("anon_context", "data.frame"))
})

test_that("new_anon_context passes additional attributes via dots", {
  result <- new_anon_context("test", custom_attr = "value")

  expect_equal(attr(result, "custom_attr"), "value")
})

# c.anon_context -----------------------------------------------------------

test_that("c() combines anon_context objects into anon_context_collection", {
  a <- new_anon_context("first")
  b <- new_anon_context("second")

  result <- c(a, b)

  expect_s3_class(result, "anon_context_collection")
  expect_s3_class(result, "anon_context")
  expect_length(result, 2)
})

test_that("c() with named arguments preserves names", {
  a <- new_anon_context("first")
  b <- new_anon_context("second")

  result <- c(foo = a, bar = b)

  expect_equal(names(result), c("foo", "bar"))
})

test_that("c() with unnamed arguments works", {
  a <- new_anon_context("first")
  b <- new_anon_context("second")

  result <- c(a, b)

  expect_null(names(result))
  expect_length(result, 2)
})

test_that("c() errors on non-anon_context objects", {
  a <- new_anon_context("first")

  expect_error(c(a, "not anon_context"), "All objects must be anon_context objects")
  expect_error(c(a, 42), "All objects must be anon_context objects")
  expect_error(c(a, list(1, 2)), "All objects must be anon_context objects")
})

test_that("c() with single element produces collection", {
  a <- new_anon_context("only one")

  result <- c(a)

  expect_s3_class(result, "anon_context_collection")
  expect_length(result, 1)
})

test_that("c() with mix of named and unnamed preserves partial names", {
  a <- new_anon_context("first")
  b <- new_anon_context("second")

  result <- c(foo = a, b)

  expect_equal(names(result), c("foo", ""))
})

# print.anon_context_collection -------------------------------------------

test_that("print.anon_context_collection outputs header", {
  a <- new_anon_context("content_a")
  b <- new_anon_context("content_b")
  collection <- c(a, b)

  expect_output(print(collection), "=== ANONYMIZED DATA CONTEXT ===")
})

test_that("print.anon_context_collection with named elements shows name headers", {
  a <- new_anon_context("content_a")
  b <- new_anon_context("content_b")
  collection <- c(alpha = a, beta = b)

  out <- capture.output(print(collection))
  combined <- paste(out, collapse = "\n")

  expect_true(grepl("alpha", combined, fixed = TRUE))
  expect_true(grepl("beta", combined, fixed = TRUE))
})

test_that("print.anon_context_collection with unnamed elements omits name headers", {
  a <- new_anon_context("content_a")
  b <- new_anon_context("content_b")
  collection <- c(a, b)

  out <- capture.output(print(collection))
  combined <- paste(out, collapse = "\n")

  expect_false(grepl("---  ---", combined, fixed = TRUE))
})

test_that("print.anon_context_collection prints each element content", {
  a <- new_anon_context("ITEM_ONE")
  b <- new_anon_context("ITEM_TWO")
  collection <- c(a, b)

  expect_output(print(collection), "ITEM_ONE")
  expect_output(print(collection), "ITEM_TWO")
})

test_that("print.anon_context_collection returns x invisibly", {
  a <- new_anon_context("test")
  collection <- c(a)

  result <- withVisible(print(collection))

  expect_false(result$visible)
  expect_identical(result$value, collection)
})

# print.anon_context ------------------------------------------------------

test_that("print.anon_context delegates to underlying class print method", {
  x <- new_anon_context("hello world")

  expect_output(print(x), "hello world")
})

test_that("print.anon_context returns x invisibly", {
  x <- new_anon_context("test")

  result <- withVisible(print(x))

  expect_false(result$visible)
})

# Integration with anon() -------------------------------------------------

test_that("anon() returns an anon_context object", {
  result <- anon(c("Alice", "Bob"), pattern_list = c("Alice"))

  expect_s3_class(result, "anon_context")
})

test_that("c() works with objects from anon()", {
  a <- anon(c("Alice", "Bob"), pattern_list = c("Alice"))
  b <- anon(c("Charlie", "Dave"), pattern_list = c("Charlie"))

  result <- c(data1 = a, data2 = b)

  expect_s3_class(result, "anon_context_collection")
  expect_equal(names(result), c("data1", "data2"))
  expect_length(result, 2)
})
