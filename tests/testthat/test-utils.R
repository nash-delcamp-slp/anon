test_that("collect_text() returns character vector as-is", {
  result <- anon:::collect_text(c("hello", "world"))

  expect_equal(result, c("hello", "world"))
})

test_that("collect_text() removes NAs from character vector", {
  result <- anon:::collect_text(c("hello", NA, "world", NA))

  expect_equal(result, c("hello", "world"))
})

test_that("collect_text() returns levels for factor input", {
  f <- factor(c("b", "a", "b", "a", "c"), levels = c("a", "b", "c"))
  result <- anon:::collect_text(f)

  expect_equal(result, c("a", "b", "c"))
})

test_that("collect_text() extracts text from character and factor columns in a data frame", {
  df <- data.frame(
    name = c("Alice", "Bob"),
    age = c(30, 25),
    city = factor(c("NYC", "LA"), levels = c("LA", "NYC")),
    active = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  result <- anon:::collect_text(df)

  expect_true("Alice" %in% result)
  expect_true("Bob" %in% result)
  expect_true("LA" %in% result)
  expect_true("NYC" %in% result)
  expect_false("30" %in% result)
  expect_false("TRUE" %in% result)
})

test_that("collect_text() returns empty character for data frame with only numeric columns", {
  df <- data.frame(x = 1:3, y = c(4.5, 5.5, 6.5))
  result <- anon:::collect_text(df)

  expect_equal(result, character(0))
})

test_that("collect_text() recursively extracts text from nested lists", {
  lst <- list(
    c("alpha", "beta"),
    list("gamma", c("delta", "alpha"))
  )
  result <- anon:::collect_text(lst)

  expect_true(all(c("alpha", "beta", "gamma", "delta") %in% result))
  expect_equal(sum(result == "alpha"), 1)
})

test_that("collect_text() returns character(0) for empty input", {
  expect_equal(anon:::collect_text(character(0)), character(0))
  expect_equal(anon:::collect_text(list()), character(0))
})

test_that("collect_text() returns character(0) for all-NA input", {
  result <- anon:::collect_text(c(NA_character_, NA_character_))

  expect_equal(result, character(0))
})

test_that("collect_text() coerces numeric vector to character", {
  result <- anon:::collect_text(c(1, 2.5, 3))

  expect_equal(result, c("1", "2.5", "3"))
})

test_that("collect_text() handles list with NULL elements", {
  lst <- list("hello", NULL, "world")
  result <- anon:::collect_text(lst)

  expect_true("hello" %in% result)
  expect_true("world" %in% result)
})
