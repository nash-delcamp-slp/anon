# as_anon_json -------------------------------------------------------------

test_that("as_anon_json() produces valid JSON from a character vector", {
  result <- as_anon_json(c("hello", "world"))

  expect_type(result, "character")
  expect_length(result, 1)
  parsed <- jsonlite::fromJSON(result)
  expect_equal(parsed, c("hello", "world"))
})

test_that("as_anon_json() respects pretty argument", {
  compact <- as_anon_json(c("a", "b"), pretty = FALSE)
  pretty <- as_anon_json(c("a", "b"), pretty = TRUE)

  expect_true(nchar(pretty) > nchar(compact))
})

# as_anon_payload.anon_context ---------------------------------------------

test_that("as_anon_payload unwraps anon_context class", {
  x <- new_anon_context(c("hello", "world"))
  result <- as_anon_payload(x)

  expect_equal(result, c("hello", "world"))
})

test_that("as_anon_payload works on anon_context wrapping a data frame", {
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  x <- new_anon_context(df)
  result <- as_anon_payload(x)

  expect_true("columns" %in% names(result))
  expect_true("rows" %in% names(result))
  expect_equal(result$columns, c("a", "b"))
})

# as_anon_payload.anon_context_collection ----------------------------------

test_that("as_anon_payload serializes collections with names", {
  a <- new_anon_context("first")
  b <- new_anon_context("second")
  collection <- c(foo = a, bar = b)

  result <- as_anon_payload(collection)

  expect_equal(result$kind, "anon_context_collection")
  expect_equal(result$schema_version, 1L)
  expect_equal(names(result$items), c("foo", "bar"))
})

test_that("as_anon_payload serializes unnamed collections", {
  a <- new_anon_context("first")
  b <- new_anon_context("second")
  collection <- c(a, b)

  result <- as_anon_payload(collection)

  expect_equal(result$kind, "anon_context_collection")
  expect_null(names(result$items))
})

# serialize_supported_value ------------------------------------------------

test_that("serialize_supported_value handles NULL", {
  result <- anon:::serialize_supported_value(NULL)

  expect_null(result)
})

test_that("serialize_supported_value converts Date to character", {
  result <- anon:::serialize_supported_value(as.Date("2024-01-15"))

  expect_equal(result, "2024-01-15")
})

test_that("serialize_supported_value converts POSIXct to character", {
  x <- as.POSIXct("2024-01-15 10:30:00", tz = "UTC")
  result <- anon:::serialize_supported_value(x)

  expect_type(result, "character")
})

test_that("serialize_supported_value converts factor to character", {
  result <- anon:::serialize_supported_value(factor(c("a", "b")))

  expect_equal(result, c("a", "b"))
})

test_that("serialize_supported_value handles matrix as table", {
  m <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("x", "y")))
  result <- anon:::serialize_supported_value(m)

  expect_true("columns" %in% names(result))
  expect_true("rows" %in% names(result))
})

test_that("serialize_supported_value handles data frame", {
  df <- data.frame(a = 1:2)
  result <- anon:::serialize_supported_value(df)

  expect_equal(result$columns, "a")
  expect_length(result$rows, 2)
})

test_that("serialize_supported_value errors on unsupported S3 class", {
  x <- structure(list(), class = "my_custom_class")

  expect_error(
    anon:::serialize_supported_value(x),
    "Unsupported class.*my_custom_class"
  )
})

test_that("serialize_supported_value handles named list", {
  result <- anon:::serialize_supported_value(list(a = 1, b = "two"))

  expect_equal(names(result), c("a", "b"))
})

test_that("serialize_supported_value handles atomic vector", {
  result <- anon:::serialize_supported_value(c(1, 2, 3))

  expect_equal(result, c(1, 2, 3))
})

# serialize_list_payload ---------------------------------------------------

test_that("serialize_list_payload preserves names on named lists", {
  result <- anon:::serialize_list_payload(list(x = 1, y = "a"))

  expect_equal(names(result), c("x", "y"))
})

test_that("serialize_list_payload returns unnamed for unnamed lists", {
  result <- anon:::serialize_list_payload(list(1, "a"))

  expect_null(names(result))
})

test_that("serialize_list_payload handles nested lists", {
  result <- anon:::serialize_list_payload(list(a = list(b = 1)))

  expect_equal(names(result), "a")
  expect_equal(names(result$a), "b")
})

# serialize_atomic_payload -------------------------------------------------

test_that("serialize_atomic_payload returns unnamed vector as-is", {
  result <- anon:::serialize_atomic_payload(c(1, 2, 3))

  expect_equal(result, c(1, 2, 3))
})

test_that("serialize_atomic_payload converts named vector to named list", {
  result <- anon:::serialize_atomic_payload(c(a = 1, b = 2))

  expect_equal(names(result), c("a", "b"))
})

test_that("serialize_atomic_payload errors on complex type", {
  expect_error(
    anon:::serialize_atomic_payload(complex(1)),
    "Unsupported class"
  )
})

test_that("serialize_atomic_payload errors on raw type", {
  expect_error(
    anon:::serialize_atomic_payload(raw(1)),
    "Unsupported class"
  )
})

# json_child_path ----------------------------------------------------------

test_that("json_child_path appends name when provided", {
  expect_equal(anon:::json_child_path("$", "foo"), "$$foo")
})

test_that("json_child_path appends index when no name", {
  expect_equal(anon:::json_child_path("$", index = 3), "$[3]")
})

test_that("json_child_path returns path unchanged when neither name nor index", {
  expect_equal(anon:::json_child_path("$"), "$")
})

test_that("json_child_path prefers name over index", {
  expect_equal(anon:::json_child_path("$", "foo", 1), "$$foo")
})

test_that("json_child_path falls back to index for NA name", {
  expect_equal(anon:::json_child_path("$", NA, 2), "$[2]")
})

test_that("json_child_path falls back to index for empty string name", {
  expect_equal(anon:::json_child_path("$", "", 2), "$[2]")
})

# unsupported_json_class_error ---------------------------------------------

test_that("unsupported_json_class_error includes path and class in message", {
  x <- structure(list(), class = c("foo", "bar"))

  expect_error(
    anon:::unsupported_json_class_error(x, path = "$.items[1]"),
    "Unsupported class.*\\$\\.items\\[1\\].*foo/bar"
  )
})
