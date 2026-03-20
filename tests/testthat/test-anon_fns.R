test_that("anon_date_shift() preserves POSIXct timezone metadata", {
  x <- as.POSIXct(
    c("2023-01-01 12:00:00", NA, "2023-01-03 18:30:00"),
    tz = "UTC"
  )

  result <- anon_date_shift(x, center_date = "2024-01-01", scramble = TRUE)

  expect_s3_class(result, "POSIXct")
  expect_identical(attr(result, "tzone"), "UTC")
  expect_equal(which(is.na(result)), 2)
})

test_that("anon_num_preserve_distribution() handles degenerate numeric inputs", {
  expect_equal(anon_num_preserve_distribution(5, method = "rank"), 5)
  expect_equal(anon_num_preserve_distribution(5, method = "noise"), 5)
  expect_equal(
    anon_num_preserve_distribution(5, method = "quantile"),
    5
  )
  expect_equal(
    anon_num_preserve_distribution(c(5, 5, NA), method = "noise"),
    c(5, 5, NA)
  )
})

test_that("anon_num_range() handles constant vectors across break strategies", {
  expected <- c("[1,1]", "[1,1]", "[1,1]")

  expect_equal(
    anon_num_range(c(1, 1, 1), method = "equal_width", clean_breaks = FALSE),
    expected
  )
  expect_equal(
    anon_num_range(c(1, 1, 1), method = "equal_count", clean_breaks = TRUE),
    expected
  )
  expect_equal(
    anon_num_range(
      c(1, 1, 1),
      method = "equal_count",
      clean_breaks = FALSE,
      keep_values = FALSE
    ),
    rep("Range_01", 3)
  )
})

test_that("more_patterns() splits punctuation-delimited tokens", {
  result <- more_patterns("John.Smith")

  expect_true("John" %in% result)
  expect_true("Smith" %in% result)
})
