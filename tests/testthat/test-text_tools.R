test_that("anon_clean_text() normalizes whitespace and blank lines", {
  text <- "  Alice   visited\r\n\r\n\r\nBoston  "

  result <- anon_clean_text(text)

  expect_equal(result, "Alice visited\n\nBoston")
})

test_that("parse_pattern_rule_text() parses replacement rules", {
  rules <- parse_pattern_rule_text(
    "PERSON = Alice | Bob\n# comment\nSITE = Boston Medical Center"
  )

  expect_equal(rules$PERSON, c("Alice", "Bob"))
  expect_equal(rules$SITE, "Boston Medical Center")
})
