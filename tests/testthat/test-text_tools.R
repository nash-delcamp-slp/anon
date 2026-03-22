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

test_that("parse_pattern_rule_text() supports unnamed default-replacement lines", {
  rules <- parse_pattern_rule_text("secret phrase\nTOKEN = alpha | beta")

  expect_equal(rules[[1]], "secret phrase")
  expect_equal(rules$TOKEN, c("alpha", "beta"))
})

test_that("format_pattern_rule_text() formats named and unnamed patterns", {
  text <- format_pattern_rule_text(list("MASK" = c("Alice", "Bob"), "secret phrase"))

  expect_match(text, "MASK = Alice | Bob", fixed = TRUE)
  expect_match(text, "secret phrase", fixed = TRUE)
})
