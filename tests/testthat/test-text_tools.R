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

test_that("anon_clean_text() keeps NA values", {
  expect_equal(anon_clean_text(c("a  b", NA)), c("a b", NA_character_))
})

test_that("parse_pattern_rule_text() ignores empty, comment and incomplete lines", {
  expect_equal(parse_pattern_rule_text(""), list())
  expect_equal(parse_pattern_rule_text(NULL), list())
  expect_equal(parse_pattern_rule_text("# only a comment\n\n"), list())
  expect_equal(
    parse_pattern_rule_text("= Alice\nPERSON = Bob\nX =  "),
    list(PERSON = "Bob")
  )
})

test_that("format_pattern_rule_text() accepts vectors and skips empty entries", {
  expect_equal(format_pattern_rule_text(c("Alice", "Bob")), "Alice\nBob")
  expect_equal(
    format_pattern_rule_text(list(PERSON = c("Alice", ""), EMPTY = "", "Bob")),
    "PERSON = Alice\nBob"
  )
})
