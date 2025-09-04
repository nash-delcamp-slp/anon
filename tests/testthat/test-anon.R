test_that("anon() works with character vectors", {
  text <- c("John Smith", "jane.doe@email.com", "Call 555-1234")

  # Basic pattern matching
  result <- anon(text, pattern_list = c("John Smith", "@\\S+", "\\d{3}-\\d{4}"))
  expect_equal(as.character(result), c("[REDACTED]", "jane.doe[REDACTED]", "Call [REDACTED]"))

  # Named patterns with custom replacements
  result2 <- anon(text, pattern_list = list("PERSON" = "John Smith",
                                            "EMAIL" = "@\\S+",
                                            "PHONE" = "\\d{3}-\\d{4}"))
  expect_equal(as.character(result2), c("PERSON", "jane.doeEMAIL", "Call PHONE"))

  # Named patterns with different kinds of replacements
  result3 <- anon(text, pattern_list = list("FIRST" = c("John", "Jane"),
                                            "EMAIL" = "@\\S+",
                                            "\\d{3}-\\d{4}"))
  expect_equal(as.character(result3), c("FIRST Smith", "FIRST.doeEMAIL", "Call [REDACTED]"))
})

test_that("anon() works with factors", {
  factor_data <- factor(c("Alice", "Bob", "Alice", "Charlie"))

  result <- anon(factor_data, pattern_list = c("Alice", "Bob"))
  expect_equal(levels(result), c("[REDACTED]", "Charlie"))
  expect_equal(as.character(result), c("[REDACTED]", "[REDACTED]", "[REDACTED]", "Charlie"))
})

test_that("anon() works with data frames - basic functionality", {
  df <- data.frame(
    name = c("Alice", "Bob"),
    email = c("alice@test.com", "bob@test.com"),
    score = c(95, 87)
  )

  # Pattern-based anonymization
  result <- anon(df, pattern_list = c("Alice", "@test\\.com"))
  expect_equal(as.character(result$name), c("[REDACTED]", "Bob"))
  expect_equal(as.character(result$email), c("[REDACTED][REDACTED]", "bob[REDACTED]"))
  expect_equal(result$score, c(95, 87))
})

test_that("anon() works with df_variable_names parameter", {
  df <- data.frame(
    name = c("Alice", "Bob"),
    email = c("alice@test.com", "bob@test.com"),
    score = c(95, 87)
  )

  # Anonymize specific columns by name
  result <- anon(df, df_variable_names = c("name", "email"))
  expect_equal(as.character(result$name), rep("[REDACTED]", 2))
  expect_equal(as.character(result$email), rep("[REDACTED]", 2))
  expect_equal(result$score, c(95, 87))

  # Named replacements
  result2 <- anon(df, df_variable_names = list("name" = "PERSON", "email" = "ADDRESS"))
  expect_equal(as.character(result2$name), rep("PERSON", 2))
  expect_equal(as.character(result2$email), rep("ADDRESS", 2))
  expect_equal(result2$score, c(95, 87))

  # combination of replacements
  result3 <- anon(df, df_variable_names = list("name", "email" = "ADDRESS"))
  expect_equal(as.character(result3$name), rep("[REDACTED]", 2))
  expect_equal(as.character(result3$email), rep("ADDRESS", 2))
  expect_equal(result3$score, c(95, 87))
})

test_that("anon() works with df_classes parameter", {
  df <- data.frame(
    name = c("Alice", "Bob"),
    email = c("alice@test.com", "bob@test.com"),
    score = c(95, 87)
  )

  # Anonymize by class
  result <- anon(df, df_classes = "character")
  expect_equal(as.character(result$name), rep("[REDACTED]", 2))
  expect_equal(as.character(result$email), rep("[REDACTED]", 2))
  expect_equal(result$score, c(95, 87))

  # Named class replacements
  result2 <- anon(df, df_classes = list("character" = "HIDDEN"))
  expect_equal(as.character(result2$name), rep("HIDDEN", 2))
  expect_equal(as.character(result2$email), rep("HIDDEN", 2))
  expect_equal(result2$score, c(95, 87))

  # combination of replacements
  result3 <- anon(df, df_classes = list("character" = "HIDDEN", "numeric"))
  expect_equal(as.character(result3$name), rep("HIDDEN", 2))
  expect_equal(as.character(result3$email), rep("HIDDEN", 2))
  expect_equal(as.character(result3$score), rep("[REDACTED]", 2))
})

test_that("anon() works with function replacements", {
  df <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    score = c(95, 87, 92)
  )

  # Function replacement using formula
  result <- anon(df, df_variable_names = list("name" = ~ paste("Person", seq_along(.x))))
  expect_equal(as.character(result$name), c("Person 1", "Person 2", "Person 3"))
  expect_equal(result$score, c(95, 87, 92))

  # Function replacement using actual function
  result2 <- anon(df, df_variable_names = list("name" = function(x) paste("ID", seq_along(x))))
  expect_equal(as.character(result2$name), c("ID 1", "ID 2", "ID 3"))
})

test_that("anon() works with lists", {
  test_list <- list(
    names = c("Alice", "Bob"),
    emails = c("alice@test.com", "bob@test.com"),
    nested = list(person = "Charlie", contact = "charlie@example.com")
  )

  result <- anon(test_list, pattern_list = c("Alice", "@\\S+"))
  expect_equal(as.character(result$names), c("[REDACTED]", "Bob"))
  expect_equal(as.character(result$emails), c("[REDACTED][REDACTED]", "bob[REDACTED]"))
  expect_equal(as.character(result$nested$person), "Charlie")
  expect_equal(as.character(result$nested$contact), "charlie[REDACTED]")
})

test_that("anon() handles names and labels correctly", {
  df <- data.frame(
    sensitive_name = c("Alice", "Bob"),
    score = c(95, 87)
  )
  colnames(df) <- c("sensitive_name", "score")
  rownames(df) <- c("row_sensitive", "row2")
  attr(df, "label") <- "sensitive data label"
  attr(df$score, "label") <- "sensitive score label"

  # With name and label checking enabled (default)
  result <- anon(df, pattern_list = c("sensitive"))
  expect_true(grepl("\\[REDACTED\\]", colnames(result)[1]))
  expect_true(grepl("\\[REDACTED\\]", rownames(result)[1]))
  expect_true(grepl("\\[REDACTED\\]", attr(result, "label")))
  expect_true(grepl("\\[REDACTED\\]", attr(result$score, "label")))

  # With name checking disabled
  result2 <- anon(df, pattern_list = c("sensitive"), check_names = FALSE)
  expect_equal(colnames(result2)[1], "sensitive_name")
  expect_equal(rownames(result2)[1], "row_sensitive")

  # With label checking disabled
  result3 <- anon(df, pattern_list = c("sensitive"), check_labels = FALSE)
  expect_equal(attr(result3, "label"), "sensitive data label")
  expect_equal(attr(result3$score, "label"), "sensitive score label")
})

test_that("anon() handles empty and edge cases", {
  # Empty character vector
  result <- anon(character(0), pattern_list = c("test"))
  expect_equal(as.character(result), character(0))

  # Empty data frame
  empty_df <- data.frame()
  result <- anon(empty_df, pattern_list = c("test"))
  expect_equal(as.data.frame(result), empty_df)

  # No patterns
  text <- c("Alice", "Bob")
  result <- anon(text, pattern_list = list())
  expect_equal(as.character(result), text)

  # Unsupported object type
  result <- anon(123, pattern_list = c("test"))
  expect_equal(as.numeric(result), 123)
})

test_that("anon() approximate matching works", {
  text <- c("Jon Smith", "Alice", "Alise") # "Alise" is close to "Alice"

  # With approximate matching enabled, should warn about "Alise"
  expect_warning(
    result <- anon(text, pattern_list = c("Alice"), check_approximate = TRUE, max_distance = 2),
    "Potential approximate match.*Alise.*similar to pattern.*Alice"
  )
  expect_equal(as.character(result), c("Jon Smith", "[REDACTED]", "Alise"))

  # With approximate matching disabled, should not warn
  expect_silent(
    result2 <- anon(text, pattern_list = c("Alice"), check_approximate = FALSE)
  )
  expect_equal(as.character(result2), c("Jon Smith", "[REDACTED]", "Alise"))
})

test_that("anon() custom default replacement works", {
  text <- c("Alice", "Bob")

  result <- anon(text, pattern_list = c("Alice"), default_replacement = "[HIDDEN]")
  expect_equal(as.character(result), c("[HIDDEN]", "Bob"))
})

test_that("anon() case insensitive matching works", {
  text <- c("alice", "ALICE", "Alice", "Bob")

  result <- anon(text, pattern_list = c("Alice"))
  expect_equal(as.character(result), c(rep("[REDACTED]", 3), "Bob"))
})

test_that("anon() with mixed named and unnamed patterns", {
  text <- c("Alice", "Bob", "secret@email.com", "public info")

  result <- anon(text,
                 pattern_list = list(
                   "PERSON" = "Alice",          # Named - replace with "PERSON"
                   "Bob",                       # Unnamed - replace with default
                   "EMAIL" = "@\\S+"            # Named - replace with "EMAIL"
                 ))
  expect_equal(as.character(result), c("PERSON", "[REDACTED]", "secretEMAIL", "public info"))
})

test_that("with_default_replacements helper works correctly", {
  # Test the helper function directly
  result1 <- with_default_replacements(
    pattern_list = list("REPLACE" = "pattern1", "pattern2"),
    default_replacement = "DEFAULT"
  )

  expect_equal(length(result1), 2)
  expect_equal(result1[[1]], c("pattern1", "REPLACE"))
  expect_equal(result1[[2]], c("pattern2", "DEFAULT"))
})

test_that("compute_approximate_distances helper works correctly", {
  # Test the helper function directly
  result <- compute_approximate_distances(c("Alice", "Alise", "Bob"), "Alice", max_distance = 2)

  expect_true(length(result$matching_strings) > 0)
  expect_true("Alise" %in% result$matching_strings)
  expect_false("Bob" %in% result$matching_strings)

  result_empty_pattern <- compute_approximate_distances(c("Alice", "Alise", "Bob"), "", max_distance = 2)
  expect_equal(result_empty_pattern$matching_strings, character(0))
})
