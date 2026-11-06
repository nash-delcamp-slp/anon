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

test_that("anon() performs basic replacements without runtime dependency failures", {
  result <- anon(c("Alice", "Bob"), pattern_list = c("Alice"))

  expect_s3_class(result, "anon_context")
  expect_equal(as.character(result), c("[REDACTED]", "Bob"))
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

test_that("later pattern groups cannot match inside earlier replacements", {
  # Hex-like fragments and greedy regexes used to corrupt the internal tokens
  # that stood in for earlier replacements.
  result <- anon(
    c("MIRACLE", "MIRACLE Metabolite"),
    pattern_list = list(DRUG = "MIRACLE", "[DATE]" = c("10", "05", "f7c", "[0-9]{2}"))
  )
  expect_equal(as.character(result), c("DRUG", "DRUG Metabolite"))

  result <- anon("MIRACLE", pattern_list = list(DRUG = "MIRACLE", "[X]" = "\\S+"))
  expect_equal(as.character(result), "DRUG")
})

test_that("invalid regex patterns are matched literally", {
  result <- anon("call a(b now", pattern_list = list(X = "a(b"))
  expect_equal(as.character(result), "call X now")
})

test_that("whole-value patterns only match complete values", {
  pats <- anon:::whole_value_patterns(c("2021", "03", "Day 1"))
  result <- anon(
    c("2021", "ab2021cd", "42.03", "2021-x", "Day 1", "Day   1", "Day11"),
    pattern_list = list("[D]" = pats)
  )
  expect_equal(
    as.character(result),
    c("[D]", "ab2021cd", "42.03", "[D]-x", "[D]", "[D]", "Day11")
  )
})

test_that("compile_patterns() shares boundaries without changing matches", {
  pats <- anon:::whole_value_patterns(c("abc", "ab"))
  compiled <- anon:::compile_patterns(list(c(pats[1], "[A]"), c(pats[2], "[A]")))
  expect_equal(
    anon:::replace_compiled(c("abc", "ab", "abd", "x ab."), compiled),
    c("[A]", "[A]", "abd", "x [A].")
  )
})

test_that("ISO 8601 patterns match dates and times but not bare years", {
  result <- anon(
    c("2021-03-14T08:00", "x 2021-03 y", "2024MARK", "2024", "08:30",
      "at 8:30:15", "PT0.5H", "2021-03-14T08:00:00Z"),
    pattern_list = list(
      "[DATE]" = anon:::iso8601_patterns("DATE"),
      "[TIME]" = anon:::iso8601_patterns("TIME")
    )
  )
  expect_equal(
    as.character(result),
    c("[DATE]", "x [DATE] y", "2024MARK", "2024", "[TIME]", "at [TIME]",
      "PT0.5H", "[DATE]")
  )
})

test_that("df_classes 'double' matches plain doubles only", {
  df <- data.frame(
    d = 1.5,
    i = 2L,
    dt = as.Date("2021-01-01"),
    stringsAsFactors = FALSE
  )
  result <- anon(df, df_classes = "double", default_replacement = "X")
  expect_equal(result$d, "X")
  expect_equal(result$i, 2L)
  expect_equal(result$dt, as.Date("2021-01-01"))
})

test_that("df_classes matches character columns whose values all parse", {
  df <- data.frame(
    num = c("24.13", " 8.7", "", NA),
    mixed = c("24.13", "<LLOQ", "", NA),
    int = c("1", "20", "", NA),
    date = c("2021-03-14", "2021-03-15", "", NA),
    dtc = c("2021-03-14T08:00", "2021-03-15", "", NA),
    blank = c("", "", "", NA),
    stringsAsFactors = FALSE
  )

  result <- anon(df, df_classes = "double", default_replacement = "X")
  expect_equal(result$num, rep("X", 4))
  expect_equal(result$int, rep("X", 4))
  expect_equal(result$mixed, df$mixed)
  expect_equal(result$blank, df$blank)

  result <- anon(df, df_classes = "integer", default_replacement = "X")
  expect_equal(result$int, rep("X", 4))
  expect_equal(result$num, df$num)

  result <- anon(df, df_classes = c("Date", "POSIXct"), default_replacement = "X")
  expect_equal(result$date, rep("X", 4))
  expect_equal(result$dtc, rep("X", 4))
  expect_equal(result$num, df$num)
})

test_that("df_classes functions receive parsed values for character columns", {
  df <- data.frame(conc = c("1.5", "2.5", NA), stringsAsFactors = FALSE)
  result <- anon(df, df_classes = list(numeric = ~ .x * 2))
  expect_equal(result$conc, c(3, 5, NA))
})

test_that("NLP text source skips columns that are replaced wholesale", {
  df <- data.frame(
    id = "0001",
    conc = 1.5,
    note = "seen on 2021-03-14",
    stringsAsFactors = FALSE
  )
  src <- anon:::nlp_text_source(df, c("id"), "double")
  expect_named(src, "note")

  src <- anon:::nlp_text_source(list(a = df), list(id = "X"), NULL)
  expect_named(src$a, c("conc", "note"))
})
