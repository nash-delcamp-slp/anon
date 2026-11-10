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

test_that("anon() redacts haven value labels and missing-value codes with check_labels", {
  skip_if_not_installed("haven")
  df <- data.frame(id = 1:2)
  df$ARMN <- haven::labelled(c(1, 2), labels = c("DRUGQX arm" = 1, Placebo = 2))
  df$ARM <- haven::labelled(c("A", "B"), labels = c("DRUGQX arm" = "A", Other = "DRUGQX"))
  df$SITE <- haven::labelled_spss(c("S1", "S2"), na_values = "DRUGQX")
  # Two replacement values, so that text columns without a match keep their
  # attributes.
  patterns <- list("[DRUG]" = "DRUGQX", "[STUDY]" = "STUDYQX")

  result <- anon(df, pattern_list = patterns)
  expect_equal(attr(result$ARMN, "labels"), c("[DRUG] arm" = 1, Placebo = 2))
  expect_equal(attr(result$ARM, "labels"), c("[DRUG] arm" = "A", Other = "[DRUG]"))
  expect_equal(attr(result$SITE, "na_values"), "[DRUG]")
  expect_equal(vctrs::vec_data(result$ARMN), c(1, 2))

  kept <- anon(df, pattern_list = patterns, check_labels = FALSE)
  expect_equal(attr(kept$ARMN, "labels"), attr(df$ARMN, "labels"))
  expect_equal(attr(kept$ARM, "labels"), attr(df$ARM, "labels"))
  expect_equal(attr(kept$SITE, "na_values"), "DRUGQX")
})

test_that("anon() keeps the attributes of text columns, with one or two pattern groups", {
  skip_if_not_installed("haven")
  df <- data.frame(id = 1:2, TXT = c("DRUGQX given", "none"))
  attr(df$TXT, "label") <- "Text"
  comment(df$TXT) <- "free text"
  df$ARM <- haven::labelled(c("DRUGQX", "B"), labels = c("Active" = "DRUGQX"), label = "Arm")
  df$OTHER <- structure(c("x", "y"), label = "No match")
  for (patterns in list(list("[DRUG]" = "DRUGQX"), list("[DRUG]" = "DRUGQX", "[STUDY]" = "STUDYQX"))) {
    result <- anon(df, pattern_list = patterns)
    expect_equal(as.character(result$TXT), c("[DRUG] given", "none"))
    expect_equal(attr(result$TXT, "label"), "Text")
    expect_equal(comment(result$TXT), "free text")
    expect_s3_class(result$ARM, "haven_labelled")
    expect_equal(vctrs::vec_data(result$ARM), c("[DRUG]", "B"))
    expect_equal(attr(result$ARM, "labels"), c("Active" = "[DRUG]"))
    expect_equal(attr(result$ARM, "label"), "Arm")
    expect_equal(attr(result$OTHER, "label"), "No match")
  }
  named <- c(a = "DRUGQX", b = "x")
  expect_equal(names(anon(named, pattern_list = list("[DRUG]" = "DRUGQX"))), c("a", "b"))
})

test_that("anon() redacts comments and other text attributes with check_labels", {
  df <- data.frame(id = 1:2, x = 1:2)
  comment(df$x) <- "about DRUGQX"
  attr(df$x, "note") <- "STUDYQX note"
  attr(df$x, "format.sas") <- "DRUGQX"
  attr(df, "note") <- "STUDYQX data"
  patterns <- list("[DRUG]" = "DRUGQX", "[STUDY]" = "STUDYQX")
  result <- anon(df, pattern_list = patterns)
  expect_equal(comment(result$x), "about [DRUG]")
  expect_equal(attr(result$x, "note"), "[STUDY] note")
  expect_equal(attr(result$x, "format.sas"), "DRUGQX")
  expect_equal(attr(result, "note"), "[STUDY] data")
  kept <- anon(df, pattern_list = patterns, check_labels = FALSE)
  expect_equal(comment(kept$x), "about DRUGQX")
  expect_equal(attr(kept, "note"), "STUDYQX data")
})

test_that("anon() merges value labels whose codes redaction made equal, and redacts names and dimnames", {
  skip_if_not_installed("haven")
  df <- data.frame(id = 1:2)
  df$DRUGQX <- haven::labelled(c("q", "z"), labels = c(Study = "STUDYQX", Drug = "DRUGQX", c = "other DRUGQX"))
  patterns <- list("[X]" = c("DRUGQX", "STUDYQX"))
  # The warning names the column as it is named in the output.
  expect_warning(
    result <- anon(df, pattern_list = patterns),
    "^Value labels of `\\[X\\]` were merged because redaction made their codes equal\\.$"
  )
  expect_equal(attr(result[["[X]"]], "labels"), c("Study / Drug" = "[X]", c = "other [X]"))
  # The joined label is redacted, and duplicate label text is allowed.
  df <- data.frame(id = 1:2)
  df$A <- haven::labelled(c("q", "z"), labels = c("DRUGQX arm" = "DRUGQX", "DRUGQX arm" = "STUDYQX"))
  expect_warning(result <- anon(df, pattern_list = patterns), "Value labels of `A` were merged")
  expect_equal(attr(result$A, "labels"), c("[X] arm / [X] arm" = "[X]"))
  df$A <- haven::labelled(c("q", "z"), labels = c("DRUGQX arm" = "a", "STUDYQX arm" = "b"))
  expect_no_warning(result <- anon(df, pattern_list = patterns))
  expect_equal(attr(result$A, "labels"), c("[X] arm" = "a", "[X] arm" = "b"))
  # Outside a data frame, and missing-value codes made equal are deduplicated.
  v <- haven::labelled_spss(c("q", "z"), labels = c(a = "DRUGQX", b = "STUDYQX"), na_values = c("DRUGQX", "STUDYQX"))
  expect_warning(result <- anon(v, pattern_list = patterns), "Value labels of a vector were merged")
  expect_equal(attr(result, "labels"), c("a / b" = "[X]"))
  expect_equal(attr(result, "na_values"), "[X]")

  tb <- tibble::tibble(id = 1:2, v = c(DRUGQX = 1, b = 2))
  tb$m <- matrix(1:4, 2, dimnames = list(c("DRUGQX", "x"), c(k = "STUDYQX", "y")))
  result <- anon(tb, pattern_list = list("[DRUG]" = "DRUGQX", "[STUDY]" = "STUDYQX"))
  expect_equal(names(result$v), c("[DRUG]", "b"))
  expect_equal(dimnames(result$m), list(c("[DRUG]", "x"), c(k = "[STUDY]", "y")))
  expect_equal(names(anon(c(DRUGQX = "a"), pattern_list = list("[DRUG]" = "DRUGQX"))), "[DRUG]")
  expect_equal(names(anon(c(DRUGQX = 1), pattern_list = list("[DRUG]" = "DRUGQX"))), "[DRUG]")
  kept <- anon(tb, pattern_list = list("[DRUG]" = "DRUGQX"), check_names = FALSE)
  expect_equal(names(kept$v), c("DRUGQX", "b"))
})

test_that("anon() redacts SPSS user-defined missing values like other values", {
  skip_if_not_installed("haven")
  df <- data.frame(id = 1:3)
  df$A <- haven::labelled_spss(c("DRUGQX a", "b", "c"), na_range = c("D", "E"))
  df$B <- haven::labelled_spss(c("DRUGQX", "x", "y"), na_values = "DRUGQX")
  # "DRUGQX a" was in the range D to E; "[DRUG] a" is not.
  expect_warning(
    result <- anon(df, pattern_list = list("[DRUG]" = "DRUGQX")),
    "^Redaction changed which values of `A` are user-defined missing values"
  )
  expect_equal(vctrs::vec_data(result$A), c("[DRUG] a", "b", "c"))
  expect_equal(vctrs::vec_data(result$B), c("[DRUG]", "x", "y"))
  expect_equal(attr(result$B, "na_values"), "[DRUG]")
  expect_equal(is.na(result$B), c(TRUE, FALSE, FALSE))
  # The ends of a text range are redacted too.
  df <- data.frame(id = 1:3)
  df$R <- haven::labelled_spss(c("DRUGQX a", "b", "c"), na_range = c("DRUGQX", "DRUGQXz"))
  expect_no_warning(result <- anon(df, pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_equal(attr(result$R, "na_range"), c("[DRUG]", "[DRUG]z"))
  expect_equal(is.na(result$R), c(TRUE, FALSE, FALSE))
})

test_that("anon() redacts value labels and labels of vectors outside data frames", {
  skip_if_not_installed("haven")
  v <- haven::labelled(c(1, 2), labels = c("DRUGQX arm" = 1), label = "DRUGQX lab")
  patterns <- list("[DRUG]" = "DRUGQX")
  for (out in list(anon(v, pattern_list = patterns), anon(list(a = v), pattern_list = patterns)$a)) {
    expect_equal(attr(out, "labels"), c("[DRUG] arm" = 1))
    expect_equal(attr(out, "label"), "[DRUG] lab")
  }
  d <- data.frame(id = 1:2)
  d$L <- I(list(v, v))
  out <- anon(d, pattern_list = patterns)$L[[2]]
  expect_equal(attr(out, "labels"), c("[DRUG] arm" = 1))
  kept <- anon(v, pattern_list = patterns, check_labels = FALSE)
  expect_equal(attr(kept, "labels"), c("DRUGQX arm" = 1))
})

test_that("anon() leaves text that can't be searched as it is, with a warning", {
  b <- "DRUGQX caf\u00e9"
  Encoding(b) <- "bytes"
  expect_warning(out <- anon(c(b, "DRUGQX"), pattern_list = list("[DRUG]" = "DRUGQX")), "can't be searched")
  expect_identical(as.character(out), c(b, "[DRUG]"))
})

test_that("anon() renames a data.table's key and indices with its columns, only with check_names", {
  dt <- data.frame(DRUGQX = 1:2, b = 2:1)
  attr(dt, "sorted") <- "DRUGQX"
  attr(dt, "index") <- structure(integer(0), "__b__DRUGQX" = 2:1)
  class(dt) <- c("data.table", "data.frame")
  out <- anon(dt, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_equal(attr(out, "sorted"), "[DRUG]")
  expect_equal(names(attributes(attr(out, "index"))), "__b__[DRUG]")
  kept <- anon(dt, pattern_list = list("[DRUG]" = "DRUGQX"), check_names = FALSE)
  expect_named(kept, c("DRUGQX", "b"))
  expect_equal(attr(kept, "sorted"), "DRUGQX")
  expect_equal(names(attributes(attr(kept, "index"))), "__b__DRUGQX")
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

  expect_equal(anon:::nlp_text_source(c("a", "b"), "id", "double"), c("a", "b"))
})

test_that("numbered backreferences in user patterns keep their meaning", {
  result <- anon(c("112", "123"), pattern_list = list(X = "([0-9])\\1"))
  expect_equal(as.character(result), c("X2", "123"))

  result <- anon(
    c("foo 112", "abab 99"),
    pattern_list = list(A = "foo", X = c("(ab)\\1", "([0-9])\\1"))
  )
  expect_equal(as.character(result), c("A X2", "X X"))

  expect_equal(
    anon:::shift_backreferences("([0-9])\\1 \\\\1", 3L),
    "([0-9])\\4 \\\\1"
  )

  # An escaped backslash before a digit is not a backreference
  expect_equal(anon:::shift_backreferences("\\\\1", 1L), "\\\\1")
  result <- anon(c("a\\1b", "zzz"), pattern_list = list(X = "\\\\1"))
  expect_equal(as.character(result), c("aXb", "zzz"))
})

test_that("TIME pattern respects whole-value boundaries", {
  result <- anon(
    c("A08:30B", "ID1T08:30", "08:30", "2021-03-14T08:30", "08:30Z", "1.08:30"),
    pattern_list = list("[TIME]" = anon:::iso8601_patterns("TIME"))
  )
  expect_equal(
    as.character(result),
    c("A08:30B", "ID1T08:30", "[TIME]", "2021-03-14T[TIME]", "[TIME]", "1.08:30")
  )
})

test_that("POSIXct matching handles ISO offsets and rejects invalid values", {
  times <- anon:::parse_iso8601_datetime(
    c("2021-03-14", "2021-03-14T08:00", "2021-03-14T08:00:00Z",
      "2021-03-14T08:00:00.5+02:00", "2021-03-14T08:00-0500", "2021-99-99T08:00")
  )
  expect_equal(
    format(times, "%Y-%m-%d %H:%M:%OS1"),
    c("2021-03-14 00:00:00.0", "2021-03-14 08:00:00.0", "2021-03-14 08:00:00.0",
      "2021-03-14 06:00:00.5", "2021-03-14 13:00:00.0", NA)
  )

  df <- data.frame(
    tz = c("2021-03-14T08:00:00Z", "2021-03-14T08:00:00.5+02:00"),
    bad = c("2021-99-99T08:00", "2021-03-14"),
    stringsAsFactors = FALSE
  )
  result <- anon(df, df_classes = "POSIXct", default_replacement = "X")
  expect_equal(result$tz, c("X", "X"))
  expect_equal(result$bad, df$bad)
})

test_that("df_variable_names and df_classes combine with their global options", {
  old <- options(anon.df_variable_names = "b", anon.df_classes = "integer")
  on.exit(options(old), add = TRUE)

  df <- data.frame(a = "x", b = "y", c = 1L, d = 2.5, stringsAsFactors = FALSE)

  result <- anon(df, default_replacement = "R")
  expect_equal(unlist(unclass(result)), c(a = "x", b = "R", c = "R", d = "2.5"))

  result <- anon(df, df_variable_names = "a", df_classes = "double", default_replacement = "R")
  expect_equal(unlist(unclass(result)), c(a = "R", b = "R", c = "R", d = "R"))
})

test_that("approximate-match warnings from list columns reach the caller", {
  df <- data.frame(id = 1)
  df$notes <- list("Alise Smith")
  expect_warning(
    anon(df, pattern_list = "Alice Smith", check_approximate = TRUE),
    "Potential approximate match"
  )
})

test_that("compute_approximate_distances() returns empty results when nothing is close", {
  empty <- list(distances = integer(0), matches = integer(0), matching_strings = character(0))
  expect_equal(compute_approximate_distances("completely different", "Alice Smith"), empty)
  expect_equal(compute_approximate_distances("x", "abc"), empty)

  # agrepl() finds a candidate, but the stricter distance check rejects it
  expect_equal(compute_approximate_distances("Alice Smith and more", "Alice Smyth", max_distance = 1), empty)

  result <- compute_approximate_distances(c("Alise Smith", "Bob"), "Alice Smith")
  expect_equal(result$matching_strings, "Alise Smith")
})

test_that("anon() treats POSIXlt values as values, not lists to recurse into", {
  lt <- as.POSIXlt(c("2021-03-14 08:00:00", "2021-03-15 09:00:00"), tz = "UTC")

  expect_equal(unclass(anon(lt)), unclass(lt))
  result <- anon(list(t = lt, note = "Alice"), pattern_list = list(P = "Alice"))
  expect_equal(result$t, lt)
  expect_equal(result$note, "P")
})

test_that("anon() recurses into nested anon_context objects", {
  x <- anon(list(child = anon("Alice")))
  result <- anon(x, pattern_list = list(PERSON = "Alice"))
  expect_equal(as.character(unclass(result)$child), "PERSON")

  y <- c(c(anon("Alice"), anon("Bob")), anon("Dan"))
  expect_setequal(collect_text(y), c("Alice", "Bob", "Dan"))
  result <- anon(y, pattern_list = list(PERSON = c("Alice", "Bob", "Dan")))
  expect_false(any(grepl("Alice|Bob|Dan", unlist(unclass(result)))))
})

test_that("is_container_list() separates containers from list-based value classes", {
  lt <- as.POSIXlt(c("2021-03-14", "2021-03-15"), tz = "UTC")
  expect_false(is_container_list(lt))
  expect_false(is_container_list("a"))
  expect_true(is_container_list(list(1)))
  expect_true(is_container_list(anon(list(child = anon("Alice")))))
  expect_true(is_container_list(c(c(anon("a"), anon("b")), anon("c"))))
  expect_true(is_container_list(data.frame(a = 1)))
  expect_true(is_container_list(structure(list(), class = "custom")))
})

test_that("anon_inventory() detects text in nested anon_context objects", {
  inventory <- anon_inventory(list(x = anon(list(child = anon("Alice")))))
  expect_true(inventory$has_text)
})

test_that("anon() names list elements in warnings, and warns on missingness changes without check_labels", {
  skip_if_not_installed("haven")
  d <- data.frame(id = 1:2)
  v <- haven::labelled_spss(c("DrugA x", "b"), na_range = c("A", "E"))
  d$L <- I(list(v, 1))
  expect_warning(anon(d, pattern_list = list("[D]" = "DrugA")), "values of `L\\[1\\]` are user-defined missing")
  expect_warning(anon(v, pattern_list = list("[D]" = "DrugA"), check_labels = FALSE), "values of a vector are user-defined missing")
  expect_warning(anon(list(a = v), pattern_list = list("[D]" = "DrugA")), "values of `a` are user-defined missing")
})

test_that("anon() keeps a missing-value range whose redacted ends would descend, with a warning", {
  skip_if_not_installed("haven")
  # Ends are compared as R compares text (testthat uses the C collation).
  v <- haven::labelled_spss(c("B", "C"), na_range = c("DrugA", "E"))
  expect_true("[D]" > "E")
  expect_warning(out <- anon(v, pattern_list = list("[D]" = "DrugA")), "range of a vector was not redacted")
  expect_equal(attr(out, "na_range"), c("DrugA", "E"))
})

test_that("anon() joins the original labels of merged value labels and redacts them once", {
  skip_if_not_installed("haven")
  v <- haven::labelled(c("q", "z"), labels = c("Drug" = "DRUGQX", "X" = "STUDYQX"))
  # A pattern that would match "[X]" in an already redacted label.
  expect_warning(out <- anon(v, pattern_list = list("[X]" = c("DRUGQX", "STUDYQX", "\\[X\\]"))), "merged")
  expect_equal(names(attr(out, "labels")), "Drug / X")
})

test_that("anon() handles column names marked as bytes, and structural attributes by class", {
  b <- "café DrugA"
  Encoding(b) <- "bytes"
  d <- data.frame(id = 1:2, v = c("DrugA", "y"))
  names(d)[2] <- b
  out <- anon(d, pattern_list = list("[D]" = "DrugA"))
  expect_identical(Encoding(names(out)[2]), "bytes")
  expect_false(grepl("DrugA", names(out)[2], useBytes = TRUE))
  expect_equal(.subset2(out, 2), c("[D]", "y"))
  d <- data.frame(n = 1:2, f = factor(c("a", "b")))
  attr(d$n, "units") <- "DrugA mg"
  stats::contrasts(d$f) <- stats::contr.sum(2)
  out <- anon(d, pattern_list = list("[D]" = "DrugA"))
  expect_equal(attr(out$n, "units"), "[D] mg")
  expect_equal(attr(out$f, "contrasts"), attr(d$f, "contrasts"))
})

test_that("anon() returns list elements with nothing to redact as they are, and keeps list names", {
  l <- list(a = 1, b = structure("DrugA", label = "DrugA"), c = list(x = "DrugA"))
  out <- anon(l, pattern_list = list("[D]" = "DrugA"))
  expect_named(out, c("a", "b", "c"))
  expect_identical(out$a, 1)
  expect_equal(attr(out$b, "label"), "[D]")
  expect_equal(as.character(out$b), "[D]")
  expect_equal(out$c$x, "[D]")
})
