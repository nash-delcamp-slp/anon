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

test_that("anon() redacts text that isn't valid UTF-8, and skips only bytes it can't convert", {
  patterns <- list("[DRUG]" = "DRUGQX")
  # A stray Windows-1252 byte: searched through a repaired copy, which is
  # written back where a pattern matched.
  out <- anon(c("DRUGQX\x92s note", "x\x92y", "\u00e9 DRUGQX"), pattern_list = patterns)
  expect_identical(as.character(out), c("[DRUG]<92>s note", "x\x92y", "\u00e9 [DRUG]"))
  v <- structure("a", label = "DRUGQX\x92s label")
  expect_identical(attr(anon(v, pattern_list = patterns), "label"), "[DRUG]<92>s label")
  # Marked "bytes": searched if valid UTF-8, otherwise left with a warning,
  # also next to non-ASCII UTF-8 text.
  valid <- "DRUGQX caf\u00e9"
  Encoding(valid) <- "bytes"
  invalid <- "DRUGQX caf\xe9"
  Encoding(invalid) <- "bytes"
  expect_warning(
    out <- anon(c(valid, invalid, "\u00e9 DRUGQX", "DRUGQX"), pattern_list = patterns),
    "marked as \"bytes\" and isn't valid UTF-8"
  )
  expect_identical(as.character(out)[c(1, 3, 4)], c("[DRUG] caf\u00e9", "\u00e9 [DRUG]", "[DRUG]"))
  expect_identical(as.character(out)[2], invalid)
})

test_that("anon() handles text of 10,000 bytes or more in lists", {
  long <- paste(c("DRUGQX", rep("x", 6000)), collapse = " ")
  out <- anon(list(a = long, b = "DRUGQX", c = c(long, "y")), pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_true(startsWith(out$a, "[DRUG] x"))
  expect_equal(out$b, "[DRUG]")
  expect_equal(out$c[2], "y")
})

test_that("anon() names list elements by their redacted names, and redacts a data frame's own labels", {
  skip_if_not_installed("haven")
  patterns <- list("[X]" = c("DRUGQX", "STUDYQX"))
  l <- list(DRUGQX_vals = haven::labelled(c("a", "b"), labels = c(a = "DRUGQX", b = "STUDYQX")), z = 1)
  expect_warning(out <- anon(l, pattern_list = patterns), "Value labels of `\\[X\\]_vals` were merged")
  expect_named(out, c("[X]_vals", "z"))
  expect_warning(anon(l, pattern_list = patterns, check_names = FALSE), "Value labels of `DRUGQX_vals` were merged")
  df <- data.frame(A = 1:2)
  attr(df, "labels") <- c(a = "DRUGQX study")
  attr(df, "na_range") <- "DRUGQX"
  out <- anon(df, pattern_list = patterns)
  expect_equal(attr(out, "labels"), c(a = "[X] study"))
  expect_equal(attr(out, "na_range"), "[X]")
})

test_that("anon() redacts the names of a factor's contrasts with its levels", {
  f <- data.frame(ARM = factor(c("DRUGQX", "Placebo")))
  stats::contrasts(f$ARM) <- stats::contr.treatment
  out <- anon(f, pattern_list = list("[D]" = "DRUGQX"))
  expect_equal(levels(out$ARM), c("[D]", "Placebo"))
  expect_equal(rownames(attr(out$ARM, "contrasts")), c("[D]", "Placebo"))
  out <- anon(f$ARM, pattern_list = list("[D]" = "DRUGQX"))
  expect_equal(rownames(attr(out, "contrasts")), c("[D]", "Placebo"))
})

test_that("anon() batch redaction warns about approximate matches only in what it redacts", {
  a <- structure(c("ok", "fine"), label = "label")
  b <- c(x = "fine", DRUGQY = "ok")
  l <- list(a = a, b = b)
  patterns <- list("[D]" = "DRUGQX")
  expect_warning(anon(l, pattern_list = patterns, check_approximate = TRUE), "DRUGQY")
  expect_no_warning(anon(l, pattern_list = patterns, check_approximate = TRUE, check_names = FALSE))
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

test_that("anon() leaves unconvertible bytes text alone next to text it changes", {
  invalid <- "Xanomeline\xff"
  Encoding(invalid) <- "bytes"
  out <- suppressWarnings(anon(c(invalid, "Café Xanomeline"), pattern_list = list("[X]" = "Xanomeline")))
  expect_identical(as.character(out), c(invalid, "Café [X]"))
  df <- data.frame(id = 1:2)
  df$NOTE <- c(invalid, "Café Xanomeline")
  expect_warning(out <- anon(df, pattern_list = list("[X]" = "Xanomeline")), "marked as \"bytes\"")
  expect_identical(out$NOTE[2], "Café [X]")
})

test_that("anon() looks for approximate matches in text that isn't valid UTF-8", {
  # Invalid text is searched repaired, next to a near-match, without an error.
  expect_warning(
    out <- anon(c("Xanomelin", "Alz\x92 Xanomeline"), pattern_list = list("[X]" = "Xanomeline"), check_approximate = TRUE),
    "approximate match: 'Xanomelin'"
  )
  expect_identical(as.character(out)[2], "Alz<92> [X]")
})

test_that("anon() redacts the names of character attributes", {
  df <- data.frame(Xanomeline = 1:2, b = c("a", "b"))
  attr(df, "variable.labels") <- c(Xanomeline = "Xanomeline dose", b = "B")
  attr(df$b, "note") <- c(Xanomeline = "x")
  out <- anon(df, pattern_list = list("[X]" = "Xanomeline"))
  expect_equal(attr(out, "variable.labels"), c("[X]" = "[X] dose", b = "B"))
  expect_equal(attr(out$b, "note"), c("[X]" = "x"))
  kept <- anon(df, pattern_list = list("[X]" = "Xanomeline"), check_labels = FALSE)
  expect_equal(attr(kept, "variable.labels"), attr(df, "variable.labels"))
})

test_that("anon() drops a contrasts matrix when redaction merges levels", {
  f <- factor(c("DrugA", "DrugB", "Placebo"))
  stats::contrasts(f) <- stats::contr.treatment(3)
  out <- anon(f, pattern_list = list("[D]" = c("DrugA", "DrugB")))
  expect_equal(levels(out), c("[D]", "Placebo"))
  expect_null(attr(out, "contrasts"))
  df <- data.frame(f = f)
  expect_null(attr(anon(df, pattern_list = list("[D]" = c("DrugA", "DrugB")))$f, "contrasts"))
})

test_that("anon() redacts inside nested data frames and keeps a list_of's class and prototype", {
  skip_if_not_installed("haven")
  inner <- function() {
    t <- tibble::tibble(DRUGQX_COL = c("DRUGQX a", "b"), L = haven::labelled(c(1, 2), labels = c("DRUGQX dose" = 1)))
    attr(t$DRUGQX_COL, "label") <- "DRUGQX label"
    attr(t, "label") <- "DRUGQX table"
    t
  }
  d <- tibble::tibble(ID = 1:2)
  d$N <- vctrs::list_of(inner(), inner())
  d$I <- I(list(inner(), NULL))
  d$P <- inner()
  out <- anon(d, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_false(any(grepl("DRUGQX", deparse(unclass(out)))))
  expect_s3_class(out$N, "vctrs_list_of")
  expect_s3_class(out$I, "AsIs")
  expect_named(out$N[[1]], c("[DRUG]_COL", "L"))
  expect_identical(out$N[[1]][["[DRUG]_COL"]][1], "[DRUG] a")
  expect_identical(attr(out$P, "label"), "[DRUG] table")
  expect_identical(names(attr(out$I[[1]]$L, "labels")), "[DRUG] dose")
  expect_identical(do.call(vctrs::vec_ptype_common, unname(as.list(unclass(out$N)))), attr(out$N, "ptype"))
})

test_that("anon() redacts text at any depth in attributes, and the names of attributes", {
  x <- 1:2
  attr(x, "assay") <- structure(factor("DRUGQX assay"), lab = "DRUGQX lab")
  attr(x, "DRUGQX_lot") <- "L1"
  attr(x, "map") <- data.frame(who = "DRUGQX site")
  df <- data.frame(x = x)
  attr(df, "variable.labels") <- list(x = "DRUGQX result", DRUGQX_y = "y")
  out <- anon(df, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_false(any(grepl("DRUGQX", deparse(unclass(out), control = "all"))))
  expect_identical(attr(out, "variable.labels"), list(x = "[DRUG] result", "[DRUG]_y" = "y"))
  expect_identical(levels(attr(out$x, "assay")), "[DRUG] assay")
  expect_identical(attr(attr(out$x, "assay"), "lab"), "[DRUG] lab")
  expect_identical(attr(out$x, "[DRUG]_lot"), "L1")
  expect_identical(attr(out$x, "map")$who, "[DRUG] site")
  # Structural and core attributes keep their names, also inside attributes.
  y <- structure("a", label = "the label", held = factor("b"))
  out <- anon(y, pattern_list = c("label", "levels", "class"))
  expect_identical(attr(out, "label"), "the [REDACTED]")
  expect_s3_class(attr(out, "held"), "factor")
  # Without check_labels, attributes are left as they are.
  expect_identical(attr(anon(df, pattern_list = "DRUGQX", check_labels = FALSE), "variable.labels"), attr(df, "variable.labels"))
})

test_that("anon() keeps a list's own attributes and class, redacted", {
  l <- structure(list(a = "DRUGQX"), note = "DRUGQX note", dimx = list(src = "DRUGQX"))
  out <- anon(l, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_identical(attr(out, "note"), "[DRUG] note")
  expect_identical(attr(out, "dimx"), list(src = "[DRUG]"))
  expect_identical(attr(anon(l, pattern_list = "DRUGQX", check_labels = FALSE), "note"), "DRUGQX note")
  # A classed list (such as an htest) stays one, its text redacted.
  h <- structure(list(data.name = "DRUGQX by arm", statistic = c(t = 1)), class = "htest")
  expect_s3_class(anon(h, pattern_list = "DRUGQX"), "htest")
  expect_identical(anon(h, pattern_list = list("[DRUG]" = "DRUGQX"))$data.name, "[DRUG] by arm")
})

test_that("anon() keeps models in list columns and warns about what it can't redact", {
  # Made in the global environment, so that their environments (which anon()
  # would copy) aren't this test's.
  fit <- stats::lm(stats::as.formula("mpg ~ wt", env = globalenv()), data = mtcars)
  d <- tibble::tibble(ID = 1:2, FIT = list(fit, NULL), F = list(local(function(x) x, globalenv()), NULL))
  expect_warning(
    out <- anon(d, pattern_list = "DRUGQX"),
    "can't redact functions, calls, formulas[^:]*: `FIT\\[1\\]\\$call`, `FIT\\[1\\]\\$terms`, `FIT\\[1\\]\\$model` \\(attribute `terms`\\), `F\\[1\\]`\\."
  )
  expect_s3_class(out$FIT[[1]], "lm")
  expect_identical(stats::coef(out$FIT[[1]]), stats::coef(fit))
  expect_warning(anon(function(x) x), ": the object itself\\.")
  # One warning lists the first ten locations and counts the others.
  many <- tibble::tibble(FIT = rep(list(fit), 12))
  w <- character(0)
  withCallingHandlers(anon(many, pattern_list = "DRUGQX"), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_length(w, 1)
  expect_match(w, "`FIT\\[4\\]\\$call`, and 26 more\\.")
})

test_that("anon() never sets attributes on objects it leaves as they are", {
  inner <- new.env()
  l <- list(f = sum, e = globalenv(), g = local(function(x) x, globalenv()))
  out <- suppressWarnings(anon(l, pattern_list = "DRUGQX"))
  expect_null(attributes(globalenv()))
  expect_identical(out$f, sum)
  expect_identical(out$e, globalenv())
  expect_identical(out$g, utils::removeSource(l$g))
  expect_identical(suppressWarnings(anon(sum)), sum)
})

test_that("anon() redacts the text fields of records and POSIXlt date-times", {
  rc <- vctrs::new_rcrd(list(name = c("DRUGQX", "b"), n = 1:2))
  out <- anon(tibble::tibble(id = 1:2, rc = rc), pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_identical(vctrs::field(out$rc, "name"), c("[DRUG]", "b"))
  lt <- as.POSIXlt("2020-01-01", tz = "UTC")
  attr(lt, "note") <- "DRUGQX note"
  out <- anon(list(lt = lt), pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_s3_class(out$lt, "POSIXlt")
  expect_identical(attr(out$lt, "note"), "[DRUG] note")
  expect_identical(format(out$lt), format(lt))
})

test_that("anon() drops secrets read from a step, with a warning", {
  step <- anon_subject_ids(list(dm = data.frame(USUBJID = c("A-1", "A-2"))), keep_secrets = TRUE, seed = 1)
  expect_s3_class(anon_secrets(step), "anon_secrets_list")
  expect_warning(
    out <- anon(list(keys = anon_secrets(step), vault = step$secrets, d = "x")),
    "dropped the secrets[^`]*`keys`, `vault`"
  )
  expect_null(out$keys)
  expect_null(out$vault)
  e <- new.env(parent = globalenv())
  assign("keys", anon_secrets(step), envir = e)
  expect_warning(a <- anon(e), "dropped the secrets")
  expect_identical(ls(a), character(0))
})

test_that("anon() keeps a grouped tibble's dataset label", {
  g <- dplyr::group_by(tibble::tibble(ARM = c("DRUGQX", "B"), N = 1:2), ARM)
  attr(g, "label") <- "DRUGQX elements"
  out <- anon(g, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_identical(attr(out, "label"), "[DRUG] elements")
  expect_s3_class(out, "grouped_df")
  expect_setequal(dplyr::group_keys(out)$ARM, c("[DRUG]", "B"))
})

test_that("anon() on an environment returns a redacted copy and leaves the environment as it is", {
  e <- new.env(parent = globalenv())
  assign("DRUGQX_notes", c("DRUGQX a", "b"), envir = e)
  assign(".hidden", "DRUGQX", envir = e)
  assign("df", data.frame(x = "DRUGQX"), envir = e)
  out <- anon(e, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_true(is.environment(out))
  expect_false(identical(out, e))
  expect_identical(parent.env(out), parent.env(e))
  expect_setequal(ls(out, all.names = TRUE), c("[DRUG]_notes", ".hidden", "df"))
  expect_identical(get("[DRUG]_notes", envir = out), c("[DRUG] a", "b"))
  expect_identical(get(".hidden", envir = out), "[DRUG]")
  expect_identical(get("df", envir = out)$x, "[DRUG]")
  # The environment given keeps its class and objects.
  expect_null(attributes(e))
  expect_setequal(ls(e, all.names = TRUE), c("DRUGQX_notes", ".hidden", "df"))
  expect_identical(e$df$x, "DRUGQX")
  expect_setequal(ls(anon(e, pattern_list = "DRUGQX", check_names = FALSE)), c("DRUGQX_notes", "df"))
})

test_that("anon() on an environment drops secrets, copies nested environments and skips active bindings", {
  e <- new.env(parent = globalenv())
  step <- anon_subject_ids(list(dm = data.frame(USUBJID = c("A-1", "A-2"))), keep_secrets = TRUE, seed = 1)
  assign("step", step, envir = e)
  inner <- new.env()
  inner$note <- "DRUGQX note"
  assign("inner", inner, envir = e)
  makeActiveBinding("DRUGQX_live", function() stop("read"), e)
  lockEnvironment(e, bindings = TRUE)
  warnings <- character(0)
  out <- withCallingHandlers(
    anon(e, pattern_list = list("[DRUG]" = "DRUGQX")),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(warnings, "Active bindings are not copied[^:]*: `\\[DRUG\\]_live`", all = FALSE)
  expect_null(out$step$secrets)
  expect_false(is.null(step$secrets))
  expect_false(identical(out$inner, inner))
  expect_identical(out$inner$note, "[DRUG] note")
  expect_identical(inner$note, "DRUGQX note")
  expect_false("[DRUG]_live" %in% ls(out))
  expect_false(environmentIsLocked(out))
  # Objects whose redacted names are the same are made unique.
  e2 <- new.env(parent = globalenv())
  assign("DRUGQX", 1, envir = e2)
  assign("drugqx", 2, envir = e2)
  expect_setequal(ls(anon(e2, pattern_list = list("[DRUG]" = "DRUGQX"))), c("[DRUG]", "[DRUG] 1"))
})

test_that("anon() on an environment copies a parent R doesn't save by reference", {
  # Made in the global environment, so that the frame's parent (which
  # anon() would copy) isn't this test's.
  maker <- local(function() {
    secret_key <- "DRUGQX-0001"
    e <- new.env()
    e$d <- data.frame(a = "x")
    e
  }, globalenv())
  frame_env <- maker()
  out <- anon(frame_env, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_false(identical(parent.env(out), parent.env(frame_env)))
  expect_identical(get("secret_key", envir = parent.env(out)), "[DRUG]-0001")
  expect_length(grepRaw("DRUGQX", serialize(out, NULL)), 0)
  expect_identical(get("secret_key", envir = parent.env(frame_env)), "DRUGQX-0001")
  for (parent in list(globalenv(), baseenv(), emptyenv(), asNamespace("stats"), as.environment("package:stats"))) {
    e <- new.env(parent = parent)
    e$v <- "a"
    expect_no_warning(out <- anon(e))
    expect_identical(parent.env(out), parent)
  }
})

test_that("anon() on an environment skips what it can't read and names what redacts to nothing", {
  e <- (function(a) environment())()
  assign("x", "DRUGQX", envir = e)
  delayedAssign("broken", stop("fails"), assign.env = e)
  delayedAssign("lazy", "DRUGQX later", assign.env = e)
  expect_warning(out <- anon(e, pattern_list = list("[DRUG]" = "DRUGQX")), "couldn't be read[^:]*: `a`, `broken`")
  expect_setequal(ls(out), c("lazy", "x"))
  expect_identical(out$lazy, "[DRUG] later")
  e2 <- new.env(parent = globalenv())
  e2$DRUGQX <- 1
  e2$other <- 2
  out <- anon(e2, default_replacement = "", pattern_list = "^DRUGQX$")
  expect_setequal(ls(out), c("[unnamed 1]", "other"))
  # A classed environment comes back as a plain one.
  e3 <- new.env(parent = globalenv())
  class(e3) <- "custom"
  expect_identical(class(anon(e3)), c("anon_context", "environment"))
})

test_that("anon() with NLP reads an environment's names and skips what it can't redact", {
  local_mocked_bindings(nlp_get_entities = function(x, entity_types = NULL, return_list = FALSE) {
    out <- stats::setNames(rep(list(character(0)), length(entity_types)), entity_types)
    if ("PERSON" %in% entity_types && any(grepl("JohnSmith", collect_text(x)))) out$PERSON <- "JohnSmith"
    if (return_list) out else unlist(out)
  })
  e <- new.env(parent = globalenv())
  e$JohnSmith_x <- "a"
  e$self <- e
  e$f <- function(x) x
  e$ns <- asNamespace("stats")
  out <- suppressWarnings(anon(e, nlp_auto = nlp_auto(person = TRUE)))
  expect_setequal(ls(out), c("[PERSON]_x", "f", "ns", "self"))
})

test_that("anon() walks POSIXlt values and records held in attributes", {
  lt <- as.POSIXlt("2024-05-01 10:00", tz = "UTC")
  rc <- vctrs::new_rcrd(list(a = c("DRUGQX", "b")))
  d <- data.frame(x = "a")
  attr(d, "extracted") <- lt
  attr(d, "rec") <- rc
  attr(d, "held") <- data.frame(when = 1)
  attr(attr(d, "held"), "rec") <- rc
  out <- anon(d, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_identical(attr(out, "extracted"), lt)
  expect_identical(vctrs::field(attr(out, "rec"), "a"), c("[DRUG]", "b"))
  expect_identical(vctrs::field(attr(attr(out, "held"), "rec"), "a"), c("[DRUG]", "b"))
  e <- new.env(parent = globalenv())
  e$d <- d
  expect_identical(vctrs::field(attr(anon(e, pattern_list = list("[DRUG]" = "DRUGQX"))$d, "rec"), "a"), c("[DRUG]", "b"))
})

test_that("closures and formulas in the copy don't refer to the original's data", {
  # Made in the global environment, so that the frame's parent (which
  # anon() would copy) isn't this test's.
  make <- local(function() {
    secret <- "DRUGQX-0001"
    dat <- data.frame(y = c(1, 3, 2, 5, 4), x = 1:5)
    list(g = function() secret, f = y ~ x, fit = stats::lm(y ~ x, data = dat))
  }, globalenv())
  objs <- make()
  e <- new.env(parent = globalenv())
  e$g <- objs$g
  e$l <- list(f = objs$f)
  e$fit <- objs$fit
  local_fn <- function() 1
  environment(local_fn) <- e
  e$local_fn <- local_fn
  w <- character(0)
  out <- withCallingHandlers(anon(e, pattern_list = list("[DRUG]" = "DRUGQX")), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_match(w, "now refer to a redacted copy of their environment[^:]*: [^.]*`fit\\$terms`", all = FALSE)
  expect_length(grepRaw("DRUGQX", serialize(out, NULL)), 0)
  # Closures and formulas made in one frame share one redacted copy of it.
  frame_copy <- environment(out$g)
  expect_false(identical(frame_copy, environment(objs$g)))
  expect_identical(out$g(), "[DRUG]-0001")
  expect_identical(attr(out$l$f, ".Environment"), frame_copy)
  expect_identical(environment(out$fit$terms), frame_copy)
  expect_identical(environment(out$local_fn), out)
  # The model still predicts.
  expect_equal(stats::predict(out$fit, newdata = data.frame(x = 6)), stats::predict(objs$fit, newdata = data.frame(x = 6)))
  # The originals keep theirs.
  expect_identical(objs$g(), "DRUGQX-0001")
  # In a plain list too.
  expect_length(grepRaw("DRUGQX", serialize(suppressWarnings(anon(objs, pattern_list = "DRUGQX")), NULL)), 0)
})

test_that("anon() keeps external pointers without a warning, and names attributes as redacted", {
  d <- data.frame(x = 1)
  attr(d, "problems") <- new("externalptr")
  expect_no_warning(anon(d))
  v <- structure("a", DRUGQX_call = quote(f(x)))
  expect_warning(anon(v, pattern_list = list("[DRUG]" = "DRUGQX")), ": attribute `\\[DRUG\\]_call`\\.")
})

test_that("anon() drops secrets held in attributes", {
  step <- anon_subject_ids(list(dm = data.frame(USUBJID = c("KEY-01", "KEY-02"))), keep_secrets = TRUE, seed = 2)
  d <- step$data$dm
  attr(d, "keys") <- anon_secrets(step)
  attr(d, "step") <- step
  expect_warning(out <- anon(d), "dropped the secrets[^`]*attribute `keys`, attribute `step`")
  expect_null(attr(out, "keys"))
  expect_null(attr(out, "step")$secrets)
  expect_false(grepl("KEY-0", paste(deparse(unclass(out)), collapse = "")))
})

test_that("anon() redacts record field names, list fields and POSIXlt names", {
  rc <- vctrs::new_rcrd(list(DRUGQX_f = c("a", "b"), l = list("DRUGQX", "y")))
  out <- anon(rc, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_identical(vctrs::fields(out), c("[DRUG]_f", "l"))
  expect_identical(vctrs::field(out, "l"), list("[DRUG]", "y"))
  lt <- as.POSIXlt(c(DRUGQX = "2024-01-01"), tz = "UTC")
  expect_identical(names(anon(lt, pattern_list = list("[DRUG]" = "DRUGQX"))), "[DRUG]")
})

test_that("NLP reads records and each object of an environment once", {
  local_mocked_bindings(nlp_get_entities = function(x, entity_types = NULL, return_list = FALSE) {
    out <- stats::setNames(rep(list(character(0)), length(entity_types)), entity_types)
    if ("PERSON" %in% entity_types && any(grepl("JohnSmith", collect_text(x)))) out$PERSON <- "JohnSmith"
    if (return_list) out else unlist(out)
  })
  expect_identical(collect_text(vctrs::new_rcrd(list(a = c("x", "y"), n = 1:2))), c("x", "y"))
  e <- new.env(parent = globalenv())
  e$rc <- vctrs::new_rcrd(list(nm = c("JohnSmith", "b")))
  delayedAssign("bad", stop("fails"), assign.env = e)
  w <- character(0)
  out <- withCallingHandlers(anon(e, nlp_auto = nlp_auto(person = TRUE)), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_identical(vctrs::field(out$rc, "nm"), c("[PERSON]", "b"))
  expect_length(w, 1)
  expect_match(w, "couldn't be read[^:]*: `bad`")
})

test_that("anon() sets nothing on objects given to it directly with .self", {
  inner <- new.env()
  # A function given directly is returned without anything set on it.
  f <- local(function() 1, globalenv())
  expect_null(attributes(anon(f, .self = TRUE)))
  # An environment is replaced by a copy, with nothing set on the original.
  out <- anon(inner, .self = TRUE)
  expect_false(identical(out, inner))
  expect_null(attributes(out))
  expect_null(attributes(inner))
  # As the data summary does when it applies column rules.
  invisible(anon_data_summary(list(e = inner, d = data.frame(x = 1)), df_variable_names = "x", example_values_n = 1))
  expect_null(attributes(inner))
})

test_that("anon() drops source references wherever it finds functions and calls", {
  # Its environment is the global one, so only its source differs.
  f <- eval(parse(text = "function() 1 # DRUGQX-003", keep.source = TRUE), globalenv())
  ex <- parse(text = "x # DRUGQX-004", keep.source = TRUE)
  e <- new.env(parent = globalenv())
  e$f <- f
  e$l <- list(g = f, ex = ex)
  e$d <- structure(data.frame(a = 1), fn = f, srcref = attr(f, "srcref"))
  e$col <- tibble::tibble(id = 1, fns = list(f))
  out <- suppressWarnings(anon(e))
  expect_false(grepl("DRUGQX", rawToChar(serialize(out, NULL, ascii = TRUE))))
  expect_identical(out$f(), 1)
  expect_null(attr(out$f, "srcref"))
  expect_null(attr(suppressWarnings(anon(f)), "srcref"))
  expect_false(is.null(attr(f, "srcref")))
})

test_that("anon() copies environments held inside objects, once each", {
  make <- local(function() {
    secret <- "DRUGQX-005"
    d <- data.frame(a = 1)
    attr(d, "env") <- environment()
    d
  }, globalenv())
  shared <- new.env(parent = globalenv())
  shared$note <- "DRUGQX note"
  shared$self <- shared
  e <- new.env(parent = globalenv())
  e$l <- list(a = shared, b = shared)
  e$col <- tibble::tibble(id = 1, env = list(shared))
  e$d <- make()
  out <- suppressWarnings(anon(e, pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_false(grepl("DRUGQX", rawToChar(serialize(out, NULL, ascii = TRUE))))
  copy <- out$l$a
  expect_false(identical(copy, shared))
  expect_identical(copy$note, "[DRUG] note")
  expect_identical(out$l$b, copy)
  expect_identical(out$col$env[[1]], copy)
  expect_identical(copy$self, copy)
  # The copy of the frame holds the data frame's copy, whose attribute is the frame's copy.
  expect_identical(attr(attr(out$d, "env")$d, "env"), attr(out$d, "env"))
  expect_identical(shared$note, "DRUGQX note")
  # Shared environments stay by reference.
  expect_identical(suppressWarnings(anon(list(g = globalenv())))$g, globalenv())
})

test_that("objects with reference semantics keep working on their redacted copy", {
  # Built as R6 builds its objects (without adding R6 as a dependency): a
  # classed environment of methods whose enclosing environment holds `self`,
  # and is held by the object as `.__enclos_env__`.
  new_person <- function(name) {
    self <- new.env(parent = emptyenv())
    enclos <- new.env(parent = globalenv())
    enclos$self <- self
    self$name <- name
    self$greet <- function() paste("hi", self$name)
    environment(self$greet) <- enclos
    self$.__enclos_env__ <- enclos
    class(self) <- c("Person", "R6")
    self
  }
  p <- new_person("DRUGQX bob")
  out <- suppressWarnings(anon(list(p = p, again = p), pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_s3_class(out$p, "R6")
  expect_identical(out$p$greet(), "hi [DRUG] bob")
  expect_identical(out$again, out$p)
  expect_identical(p$greet(), "hi DRUGQX bob")
  expect_false(grepl("DRUGQX", rawToChar(serialize(out, NULL, ascii = TRUE))))
  # Environments are copied before the closures that refer to them, whatever
  # their names.
  e <- new.env(parent = globalenv())
  holder <- new.env(parent = globalenv())
  holder$v <- "DRUGQX"
  e$a_reader <- function() v
  environment(e$a_reader) <- holder
  e$z_holder <- holder
  out <- suppressWarnings(anon(e, pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_identical(environment(out$a_reader), out$z_holder)
  expect_identical(out$a_reader(), "[DRUG]")
})

test_that("dropping source references leaves the original function as it is", {
  # Its environment is the global one, which saving it doesn't save.
  f <- eval(parse(text = "function() {\n  1 # DRUGQX-007\n}", keep.source = TRUE), globalenv())
  before <- serialize(f, NULL)
  out <- suppressWarnings(anon(list(f = f)))
  expect_identical(serialize(f, NULL), before)
  expect_null(attr(out$f, "srcref"))
  expect_false(grepl("DRUGQX", rawToChar(serialize(out, NULL, ascii = TRUE))))
})

test_that("NLP finds text in nested environments, objects with reference semantics and closures' environments", {
  local_mocked_bindings(nlp_get_entities = function(x, entity_types = NULL, return_list = FALSE) {
    out <- stats::setNames(rep(list(character(0)), length(entity_types)), entity_types)
    found <- intersect(c("JohnSmith", "MaryJones", "AnnLee"), unlist(strsplit(collect_text(x), " ")))
    if ("PERSON" %in% entity_types) out$PERSON <- found
    if (return_list) out else unlist(out)
  })
  inner <- new.env(parent = globalenv())
  inner$note <- "seen by JohnSmith"
  inner$self <- inner
  obj <- new.env(parent = emptyenv())
  obj$investigator <- "MaryJones signed"
  class(obj) <- "R6"
  factory <- local(function() {
    who <- "AnnLee"
    function() who
  }, globalenv())
  top <- new.env(parent = globalenv())
  top$cfg <- list(env = inner)
  top$obj <- obj
  top$get_who <- factory()
  out <- suppressWarnings(anon(top, nlp_auto = nlp_auto(person = TRUE)))
  expect_identical(out$cfg$env$note, "seen by [PERSON]")
  expect_identical(out$obj$investigator, "[PERSON] signed")
  expect_identical(out$get_who(), "[PERSON]")
})

test_that("anon() drops reference class objects, with a warning", {
  Person <- methods::setRefClass("AnonTestPerson", fields = list(name = "character"), where = globalenv())
  on.exit(methods::removeClass("AnonTestPerson", where = globalenv()), add = TRUE)
  p <- Person$new(name = "DRUGQX alice")
  e <- new.env(parent = globalenv())
  e$p <- p
  e$l <- list(p = p)
  e$d <- structure(data.frame(a = 1), owner = p)
  w <- character(0)
  out <- withCallingHandlers(anon(e), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  dropped <- regmatches(w, regexpr("dropped these reference class objects[^:]*: [^\n]*", w))
  for (location in c("`p`", "`l$p`", "`d` (attribute `owner`)")) {
    expect_true(grepl(location, dropped, fixed = TRUE), info = location)
  }
  expect_false("p" %in% ls(out))
  expect_null(out$l$p)
  expect_null(attr(out$d, "owner"))
  expect_false(grepl("DRUGQX", rawToChar(serialize(out, NULL, ascii = TRUE))))
  expect_identical(p$name, "DRUGQX alice")
})

test_that("anon() redacts a function's own attributes", {
  f <- structure(local(function() 1, globalenv()), note = "DRUGQX note")
  out <- suppressWarnings(anon(list(f = f), pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_identical(attr(out$f, "note"), "[DRUG] note")
})

test_that("anon() gives a clear error for environments nested too deeply", {
  top <- new.env(parent = globalenv())
  current <- top
  for (i in seq_len(3000)) {
    nested <- new.env(parent = emptyenv())
    current$child <- nested
    current <- nested
  }
  expect_error(anon(list(top = top)), "can't follow objects nested this deeply", class = "anon_too_deep")
})

test_that("anon() names S7 objects it can't redact", {
  skip_if_not_installed("S7")
  # S7 isn't a dependency: its functions are looked up only when installed.
  new_class <- getExportedValue("S7", "new_class")
  class_character <- getExportedValue("S7", "class_character")
  Note <- new_class("AnonTestNote", properties = list(text = class_character))
  expect_warning(anon(list(n = Note(text = "DRUGQX"))), "can't redact[^:]*: `n`")
})

test_that("anon() keeps missing arguments in lists, as in a list of formals", {
  l <- list(args = alist(a = , b = "DRUGQX"))
  out <- anon(l, pattern_list = list("[DRUG]" = "DRUGQX"))
  expect_identical(out$args$a, quote(expr = ))
  expect_identical(out$args$b, "[DRUG]")
})

test_that("closures = \"detach\" keeps no data of closures' environments, and \"copy\" keeps closures working", {
  make <- local(function() {
    who <- "JanePlain"
    list(get_who = function() who, f = y ~ x)
  }, globalenv())
  objs <- make()
  e <- new.env(parent = globalenv())
  e$get_who <- objs$get_who
  e$l <- list(f = objs$f)
  e$df <- data.frame(a = "kept")
  w <- character(0)
  detached <- withCallingHandlers(anon(e, closures = "detach"), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_false(grepl("JanePlain", rawToChar(serialize(detached, NULL, ascii = TRUE))))
  expect_identical(environment(detached$get_who), globalenv())
  expect_identical(attr(detached$l$f, ".Environment"), globalenv())
  expect_identical(detached$df$a, "kept")
  expect_match(w, "refer to the global environment instead of their own[^:]*: `get_who`", all = FALSE)
  copied <- suppressWarnings(anon(e, closures = "copy"))
  expect_identical(copied$get_who(), "JanePlain")
  # The global option sets the default.
  old <- options(anon.closures = "detach")
  on.exit(options(old), add = TRUE)
  expect_identical(environment(suppressWarnings(anon(e))$get_who), globalenv())
  expect_error(anon(e, closures = "other"), "should be one of")
})

test_that("closures = \"detach\" gives copied environments the global environment as parent", {
  make_env <- local(function() {
    hidden <- "JanePlain"
    e <- new.env()
    e$x <- 1
    e
  }, globalenv())
  saved <- function(x) rawToChar(serialize(x, NULL, ascii = TRUE))
  w <- character(0)
  out <- withCallingHandlers(anon(make_env(), closures = "detach"), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_identical(parent.env(out), globalenv())
  expect_false(grepl("JanePlain", saved(out)))
  expect_match(w, "parents of these environments were replaced by the global environment[^:]*: the environment\\.", all = FALSE)
  nested <- suppressWarnings(anon(list(e = make_env()), closures = "detach"))
  expect_identical(parent.env(nested$e), globalenv())
  expect_false(grepl("JanePlain", saved(nested)))
  # Under "copy", the parent is copied, as documented.
  copied <- anon(make_env())
  expect_identical(get("hidden", envir = parent.env(copied)), "JanePlain")
})

test_that("anon() names raw vectors, which it can't search", {
  expect_warning(anon(list(r = charToRaw("DRUGQX")), pattern_list = "DRUGQX"), "can't redact[^:]*: `r`")
  d <- data.frame(id = 1:2)
  d$blob <- I(list(as.raw(1), as.raw(2)))
  d$bytes <- as.raw(1:2)
  expect_warning(anon(d), "can't redact[^\n]*`blob\\[1\\]`, `blob\\[2\\]`, `bytes`")
  expect_warning(anon(structure(1, payload = as.raw(1))), "can't redact[^:]*: attribute `payload`")
})

test_that("anon_data_summary() with column rules handles environments and closures", {
  objs <- list(d = data.frame(a = c("x", "y")), l = list(e = new.env(), f = local({
    z <- 1
    function() z
  })))
  expect_s3_class(suppressWarnings(anon_data_summary(objs, df_variable_names = "a", example_values_n = 2)), "anon_data_summary")
  expect_s3_class(suppressWarnings(anon_report(objs, df_variable_names = "a", example_values_n = 2)), "anon_report")
})

test_that("anon() drops a step's secrets environment met as a closure's environment or a parent", {
  step <- anon_subject_ids(list(dm = data.frame(USUBJID = c("REALKEY-01", "REALKEY-02"))), keep_secrets = TRUE, seed = 1)
  f <- local(function() 1, globalenv())
  environment(f) <- step$secrets
  fo <- stats::as.formula("y ~ x", env = step$secrets)
  e <- new.env(parent = step$secrets)
  e$x <- 1
  saved <- function(x) rawToChar(serialize(x, NULL, ascii = TRUE))
  w <- character(0)
  out <- withCallingHandlers(anon(list(f = f, fo = fo, e = e)), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_false(grepl("REALKEY", saved(out)))
  expect_match(w, "dropped the secrets[^:]*: read them", all = FALSE)
  expect_match(w, "the environment of `f`", all = FALSE)
  expect_match(w, "the parent of `e`", all = FALSE)
  expect_false(grepl("REALKEY", saved(suppressWarnings(anon(e)))))
})

test_that("S4 functions stay S4, and reference class generators are dropped", {
  where <- new.env()
  methods::setGeneric("anonTestArea", function(shape) standardGeneric("anonTestArea"), where = where)
  methods::setClass("AnonTestSq", methods::representation(s = "numeric"), where = where)
  methods::setMethod("anonTestArea", "AnonTestSq", function(shape) shape@s^2, where = where)
  generic <- get("anonTestArea", envir = where)
  out <- suppressWarnings(anon(list(gen = generic)))
  expect_true(isS4(out$gen))
  Gen <- methods::setRefClass("AnonTestGen", fields = list(n = "character"), where = globalenv())
  on.exit(methods::removeClass("AnonTestGen", where = globalenv()), add = TRUE)
  expect_warning(out <- anon(list(G = Gen, keep = 1)), "dropped these reference class objects[^:]*: `G`")
  expect_null(out$G)
})

test_that("closures, formulas, quosures and terms given to anon() directly are handled as inside objects", {
  make <- local(function() {
    hidden <- "JanePlain"
    list(f = function() hidden, fo = y ~ x)
  }, globalenv())
  saved <- function(x) rawToChar(serialize(x, NULL, ascii = TRUE))
  objs <- make()
  for (x in list(objs$f, objs$fo, stats::terms(objs$fo))) {
    expect_false(grepl("JanePlain", saved(suppressWarnings(anon(x, closures = "detach")))))
    copied <- suppressWarnings(anon(x, pattern_list = list("[J]" = "JanePlain")))
    expect_false(grepl("JanePlain", saved(copied)))
  }
  expect_identical(suppressWarnings(anon(objs$f, pattern_list = list("[J]" = "JanePlain")))(), "[J]")
})

test_that("a function held in an attribute has its attributes cleaned and redacted", {
  g <- structure(local(function() 1, globalenv()), lab = "DRUGQX lab", stash = list2env(list(s = "DRUGQX")))
  out <- suppressWarnings(anon(structure(1:2, holder = g), pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_identical(attr(attr(out, "holder"), "lab"), "[DRUG] lab")
  expect_identical(attr(attr(out, "holder"), "stash")$s, "[DRUG]")
  expect_false(grepl("DRUGQX", rawToChar(serialize(out, NULL, ascii = TRUE))))
})

test_that("NLP also reads parents, environments in attributes and list columns", {
  local_mocked_bindings(nlp_get_entities = function(x, entity_types = NULL, return_list = FALSE) {
    out <- stats::setNames(rep(list(character(0)), length(entity_types)), entity_types)
    found <- intersect(c("JohnSmith", "MaryJones", "AnnLee"), unlist(strsplit(collect_text(x), " ")))
    if ("PERSON" %in% entity_types) out$PERSON <- found
    if (return_list) out else unlist(out)
  })
  parent <- new.env(parent = globalenv())
  parent$note <- "by JohnSmith"
  child <- new.env(parent = parent)
  in_attr <- new.env(parent = globalenv())
  in_attr$note <- "by MaryJones"
  in_col <- new.env(parent = globalenv())
  in_col$note <- "by AnnLee"
  d <- tibble::tibble(id = 1, env = list(in_col))
  top <- new.env(parent = globalenv())
  top$child <- child
  top$v <- structure(1, holder = in_attr)
  top$d <- d
  out <- suppressWarnings(anon(top, nlp_auto = nlp_auto(person = TRUE)))
  expect_identical(get("note", envir = parent.env(out$child)), "by [PERSON]")
  expect_identical(attr(out$v, "holder")$note, "by [PERSON]")
  expect_identical(out$d$env[[1]]$note, "by [PERSON]")
})

test_that("an S4 class that contains an environment keeps its S4 bit", {
  where <- new.env()
  methods::setClass("AnonTestEnvS4", contains = "environment", where = where)
  s4e <- methods::new(methods::getClass("AnonTestEnvS4", where = where))
  assign("n", "DRUGQX", envir = s4e)
  out <- suppressWarnings(anon(list(s = s4e), pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_true(isS4(out$s))
  expect_identical(get("n", envir = out$s), "[DRUG]")
  expect_identical(parent.env(out$s), globalenv())
})

test_that("an invalid closures leaves no state behind", {
  expect_error(anon("a", closures = "bogus"), "should be one of")
  expect_null(copying$memo)
  expect_null(copying$closures)
})

test_that("a function's `...` is rebuilt redacted in copied frames", {
  value <- paste0("DRUG", "QX")
  factory <- local(function(...) {
    force(list(...))
    function() c(...)
  }, globalenv())
  frame_of <- local(function(...) {
    force(list(...))
    environment()
  }, globalenv())
  saved <- function(x) rawToChar(serialize(x, NULL, ascii = TRUE))
  pl <- list("[DRUG]" = "DRUGQX")
  f <- factory(value, note = value)
  out <- suppressWarnings(anon(list(f = f), pattern_list = pl))
  expect_identical(out$f(), c("[DRUG]", note = "[DRUG]"))
  expect_false(grepl("DRUGQX", saved(out)))
  expect_identical(f(), c(value, note = value))
  for (mode in c("copy", "detach")) {
    out <- suppressWarnings(anon(list(fr = frame_of(value)), pattern_list = pl, closures = mode))
    expect_false(grepl("DRUGQX", saved(out)), info = mode)
    expect_identical(evalq(list(...), out$fr), list("[DRUG]"), info = mode)
  }
  # A `...` whose value can't be computed isn't copied.
  broken <- local(function(...) environment(), globalenv())(stop("fails"))
  w <- character(0)
  out <- withCallingHandlers(anon(list(b = broken)), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  expect_false(exists("...", envir = out$b, inherits = FALSE))
  expect_match(w, "couldn't be read[^:]*: `b\\$\\.\\.\\.`", all = FALSE)
  # NLP reads the values.
  local_mocked_bindings(nlp_get_entities = function(x, entity_types = NULL, return_list = FALSE) {
    out <- stats::setNames(rep(list(character(0)), length(entity_types)), entity_types)
    if ("PERSON" %in% entity_types) out$PERSON <- intersect("JohnSmith", collect_text(x))
    if (return_list) out else unlist(out)
  })
  e <- new.env(parent = globalenv())
  e$fr <- frame_of("JohnSmith")
  out <- suppressWarnings(anon(e, nlp_auto = nlp_auto(person = TRUE)))
  expect_identical(evalq(list(...), out$fr), list("[PERSON]"))
})

test_that("an S4 object containing an environment is copied once, keeping its class and slots", {
  where <- new.env()
  methods::setClass("AnonTestBox", contains = "environment", slots = c(note = "character"), where = where)
  make <- local(function(class_def) {
    secret <- "SECRETKEY-1"
    b <- methods::new(class_def, note = "DRUGQX note")
    assign("v", "DRUGQX", envir = b@.xData)
    b
  }, globalenv())
  b <- make(methods::getClass("AnonTestBox", where = where))
  saved <- function(x) rawToChar(serialize(x, NULL, ascii = TRUE))
  pl <- list("[DRUG]" = "DRUGQX")
  for (mode in c("copy", "detach")) {
    out <- suppressWarnings(anon(list(b = b), pattern_list = pl, closures = mode))
    expect_true(isS4(out$b), info = mode)
    expect_identical(out$b@note, "[DRUG] note", info = mode)
    expect_identical(get("v", envir = out$b), "[DRUG]", info = mode)
    expect_identical(out$b@.xData, as.environment(out$b), info = mode)
    expect_identical(parent.env(out$b@.xData), globalenv(), info = mode)
    expect_false(grepl("DRUGQX|SECRETKEY", saved(out)), info = mode)
  }
  top <- suppressWarnings(anon(b, pattern_list = pl))
  expect_true(methods::is(top, "AnonTestBox"))
  expect_identical(top@note, "[DRUG] note")
  expect_identical(get("v", envir = b), "DRUGQX")
})

test_that("formulas' own text attributes are redacted, and terms keep what models need", {
  pl <- list("[DRUG]" = "DRUGQX")
  fo <- structure(stats::as.formula("y ~ x", env = globalenv()), note = "DRUGQX note")
  out <- suppressWarnings(anon(list(fo = fo), pattern_list = pl))
  expect_identical(attr(out$fo, "note"), "[DRUG] note")
  expect_s3_class(out$fo, "formula")
  tt <- structure(stats::terms(stats::as.formula("y ~ x", env = globalenv())), note = "DRUGQX")
  out <- suppressWarnings(anon(list(tt = tt), pattern_list = pl))
  expect_identical(attr(out$tt, "note"), "[DRUG]")
  expect_identical(attr(out$tt, "term.labels"), "x")
  expect_identical(attr(out$tt, "variables"), attr(tt, "variables"))
  fit <- stats::lm(stats::as.formula("mpg ~ wt", env = globalenv()), data = mtcars)
  out <- suppressWarnings(anon(list(fit = fit), pattern_list = pl))
  expect_equal(stats::predict(out$fit, newdata = data.frame(wt = 3)), stats::predict(fit, newdata = data.frame(wt = 3)))
})

test_that("anon() names a raw field of a record", {
  r <- vctrs::new_rcrd(list(a = as.raw(1:2), b = 1:2))
  expect_warning(anon(list(r = r)), "can't redact.*`r\\$a`")
})

test_that("a secrets environment met as a parent becomes the global environment", {
  step <- anon_subject_ids(list(dm = data.frame(USUBJID = c("REALKEY-01", "REALKEY-02"))), keep_secrets = TRUE, seed = 1)
  child <- new.env(parent = step$secrets)
  child$n <- 2
  g <- local(function() n + 1, globalenv())
  environment(g) <- child
  out <- suppressWarnings(anon(list(g = g)))
  expect_identical(parent.env(environment(out$g)), globalenv())
  expect_identical(out$g(), 3)
})

test_that("an empty `...` is kept as it is", {
  mk <- local(function(x, ...) function() x, globalenv())
  frame_of <- local(function(...) environment(), globalenv())
  for (mode in c("copy", "detach")) {
    out <- suppressWarnings(anon(list(f = mk(1), fr = frame_of()), closures = mode))
    expect_true(exists("...", envir = out$fr, inherits = FALSE), info = mode)
    expect_length(eval(as.call(list(base::list, quote(...))), out$fr), 0)
  }
  expect_identical(suppressWarnings(anon(list(f = mk(1))))$f(), 1)
  skip_if_not_installed("memoise")
  # memoise isn't a dependency: its function is looked up only when installed.
  memoise <- getExportedValue("memoise", "memoise")
  m <- memoise(local(function(x) x + 1, globalenv()))
  expect_identical(suppressWarnings(anon(list(m = m)))$m(1), 2)
})

test_that("calls and symbols in `...`, or given to anon(), aren't evaluated", {
  ran <- new.env(parent = emptyenv())
  ran$count <- 0
  frame_of <- local(function(key, ...) {
    force(key)
    list(...)
    environment()
  }, globalenv())
  side <- bquote(assign("count", count + 1, envir = .(ran)))
  fr <- frame_of(paste0("DRUG", "QX"), side, quote(environment()), quote(undefined_symbol))
  out <- suppressWarnings(anon(list(fr = fr), pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_identical(ran$count, 0)
  dots <- eval(as.call(list(base::list, quote(...))), out$fr)
  expect_identical(dots[[2]], quote(environment()))
  expect_identical(dots[[3]], quote(undefined_symbol))
  expect_false(grepl("DRUGQX", rawToChar(serialize(out, NULL, ascii = TRUE))))
  # Given to anon() directly.
  f <- function(x) stop("evaluated")
  expect_identical(suppressWarnings(anon(quote(f(x)))), quote(f(x)))
  expect_identical(suppressWarnings(anon(as.name("zz"))), as.name("zz"))
})

test_that("a formula keeps the order of its attributes", {
  fo <- structure(stats::as.formula("y ~ x", env = globalenv()), note = "DRUGQX")
  out <- suppressWarnings(anon(list(fo = fo), pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_identical(names(attributes(out$fo)), names(attributes(fo)))
})

test_that("methods frames reached as a plain environment's parent aren't copied", {
  where <- new.env()
  methods::setClass("AnonTestBox2", contains = "environment", where = where)
  b <- methods::new(methods::getClass("AnonTestBox2", where = where))
  e <- new.env(parent = b@.xData)
  w <- character(0)
  out <- withCallingHandlers(anon(list(e = e)), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  # The S4 object's environment is copied; its parent (the frame of the
  # initialize() method) isn't.
  expect_false(identical(parent.env(out$e), b@.xData))
  expect_identical(parent.env(parent.env(out$e)), globalenv())
  expect_false(any(grepl("couldn't be read", w)))
})

test_that("`...` is read with base's list(), whatever the frame defines", {
  frame_of <- local(function(...) {
    list <- function(...) stop("the frame's own list()")
    environment()
  }, globalenv())
  out <- suppressWarnings(anon(list(fr = frame_of("DRUGQX")), pattern_list = list("[DRUG]" = "DRUGQX")))
  expect_identical(eval(as.call(list(base::list, quote(...))), out$fr), list("[DRUG]"))
})
