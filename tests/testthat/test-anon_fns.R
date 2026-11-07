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

# ---------------------------------------------------------------------------
# anon_date_shift() — additional coverage
# ---------------------------------------------------------------------------

test_that("anon_date_shift() works with Date objects", {
  dates <- as.Date(c("2023-01-15", "2023-02-20", "2023-03-10"))
  result <- anon_date_shift(dates, center_date = "2024-06-01")

  expect_s3_class(result, "Date")
  expect_length(result, 3)
  # Relative ordering preserved
  expect_equal(order(result), order(dates))
})

test_that("anon_date_shift() parses character center_date formats", {
  dates <- as.Date(c("2023-06-01", "2023-07-01"))

  # Year-only format: "2020" -> "2020-01-01"
  result_year <- anon_date_shift(dates, center_date = "2020")
  expect_s3_class(result_year, "Date")
  expect_true(all(format(result_year, "%Y") %in% c("2019", "2020")))

  # Year-month format: "2020-01" -> "2020-01-01"
  result_ym <- anon_date_shift(dates, center_date = "2020-01")
  expect_s3_class(result_ym, "Date")

  # Full date format
  result_full <- anon_date_shift(dates, center_date = "2020-01-01")
  expect_s3_class(result_full, "Date")

  # Year-only and year-month-01 with same implied date should be identical
  expect_equal(result_year, result_full)
  expect_equal(result_ym, result_full)
})

test_that("anon_date_shift() returns all-NA input unchanged", {
  na_dates <- as.Date(c(NA, NA, NA))
  result <- anon_date_shift(na_dates, center_date = "2024-01-01")
  expect_true(all(is.na(result)))
  expect_length(result, 3)

  na_posix <- as.POSIXct(c(NA, NA), tz = "UTC")
  result2 <- anon_date_shift(na_posix, center_date = "2024-01-01")
  expect_true(all(is.na(result2)))
})

test_that("anon_date_shift() scramble works with Date objects", {
  set.seed(42)
  dates <- as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01"))
  result <- anon_date_shift(dates, center_date = "2025-01-01", scramble = TRUE)

  expect_s3_class(result, "Date")
  expect_length(result, 4)
  # Same set of intervals, but possibly reordered
  original_intervals <- sort(as.numeric(dates - mean(dates)))
  result_intervals <- sort(as.numeric(result - mean(result)))
  expect_equal(original_intervals, result_intervals)
})

test_that("anon_date_shift() handles negative offset (shift to the past)", {
  dates <- as.Date(c("2025-06-01", "2025-07-01"))
  result <- anon_date_shift(dates, center_date = "2000-01-01")

  expect_s3_class(result, "Date")
  expect_true(all(result < as.Date("2001-01-01")))
})

# ---------------------------------------------------------------------------
# anon_email() — additional coverage
# ---------------------------------------------------------------------------

test_that("anon_email() replaces @ emails", {
  x <- c("john@company.com", "jane@work.org")
  result <- anon_email(x)

  expect_equal(result[1], "user001@domain.com")
  expect_equal(result[2], "user002@domain.com")
})

test_that("anon_email() replaces 'at...dot' emails", {
  x <- c("john at company dot com")
  result <- anon_email(x)

  expect_false(grepl("john", result[1]))
  expect_true(grepl("@domain.com", result[1]))
})

test_that("anon_email() handles mixed text with embedded email", {
  x <- "Please contact john@company.com for details"
  result <- anon_email(x)

  expect_true(grepl("Please contact", result))
  expect_true(grepl("@domain.com", result))
  expect_false(grepl("john@company.com", result))
})

test_that("anon_email() returns original when no email found", {
  x <- c("no email here", "just plain text")
  result <- anon_email(x)

  expect_equal(result, x)
})

test_that("anon_email() handles NA values", {
  x <- c("john@company.com", NA, "jane@work.org")
  result <- anon_email(x)

  expect_equal(result[1], "user001@domain.com")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "user002@domain.com")
})

test_that("anon_email() respects custom start and domain", {
  x <- c("test@example.com")
  result <- anon_email(x, start = "anon", domain = "secret.net")

  expect_equal(result, "anon001@secret.net")
})

test_that("anon_email() replaces multiple emails in one string", {
  x <- "Contact john@a.com or jane@b.com"
  result <- anon_email(x)

  expect_false(grepl("john@a.com", result))
  expect_false(grepl("jane@b.com", result))
  expect_equal(
    length(regmatches(result, gregexpr("@domain.com", result))[[1]]),
    2
  )
})

test_that("anon_email() maps duplicate emails to the same replacement", {
  x <- c("john@company.com", "john@company.com", "jane@work.org")
  result <- anon_email(x)

  expect_equal(result[1], result[2])
  expect_false(result[1] == result[3])
})

# ---------------------------------------------------------------------------
# anon_id_chr_sequence() — additional coverage
# ---------------------------------------------------------------------------

test_that("anon_id_chr_sequence() creates sequential character IDs", {
  x <- c("A", "B", "C")
  result <- anon_id_chr_sequence(x)

  expect_equal(result, c("ID 1", "ID 2", "ID 3"))
})

test_that("anon_id_chr_sequence() maps duplicates to the same ID", {
  x <- c("A", "B", "A", "C", "B")
  result <- anon_id_chr_sequence(x)

  expect_equal(result[1], result[3])
  expect_equal(result[2], result[5])
  expect_length(unique(result), 3)
})

test_that("anon_id_chr_sequence() scramble reorders mapping", {
  set.seed(123)
  x <- c("A", "B", "C", "D", "E")
  result_scramble <- anon_id_chr_sequence(x, scramble = TRUE)

  result_no_scramble <- anon_id_chr_sequence(x, scramble = FALSE)

  # Same set of IDs but different assignment
  expect_equal(sort(unique(result_scramble)), sort(unique(result_no_scramble)))
  # Very likely different order (not guaranteed for all seeds, but highly probable)
  expect_false(all(result_scramble == result_no_scramble))
})

test_that("anon_id_chr_sequence() custom prefix", {
  x <- c("X", "Y")
  result <- anon_id_chr_sequence(x, start = "Patient_")

  expect_true(all(grepl("^Patient_", result)))
})

test_that("anon_id_chr_sequence() with padding disabled", {
  x <- c("A", "B", "C")
  result <- anon_id_chr_sequence(x, padding = FALSE)

  expect_equal(result, c("ID 1", "ID 2", "ID 3"))
})

test_that("anon_id_chr_sequence() zero-pads when unique count >= 10", {
  x <- paste0("item", 1:12)
  result <- anon_id_chr_sequence(x, start = "")

  # With 12 items, padding should produce "01", "02", ..., "12"
  expect_equal(result[1], "01")
  expect_equal(result[9], "09")
  expect_equal(result[12], "12")
})

test_that("anon_id_chr_sequence() custom padding_chr", {
  x <- paste0("item", 1:12)
  result <- anon_id_chr_sequence(x, start = "", padding_chr = " ")

  expect_true(grepl("1", result[1]))
  expect_true(grepl("12", result[12]))
})

# ---------------------------------------------------------------------------
# anon_id_num_sequence() — additional coverage
# ---------------------------------------------------------------------------

test_that("anon_id_num_sequence() creates sequential numeric IDs", {
  x <- c("A", "B", "C")
  result <- anon_id_num_sequence(x)

  expect_equal(result, c(1, 2, 3))
})

test_that("anon_id_num_sequence() maps duplicates to the same ID", {
  x <- c("A", "B", "A", "C", "B")
  result <- anon_id_num_sequence(x)

  expect_equal(result[1], result[3])
  expect_equal(result[2], result[5])
})

test_that("anon_id_num_sequence() scramble reorders mapping", {
  set.seed(101)
  x <- c("A", "B", "C", "D", "E")
  result_scramble <- anon_id_num_sequence(x, scramble = TRUE)

  result_no_scramble <- anon_id_num_sequence(x, scramble = FALSE)

  expect_equal(sort(unique(result_scramble)), sort(unique(result_no_scramble)))
  expect_false(all(result_scramble == result_no_scramble))
})

test_that("anon_id_num_sequence() maps NA as a unique value", {
  x <- c("A", NA, "B", NA)
  result <- anon_id_num_sequence(x)

  expect_length(result, 4)
  # NA inputs get the same mapping as each other
  expect_equal(result[2], result[4])
  # All three unique values get distinct IDs
  expect_length(unique(result), 3)
})

# ---------------------------------------------------------------------------
# anon_num_preserve_distribution() — additional coverage
# ---------------------------------------------------------------------------

test_that("anon_num_preserve_distribution() rank method with non-degenerate data", {
  set.seed(50)
  x <- c(10, 20, 30, 40, 50)
  result <- anon_num_preserve_distribution(x, method = "rank")

  expect_length(result, 5)
  # The result uses the same set of values (sorted), just scrambled
  expect_equal(sort(result), sort(x))
})

test_that("anon_num_preserve_distribution() noise method with custom noise_sd", {
  set.seed(77)
  x <- c(100, 200, 300, 400, 500)
  result <- anon_num_preserve_distribution(x, method = "noise", noise_sd = 0.01)

  expect_length(result, 5)
  # With very small noise, values should be very close to originals
  expect_true(all(abs(result - x) < 1))
})

test_that("anon_num_preserve_distribution() quantile normal family", {
  set.seed(10)
  x <- c(5, 10, 15, 20, 25, 30)
  result <- anon_num_preserve_distribution(
    x,
    method = "quantile",
    quantile_dist_family = "normal"
  )

  expect_length(result, 6)
  expect_true(all(is.numeric(result)))
})

test_that("anon_num_preserve_distribution() quantile uniform family", {
  set.seed(11)
  x <- c(5, 10, 15, 20, 25, 30)
  result <- anon_num_preserve_distribution(
    x,
    method = "quantile",
    quantile_dist_family = "uniform"
  )

  expect_length(result, 6)
  # Uniform draws should be within original range
  expect_true(all(result >= min(x) & result <= max(x)))
})

test_that("anon_num_preserve_distribution() quantile exponential family", {
  set.seed(12)
  x <- c(5, 10, 15, 20, 25, 30)
  result <- anon_num_preserve_distribution(
    x,
    method = "quantile",
    quantile_dist_family = "exponential"
  )

  expect_length(result, 6)
  expect_true(all(result > 0))
})

test_that("anon_num_preserve_distribution() handles NA in non-degenerate data", {
  set.seed(88)
  x <- c(10, NA, 20, 30, NA, 40)
  result <- anon_num_preserve_distribution(x, method = "noise")

  expect_true(is.na(result[2]))
  expect_true(is.na(result[5]))
  expect_true(!is.na(result[1]))
  expect_true(!is.na(result[4]))
  expect_length(result, 6)
})

test_that("anon_num_preserve_distribution() preserves length", {
  set.seed(13)
  x <- rnorm(100)
  for (m in c("rank", "noise", "quantile")) {
    result <- anon_num_preserve_distribution(x, method = m)
    expect_length(result, 100)
  }
})

# ---------------------------------------------------------------------------
# anon_num_range() — additional coverage
# ---------------------------------------------------------------------------

test_that("anon_num_range() equal_width creates range labels", {
  x <- c(1, 5, 10, 15, 20)
  result <- anon_num_range(x, n_breaks = 4, method = "equal_width")

  expect_length(result, 5)
  expect_true(all(!is.na(result)))
  expect_type(result, "character")
})

test_that("anon_num_range() equal_count creates range labels", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  result <- anon_num_range(x, n_breaks = 2, method = "equal_count")

  expect_length(result, 10)
  expect_type(result, "character")
})

test_that("anon_num_range() clean_breaks=FALSE uses exact breakpoints", {
  x <- c(1, 5, 10, 15, 20)
  result_clean <- anon_num_range(
    x,
    n_breaks = 4,
    method = "equal_width",
    clean_breaks = TRUE
  )
  result_exact <- anon_num_range(
    x,
    n_breaks = 4,
    method = "equal_width",
    clean_breaks = FALSE
  )

  # Both produce character results, but breakpoints may differ
  expect_type(result_clean, "character")
  expect_type(result_exact, "character")
})

test_that("anon_num_range() keep_values=FALSE produces generic labels", {
  x <- c(1, 5, 10, 15, 20)
  result <- anon_num_range(x, n_breaks = 3, keep_values = FALSE)

  expect_true(all(grepl("^Range_\\d{2}$", result)))
})

test_that("anon_num_range() scramble reorders generic labels", {
  set.seed(55)
  x <- c(1, 5, 10, 15, 20)
  result_scramble <- anon_num_range(
    x,
    n_breaks = 3,
    keep_values = FALSE,
    scramble = TRUE
  )

  result_no_scramble <- anon_num_range(
    x,
    n_breaks = 3,
    keep_values = FALSE,
    scramble = FALSE
  )

  # Same set of labels
  expect_equal(
    sort(unique(result_scramble)),
    sort(unique(result_no_scramble))
  )
})

test_that("anon_num_range() handles NA values", {
  x <- c(1, NA, 10, NA, 20)
  result <- anon_num_range(x, n_breaks = 2)

  expect_true(is.na(result[2]))
  expect_true(is.na(result[4]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[5]))
})

test_that("anon_num_range() handles negative values", {
  x <- c(-20, -10, 0, 10, 20)
  result <- anon_num_range(x, n_breaks = 3)

  expect_length(result, 5)
  expect_type(result, "character")
  expect_true(all(!is.na(result)))
})

# ---------------------------------------------------------------------------
# anon_phone_number() — additional coverage
# ---------------------------------------------------------------------------

test_that("anon_phone_number() detects dash-separated format", {
  x <- "123-456-7890"
  result <- anon_phone_number(x)

  expect_false(grepl("123-456-7890", result))
  expect_true(grepl("555-000", result))
})

test_that("anon_phone_number() detects parentheses format", {
  x <- "(123) 456-7890"
  result <- anon_phone_number(x)

  expect_false(grepl("(123) 456-7890", result, fixed = TRUE))
  expect_true(grepl("555-000", result))
})

test_that("anon_phone_number() detects 10-digit continuous format", {
  x <- "1234567890"
  result <- anon_phone_number(x)

  expect_false(grepl("1234567890", result))
  expect_true(grepl("555-000", result))
})

test_that("anon_phone_number() detects space-separated format", {
  x <- "123 456 7890"
  result <- anon_phone_number(x)

  expect_false(grepl("123 456 7890", result))
  expect_true(grepl("555-000", result))
})

test_that("anon_phone_number() returns original when no phone found", {
  x <- c("no phone here", "just words")
  result <- anon_phone_number(x)

  expect_equal(result, x)
})

test_that("anon_phone_number() handles NA values", {
  x <- c("123-456-7890", NA, "987-654-3210")
  result <- anon_phone_number(x)

  expect_true(is.na(result[2]))
  expect_true(grepl("555-000", result[1]))
  expect_true(grepl("555-000", result[3]))
})

test_that("anon_phone_number() respects custom start", {
  x <- "123-456-7890"
  result <- anon_phone_number(x, start = "800-555")

  expect_true(grepl("800-555", result))
})

test_that("anon_phone_number() handles phone embedded in text", {
  x <- "Call me at 123-456-7890 anytime"
  result <- anon_phone_number(x)

  expect_true(grepl("Call me at", result))
  expect_true(grepl("anytime", result))
  expect_false(grepl("123-456-7890", result))
})

test_that("anonymizers return all-missing input unchanged", {
  expect_equal(anon_email(c(NA_character_, NA_character_)), c(NA_character_, NA_character_))
  expect_equal(anon_phone_number(NA_character_), NA_character_)
  expect_equal(anon_num_preserve_distribution(c(NA_real_, NA_real_)), c(NA_real_, NA_real_))
  expect_equal(anon_num_range(c(NA_real_, NA_real_)), c(NA_real_, NA_real_))
})

test_that("anon_date_shift() handles POSIXct without a time zone and character dates", {
  x <- as.POSIXct(c("2023-01-01 10:00:00", "2023-01-03 10:00:00"), tz = "UTC")
  attr(x, "tzone") <- NULL
  result <- anon_date_shift(x, center_date = "2024-06-02")
  expect_s3_class(result, "POSIXct")
  expect_equal(as.numeric(diff(result), units = "days"), 2)

  result <- anon_date_shift(c("2023-01-01", "2023-01-03"), center_date = "2024-06-02")
  expect_equal(result, c("2024-06-01", "2024-06-03"))
})

test_that("anon_num_range() equal_count handles unclean and collapsing breaks", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  result <- anon_num_range(x, n_breaks = 2, method = "equal_count", clean_breaks = FALSE)
  expect_length(unique(result), 2)

  # A single pretty() break inside the data still yields two ranges
  result <- anon_num_range(c(1.001, 1.009), n_breaks = 2, method = "equal_count")
  expect_true(all(!is.na(result)))
})

test_that("anon_num_range() rejects n_breaks that is not a single number >= 1", {
  x <- c(150, 160, 170)
  msg <- "`n_breaks` must be a single number of at least 1."
  expect_error(anon_num_range(x, n_breaks = 0), msg, fixed = TRUE)
  expect_error(
    anon_num_range(x, n_breaks = 0, method = "equal_width", clean_breaks = FALSE),
    msg,
    fixed = TRUE
  )
  expect_error(anon_num_range(x, n_breaks = -1), msg, fixed = TRUE)
  expect_error(anon_num_range(x, n_breaks = NA), msg, fixed = TRUE)
  expect_error(anon_num_range(x, n_breaks = NA_real_), msg, fixed = TRUE)
  expect_error(anon_num_range(x, n_breaks = c(2, 3)), msg, fixed = TRUE)
})

test_that("anon_num_range() labels identical values with a single-point range", {
  expect_equal(
    anon_num_range(c(160, 160, 160), n_breaks = 1),
    rep("[160,160]", 3)
  )
})

test_that("build_single_range_result() labels with the min and max of values", {
  expect_equal(
    build_single_range_result(c(170, 150, 160), keep_values = TRUE),
    rep("[150,170]", 3)
  )
  expect_equal(
    build_single_range_result(c(5, 5), keep_values = TRUE),
    rep("[5,5]", 2)
  )
  expect_equal(
    build_single_range_result(c(170, 150), keep_values = FALSE),
    rep("Range_01", 2)
  )
})

test_that("more_patterns() returns nothing for empty or all-missing input", {
  expect_equal(more_patterns(character(0)), character(0))
  expect_equal(more_patterns(NA_character_), character(0))
})

test_that("anon_num_range() equal_count with clean breaks assigns every value a range", {
  x <- c(150, 160, 170, 175, 180, 190)
  result <- anon_num_range(x, n_breaks = 2, method = "equal_count")
  expect_false(anyNA(result))
  expect_equal(result[1], "[150,160)")
  expect_equal(result[6], "[180,190]")

  result <- anon_num_range(c(x, NA), n_breaks = 2, method = "equal_count", keep_values = FALSE)
  expect_equal(is.na(result), c(rep(FALSE, 6), TRUE))
})

test_that("anon_date_shift() shifts every value by a fixed offset", {
  dates <- as.Date(c("2023-01-15", "2023-02-20", NA))
  expect_equal(
    anon_date_shift(dates, offset = -10),
    as.Date(c("2023-01-05", "2023-02-10", NA))
  )

  times <- as.POSIXct(c("2023-01-15 08:30:00", "2023-01-16 23:45:00"), tz = "UTC")
  result <- anon_date_shift(times, offset = 3)
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), c("2023-01-18 08:30:00", "2023-01-19 23:45:00"))
  expect_equal(attr(result, "tzone"), "UTC")

  # Separate columns shifted with the same offset keep the intervals between them
  dose <- anon_date_shift("2023-01-15T08:00", offset = 100)
  sample <- anon_date_shift("2023-01-16T09:30", offset = 100)
  expect_equal(
    as.numeric(difftime(
      as.POSIXct(sample, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
      as.POSIXct(dose, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
      units = "hours"
    )),
    25.5
  )
})

test_that("anon_date_shift() validates offset", {
  expect_error(anon_date_shift(Sys.Date(), offset = c(1, 2)), "single whole number of days")
  expect_error(anon_date_shift(Sys.Date(), offset = 1.5), "single whole number of days")
  expect_error(anon_date_shift(Sys.Date(), offset = NA), "single whole number of days")
  expect_error(anon_date_shift(Sys.Date(), offset = "10"), "single whole number of days")
  expect_error(anon_date_shift(Sys.Date(), offset = 10, scramble = TRUE), "scramble")
  expect_equal(
    anon_date_shift(as.Date("2023-01-15"), offset = as.difftime(2, units = "days")),
    as.Date("2023-01-17")
  )
})

test_that("anon_date_shift() shifts ISO 8601 strings and keeps their precision", {
  x <- c(
    "2014-01-02", "2014-01-02T08:30", "2014-01-02T08:30:15", "2014-01-02T08:30:15.5Z",
    "2014-01-02T08", "", NA
  )
  expect_equal(
    anon_date_shift(x, offset = 30),
    c("2014-02-01", "2014-02-01T08:30", "2014-02-01T08:30:15", "2014-02-01T08:30:15.5Z",
      "2014-02-01T08", "", NA)
  )
})

test_that("anon_date_shift() shifts or removes partial ISO 8601 dates", {
  x <- c("2014-03", "2014", "2014-03-10")
  # The middle of each period moves by the offset; precision is kept
  expect_equal(anon_date_shift(x, offset = 30), c("2014-04", "2014", "2014-04-09"))
  expect_equal(anon_date_shift(x, offset = -200), c("2013-08", "2013", "2013-08-22"))
  expect_equal(anon_date_shift(x, offset = 30, partial = "remove"), c("", "", "2014-04-09"))
})

test_that("shift_iso8601() shifts year-and-month dates from day 15 of the month", {
  # From day 15, +16 and +17 land on 31 March and 1 April, -14 and -15 on
  # 1 March and 28 February; from day 14 or 16 one of each pair would differ.
  expect_identical(
    shift_iso8601(rep("2031-03", 4), c(16, 17, -14, -15)),
    c("2031-03", "2031-04", "2031-03", "2031-02")
  )
  expect_identical(shift_iso8601("2031-03--T08:00", 17), "2031-04--T08:00")
})

test_that("anon_date_shift() removes and warns about values that are not ISO 8601 dates", {
  expect_warning(
    result <- anon_date_shift(c("2014-01-02", "2014-13-45", "unknown"), offset = 1),
    "2 values are not recognised ISO 8601 dates"
  )
  expect_equal(result, c("2014-01-03", "", ""))
})

test_that("anon_date_shift() centers character input on center_date", {
  x <- c("2023-01-01T10:00", "2023-01-03", "2023-01")
  result <- anon_date_shift(x, center_date = "2024-06-02")
  expect_equal(result[1:2], c("2024-06-01T10:00", "2024-06-03"))
  expect_error(anon_date_shift(x, scramble = TRUE), "scramble")
})

test_that("anon_date_shift() ignores center_date when offset is given", {
  expect_equal(
    anon_date_shift(as.Date("2023-01-15"), offset = 1, center_date = "unused"),
    as.Date("2023-01-16")
  )
})

test_that("anon_date_shift() removes date-times with an invalid time part", {
  expect_warning(
    result <- anon_date_shift(
      c("2014-01-02T99:99", "2014-01-02Tunknown", "2014-01-02T08:61", "2014-01-02T23:59:59+05:30"),
      offset = 1
    ),
    "3 values are not recognised ISO 8601 dates"
  )
  expect_equal(result, c("", "", "", "2014-01-03T23:59:59+05:30"))
})
