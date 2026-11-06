#' Shift dates to a new time period
#'
#' Shifts dates to a new time period while preserving relative relationships
#' between dates. Supports Date and POSIXct vectors and ISO 8601 character
#' strings such as SDTM `--DTC` variables.
#'
#' By default, dates are moved so that their mean falls on `center_date`. Each
#' call computes its own shift, so separate columns or datasets move by
#' different amounts. To keep intervals between columns and datasets (for
#' example dose and sample times), pass the same `offset` to every call.
#'
#' Character input is read as ISO 8601 and returned as character in the same
#' format and precision: a full date (`2014-01-02`) is shifted and any time part
#' after `T` is kept as is. Partial dates (`2014-03`, `2014`) are handled by
#' `partial`. Empty strings and `NA` are returned unchanged, and other values
#' are removed with a warning.
#'
#' @param x A Date, POSIXct or ISO 8601 character vector to anonymize
#' @param center_date New center point for the date range (default: current
#'   date). Ignored when `offset` is given.
#' @param scramble Logical, whether to randomize the assignment order of
#'   generated results. Not available with `offset` or character input.
#' @param offset Optional number of days (a whole number, or a `difftime`) to
#'   add to every value. Use the same `offset` for all columns that must stay
#'   aligned.
#' @param partial How to handle partial ISO 8601 dates in character input:
#'   `"shift"` (default) moves the middle of the month or year by the offset and
#'   keeps the original precision; `"remove"` replaces them with `""`.
#'
#' @return A vector of the same class as `x` (character for character input).
#'
#' @examples
#' dates <- as.Date(c("2023-01-15", "2023-02-20", "2023-03-10"))
#' anon_date_shift(dates, center_date = "2024-06-01")
#'
#' # One offset for every column keeps intervals between them
#' offset <- -400
#' anon_date_shift(c("2023-01-15T08:00", "2023-02-20", "2023-03"), offset = offset)
#' anon_date_shift(as.Date("2023-01-16"), offset = offset)
#'
#' @export
anon_date_shift <- function(
  x,
  center_date = Sys.Date(),
  scramble = FALSE,
  offset = NULL,
  partial = c("shift", "remove")
) {
  partial <- match.arg(partial)

  if (!is.null(offset)) {
    offset <- validate_date_offset(offset)
    if (isTRUE(scramble)) {
      stop(
        "`scramble` cannot be combined with `offset`; a fixed offset keeps every interval.",
        call. = FALSE
      )
    }
  }

  if (is.character(x)) {
    if (isTRUE(scramble)) {
      stop("`scramble` is not supported for character input.", call. = FALSE)
    }
    if (is.null(offset)) {
      offset <- iso8601_center_offset(x, parse_center_date(center_date))
    }
    return(shift_iso8601(x, offset, partial))
  }

  # Handle missing values
  na_idx <- is.na(x)
  x_clean <- x[!na_idx]

  if (length(x_clean) == 0) {
    return(x)
  }

  # Check if input is POSIXct (date-time)
  is_datetime <- inherits(x_clean, "POSIXct")
  datetime_tz <- NULL
  if (is_datetime) {
    datetime_tz <- attr(x_clean, "tzone", exact = TRUE)
    if (is.null(datetime_tz) || length(datetime_tz) == 0) {
      datetime_tz <- ""
    } else {
      datetime_tz <- datetime_tz[[1]]
    }
  }

  # Convert to Date for centering calculation, preserving original class info
  if (is_datetime) {
    x_dates <- as.Date(x_clean, tz = datetime_tz)
  } else if (!inherits(x_clean, "Date")) {
    x_dates <- as.Date(x_clean)
  } else {
    x_dates <- x_clean
  }

  # Calculate the offset from original center to new center
  if (is.null(offset)) {
    center_date <- parse_center_date(center_date)
    original_center <- mean(x_dates, na.rm = TRUE)
    offset <- as.numeric(center_date - original_center)
  }

  # Apply the shift
  if (is_datetime) {
    # For POSIXct, shift by days but preserve time
    result <- x_clean + (offset * 86400) # 86400 seconds per day

    # Optionally scramble while preserving intervals
    if (scramble) {
      # Calculate intervals from the shifted center (convert center to POSIXct)
      center_datetime <- as.POSIXct(center_date, tz = datetime_tz)
      intervals <- as.numeric(result - center_datetime)
      # Scramble the intervals but keep the same set of values
      scrambled_intervals <- sample(intervals)
      result <- center_datetime + scrambled_intervals
    }
  } else {
    # For Date objects
    result <- x_dates + offset

    # Optionally scramble while preserving intervals
    if (scramble) {
      # Calculate intervals from the shifted center
      intervals <- as.numeric(result - center_date)
      # Scramble the intervals but keep the same set of values
      scrambled_intervals <- sample(intervals)
      result <- center_date + scrambled_intervals
    }
  }

  # Restore missing values with appropriate NA type
  if (is_datetime) {
    final_result <- as.POSIXct(
      rep(NA_real_, length(x)),
      origin = "1970-01-01",
      tz = datetime_tz
    )
  } else {
    final_result <- rep(as.Date(NA), length(x))
  }
  final_result[!na_idx] <- result
  final_result
}

# A whole number of days from a number or difftime.
validate_date_offset <- function(offset) {
  if (inherits(offset, "difftime")) {
    offset <- as.numeric(offset, units = "days")
  }
  if (!is.numeric(offset) || length(offset) != 1 || !is.finite(offset) ||
    offset != round(offset)) {
    stop("`offset` must be a single whole number of days.", call. = FALSE)
  }
  as.numeric(offset)
}

# Parse center_date given as a Date or as "YYYY", "YYYY-MM" or "YYYY-MM-DD".
parse_center_date <- function(center_date) {
  if (!is.character(center_date)) {
    return(center_date)
  }
  if (grepl("^\\d{4}$", center_date)) {
    center_date <- paste0(center_date, "-01-01")
  } else if (grepl("^\\d{4}-\\d{2}$", center_date)) {
    center_date <- paste0(center_date, "-01")
  }
  as.Date(center_date)
}

# Classify ISO 8601 strings and give each a reference date: the date itself
# for full dates (optionally with a time of hours, minutes, seconds and a UTC
# offset), the middle of the month (day 15) or year (2 July) for
# partial dates. Values that are not ISO 8601 dates have kind "invalid".
parse_iso8601_dates <- function(x) {
  kind <- rep("empty", length(x))
  reference <- rep(as.Date(NA), length(x))
  present <- !is.na(x) & nzchar(trimws(x))

  time_part <- paste0(
    "T([01][0-9]|2[0-3])(:[0-5][0-9](:[0-5][0-9](\\.[0-9]+)?)?)?",
    "(Z|[+-][0-9]{2}(:?[0-9]{2})?)?"
  )
  is_full <- present & grepl(paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}(", time_part, ")?$"), x)
  is_month <- present & grepl("^[0-9]{4}-[0-9]{2}$", x)
  is_year <- present & grepl("^[0-9]{4}$", x)

  reference[is_full] <- as.Date(substr(x[is_full], 1, 10), format = "%Y-%m-%d", optional = TRUE)
  reference[is_month] <- as.Date(paste0(x[is_month], "-15"), format = "%Y-%m-%d", optional = TRUE)
  reference[is_year] <- as.Date(paste0(x[is_year], "-07-02"), format = "%Y-%m-%d", optional = TRUE)

  kind[is_full] <- "full"
  kind[is_month] <- "month"
  kind[is_year] <- "year"
  kind[present & is.na(reference)] <- "invalid"

  list(kind = kind, reference = reference)
}

# Offset in whole days that moves the mean of the full dates (or, if there are
# none, of the partial dates) to center_date.
iso8601_center_offset <- function(x, center_date) {
  parsed <- parse_iso8601_dates(x)
  usable <- parsed$kind == "full"
  if (!any(usable)) {
    usable <- parsed$kind %in% c("month", "year")
  }
  if (!any(usable)) {
    return(0)
  }
  round(as.numeric(center_date - mean(parsed$reference[usable])))
}

# With year_step = TRUE, year-only values move by whole years (at least one, in
# the direction of the offset) instead of from the middle of the year, so a
# non-zero offset always changes the year.
shift_iso8601 <- function(x, offset, partial = "shift", year_step = FALSE) {
  parsed <- parse_iso8601_dates(x)
  kind <- parsed$kind
  shifted <- parsed$reference + offset
  out <- x

  full <- kind == "full"
  out[full] <- paste0(format(shifted[full], "%Y-%m-%d"), substring(x[full], 11))

  is_partial <- kind %in% c("month", "year")
  if (partial == "shift") {
    out[kind == "month"] <- format(shifted[kind == "month"], "%Y-%m")
    is_year <- kind == "year"
    if (year_step && any(is_year)) {
      year_offset <- rep_len(offset, length(x))[is_year]
      years <- as.integer(x[is_year]) +
        sign(year_offset) * pmax(1, round(abs(year_offset) / 365.25))
      out[is_year] <- sprintf("%04d", as.integer(years))
    } else {
      out[is_year] <- format(shifted[is_year], "%Y")
    }
  } else {
    out[is_partial] <- ""
  }

  invalid <- kind == "invalid"
  if (any(invalid)) {
    n <- sum(invalid)
    warning(
      n, if (n == 1) " value is" else " values are",
      " not ISO 8601 dates and were removed.",
      call. = FALSE
    )
    out[invalid] <- ""
  }

  out
}

#' Replace email addresses with pseudonymous addresses
#'
#' Replaces each distinct email address with a sequential fake address.
#'
#' @param x A character vector to anonymize
#' @param start Character prefix for generated results (default: "user")
#' @param domain Domain for generated emails (default: "domain.com")
#'
#' @examples
#' emails <- c("john@company.com", "not_an_email", "jane at work dot org")
#' anon_email(emails)
#'
#' @export
anon_email <- function(x, start = "user", domain = "domain.com") {
  # Handle missing values
  na_idx <- is.na(x)
  x_clean <- x[!na_idx]

  if (length(x_clean) == 0) {
    return(x)
  }

  # Define email patterns
  at_symbol_pattern <- "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
  at_dot_pattern <- "\\b\\S+\\s+at\\s+\\S+\\s+dot\\s+\\S+\\b"

  # Find all unique email patterns across all strings
  all_emails <- character(0)

  # Extract emails with @ symbol
  at_emails <- regmatches(
    x_clean,
    gregexpr(at_symbol_pattern, x_clean, ignore.case = TRUE)
  )
  all_emails <- c(all_emails, unlist(at_emails))

  # Extract "at ... dot" patterns
  dot_emails <- regmatches(
    x_clean,
    gregexpr(at_dot_pattern, x_clean, ignore.case = TRUE)
  )
  all_emails <- c(all_emails, unlist(dot_emails))

  # Get unique emails and create mapping
  unique_emails <- unique(all_emails)

  if (length(unique_emails) == 0) {
    # No emails found, return original
    final_result <- rep(NA_character_, length(x))
    final_result[!na_idx] <- x_clean
    return(final_result)
  }

  # Create anonymized emails
  anon_emails <- paste0(
    start,
    sprintf("%03d", seq_along(unique_emails)),
    "@",
    domain
  )
  email_mapping <- stats::setNames(anon_emails, unique_emails)

  # Replace emails in each string
  result <- x_clean
  for (i in seq_along(result)) {
    # Replace @ symbol emails
    for (email in names(email_mapping)) {
      if (grepl(email, result[i], fixed = TRUE)) {
        result[i] <- gsub(email, email_mapping[email], result[i], fixed = TRUE)
      }
    }
  }

  # Restore missing values
  final_result <- rep(NA_character_, length(x))
  final_result[!na_idx] <- result
  final_result
}

#' Pseudonymize identifiers as sequential character IDs
#'
#' Without `scramble = TRUE`, IDs are numbered in order of first appearance,
#' so if `x` is sorted by something sensitive (for example the original ID,
#' enrolment date or name), the new IDs keep that order.
#'
#' @param x A vector to anonymize
#' @param scramble Logical, whether to randomize the assignment order of generated results
#' @param start Character prefix for generated results (default: "ID ")
#' @param padding Logical, whether to zero-pad numbers for consistent width
#' @param padding_chr Character used for padding (default: "0")
#'
#' @examples
#' ids <- c("A123", "B456", "A123", "C789")
#' anon_id_chr_sequence(ids)
#'
#' @export
anon_id_chr_sequence <- function(
  x,
  scramble = FALSE,
  start = "ID ",
  padding = TRUE,
  padding_chr = "0"
) {
  x_unique <- unique(x)
  if (scramble) {
    x_unique <- sample(x_unique)
  }

  x_key <- seq_along(x_unique)
  if (padding) {
    stopifnot(nchar(padding_chr) == 1)
    max_chars <- max(nchar(x_key))
    x_key <- sprintf(paste0("%", padding_chr, max_chars, "d"), x_key)
  }

  x_key <- paste0(start, x_key) |>
    stats::setNames(x_unique)

  unname(x_key[x])
}

#' Pseudonymize identifiers as sequential numeric IDs
#'
#' Without `scramble = TRUE`, IDs are numbered in order of first appearance,
#' so if `x` is sorted by something sensitive (for example the original ID,
#' enrolment date or name), the new IDs keep that order.
#'
#' @param x A vector to anonymize
#' @param scramble Logical, whether to randomize the assignment order of generated results
#'
#' @export
anon_id_num_sequence <- function(x, scramble = FALSE) {
  x_unique <- unique(x)
  if (scramble) {
    x_unique <- sample(x_unique)
  }

  x_key <- seq_along(x_unique) |>
    stats::setNames(x_unique)

  unname(x_key[x])
}

#' Shuffle, add noise to, or resample numeric values
#'
#' Replaces numeric values while keeping their overall distribution, using one
#' of three methods:
#' - `"rank"` (default) reassigns the original values to other records (data
#'   swapping). Every original value, including outliers, is still present.
#' - `"noise"` adds random noise to each value (randomization).
#' - `"quantile"` replaces values with draws from a fitted theoretical
#'   distribution at shuffled quantiles.
#'
#' @param x A numeric vector to anonymize
#' @param method Transformation method: "rank", "noise", or "quantile"
#' @param noise_sd Standard deviation for noise method (used when method = "noise") (default: NULL resulting in standard deviation being calculated)
#' @param quantile_dist_family Distribution family for quantile method: "normal", "uniform", "exponential" (used when method = "quantile")
#'
#' @export
anon_num_preserve_distribution <- function(
  x,
  method = c("rank", "noise", "quantile"),
  noise_sd = NULL,
  quantile_dist_family = c("normal", "uniform", "exponential")
) {
  method <- match.arg(method)

  # Handle missing values
  na_idx <- is.na(x)
  x_clean <- x[!na_idx]

  if (length(x_clean) == 0) {
    return(x)
  }

  if (length(x_clean) <= 1 || length(unique(x_clean)) <= 1) {
    final_result <- rep(NA_real_, length(x))
    final_result[!na_idx] <- as.numeric(x_clean)
    return(final_result)
  }

  result <- switch(
    method,
    "rank" = {
      # Rank-based transformation: preserves exact distribution shape
      ranks <- rank(x_clean, ties.method = "average")
      # Map ranks to new values drawn from same distribution
      sorted_values <- sort(x_clean)
      scrambled_values <- sample(sorted_values)
      # Interpolate for tied ranks
      stats::approx(
        seq_along(scrambled_values),
        scrambled_values,
        xout = ranks,
        rule = 2
      )$y
    },
    "noise" = {
      # Add calibrated noise to preserve mean and variance
      if (is.null(noise_sd)) {
        noise_sd <- stats::sd(x_clean) * 0.1
      }
      x_clean + stats::rnorm(length(x_clean), mean = 0, sd = noise_sd)
    },
    "quantile" = {
      # Quantile-based transformation using theoretical distribution
      quantile_dist_family <- match.arg(quantile_dist_family)
      n <- length(x_clean)

      # Get empirical quantiles
      empirical_quantiles <- (rank(x_clean) - 0.5) / n

      # Generate new values from theoretical distribution
      quantiles_to_use <- sample(empirical_quantiles)

      switch(
        quantile_dist_family,
        "normal" = stats::qnorm(
          quantiles_to_use,
          mean = mean(x_clean),
          sd = stats::sd(x_clean)
        ),
        "uniform" = stats::qunif(
          quantiles_to_use,
          min = min(x_clean),
          max = max(x_clean)
        ),
        "exponential" = stats::qexp(quantiles_to_use, rate = 1 / mean(x_clean))
      )
    }
  )

  # Restore missing values
  final_result <- rep(NA_real_, length(x))
  final_result[!na_idx] <- result
  final_result
}

#' Generalize numeric values into range categories
#'
#' Converts numeric values into range categories, either preserving actual
#' ranges or creating generic range labels.
#'
#' @param x A vector to anonymize
#' @param n_breaks Number of range categories to create, at least 1
#'   (default: 5)
#' @param method Method for creating breaks: "equal_width" or "equal_count"
#' @param clean_breaks Logical, whether to use pretty break points
#' @param scramble Logical, whether to randomize the assignment order of generated results
#' @param keep_values Logical, whether to keep actual range values or use generic labels
#'
#' @examples
#' values <- c(150, 165, 180, 175, 160, 190)
#' anon_num_range(values, n_breaks = 3)
#'
#' @export
anon_num_range <- function(
  x,
  n_breaks = 5,
  method = c("equal_width", "equal_count"),
  clean_breaks = TRUE,
  scramble = FALSE,
  keep_values = TRUE
) {
  method <- match.arg(method)

  if (
    !is.numeric(n_breaks) ||
      length(n_breaks) != 1 ||
      !is.finite(n_breaks) ||
      n_breaks < 1
  ) {
    stop("`n_breaks` must be a single number of at least 1.", call. = FALSE)
  }

  # Handle missing values
  na_idx <- is.na(x)
  x_clean <- x[!na_idx]

  if (length(x_clean) == 0) {
    return(x)
  }

  if (min(x_clean) == max(x_clean)) {
    result <- build_single_range_result(x_clean, keep_values)

    final_result <- rep(NA_character_, length(x))
    final_result[!na_idx] <- result
    return(final_result)
  }

  # Create breaks based on method
  if (method == "equal_width") {
    if (clean_breaks) {
      # Use pretty() to get clean break points
      breaks <- pretty(x_clean, n = n_breaks)
      # Ensure we cover the full range
      breaks[1] <- min(breaks[1], min(x_clean))
      breaks[length(breaks)] <- max(breaks[length(breaks)], max(x_clean))
    } else {
      breaks <- seq(min(x_clean), max(x_clean), length.out = n_breaks + 1)
    }
  } else {
    # equal_count
    if (clean_breaks) {
      # Get quantile breaks then make them prettier
      quantile_breaks <- stats::quantile(
        x_clean,
        probs = seq(0, 1, length.out = n_breaks + 1)
      )
      breaks <- pretty(
        c(min(x_clean), quantile_breaks, max(x_clean)),
        n = n_breaks
      )
      # Keep the pretty breaks inside the data and use the data's min and max
      # as the outer breaks, so every value falls in a range
      inside <- breaks[breaks > min(x_clean) & breaks < max(x_clean)]
      breaks <- c(min(x_clean), inside, max(x_clean))
    } else {
      breaks <- stats::quantile(
        x_clean,
        probs = seq(0, 1, length.out = n_breaks + 1)
      )
    }
  }

  breaks <- unique(as.numeric(breaks))
  if (length(breaks) < 2) {
    result <- build_single_range_result(x_clean, keep_values)

    final_result <- rep(NA_character_, length(x))
    final_result[!na_idx] <- result
    return(final_result)
  }

  # Cut the data into ranges
  ranges <- cut(x_clean, breaks = breaks, include.lowest = TRUE, right = FALSE)

  if (keep_values) {
    # Return the original range labels (e.g., [150,170), [170,190))
    result <- as.character(ranges)
  } else {
    # Get unique ranges for mapping
    unique_ranges <- levels(ranges)

    # Create anonymized range labels
    n_ranges <- length(unique_ranges)
    range_indices <- seq_len(n_ranges)

    if (scramble) {
      range_indices <- sample(range_indices)
    }

    # Create mapping from original ranges to anonymized ranges
    anon_labels <- paste0("Range_", sprintf("%02d", range_indices))
    range_mapping <- stats::setNames(anon_labels, unique_ranges)

    # Apply the mapping
    result <- as.character(range_mapping[as.character(ranges)])
  }

  # Restore missing values
  final_result <- rep(NA_character_, length(x))
  final_result[!na_idx] <- result
  final_result
}

# Labels every value with one range spanning the minimum and maximum of
# `values`, or with "Range_01" when `keep_values` is FALSE.
build_single_range_result <- function(values, keep_values) {
  if (keep_values) {
    lower <- format(min(values), trim = TRUE, scientific = FALSE)
    upper <- format(max(values), trim = TRUE, scientific = FALSE)
    rep(paste0("[", lower, ",", upper, "]"), length(values))
  } else {
    rep("Range_01", length(values))
  }
}

#' Replace phone numbers with pseudonymous numbers
#'
#' Replaces each distinct phone number matching common North American formats
#' (9 or 10 digits, with or without separators) with a sequential fake number.
#' Other formats are not detected, and other 9- or 10-digit numbers may be
#' replaced.
#'
#' @param x A character vector to anonymize
#' @param start Character prefix for generated results (default: "555-000")
#'
#' @examples
#' phones <- c("Call me at 123-456-7890", "1234567890", "(123) 456-7890")
#' anon_phone_number(phones)
#'
#' @export
anon_phone_number <- function(x, start = "555-000") {
  # Handle missing values
  na_idx <- is.na(x)
  x_clean <- x[!na_idx]

  if (length(x_clean) == 0) {
    return(x)
  }

  # Define phone number patterns (more comprehensive)
  patterns <- c(
    # 10 digits with various formatting
    "\\b\\d{3}[-.]\\d{3}[-.]\\d{4}\\b", # 123-456-7890 or 123.456.7890
    "\\(\\d{3}\\)\\s?\\d{3}[-.]?\\d{4}\\b", # (123) 456-7890 or (123)456-7890
    "\\b\\d{3}\\s\\d{3}\\s\\d{4}\\b", # 123 456 7890
    "\\b\\d{10}\\b", # 1234567890
    # 9 digits with formatting
    "\\b\\d{3}[-.]\\d{3}[-.]\\d{3}\\b", # 123-456-789
    "\\b\\d{9}\\b" # 123456789
  )

  # Find all unique phone numbers across all strings
  all_phones <- character(0)

  for (pattern in patterns) {
    phone_matches <- regmatches(
      x_clean,
      gregexpr(pattern, x_clean)
    )
    all_phones <- c(all_phones, unlist(phone_matches))
  }

  # Get unique phone numbers
  unique_phones <- unique(all_phones)

  if (length(unique_phones) == 0) {
    # No phone numbers found, return original
    final_result <- rep(NA_character_, length(x))
    final_result[!na_idx] <- x_clean
    return(final_result)
  }

  # Create anonymized phone numbers
  anon_phones <- paste0(start, "-", sprintf("%04d", seq_along(unique_phones)))
  phone_mapping <- stats::setNames(anon_phones, unique_phones)

  # Replace phone numbers in each string
  result <- x_clean
  for (i in seq_along(result)) {
    for (phone in names(phone_mapping)) {
      if (grepl(phone, result[i], fixed = TRUE)) {
        result[i] <- gsub(phone, phone_mapping[phone], result[i], fixed = TRUE)
      }
    }
  }

  # Restore missing values
  final_result <- rep(NA_character_, length(x))
  final_result[!na_idx] <- result
  final_result
}
