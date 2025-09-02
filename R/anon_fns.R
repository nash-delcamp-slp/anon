#' Anonymization Functions
#'
#' A collection of functions for anonymizing different types of data.
#'
#' @format A list containing the following anonymization functions:
#' \describe{
#'   \item{\code{date_shift}}{Shifts dates while preserving relative relationships}
#'   \item{\code{email}}{Anonymizes email addresses while preserving email-like structure}
#'   \item{\code{id_chr_sequence}}{Converts unique identifiers to sequential character IDs}
#'   \item{\code{id_num_sequence}}{Converts unique identifiers to sequential numeric IDs}
#'   \item{\code{num_preserve_distribution}}{Anonymizes numeric data while preserving distribution}
#'   \item{\code{num_range}}{Converts numeric values to range categories}
#'   \item{\code{phone_number}}{Anonymizes phone numbers with sequential fake numbers}
#' }
#'
#' @section date_shift:
#' Shifts dates to a new time period while preserving relative relationships
#' between dates. Supports both Date and POSIXct objects.
#'
#' Parameters:
#' \describe{
#'   \item{\code{x}}{Date or POSIXct vector to shift}
#'   \item{\code{center_date}}{New center point for the date range (default: current date)}
#'   \item{\code{scramble}}{Logical, whether to scramble dates while preserving intervals}
#' }
#'
#' @section email:
#' Anonymizes email addresses by replacing them with sequential fake emails,
#' only affecting strings that look like email addresses.
#'
#' Parameters:
#' \describe{
#'   \item{\code{x}}{Character vector potentially containing email addresses}
#'   \item{\code{start}}{Username prefix for generated emails (default: "user")}
#'   \item{\code{domain}}{Domain for generated emails (default: "domain.com")}
#' }
#'
#' @section id_chr_sequence:
#' Converts unique values to sequential character identifiers with customizable formatting.
#'
#' Parameters:
#' \describe{
#'   \item{\code{x}}{Vector of values to anonymize}
#'   \item{\code{scramble}}{Logical, whether to randomize the assignment order}
#'   \item{\code{start}}{Character prefix for generated IDs (default: "ID ")}
#'   \item{\code{padding}}{Logical, whether to zero-pad numbers for consistent width}
#'   \item{\code{padding_chr}}{Character used for padding (default: "0")}
#' }
#'
#' @section id_num_sequence:
#' Converts unique values to sequential numeric identifiers.
#'
#' Parameters:
#' \describe{
#'   \item{\code{x}}{Vector of values to anonymize}
#'   \item{\code{scramble}}{Logical, whether to randomize the assignment order}
#' }
#'
#' @section num_preserve_distribution:
#' Anonymizes numeric data while preserving distributional properties using
#' various transformation methods.
#'
#' Parameters:
#' \describe{
#'   \item{\code{x}}{Numeric vector to anonymize}
#'   \item{\code{method}}{Transformation method: "rank", "noise", or "quantile"}
#'   \item{\code{...}}{Additional method-specific parameters:}
#'   \item{}{  - \code{noise_sd}: Standard deviation for noise method}
#'   \item{}{  - \code{dist_family}: Distribution family for quantile method ("normal", "uniform", "exponential")}
#' }
#'
#' @section num_range:
#' Converts numeric values into range categories, either preserving actual ranges
#' or creating anonymized range labels.
#'
#' Parameters:
#' \describe{
#'   \item{\code{x}}{Numeric vector to categorize}
#'   \item{\code{n_breaks}}{Number of range categories to create (default: 5)}
#'   \item{\code{method}}{Method for creating breaks: "equal_width" or "equal_count"}
#'   \item{\code{clean_breaks}}{Logical, whether to use pretty break points}
#'   \item{\code{scramble}}{Logical, whether to randomize range label assignment}
#'   \item{\code{keep_values}}{Logical, whether to keep actual range values or use generic labels}
#' }
#'
#' @section phone_number:
#' Anonymizes phone numbers by replacing them with sequential fake phone numbers
#' in 555-xxxx format, only affecting strings that match phone number patterns.
#'
#' Parameters:
#' \describe{
#'   \item{\code{x}}{Character vector potentially containing phone numbers}
#' }
#'
#' @examples
#' # Character ID sequence
#' ids <- c("A123", "B456", "A123", "C789")
#' anon_fns$id_chr_sequence(ids)
#'
#' # Email anonymization
#' emails <- c("john@company.com", "not_an_email", "jane at work dot org")
#' anon_fns$email(emails)
#'
#' # Numeric range categorization
#' values <- c(150, 165, 180, 175, 160, 190)
#' anon_fns$num_range(values, n_breaks = 3)
#'
#' # Date shifting
#' dates <- as.Date(c("2023-01-15", "2023-02-20", "2023-03-10"))
#' anon_fns$date_shift(dates, center_date = "2024-06-01")
anon_fns <- list(
  date_shift = function(x, center_date = Sys.Date(), scramble = FALSE) {
    # Handle missing values
    na_idx <- is.na(x)
    x_clean <- x[!na_idx]

    if (length(x_clean) == 0) {
      return(x)
    }

    # Check if input is POSIXct (date-time)
    is_datetime <- inherits(x_clean, "POSIXct")

    # Convert to Date for centering calculation, preserving original class info
    if (is_datetime) {
      x_dates <- as.Date(x_clean)
    } else if (!inherits(x_clean, "Date")) {
      x_dates <- as.Date(x_clean)
    } else {
      x_dates <- x_clean
    }

    # Parse center_date if character
    if (is.character(center_date)) {
      # Handle flexible formats: "2020", "2020-01", "2020-01-01"
      if (grepl("^\\d{4}$", center_date)) {
        # Just year: "2020" -> "2020-01-01"
        center_date <- paste0(center_date, "-01-01")
      } else if (grepl("^\\d{4}-\\d{2}$", center_date)) {
        # Year-month: "2020-01" -> "2020-01-01"
        center_date <- paste0(center_date, "-01")
      }
      center_date <- as.Date(center_date)
    }

    # Calculate the offset from original center to new center
    original_center <- mean(x_dates, na.rm = TRUE)
    offset <- as.numeric(center_date - original_center)

    # Apply the shift
    if (is_datetime) {
      # For POSIXct, shift by days but preserve time
      result <- x_clean + (offset * 86400) # 86400 seconds per day

      # Optionally scramble while preserving intervals
      if (scramble) {
        # Calculate intervals from the shifted center (convert center to POSIXct)
        center_datetime <- as.POSIXct(paste(center_date, "00:00:00"))
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
      final_result <- rep(as.POSIXct(NA), length(x))
    } else {
      final_result <- rep(as.Date(NA), length(x))
    }
    final_result[!na_idx] <- result
    final_result
  },
  email = function(x, start = "user", domain = "domain.com") {
    # Handle missing values
    na_idx <- is.na(x)
    x_clean <- x[!na_idx]

    if (length(x_clean) == 0) {
      return(x)
    }

    # Identify email-like patterns: @ symbol OR "at ... dot" pattern
    has_at_symbol <- grepl("@", x_clean)
    has_at_dot_pattern <- grepl("\\bat\\b.*\\bdot\\b", x_clean, ignore.case = TRUE)
    is_email_like <- has_at_symbol | has_at_dot_pattern

    # Only anonymize email-like strings
    result <- x_clean
    if (any(is_email_like)) {
      x_to_anonymize <- x_clean[is_email_like]
      x_unique <- unique(x_to_anonymize)
      x_key <- paste0(start, sprintf("%03d", seq_along(x_unique)), "@", domain) |>
        setNames(x_unique)

      result[is_email_like] <- unname(x_key[x_to_anonymize])
    }

    # Restore missing values
    final_result <- rep(NA_character_, length(x))
    final_result[!na_idx] <- result
    final_result
  },
  id_chr_sequence = function(x, scramble = FALSE, start = "ID ", padding = TRUE, padding_chr = "0") {
    x_unique <- unique(x)
    if (scramble) x_unique <- sample(x_unique)

    x_key <- seq_along(x_unique)
    if (padding) {
      stopifnot(nchar(padding) == 1)
      max_chars <- max(nchar(x_key))
      x_key <- sprintf(paste0("%", padding_chr, max_chars, "d"), x_key)
    }

    x_key <- paste0(start, x_key) |>
      setNames(x_unique)

    unname(x_key[x])
  },
  id_num_sequence = function(x, scramble = FALSE) {
    x_unique <- unique(x)
    if (scramble) x_unique <- sample(x_unique)

    x_key <- seq_along(x_unique) |>
      setNames(x_unique)

    unname(x_key[x])
  },
  num_preserve_distribution = function(x, method = c("rank", "noise", "quantile"), ...) {
    method <- match.arg(method)

    # Handle missing values
    na_idx <- is.na(x)
    x_clean <- x[!na_idx]

    if (length(x_clean) == 0) {
      return(x)
    }

    result <- switch(method,
      "rank" = {
        # Rank-based transformation: preserves exact distribution shape
        ranks <- rank(x_clean, ties.method = "average")
        # Map ranks to new values drawn from same distribution
        sorted_values <- sort(x_clean)
        scrambled_values <- sample(sorted_values)
        # Interpolate for tied ranks
        approx(seq_along(scrambled_values), scrambled_values,
          xout = ranks, rule = 2
        )$y
      },
      "noise" = {
        # Add calibrated noise to preserve mean and variance
        noise_sd <- list(...)$noise_sd %||% sd(x_clean) * 0.1
        x_clean + rnorm(length(x_clean), mean = 0, sd = noise_sd)
      },
      "quantile" = {
        # Quantile-based transformation using theoretical distribution
        dist_family <- list(...)$dist_family %||% "normal"
        n <- length(x_clean)

        # Get empirical quantiles
        empirical_quantiles <- (rank(x_clean) - 0.5) / n

        # Generate new values from theoretical distribution
        quantiles_to_use <- sample(empirical_quantiles)

        switch(dist_family,
          "normal" = qnorm(quantiles_to_use,
            mean = mean(x_clean), sd = sd(x_clean)
          ),
          "uniform" = qunif(quantiles_to_use,
            min = min(x_clean), max = max(x_clean)
          ),
          "exponential" = qexp(quantiles_to_use,
            rate = 1 / mean(x_clean)
          )
        )
      }
    )

    # Restore missing values
    final_result <- rep(NA_real_, length(x))
    final_result[!na_idx] <- result
    final_result
  },
  num_range = function(x, n_breaks = 5, method = c("equal_width", "equal_count"),
                       clean_breaks = TRUE, scramble = FALSE, keep_values = TRUE) {
    method <- match.arg(method)

    # Handle missing values
    na_idx <- is.na(x)
    x_clean <- x[!na_idx]

    if (length(x_clean) == 0) {
      return(x)
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
    } else { # equal_count
      if (clean_breaks) {
        # Get quantile breaks then make them prettier
        quantile_breaks <- quantile(x_clean, probs = seq(0, 1, length.out = n_breaks + 1))
        breaks <- pretty(c(min(x_clean), quantile_breaks, max(x_clean)), n = n_breaks)
        breaks <- breaks[breaks >= min(x_clean) & breaks <= max(x_clean)]
        # Ensure we have enough breaks
        if (length(breaks) < 2) {
          breaks <- c(min(x_clean), max(x_clean))
        }
      } else {
        breaks <- quantile(x_clean, probs = seq(0, 1, length.out = n_breaks + 1))
      }
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
      range_mapping <- setNames(anon_labels, unique_ranges)

      # Apply the mapping
      result <- as.character(range_mapping[as.character(ranges)])
    }

    # Restore missing values
    final_result <- rep(NA_character_, length(x))
    final_result[!na_idx] <- result
    final_result
  },
  phone_number = function(x) {
    # Handle missing values
    na_idx <- is.na(x)
    x_clean <- x[!na_idx]

    if (length(x_clean) == 0) {
      return(x)
    }

    # Pattern to match phone numbers: 7-11 digits with optional formatting
    # Allows hyphens, dots, spaces, parentheses between digit groups
    phone_pattern <- "\\b(?:\\+?1[-\\s\\.]?)?(?:\\(?[0-9]{3}\\)?[-\\s\\.]?)?[0-9]{3}[-\\s\\.][0-9]{4}\\b|\\b[0-9]{7,11}\\b"
    is_phone_like <- grepl(phone_pattern, x_clean)

    # Only anonymize phone-like strings
    result <- x_clean
    if (any(is_phone_like)) {
      x_to_anonymize <- x_clean[is_phone_like]
      x_unique <- unique(x_to_anonymize)

      # Generate sequential anonymous phone numbers
      base_number <- 5550001 # Start with 555-0001
      phone_numbers <- base_number + seq_along(x_unique) - 1

      # Format as phone numbers (555-0001, 555-0002, etc.)
      anonymous_phones <- sprintf(
        "%03d-%03d-%04d",
        phone_numbers %/% 10000,
        (phone_numbers %% 10000) %/% 10,
        phone_numbers %% 10
      )

      x_key <- anonymous_phones |>
        setNames(x_unique)

      result[is_phone_like] <- unname(x_key[x_to_anonymized])
    }

    # Restore missing values
    final_result <- rep(NA_character_, length(x))
    final_result[!na_idx] <- result
    final_result
  }
)

class(anon_fns) <- "fn_list"

#' Print method for list of functions
#' @param x A list of funtions with class `fn_list`
#' @param ... Additional arguments (ignored)
#' @export
print.fn_list <- function(x, ...) {
    
  for (i in seq_along(x)) {
    fn_name <- names(x)[i]
    fn <- x[[i]]
    
    # Extract function arguments
    args <- formals(fn)
    
    # Format arguments with defaults
    arg_strings <- character(length(args))
    for (j in seq_along(args)) {
      arg_name <- names(args)[j]
      arg_default <- args[[j]]
      
      if (missing(arg_default) || is.name(arg_default) && as.character(arg_default) == "") {
        # No default value
        arg_strings[j] <- arg_name
      } else {
        # Has default value
        if (is.character(arg_default)) {
          arg_strings[j] <- paste0(arg_name, ' = "', arg_default, '"')
        } else if (is.logical(arg_default)) {
          arg_strings[j] <- paste0(arg_name, " = ", toupper(as.character(arg_default)))
        } else {
          arg_strings[j] <- paste0(arg_name, " = ", deparse(arg_default))
        }
      }
    }
    
    # Combine arguments
    args_str <- paste(arg_strings, collapse = ", ")
    
    cat(sprintf("%d. anon_fns$%s(%s)\n", i, fn_name, args_str))
  }
  
  invisible(x)
}
