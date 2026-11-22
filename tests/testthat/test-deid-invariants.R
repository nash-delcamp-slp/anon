# Privacy and fidelity invariants for every de-identification pipeline. The
# fixture, pipelines, exits and known gaps live in helper-deid.R; register new
# functions and new ways out of the package there.

# The value each fixture column should have after de-identification, given
# each row's original key: keys map through the secrets, SUBJID becomes the
# pseudonym's number, dates move by the subject's offset, and everything else
# is unchanged. Full dates move by the offset (POSIXct by calendar days,
# keeping the clock time), year-and-month dates from day 15 of the month,
# year-only dates by whole years (at least one, in the subject's own random
# direction, kept in the secrets apart from the offset), each end of an
# interval on its own; durations are kept. In rows without a key, the key
# stays blank and SUBJID and dates are removed (set to the column's missing
# value).
deid_date_cols <- c(
  "RFSTDTC", "rficdtc", "RFSTDT", "VSDTC", "VSDTM", "VSNYDTM", "EXSTDTC", "EXENDTC", "QVAL",
  "PRSTDTC", "PRENDTC", "PRRNDTC", "PRDUDTC", "PRMTDTC", "PRYDDTC", "PRYRDTC",
  # ADaM dates of the profile fixture.
  "RANDDT", "TRTSDT", "TRTEDT", "ADT",
  # Inside nested data.
  "DT", "VSTM", "LBDTC", "RPDT", "MXDTC", "PKDTC", "SPDTC", "SPDT"
)

# The missing value a text column already uses: "" if it has blank strings,
# otherwise NA.
deid_missing_like <- function(x) {
  if (any(!is.na(x) & x == "")) "" else NA_character_
}

deid_shift_text <- function(x, off, direction) {
  if (is.na(x) || !nzchar(x) || grepl("^P", x)) {
    return(x)
  }
  if (grepl("/", x, fixed = TRUE)) {
    ends <- strsplit(x, "/", fixed = TRUE)[[1]]
    return(paste(vapply(ends, deid_shift_text, character(1), off = off, direction = direction, USE.NAMES = FALSE), collapse = "/"))
  }
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)) {
    return(paste0(format(as.Date(substr(x, 1, 10)) + off), substring(x, 11)))
  }
  if (grepl("^[0-9]{4}-[0-9]{2}($|--)", x)) {
    return(paste0(format(as.Date(paste0(substr(x, 1, 7), "-15")) + off, "%Y-%m"), substring(x, 8)))
  }
  if (grepl("^[0-9]{4}($|---)", x)) {
    years <- direction * max(1, round(abs(off) / 365.25))
    return(paste0(sprintf("%04d", as.integer(substr(x, 1, 4)) + years), substring(x, 5)))
  }
  stop("No expected value for date ", x)
}

deid_expected_column <- function(x, col, keys, oracle, prefix, key) {
  if (is.list(x)) {
    return(deid_expected_nested(x, keys, oracle, prefix, key, keyed = TRUE))
  }
  off <- unname(oracle$offset[keys])
  direction <- unname(oracle$year_direction[keys])
  if (col %in% c(key, "RSUBJID")) {
    # A key's haven missing-value codes become missing; numeric keys get
    # numbers.
    x <- deid_zap_codes(x)
    k <- as.character(x)
    new <- ifelse(is.na(k) | k == "", k, unname(oracle$new_key[k]))
    return(if (is.numeric(x)) as.numeric(new) else new)
  }
  if (col == "SUBJID") {
    out <- sub(prefix, "", unname(oracle$new_key[keys]), fixed = TRUE)
    out[is.na(out) & !is.na(x)] <- deid_missing_like(as.character(x))
    return(out)
  }
  if (!col %in% deid_date_cols) {
    return(x)
  }
  if (inherits(x, c("Date", "POSIXct"))) {
    return(deid_shift_value(x, off))
  }
  x <- as.character(x)
  present <- !is.na(x) & nzchar(x)
  no_key <- is.na(off)
  out <- x
  out[!no_key] <- vapply(which(!no_key), function(i) deid_shift_text(x[i], off[i], direction[i]), character(1))
  out[no_key & present] <- deid_missing_like(x)
  out
}

# A Date or POSIXct value moved by `off` days (POSIXct by calendar days,
# keeping the clock time); missing without an offset.
deid_shift_value <- function(x, off) {
  if (inherits(x, "Date")) {
    return(x + off)
  }
  tz <- attr(x, "tzone")
  lt <- as.POSIXlt(x, tz = tz)
  lt$mday <- lt$mday + off
  lt$isdst <- rep(-1L, length(x))
  as.POSIXct(lt, tz = tz)
}

# Nested data, as each pipeline should leave it: a packed data frame column's
# rows belong to their outer rows' subjects (`keys`), each element of a list
# column to its row's subject, and a data frame with its own key column to the
# subjects in it, whose rows are then sorted by the new key, as in a dataset.
# Values are expected as at the top level (in a keyed context; unchanged
# otherwise), with their type and attributes, and the text that describes the
# data rewritten (see deid_rewrite_frame()). Date and date-time vectors in
# list columns move by their row's subject's offset. Objects that can't be
# de-identified are left as they are.
deid_expected_nested <- function(x, keys, oracle, prefix, key, keyed) {
  if (is.data.frame(x)) {
    # A packed column: its rows are its outer rows'.
    return(deid_expected_frame(x, keys, oracle, prefix, key, keyed, filter = FALSE))
  }
  if (!deid_plain_list(x)) {
    return(x)
  }
  cls <- oldClass(x)
  out <- unclass(x)
  for (i in seq_along(out)) {
    if (!is.null(out[[i]])) out[[i]] <- deid_expected_element(out[[i]], keys[i], oracle, prefix, key, keyed)
  }
  if (inherits(x, "vctrs_list_of")) {
    # The elements of a list_of share one type: a replaced or shifted factor
    # in them has as levels all the new values found in any of them, sorted.
    for (col in deid_list_of_factors(out, c(key, deid_date_cols))) {
      levels_all <- sort(unique(unlist(lapply(Filter(Negate(is.null), out), function(e) as.character(e[[col]])))))
      for (i in seq_along(out)) {
        if (!is.null(out[[i]])) out[[i]][[col]] <- deid_relevel(out[[i]][[col]], levels_all)
      }
    }
  }
  class(out) <- cls
  out <- deid_rewrite_attributes(out, oracle)
  if (inherits(x, "vctrs_list_of")) {
    attr(out, "ptype") <- deid_expected_ptype(x, out, oracle, prefix, key, keyed)
  }
  out
}

# The names of the factor columns, among `cols`, of a list_of's data frames.
deid_list_of_factors <- function(x, cols) {
  elements <- Filter(Negate(is.null), unclass(x))
  if (length(elements) == 0 || !is.data.frame(elements[[1]])) {
    return(character(0))
  }
  intersect(names(elements[[1]])[vapply(elements[[1]], is.factor, logical(1))], cols)
}

# A factor with the same values and attributes and the given levels.
deid_relevel <- function(f, levels) {
  a <- attributes(f)
  out <- factor(as.character(f), levels = levels)
  attributes(out) <- c(attributes(out), a[setdiff(names(a), c("levels", "class"))])
  out
}

# A list_of's prototype as each pipeline should leave it: a data frame of no
# rows, de-identified like its elements (a replaced or shifted factor keeps
# no levels when the elements give it none; it would otherwise hold keys or
# real dates).
# Its factor columns have the levels its expected elements (`elements`) hold
# (in the old prototype's order when they are the same).
deid_expected_ptype <- function(x, elements, oracle, prefix, key, keyed) {
  ptype <- attr(x, "ptype", exact = TRUE)
  if (!is.data.frame(ptype)) {
    return(deid_rewrite_attributes(ptype, oracle))
  }
  out <- deid_expected_frame(ptype, character(0), oracle, prefix, key, keyed || key %in% names(ptype), sort = TRUE)
  present <- Filter(Negate(is.null), unclass(elements))
  for (col in names(out)[vapply(out, is.factor, logical(1))]) {
    held <- unique(unlist(lapply(present, function(e) levels(e[[col]]))))
    if (!setequal(held, levels(ptype[[col]]))) {
      out[[col]] <- deid_relevel(out[[col]], held)
    }
  }
  out
}

deid_expected_element <- function(e, owner, oracle, prefix, key, keyed) {
  if (is.data.frame(e)) {
    return(deid_expected_frame(e, rep(owner, nrow(e)), oracle, prefix, key, keyed, sort = TRUE))
  }
  if (isS4(e)) {
    return(e)
  }
  if (inherits(e, "POSIXlt")) {
    if (!keyed) {
      return(e)
    }
    tz <- attr(e, "tzone")
    out <- as.POSIXlt(deid_shift_value(as.POSIXct(e), unname(oracle$offset[owner])), tz = tz[[1]])
    attr(out, "tzone") <- tz
    return(out)
  }
  if (inherits(e, c("Date", "POSIXct")) && keyed) {
    e[] <- deid_shift_value(e, unname(oracle$offset[owner]))
  }
  if (is.atomic(e)) {
    return(deid_rewrite_attributes(e, oracle))
  }
  if (!deid_plain_list(e)) {
    return(e)
  }
  cls <- oldClass(e)
  out <- unclass(e)
  for (i in seq_along(out)) {
    if (!is.null(out[[i]])) out[[i]] <- deid_expected_element(out[[i]], owner, oracle, prefix, key, keyed)
  }
  class(out) <- cls
  deid_rewrite_attributes(out, oracle)
}

# A data frame as each pipeline should leave it, its rows belonging to `keys`
# (NA: no subject) unless it has subjects of its own (see
# deid_frame_owners(); those found through a list column only when its rows
# don't already belong to subjects). With `sort` (a data frame held in a list
# column, or a dataset), rows with subjects of their own are sorted by the new
# key; inside nested data, with subjects, a data frame's row names are reset.
# With `filter`, rows with subjects of their own that the pipeline drops
# (`oracle$dropped`) are removed; a packed column (`filter = FALSE`) keeps
# its rows, which are its outer rows'.
deid_expected_frame <- function(df, keys, oracle, prefix, key, keyed, sort = FALSE, nested = TRUE, filter = TRUE) {
  o <- deid_frame_owners(df, key)
  own <- !is.null(o) && (o$from != "list" || !keyed)
  if (own) {
    keys <- o$keys
  }
  keyed <- keyed || own
  cls <- oldClass(df)
  x <- unclass(df)
  for (j in seq_along(x)) {
    v <- x[[j]]
    x[[j]] <- if (is.list(v)) {
      deid_expected_nested(v, keys, oracle, prefix, key, keyed)
    } else if (keyed) {
      values <- deid_expected_column(v, names(x)[j], keys, oracle, prefix, key)
      if (is.factor(v) && names(x)[j] %in% c(key, deid_date_cols)) {
        # A replaced or shifted factor has the new values as levels.
        a <- attributes(v)
        v <- factor(as.character(values))
        attributes(v) <- c(attributes(v), a[setdiff(names(a), c("levels", "class", "contrasts", "names"))])
      } else if (!identical(values, v)) {
        v[] <- values
      }
      deid_rewrite_attributes(v, oracle)
    } else {
      deid_rewrite_attributes(v, oracle)
    }
  }
  # Row names are reset when the rows are sorted. Inside nested data with
  # subjects, row names that are row positions (integer, or digits) are reset
  # too; other row names of data frames belonging to their outer row are
  # kept, rewritten.
  rn <- attr(df, "row.names")
  positions <- is.numeric(rn) || all(grepl("^[0-9]+$", rn))
  x <- deid_rewrite_frame_text(x, df, cls, oracle)
  if (filter && own && length(oracle$dropped) > 0) {
    kept <- is.na(keys) | !keys %in% oracle$dropped
    x <- vctrs::vec_slice(x, kept)
    keys <- keys[kept]
  }
  if (sort && own && nrow(x) > 1) {
    x <- vctrs::vec_slice(x, order(unname(oracle$new_key[keys]), seq_along(keys), na.last = TRUE))
  }
  if (sort && own) {
    x <- deid_no_row_names(x, packed = TRUE)
  } else if (nested && keyed && positions) {
    x <- deid_no_row_names(x, packed = FALSE)
  }
  x
}

# A data frame (or tibble) with automatic row names; with `packed`, its packed
# data frame columns too, at any depth (their rows move with its rows).
deid_no_row_names <- function(x, packed) {
  attr(x, "row.names") <- seq_len(nrow(x))
  attr(x, "row.names") <- .set_row_names(nrow(x))
  if (packed) {
    cls <- oldClass(x)
    y <- unclass(x)
    for (j in seq_along(y)) {
      if (is.data.frame(y[[j]])) y[[j]] <- deid_no_row_names(y[[j]], packed = TRUE)
    }
    class(y) <- cls
    x <- y
  }
  x
}

# The oracle for text in attributes and names: the step's own, or, for a
# pipeline without secrets, that of the pipeline it must match.
deid_text_oracle <- function(pipeline, step) {
  if (isTRUE(pipeline$keep_secrets)) {
    return(deid_oracle(step, pipeline$other_attributes, pipeline$dropped))
  }
  if (is.null(pipeline$same_as)) {
    stop("A pipeline without secrets needs `same_as` to check its attributes and names.")
  }
  twin <- deid_complete_pipeline(deid_pipelines[[pipeline$same_as]])
  deid_oracle(deid_run(twin, twin$fixture())$step, pipeline$other_attributes, twin$dropped)
}

# Columns of dataset `ds` named in a pipeline's `removed` ("ds.col").
deid_removed_columns <- function(removed, ds) {
  prefix <- paste0(ds, ".")
  substring(removed[startsWith(removed, prefix)], nchar(prefix) + 1)
}

for (pipeline_name in names(deid_pipelines)) {
  pipeline <- deid_complete_pipeline(deid_pipelines[[pipeline_name]])
  key <- pipeline$key

  test_that(paste0(pipeline_name, ": the pipeline's offsets keep the fixture's sentinels apart"), {
    expect_gt(pipeline$min_days, deid_min_spacing)
    expect_gt(as.numeric(min(diff(deid_all_starts))), pipeline$max_days + deid_min_spacing)
  })

  test_that(paste0(pipeline_name, ": what can't be de-identified is named in a warning, with no other column"), {
    run <- deid_run(pipeline, pipeline$fixture())
    # Warnings print locations with keys and dates rewritten.
    text_oracle <- deid_text_oracle(pipeline, run$step)
    flagged <- lapply(pipeline$flagged, deid_rewrite_flag, oracle = text_oracle)
    for (f in flagged) {
      pattern <- deid_flag_pattern(f)
      expect_true(any(grepl(pattern, run$warnings)), info = pattern)
    }
    reasons <- Filter(Negate(is.null), lapply(flagged, `[[`, "reason"))
    for (reason in unique(unlist(reasons))) {
      expected <- unlist(lapply(flagged, function(f) {
        if (identical(f$reason, reason)) paste0(f$dataset, ".", f$column)
      }))
      for (w in grep(reason, run$warnings, fixed = TRUE, value = TRUE)) {
        expect_setequal(deid_listed_columns(w), expected)
      }
    }
  })

  test_that(paste0(pipeline_name, ": every exit runs, and no original ID, real date or user term leaves the package"), {
    run <- deid_run(pipeline, pipeline$fixture())
    expect_identical(!is.null(run$step$secrets), pipeline$keep_secrets)
    step <- deid_drop_flagged(run$step, pipeline$flagged)
    results <- deid_run_exits(step)

    errored <- names(results)[vapply(results, function(r) r$errored, logical(1))]
    expect_setequal(errored, deid_expected_errors(step))
    for (name in setdiff(names(results), errored)) {
      r <- results[[name]]
      expect_true(length(r$raw) > 0 || any(nzchar(r$text)), info = paste("exit returned nothing:", name))
    }

    results$pipeline <- list(text = c(run$warnings, run$messages), patterns = FALSE)
    expect_equal(deid_leaks(results), character(0))
  })

  if (isTRUE(pipeline$keep_secrets)) {
    test_that(paste0(pipeline_name, ": redacted copies made with anon() carry no secrets"), {
      step <- deid_run(pipeline, pipeline$fixture())$step
      # The S4 object, the call and the function the fixture holds in
      # attributes are named in one warning, which must name nothing else.
      s4_only <- function(expr) {
        expected <- c("`VSSTRESN` (attribute `DRUGQX_call`)", "`VSSTRESN@DRUGQX_env$f`")
        withCallingHandlers(expr, warning = function(w) {
          m <- conditionMessage(w)
          listed <- sub("\\. Remove .*$", "", sub("^anon\\(\\) can't redact [^:]*: ", "", m))
          locations <- strsplit(listed, ", ", fixed = TRUE)[[1]]
          s4 <- grepl("^`[^`]*` \\(attribute `note`\\)$", locations)
          if (!grepl("\n", m) && startsWith(m, "anon() can't redact") && sum(s4) == 1 &&
            setequal(locations[!s4], expected)) {
            invokeRestart("muffleWarning")
          }
        })
      }
      expect_null(s4_only(anon(step))$secrets)
      expect_null(s4_only(anon(list(a = list(step = step))))$a$step$secrets)
    })

    test_that(paste0(pipeline_name, ": offsets and pseudonyms don't follow a pattern"), {
      step <- deid_run(pipeline, pipeline$fixture())$step
      oracle <- deid_oracle(step, pipeline$other_attributes, pipeline$dropped)
      n <- length(deid_keys)
      expect_setequal(names(oracle$new_key), pipeline$keys)
      expect_setequal(names(oracle$offset), pipeline$keys)

      off <- oracle$offset
      expect_true(all(abs(off) >= pipeline$min_days))
      expect_true(all(abs(off) <= pipeline$max_days))
      # No offset is shared by most subjects.
      expect_lte(max(table(off)), n / 2)
      expect_gte(length(unique(off)), 3)

      # Pseudonyms are not numbered in the original key order, in reverse, or
      # in a rotation of it.
      numbers <- as.integer(sub(pipeline$prefix, "", oracle$new_key[pipeline$keys[seq_len(n)]], fixed = TRUE))
      s <- seq_len(n)
      patterned <- c(lapply(s - 1L, function(k) c(utils::tail(s, n - k), utils::head(s, k))), list(rev(s)))
      expect_false(any(vapply(patterned, identical, logical(1), numbers)))
    })

    test_that(paste0(pipeline_name, ": every value matches its subject's pseudonym and offset, and the rest is unchanged"), {
      data <- pipeline$fixture()
      step <- deid_run(pipeline, data)$step
      oracle <- deid_oracle(step, pipeline$other_attributes, pipeline$dropped)
      flagged_datasets <- unlist(lapply(pipeline$flagged, function(f) if (!is.null(f$dataset) && is.null(f$column)) f$dataset))
      flagged_columns <- unlist(lapply(pipeline$flagged, function(f) if (!is.null(f$column)) paste0(f$dataset, ".", f$column)))

      for (ds in setdiff(names(data), pipeline$removed)) {
        out <- step$data[[deid_rewrite_text(ds, oracle, names = TRUE)]]
        original <- data[[ds]]
        # A flagged dataset is documented as left unchanged, apart from keys
        # and dates in its attributes and names. A dataset without the key
        # column is too, unless its rows get subjects through nested data
        # (and are then sorted and shifted), and nested data frames with the
        # key are de-identified.
        if (ds %in% flagged_datasets) {
          expect_identical(out, deid_rewrite_frame(original, oracle), info = ds)
          next
        }
        if (!key %in% names(original)) {
          no_key <- rep(NA_character_, nrow(original))
          want <- deid_expected_frame(original, no_key, oracle, pipeline$prefix, key, FALSE, sort = TRUE, nested = FALSE)
          expect_identical(out, want, info = ds)
          next
        }
        keep <- pipeline$keep_rows(ds, original, oracle)
        all_keys <- as.character(deid_zap_codes(original[[key]]))
        keys <- all_keys[keep]
        # Rows are sorted by the new key, keeping their order within a
        # subject; rows without a key come last.
        ord <- order(unname(oracle$new_key[keys]), seq_along(keys))
        out_keys <- as.character(out[[key]])
        blank <- is.na(out_keys) | out_keys == ""
        expect_false(is.unsorted(out_keys[!blank]), info = ds)
        expect_identical(blank, sort(blank), info = ds)

        for (col in setdiff(names(original), deid_removed_columns(pipeline$removed, ds))) {
          id <- paste0(ds, ".", col)
          expected <- if (!is.null(pipeline$expected[[id]])) {
            pipeline$expected[[id]](original[[col]], all_keys, oracle)
          } else if (id %in% flagged_columns) {
            original[[col]]
          } else {
            deid_expected_column(original[[col]], col, all_keys, oracle, pipeline$prefix, key)
          }
          got <- out[[deid_rewrite_text(col, oracle, names = TRUE)]]
          if (is.data.frame(got)) {
            # A packed data frame column: its rows move with the data, which
            # is sorted, so its row names are reset.
            expect_identical(got, deid_no_row_names(vctrs::vec_slice(expected, which(keep)[ord]), packed = TRUE), info = id)
          } else if (is.factor(got) || is.character(got)) {
            expect_identical(as.character(got), as.character(expected)[keep][ord], info = id)
          } else if (is.list(got)) {
            # A list column: its elements (its own attributes are checked
            # with the shape of the data).
            elements <- function(x) {
              x <- unclass(x)
              attributes(x) <- NULL
              x
            }
            expect_identical(elements(got), elements(expected)[keep][ord], info = id)
          } else {
            expect_identical(as.vector(unclass(got)), as.vector(unclass(expected))[keep][ord], info = id)
          }
          # Names of a vector column follow its values, rewritten.
          if (!is.data.frame(got)) {
            want_names <- names(original[[col]])
            if (!is.null(want_names)) {
              want_names <- deid_rewrite_text(want_names, oracle, names = TRUE)[keep][ord]
            }
            expect_identical(names(got), want_names, info = id)
          }
        }
      }
    })
  }

  if (!is.null(pipeline$risk)) {
    test_that(paste0(pipeline_name, ": the released data is k-anonymous for the target the input misses"), {
      r <- pipeline$risk
      data <- pipeline$fixture()
      before <- anon_risk(data, quasi = r$quasi, max_risk = r$max_risk)
      expect_false(before$summary$meets_target)
      step <- deid_run(pipeline, data)$step
      oracle <- deid_oracle(step, pipeline$other_attributes, pipeline$dropped)
      truth <- if (is.null(r$truth)) deid_release_truth else r$truth
      expect_true(all(deid_release_k(data, step, oracle, truth) >= risk_k_target(r$max_risk)))
      expect_true(step$settings$suppress$meets_target)
      if (!is.null(step$settings$deidentify)) {
        # The measured risk meets the target; what the fixture holds that
        # the profile can't protect is each kind of reason it isn't met.
        expect_true(step$risk_after$summary$meets_target)
        expect_setequal(step$settings$deidentify$not_met, pipeline$not_met)
        expect_identical(step$settings$deidentify$meets_target, length(pipeline$not_met) == 0)
      }
      # Measured from the step's record of blanked cells too.
      expect_true(anon_risk(step, quasi = r$quasi, max_risk = r$max_risk)$summary$meets_target)
    })
  }

  if (!is.null(pipeline$deidentify)) {
    test_that(paste0(pipeline_name, ": removed columns are blank, copies agree, and derived columns follow their quasi-identifier"), {
      checks <- pipeline$deidentify
      data <- pipeline$fixture()
      step <- deid_run(pipeline, data)$step
      oracle <- deid_oracle(step, pipeline$other_attributes, pipeline$dropped)
      out <- step$data
      blank <- function(x) {
        x <- if (is.factor(x)) as.character(x) else unclass(x)
        is.na(x) | (is.character(x) & !is.na(x) & trimws(x) == "")
      }
      for (id in checks$removed) {
        parts <- strsplit(id, ".", fixed = TRUE)[[1]]
        expect_true(all(blank(out[[parts[1]]][[parts[2]]])), info = id)
      }
      # A copy's value for each subject (by original key): every row of a
      # column copy must agree; a test copy gives its first matching row.
      copy_values <- function(spec) {
        ds <- sub("\\..*$", "", spec)
        col <- sub("\\[.*$", "", sub("^[^.]*\\.", "", spec))
        df <- out[[ds]]
        keys <- names(oracle$new_key)[match(as.character(df[[key]]), oracle$new_key)]
        rows <- rep(TRUE, nrow(df))
        if (grepl("[", spec, fixed = TRUE)) {
          for (cond in strsplit(sub("^.*\\[(.*)\\]$", "\\1", spec), ",", fixed = TRUE)[[1]]) {
            kv <- strsplit(cond, "=", fixed = TRUE)[[1]]
            rows <- rows & as.character(df[[kv[1]]]) %in% kv[2]
          }
        }
        text <- ifelse(blank(df[[col]]), "<blank>", as.character(df[[col]]))
        values <- text[rows]
        k <- keys[rows]
        if (!grepl("[", spec, fixed = TRUE)) {
          # Every row of a subject shows one value.
          expect_true(all(tapply(values, k, function(v) length(unique(v))) == 1), info = spec)
        }
        stats::setNames(values[!duplicated(k)], k[!duplicated(k)])[deid_keys]
      }
      for (q in names(checks$copies)) {
        specs <- checks$copies[[q]]
        reference <- copy_values(specs[1])
        for (spec in specs[-1]) {
          expect_identical(copy_values(spec), reference, info = paste(q, spec))
        }
      }
      # Derived values: the row's original value where shown, and one value
      # (or blank) per released value of the quasi-identifier.
      for (id in names(checks$derived)) {
        ds <- sub("\\..*$", "", id)
        col <- sub("^[^.]*\\.", "", id)
        from <- sub("^[^.]*\\.", "", checks$derived[[id]])
        original <- data[[ds]]
        keys <- names(oracle$new_key)[match(as.character(out[[ds]][[key]]), oracle$new_key)]
        # Rows keep their order within a subject.
        occurrence <- stats::ave(seq_along(keys), keys, FUN = seq_along)
        original_rows <- vapply(seq_along(keys), function(i) {
          which(as.character(original[[key]]) == keys[i])[occurrence[i]]
        }, integer(1))
        got <- out[[ds]][[col]]
        shown <- !blank(got)
        expect_identical(as.character(got[shown]), as.character(original[[col]][original_rows][shown]), info = id)
        released <- ifelse(blank(out[[ds]][[from]]), "<blank>", as.character(out[[ds]][[from]]))
        derived <- ifelse(shown, as.character(got), "<blank>")
        expect_true(all(tapply(derived, released, function(v) length(unique(v))) == 1), info = id)
      }
    })
  }

  if (!is.null(pipeline$same_as)) {
    test_that(paste0(pipeline_name, ": gives the same data as ", pipeline$same_as, ", and holds nothing to link back"), {
      step <- deid_run(pipeline, pipeline$fixture())$step
      twin_pipeline <- deid_complete_pipeline(deid_pipelines[[pipeline$same_as]])
      twin <- deid_run(twin_pipeline, twin_pipeline$fixture())$step
      expect_identical(step$data, twin$data)
      saved <- list(serialize = list(raw = serialize(deid_drop_flagged(step, pipeline$flagged), NULL), patterns = FALSE))
      expect_equal(deid_leaks(saved), character(0))
    })
  }

  test_that(paste0(pipeline_name, ": the shape of the data and its attributes are kept"), {
    data <- pipeline$fixture()
    step <- deid_run(pipeline, data)$step
    out <- step$data
    oracle <- if (isTRUE(pipeline$keep_secrets)) deid_oracle(step, pipeline$other_attributes, pipeline$dropped)
    # Keys and dates in attributes and names are rewritten.
    text_oracle <- deid_text_oracle(pipeline, step)
    rewrite <- function(x) deid_rewrite_text(x, text_oracle, names = TRUE)
    kept <- setdiff(names(data), pipeline$removed)
    expect_named(out, rewrite(kept))
    replaced_cols <- c(key, "SUBJID", "RSUBJID", deid_date_cols)
    # Attributes that change on purpose: row names (reset), factor levels
    # (they hold keys or dates), and haven value labels and missing-value
    # codes (they describe the original values, and are dropped from replaced
    # and shifted columns; elsewhere they are checked below). Names are
    # checked on their own, a column's with its values.
    changing <- c("row.names", "levels", "labels", "na_values", "na_range")
    keep <- function(x, also = character(0)) {
      a <- attributes(x)
      a[sort(setdiff(names(a), c(changing, also)))]
    }
    for (ds in kept) {
      got <- out[[rewrite(ds)]]
      expected <- deid_rewrite_frame(data[[ds]], text_oracle)
      cols <- setdiff(names(data[[ds]]), deid_removed_columns(pipeline$removed, ds))
      expect_named(got, rewrite(cols))
      expect_identical(nrow(got), sum(pipeline$keep_rows(ds, data[[ds]], oracle)), info = ds)
      expect_identical(keep(got, "names"), keep(expected, "names"), info = ds)
      for (col in cols) {
        id <- paste0(ds, ".", col)
        x <- got[[rewrite(col)]]
        type <- if (is.null(pipeline$types[[id]])) typeof(data[[ds]][[col]]) else pipeline$types[[id]]
        expect_identical(typeof(x), type, info = id)
        also <- pipeline$attributes[[id]]
        want <- expected[[rewrite(col)]]
        if (inherits(want, "vctrs_list_of")) {
          has_key <- key %in% names(data[[ds]])
          row_keys <- if (has_key) as.character(data[[ds]][[key]]) else rep(NA_character_, nrow(data[[ds]]))
          attr(want, "ptype") <- attr(
            deid_expected_nested(data[[ds]][[col]], row_keys, text_oracle, pipeline$prefix, key, has_key), "ptype"
          )
        }
        # Replaced and shifted factors lose a contrasts matrix named by the old
        # levels (a contrasts function's name would stay).
        if (col %in% replaced_cols && is.matrix(attr(want, "contrasts", exact = TRUE))) {
          attr(want, "contrasts") <- NULL
          expect_null(attr(x, "contrasts", exact = TRUE), info = id)
        }
        expect_identical(keep(x, c(also, "names")), keep(want, c(also, "names")), info = id)
        # Names of a vector column follow its values (in any row order, of
        # the rows kept), rewritten; a packed data frame column's names are
        # rewritten.
        if (is.data.frame(x)) {
          expect_identical(names(x), rewrite(names(data[[ds]][[col]])), info = id)
        } else if (is.null(names(data[[ds]][[col]]))) {
          expect_null(names(x), info = id)
        } else {
          kept_rows <- pipeline$keep_rows(ds, data[[ds]], oracle)
          expect_identical(sort(names(x)), sort(rewrite(names(data[[ds]][[col]]))[kept_rows]), info = id)
        }
        # Value labels and missing-value codes are kept on columns that are
        # neither replaced nor shifted, and are never anything but the
        # rewritten original.
        for (name in setdiff(c("labels", "na_values", "na_range"), also)) {
          want <- attr(expected[[rewrite(col)]], name, exact = TRUE)
          if (!col %in% replaced_cols || !is.null(attr(x, name, exact = TRUE))) {
            expect_identical(attr(x, name, exact = TRUE), want, info = paste(id, name))
          }
        }
      }
    }
  })

  # anon_check_fidelity() on the fixture: no check fails. The harness's own
  # checks of the same properties, above and below, stay independent of it.
  test_that(paste0(pipeline_name, ": anon_check_fidelity() finds no regression against the fixture"), {
    data <- pipeline$fixture()
    step <- deid_run(pipeline, data)$step
    fidelity <- anon_check_fidelity(data, step, key = key)
    failed <- fidelity[fidelity$status == "fail", , drop = FALSE]
    expect_identical(paste(failed$dataset, failed$check, failed$detail, sep = ": "), character(0))
    # The result names no original key, real date or user term (the original's
    # names that hold them are counted, not shown), as text or serialized, nor
    # does a report that includes it.
    report <- anon_report(deid_drop_flagged(step, pipeline$flagged), fidelity = fidelity)
    shown <- list(
      print = list(text = capture.output(print(fidelity)), patterns = FALSE),
      json = list(text = as.character(as_anon_json(fidelity)), patterns = FALSE),
      serialize = list(raw = serialize(fidelity, NULL), patterns = FALSE),
      report = list(text = as.character(as_anon_json(report)), raw = serialize(report, NULL), patterns = FALSE)
    )
    expect_equal(deid_leaks(shown), character(0))
  })

  test_that(paste0(pipeline_name, ": datasets still join and study days stay consistent"), {
    out <- deid_run(pipeline, pipeline$fixture())$step$data
    keys <- as.character(out$dm[[key]])
    expect_false(anyDuplicated(keys) > 0)
    for (ds in setdiff(names(out), "dm")) {
      if (!key %in% names(out[[ds]])) next
      k <- as.character(out[[ds]][[key]])
      expect_true(all(k[!is.na(k) & k != ""] %in% keys), info = ds)
    }
    expect_true(all(out$relsub$RSUBJID %in% keys))

    ref <- as.Date(out$dm$RFSTDTC[match(as.character(out$vs[[key]]), keys)])
    expect_identical(deid_study_day(as.Date(substr(out$vs$VSDTC, 1, 10)), ref), out$vs$VSDY)
    ref_ex <- as.Date(out$dm$RFSTDTC[match(as.character(out$ex[[key]]), keys)])
    expect_identical(deid_study_day(as.Date(out$ex$EXSTDTC), ref_ex), out$ex$EXSTDY)

    # Dates of one subject move together, in every column type; POSIXct keeps
    # its clock time in its own time zone, across daylight saving changes.
    expect_identical(as.numeric(as.Date(out$dm$RFSTDTC)), as.numeric(out$dm$RFSTDT))
    for (col in c("VSDTM", "VSNYDTM")) {
      tz <- attr(out$vs[[col]], "tzone")
      expect_identical(format(out$vs[[col]], "%Y-%m-%d", tz = tz), substr(out$vs$VSDTC, 1, 10), info = col)
      expect_identical(format(out$vs[[col]], "%H:%M:%S", tz = tz), rep("08:30:00", nrow(out$vs)), info = col)
    }
    expect_match(out$ex$EXENDTC, "^[0-9]{4}-[0-9]{2}$")

    # Inside nested data too: study days, clock times, and dates of a subject
    # at every depth.
    vsn <- out$vsn
    ref <- as.Date(out$dm$RFSTDTC[match(as.character(vsn[[key]]), keys)])
    for (i in seq_len(nrow(vsn))) {
      rows <- vsn$data[[i]]
      expect_identical(deid_study_day(as.Date(rows$VSDTC), ref[i]), rows$VSDY)
      expect_identical(format(rows$P$VSTM, "%Y-%m-%d %H:%M"), paste(rows$VSDTC, "08:30"))
      expect_identical(as.numeric(out$dm$WHEN[[match(as.character(vsn[[key]][i]), keys)]]), as.numeric(ref[i]))
    }
    expect_s3_class(vsn$data, "vctrs_list_of")
    for (rows in out$lbn$data) {
      k <- as.character(rows[[key]])
      # A blank key is missing in the numeric fixture.
      blank <- is.na(k) | k == ""
      present <- !blank & !is.na(rows$LBDTC) & rows$LBDTC != ""
      expect_false(is.unsorted(k[!blank]))
      day <- as.Date(substr(rows$LBDTC[present], 1, 10))
      expect_identical(deid_study_day(day, as.Date(out$dm$RFSTDTC[match(k[present], keys)])), rows$LBDY[present])
      expect_identical(do.call(c, lapply(rows$REPS[present], `[[`, "RPDT")), day + 1)
      expect_true(all(is.na(rows$LBDTC[blank])))
    }
  })

  for (gap_name in names(deid_known_gaps)) {
    gap <- deid_known_gaps[[gap_name]]
    test_that(paste0(pipeline_name, ": known gap '", gap_name, "' still leaks or breaks its exits"), {
      for (pkg in gap$needs) skip_if_not_installed(pkg)
      run <- tryCatch(deid_run(pipeline, gap$plant(pipeline$fixture())), error = function(e) e)
      if (inherits(run, "error")) {
        fail(paste0("gap '", gap_name, "' breaks the pipeline: ", conditionMessage(run)))
        return()
      }
      step <- deid_drop_flagged(run$step, pipeline$flagged)
      exits <- deid_gap_exits()
      results <- deid_run_exits(step, exits)
      errored <- names(results)[vapply(results, function(r) r$errored, logical(1))]
      expected <- union(deid_expected_errors(step, exits), gap$errors)
      unexpected <- setdiff(errored, expected)
      expect(
        length(unexpected) == 0,
        paste0("gap '", gap_name, "' makes exits error that it doesn't declare: ", paste(unexpected, collapse = ", "))
      )
      expect(
        all(expected %in% errored),
        paste0(
          "gap '", gap_name, "' no longer makes these exits error: ", paste(setdiff(expected, errored), collapse = ", "),
          ". If it is fixed, move it into deid_fixture() and remove it from deid_known_gaps"
        )
      )
      # A gap that now makes an exit error is not fixed.
      if (!is.null(gap$sentinel) && length(unexpected) == 0) {
        if (isTRUE(gap$pattern_exits_only)) {
          results <- results[vapply(results, function(r) r$patterns, logical(1))]
        }
        results$pipeline <- list(text = c(run$warnings, run$messages), patterns = FALSE)
        leaks <- deid_leaks(results, sentinels = gap$sentinel)
        if (pipeline_name %in% gap$handled_by) {
          expect(length(leaks) == 0, paste0("gap '", gap_name, "' leaks in a pipeline that handles it: ", paste(leaks, collapse = "; ")))
        } else {
          expect(
            length(leaks) > 0,
            paste0("gap '", gap_name, "' no longer leaks: move it into deid_fixture() and remove it from deid_known_gaps")
          )
        }
      }
    })
  }
}

test_that("the fixture holds its haven-labelled columns", {
  # Without haven they are not planted, and the invariants cover less.
  skip_if_not_installed("haven")
  expect_true(all(vapply(deid_fixture()$dm[c("GRP", "ARMCD", "SITE")], inherits, logical(1), "haven_labelled")))
})

test_that("each subject's New York times are on either side of a daylight saving change", {
  vs <- deid_fixture()$vs
  dst <- as.POSIXlt(vs$VSNYDTM)$isdst
  expect_true(all(tapply(dst, as.character(vs$USUBJID), function(x) length(unique(x)) == 2)))
})

test_that("a numeric sentinel is found unless it is part of a longer number", {
  found <- function(text) deid_found("70004", charToRaw(tolower(text)))
  expect_true(found("a.70004"))
  expect_true(found("see 70004, 70005"))
  expect_false(found("25.70004"))
  expect_false(found("170004"))
  expect_false(found("700041"))
})
