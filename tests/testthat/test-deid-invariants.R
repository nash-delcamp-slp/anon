# Privacy and fidelity invariants for every de-identification pipeline. The
# fixture, pipelines, exits and known gaps live in helper-deid.R; register new
# functions and new ways out of the package there.

# The value each fixture column should have after de-identification, given
# each row's original key: keys map through the secrets, SUBJID becomes the
# pseudonym's number, dates move by the subject's offset, and everything else
# is unchanged. Full dates move by the offset (POSIXct by calendar days,
# keeping the clock time), year-and-month dates from day 15 of the month,
# year-only dates by whole years (at least one, in the direction of the
# offset), each end of an interval on its own; durations are kept. In rows
# without a key, the key stays blank and SUBJID and dates are removed (set to
# the column's missing value).
deid_date_cols <- c(
  "RFSTDTC", "rficdtc", "RFSTDT", "VSDTC", "VSDTM", "VSNYDTM", "EXSTDTC", "EXENDTC", "QVAL",
  "PRSTDTC", "PRENDTC", "PRRNDTC", "PRDUDTC", "PRMTDTC", "PRYDDTC", "PRYRDTC"
)

# The missing value a text column already uses: "" if it has blank strings,
# otherwise NA.
deid_missing_like <- function(x) {
  if (any(!is.na(x) & x == "")) "" else NA_character_
}

deid_shift_text <- function(x, off) {
  if (is.na(x) || !nzchar(x) || grepl("^P", x)) {
    return(x)
  }
  if (grepl("/", x, fixed = TRUE)) {
    ends <- strsplit(x, "/", fixed = TRUE)[[1]]
    return(paste(vapply(ends, deid_shift_text, character(1), off = off, USE.NAMES = FALSE), collapse = "/"))
  }
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)) {
    return(paste0(format(as.Date(substr(x, 1, 10)) + off), substring(x, 11)))
  }
  if (grepl("^[0-9]{4}-[0-9]{2}($|--)", x)) {
    return(paste0(format(as.Date(paste0(substr(x, 1, 7), "-15")) + off, "%Y-%m"), substring(x, 8)))
  }
  if (grepl("^[0-9]{4}($|---)", x)) {
    years <- sign(off) * max(1, round(abs(off) / 365.25))
    return(paste0(sprintf("%04d", as.integer(substr(x, 1, 4)) + years), substring(x, 5)))
  }
  stop("No expected value for date ", x)
}

deid_expected_column <- function(x, col, keys, oracle, prefix, key) {
  off <- unname(oracle$offset[keys])
  if (col %in% c(key, "RSUBJID")) {
    k <- as.character(x)
    return(ifelse(is.na(k) | k == "", k, unname(oracle$new_key[k])))
  }
  if (col == "SUBJID") {
    out <- sub(prefix, "", unname(oracle$new_key[keys]), fixed = TRUE)
    out[is.na(out) & !is.na(x)] <- deid_missing_like(as.character(x))
    return(out)
  }
  if (!col %in% deid_date_cols) {
    return(x)
  }
  if (inherits(x, "Date")) {
    return(x + off)
  }
  if (inherits(x, "POSIXct")) {
    tz <- attr(x, "tzone")
    lt <- as.POSIXlt(x, tz = tz)
    lt$mday <- lt$mday + off
    lt$isdst <- -1L
    return(as.POSIXct(lt, tz = tz))
  }
  x <- as.character(x)
  present <- !is.na(x) & nzchar(x)
  no_key <- is.na(off)
  out <- x
  out[!no_key] <- vapply(which(!no_key), function(i) deid_shift_text(x[i], off[i]), character(1))
  out[no_key & present] <- deid_missing_like(x)
  out
}

# The oracle for text in attributes and names: the step's own, or, for a
# pipeline without secrets, that of the pipeline it must match.
deid_text_oracle <- function(pipeline, step) {
  if (isTRUE(pipeline$keep_secrets)) {
    return(deid_oracle(step))
  }
  if (is.null(pipeline$same_as)) {
    stop("A pipeline without secrets needs `same_as` to check its attributes and names.")
  }
  twin <- deid_complete_pipeline(deid_pipelines[[pipeline$same_as]])
  deid_oracle(deid_run(twin, deid_fixture())$step)
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
    expect_gt(as.numeric(min(diff(deid_start))), pipeline$max_days + deid_min_spacing)
  })

  test_that(paste0(pipeline_name, ": what can't be de-identified is named in a warning, with no other column"), {
    run <- deid_run(pipeline, deid_fixture())
    for (f in pipeline$flagged) {
      pattern <- deid_flag_pattern(f)
      expect_true(any(grepl(pattern, run$warnings)), info = pattern)
    }
    reasons <- Filter(Negate(is.null), lapply(pipeline$flagged, `[[`, "reason"))
    for (reason in unique(unlist(reasons))) {
      expected <- unlist(lapply(pipeline$flagged, function(f) {
        if (identical(f$reason, reason)) paste0(f$dataset, ".", f$column)
      }))
      for (w in grep(reason, run$warnings, fixed = TRUE, value = TRUE)) {
        expect_setequal(deid_listed_columns(w), expected)
      }
    }
  })

  test_that(paste0(pipeline_name, ": every exit runs, and no original ID, real date or user term leaves the package"), {
    run <- deid_run(pipeline, deid_fixture())
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
      step <- deid_run(pipeline, deid_fixture())$step
      expect_null(anon(step)$secrets)
      expect_null(anon(list(a = list(step = step)))$a$step$secrets)
    })

    test_that(paste0(pipeline_name, ": offsets and pseudonyms don't follow a pattern"), {
      step <- deid_run(pipeline, deid_fixture())$step
      oracle <- deid_oracle(step)
      n <- length(deid_keys)
      expect_setequal(names(oracle$new_key), deid_keys)
      expect_setequal(names(oracle$offset), deid_keys)

      off <- oracle$offset
      expect_true(all(abs(off) >= pipeline$min_days))
      expect_true(all(abs(off) <= pipeline$max_days))
      # No offset is shared by most subjects.
      expect_lte(max(table(off)), n / 2)
      expect_gte(length(unique(off)), 3)

      # Pseudonyms are not numbered in the original key order, in reverse, or
      # in a rotation of it.
      numbers <- as.integer(sub(pipeline$prefix, "", oracle$new_key[deid_keys], fixed = TRUE))
      s <- seq_len(n)
      patterned <- c(lapply(s - 1L, function(k) c(utils::tail(s, n - k), utils::head(s, k))), list(rev(s)))
      expect_false(any(vapply(patterned, identical, logical(1), numbers)))
    })

    test_that(paste0(pipeline_name, ": every value matches its subject's pseudonym and offset, and the rest is unchanged"), {
      data <- deid_fixture()
      step <- deid_run(pipeline, data)$step
      oracle <- deid_oracle(step)
      flagged_datasets <- unlist(lapply(pipeline$flagged, function(f) if (!is.null(f$dataset) && is.null(f$column)) f$dataset))
      flagged_columns <- unlist(lapply(pipeline$flagged, function(f) if (!is.null(f$column)) paste0(f$dataset, ".", f$column)))

      for (ds in setdiff(names(data), pipeline$removed)) {
        out <- step$data[[deid_rewrite_text(ds, oracle)]]
        original <- data[[ds]]
        # Documented as left unchanged, apart from keys and dates in its
        # attributes and names.
        if (!key %in% names(original) || ds %in% flagged_datasets) {
          expect_identical(out, deid_rewrite_frame(original, oracle), info = ds)
          next
        }
        keep <- pipeline$keep_rows(ds, original, oracle)
        all_keys <- as.character(original[[key]])
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
          got <- out[[deid_rewrite_text(col, oracle)]]
          if (is.data.frame(got)) {
            # A packed data frame column: its rows move with the data.
            expect_identical(got, vctrs::vec_slice(expected, which(keep)[ord]), info = id)
          } else if (is.factor(got) || is.character(got)) {
            expect_identical(as.character(got), as.character(expected)[keep][ord], info = id)
          } else {
            expect_identical(as.vector(unclass(got)), as.vector(unclass(expected))[keep][ord], info = id)
          }
          # Names of a vector column follow its values, rewritten.
          if (!is.data.frame(got)) {
            want_names <- names(original[[col]])
            if (!is.null(want_names)) {
              want_names <- deid_rewrite_text(want_names, oracle)[keep][ord]
            }
            expect_identical(names(got), want_names, info = id)
          }
        }
      }
    })
  }

  if (!is.null(pipeline$same_as)) {
    test_that(paste0(pipeline_name, ": gives the same data as ", pipeline$same_as, ", and holds nothing to link back"), {
      step <- deid_run(pipeline, deid_fixture())$step
      twin <- deid_run(deid_complete_pipeline(deid_pipelines[[pipeline$same_as]]), deid_fixture())$step
      expect_identical(step$data, twin$data)
      saved <- list(serialize = list(raw = serialize(deid_drop_flagged(step, pipeline$flagged), NULL), patterns = FALSE))
      expect_equal(deid_leaks(saved), character(0))
    })
  }

  test_that(paste0(pipeline_name, ": the shape of the data and its attributes are kept"), {
    data <- deid_fixture()
    step <- deid_run(pipeline, data)$step
    out <- step$data
    oracle <- if (isTRUE(pipeline$keep_secrets)) deid_oracle(step)
    # Keys and dates in attributes and names are rewritten.
    text_oracle <- deid_text_oracle(pipeline, step)
    rewrite <- function(x) deid_rewrite_text(x, text_oracle)
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
        expect_identical(keep(x, c(also, "names")), keep(expected[[rewrite(col)]], c(also, "names")), info = id)
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

  test_that(paste0(pipeline_name, ": datasets still join and study days stay consistent"), {
    out <- deid_run(pipeline, deid_fixture())$step$data
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
  })

  for (gap_name in names(deid_known_gaps)) {
    gap <- deid_known_gaps[[gap_name]]
    test_that(paste0(pipeline_name, ": known gap '", gap_name, "' still leaks or breaks its exits"), {
      for (pkg in gap$needs) skip_if_not_installed(pkg)
      run <- tryCatch(deid_run(pipeline, gap$plant(deid_fixture())), error = function(e) e)
      if (inherits(run, "error")) {
        fail(paste0("gap '", gap_name, "' breaks the pipeline: ", conditionMessage(run)))
        return()
      }
      step <- deid_drop_flagged(run$step, pipeline$flagged)
      results <- deid_run_exits(step)
      errored <- names(results)[vapply(results, function(r) r$errored, logical(1))]
      expected <- union(deid_expected_errors(step), gap$errors)
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
        expect(
          length(deid_leaks(results, sentinels = gap$sentinel)) > 0,
          paste0("gap '", gap_name, "' no longer leaks: move it into deid_fixture() and remove it from deid_known_gaps")
        )
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
