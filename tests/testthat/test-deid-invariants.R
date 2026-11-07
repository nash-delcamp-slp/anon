# Privacy and fidelity invariants for every de-identification pipeline. The
# fixture, pipelines, exits and known gaps live in helper-deid.R; register new
# functions and new ways out of the package there.

# The value each fixture column should have after de-identification, given a
# row's original key: keys map through the secrets, SUBJID becomes the
# pseudonym's number, dates move by the subject's offset (partial months from
# day 15), and everything else is unchanged.
deid_key_cols <- c("USUBJID", "RSUBJID")
deid_date_cols <- c("RFSTDTC", "rficdtc", "RFSTDT", "VSDTC", "VSDTM", "EXSTDTC", "EXENDTC", "QVAL")

deid_expected_column <- function(x, col, keys, oracle, prefix) {
  off <- unname(oracle$offset[keys])
  if (col %in% deid_key_cols) {
    return(unname(oracle$new_key[as.character(x)]))
  }
  if (col == "SUBJID") {
    return(sub(prefix, "", unname(oracle$new_key[keys]), fixed = TRUE))
  }
  if (!col %in% deid_date_cols) {
    return(x)
  }
  if (inherits(x, "Date")) {
    return(x + off)
  }
  if (inherits(x, "POSIXct")) {
    return(x + off * 86400)
  }
  x <- as.character(x)
  month <- grepl("^[0-9]{4}-[0-9]{2}$", x)
  out <- x
  out[!month] <- paste0(format(as.Date(substr(x[!month], 1, 10)) + off[!month]), substring(x[!month], 11))
  if (any(month)) {
    out[month] <- format(as.Date(paste0(x[month], "-15")) + off[month], "%Y-%m")
  }
  out
}

for (pipeline_name in names(deid_pipelines)) {
  pipeline <- deid_pipelines[[pipeline_name]]

  test_that(paste0(pipeline_name, ": the pipeline's offsets keep the fixture's sentinels apart"), {
    expect_gt(pipeline$min_days, deid_min_spacing)
  })

  test_that(paste0(pipeline_name, ": what can't be de-identified is named in a warning"), {
    run <- deid_run(pipeline, deid_fixture())
    for (f in deid_flagged) {
      expect_true(any(grepl(f$pattern, run$warnings)), info = f$pattern)
    }
  })

  test_that(paste0(pipeline_name, ": every exit runs, and no original ID, real date or user term leaves the package"), {
    run <- deid_run(pipeline, deid_fixture())
    expect_identical(!is.null(run$step$secrets), pipeline$keep_secrets)
    step <- deid_drop_flagged(run$step)
    results <- deid_run_exits(step)

    errored <- names(results)[vapply(results, function(r) r$errored, logical(1))]
    expected_errors <- vapply(deid_exits, function(e) {
      isTRUE(if (is.function(e$errors)) e$errors(step) else e$errors)
    }, logical(1))
    expect_setequal(errored, names(deid_exits)[expected_errors])
    for (name in setdiff(names(results), errored)) {
      expect_true(any(nzchar(results[[name]]$text)), info = paste("exit returned no text:", name))
    }

    results$warnings <- list(text = run$warnings, patterns = FALSE)
    expect_equal(deid_leaks(results), character(0))
  })

  if (isTRUE(pipeline$keep_secrets)) {
    test_that(paste0(pipeline_name, ": redacted copies made with anon() carry no secrets"), {
      step <- deid_run(pipeline, deid_fixture())$step
      expect_null(anon(step)$secrets)
      expect_null(anon(list(a = list(step = step)))$a$step$secrets)
    })

    test_that(paste0(pipeline_name, ": every row keeps its own subject's pseudonym and date offset"), {
      data <- deid_fixture()
      step <- deid_drop_flagged(deid_run(pipeline, data)$step)
      oracle <- deid_oracle(step)

      expect_setequal(names(oracle$new_key), deid_keys)
      expect_setequal(names(oracle$offset), deid_keys)
      # One offset per subject, not one for all; pseudonyms not numbered in
      # the original key order.
      expect_gt(length(unique(oracle$offset)), 1)
      numbers <- as.integer(sub(pipeline$prefix, "", oracle$new_key[deid_keys], fixed = TRUE))
      expect_false(identical(numbers, seq_along(deid_keys)))
      expect_true(all(abs(oracle$offset) >= pipeline$min_days))

      for (ds in names(step$data)) {
        out <- step$data[[ds]]
        if (!"USUBJID" %in% names(out)) next
        original <- data[[ds]]
        keys <- as.character(original$USUBJID)
        # Rows are sorted by the new key, keeping their order within a subject.
        ord <- order(unname(oracle$new_key[keys]), seq_along(keys))
        expect_false(is.unsorted(as.character(out$USUBJID)), info = ds)
        for (col in names(out)) {
          expected <- deid_expected_column(original[[col]], col, keys, oracle, pipeline$prefix)
          got <- out[[col]]
          if (is.factor(got) || is.character(got)) {
            expect_identical(as.character(got), as.character(expected)[ord], info = paste0(ds, ".", col))
          } else {
            expect_equal(as.vector(unclass(got)), as.vector(unclass(expected))[ord], info = paste0(ds, ".", col))
          }
        }
      }
    })
  }

  if (!is.null(pipeline$same_as)) {
    test_that(paste0(pipeline_name, ": gives the same data as ", pipeline$same_as, ", and holds nothing to link back"), {
      step <- deid_run(pipeline, deid_fixture())$step
      twin <- deid_run(deid_pipelines[[pipeline$same_as]], deid_fixture())$step
      expect_identical(step$data, twin$data)
      saved <- list(serialize = list(text = rawToChar(serialize(deid_drop_flagged(step), NULL, ascii = TRUE)), patterns = FALSE))
      expect_equal(deid_leaks(saved), character(0))
    })
  }

  test_that(paste0(pipeline_name, ": the shape of the data and its attributes are kept"), {
    data <- deid_fixture()
    out <- deid_run(pipeline, data)$step$data
    expect_named(out, names(data))
    # Attributes that change on purpose: row names (reset), factor levels
    # (they hold keys or dates), and haven value labels and missing-value
    # codes (they describe the original values).
    changing <- c("row.names", "levels", "labels", "na_values", "na_range")
    keep <- function(x) {
      a <- attributes(x)
      a[sort(setdiff(names(a), changing))]
    }
    for (ds in names(data)) {
      expect_named(out[[ds]], names(data[[ds]]))
      expect_equal(nrow(out[[ds]]), nrow(data[[ds]]), info = ds)
      expect_identical(keep(out[[ds]]), keep(data[[ds]]), info = ds)
      for (col in names(data[[ds]])) {
        expect_identical(typeof(out[[ds]][[col]]), typeof(data[[ds]][[col]]), info = paste0(ds, ".", col))
        expect_identical(keep(out[[ds]][[col]]), keep(data[[ds]][[col]]), info = paste0(ds, ".", col))
      }
    }
  })

  test_that(paste0(pipeline_name, ": datasets still join and study days stay consistent"), {
    out <- deid_run(pipeline, deid_fixture())$step$data
    keys <- as.character(out$dm$USUBJID)
    expect_false(anyDuplicated(keys) > 0)
    for (ds in c("vs", "ex", "suppdm", "relsub", "ad")) {
      expect_true(all(as.character(out[[ds]]$USUBJID) %in% keys), info = ds)
    }
    expect_true(all(out$relsub$RSUBJID %in% keys))

    ref <- as.Date(out$dm$RFSTDTC[match(as.character(out$vs$USUBJID), keys)])
    expect_equal(deid_study_day(as.Date(substr(out$vs$VSDTC, 1, 10)), ref), out$vs$VSDY)
    ref_ex <- as.Date(out$dm$RFSTDTC[match(out$ex$USUBJID, keys)])
    expect_equal(deid_study_day(as.Date(out$ex$EXSTDTC), ref_ex), out$ex$EXSTDY)

    # Dates of one subject move together, in every column type; POSIXct keeps
    # its clock time in its own time zone.
    expect_equal(as.Date(out$dm$RFSTDTC), out$dm$RFSTDT)
    tz <- attr(out$vs$VSDTM, "tzone")
    expect_equal(format(out$vs$VSDTM, "%Y-%m-%d", tz = tz), substr(out$vs$VSDTC, 1, 10))
    expect_equal(format(out$vs$VSDTM, "%H:%M", tz = tz), rep("08:30", nrow(out$vs)))
    expect_match(out$ex$EXENDTC, "^[0-9]{4}-[0-9]{2}$")
  })
}

test_that("known gaps still leak; a gap that no longer leaks should move into the fixture", {
  pipeline <- deid_pipelines[[1]]
  for (gap_name in names(deid_known_gaps)) {
    gap <- deid_known_gaps[[gap_name]]
    data <- gap$plant(deid_fixture())
    run <- deid_run(pipeline, data)
    results <- deid_run_exits(deid_drop_flagged(run$step))
    if (isTRUE(gap$pattern_exits_only)) {
      results <- results[vapply(results, function(r) r$patterns, logical(1))]
    }
    results$warnings <- list(text = run$warnings, patterns = FALSE)
    expect_true(
      length(deid_leaks(results, sentinels = gap$sentinel)) > 0,
      info = paste0("gap '", gap_name, "' no longer leaks: move it into deid_fixture() and remove it from deid_known_gaps")
    )
  }
})
