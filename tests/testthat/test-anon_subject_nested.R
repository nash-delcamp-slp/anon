# Nested data in anon_subject_ids() and anon_subject_dates(). Nested data is
# built with base R and vctrs, as tidyr::nest() would make it.

nested_lb <- function() {
  data.frame(
    USUBJID = rep(c("S-01", "S-02", "S-03"), c(3, 2, 2)),
    LBTESTCD = c("A", "B", "A", "A", "B", "A", "B"),
    LBDTC = c("2014-01-02T08:00", "2014-01-05", "2014-01-09", "2015-03-01", "2015-03-04", "2016-07-10", "2016-07-12"),
    LBDY = c(2, 5, 9, 1, 4, 1, 3),
    stringsAsFactors = FALSE
  )
}
nested_dm <- function() {
  data.frame(USUBJID = c("S-01", "S-02", "S-03"), RFSTDTC = c("2014-01-01", "2015-03-01", "2016-07-10"))
}

# `df` with list column `by`'s data frames split by `col` (tibbles, as
# tidyr::nest() makes them), as a plain list or a vctrs list_of.
nest_base <- function(df, col, list_of = FALSE) {
  parts <- lapply(split(df[setdiff(names(df), col)], df[[col]]), function(x) {
    rownames(x) <- NULL
    tibble::as_tibble(x)
  })
  out <- tibble::tibble(!!col := names(parts))
  out$data <- if (list_of) do.call(vctrs::list_of, unname(parts)) else unname(parts)
  out
}

# The rows of a list column of data frames, with the outer columns repeated.
unnest_base <- function(df) {
  sizes <- vapply(df$data, NROW, integer(1))
  inner <- do.call(rbind, lapply(df$data, as.data.frame))
  outer <- as.data.frame(df)[rep(seq_len(nrow(df)), sizes), setdiff(names(df), "data"), drop = FALSE]
  out <- cbind(outer, inner)
  rownames(out) <- NULL
  out
}

step_warnings <- function(expr) {
  w <- character(0)
  value <- withCallingHandlers(expr, warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = w)
}

test_that("nested data de-identified and unnested gives back the original rows through the secrets", {
  for (by in c("USUBJID", "LBTESTCD")) {
    for (list_of in c(FALSE, TRUE)) {
      lb <- nested_lb()
      lb$LBDT <- as.Date(substr(lb$LBDTC, 1, 10))
      nested <- nest_base(lb, by, list_of)
      step <- anon_subject_dates(
        anon_subject_ids(list(dm = nested_dm(), lb = nested), keep_secrets = TRUE, seed = 3),
        keep_secrets = TRUE, seed = 4
      )
      info <- paste(by, list_of)
      expect_identical(inherits(step$data$lb$data, "vctrs_list_of"), list_of, info = info)
      secrets <- anon_secrets(step)
      out <- unnest_base(step$data$lb)
      # Study days stay consistent with the shifted reference dates.
      ref <- as.Date(step$data$dm$RFSTDTC[match(out$USUBJID, step$data$dm$USUBJID)])
      day <- as.numeric(as.Date(substr(out$LBDTC, 1, 10)) - ref)
      expect_equal(ifelse(day >= 0, day + 1, day), out$LBDY, info = info)
      # Back through the secrets: the offsets are keyed by the pseudonym, as
      # the IDs step ran first.
      off <- secrets$date_offsets$offset_days[match(out$USUBJID, secrets$date_offsets$key)]
      back <- data.frame(
        USUBJID = secrets$subject_ids$key[match(out$USUBJID, secrets$subject_ids$pseudonym)],
        LBTESTCD = out$LBTESTCD,
        LBDTC = paste0(format(as.Date(substr(out$LBDTC, 1, 10)) - off), substring(out$LBDTC, 11)),
        LBDY = out$LBDY,
        LBDT = out$LBDT - off,
        stringsAsFactors = FALSE
      )
      sort_rows <- function(x) {
        x <- x[do.call(order, unname(as.list(x[c("USUBJID", "LBTESTCD", "LBDTC")]))), names(lb)]
        rownames(x) <- NULL
        x
      }
      expect_equal(sort_rows(back), sort_rows(lb), info = info)
    }
  }
})

test_that("data frames with their own key are sorted by it; others keep their row order", {
  lb <- nested_lb()
  nested <- nest_base(lb, "LBTESTCD")
  # A plain data frame element, whose row names are reset when it is sorted.
  nested$data[[1]] <- as.data.frame(nested$data[[1]])
  rownames(nested$data[[1]]) <- paste0("r", seq_len(nrow(nested$data[[1]])))
  step <- anon_subject_ids(list(dm = nested_dm(), lb = nested), keep_secrets = TRUE, seed = 5)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  for (i in 1:2) {
    got <- step$data$lb$data[[i]]
    original <- nested$data[[i]]
    ord <- order(new[original$USUBJID], seq_len(nrow(original)))
    expect_identical(got$USUBJID, unname(new[original$USUBJID][ord]))
    expect_identical(got$LBDTC, original$LBDTC[ord])
  }
  expect_identical(rownames(step$data$lb$data[[1]]), as.character(seq_len(nrow(nested$data[[1]]))))
  # Nested by subject: each element keeps its rows in order.
  by_subject <- nest_base(lb, "USUBJID")
  step <- anon_subject_ids(by_subject, keep_secrets = TRUE, seed = 5)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  at <- match(new[by_subject$USUBJID], step$data$USUBJID)
  expect_identical(lapply(step$data$data[at], `[[`, "LBDTC"), lapply(by_subject$data, `[[`, "LBDTC"))
})

test_that("a subject seen only in nested data gets a pseudonym, the same in every dataset", {
  lb <- nested_lb()
  lb$USUBJID[lb$USUBJID == "S-03"] <- "S-09"
  nested <- nest_base(lb, "LBTESTCD")
  rel <- data.frame(USUBJID = "S-01", RSUBJID = "S-09")
  step <- anon_subject_ids(list(dm = nested_dm(), lb = nested, rel = rel), keep_secrets = TRUE, seed = 6)
  ids <- anon_secrets(step)$subject_ids
  expect_setequal(ids$key, c("S-01", "S-02", "S-03", "S-09"))
  expect_true(ids$pseudonym[ids$key == "S-09"] %in% unlist(lapply(step$data$lb$data, `[[`, "USUBJID")))
  expect_identical(step$data$rel$RSUBJID, ids$pseudonym[ids$key == "S-09"])
  # A dataset whose key is only in nested data is de-identified through it.
  only <- anon_subject_dates(nested, keep_secrets = TRUE, seed = 6)
  expect_false(identical(only$data$data[[1]]$LBDTC, nested$data[[1]]$LBDTC))
  expect_false(any(grepl("no `USUBJID` column", only$log$detail)))
  expect_error(anon_subject_ids(list(a = data.frame(X = 1))), "not found in any dataset")
})

test_that("nested rows without a subject follow the top-level rules", {
  lb <- nested_lb()
  lb$USUBJID[2] <- ""
  nested <- nest_base(lb, "LBTESTCD")
  r <- step_warnings(anon_subject_dates(list(dm = nested_dm(), lb = nested), seed = 7))
  expect_match(r$warnings, "^1 date value\\(s\\) in rows without a `USUBJID` were removed\\.$", all = FALSE)
  rows <- unnest_base(r$value$data$lb)
  expect_identical(rows$LBDTC[rows$USUBJID == ""], NA_character_)
  # An unkeyed dataset holding data frames without the key: left unchanged,
  # and named with the dates nested in it.
  ts <- tibble::tibble(TSPARMCD = "X")
  ts$data <- list(tibble::tibble(TSVAL = "2014-01-01", TSDT = as.Date("2014-01-01")))
  r <- step_warnings(anon_subject_dates(list(dm = nested_dm(), ts = ts), seed = 7))
  expect_identical(r$value$data$ts, ts)
  expect_match(
    r$warnings, "Datasets without `USUBJID` kept their values unchanged but contain identifiers or dates: ts \\(data\\[\\[\\]\\]\\[2\\], data\\[\\[\\]\\]\\[1\\]\\)\\.",
    all = FALSE
  )
  # In keyed data, an element belongs to its row, whose blank key leaves its
  # dates without a subject.
  d <- tibble::tibble(USUBJID = c("S-01", ""), D = list(tibble::tibble(X = as.Date("2014-01-01")), tibble::tibble(X = as.Date("2014-01-02"))))
  r <- step_warnings(anon_subject_dates(d, seed = 7))
  expect_identical(r$value$data$D[[2]]$X, as.Date(NA))
  expect_match(r$warnings, "^1 date value", all = FALSE)
})

test_that("dates at any depth of nested data move with their subject, keeping type and clock time", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"), RFSTDTC = c("2014-01-01", "2015-03-01"))
  d$P <- tibble::tibble(PDT = as.Date(c("2014-01-02", "2015-03-02")))
  d$P$IN <- list(tibble::tibble(Y = as.Date("2014-01-03")), NULL)
  d$L <- list(
    list(a = as.Date("2014-01-04"), b = list(as.POSIXct("2014-03-08 08:30", tz = "America/New_York"))),
    tibble::tibble(ZDTC = factor("2015-03-05"), W = tibble::tibble(V = as.Date("2015-03-06")))
  )
  d$E <- list(tibble::tibble(X = as.Date(character(0))), NULL)
  step <- anon_subject_dates(d, keep_secrets = TRUE, seed = 8)
  off <- stats::setNames(anon_secrets(step)$date_offsets$offset_days, anon_secrets(step)$date_offsets$key)
  out <- step$data
  expect_identical(out$P$PDT, d$P$PDT + unname(off))
  expect_identical(out$P$IN[[1]]$Y, as.Date("2014-01-03") + off[["S-01"]])
  expect_null(out$P$IN[[2]])
  expect_identical(out$L[[1]]$a, as.Date("2014-01-04") + off[["S-01"]])
  tm <- out$L[[1]]$b[[1]]
  expect_identical(format(tm, "%H:%M"), "08:30")
  expect_identical(as.Date(format(tm, "%Y-%m-%d")), as.Date("2014-03-08") + off[["S-01"]])
  expect_identical(attr(tm, "tzone"), "America/New_York")
  expect_s3_class(out$L[[2]]$ZDTC, "factor")
  expect_identical(as.character(out$L[[2]]$ZDTC), format(as.Date("2015-03-05") + off[["S-02"]]))
  expect_identical(out$L[[2]]$W$V, as.Date("2015-03-06") + off[["S-02"]])
  expect_identical(out$E, d$E)
  expect_true(any(step$log$variable %in% "L[[]][1]"))
})

test_that("keys, also and key_cols columns inside nested data are replaced", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"), SUBJID = c("01", "02"))
  d$L <- list(tibble::tibble(SUBJID = "01", RSUBJID = "S-02"), tibble::tibble(SUBJID = "02", RSUBJID = "S-01"))
  step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 9)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  out <- step$data
  for (i in 1:2) {
    expect_identical(out$L[[i]]$SUBJID, out$SUBJID[i])
    expect_identical(out$L[[i]]$RSUBJID, unname(new[setdiff(c("S-01", "S-02"), names(new)[new == out$USUBJID[i]])]))
  }
  expect_true(all(c("L[[]][SUBJID]", "L[[]][RSUBJID]") %in% step$log$variable))
})

test_that("vars selects columns by name inside nested data too, and SUPP QVAL is shifted there", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"))
  d$L <- list(
    tibble::tibble(RES = "2014-01-01", QNAM = "X", QVAL = "2014-02-02"),
    tibble::tibble(RES = "2015-01-01", QNAM = "X", QVAL = "2015-02-02")
  )
  w <- step_warnings(anon_subject_dates(d, keep_secrets = TRUE, seed = 10))
  expect_match(w$warnings, "not shifted: data.L\\[\\[\\]\\]\\[1\\]\\.", all = FALSE)
  step <- w$value
  off <- anon_secrets(step)$date_offsets$offset_days
  expect_identical(vapply(step$data$L, `[[`, "", "QVAL"), format(as.Date(c("2014-02-02", "2015-02-02")) + off))
  w <- step_warnings(anon_subject_dates(d, vars = "RES", keep_secrets = TRUE, seed = 10))
  expect_identical(vapply(w$value$data$L, `[[`, "", "RES"), format(as.Date(c("2014-01-01", "2015-01-01")) + off))
  expect_match(w$warnings, "not shifted: data.L\\[\\[\\]\\]\\[3\\]\\.", all = FALSE)
})

test_that("text values in nested data are searched and named where they are", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"))
  d$L <- list(tibble::tibble(NOTE = "about S-02", SEEN = "2014-01-05"), NULL)
  r <- step_warnings(anon_subject_ids(d, seed = 11))
  expect_match(r$warnings, "possibly inside text: data.L\\[\\[\\]\\]\\[NOTE\\]\\. .*`ds.col\\[\\[\\]\\]\\[sub\\]`", all = FALSE)
  r <- step_warnings(anon_subject_dates(d, seed = 11))
  expect_match(r$warnings, "not shifted: data.L\\[\\[\\]\\]\\[2\\]\\.", all = FALSE)
  # A column named like the key in another case, in keyed nested data.
  d$L[[2]] <- tibble::tibble(usubjid = "x")
  r <- step_warnings(anon_subject_ids(d, seed = 11))
  expect_match(r$warnings, "another case and were not replaced: data.L\\[\\[\\]\\]\\[usubjid\\]\\.", all = FALSE)
})

test_that("attributes and names inside nested data are rewritten, or dropped with other_attributes = \"drop\"", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"))
  inner <- tibble::tibble(V = 1)
  attr(inner$V, "label") <- "Value for S-01 on 2014-01-05"
  attr(inner$V, "note") <- "S-02"
  attr(inner, "label") <- "Rows of S-02"
  inner[["S-01"]] <- 2
  d$L <- list(inner, NULL)
  d$P <- tibble::tibble(Q = 1:2)
  attr(d$P, "label") <- "Packed for S-01"
  attr(d$P, "cut") <- as.Date("2014-01-05")
  step <- anon_subject_dates(anon_subject_ids(d, keep_secrets = TRUE, seed = 12), seed = 13)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  got <- step$data$L[[which(!vapply(step$data$L, is.null, logical(1)))]]
  expect_identical(attr(got$V, "label"), paste("Value for", new[["S-01"]], "on [DATE]"))
  expect_identical(attr(got$V, "note"), new[["S-02"]])
  expect_identical(attr(got, "label"), paste("Rows of", new[["S-02"]]))
  expect_named(got, c("V", new[["S-01"]]))
  expect_identical(attr(step$data$P, "label"), paste("Packed for", new[["S-01"]]))
  expect_identical(attr(step$data$P, "cut"), "[DATE]")
  dropped <- anon_subject_ids(d, seed = 12, other_attributes = "drop")$data
  got <- dropped$L[[which(!vapply(dropped$L, is.null, logical(1)))]]
  expect_null(attr(got$V, "note"))
  expect_null(attr(dropped$P, "cut"))
  expect_match(attr(got$V, "label"), "^Value for SUBJ-")
})

test_that("nested data keeps its type and structure", {
  d <- tibble::tibble(USUBJID = c("S-01", "S-02", "S-03"), RFSTDTC = c("2014-01-01", "2015-03-01", "2016-07-10"))
  g <- function(day) {
    dplyr::group_by(tibble::tibble(DT = as.Date(day) + 0:1, K = c("a", "b"), N = 1:2), DT)
  }
  d$G <- list(g("2014-01-02"), g("2015-03-02"), g("2016-07-11"))
  d$LO <- vctrs::list_of(
    tibble::tibble(USUBJID = factor("S-02"), X = 1), tibble::tibble(USUBJID = factor("S-01"), X = 2),
    NULL
  )
  step <- anon_subject_dates(anon_subject_ids(d, keep_secrets = TRUE, seed = 14), seed = 15)
  out <- step$data
  # Grouped tibbles stay grouped, regrouped by their shifted dates.
  for (e in out$G) {
    expect_s3_class(e, "grouped_df")
    expect_identical(dplyr::group_data(e)$DT, e$DT)
  }
  # A list_of keeps its class, and its prototype matches the new elements
  # (factor levels are the new keys).
  expect_s3_class(out$LO, "vctrs_list_of")
  expect_null(out$LO[[which(out$USUBJID == anon_secrets(step)$subject_ids$pseudonym[3])]])
  elements <- Filter(Negate(is.null), unclass(out$LO))
  expect_identical(do.call(vctrs::vec_ptype_common, unname(elements)), attr(out$LO, "ptype"))
  expect_true(all(levels(attr(out$LO, "ptype")$USUBJID) %in% anon_secrets(step)$subject_ids$pseudonym))
  expect_no_error(out$LO[[1]] <- elements[[1]])
})

test_that("objects that can't be de-identified are named in one warning and left as they are", {
  fit <- stats::lm(y ~ x, data = data.frame(x = 1:3, y = c(2, 4, 7)))
  split <- structure(list(data = data.frame(USUBJID = "S-01"), in_id = 1L), class = "rsplit")
  e <- new.env()
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"))
  d$M <- list(fit, NULL)
  d$S <- list(split, 1)
  d$X <- list(function(x) x, e)
  d$N <- list(tibble::tibble(R = list(y ~ x)), NULL)
  for (step_fn in list(anon_subject_ids, anon_subject_dates)) {
    r <- step_warnings(step_fn(d, seed = 16))
    w <- grep("can't be de-identified", r$warnings, value = TRUE)
    expect_length(w, 1)
    expect_match(w, "\\(models, recipes, resampling splits, environments, functions or similar\\): data.M, data.S, data.X, data.N\\[\\[\\]\\]\\[(R|1)\\]\\. They may hold original data")
    out <- r$value$data
    ord <- match(c("S-01", "S-02"), d$USUBJID)
    expect_identical(out$M[[1]]$coefficients, fit$coefficients)
    expect_identical(unclass(out$S[[match(1, vapply(out$S, is.numeric, logical(1)))]]), 1)
    expect_true(any(vapply(out$X, is.environment, logical(1))))
    # Their contents are not searched.
    expect_false(any(grepl("still contain original", r$warnings)))
  }
})

test_that("nested changes are logged with counts, by dataset and column path", {
  lb <- nested_lb()
  step <- anon_subject_dates(anon_subject_ids(nest_base(lb, "LBTESTCD"), seed = 17), seed = 18)
  rows <- step$log[step$log$dataset %in% "data" & !is.na(step$log$variable), ]
  expect_identical(rows$variable, c("data[[]][USUBJID]", "data[[]][2]"))
  expect_identical(rows$n_changed, c(7L, 7L))
  # Heterogeneous data frames in one list column are logged together.
  d <- tibble::tibble(USUBJID = c("S-01", "S-02"))
  d$L <- list(tibble::tibble(DT = as.Date("2014-01-01")), tibble::tibble(DT = as.Date("2014-01-01"), X = "a"))
  step <- anon_subject_dates(d, seed = 19)
  expect_identical(step$log$n_changed[step$log$variable %in% "L[[]][1]"], 2L)
})

test_that("many nested data frames are handled at once", {
  n <- 5000
  d <- tibble::tibble(USUBJID = sprintf("S-%05d", seq_len(n)))
  d$data <- lapply(seq_len(n), function(i) tibble::tibble(LBDTC = c("2014-01-02", "2014-01-03"), LBDT = as.Date("2014-01-04")))
  expect_lt(system.time(anon_subject_dates(anon_subject_ids(d, seed = 1), seed = 2))[["elapsed"]], 10)
})

test_that("data frames that differ only in their values are rewritten alike, and counted each", {
  inner <- function(v) {
    t <- tibble::tibble(V = v, W = "x")
    attr(t$V, "label") <- "Value for S-01"
    t
  }
  d <- tibble::tibble(USUBJID = c("S-01", "S-02", "S-03"))
  d$L <- list(inner(1), inner(2:3), inner(4))
  attr(d$L[[3]]$W, "label") <- "About S-02"
  step <- anon_subject_ids(d, keep_secrets = TRUE, seed = 20)
  new <- stats::setNames(anon_secrets(step)$subject_ids$pseudonym, anon_secrets(step)$subject_ids$key)
  for (e in step$data$L) {
    expect_identical(attr(e$V, "label"), paste("Value for", new[["S-01"]]))
    expect_identical(.row_names_info(e, 0L), .row_names_info(tibble::tibble(x = e$V), 0L))
  }
  expect_identical(attr(step$data$L[[match(new[["S-03"]], step$data$USUBJID)]]$W, "label"), paste("About", new[["S-02"]]))
  expect_match(step$log$detail[step$log$dataset %in% "data" & is.na(step$log$variable)], "4 attribute value")
})
