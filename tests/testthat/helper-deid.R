# Shared fixture and registries for the de-identification invariants in
# test-deid-invariants.R.
#
# The fixture plants sentinel identifiers, real dates and user-pattern terms
# (such as a drug name in a label) in every place they can hide. Each
# pipeline in `deid_pipelines` is run on it, and every way data can leave the
# package (`deid_exits`) is checked for the sentinels. Rows are checked
# against the step's secrets: each row keeps its own subject's pseudonym and
# date offset, and every other value is unchanged.
#
# To register new work:
# - a de-identification function: add a pipeline record to `deid_pipelines`;
# - a way to print, report, serialize or share an object: add an exit to
#   `deid_exits`;
# - a hiding place the package can't handle yet: add it to `deid_known_gaps`
#   (the test fails when a gap is fixed, so it can move into the fixture).

# Original subject keys, related IDs and per-subject real dates. Each
# subject's full dates lie within `deid_min_spacing` days of each other (from
# 14 days before its start date to 7 days after), so an offset of more than
# `deid_min_spacing` days can't land on the same subject's other real dates:
# pipelines must use `min_days` above it. Start dates are eleven years apart,
# more than any `max_days` (at most 3650) plus `deid_min_spacing`, so a
# shifted date can't land on another subject's real dates. The tests assert
# both for every pipeline. Each start date lies a few days before a daylight
# saving change in America/New_York (in March or November), and each
# subject's two VS times are a week apart, on either side of the change, so
# under almost any offset at least one of them crosses a change. The last key
# starts with the first, so text holding it must not be taken for the first.
deid_keys <- c(sprintf("ZQ-7701-%04d", 1:7), "ZQ-7701-0001-2")
deid_subjids <- sprintf("ZQ%04d", 1:8)
deid_start <- as.Date(c(
  "2031-03-06", "2042-10-28", "2053-03-08", "2064-10-27",
  "2075-03-08", "2086-10-30", "2097-03-07", "2108-10-30"
))
deid_min_spacing <- 21

# Seeds used by the pipelines. The step's settings and log say whether a seed
# was set but not its value, so the values are sentinels too.
deid_seeds <- 770193:770196

# Terms that only anon() with `deid_patterns` removes, such as a drug or study
# name in a variable or dataset label.
deid_patterns <- list("[DRUG]" = "DRUGQX", "[STUDY]" = "STUDYQX")

# What the pipelines can't de-identify, each of which must be named in a
# warning. An entry has either `reason` (text of the warning that lists
# columns as "ds.col, ds.col", or "ds.col[sub]" inside a packed column) with
# `dataset` and `column`, or a regular expression `pattern` for the whole
# warning. A `column` may hold original keys or dates, as the warning prints
# them rewritten. Entries with a `dataset` (and
# `column`, unless the whole dataset is meant) are left unchanged by the
# pipelines; the invariants then drop them, as a user acting on the warnings
# would, before checking that nothing else leaks. Entries without a `dataset`
# describe values that were removed, so there is nothing to drop.
deid_flagged <- list(
  list(dataset = "dm", column = "COVAL", reason = "still contain original `USUBJID` values"),
  list(dataset = "dm", column = "LINKS", reason = "still contain original `USUBJID` values"),
  list(dataset = "dm", column = "REFS", reason = "still contain original `USUBJID` values"),
  # A packed data frame column is data, like a list column: the packed column
  # (one of its columns is named with a key) and its columns holding keys are
  # named; dropping any of them drops the packed column.
  list(dataset = "dm", column = "PACK", reason = "still contain original `USUBJID` values"),
  list(dataset = "dm", column = "PACK[REF]", reason = "still contain original `USUBJID` values"),
  list(dataset = "dm", column = paste0("PACK[", deid_keys[2], "]"), reason = "still contain original `USUBJID` values"),
  list(dataset = "dm", column = "COVAL", reason = "contain dates that were not shifted"),
  # A column inside the packed column, named with a key, that holds dates.
  # The dates step names columns inside packed columns by position (it can't
  # recognise original keys there once the IDs step has run).
  list(dataset = "dm", column = "PACK[4]", reason = "contain dates that were not shifted"),
  list(dataset = "vs", column = "usubjid", reason = "named like identifier columns in another case"),
  list(dataset = "ad", column = "TRTSDT", reason = "SAS date or datetime format"),
  list(dataset = "notes", pattern = "^Datasets without `USUBJID`[^.]*: notes \\(usubjid, NOTEDTC\\)\\.$"),
  # The row of `pr` with a blank key: its SUBJID and its 7 dates.
  list(pattern = "^1 value\\(s\\) of `SUBJID` in rows without a `USUBJID` could not be mapped and were removed\\.$"),
  list(pattern = "^7 date value\\(s\\) in rows without a `USUBJID` were removed\\.$")
)
# An S4 object held in an attribute of a column named with a key: kept, and
# named with the key rewritten (or shown as "[KEY]" by the dates step). It is
# dropped with other_attributes = "drop".
deid_flagged <- c(deid_flagged, list(list(
  id = "s4_attribute",
  pattern = "^These columns or datasets hold S4 objects in attributes, [^:]*: vs\\.(SUBJ-[0-9]+|\\[KEY\\])\\. Use"
)))
if (requireNamespace("haven", quietly = TRUE)) {
  deid_flagged <- c(deid_flagged, list(
    list(pattern = "^Value labels of these columns were merged because the rewrite made their codes equal: dm\\.VISIT\\.$")
  ))
}

# `flagged` with the entries whose `id` is in `drop` removed and those in
# `replace` (a list of entries by id) replaced.
deid_flagged_with <- function(drop = character(0), replace = list(), flagged = deid_flagged) {
  out <- list()
  for (f in flagged) {
    if (isTRUE(f$id %in% drop)) next
    if (!is.null(f$id) && !is.null(replace[[f$id]])) f <- utils::modifyList(f, replace[[f$id]])
    out <- c(out, list(f))
  }
  out
}

# An S4 class for the fixture, defined in an environment of its own.
deid_s4_env <- new.env()
deid_note <- methods::setClass("DeidNote", methods::representation(text = "character"), where = deid_s4_env)

# The regular expression a flagged entry's warning must match: for a
# `reason`, the column must appear in that reason's own column list.
deid_flag_pattern <- function(f) {
  if (!is.null(f$pattern)) {
    return(f$pattern)
  }
  column <- gsub("([][.\\\\(){}^$*+?|])", "\\\\\\1", paste0(f$dataset, ".", f$column), perl = TRUE)
  paste0(f$reason, "[^.]*: ([^ ,;]+, )*", column, "([,.;]|$)")
}

# A flagged entry with its column as the warnings print it: rewritten as the
# pipeline rewrites text (see deid_rewrite_text()).
deid_rewrite_flag <- function(f, oracle) {
  if (!is.null(f$column)) {
    f$column <- deid_rewrite_text(f$column, oracle)
  }
  f
}

# The columns listed in a warning of the form "...: ds.col, ds.col. ...".
deid_listed_columns <- function(warning) {
  strsplit(sub("\\.( .*)?$", "", sub("^[^:]*: ", "", warning)), ", ", fixed = TRUE)[[1]]
}

deid_study_day <- function(date, ref) as.integer(date - ref) + (date >= ref)

# format() with English month abbreviations whatever the user's locale.
deid_format_c <- function(x, format) {
  old <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  format(x, format)
}

deid_vs_times <- function(tz) {
  dates <- rep(deid_start, each = 2) + c(0, 7)
  as.POSIXct(paste(format(dates), "08:30"), tz = tz)
}

deid_fixture <- function() {
  n <- length(deid_keys)

  dm <- data.frame(
    USUBJID = deid_keys,
    SUBJID = deid_subjids,
    AGE = c(34, 51, 47, 62, 29, 58, 41, 66),
    RFSTDTC = format(deid_start),
    # A factor date column with a lower-case name, and an unused level that
    # still holds a real date.
    rficdtc = factor(format(deid_start - 14), levels = c(format(deid_start - 14), "2081-01-01")),
    RFSTDT = deid_start,
    COVAL = paste("Patient", deid_keys, "seen on", format(deid_start + 3)),
    row.names = deid_keys,
    stringsAsFactors = FALSE
  )
  attr(dm$USUBJID, "label") <- "Unique Subject Identifier"
  attr(dm$RFSTDTC, "label") <- "Subject Reference Start Date/Time"
  attr(dm$rficdtc, "label") <- "Date/Time of Informed Consent"
  # A list column holding a key, which can't be rewritten and is flagged.
  dm$LINKS <- I(as.list(c(deid_keys[1], "x", "y", "z", "w", "v", "u", "t")))
  # A list column whose elements are named with keys, which is flagged too.
  dm$REFS <- I(lapply(c(2:8, 1), function(i) stats::setNames(list(1), deid_keys[i])))
  # A packed data frame column holding other subjects' keys, one of its
  # columns named with a key.
  dm$PACK <- data.frame(REF = deid_keys[c(3:8, 1:2)], N = 1:8)
  dm$PACK[[deid_keys[2]]] <- deid_keys[c(4:8, 1:3)]
  dm$PACK[[deid_keys[7]]] <- format(deid_start[c(2:8, 1)] + 2)
  # A factor with contrasts, which are structural.
  dm$SEXF <- factor(rep(c("F", "M"), 4))
  stats::contrasts(dm$SEXF) <- stats::contr.sum(2)
  # A factor whose contrasts matrix is named by a level holding a
  # user-pattern term, which anon() must redact with the levels.
  dm$ARMF <- factor(rep(c("DRUGQX", "Placebo"), 4))
  stats::contrasts(dm$ARMF) <- stats::contr.treatment(2)
  # SPSS variable labels (as foreign::read.spss writes them), named by
  # column, one name holding a user-pattern term.
  attr(dm, "variable.labels") <- c(AGE = "Age", DRUGQX_DOSE = "Dose")
  # haven-labelled columns (when haven is installed): a key and a user-pattern
  # term in value labels, and a key as a user-defined missing value. Every exit
  # must also handle them, JSON included.
  if (requireNamespace("haven", quietly = TRUE)) {
    # Value labels whose codes are real dates: both become "[DATE]", so they
    # are merged, with a warning.
    dm$VISIT <- haven::labelled(
      rep(c("V1", "V2"), 4),
      labels = stats::setNames(format(deid_start[5:6]), c("first visit", "second visit"))
    )
    dm$GRP <- haven::labelled(c(1, 2, 2, 2, 2, 2, 2, 2), labels = stats::setNames(c(1, 2), c(deid_keys[1], "Other")))
    dm$ARMCD <- haven::labelled(c(1, 2, 1, 2, 1, 2, 1, 2), labels = c("DRUGQX arm" = 1, "Placebo" = 2))
    dm$SITE <- haven::labelled_spss(rep(c("S1", "S1", "S2", "S2"), 2), na_values = deid_keys[5])
    # A list column of labelled vectors whose value labels hold a user-pattern
    # term, which anon() must redact there too.
    dm$ARMS <- I(lapply(1:8, function(i) haven::labelled(i, labels = c("DRUGQX dose" = 1))))
  }

  vs_dates <- rep(deid_start, each = 2) + c(0, 7)
  vs <- data.frame(
    USUBJID = factor(rep(deid_keys, each = 2), levels = c(deid_keys, "ZQ-7701-0009")),
    VSDTC = paste0(format(vs_dates), "T08:30"),
    VSDY = deid_study_day(vs_dates, rep(deid_start, each = 2)),
    VSSTRESN = seq_len(2 * n) + 60,
    VSDTM = deid_vs_times("UTC"),
    # Local times on either side of a daylight saving change: shifting by
    # calendar days keeps 08:30, shifting by 86400 seconds a day does not.
    VSNYDTM = deid_vs_times("America/New_York"),
    usubjid = rep(deid_keys, each = 2),
    stringsAsFactors = FALSE
  )
  attr(vs$USUBJID, "label") <- "Unique Subject Identifier"
  # A contrasts matrix on the key, named by the keys: dropped when the keys
  # are replaced.
  stats::contrasts(vs$USUBJID) <- stats::contr.treatment(levels(vs$USUBJID))
  attr(vs$VSSTRESN, "label") <- "DRUGQX Concentration"
  # Text that isn't valid UTF-8 (a stray Windows-1252 apostrophe) holding a
  # user-pattern term, in values and a label: anon() must still redact it.
  vs$VSCOM <- rep(c("DRUGQX\x92s note", "none"), n)
  attr(vs$VSCOM, "label") <- "DRUGQX\x92s comment"
  attr(vs, "label") <- "STUDYQX Vital Signs"
  # Free-text units on a number (as Hmisc writes them) holding a key and a
  # date: text there, though `units` is structural on a difftime.
  vs$VSWT <- 70 + seq_len(2 * n)
  attr(vs$VSWT, "units") <- paste("kg, weighed for", deid_keys[5], "on", format(deid_start[5] + 1))
  # Keys in a variable label, a comment, a dataset attribute and a column name.
  vs$VSPOS <- "SITTING"
  attr(vs$VSPOS, "label") <- paste("Position for", deid_keys[3])
  comment(vs$VSPOS) <- deid_keys[4]
  attr(vs, "note") <- paste("from", deid_keys[8])
  vs[[deid_keys[3]]] <- 1
  attr(vs[[deid_keys[3]]], "note") <- deid_note(text = "entered by the site")
  # A key and a real date in an attribute that holds a list, and in an
  # attribute of that list's element.
  attr(vs$VSPOS, "source") <- list(by = structure(paste("entered for", deid_keys[6]), on = paste("on", format(deid_start[2]))))
  # A data frame held in an attribute, whose values are text about the data.
  attr(vs$VSPOS, "map") <- data.frame(who = deid_keys[7], when = factor(format(deid_start[4] + 1)))

  ex <- data.frame(EXSTDTC = format(deid_start), EXENDTC = substr(format(deid_start + 20), 1, 7))
  ex$USUBJID <- structure(deid_keys, label = "Unique Subject Identifier")
  ex$EXSTDY <- deid_study_day(deid_start, deid_start)
  # A real date in a variable label.
  ex$EXDOSE <- 10
  attr(ex$EXDOSE, "label") <- paste("Dose from", format(deid_start[1]))
  # Real dates held in attributes as Date and POSIXct values.
  attr(ex$EXDOSE, "cut") <- deid_start[3]
  attr(ex$EXDOSE, "cut_time") <- deid_vs_times("UTC")[5]

  suppdm <- data.frame(
    USUBJID = deid_keys, QNAM = "ENRLDT", QVAL = format(deid_start - 10),
    stringsAsFactors = FALSE
  )
  relsub <- data.frame(USUBJID = deid_keys, RSUBJID = deid_keys[c(2:n, 1)])

  # A numeric SAS date (days since 1960-01-01), which can't be shifted.
  ad <- data.frame(USUBJID = deid_keys)
  ad$TRTSDT <- as.numeric(deid_start - as.Date("1960-01-01"))
  attr(ad$TRTSDT, "format.sas") <- "DATE9"

  # A dataset without the key that still holds keys and dates.
  notes <- data.frame(usubjid = deid_keys[1], NOTEDTC = format(deid_start[1] + 2))

  # The forms of ISO 8601 dates, SUBJID in a second dataset, and a last row
  # with a blank key (holding the first subject's dates and another SUBJID)
  # whose SUBJID and dates can't be tied to a subject.
  start <- c(deid_start, deid_start[1])
  day <- function(k) format(start + k)
  pr <- data.frame(
    USUBJID = c(deid_keys, ""),
    SUBJID = c(deid_subjids, "ZQ0099"),
    PRSTDTC = paste0(day(-1), "T08:30:15"),
    PRENDTC = paste0(day(5), "T09:45:30+01:00"),
    PRRNDTC = paste0(day(-3), "/", day(4)),
    PRDUDTC = paste0(day(1), "/P1D"),
    PRMTDTC = paste0(substr(day(0), 1, 7), "--T08:00"),
    PRYDDTC = paste0(substr(day(0), 1, 4), "---", substr(day(0), 9, 10)),
    PRYRDTC = substr(day(0), 1, 4),
    stringsAsFactors = FALSE
  )

  # A dataset without the key whose row names are keys, and a dataset named
  # with a key.
  ta <- data.frame(ARM = c("A", "B"), row.names = deid_keys[2:3])
  # A time series column, whose `tsp` is structural (in a dataset without the
  # key: sorting rows drops a time series' class).
  ta$TSV <- stats::ts(c(1, 2))
  # A rowwise tibble, whose `groups` is structural.
  tw <- dplyr::rowwise(tibble::tibble(ARM = c("A", "B"), N = 1:2))
  named <- data.frame(X = 1)
  # A grouped tibble with a dataset label, which must keep its label and
  # grouping.
  te <- dplyr::group_by(tibble::tibble(ARMCD = c("A", "B", "A"), ETCD = c("SCRN", "TRT", "FU")), ARMCD)
  attr(te, "label") <- "Trial Elements"

  # A vector column named by subject, as made with split() (a tibble keeps the
  # names, a data frame drops them).
  vs_means <- vapply(split(vs$VSSTRESN, as.character(vs$USUBJID)), mean, numeric(1))
  vsum <- tibble::tibble(USUBJID = deid_keys, VSMEAN = vs_means[deid_keys])

  out <- list(
    dm = dm, vs = vs, ex = ex, suppdm = suppdm, relsub = relsub, ad = ad, notes = notes, pr = pr, ta = ta, te = te,
    vsum = vsum, tw = tw
  )
  out[[deid_keys[4]]] <- named
  out
}

# Text that describes the data (attributes and names) as the pipelines should
# leave it: each original key, as a whole word, becomes its subject's
# pseudonym (longer keys first, as one key starts another), and each ISO 8601
# full date becomes "[DATE]".
deid_rewrite_text <- function(x, oracle) {
  # Text is searched in UTF-8 (text marked latin1 converted), with the bytes
  # of text that isn't valid UTF-8 written as "<92>"; that converted text is
  # kept only where something was rewritten.
  repaired <- enc2utf8(x)
  invalid <- !is.na(repaired) & !validUTF8(repaired)
  repaired[invalid] <- iconv(repaired[invalid], "UTF-8", "UTF-8", sub = "byte")
  new <- repaired
  keys <- names(oracle$new_key)
  for (k in keys[order(-nchar(keys))]) {
    new <- gsub(paste0("(?<![[:alnum:]])\\Q", k, "\\E(?![[:alnum:]])"), oracle$new_key[[k]], new, perl = TRUE)
  }
  new <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}", "[DATE]", new)
  keep <- !is.na(new) & new == repaired
  new[keep] <- x[keep]
  new
}

# Attributes the pipelines always keep (structural ones, and the grouping of
# a grouped tibble) or always rewrite (labels, comments, value labels and
# missing-value codes). Others are rewritten, or dropped when the oracle's
# `other` is "drop".
deid_core <- c("label", "comment", "labels", "na_values", "na_range")

# The attributes of `x` that are structural for its class (kept apart from
# the package's own table, covering the same cases): elsewhere an attribute
# with one of these names is text.
deid_structural <- function(x) {
  c(
    "class", "names", "dimnames", "row.names", "dim",
    if (is.factor(x)) c("levels", "contrasts"),
    if (inherits(x, "POSIXt")) "tzone",
    if (inherits(x, c("difftime", "units"))) "units",
    if (inherits(x, "ts")) "tsp",
    if (inherits(x, "Surv")) c("type", "states"),
    if (inherits(x, c("vctrs_list_of", "vctrs_vctr"))) "ptype",
    if (inherits(x, c("grouped_df", "rowwise_df"))) "groups",
    if (inherits(x, "data.table")) c("sorted", "index", ".internal.selfref"),
    if (is.atomic(x)) c("format.sas", "informat.sas", "format.spss", "format.stata"),
    if (is.atomic(x) && is.numeric(attr(x, "display_width", exact = TRUE))) "display_width"
  )
}

# A data frame with the text that describes it rewritten as
# deid_rewrite_text() says, and its values untouched: column names, character
# row names, and the attributes of the data frame and its columns (see
# deid_rewrite_attributes()). Packed data frame columns are data and are left
# as they are. With `values` (a data frame held in an attribute), its text
# values are rewritten too.
deid_rewrite_frame <- function(df, oracle, values = FALSE) {
  cls <- oldClass(df)
  x <- unclass(df)
  for (j in seq_along(x)) {
    if (values) {
      x[[j]] <- deid_rewrite_value(x[[j]], oracle)
    } else if (!is.data.frame(x[[j]])) {
      x[[j]] <- deid_rewrite_attributes(x[[j]], oracle)
    }
  }
  attr(x, "names") <- deid_rewrite_text(names(x), oracle)
  row_names <- attr(x, "row.names")
  if (is.character(row_names)) {
    attr(x, "row.names") <- deid_rewrite_text(row_names, oracle)
  }
  x <- deid_rewrite_attributes(x, oracle, frame = TRUE, structural = deid_structural(df))
  class(x) <- cls
  x
}

# Attributes rewritten at any depth, except the structural ones (and a data
# frame's names, and a grouped tibble's grouping); with `oracle$other`
# "drop", the attributes that are neither structural nor core are dropped.
# Value-label and missing-value codes are rewritten only when no value of the
# column equals the code or its rewrite; value labels whose codes become equal
# are merged into one, their labels joined with " / ", and equal missing-value
# codes into one.
deid_rewrite_attributes <- function(x, oracle, frame = FALSE, structural = deid_structural(x)) {
  values <- if (!frame && !is.list(x)) as.character(if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else unclass(x))
  for (name in setdiff(names(attributes(x)), structural)) {
    value <- attr(x, name, exact = TRUE)
    if (isS4(value) && !identical(oracle$other, "drop")) {
      next
    }
    if (identical(oracle$other, "drop") && !name %in% deid_core) {
      value <- NULL
    } else if (name %in% c("labels", "na_values", "na_range")) {
      if (!is.null(names(value))) {
        names(value) <- deid_rewrite_text(names(value), oracle)
      }
      if (is.character(value)) {
        original <- attr(x, name, exact = TRUE)
        new <- deid_rewrite_text(value, oracle)
        free <- !value %in% values & !new %in% values
        # A range whose ends would be in descending order is kept.
        if (name == "na_range" && length(new) == 2 && !isTRUE(new[[1]] <= new[[2]])) {
          free[] <- FALSE
        }
        value[free] <- new[free]
        if (name != "na_range" && anyDuplicated(unname(value))) {
          first <- !duplicated(unname(value))
          if (!is.null(names(value))) {
            # The original labels are joined, then rewritten once.
            names(value)[first] <- vapply(unname(value)[first], function(code) {
              members <- unname(value) == code
              if (sum(members) == 1) {
                return(names(value)[members])
              }
              deid_rewrite_text(paste(names(original)[members], collapse = " / "), oracle)
            }, character(1))
          }
          value <- value[first]
        }
      }
    } else {
      value <- deid_rewrite_value(value, oracle)
    }
    attr(x, name) <- value
  }
  if (!frame && !is.null(names(x))) {
    names(x) <- deid_rewrite_text(names(x), oracle)
  }
  if (!is.null(dimnames(x))) {
    dimnames(x) <- lapply(dimnames(x), function(d) if (!is.null(d)) deid_rewrite_text(d, oracle))
  }
  x
}

# A value held in an attribute: its text and its attributes, at any depth.
# Date and date-time values become "[DATE]".
deid_rewrite_value <- function(v, oracle) {
  if (inherits(v, c("Date", "POSIXt"))) {
    return(rep("[DATE]", length(v)))
  }
  if (is.data.frame(v)) {
    return(deid_rewrite_frame(v, oracle, values = TRUE))
  }
  if (is.factor(v)) {
    levels(v) <- deid_rewrite_text(levels(v), oracle)
  }
  if (is.list(v)) {
    for (i in seq_along(v)) {
      if (!is.null(v[[i]])) v[[i]] <- deid_rewrite_value(v[[i]], oracle)
    }
  } else if (is.character(v)) {
    v[] <- deid_rewrite_text(v, oracle)
  }
  deid_rewrite_attributes(v, oracle)
}

# Hiding places the package doesn't handle yet. Each `plant` adds one to the
# fixture. A gap with `sentinel` must leak it through some exit (only exits
# that apply `deid_patterns` when `pattern_exits_only` is TRUE). A gap with
# `errors` must make those exits error, besides the exits expected to error
# without it. `needs` names suggested packages the gap uses; it is skipped
# without them. `handled_by` names pipelines in which the gap must not leak
# (for example because they drop the attribute). When a fix makes a gap stop
# leaking or stop breaking its exits, its test fails: move the planting into
# deid_fixture() (with the expected handling) and remove the gap.
deid_known_gaps <- list(
  # anon() redacts character attributes but not other ones: list-valued, such
  # as `variable.labels` (haven/foreign SPSS) or readr's `spec`, or
  # factor-valued. Pipelines that drop other attributes remove them.
  list_attribute_pattern = list(
    sentinel = "DRUGQX",
    pattern_exits_only = TRUE,
    handled_by = "ids_then_dates_drop",
    plant = function(d) {
      attr(d$vs, "variable.labels") <- list(VSSTRESN = "DRUGQX result")
      # A factor held in an attribute, which anon() doesn't redact either.
      attr(d$vs$VSSTRESN, "assay") <- factor("DRUGQX assay")
      d
    }
  ),
  # Nested data (its own PR): dates in nested tibbles, in list columns and in
  # attributes of list-column elements are not shifted or flagged, and keys in
  # a nested tibble with its own key column are not replaced.
  nested_tibble_date = list(
    sentinel = as.character(as.numeric(deid_start[3])),
    plant = function(d) {
      d$dm$NEST <- I(lapply(seq_along(deid_start), function(i) tibble::tibble(DT = deid_start[i])))
      d
    }
  ),
  list_column_dates = list(
    sentinel = as.character(as.numeric(deid_start[4])),
    plant = function(d) {
      d$dm$WHEN <- I(as.list(deid_start))
      d
    }
  ),
  list_element_date_attribute = list(
    sentinel = as.character(as.numeric(deid_start[5])),
    plant = function(d) {
      d$dm$OBS <- I(lapply(seq_along(deid_start), function(i) structure(i, when = deid_start[i])))
      d
    }
  ),
  nested_keyed_tibble = list(
    sentinel = deid_keys[6],
    plant = function(d) {
      d$dm$SUB <- I(lapply(deid_keys, function(k) tibble::tibble(USUBJID = k, X = 1)))
      d
    }
  ),
  # Keys glued to digits in names (as unlist(split(V, USUBJID)) makes them,
  # "<key>1"), which the whole-word search doesn't find (Q2 PR).
  key_glued_to_digits = list(
    sentinel = deid_keys[1],
    plant = function(d) {
      d$vsx <- tibble::tibble(V = unlist(split(d$vs$VSSTRESN, as.character(d$vs$USUBJID))))
      d
    }
  ),
  subjid_in_text = list(
    sentinel = "ZQ0002",
    plant = function(d) {
      d$dm$COMM <- c("see ZQ0002", "", "", "", "", "", "", "")
      d
    }
  )
)

# Strings that must not appear in anything that leaves the package: keys and
# related IDs (also without separators), every planted real full date (also as
# 06MAR2031 and 20310306), the numbers behind real Date and POSIXct values,
# and the pipelines' seeds. Matching ignores case; numeric sentinels match
# only as whole numbers. Partial and year-only dates are checked against the
# secrets instead, because a shifted full date may fall in the same month or
# year. With `patterns = TRUE`, the user-pattern terms too.
deid_sentinels <- function(patterns = FALSE) {
  days <- c(-14, -10, -3, -1, 0, 1, 2, 3, 4, 5, 7)
  dates <- rep(deid_start, each = length(days)) + days
  # Including the unused factor level planted in vs$USUBJID and the SUBJID of
  # the row of `pr` without a key.
  ids <- c(deid_keys, "ZQ-7701-0009", deid_subjids, "ZQ0099")
  c(
    unique(c(ids, gsub("[^[:alnum:]]", "", ids))),
    # The unused factor level planted in dm$rficdtc.
    "2081-01-01",
    format(dates),
    toupper(deid_format_c(dates, "%d%b%Y")),
    format(dates, "%Y%m%d"),
    as.character(as.numeric(c(deid_start, deid_start + 7))),
    as.character(as.numeric(c(deid_vs_times("UTC"), deid_vs_times("America/New_York")))),
    as.character(deid_seeds),
    if (patterns) unlist(deid_patterns, use.names = FALSE)
  )
}

# Each pipeline record has `run` (takes the fixture, returns a step),
# `keep_secrets`, `prefix` (of character pseudonyms), `min_days` and
# `max_days`. A pipeline without secrets names, in `same_as`, the pipeline
# with secrets it must match. Pipelines that handle the fixture differently
# on purpose describe it with these optional fields, rather than loosen the
# checks (defaults in `deid_pipeline_defaults`):
# - `key`: the subject key column;
# - `flagged`: what can't be de-identified (see `deid_flagged`);
# - `expected`: named list, `"ds.col" = function(x, keys, oracle)` giving a
#   column's expected values for all of the input's rows, instead of
#   deid_expected_column();
# - `types`: named list, `"ds.col" = ` the column's expected typeof(), when it
#   differs from the input's;
# - `attributes`: named list, `"ds.col" = ` names of attributes allowed to
#   change;
# - `keep_rows`: `function(ds, original, oracle)` giving a logical vector of
#   the input rows expected to remain (`oracle` is NULL for pipelines without
#   secrets);
# - `removed`: `"ds.col"` or `"ds"` names of columns and datasets expected to
#   be removed;
# - `other_attributes`: what the pipeline does with attributes beyond the
#   core ones ("rewrite" or "drop"), which the expected attributes follow.
deid_pipelines <- list(
  ids_then_dates = list(
    run = function(data) {
      anon_subject_dates(
        anon_subject_ids(data, keep_secrets = TRUE, seed = deid_seeds[1]),
        keep_secrets = TRUE, seed = deid_seeds[2]
      )
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365
  ),
  dates_then_ids = list(
    run = function(data) {
      anon_subject_ids(
        anon_subject_dates(data, keep_secrets = TRUE, seed = deid_seeds[3]),
        keep_secrets = TRUE, seed = deid_seeds[4]
      )
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365
  ),
  ids_then_dates_no_secrets = list(
    run = function(data) anon_subject_dates(anon_subject_ids(data, seed = deid_seeds[1]), seed = deid_seeds[2]),
    keep_secrets = FALSE, prefix = "SUBJ-", min_days = 30, max_days = 365, same_as = "ids_then_dates"
  ),
  ids_then_dates_drop = list(
    run = function(data) {
      anon_subject_dates(
        anon_subject_ids(data, keep_secrets = TRUE, seed = deid_seeds[1], other_attributes = "drop"),
        keep_secrets = TRUE, seed = deid_seeds[2], other_attributes = "drop"
      )
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365, other_attributes = "drop",
    flagged = deid_flagged_with(drop = "s4_attribute")
  )
)

deid_pipeline_defaults <- list(
  key = "USUBJID",
  flagged = deid_flagged,
  max_days = 365,
  expected = list(),
  types = list(),
  attributes = list(),
  keep_rows = function(ds, original, oracle) rep(TRUE, nrow(original)),
  removed = character(0),
  other_attributes = "rewrite"
)

# A pipeline record with the optional fields filled in.
deid_complete_pipeline <- function(pipeline) {
  for (field in names(deid_pipeline_defaults)) {
    if (is.null(pipeline[[field]])) {
      pipeline[field] <- list(deid_pipeline_defaults[[field]])
    }
  }
  pipeline
}

# Each exit has `fn` (takes a step, returns text, or raw bytes for
# serialized objects: what a user could print, save, paste into a prompt or
# see in the app), `patterns` (TRUE if it applies `deid_patterns`, so
# user-pattern terms must be gone too) and `errors` (TRUE, or a function of
# the step, if it is expected to error; its error message is then checked).
# Messages and warnings raised by an exit are checked as well.
# `anon_secrets()` and serialize() of a step with secrets are left out on
# purpose: they are documented to hold the secrets.
deid_dput <- function(x) {
  capture.output(dput(x, control = c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17")))
}

deid_env_with <- function(step) {
  e <- new.env()
  assign("step", step, envir = e)
  e
}

deid_exits <- list(
  print = list(fn = function(step) capture.output(print(step))),
  print_nested = list(fn = function(step) capture.output(print(list(a = step)))),
  format = list(fn = function(step) format(step)),
  as_character = list(fn = function(step) as.character(step)),
  str = list(fn = function(step) capture.output(str(step))),
  summary = list(fn = function(step) capture.output(summary(step))),
  dput_data = list(fn = function(step) deid_dput(step$data)),
  dput_log_settings = list(fn = function(step) deid_dput(unclass(step)[c("log", "settings")])),
  print_secrets = list(fn = function(step) c(capture.output(print(step$secrets)), capture.output(str(step$secrets)))),
  json_secrets = list(
    fn = function(step) as_anon_json(list(secrets = step$secrets)),
    errors = function(step) !is.null(step$secrets)
  ),
  json = list(fn = function(step) as_anon_json(step)),
  json_data = list(fn = function(step) as_anon_json(step$data)),
  json_nested = list(fn = function(step) as_anon_json(list(a = list(step = step)))),
  anon = list(
    fn = function(step) {
      a <- anon(step, pattern_list = deid_patterns)
      c(capture.output(print(a)), deid_dput(unclass(a)))
    },
    patterns = TRUE
  ),
  anon_nested = list(
    fn = function(step) deid_dput(unclass(anon(list(a = step), pattern_list = deid_patterns))),
    patterns = TRUE
  ),
  serialize_anon = list(
    fn = function(step) serialize(anon(step, pattern_list = deid_patterns), NULL),
    patterns = TRUE
  ),
  inventory = list(
    fn = function(step) capture.output(print(anon_inventory(list(step = step), pattern_list = deid_patterns))),
    patterns = TRUE
  ),
  data_summary = list(
    fn = function(step) {
      capture.output(print(anon_data_summary(step, pattern_list = deid_patterns, example_values_n = 5, example_rows = 3)))
    },
    patterns = TRUE
  ),
  data_summary_keyed_rows = list(
    fn = function(step) {
      rows <- anon_example_rows(n = 3, key = "USUBJID")
      deid_dput(anon_data_summary(step, pattern_list = deid_patterns, example_values_n = 5, example_rows = rows))
    },
    patterns = TRUE
  ),
  report = list(
    fn = function(step) {
      report <- anon_report(deid_env_with(step), pattern_list = deid_patterns, example_values_n = 5, example_rows = 3)
      c(capture.output(print(report)), as_anon_json(report))
    },
    patterns = TRUE
  ),
  prompt_bundles = list(
    fn = function(step) {
      report <- anon_report(deid_env_with(step), pattern_list = deid_patterns, example_values_n = 5, example_rows = 3)
      unlist(lapply(c("markdown", "text", "json"), function(f) {
        as.character(anon_prompt_bundle(report = report, format = f))
      }))
    },
    patterns = TRUE
  ),
  # What the app shows: its inventory and object choices. The state also holds
  # the objects themselves, unredacted, for building reports.
  app_state = list(
    fn = function(step) {
      state <- anon_app_initial_state(deid_env_with(step), pattern_list = deid_patterns, initial_selection = "step")
      deid_dput(state[c("inventory", "raw_names", "choices", "selected")])
    },
    patterns = TRUE
  ),
  risk = list(
    fn = function(step) {
      risk <- anon_risk(step, quasi = list(dm = "AGE"))
      c(capture.output(print(risk)), as_anon_json(risk), deid_dput(anon_risk_subjects(step, quasi = list(dm = "AGE"))))
    }
  )
)

# Names of the exits expected to error on a step.
deid_expected_errors <- function(step) {
  expected <- vapply(deid_exits, function(e) {
    isTRUE(if (is.function(e$errors)) e$errors(step) else e$errors)
  }, logical(1))
  names(deid_exits)[expected]
}

# Run a pipeline, collecting its warnings and messages instead of printing
# them.
deid_run <- function(pipeline, data) {
  warnings <- character(0)
  messages <- character(0)
  step <- withCallingHandlers(
    pipeline$run(data),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(step = step, warnings = warnings, messages = messages)
}

# Drop the flagged columns and datasets from a step's data.
deid_drop_flagged <- function(step, flagged = deid_flagged) {
  for (f in flagged) {
    if (is.null(f$dataset)) {
      next
    }
    if (is.null(f$column)) {
      step$data[[f$dataset]] <- NULL
    } else {
      # "packed[column]": drop the packed data frame column.
      step$data[[f$dataset]][[sub("\\[.*", "", f$column)]] <- NULL
    }
  }
  step
}

# Run every exit on a step: a list with each exit's text (including messages,
# warnings and any error), its raw bytes if it returned raw, whether it
# errored, and whether it applies `deid_patterns`.
deid_run_exits <- function(step) {
  lapply(deid_exits, function(exit) {
    conditions <- character(0)
    errored <- FALSE
    out <- withCallingHandlers(
      tryCatch(exit$fn(step), error = function(e) {
        errored <<- TRUE
        conditionMessage(e)
      }),
      warning = function(w) {
        conditions <<- c(conditions, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        conditions <<- c(conditions, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    raw <- if (is.raw(out)) out
    text <- if (is.raw(out)) character(0) else as.character(out)
    list(text = c(text, conditions), raw = raw, errored = errored, patterns = isTRUE(exit$patterns))
  })
}

# ASCII letters in raw bytes, in lower case.
deid_lower_bytes <- function(bytes) {
  b <- as.integer(bytes)
  upper <- b >= 65L & b <= 90L
  b[upper] <- b[upper] + 32L
  as.raw(b)
}

# Whether `sentinel` occurs in `bytes`, ignoring case. A sentinel made only
# of digits must not be part of a longer number.
deid_found <- function(sentinel, bytes) {
  pattern <- deid_lower_bytes(charToRaw(sentinel))
  at <- grepRaw(pattern, bytes, fixed = TRUE, all = TRUE)
  if (length(at) == 0 || !grepl("^[0-9]+$", sentinel)) {
    return(length(at) > 0)
  }
  digit <- function(i) {
    ok <- i >= 1 & i <= length(bytes)
    out <- rep(FALSE, length(i))
    out[ok] <- bytes[i[ok]] >= as.raw(0x30) & bytes[i[ok]] <= as.raw(0x39)
    out
  }
  dot <- function(i) i >= 1 & i <= length(bytes) & bytes[pmax(i, 1)] == as.raw(0x2e)
  before <- at - 1
  after <- at + length(pattern)
  any(!digit(before) & !dot(before) & !digit(after))
}

# Sentinels found in each exit's text and raw bytes, as "exit: sentinel"
# strings. By default each exit is checked against deid_sentinels(), with the
# user-pattern terms for exits that apply them.
deid_leaks <- function(results, sentinels = NULL) {
  found <- character(0)
  for (name in names(results)) {
    r <- results[[name]]
    s <- if (is.null(sentinels)) deid_sentinels(patterns = isTRUE(r$patterns)) else sentinels
    bytes <- deid_lower_bytes(c(charToRaw(paste(r$text, collapse = "\n")), as.raw(0x0a), r$raw))
    hit <- vapply(s, deid_found, logical(1), bytes = bytes)
    if (any(hit)) {
      found <- c(found, paste0(name, ": ", paste(s[hit], collapse = ", ")))
    }
  }
  found
}

# From a step's secrets: per original key, the new key and the date offset in
# days; and what the pipeline does with other attributes (`other`).
deid_oracle <- function(step, other = "rewrite") {
  secrets <- anon_secrets(step)
  ids <- secrets$subject_ids
  offsets <- secrets$date_offsets
  new_key <- stats::setNames(as.character(ids$pseudonym), as.character(ids$key))
  # The offsets are keyed by the key as it was when the dates step ran: the
  # pseudonym if the IDs step ran first, otherwise the original key.
  offset_key <- as.character(offsets$key)
  steps <- names(step$settings)
  if (match("subject_ids", steps) < match("subject_dates", steps)) {
    offset_key <- names(new_key)[match(offset_key, new_key)]
  }
  list(new_key = new_key, offset = stats::setNames(offsets$offset_days, offset_key), other = other)
}
