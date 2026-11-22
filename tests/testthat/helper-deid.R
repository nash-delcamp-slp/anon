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

# A subject found only inside nested data (in the first of two shapes of data
# frames in a list column), in no dataset's key column, with its own start
# date, eleven years after the others'.
deid_nested_key <- "ZQ-7701-0010"
deid_nested_start <- as.Date("2119-03-07")
deid_all_keys <- c(deid_keys, deid_nested_key)
deid_all_starts <- c(deid_start, deid_nested_start)

# Numeric keys of at least 5 digits for the same subjects (and for the unused
# factor level planted in vs$USUBJID), by original key, for the numeric
# variant of the fixture (deid_numeric_fixture()). The last main key continues
# the first's digits, so the first must not be found inside it: numbers are
# matched as whole numbers. With fewer than ten subjects, pseudonym numbers
# sort the same as text and as numbers, as the invariants sort them.
deid_numeric_keys <- stats::setNames(
  c(77010001 + 0:6, 770100012, 77010010, 77010009),
  c(deid_all_keys, "ZQ-7701-0009")
)
# SUBJIDs of 5 digits for the numeric fixture, by original SUBJID: searched,
# and could be part of a number (see deid_scan_ids()).
deid_numeric_subjids <- stats::setNames(sprintf("%05d", 70000 + c(1:8, 99)), c(deid_subjids, "ZQ0099"))

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
  # The text values of a packed data frame column are data, like those of a
  # list column: its columns holding keys are named (one of them by its name,
  # which held a key and is rewritten); dropping any of them drops the packed
  # column.
  list(dataset = "dm", column = "PACK[REF]", reason = "still contain original `USUBJID` values"),
  list(dataset = "dm", column = paste0("PACK[", deid_keys[2], "]"), reason = "still contain original `USUBJID` values"),
  list(dataset = "dm", column = "COVAL", reason = "contain dates that were not shifted"),
  # A comment holding a SUBJID, and a key followed by digits at the end of a
  # value (as unlist() names elements): values are searched, not rewritten.
  list(dataset = "dm", column = "COMM", reason = "still contain original `SUBJID` values"),
  list(id = "comm_glued_key", dataset = "dm", column = "COMM", reason = "still contain original `USUBJID` values"),
  # A column inside the packed column, named with a key, that holds dates as
  # text. The dates step names columns inside nested data by position.
  list(dataset = "dm", column = "PACK[4]", reason = "contain dates that were not shifted"),
  # A list column holding objects that can't be de-identified.
  list(dataset = "dm", column = "FIT", reason = "hold objects that can't be de-identified"),
  # A factor naming subjects inside nested data.
  list(dataset = "lof", column = "data[[]][V]", reason = "still contain original `USUBJID` values"),
  list(dataset = "vs", column = "usubjid", reason = "named like identifier columns in another case"),
  list(dataset = "ad", column = "TRTSDT", reason = "SAS date or datetime format"),
  # With a per-test table, not a dataset of subjects: only some of its rows
  # hold a data frame with a subject, so its own dates are left as they are.
  list(dataset = "notes", pattern = "^Datasets without `USUBJID`[^.]*: notes \\(usubjid, NOTEDTC\\); qc \\(RUNDT\\)\\.$"),
  # The row of `pr` with a blank key: its SUBJID and its 7 dates; and the row
  # of a data frame nested in `lbn` with a blank key: its date, and the date
  # of the data frame nested in that row.
  list(pattern = "^1 value\\(s\\) of `SUBJID` in rows without a `USUBJID` could not be mapped and were removed\\.$"),
  list(pattern = "^9 date value\\(s\\) in rows without a `USUBJID` were removed\\.$")
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
    list(pattern = "^Value labels of these columns were merged because the rewrite made their codes equal: dm\\.VISIT\\.$"),
    # A key's missing-value code that is another subject's key: the value
    # becomes missing, with a warning.
    list(pattern = "^These identifier columns had missing-value codes that are also subject keys elsewhere: sv\\.USUBJID\\. ")
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

# Weights in vs, two rows per subject (see deid_risk_quasi).
deid_vs_weights <- c(71.3, 68.9, 69.6, 72.4, 70.8, 67.7, 73.2, 70.6, 61.4, 63, 58.6, 62.2, 79.5, 81.3, 88.7, 91.2)

deid_fixture <- function() {
  n <- length(deid_keys)

  dm <- data.frame(
    USUBJID = deid_keys,
    SUBJID = deid_subjids,
    # Ages and races for the generalization pipelines (see
    # deid_generalized_expected()).
    AGE = c(34, 37, 51, 58, 43, 47, 62, 66),
    RACE = c("WHITE", "WHITE", "WHITE", "WHITE", "ASIAN", "WHITE", "WHITE", "WHITE"),
    RFSTDTC = format(deid_start),
    # A factor date column with a lower-case name, and an unused level that
    # still holds a real date.
    rficdtc = factor(format(deid_start - 14), levels = c(format(deid_start - 14), "2081-01-01")),
    RFSTDT = deid_start,
    COVAL = paste("Patient", deid_keys, "seen on", format(deid_start + 3)),
    COMM = c(paste("see", deid_subjids[2]), paste0(deid_keys[3], "1"), rep("", n - 2)),
    row.names = deid_keys,
    stringsAsFactors = FALSE
  )
  # A SUBJID in a label: rewritten to the pseudonym's number.
  attr(dm$AGE, "label") <- paste("Age of", deid_subjids[3], "at screening")
  # SUBJIDs next to "." and "," (numbers in the numeric fixture, where those
  # after a number and a separator, or before ".5", are parts of numbers).
  attr(dm$COMM, "label") <- paste0(
    "Ratio 25.", deid_subjids[6], ", dose ", deid_subjids[6], ".5, list 1.", deid_subjids[7], ",", deid_subjids[8],
    "; ", deid_subjids[4], ",", deid_subjids[5]
  )
  attr(dm$USUBJID, "label") <- "Unique Subject Identifier"
  attr(dm$RFSTDTC, "label") <- "Subject Reference Start Date/Time"
  attr(dm$rficdtc, "label") <- "Date/Time of Informed Consent"
  # A list column holding a key, which can't be rewritten and is flagged.
  dm$LINKS <- I(as.list(c(deid_keys[1], "x", "y", "z", "w", "v", "u", "t")))
  # A list column whose elements are named with keys: names are rewritten.
  dm$REFS <- I(lapply(c(2:8, 1), function(i) stats::setNames(list(1), deid_keys[i])))
  # A packed data frame column holding other subjects' keys, one of its
  # columns named with a key, and a key in its label.
  dm$PACK <- data.frame(REF = deid_keys[c(3:8, 1:2)], N = 1:8)
  dm$PACK[[deid_keys[2]]] <- deid_keys[c(4:8, 1:3)]
  dm$PACK[[deid_keys[7]]] <- format(deid_start[c(2:8, 1)] + 2)
  attr(dm$PACK, "label") <- paste("Links of", deid_keys[1])
  # Nested data: a data frame per row holding the row's subject's date (with
  # the subject's key in its label, so that each differs in more than its
  # values), a list column of dates, numbers with a date in an attribute, and
  # a data frame per row with its own key column.
  dm$NEST <- I(lapply(seq_along(deid_start), function(i) {
    tibble::tibble(DT = structure(deid_start[i], label = paste("Date of", deid_keys[i])))
  }))
  dm$WHEN <- I(as.list(deid_start))
  dm$OBS <- I(lapply(seq_along(deid_start), function(i) structure(i, when = deid_start[i])))
  # Numbers alike but for the key in a note: each is rewritten with its own.
  dm$NOTED <- I(lapply(seq_len(n), function(i) structure(i, note = paste("for", deid_keys[i]))))
  # Each data frame's own key is another subject's than its row's: its own
  # key wins.
  dm$SUB <- I(lapply(deid_keys[c(2:n, 1)], function(k) tibble::tibble(USUBJID = k, X = 1)))
  # A list column named with a key followed by a digit, holding data frames
  # with the key: the log names its columns by path, rewritten.
  dm[[paste0(deid_keys[1], "1")]] <- I(lapply(deid_keys, function(k) tibble::tibble(USUBJID = k, Y = 2)))
  # Data frames of two shapes, with their own key: the first shape holds rows
  # of the subject found only here; one data frame holds rows of two subjects
  # in an order the keys don't follow.
  dm$MIX <- I(lapply(seq_len(n), function(i) {
    ks <- if (i == 1) c(deid_nested_key, deid_keys[i]) else deid_keys[i]
    start <- deid_all_starts[match(ks, deid_all_keys)]
    out <- tibble::tibble(USUBJID = ks, MXDTC = format(start + 1))
    if (i > 4) out$MXN <- 1
    # A tibble holding text row names: reset as it is sorted.
    if (i == 1) attr(out, "row.names") <- c("first", "second")
    out
  }))
  # Plain data frames belonging to their row, with the row names of the rows
  # they were taken from, which the IDs step resets.
  dm$RN <- I(lapply(seq_len(n), function(i) data.frame(V = i, row.names = 10L + i)))
  # The same in tibbles holding row names (as tibble::as_tibble(rownames =
  # NA) keeps them), as positions in text.
  dm$TRN <- I(lapply(seq_len(n), function(i) {
    tibble::as_tibble(data.frame(V = i, row.names = as.character(20L + i)), rownames = NA)
  }))
  # A packed column with text row names: its rows move with the dataset's,
  # which are sorted, so they are reset.
  dm$PRN <- data.frame(Z = seq_len(n), row.names = paste0("obs", seq_len(n)))
  # Tables of model coefficients, named by term: their row names are text
  # and are kept.
  dm$COEF <- I(lapply(seq_len(n), function(i) data.frame(Estimate = c(1, i), row.names = c("(Intercept)", "TIME"))))
  # Date-times held in a list column as POSIXlt values, in a zone whose
  # `tzone` names its standard and daylight saving abbreviations too.
  dm$LT <- I(lapply(seq_len(n), function(i) as.POSIXlt(paste(format(deid_start[i] + 1), "09:00"), tz = "America/New_York")))
  # A list column holding objects that can't be de-identified (a resampling
  # split holding keys, as rsample makes them, and a fitted model).
  dm$FIT <- I(c(
    list(structure(list(data = data.frame(USUBJID = deid_keys[1:2]), in_id = 1L), class = "rsplit")),
    list(structure(list(coefficients = c(AGE = 0.5)), class = "lm")),
    rep(list(NULL), n - 2)
  ))
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
    VSSTRESN = deid_vs_weights,
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
  # Weight results as SDTM writes them: each subject's first row in the
  # standard units, its second in pounds.
  vs$VSTESTCD <- "WEIGHT"
  vs$VSORRES <- sprintf("%.1f", unclass(vs$VSSTRESN) * rep(c(1, 2.2), n))
  vs$VSORRESU <- rep(c("kg", "LB"), n)
  vs$VSSTRESC <- sprintf("%.1f", unclass(vs$VSSTRESN))
  vs$VSSTRESU <- "kg"
  # User-pattern terms in attributes that aren't text: SPSS variable labels
  # held in a list (as haven and foreign write them), a factor with an
  # attribute of its own, and the name of an attribute. anon() must redact
  # them.
  attr(vs, "variable.labels") <- list(VSSTRESN = "DRUGQX result")
  attr(vs$VSSTRESN, "assay") <- structure(factor("DRUGQX assay"), lab = "STUDYQX lab")
  attr(vs$VSSTRESN, "DRUGQX_lot") <- "L1"
  # A call held in an attribute named with a user-pattern term: anon() can't
  # redact the call, and must name the attribute redacted in its warning.
  attr(vs$VSSTRESN, "DRUGQX_call") <- quote(f(x))
  # An environment held in an attribute named with a user-pattern term, which
  # holds a function: anon() copies the environment and names the function
  # by a path built from the redacted attribute name.
  held <- new.env(parent = emptyenv())
  # (Made with as.function(), so that it has no source reference holding
  # this file's text, which the steps keep.)
  held$f <- as.function(alist(1), envir = globalenv())
  attr(vs$VSSTRESN, "DRUGQX_env") <- held
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
  # Keys and SUBJIDs in comma-separated lists (numbers in the numeric
  # fixture): each is found.
  attr(relsub, "label") <- paste(
    "Pairs", paste(deid_keys[1:2], collapse = ","), "and", paste(deid_subjids[1:2], collapse = ","),
    # Lists after a number that isn't an id: each id is still found.
    "; excluded", paste(c("9999", deid_keys[3:4]), collapse = ","),
    "; failures", paste(c("10500", deid_subjids[3:4]), collapse = ",")
  )

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
  # A column named with a SUBJID, and columns named with a key made unique
  # by make.unique() ("<key>.1").
  ta[[deid_subjids[5]]] <- c(1, 2)
  ta[make.unique(rep(deid_keys[6], 2))] <- list(1:2, 3:4)
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
  # A vector named by subject and position, as unlist(split()) makes it: each
  # key followed by digits ("<key>1"), the last key's names starting with the
  # first key.
  vsx <- tibble::tibble(V = unlist(split(vs$VSSTRESN, as.character(vs$USUBJID))))
  # A data frame with a column per subject, named by make.names() as
  # data.frame() names them ("ZQ.7701.0001").
  wide <- data.frame(split(vs$VSSTRESN, as.character(vs$USUBJID)))

  # Vital signs nested by subject, as a vctrs list_of (as tidyr::nest()
  # makes it), with a packed column inside: each element belongs to its row's
  # subject.
  vs_rows <- function(i) {
    dates <- deid_start[i] + c(0, 7)
    tibble::tibble(
      VSDTC = format(dates), VSDY = deid_study_day(dates, deid_start[i]),
      P = tibble::tibble(VSTM = as.POSIXct(paste(format(dates), "08:30"), tz = "America/New_York"))
    )
  }
  vsn <- tibble::tibble(USUBJID = deid_keys)
  vs_list <- lapply(seq_len(n), function(i) {
    out <- vs_rows(i)
    attr(out$VSDTC, "label") <- "Vital Signs Date/Time"
    out
  })
  # Its prototype keeps the columns' labels, as dplyr::group_nest() makes it
  # (tidyr::unnest() gives them to the unnested columns).
  vsn$data <- vctrs::new_list_of(vs_list, ptype = vctrs::vec_ptype(vs_list[[1]]))

  # A dataset whose key is only in a packed column, with its rows in an order
  # the keys don't follow: each row has the packed key's subject.
  pk <- tibble::tibble(PKDTC = format(deid_start[c(3, 1, 2, 8:4)] + 1))
  pk$ID <- tibble::tibble(USUBJID = deid_keys[c(3, 1, 2, 8:4)])
  # A dataset of data frames split by subject (each holding one subject), in
  # an order the keys don't follow: each row has its data frame's subject.
  spl <- tibble::tibble(SPDTC = format(deid_start[c(5, 2, 7)] + 2))
  # They are plain data frames with text row names naming the rows they were
  # split from, which the IDs step resets as it sorts them.
  spl$data <- lapply(c(5, 2, 7), function(i) {
    data.frame(USUBJID = deid_keys[i], SPDT = deid_start[i] + c(3, 4), row.names = paste0("obs", 2 * i + 0:1))
  })

  # Labs nested by test, in a dataset without the key: the nested data frames
  # hold the key, rows of several subjects in an order the keys don't follow,
  # a row with a blank key (holding the first subject's dates), and, in each
  # row, a data frame of its own.
  lab_rows <- function(ks, day) {
    start <- deid_start[match(ks, deid_keys)]
    start[ks == ""] <- deid_start[1]
    out <- tibble::tibble(
      USUBJID = ks, LBDTC = paste0(format(start + day), "T09:00"), LBDY = deid_study_day(start + day, start)
    )
    out$REPS <- lapply(start, function(s) tibble::tibble(RPDT = s + day + 1))
    out
  }
  lbn <- tibble::tibble(LBTESTCD = c("ALB", "GLUC"))
  lbn$data <- list(lab_rows(deid_keys[c(3, 1, 2)], 2), lab_rows(c(deid_keys[4:8], ""), 3))
  # A row with a blank date in the first data frame: the date removed from the
  # second, of the same shape, takes that data frame's own missing value (NA).
  lbn$data[[1]] <- vctrs::vec_rbind(
    lbn$data[[1]],
    tibble::tibble(USUBJID = deid_keys[4], LBDTC = "", LBDY = NA_integer_, REPS = list(NULL))
  )

  # A per-test table of the extreme values found, with the run date of the
  # analysis: one test has a subject's row, one none (no row), one no data
  # frame.
  qc <- tibble::tibble(LBTESTCD = c("ALB", "GLUC", "HGB"), RUNDT = as.Date("2150-05-01") + 0:2)
  qc$outliers <- list(
    tibble::tibble(USUBJID = deid_keys[2], LBSTRESN = 99),
    tibble::tibble(USUBJID = character(0), LBSTRESN = numeric(0)),
    NULL
  )
  # A list_of whose data frames hold a factor naming subjects, as data (not
  # the key): its prototype must match the elements, whose levels aren't
  # rewritten.
  lof <- tibble::tibble(USUBJID = deid_keys[1:2])
  lof_rows <- lapply(1:2, function(i) tibble::tibble(V = factor(deid_keys[i])))
  lof$data <- vctrs::new_list_of(lof_rows, ptype = vctrs::vec_ptype(vctrs::vec_rbind(lof_rows[[1]], lof_rows[[2]])))

  # list_of columns whose elements give their prototype nothing to follow (no
  # element, only NULL, only data frames without rows): the prototype's key
  # and date factors hold the keys and a real date as levels, which must go.
  lab_ptype <- function(k) {
    tibble::tibble(USUBJID = factor(k, levels = deid_keys[1:3]), LBDTC = factor(format(deid_start[1] + 2)))
  }
  ptype0 <- vctrs::vec_ptype(lab_ptype(deid_keys[1]))
  lz <- tibble::tibble(ID = character(0))
  lz$data <- vctrs::new_list_of(list(), ptype = ptype0)
  ln <- tibble::tibble(ID = c("a", "b"))
  ln$data <- vctrs::new_list_of(list(NULL, NULL), ptype = ptype0)
  lr <- tibble::tibble(ID = c("a", "b"))
  lr$data <- vctrs::new_list_of(lapply(deid_keys[2:3], function(k) vctrs::vec_slice(lab_ptype(k), 0)), ptype = ptype0)

  # A list_of whose factor key and date have rows: the prototype takes the
  # levels its elements hold, new keys and shifted dates, and drops a level
  # of another factor that no element holds.
  lk_rows <- function(i) {
    tibble::tibble(
      USUBJID = factor(deid_keys[i], levels = deid_keys[1:3]),
      LBDTC = factor(format(deid_start[i] + 3), levels = format(deid_start[1:3] + 3)),
      NOTE = factor("x")
    )
  }
  lk_ptype <- vctrs::vec_ptype(lk_rows(1))
  lk_ptype$NOTE <- factor(character(0), levels = c("x", "y"))
  lk <- tibble::tibble(G = 1:2)
  lk$data <- vctrs::new_list_of(lapply(1:2, lk_rows), ptype = lk_ptype)

  out <- list(
    dm = dm, vs = vs, ex = ex, suppdm = suppdm, relsub = relsub, ad = ad, notes = notes, pr = pr, ta = ta, te = te,
    vsum = vsum, vsx = vsx, wide = wide, tw = tw, vsn = vsn, lbn = lbn, pk = pk, spl = spl, qc = qc, lof = lof,
    lz = lz, ln = ln, lr = lr, lk = lk
  )
  # A key column whose missing-value code is another subject's key (when
  # haven is installed): that value becomes missing, with a warning.
  if (requireNamespace("haven", quietly = TRUE)) {
    sv <- data.frame(SVN = 1:2)
    sv$USUBJID <- haven::labelled_spss(deid_keys[c(1, 6)], na_values = deid_keys[6])
    out$sv <- sv
  }
  out[[deid_keys[4]]] <- named
  out
}

# The fixture for the profile pipelines (anon_deidentify()): the fixture,
# with a birth date (a direct identifier), SEX (a quasi-identifier only
# suppressed) and nested copies of AGE (a packed column, AGEPACK, and data
# frames without a key in a list column, AGELIST) in dm, and, with `adam`, an ADSL and a BDS dataset carrying
# ADSL variables: copies of AGE, SEX and RACE, AGEGR1 and AGEGR1N derived
# from AGE (cut at 55), RACEN from RACE, baseline weight (WEIGHTBL, each
# subject's first vs WEIGHT, but from another visit for subject 1), BMIBL
# and the BMI parameter (derived from height and weight), a birth date, and
# dates (RANDDT, TRTSDT, TRTEDT, ADT) among the fixture's sentinel dates.
deid_profile_sex <- c("F", "F", "M", "M", "F", "M", "F", "F")
deid_profile_fixture <- function(adam = FALSE) {
  data <- deid_fixture()
  n <- length(deid_keys)
  data$dm$BRTHDTC <- format(deid_start - 10)
  data$dm$SEX <- deid_profile_sex
  # Subject 3 is 50, on the lower bound of its band.
  data$dm$AGE[3] <- 50
  data$dm$AGEPACK <- data.frame(AGE = as.numeric(data$dm$AGE))
  data$dm$AGELIST <- I(lapply(as.numeric(data$dm$AGE), function(a) data.frame(AGE = a)))
  if (!adam) {
    return(data)
  }
  age <- data$dm$AGE
  first_weight <- deid_vs_weights[seq(1, 2 * n, by = 2)]
  adsl <- data.frame(
    USUBJID = deid_keys, SUBJID = deid_subjids, AGE = as.numeric(age),
    AGEGR1 = ifelse(age < 55, "<55", ">=55"), AGEGR1N = ifelse(age < 55, 1, 2),
    SEX = deid_profile_sex, RACE = data$dm$RACE, RACEN = ifelse(data$dm$RACE == "ASIAN", 2, 1),
    # Baseline weight from another visit for subject 1.
    WEIGHTBL = replace(first_weight, 1, 76.3), BMIBL = round(first_weight / 1.7^2, 1),
    BRTHDT = deid_start + 7, RANDDT = deid_start, TRTSDT = deid_start, TRTEDT = deid_start + 7,
    stringsAsFactors = FALSE
  )
  attr(adsl$AGEGR1, "label") <- "Pooled Age Group 1"
  rows <- rep(seq_len(n), each = 3)
  adbds <- data.frame(
    USUBJID = deid_keys[rows], AGE = adsl$AGE[rows], AGEGR1 = adsl$AGEGR1[rows], SEX = deid_profile_sex[rows],
    RACE = adsl$RACE[rows], PARAMCD = rep(c("WEIGHT", "WEIGHT", "BMI"), n),
    AVAL = as.vector(rbind(matrix(deid_vs_weights, 2), adsl$BMIBL)),
    BASE = as.vector(rbind(first_weight, first_weight, adsl$BMIBL)),
    ABLFL = rep(c("Y", "", "Y"), n), ADT = deid_start[rows] + rep(c(0, 7, 0), n),
    stringsAsFactors = FALSE
  )
  adbds$CHG <- round(adbds$AVAL - adbds$BASE, 10)
  data$adsl <- adsl
  data$adbds <- adbds
  data
}

# The fixture with numeric keys (see deid_numeric_keys): key columns (USUBJID
# and RSUBJID) at any depth become numbers, and each key in text, attributes
# and names becomes its number, followed by the same digits where it was
# followed by digits (see deid_rewrite_text()). Factor keys become plain
# numbers, losing their levels and contrasts; haven-labelled keys stay
# labelled. SUBJIDs become numbers of 5 digits, as text
# (deid_numeric_subjids).
deid_numeric_fixture <- function() {
  deid_to_numeric(deid_fixture())
}

deid_numeric_text <- function(x) {
  map <- list(new_key = c(
    stats::setNames(as.character(deid_numeric_keys), names(deid_numeric_keys)),
    deid_numeric_subjids
  ))
  deid_rewrite_text(x, map, names = TRUE, dates = FALSE)
}

# `x` with keys made numeric, at any depth through lists, data frames and
# attributes; `id` is TRUE for a key column. S4 objects, environments and
# functions are left as they are.
deid_to_numeric <- function(x, id = FALSE) {
  if (is.null(x) || isS4(x) || is.environment(x) || is.function(x)) {
    return(x)
  }
  if (id && is.atomic(x)) {
    return(deid_numeric_id(x))
  }
  a <- attributes(x)
  if (is.list(x)) {
    ids <- if (is.data.frame(x)) names(x) %in% c("USUBJID", "RSUBJID") else rep(FALSE, length(unclass(x)))
    v <- unclass(x)
    attributes(v) <- NULL
    for (i in seq_along(v)) {
      if (!is.null(v[[i]])) v[[i]] <- deid_to_numeric(v[[i]], id = ids[i])
    }
  } else {
    v <- x
    attributes(v) <- NULL
    if (is.character(v)) v <- deid_numeric_text(v)
  }
  attributes(v) <- deid_numeric_attributes(a)
  v
}

deid_numeric_attributes <- function(a) {
  for (name in names(a)) {
    value <- a[[name]]
    a[[name]] <- if (name %in% c("names", "levels") || (name == "row.names" && is.character(value))) {
      deid_numeric_text(value)
    } else if (name == "dimnames") {
      lapply(value, function(d) if (!is.null(d)) deid_numeric_text(d))
    } else if (name %in% c("class", "row.names")) {
      value
    } else {
      deid_to_numeric(value)
    }
  }
  a
}

# A key column as numbers (blank keys missing), keeping its other attributes.
deid_numeric_id <- function(x) {
  a <- attributes(x)
  text <- as.character(if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else x)
  stopifnot(all(is.na(text) | text == "" | text %in% names(deid_numeric_keys)))
  v <- unname(deid_numeric_keys[text])
  if (inherits(x, "haven_labelled_spss")) {
    return(haven::labelled_spss(v, na_values = unname(deid_numeric_keys[a$na_values]), label = a$label))
  }
  attributes(v) <- deid_numeric_attributes(a[setdiff(names(a), c("levels", "class", "contrasts", "names"))])
  v
}

# A key column with haven user-defined missing values made missing, as the
# steps do in identifier columns; other columns as they are.
deid_zap_codes <- function(x) {
  if (!inherits(x, "haven_labelled_spss")) {
    return(x)
  }
  v <- vctrs::vec_data(x)
  v[is.na(x)] <- NA
  v
}

# Text that describes the data (attributes and names) as the pipelines should
# leave it. Ids are found by deid_scan_ids(), kept apart from the package's
# regular expressions: each original key becomes its subject's pseudonym, and
# each SUBJID (`oracle$new_also`) the pseudonym's number; keys holding only
# digits, and SUBJIDs, only from 5 characters (a SUBJID equal to a key is
# left to the key). In `names`, an id as make.names() writes it ("X77010001",
# "ZQ.7701.0001") becomes its replacement as make.names() writes it. With
# `dates`, each ISO 8601 full date becomes "[DATE]".
deid_rewrite_text <- function(x, oracle, names = FALSE, dates = TRUE) {
  # Text is searched in UTF-8 (text marked latin1 converted), with the bytes
  # of text that isn't valid UTF-8 written as "<92>"; that converted text is
  # kept only where something was rewritten.
  repaired <- enc2utf8(x)
  invalid <- !is.na(repaired) & !validUTF8(repaired)
  repaired[invalid] <- iconv(repaired[invalid], "UTF-8", "UTF-8", sub = "byte")
  keys <- names(oracle$new_key)
  keys <- keys[grepl("[^0-9]", keys) | nchar(keys) >= 5]
  also <- names(oracle$new_also)
  also <- also[nchar(also) >= 5 & !also %in% keys]
  ids <- c(oracle$new_key[keys], oracle$new_also[also])
  if (names) {
    variants <- stats::setNames(make.names(unname(ids)), make.names(names(ids)))
    ids <- c(ids, variants[!names(variants) %in% names(ids)])
  }
  new <- repaired
  for (i in which(!is.na(repaired))) {
    new[i] <- deid_scan_ids(repaired[i], ids, names, related = also)
  }
  if (dates) {
    new <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}", "[DATE]", new)
  }
  keep <- !is.na(new) & new == repaired
  new[keep] <- x[keep]
  new
}

# `text` with each id (the names of `ids`) replaced by its value. The text is
# read from left to right for places where an id fits: where the character
# before it, if any, is not a letter or digit, and the character after it, if
# any, is not one either (so "," and "." separate ids, and a number is found
# only whole), or, in `names`, where digits follow it up to the end of the
# text (kept after the replacement: "<key>1", as unlist() names elements). At
# each place the longest id that fits is taken, and reading goes on after it.
# An id made only of digits may be part of a number:
# - with "<digit>." just before it, it is replaced only if the id just before
#   the "." is (after "<digit>," it may be a list or a decimal comma, and is
#   replaced);
# - if it is a SUBJID (`related`) with ".<digit>" or ",<digit>" just after
#   it, it is replaced only if the id just after the separator is, or (in
#   `names`) if "." and digits follow it up to the end.
# The ids replaced are the most that keep both rules.
deid_scan_ids <- function(text, ids, names = FALSE, related = character(0)) {
  present <- names(ids)[vapply(names(ids), function(id) grepl(id, text, fixed = TRUE), logical(1))]
  if (length(present) == 0) {
    return(text)
  }
  present <- present[order(-nchar(present))]
  chars <- strsplit(text, "")[[1]]
  n <- length(chars)
  word <- grepl("^[[:alnum:]]$", chars)
  digit <- grepl("^[0-9]$", chars)
  sep <- chars %in% c(".", ",")
  # The places where an id fits: start, end of the id, end with the digits
  # that follow it, and the id.
  found <- list()
  i <- 1
  while (i <= n) {
    fit <- NULL
    if (i == 1 || !word[i - 1]) {
      for (id in present) {
        end <- i + nchar(id) - 1
        if (end > n || paste(chars[i:end], collapse = "") != id) next
        if (end == n || !word[end + 1]) {
          fit <- list(start = i, end = end, last = end, id = id)
        } else if (names && all(digit[(end + 1):n])) {
          fit <- list(start = i, end = end, last = n, id = id)
        }
        if (!is.null(fit)) break
      }
    }
    if (is.null(fit)) {
      i <- i + 1
    } else {
      found <- c(found, list(fit))
      i <- fit$last + 1
    }
  }
  if (length(found) == 0) {
    return(text)
  }
  starts <- vapply(found, `[[`, numeric(1), "start")
  lasts <- vapply(found, `[[`, numeric(1), "last")
  replaced <- rep(TRUE, length(found))
  repeat {
    changed <- FALSE
    for (k in seq_along(found)) {
      f <- found[[k]]
      if (!replaced[k] || !grepl("^[0-9]+$", f$id)) next
      bad <- FALSE
      if (f$start > 2 && chars[f$start - 1] == "." && digit[f$start - 2]) {
        before <- which(lasts == f$start - 2)
        bad <- length(before) == 0 || !replaced[before]
      }
      tail_free <- f$last == f$end
      if (!bad && f$id %in% related && tail_free && f$end + 2 <= n && sep[f$end + 1] && digit[f$end + 2]) {
        rest <- paste(chars[(f$end + 1):n], collapse = "")
        after <- which(starts == f$end + 2)
        bad <- (length(after) == 0 || !replaced[after]) && !(names && grepl("^[.][0-9]+$", rest))
      }
      if (bad) {
        replaced[k] <- FALSE
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  out <- chars
  for (k in rev(which(replaced))) {
    f <- found[[k]]
    tail <- if (f$last > f$end) chars[(f$end + 1):f$last] else character(0)
    out <- c(out[seq_len(f$start - 1)], ids[[f$id]], tail, out[-seq_len(f$last)])
  }
  paste(out, collapse = "")
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
# deid_rewrite_attributes()), and inside nested data the same for packed data
# frame columns and, in list columns, for what their elements hold (see
# deid_rewrite_list()). With `values` (a data frame held in an attribute), its
# text values are rewritten too.
deid_rewrite_frame <- function(df, oracle, values = FALSE) {
  cls <- oldClass(df)
  x <- unclass(df)
  for (j in seq_along(x)) {
    if (values) {
      x[[j]] <- deid_rewrite_value(x[[j]], oracle)
    } else if (is.data.frame(x[[j]])) {
      x[[j]] <- deid_rewrite_frame(x[[j]], oracle)
    } else if (is.list(x[[j]]) && !isS4(x[[j]])) {
      x[[j]] <- deid_rewrite_list(x[[j]], oracle)
    } else {
      x[[j]] <- deid_rewrite_attributes(x[[j]], oracle)
    }
  }
  deid_rewrite_frame_text(x, df, cls, oracle)
}

# The names, character row names and attributes of a data frame (`x`, its
# unclassed copy holding the new columns) rewritten, its class put back.
deid_rewrite_frame_text <- function(x, df, cls, oracle) {
  attr(x, "names") <- deid_rewrite_text(names(x), oracle, names = TRUE)
  row_names <- attr(x, "row.names")
  if (is.character(row_names)) {
    attr(x, "row.names") <- deid_rewrite_text(row_names, oracle, names = TRUE)
  }
  x <- deid_rewrite_attributes(x, oracle, frame = TRUE, structural = deid_structural(df))
  class(x) <- cls
  x
}

# Lists the steps walk into (kept apart from the package's own test, covering
# the same cases): unclassed lists, or lists marked only as AsIs or vctrs
# lists. Other objects in list columns are not entered.
deid_plain_list <- function(x) {
  is.list(x) && !isS4(x) && !is.data.frame(x) &&
    all(oldClass(x) %in% c("AsIs", "list", "vctrs_list_of", "vctrs_vctr"))
}

# A list column with the text that describes what it holds rewritten: its own
# attributes, and at any depth through plain lists the attributes and names of
# vectors and lists and the text of data frames (deid_rewrite_frame()). Text
# values are data and are left as they are.
deid_rewrite_list <- function(l, oracle) {
  if (!deid_plain_list(l)) {
    return(l)
  }
  cls <- oldClass(l)
  x <- unclass(l)
  for (i in seq_along(x)) {
    e <- x[[i]]
    if (is.null(e)) next
    x[[i]] <- if (is.data.frame(e)) {
      deid_rewrite_frame(e, oracle)
    } else if (is.atomic(e) && !isS4(e)) {
      deid_rewrite_attributes(e, oracle)
    } else {
      deid_rewrite_list(e, oracle)
    }
  }
  class(x) <- cls
  deid_rewrite_attributes(x, oracle)
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
    names(x) <- deid_rewrite_text(names(x), oracle, names = TRUE)
  }
  if (!is.null(dimnames(x))) {
    dimnames(x) <- lapply(dimnames(x), function(d) if (!is.null(d)) deid_rewrite_text(d, oracle, names = TRUE))
  }
  x
}

# A value held in an attribute: its text and its attributes, at any depth.
# Date and date-time values become "[DATE]".
deid_rewrite_value <- function(v, oracle) {
  # Environments are left as they are, as the steps leave them.
  if (is.environment(v)) {
    return(v)
  }
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
deid_known_gaps <- list()

# Strings that must not appear in anything that leaves the package: keys and
# related IDs (also without separators, and the numeric keys of
# deid_numeric_fixture()), every planted real full date (also as
# 06MAR2031 and 20310306), the numbers behind real Date and POSIXct values,
# and the pipelines' seeds. Matching ignores case; numeric sentinels match
# only as whole numbers. Partial and year-only dates are checked against the
# secrets instead, because a shifted full date may fall in the same month or
# year. With `patterns = TRUE`, the user-pattern terms too.
deid_sentinels <- function(patterns = FALSE) {
  days <- c(-14, -10, -3, -1, 0, 1, 2, 3, 4, 5, 7)
  dates <- rep(deid_all_starts, each = length(days)) + days
  # Including the unused factor level planted in vs$USUBJID and the SUBJID of
  # the row of `pr` without a key.
  ids <- c(deid_all_keys, "ZQ-7701-0009", deid_subjids, "ZQ0099", as.character(deid_numeric_keys), deid_numeric_subjids)
  # As make.names() writes them (data.frame() column names).
  ids <- c(ids, make.names(ids))
  c(
    unique(c(ids, gsub("[^[:alnum:]]", "", ids))),
    # The unused factor level planted in dm$rficdtc.
    "2081-01-01",
    format(dates),
    toupper(deid_format_c(dates, "%d%b%Y")),
    format(dates, "%Y%m%d"),
    as.character(as.numeric(c(deid_all_starts, deid_start + 7))),
    as.character(as.numeric(c(deid_vs_times("UTC"), deid_vs_times("America/New_York")))),
    as.character(deid_seeds),
    if (patterns) unlist(deid_patterns, use.names = FALSE)
  )
}

# The pipelines that generalize and suppress: AGE in bands of 10 from 0
# (60 and over become 60), races held by fewer than 2 subjects folded into
# "OTHER", and every WEIGHT row rounded to tens, with its text results.
deid_generalize_rules <- function() {
  list(
    dm = list(AGE = anon_band(10, top = 60), RACE = anon_fold(2, "OTHER")),
    anon_rule(anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN"), anon_round(-1))
  )
}
# What they measure, and the target they must meet. After generalization the
# subjects (AGE, RACE, first WEIGHT) are: 1-2 (30, WHITE, 70) and 3-4 (50,
# WHITE, 70), pairs; 5 (40, OTHER, 60) and 6 (40, WHITE, 60), which differ
# only by RACE; 7 (60, WHITE, 80) and 8 (60, WHITE, 90), only by WEIGHT.
# Each of 5-8 has exactly one record that a single cell makes consistent
# with it, the cheapest by `order`: the other of its pair. Suppressing cells
# blanks RACE of 5 and 6 and WEIGHT of 7 and 8 (every WEIGHT row) whatever
# order they are handled in (which follows the pseudonyms); dropping removes
# 5 to 8.
deid_risk_quasi <- list(dm = c("AGE", "RACE"), anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN"))
deid_risk_max <- 1 / 2

# Expected values of the generalized columns, with the quasi-identifiers in
# `blank` (by subject, as positions in deid_keys) blanked: "" for text, as
# none of these columns uses NA for missing text, NA for numbers.
deid_generalized_expected <- function(blank = list()) {
  blanked <- function(keys, what) keys %in% deid_keys[blank[[what]]]
  weight <- function(generalized, missing) {
    function(x, keys, oracle) {
      out <- generalized(x)
      out[blanked(keys, "WEIGHT")] <- missing
      out
    }
  }
  # VSSTRESN rounded to tens; VSORRESU is kg on each subject's first row, LB
  # on its second.
  rounded <- round(deid_vs_weights, -1)
  pounds <- rep(c(FALSE, TRUE), length(deid_vs_weights) / 2)
  list(
    dm.AGE = function(x, keys, oracle) {
      out <- ifelse(x >= 60, 60, floor(unclass(x) / 10) * 10)
      out[blanked(keys, "AGE")] <- NA
      out
    },
    dm.RACE = function(x, keys, oracle) {
      out <- ifelse(x %in% "ASIAN", "OTHER", x)
      out[blanked(keys, "RACE")] <- ""
      out
    },
    vs.VSSTRESN = weight(function(x) rounded, NA),
    vs.VSSTRESC = weight(function(x) sprintf("%.1f", rounded), ""),
    # In kg as VSSTRESN; in pounds, by each row's own factor (original
    # VSORRES / VSSTRESN), at one decimal.
    vs.VSORRES = weight(function(x) {
      factor <- ifelse(pounds, as.numeric(x) / deid_vs_weights, 1)
      sprintf("%.1f", round(rounded * factor, 1))
    }, "")
  )
}

# The true (generalized, not blanked) values of the quasi-identifiers the
# generalize pipelines measure, by subject: AGE and RACE of dm, and each
# subject's first WEIGHT in vs.
deid_release_truth <- function(original, oracle) {
  expected <- deid_generalized_expected()
  data.frame(
    key = deid_keys,
    AGE = expected$dm.AGE(original$dm$AGE, deid_keys, oracle),
    RACE = expected$dm.RACE(original$dm$RACE, deid_keys, oracle),
    WEIGHT = round(deid_vs_weights[seq(1, length(deid_vs_weights), by = 2)], -1)
  )
}

# k-anonymity of the released data for the pipelines that generalize and
# suppress, written apart from the package: for each subject left in dm,
# the number of released subjects whose quasi-identifiers (the columns of
# `truth`: those of dm, and each subject's first WEIGHT in vs) form the same
# pattern, a cell blanked (missing where the true, generalized value isn't)
# counting as "<blank>" and a real missing value as "<NA>".
deid_release_k <- function(original, step, oracle, truth = deid_release_truth) {
  truth <- truth(original, oracle)
  quasi <- setdiff(names(truth), "key")
  out <- step$data
  back <- function(df) names(oracle$new_key)[match(as.character(df$USUBJID), oracle$new_key)]
  vs_keys <- back(out$vs)
  first <- !duplicated(vs_keys) & out$vs$VSTESTCD == "WEIGHT"
  released <- data.frame(key = back(out$dm))
  for (q in setdiff(quasi, "WEIGHT")) {
    # "ds.col" for a column of another dataset, by subject.
    ds <- if (grepl(".", q, fixed = TRUE)) sub("\\..*$", "", q) else "dm"
    col <- sub("^.*\\.", "", q)
    x <- out[[ds]][[col]][match(released$key, back(out[[ds]]))]
    released[[q]] <- if (is.numeric(truth[[q]])) as.numeric(x) else as.character(x)
  }
  released$WEIGHT <- as.numeric(out$vs$VSSTRESN[first][match(released$key, vs_keys[first])])
  truth <- truth[match(released$key, truth$key), ]
  missing <- function(x) is.na(x) | (is.character(x) & !is.na(x) & x == "")
  pattern <- vapply(quasi, function(q) {
    rel <- released[[q]]
    ifelse(missing(rel) & !missing(truth[[q]]), "<blank>", ifelse(missing(rel), "<NA>", as.character(rel)))
  }, character(nrow(released)))
  key <- apply(matrix(pattern, nrow(released)), 1, paste, collapse = "\r")
  as.vector(table(key)[key])
}

# The subjects a data frame's rows have of their own (kept apart from the
# package's own rule, covering the same cases): from its key column; from a
# packed column that has its own (each packed row belongs to its outer row);
# or from a list column whose data frames hold one subject per row. Returns
# the keys (NA: none) and where they came from, or NULL.
deid_frame_owners <- function(df, key) {
  if (key %in% names(df)) {
    k <- as.character(deid_zap_codes(df[[key]]))
    # Blank keys, also of spaces only, are no key.
    k[!is.na(k) & trimws(k) == ""] <- NA
    return(list(keys = k, from = "own"))
  }
  for (v in unclass(df)) {
    if (is.data.frame(v)) {
      o <- deid_frame_owners(v, key)
      if (!is.null(o) && o$from != "list") return(list(keys = o$keys, from = "packed"))
    }
  }
  # The subjects of all data frames a list element holds, at any depth
  # through plain lists.
  held <- function(e) {
    if (is.data.frame(e)) {
      return(deid_frame_owners(e, key)$keys)
    }
    if (deid_plain_list(e)) {
      return(unlist(lapply(e, held), use.names = FALSE))
    }
    NULL
  }
  for (v in unclass(df)) {
    if (!is.data.frame(v) && deid_plain_list(v)) {
      per_row <- lapply(v, function(e) unique(stats::na.omit(held(e))))
      # Every row must hold exactly one subject.
      if (length(per_row) > 0 && all(lengths(per_row) == 1)) {
        return(list(keys = unlist(per_row), from = "list"))
      }
    }
  }
  NULL
}

# keep_rows for pipelines that drop subjects (positions in deid_keys): rows
# of the dropped subjects go, whether their subject comes from the key
# column, a packed column with it, or a list column of one subject per row
# (see deid_frame_owners()), and rows referring to them in RSUBJID.
deid_keep_rows_without <- function(dropped) {
  function(ds, original, oracle) {
    keep <- rep(TRUE, nrow(original))
    owners <- deid_frame_owners(original, "USUBJID")
    if (!is.null(owners)) {
      keep <- keep & !owners$keys %in% deid_keys[dropped]
    }
    if ("RSUBJID" %in% names(original)) {
      keep <- keep & !as.character(original$RSUBJID) %in% deid_keys[dropped]
    }
    keep
  }
}

# The profile pipelines (anon_deidentify() on deid_profile_fixture(), with
# min_n = 2 and max_risk = 1/2) measure AGE, SEX and RACE of dm and each
# subject's first (baseline) WEIGHT; with the ADaM datasets, WEIGHTBL too,
# which shows subject 1 another value than vs. Climbing the ladders, AGE
# goes to bands of 10 and WEIGHT (with WEIGHTBL) is rounded to tens (each
# step tried after these needs more cells or no fewer, and is undone), so
# the subjects are: 1 (30, F, WHITE, 70; WEIGHTBL 80) and 2 (30, F, WHITE,
# 70; 70); 3-4 (50, M, WHITE, 70; 70), a pair; 5 (40, F, ASIAN, 60; 60) and
# 6 (40, M, WHITE, 60; 60), which differ by SEX and RACE; 7 (60, F, WHITE,
# 80; 80) and 8 (60, F, WHITE, 90; 90), by WEIGHT. Suppression blanks SEX
# and RACE of 5 and 6, and WEIGHT of 7 and 8 in every copy (every row of
# vs and the WEIGHT rows of the BDS dataset); with WEIGHTBL, it blanks
# WEIGHTBL of 1, 2, 7 and 8, and (not the fewest cells, as PR 3's heuristic
# allows) RACE of 7 and 8.
deid_profile_quasi <- list(dm = c("AGE", "SEX", "RACE"), anon_quasi_values("vs", "VSTESTCD", "WEIGHT", "VSSTRESN"))
deid_profile_run <- function(profile) {
  # The fixture's dm.SITE (two sites of four subjects each) is kept
  # knowingly; a column that looks like a site without a rule fails the target.
  profile <- c(profile, anon_profile(anon_profile_rule("SITE", "keep"), name = "harness"))
  function(data) anon_deidentify(data, profile, max_risk = 1/2, keep_secrets = TRUE, seed = deid_seeds[1])
}
# What the fixture holds that the profiles can't protect, released as it
# is: anon_deidentify()'s final audit names it in warnings, and each kind is
# a reason the target isn't met (the measured risk meets it). `notes` has no
# key and holds an original key in another case and a date (and `qc`, a
# date column of the ADaM profile's `*DT`); dm.SEXF is a sex without a rule;
# vs.usubjid is the key in another case; ad.TRTSDT, numbers of days with a
# SAS format, is a date of the ADaM profile that can't be offset. They are
# matched as patterns, so that the checks on what leaves the package still
# see these columns.
deid_profile_flagged <- function(adam = FALSE) {
  c(deid_flagged, list(
    list(pattern = paste0(
      "^These datasets have no `USUBJID` column[^:]*: notes \\(usubjid, NOTEDTC\\)",
      if (adam) "; qc \\(RUNDT\\)", "\\. They are released as they are"
    )),
    list(pattern = "^These columns look like quasi-identifiers or birth dates but no profile rule matches them: dm\\.SEXF\\. "),
    list(pattern = "^These columns match a profile rule but were left as they are [^:]*: vs\\.usubjid\\.$")
  ), if (adam) list(list(pattern = "^These columns are dates in the profile but were not offset [^:]*: ad\\.TRTSDT\\. ")))
}
deid_profile_not_met <- function(adam = FALSE) {
  c(
    "datasets without the key hold identifiers, quasi-identifiers or dates",
    "columns that look like quasi-identifiers have no profile rule",
    if (adam) "profile date columns were not offset",
    "columns that match a profile rule were left as they are"
  )
}
deid_profile_truth <- function(original, oracle) {
  out <- data.frame(
    key = deid_keys, AGE = floor(original$dm$AGE / 10) * 10, SEX = original$dm$SEX, RACE = original$dm$RACE,
    WEIGHT = round(deid_vs_weights[seq(1, length(deid_vs_weights), by = 2)], -1)
  )
  if (!is.null(original$adsl)) {
    out$adsl.WEIGHTBL <- round(original$adsl$WEIGHTBL[match(deid_keys, original$adsl$USUBJID)], -1)
  }
  out
}

# Expected values of the profile pipelines' columns (see above), by subject
# (positions in deid_keys) for every row: the fixture's own generalized
# columns, and with `adam` those of adsl and the BDS dataset. Blanks are ""
# for text and NA for numbers and dates.
deid_profile_expected <- function(adam = FALSE) {
  subject <- function(keys) match(keys, deid_keys)
  per_subject <- function(values) function(x, keys, oracle) values[subject(keys)]
  age <- c(30, 30, 50, 50, 40, 40, 60, 60)
  sex <- replace(deid_profile_sex, 5:6, "")
  race <- c("WHITE", "WHITE", "WHITE", "WHITE", "", "", if (adam) c("", "") else c("WHITE", "WHITE"))
  weights <- deid_generalized_expected(list(WEIGHT = 7:8))
  out <- c(
    list(
      dm.AGE = per_subject(age), dm.SEX = per_subject(sex), dm.RACE = per_subject(race),
      dm.BRTHDTC = function(x, keys, oracle) rep("", length(x)),
      # The nested copies of AGE are generalized with it.
      dm.AGEPACK = function(x, keys, oracle) data.frame(AGE = age[subject(keys)]),
      dm.AGELIST = function(x, keys, oracle) I(lapply(age[subject(keys)], function(a) data.frame(AGE = a)))
    ),
    weights[c("vs.VSSTRESN", "vs.VSSTRESC", "vs.VSORRES")]
  )
  if (!adam) {
    return(out)
  }
  # AGEGR1 ("<55" up to 50, ">=55" from 58 in the data) is kept where the
  # band lies within one category: 30-39, 40-49 and 60-69; 50-59 could hold
  # 51 to 57, whose category the data doesn't show, so it is blanked for 3
  # (truly 50, on the band's bound) and 4.
  group <- c("<55", "<55", "", "", "<55", "<55", ">=55", ">=55")
  group_n <- c(1, 1, NA, NA, 1, 1, 2, 2)
  # RACEN follows RACE: blank where RACE is.
  race_n <- c(1, 1, 1, 1, NA, NA, NA, NA)
  bds_weight <- function(x, keys, oracle) {
    s <- subject(keys)
    weight_row <- rep(c(TRUE, TRUE, FALSE), length(deid_keys))
    out <- ifelse(weight_row, round(x, -1), NA)
    out[s %in% 7:8] <- NA
    out
  }
  c(out, list(
    adsl.AGE = per_subject(age), adsl.SEX = per_subject(sex), adsl.RACE = per_subject(race),
    adsl.AGEGR1 = per_subject(group), adsl.AGEGR1N = per_subject(group_n), adsl.RACEN = per_subject(race_n),
    adsl.WEIGHTBL = per_subject(c(NA, NA, 70, 70, 60, 60, NA, NA)),
    # From height and weight, which changed for every subject.
    adsl.BMIBL = per_subject(rep(NA_real_, 8)),
    adsl.BRTHDT = function(x, keys, oracle) x[NA_integer_][seq_along(x)],
    adbds.AGE = per_subject(age), adbds.SEX = per_subject(sex), adbds.RACE = per_subject(race),
    adbds.AGEGR1 = per_subject(group),
    # WEIGHT rows rounded to tens (BASE is the baseline AVAL, CHG recomputed);
    # BMI rows blanked, with their results.
    adbds.AVAL = bds_weight, adbds.BASE = bds_weight,
    adbds.CHG = function(x, keys, oracle) ifelse(rep(c(TRUE, TRUE, FALSE), 8) & subject(keys) <= 6, 0, NA)
  ))
}

# What the profile pipelines must leave blank, keep equal across copies and
# keep consistent (see the "deidentify" test): `removed`, "ds.col" columns
# blank in every row; `copies`, by quasi-identifier, its copies as "ds.col"
# (every row of a subject) or "ds.col[COL=value,...]" (the subject's first
# row matching), which must show each subject the same value; `derived`,
# "ds.col" = "ds.col" of its quasi-identifier in the same rows: wherever
# shown, a derived value must be the row's original one, and rows with the
# same released quasi-identifier value must show the same derived value.
deid_profile_checks <- function(adam = FALSE) {
  out <- list(
    removed = "dm.BRTHDTC",
    copies = list(),
    derived = list()
  )
  if (adam) {
    out$removed <- c(out$removed, "adsl.BRTHDT")
    out$copies <- list(
      AGE = c("dm.AGE", "adsl.AGE", "adbds.AGE"),
      SEX = c("dm.SEX", "adsl.SEX", "adbds.SEX"),
      RACE = c("dm.RACE", "adsl.RACE", "adbds.RACE"),
      # WEIGHTBL shows subject 1 another value: it is measured on its own
      # (see deid_profile_truth()), not a copy that must agree.
      WEIGHT = c("vs.VSSTRESN[VSTESTCD=WEIGHT]", "adbds.AVAL[PARAMCD=WEIGHT,ABLFL=Y]")
    )
    out$derived <- list(
      adsl.AGEGR1 = "adsl.AGE", adsl.AGEGR1N = "adsl.AGE", adsl.RACEN = "adsl.RACE",
      adsl.BMIBL = "adsl.WEIGHTBL", adbds.AGEGR1 = "adbds.AGE"
    )
  }
  out
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
#   core ones ("rewrite" or "drop"), which the expected attributes follow;
# - `fixture`: the function giving the input (deid_fixture(), or its numeric
#   variant), and `keys`: its subject keys as text, in the order of
#   `deid_all_keys`;
# - `risk`: `list(quasi, max_risk)`, a target the input misses and the
#   result must meet, as k-anonymity of the released data (deid_release_k()),
#   and optionally `truth`, the function giving the true generalized values
#   it compares the released ones with (default deid_release_truth());
# - `deidentify`: for anon_deidentify() pipelines, the columns removed, the
#   copies of each quasi-identifier and the derived columns to check (see
#   deid_profile_checks());
# - `not_met`: for anon_deidentify() pipelines, the reasons the target isn't
#   met, from what the fixture holds that the profile can't protect (see
#   deid_profile_flagged());
# - `dropped`: subjects (positions in deid_keys) whose rows the pipeline
#   removes from data frames nested at any depth (see deid_expected_frame()).
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
  ),
  # Numeric keys get numbers as pseudonyms, also in text.
  ids_then_dates_numeric = list(
    run = function(data) {
      anon_subject_dates(
        anon_subject_ids(data, keep_secrets = TRUE, seed = deid_seeds[1]),
        keep_secrets = TRUE, seed = deid_seeds[2]
      )
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365,
    fixture = deid_numeric_fixture, keys = as.character(deid_numeric_keys[deid_all_keys]),
    flagged = lapply(deid_flagged, function(f) {
      if (!is.null(f$column)) f$column <- deid_numeric_text(f$column)
      if (identical(f$id, "s4_attribute")) f$pattern <- sub("SUBJ-[0-9]+", "[0-9]+", f$pattern, fixed = TRUE)
      f
    })
  ),
  # Generalization and suppression of cells after the IDs and dates steps.
  generalize_suppress = list(
    run = function(data) {
      step <- anon_subject_dates(
        anon_subject_ids(data, keep_secrets = TRUE, seed = deid_seeds[1]),
        keep_secrets = TRUE, seed = deid_seeds[2]
      )
      anon_suppress(anon_generalize(step, deid_generalize_rules()), deid_risk_quasi, max_risk = deid_risk_max)
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365,
    expected = deid_generalized_expected(list(RACE = 5:6, WEIGHT = 7:8)),
    risk = list(quasi = deid_risk_quasi, max_risk = deid_risk_max)
  ),
  # The same before the dates and IDs steps, which run on the generalized
  # data.
  generalize_first = list(
    run = function(data) {
      step <- anon_suppress(anon_generalize(data, deid_generalize_rules()), deid_risk_quasi, max_risk = deid_risk_max)
      anon_subject_ids(
        anon_subject_dates(step, keep_secrets = TRUE, seed = deid_seeds[3]),
        keep_secrets = TRUE, seed = deid_seeds[4]
      )
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365,
    expected = deid_generalized_expected(list(RACE = 5:6, WEIGHT = 7:8)),
    risk = list(quasi = deid_risk_quasi, max_risk = deid_risk_max)
  ),
  # Subjects at risk dropped from every dataset, at any depth (rows of data
  # frames nested with the key too; `dropped` names them for the oracle).
  suppress_drop = list(
    run = function(data) {
      step <- anon_subject_dates(
        anon_subject_ids(data, keep_secrets = TRUE, seed = deid_seeds[1]),
        keep_secrets = TRUE, seed = deid_seeds[2]
      )
      step <- anon_generalize(step, deid_generalize_rules())
      anon_suppress(step, deid_risk_quasi, max_risk = deid_risk_max, method = "drop")
    },
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365,
    expected = deid_generalized_expected(),
    keep_rows = deid_keep_rows_without(5:8),
    dropped = 5:8,
    risk = list(quasi = deid_risk_quasi, max_risk = deid_risk_max)
  ),
  # anon_deidentify() with the SDTM profile, the ADaM profile and both
  # combined (see deid_profile_expected()). With the ADaM profile alone,
  # baseline weight is measured from adsl.WEIGHTBL; combined, from vs.
  deidentify_sdtm = list(
    run = deid_profile_run(anon_profile_sdtm(min_n = 2)),
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365,
    fixture = function() deid_profile_fixture(),
    flagged = deid_profile_flagged(), not_met = deid_profile_not_met(),
    expected = deid_profile_expected(),
    risk = list(quasi = deid_profile_quasi, max_risk = 1/2, truth = deid_profile_truth),
    deidentify = deid_profile_checks()
  ),
  deidentify_adam = list(
    run = deid_profile_run(anon_profile_adam(min_n = 2)),
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365,
    fixture = function() deid_profile_fixture(adam = TRUE),
    flagged = deid_profile_flagged(adam = TRUE), not_met = deid_profile_not_met(adam = TRUE),
    expected = deid_profile_expected(adam = TRUE),
    risk = list(quasi = deid_profile_quasi, max_risk = 1/2, truth = deid_profile_truth),
    deidentify = deid_profile_checks(adam = TRUE)
  ),
  deidentify_combined = list(
    run = deid_profile_run(c(anon_profile_sdtm(min_n = 2), anon_profile_adam(min_n = 2))),
    keep_secrets = TRUE, prefix = "SUBJ-", min_days = 30, max_days = 365,
    fixture = function() deid_profile_fixture(adam = TRUE),
    flagged = deid_profile_flagged(adam = TRUE), not_met = deid_profile_not_met(adam = TRUE),
    expected = deid_profile_expected(adam = TRUE),
    risk = list(quasi = deid_profile_quasi, max_risk = 1/2, truth = deid_profile_truth),
    deidentify = deid_profile_checks(adam = TRUE)
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
  other_attributes = "rewrite",
  fixture = deid_fixture,
  keys = deid_all_keys,
  risk = NULL,
  deidentify = NULL,
  not_met = character(0),
  dropped = integer(0)
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
# user-pattern terms must be gone too), `errors` (TRUE, or a function of
# the step, if it is expected to error; its error message is then checked)
# and `gaps` (FALSE to leave it out of the known-gap tests).
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

# An environment holding `step`, its `dm` (under a name holding a
# user-pattern term), a closure whose environment holds the step, one whose
# environment is the environment itself, a function whose source reference
# holds a key, an environment in a list holding a user-pattern term and the
# environment itself, and, for a step with secrets, the secrets read with
# anon_secrets() (also in an attribute of `dm`) and the step's secrets
# environment, redacted with anon(): the copy must be a new environment
# holding the step (still a step, without secrets), the data frame, the
# functions and the list, under the expected names, without the secrets;
# the closure must be on a redacted copy of its environment; saving the copy
# must not save the original environments or the source; and the
# environment given must not change.
# With `closures = "detach"`, the closures must refer to the global
# environment instead, and the environment's parent is an environment
# holding the original keys, which saving the copy must not save (its parent
# is then the global environment). With "copy", the parent is the global
# environment: a parent that isn't is copied, redacted by patterns only.
deid_anon_env <- function(step, closures = "copy") {
  parent <- globalenv()
  if (closures == "detach") {
    parent <- new.env(parent = globalenv())
    parent$original_keys <- deid_all_keys
  }
  e <- new.env(parent = parent)
  assign("step", step, envir = e)
  dm <- step$data$dm
  if (!is.null(step$secrets)) {
    attr(dm, "keys") <- anon_secrets(step)
    assign("keys", anon_secrets(step), envir = e)
    assign("vault", step$secrets, envir = e)
  }
  assign("DRUGQX_dm", dm, envir = e)
  holder <- new.env(parent = globalenv())
  holder$step <- step
  reader <- function() nrow(step$data$dm)
  environment(reader) <- holder
  assign("reader", reader, envir = e)
  own <- function() length(ls())
  environment(own) <- e
  assign("own", own, envir = e)
  assign("sourced", eval(parse(text = paste0("function() 1 # ", deid_keys[1]), keep.source = TRUE), globalenv()), envir = e)
  inner <- new.env(parent = globalenv())
  inner$note <- "DRUGQX note"
  inner$outer <- e
  assign("box", list(inner = inner), envir = e)
  # A function's `...` holding a user-pattern term, as data (the frame) and
  # as a closure's frame: anon() must rebuild it redacted. (The function
  # making them is made in the global environment, so that the frames'
  # parent isn't this function's frame.)
  with_dots <- local(function(...) {
    force(list(...))
    list(frame = environment(), fn = function() c(...))
  }, globalenv())
  made <- with_dots(deid_patterns[[1]])
  assign("dots_frame", made$frame, envir = e)
  assign("dots_fn", made$fn, envir = e)
  # An empty `...`, and one holding a call that would save the frame making
  # the copy if it were run.
  assign("dots_empty", with_dots()$frame, envir = e)
  assign("dots_call", with_dots(quote(environment()))$frame, envir = e)
  # The objects, saved (`own` and `box` are left out: saving them saves `e`
  # itself).
  snapshot <- function() serialize(mget(setdiff(ls(e, all.names = TRUE), c("own", "box")), envir = e), NULL)
  before <- snapshot()
  a <- anon(e, pattern_list = deid_patterns, closures = closures)
  if (!is.environment(a) || identical(a, e)) stop("anon() didn't return a new environment")
  if (!is.null(attributes(e)) || !identical(snapshot(), before) || !identical(environment(e$own), e) ||
    !identical(inner$note, "DRUGQX note") || !identical(inner$outer, e)) {
    stop("anon() changed the environment it was given")
  }
  if (!identical(sort(ls(a, all.names = TRUE)), sort(c("[DRUG]_dm", "box", "dots_call", "dots_empty", "dots_fn", "dots_frame", "own", "reader", "sourced", "step")))) {
    stop("anon() returned an environment holding other objects than expected")
  }
  if (!inherits(a$step, "anon_deid_step")) stop("anon() didn't keep the step a step")
  reader_env <- environment(a$reader)
  if (closures == "detach") {
    if (!identical(reader_env, globalenv()) || !identical(environment(a$own), globalenv())) {
      stop("anon(closures = \"detach\") didn't give the closures the global environment")
    }
  } else if (!identical(environment(a$own), a) || identical(reader_env, holder) ||
    !is.null(unclass(reader_env$step)$secrets)) {
    stop("anon() didn't replace the closures' environments with redacted copies")
  }
  # Column rules reach a data frame held in a closure's `...`.
  ruled <- anon(list(fn = with_dots(step$data$dm)$fn), df_variable_names = "USUBJID", closures = closures)
  if (closures == "copy" && !all(ruled$fn()$USUBJID == "[REDACTED]")) {
    stop("anon() didn't apply the column rules inside `...`")
  }
  if (!identical(eval(as.call(list(base::list, quote(...))), a$dots_call), list(quote(environment())))) {
    stop("anon() ran a call held in `...`")
  }
  if (closures == "copy" && !identical(a$dots_fn(), names(deid_patterns)[1])) {
    stop("anon() didn't rebuild a closure's `...` redacted")
  }
  if (identical(a$box$inner, inner) || !identical(a$box$inner$outer, a)) {
    stop("anon() didn't copy the nested environment, mapping its reference to the copy")
  }
  copies <- lapply(mget(sort(ls(a, all.names = TRUE)), envir = a), unclass)
  if (!is.null(copies$step$secrets)) stop("anon() kept the step's secrets")
  # The text, then the saved copy as bytes (as the other serialized exits).
  text <- c(ls(a, all.names = TRUE), capture.output(print(a)), deid_dput(copies))
  c(charToRaw(paste(text, collapse = "\n")), as.raw(0x0a), serialize(a, NULL))
}

# An environment whose object names and step dataset names hold user-pattern
# terms: the step under such a name, one of its datasets renamed with such a
# name (its datasets are then named like "STUDYQX_step$dm" and
# "STUDYQX_step$DRUGQX_vs"), and a data frame.
deid_env_named <- function(step) {
  names(step$data)[names(step$data) == "vs"] <- "DRUGQX_vs"
  e <- new.env()
  assign("STUDYQX_step", step, envir = e)
  assign("DRUGQX_dm", step$data$dm, envir = e)
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
  anon_env = list(fn = deid_anon_env, patterns = TRUE),
  anon_env_detach = list(fn = function(step) deid_anon_env(step, closures = "detach"), patterns = TRUE, gaps = FALSE),
  # Reports, prompt bundles and what the app shows, on objects and step
  # datasets named with user-pattern terms. Reports and the app are given the
  # real names to select, and must select every object. Not run for the known
  # gaps, which it doesn't concern.
  report_names = list(
    fn = function(step) {
      e <- deid_env_named(step)
      n_objects <- length(step$data) + 1
      out <- character(0)
      for (rows in list(3, anon_example_rows(n = 3, key = "USUBJID"))) {
        report <- anon_report(
          e, selection = c("STUDYQX_step", "DRUGQX_dm"), pattern_list = deid_patterns,
          example_values_n = 5, example_rows = rows
        )
        if (length(report$object_names) != n_objects) stop("anon_report() didn't select every object by its real name")
        bundles <- vapply(c("markdown", "text", "json"), function(f) {
          as.character(anon_prompt_bundle(report = report, format = f))
        }, character(1))
        out <- c(out, capture.output(print(report)), as_anon_json(report), bundles, deid_dput(unclass(report)))
      }
      state <- anon_app_initial_state(e, pattern_list = deid_patterns, initial_selection = c("STUDYQX_step", "DRUGQX_dm"))
      if (length(state$selected) != n_objects ||
        !setequal(app_selected_names(state, state$selected), c(paste0("STUDYQX_step$", names(e$STUDYQX_step$data)), "DRUGQX_dm"))) {
        stop("The app didn't select every object by its real name")
      }
      c(
        out,
        capture.output(print(anon_inventory(e, pattern_list = deid_patterns))),
        deid_dput(state[c("inventory", "choices", "selected")])
      )
    },
    patterns = TRUE,
    gaps = FALSE
  ),
  risk = list(
    fn = function(step) {
      risk <- anon_risk(step, quasi = list(dm = "AGE"))
      c(capture.output(print(risk)), as_anon_json(risk), deid_dput(anon_risk_subjects(step, quasi = list(dm = "AGE"))))
    }
  )
)

# Names of the exits expected to error on a step.
deid_expected_errors <- function(step, exits = deid_exits) {
  expected <- vapply(exits, function(e) {
    isTRUE(if (is.function(e$errors)) e$errors(step) else e$errors)
  }, logical(1))
  names(exits)[expected]
}

# The exits run for the known gaps: all but those marked `gaps = FALSE`.
deid_gap_exits <- function() Filter(function(e) !isFALSE(e$gaps), deid_exits)

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
deid_run_exits <- function(step, exits = deid_exits) {
  lapply(exits, function(exit) {
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
# of digits must not be part of a longer number: digits around it, or
# "<digit>." before it (a fraction), make it part of one.
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
  dot <- function(i) i >= 1 & i <= length(bytes) & bytes[pmax(i, 1)] == as.raw(0x2e) & digit(i - 1)
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

# From a step's secrets: per original key, the new key, the date offset in
# days and the direction of year-only dates; the original keys of subjects
# the pipeline drops (`dropped`); per fixture SUBJID, the pseudonym's number as the SUBJID column gets
# it (`new_also`); and what the pipeline does with other attributes
# (`other`).
deid_oracle <- function(step, other = "rewrite", dropped = integer(0)) {
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
  # The fixture's SUBJIDs belong to deid_keys, or are numbers of their own
  # in the numeric fixture.
  numeric <- !all(deid_keys %in% names(new_key))
  own <- if (numeric) as.character(deid_numeric_keys[deid_keys]) else deid_keys
  numbers <- as.integer(sub(step$settings$subject_ids$prefix, "", new_key[own], fixed = TRUE))
  subjids <- if (numeric) unname(deid_numeric_subjids[deid_subjids]) else deid_subjids
  new_also <- stats::setNames(formatC(numbers, width = nchar(length(new_key)), flag = "0"), subjids)
  list(
    new_key = new_key, new_also = new_also, offset = stats::setNames(offsets$offset_days, offset_key),
    year_direction = stats::setNames(offsets$year_direction, offset_key), other = other,
    dropped = deid_keys[dropped]
  )
}
