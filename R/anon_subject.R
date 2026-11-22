#' Pseudonymize subject identifiers consistently across datasets
#'
#' Replaces each subject's key (for example `USUBJID`) with a random pseudonym
#' that is the same in every dataset, so the datasets can still be joined.
#' Related identifier columns listed in `also` (for example `SUBJID`) are
#' replaced with the pseudonym's number so they stay aligned with the key, and
#' columns listed in `key_cols` that hold other subjects' keys (for example
#' `RSUBJID`) are mapped through the same pseudonyms.
#'
#' @param data A data frame, a named list of data frames linked by `key`, or
#'   the result of an earlier step such as [anon_subject_dates()]. Data frames
#'   nested in them are de-identified too (see "Nested data" in Details). In
#'   datasets without the `key` column, at the top or in nested data, the
#'   values are left unchanged; keys in their attributes and names are
#'   rewritten (see Details).
#' @param key Name of the subject identifier column.
#' @param also Names of related identifier columns that hold one value per
#'   subject (default `"SUBJID"`). They are replaced with the pseudonym's
#'   number. Columns not present in a dataset are skipped.
#' @param key_cols Names of columns that hold subject keys, possibly of other
#'   subjects (default `"RSUBJID"`, as in SDTM `RELSUB`). Each value is replaced
#'   with that subject's pseudonym; values that are not a known key are removed
#'   with a warning.
#' @param prefix Prefix for character pseudonyms, followed by a zero-padded
#'   number (e.g. `"SUBJ-042"`). Numeric keys get the number itself.
#' @param keep_secrets Logical. If `TRUE`, keep the mapping from keys to
#'   pseudonyms; read it with [anon_secrets()]. Default `FALSE`: the mapping is
#'   not kept anywhere.
#' @param seed Optional seed for the random assignment, for reproducible tests.
#'   Anyone with the seed and the original data can rebuild the mapping, so do
#'   not set it for data you share. The global random number state is restored
#'   afterwards.
#' @param other_attributes What to do with attributes beyond the core ones
#'   (variable and dataset labels, comments, haven value labels and
#'   user-defined missing values, and names, which are always rewritten):
#'   custom text attributes, list- and data frame-valued attributes, Date or
#'   factor values held in attributes, and attributes of attributes.
#'   `"rewrite"` (default) rewrites what it can find in them, at any depth, as
#'   a best effort. `"drop"` removes them, and is the strict choice: nothing
#'   can hide in an attribute that is gone. Structural attributes are never
#'   dropped, and are structural only on the classes that use them: `class`,
#'   `names`, `row.names`, `dim` and `dimnames` on anything; `levels` and
#'   `contrasts` on factors; `tzone` on date-times; `units` on difftime and
#'   units objects; `tsp` on time series; `type` and `states` on
#'   `survival::Surv` objects; `ptype` on vctrs objects (its text is still
#'   rewritten); `groups` on grouped and rowwise tibbles; `sorted`,
#'   `index` and `.internal.selfref` on data.tables; SAS, SPSS and Stata
#'   formats, and a numeric `display_width`, on vectors. Elsewhere an
#'   attribute with one of these names is text (for example free-text `units`
#'   on a number). The log counts the attributes dropped in each dataset.
#'
#' @details
#' Pseudonyms are numbered in random order, and rows of each dataset are then
#' sorted by the new key (keeping the order of rows within each subject), so
#' neither the pseudonyms nor the row order reveal the order of the original
#' keys. Column types and attributes such as variable labels are kept
#' (original keys in their text are rewritten, see below). Character keys get
#' character pseudonyms, numeric keys get numbers, factors stay factors. Value
#' labels (haven `labels`), which describe the original values, are dropped
#' from replaced columns. Missing and blank keys are left
#' as they are. Values of `also` columns in rows without a key cannot be mapped
#' and are removed (set to missing), with a warning. `key`, `also` and
#' `key_cols` must not overlap.
#'
#' Original keys in text that describes the data are replaced with the
#' subject's pseudonym (a numeric key with its number), and original values of
#' `also` columns with the pseudonym's number as their column gets it (with
#' the same zero padding), in every dataset (with or without `key`), when they
#' appear as a whole value or a whole word, not next to a letter or digit (so
#' a number only as a whole number), compared without leading or trailing
#' spaces, and next to `","` or `"."` too (`"AB-1001,AB-1002"`,
#' `"AB-1001.1"`, `"77010001,5"`: such text is ambiguous and is rewritten).
#' Values made only of digits could be part of a number, so two rules apply
#' to them: after a digit and `"."` (the fraction of `"25.701054"` or
#' `"10.10005"`) they are found only when the number before the `"."` is
#' itself found as a key or `also` value (after a digit and `","`, as in
#' `"9999,10000"`, they are found);
#' and values of `also` columns before `"."` or `","` and a digit (`"10001.5"`)
#' only when the number after it is found, or, in names, before `"."` and
#' digits at the end (as `make.unique()` names elements). In names, keys and
#' `also` values as `make.names()` writes them (as `data.frame()` names
#' columns: `"X77010001"`, `"AB.1001"`) are rewritten too, to their
#' replacement as `make.names()` writes it. They are replaced in the attributes of columns, data
#' frames and the list of datasets (such as `label`, `comment` or a custom
#' note), at any depth (text in list or factor attributes, the values of data
#' frames held in attributes, names of attribute vectors, attributes of
#' attributes, attribute names); in the names of vector columns and the
#' dimnames of matrix columns; in haven value labels of columns that are not
#' replaced (the label names, and the label values of character columns) and
#' in character user-defined missing values (`na_values`, `na_range`) of those
#' columns; and in column names, character row names and dataset names.
#' Structural attributes (see `other_attributes`) are left alone, except that
#' text in a vctrs `ptype`, and the names of a factor's contrasts matrix, are
#' rewritten; a factor whose values are replaced or shifted drops a contrasts
#' matrix (named by its old levels, it no longer fits the new ones), and keeps
#' a contrasts function's name such as `"contr.sum"`. So are environments,
#' functions and S4
#' objects held in attributes (they can refer to themselves, are shared with
#' whoever else holds them, or must keep valid slots; S4 objects are named in
#' a warning). The grouping of a grouped or rowwise tibble (`groups`) and the
#' key and indices of a data.table (`sorted`, `index`) are renamed with the
#' columns. A grouped tibble held
#' in an attribute has its grouping values rewritten with its values. With
#' `other_attributes = "drop"`, attributes beyond the core ones are dropped
#' instead of rewritten. A label value or missing-value code is rewritten only
#' when no value of the column equals it or its rewrite (or, for `na_range`,
#' falls in either range), so the mapping from values to labels and which
#' values are missing don't change; a code kept this way is named in a
#' warning, as is a missing-value range whose rewritten ends would be in
#' descending order. Value labels whose codes the rewrite made equal are
#' merged into one entry, their original labels joined with `" / "` and then
#' rewritten (haven requires distinct codes; label text may repeat), and equal
#' missing-value codes are deduplicated; a warning names each. Ranges are
#' compared as R compares text in the session's collation, as haven does. A
#' rewritten name that clashes with another gets a numeric suffix (`"_1"`),
#' and settings and the log name the suffixed column; a column name
#' in the settings that was renamed differently in different datasets is
#' rewritten as text instead. Column and dataset names marked as `"bytes"`
#' are handled like other names and stay marked as `"bytes"` unless
#' rewritten. A dataset with nothing to rewrite is returned as the same
#' object, so `:=` on a data.table changes the input too; once a later step
#' changes it, its `.internal.selfref` is stale (call `data.table::setDT()`).
#' Keys made only of digits, numeric keys and values of `also` columns are
#' searched for and rewritten only when at least 5 characters long (digits
#' count): shorter ones match too many unrelated numbers and codes. A value of
#' an `also` column that belongs to more than one subject becomes the
#' column's name in brackets, such as `"[SUBJID]"` (whose number to give it
#' isn't known), and a warning counts those found. A
#' value found in both a text and a numeric `also` column becomes the text
#' column's form (the padded number). In
#' names (column, row and dataset names, vector and list names and dimnames,
#' at any depth), a key or `also` value followed by digits at the end of the
#' name, as `unlist()` names the elements of a list named by key (`"AB-1001"`
#' gives `"AB-10011"`, `"AB-10012"`), becomes its replacement followed by the
#' same digits, unless the whole name is itself a key (the longest key wins).
#' This can't tell such a suffix from a name that happens to continue a key
#' with digits (for a numeric key, a longer number): such names are rewritten
#' all the same. The log has one row per dataset
#' with such rewrites (and one for the list of datasets), with counts by kind;
#' this and earlier steps' logs and settings are renamed and rewritten to
#' match. The same text is rewritten inside nested data (packed data frame
#' columns and the data frames in list columns, see below), and in list
#' columns the attributes and names of the vectors and plain lists the
#' elements hold, at any depth. Text values in the data are not rewritten,
#' neither in columns nor in list-column elements or nested data frames; they
#' are searched (see below).
#'
#' **Nested data.** Data frames nested in the datasets are de-identified like
#' datasets: packed data frame columns (as made by `tidyr::pack()`) and list
#' columns of data frames (plain lists or vctrs `list_of`, as made by
#' `tidyr::nest()`, `dplyr::group_nest()` or `dplyr::nest_by()`), at any depth,
#' including data frames held in plain lists inside list columns. Each row of
#' a packed column belongs to its outer row's subject, and each data frame in
#' a list column to its row's subject, unless it has its own `key` column: then
#' each of its rows belongs to the subject in that column. Its `key`, `also`
#' and `key_cols` columns are replaced as in a dataset, subjects found only in
#' nested data get pseudonyms too, and a data frame with its own key has its
#' rows sorted by the new key (row names reset); others keep their rows in
#' order. Row names of nested data frames (tibbles too) are reset when their
#' rows are sorted, with those of their packed columns, whose rows move with
#' them (as in a dataset sorted by the new key); in those that belong to their
#' outer row, only row names that are
#' row positions (integer, or made of digits) are reset, and other row names,
#' such as the terms of a table of model coefficients, are kept and rewritten
#' as text. A dataset (or nested
#' data frame) without the `key` column whose rows still get one subject each
#' from nested data is de-identified as if it had the key, each row having
#' that subject: its rows are sorted by the new key and its own dates shifted.
#' This is so when a packed column has the key (as `tidyr::pack()` makes it),
#' and, when its rows don't already belong to subjects, when the data frames
#' in one of its list columns hold exactly one subject in every row (such as
#' data split by subject; a row with no data frame, or none with a key, holds
#' none). A table where every row happens to hold one subject (such as a
#' per-test table where each test found one subject) is treated the same way.
#' When nested data gives the rows subjects in ways that disagree (two packed
#' columns with the key, or a packed and a list column), a warning names the
#' table. When they hold several subjects (such as data nested by test,
#' each element holding `USUBJID`), the nested data frames are de-identified
#' through their key, and the dataset's own columns are left unchanged, in
#' their order, like a dataset without the key. Subjects are collected from
#' every nested data frame. Rows with no subject (a missing key, or a blank key
#' in the outer row) follow the rules above, the missing value being chosen
#' for each data frame; nested data frames with no key, in a dataset without
#' the key, are left unchanged like such datasets. Column types and labels are
#' kept; a `list_of` keeps its class, and its prototype (with its columns'
#' labels) follows the new elements (such as factor levels); grouped tibbles
#' are regrouped. List-column elements
#' that are not data frames, vectors, plain lists or `NULL` (fitted models such
#' as `lm` or parsnip fits, recipes, workflows, `rsplit` and `rset` objects,
#' environments, functions, formulas, S4 objects) are not entered or changed,
#' and their columns are named in a warning. They can hold copies of the
#' original data: de-identify the raw data first, and build models, recipes
#' and resampling splits on the de-identified copy. Changes inside nested data
#' are logged with counts, per dataset, with the column's path as `variable`
#' (such as `"data[[]][USUBJID]"`).
#'
#' A warning lists any other character or factor column (values or factor
#' levels), list column (the text values its elements hold: character values
#' and factor levels, at any depth), or column of a nested data frame (named as
#' `dataset.column[sub]` inside a packed column, `dataset.column[[]][sub]` for
#' column `sub` of the data frames in list column `column`, and deeper as
#' `dataset.column[sub][[]][subsub]`), in any dataset, that still contains
#' original key values as whole
#' values or as whole words inside text, or followed by digits at the end of a
#' value (searched for as described above; values are not rewritten), and, in
#' a second warning, original values of `also` columns (such as `SUBJID`) the
#' same way; and
#' any dataset without `key` that has a column named like `key`, `also` or
#' `key_cols` in any case. In datasets with `key`, and in nested data frames
#' with a subject, a column named like one of them in another case (such as
#' `usubjid` beside `USUBJID`) is not replaced and is named in a warning. Only
#' text or factor columns can be given in `key_cols`; the others must be
#' redacted or removed. Locations printed in warnings have keys rewritten, as
#' names can hold them.
#' Columns of S4 objects are not searched or changed, and are named in a
#' warning as data that can't be checked.
#' Missing values in
#' `also` columns stay missing. In `key`, `also` and `key_cols` columns, haven
#' user-defined missing values (`na_values`, `na_range`) become plain missing
#' values and the codes are dropped, because a code can't be told apart from a
#' real identifier; a warning names the columns where a value that became
#' missing this way is also a subject key elsewhere (both steps warn for the
#' `key` column).
#'
#' Random numbers come from R's random number generator. If your script calls
#' [set.seed()] before this step, anyone with the script and the data can
#' reproduce the assignment.
#'
#' Pseudonymized data remains personal data for anyone who holds the mapping or
#' can re-identify subjects by other means; measure the remaining risk with
#' [anon_risk()].
#'
#' @return An object of class `"anon_deid_step"`: a list with `data` (the
#'   transformed data, in the same form as the input), `log` (a tibble with
#'   `step`, `dataset`, `variable`, `n_changed`, `n_removed` and `detail`; the
#'   first row of each step records its settings; earlier steps are included
#'   when `data` was a step result), `settings` (a named list with each step's
#'   arguments, without the seed value) and
#'   `secrets` (`NULL` unless `keep_secrets = TRUE` here or in an earlier step).
#'   Saving the object (for example with [saveRDS()]) also saves its secrets.
#'
#' @seealso [anon_subject_dates()], [anon_secrets()], [anon_risk()]
#'
#' @examples
#' data <- list(
#'   dm = data.frame(USUBJID = c("01-701-1015", "01-701-1023"), SUBJID = c("1015", "1023")),
#'   vs = data.frame(USUBJID = c("01-701-1015", "01-701-1023"), VSSTRESN = c(70, 80))
#' )
#' result <- anon_subject_ids(data)
#' result$data
#'
#' @export
anon_subject_ids <- function(
  data,
  key = "USUBJID",
  also = "SUBJID",
  key_cols = "RSUBJID",
  prefix = "SUBJ-",
  keep_secrets = FALSE,
  seed = NULL,
  other_attributes = c("rewrite", "drop")
) {
  other_attributes <- match.arg(other_attributes)
  if (key %in% also || length(intersect(also, key_cols)) > 0) {
    stop("`key`, `also` and `key_cols` must not overlap.", call. = FALSE)
  }
  key_cols <- setdiff(key_cols, key)

  input <- as_dataset_list(data)
  data <- input$data
  check_key_name(key)
  local_text_cache(key = key)
  tables <- Map(nested_tables, data, key = key, dataset = seq_along(data))
  keyed <- keyed_datasets(data, key, tables)
  for (ds in names(tables)) {
    for (t in seq_along(tables[[ds]])) {
      if (tables[[ds]][[t]]$keyed) {
        tables[[ds]][[t]] <- zap_table_user_missing(tables[[ds]][[t]], c(key, also, key_cols), ids = c(key, also, key_cols))
      }
    }
  }
  frames <- key_frames(tables, key)
  keys <- subject_keys(frames, key)

  numbers <- sample_int_with_seed(length(keys), length(keys), seed = seed)
  width <- nchar(as.character(length(keys)))
  padded <- formatC(numbers, width = width, flag = "0")
  pseudonyms <- paste0(prefix, padded)

  log <- deid_log_start(
    "subject_ids", length(keys), key, seed,
    settings = paste0(
      "also = ", deparse_list(also), ", key_cols = ", deparse_list(key_cols),
      ", prefix = \"", prefix, "\"; rows sorted by the new key"
    )
  )
  removed_also <- 0
  removed_key_cols <- 0
  also_values <- list()
  details <- c(key = "replaced with pseudonyms", also = "replaced with the pseudonym number", key_cols = "replaced with pseudonyms")

  for (ds in names(tables)) {
    tabs <- tables[[ds]]
    tally <- log_tally()
    for (t in seq_along(tabs)) {
      tb <- tabs[[t]]
      if (!tb$keyed) {
        next
      }
      idx <- table_owner(tabs, t, keys, key)
      tabs[[t]]$idx <- idx
      df <- tb$df
      cols <- names(df)
      replaced <- integer(0)

      if (tb$has_key) {
        df[[key]] <- replace_ids(df[[key]], idx, pseudonyms, numbers)
        replaced <- c(replaced, match(key, cols))
        tally$add(table_column_path(tb, match(key, cols)), "key", list(changed = sum(!is.na(idx)), removed = 0))
      }

      for (col in intersect(also, cols)) {
        check_one_value_per_subject(df[[col]], idx, col)
        present <- !is.na(blank_to_na(df[[col]]))
        unmapped <- is.na(idx) & present
        also_idx <- ifelse(present, idx, NA_integer_)
        removed_also <- removed_also + sum(unmapped)
        # In text, a value becomes what its column gets: the padded number, or
        # the number in a numeric column.
        mapped <- !is.na(also_idx)
        is_number <- is.numeric(df[[col]])
        new_value <- if (is_number) as.character(numbers) else padded
        also_values[[length(also_values) + 1]] <- list(
          column = col, value = normalize_keys(df[[col]])[mapped], idx = also_idx[mapped],
          new = new_value[also_idx[mapped]], numeric = is_number
        )
        df[[col]] <- replace_ids(df[[col]], also_idx, padded, numbers, remove = unmapped, missing = table_missing(tb, df[[col]]))
        replaced <- c(replaced, match(col, cols))
        tally$add(table_column_path(tb, match(col, cols)), "also", list(changed = sum(!is.na(also_idx)), removed = sum(unmapped)))
      }

      for (col in setdiff(intersect(key_cols, cols), c(key, also))) {
        col_idx <- match(normalize_keys(df[[col]]), keys)
        unmapped <- is.na(col_idx) & !is.na(normalize_keys(df[[col]]))
        removed_key_cols <- removed_key_cols + sum(unmapped)
        df[[col]] <- replace_ids(df[[col]], col_idx, pseudonyms, numbers, remove = unmapped, missing = table_missing(tb, df[[col]]))
        replaced <- c(replaced, match(col, cols))
        tally$add(table_column_path(tb, match(col, cols)), "key_cols", list(changed = sum(!is.na(col_idx)), removed = sum(unmapped)))
      }
      tabs[[t]]$df <- df
      tabs[[t]]$changed <- union(tb$changed, replaced)
      # Row names of nested data frames are reset below, as at the top level.
      tabs[[t]]$touch <- t > 1
    }
    for (row in tally$rows()) {
      log <- deid_log_add(log, ds, row$variable, details[[row$kind]], row$counts$changed, row$counts$removed)
    }
    # Data frames in list columns that own their rows are sorted by the new
    # key, as datasets are; those that belong to their outer row keep their
    # order. Row names are reset when the rows are sorted, as at the top level
    # (they could link pseudonyms to the original rows); in data frames that
    # belong to their outer row, only row positions (integer, or text made of
    # digits) are reset, and other row names (such as the terms of a table of
    # model coefficients) are text, rewritten with the other names, and kept.
    data[[ds]] <- assemble_tables(tabs, function(df, tb, m) {
      if (tb$owns && tb$kind == "list") {
        return(sort_by_new_key(df, if (is.null(m$rows)) tb$idx else tb$idx[m$rows], numbers))
      }
      if (!has_text_row_names(df)) {
        df <- reset_row_names(df, packed = FALSE)
      }
      df
    })
    if (ds %in% keyed) {
      data[[ds]] <- sort_by_new_key(data[[ds]], tabs[[1]]$idx, numbers)
    }
  }
  # Keys, and the original values of `also` columns, are rewritten in text
  # that describes the data: a key to its pseudonym (a numeric key to its
  # number), a related ID to the pseudonym's number as its column got it.
  # Values made only of digits, and related IDs, are searched for only when
  # long enough (see searchable_keys()); other attributes are still dropped
  # when asked.
  numeric <- any_numeric_key(frames, key)
  new_ids <- if (numeric) as.character(numbers) else pseudonyms
  searchable <- searchable_keys(keys, numeric)
  related <- related_id_map(also_values, searchable$text)
  ids <- c(searchable$text, related$values)
  replacements <- c(new_ids[searchable$idx], related$new)
  searched <- length(ids) > 0
  rewrite <- id_rewriter(ids, replacements, related$values)
  rewrite_names <- id_rewriter(ids, replacements, related$values, names = TRUE)
  nested_key <- setdiff(datasets_with_key(tables), keyed)
  rewritten <- rewrite_described_text(
    data, rewrite, log, "original keys replaced with pseudonyms",
    other = other_attributes, rewrite_names = rewrite_names
  )
  data <- rewritten$data
  log <- rewritten$log
  keyed <- rewritten$rename(keyed)
  input$previous <- rewrite_previous_step(input$previous, rewritten)
  log <- deid_log_unkeyed(log, data, keyed, key, rewritten$rename(nested_key))
  # Warnings print locations, whose names can hold keys and dates (which this
  # step doesn't rewrite in data but must not print).
  rewrite_path <- path_rewriter(rewrite_names)
  locate <- function(x) format_locations(x, function(text) date_rewriter(rewrite_path(text)))

  if (removed_also > 0) {
    warning(
      removed_also, " value(s) of ", paste0("`", also, "`", collapse = ", "),
      " in rows without a `", key, "` could not be mapped and were removed.",
      call. = FALSE
    )
  }
  if (removed_key_cols > 0) {
    warning(
      removed_key_cols, " value(s) of ", paste0("`", key_cols, "`", collapse = ", "),
      " are not known `", key, "` values and were removed.",
      call. = FALSE
    )
  }
  warn_missing_code_keys(tables, keys, locate = locate)
  warn_leftover_keys(
    data, key, searchable$text,
    skip = c(also, key_cols), locate = locate, related = related$values, related_cols = related$columns
  )
  warn_ambiguous_related(related, also, ids, replacements, rewritten$texts, rewritten$name_texts)
  warn_id_name_variants(data, c(key, also, key_cols), locate = locate)
  warn_unkeyed_identifiers(data, key, c(also, key_cols), check_dates = FALSE, locate = locate)
  warn_merged_codes(rewritten$merged, rewritten$deduplicated, locate = locate)
  warn_s4_columns(data, paste0("original `", key, "` values"), rewritten$s4, locate = locate)
  warn_opaque_columns(data, locate = locate)
  warn_owner_conflicts(tables, locate = locate)
  if (searched) {
    warn_kept_codes(rewritten$kept, paste0("original `", key, "` values"), locate = locate)
    warn_invalid_utf8(data, rewritten$invalid, locate = locate)
  }

  secrets <- if (isTRUE(keep_secrets)) {
    list(subject_ids = tibble::tibble(key = keys, pseudonym = if (numeric) numbers else pseudonyms))
  }
  settings <- list(subject_ids = list(
    key = key, also = also, key_cols = key_cols, prefix = prefix,
    keep_secrets = isTRUE(keep_secrets), seeded = !is.null(seed), other_attributes = other_attributes
  ))
  settings <- rewritten$settings(settings)
  new_deid_step(input$unwrap(data), log, secrets, previous = input$previous, settings = settings)
}

#' Shift each subject's dates by a random offset
#'
#' Gives every subject one random offset, in whole days, and shifts all of that
#' subject's dates in every dataset by it. Intervals within a subject (time
#' since dose, study days, visit windows) are unchanged, while calendar dates no
#' longer line up between subjects or with real dates. This is the "date
#' offsetting" technique described in EMA's Anonymisation Report Form
#' Instructions: a random offset, with the same offset applied to all dates for
#' each participant.
#'
#' @inheritParams anon_subject_ids
#' @param also Names of related identifier columns (default `"SUBJID"`), as in
#'   [anon_subject_ids()]. This step doesn't replace them; their values are
#'   masked in the locations its warnings and errors print, each as the
#'   column's name in brackets (such as `"[SUBJID]"`). When `data` is the
#'   result of [anon_subject_ids()], that step's `also` is used instead (and
#'   its values are already replaced). Recorded in the settings.
#' @param vars Names of date columns to shift. By default, every character or
#'   factor column whose name ends in `DTC` (any case; ISO 8601 dates, as in
#'   SDTM) and every Date, POSIXct or POSIXlt column, in each dataset whose
#'   rows have subjects and in nested data frames with a subject (see "Nested data" in
#'   [anon_subject_ids()]); a list column holding Date, POSIXct or POSIXlt
#'   values counts as a Date column. `vars` names columns at every depth of
#'   nested data.
#'   In SDTM supplemental qualifier datasets (with `QNAM` and `QVAL`), `QVAL`
#'   values that are ISO 8601 dates with more than a bare year are shifted too
#'   (year-only values are left alone because they can't be told apart from
#'   numbers; a warning names the columns where they were kept). In columns not
#'   named `*DTC`, such as `QVAL` or a result column given in `vars`, only the
#'   values that are dates are shifted and other values are kept; bare
#'   four-digit numbers are kept too (with a warning),
#'   unless every value of the column, in every dataset, is a bare year (such
#'   as a column of birth years). Columns given in
#'   `vars` must be Date, POSIXct, or character or factor columns of ISO 8601
#'   dates.
#' @param max_days,min_days Each subject's offset is drawn uniformly from the
#'   whole numbers between `min_days` and `max_days` days, earlier or later.
#'   Defaults 30 and 365; `max_days` can be at most 3650. A warning is given
#'   when the range is so narrow, or includes 0, that offsets are easy to guess
#'   or some subjects keep their real dates.
#' @param partial How to handle partial ISO 8601 dates in character columns:
#'   `"shift"` (default) or `"remove"` (replace them with the column's missing
#'   value). With `"shift"`, a year-and-month date (`"2014-03"`, or SDTM
#'   `"2014-03--T08:00"` with the time kept) moves the middle of the month by
#'   the subject's offset and keeps month precision; a year-only date
#'   (`"2014"`, or SDTM `"2014---15"` with the day kept) moves by whole years,
#'   at least one, in the direction of the offset, so the real year is never
#'   kept. Both ends of an interval (`"2014-01-02/2014-01-05"`) are shifted.
#' @param keep_secrets Logical. If `TRUE`, keep each subject's offset; read it
#'   with [anon_secrets()]. Default `FALSE`.
#'
#' @details
#' Full ISO 8601 dates move by exactly the subject's offset and keep their
#' format and any time part. Date columns keep their type, and POSIXct and
#' POSIXlt columns keep their type, time zone and clock time (the calendar date moves, so a
#' shift across a daylight saving change keeps 08:00 at 08:00). Column
#' attributes such as variable labels are kept, with the dates in them
#' replaced by `"[DATE]"` (see below); value labels (haven `labels`) are
#' dropped from shifted columns, and haven user-defined missing values
#' (`na_values`, `na_range`) in the key and date columns become plain missing
#' values. A clock time that doesn't exist on the new date (inside a daylight
#' saving gap) moves forward by the gap. Study day variables (`--DY`) are not
#' changed and stay consistent with the shifted full dates, because each
#' subject's dates all move together.
#'
#' Dates can't be tied to one subject's offset in text that describes the
#' data, so there they are replaced with `"[DATE]"` (the default date
#' replacement of [nlp_default_replacements()]): in every dataset, in the same
#' attributes and names that [anon_subject_ids()] rewrites keys in (character
#' attributes at any depth, vector names and dimnames, value labels and
#' user-defined missing values under the same rule, column, row and dataset
#' names), and in this and earlier steps' logs and settings, with
#' `other_attributes` as described there. Date and date-time values held in
#' attributes (Date, POSIXct, POSIXlt) become the text `"[DATE]"` (zero-length
#' ones, such as a vctrs prototype, hold no date and are left alone), and a
#' factor held in an attribute whose levels now read the same becomes text
#' rather than have its levels merged. Value labels whose codes become equal
#' are merged as described there. The forms
#' replaced are those found inside text by the warning described below: ISO
#' 8601 full and year-and-month dates, SDTM partial dates (`"2014-01--T08:00"`
#' becomes `"[DATE]T08:00"`, and `"2014---15"`), and dates with a month name or
#' as numbers with separators. The log has one row per dataset with such
#' replacements, with counts. Values in the data are not changed by this
#' rule.
#'
#' Dates in nested data move by the offset of the subject their row belongs
#' to (see "Nested data" in [anon_subject_ids()]), with the same rules, and so
#' do Date and date-time values held in list columns, at any depth through
#' plain lists. Text values in list columns are not shifted. Objects that
#' can't be de-identified, such as fitted models, recipes and resampling
#' splits, are named in a warning: de-identify the raw data first, and build
#' them on the de-identified copy.
#'
#' Year-and-month dates are shifted from the middle of the month, and year-only
#' dates by whole years, so the order between a partial date and a full date of
#' the same subject can change (for example a start month can end up after an
#' end date).
#'
#' Dates in rows without a key cannot be tied to a subject's offset and are
#' removed, with a warning. In `*DTC` columns, values that are not recognised
#' ISO 8601 dates, or whose year would fall outside 0001-9999, are removed with
#' a warning; this includes a month and day without a year (`"--12-15"`).
#' Time-only values (`"-----T07:15"`) and durations (`"P1D"`) have no date and
#' are kept. Text columns given in `vars` must contain at least one recognised
#' date. In datasets without the `key` column (for example trial-level domains
#' such as `TS`, `TV` and `TA`) the values are left unchanged (dates in their
#' attributes and names are replaced as above), and they are listed in the
#' log; their real trial dates can narrow down subjects' offsets. A warning
#' lists any other
#' column in a keyed dataset that holds dates but was not shifted: Date or
#' POSIXct columns left out of `vars`, numeric columns with a SAS date format
#' (in a separate warning; convert them to Date or POSIXct to shift them), and
#' text columns with ISO 8601 dates as whole values, or dates inside text
#' (which can't be shifted) written as ISO 8601, with a month name
#' (`05JAN2014`, `UNK-JAN-2014`, `5 Jan 2014`, `Jan 5, 2014`, `JAN2014`) or as
#' numbers with separators (`01/05/2014`, `1/5/14`, `01.05.2014`,
#' `2014/01/05`), including the values kept in mixed columns such as `QVAL`,
#' list columns (the text values their elements hold) and the columns of
#' nested data frames. Other formats, such as `20140105`, `2014.01.05`,
#' `2014-1-5`, `Jan-05-2014`, `12/2014` and a month and day without a year
#' (`--12-15`), are not detected, nor replaced in attributes and names. It also
#' lists any dataset without `key` that has such columns or a column named
#' like `key` in another case.
#'
#' Text that isn't valid UTF-8 (such as a stray Windows-1252 byte) is
#' searched through a repaired copy with the invalid
#' bytes shown as hex (e.g. `"<92>"`); where something is rewritten, the
#' repaired text is written back. A pattern that matches digits can match
#' inside such a marker. Both steps warn about text marked as `"bytes"`, which
#' can't be searched, in values, attributes and names: they count distinct
#' texts and name the columns, or datasets (`"(list)"` for attributes of the
#' list of datasets and dataset names), holding it. Convert it to UTF-8 (for example with [iconv()]) and run the
#' step again. Every location printed in the steps' warnings and errors has
#' keys and dates rewritten and unprintable bytes escaped (`"<e9>"`): this
#' step shows original keys as `"[KEY]"` and the original values of the
#' `also` columns in data with subjects as the column's name in brackets
#' (`"[SUBJID]"`). It names the columns inside nested
#' data by position (`dataset.column[2]`, `dataset.column[[]][2]`), in
#' warnings and in the log, so that a column is named the same way whether or
#' not [anon_subject_ids()], which rewrites keys in nested names, ran first.
#'
#' Random numbers come from R's random number generator; see
#' [anon_subject_ids()]. Run this before or after [anon_subject_ids()]; each
#' subject keeps one offset either way. Under HIPAA, HHS does not address date
#' shifting directly, but Safe Harbor removes all elements of dates except the
#' year, so shifted dates do not meet it; see the package README.
#'
#' @return An object of class `"anon_deid_step"`, as for [anon_subject_ids()].
#'
#' @examples
#' data <- list(
#'   dm = data.frame(USUBJID = c("a", "b"), RFSTDTC = c("2014-01-02", "2013-07-19")),
#'   pc = data.frame(USUBJID = c("a", "a", "b"),
#'                   PCDTC = c("2014-01-02T08:00", "2014-01-03T09:30", "2013-07-19T08:00"))
#' )
#' anon_subject_dates(data)$data
#'
#' @export
anon_subject_dates <- function(
  data,
  key = "USUBJID",
  vars = NULL,
  max_days = 365,
  min_days = 30,
  partial = c("shift", "remove"),
  keep_secrets = FALSE,
  seed = NULL,
  other_attributes = c("rewrite", "drop"),
  also = "SUBJID"
) {
  partial <- match.arg(partial)
  other_attributes <- match.arg(other_attributes)
  validate_offset_range(max_days, min_days)

  input <- as_dataset_list(data)
  data <- input$data
  ids_runs <- if (!is.null(input$previous)) startsWith(names(input$previous$settings), "subject_ids") else logical(0)
  ids_done <- any(ids_runs)
  # The related-ID columns: those of an earlier anon_subject_ids() run, which
  # replaced them; otherwise `also`, used only to mask their values in the
  # locations printed.
  if (ids_done) {
    also <- unique(unlist(lapply(input$previous$settings[ids_runs], `[[`, "also"), use.names = FALSE))
  }
  # Columns inside nested data are named by position, so that a column is
  # named the same way whether or not the IDs step, which rewrites original
  # keys in nested names, ran first.
  check_key_name(key)
  local_text_cache(key = key, positional = TRUE)
  tables <- Map(nested_tables, data, key = key, positional = TRUE, dataset = seq_along(data))
  keyed <- keyed_datasets(data, key, tables)
  for (ds in names(tables)) {
    for (t in seq_along(tables[[ds]])) {
      if (tables[[ds]][[t]]$keyed) {
        tables[[ds]][[t]] <- zap_table_user_missing(tables[[ds]][[t]], c(key, vars), ids = key)
      }
    }
  }
  frames <- key_frames(tables, key)
  keys <- subject_keys(frames, key)
  # Warnings and errors print locations, whose names can hold dates and,
  # unless the IDs step already ran, original keys and related IDs: keys are
  # shown as "[KEY]", and the values of each `also` column in keyed data as
  # the column's name in brackets ("[SUBJID]").
  mask_keys <- if (ids_done) {
    identity
  } else {
    masked <- searchable_keys(keys, any_numeric_key(frames, key))$text
    also_values <- lapply(also, function(col) {
      values <- unlist(lapply(tables, function(tabs) {
        lapply(Filter(function(tb) tb$keyed && col %in% names(tb$df), tabs), function(tb) normalize_keys(tb$df[[col]]))
      }), use.names = FALSE)
      list(value = values, column = col, idx = 0L, new = paste0("[", col, "]"), numeric = FALSE)
    })
    related <- related_id_map(also_values, masked)
    ids <- c(masked, related$values)
    replacements <- c(rep("[KEY]", length(masked)), related$new)
    path_rewriter(id_rewriter(ids, replacements, related$values, names = TRUE))
  }
  locate <- function(x) format_locations(x, function(text) date_rewriter(mask_keys(text)))

  # Date columns of every table in keyed data, the datasets' own by name.
  for (ds in names(tables)) {
    for (t in seq_along(tables[[ds]])) {
      tb <- tables[[ds]][[t]]
      if (tb$keyed) {
        where <- if (nzchar(tb$path)) location(ds, tb$path) else ds
        tb$date_vars <- dataset_date_vars(tb$df, vars, key, where, locate, list_dates = tb$list_dates)
        tables[[ds]][[t]] <- zap_table_user_missing(tb, tb$date_vars)
      }
    }
  }
  date_vars <- lapply(stats::setNames(keyed, keyed), function(ds) tables[[ds]][[1]]$date_vars)
  keyed_tables <- unlist(lapply(tables, function(tabs) Filter(function(tb) tb$keyed, tabs)), recursive = FALSE)
  if (!is.null(vars) && length(unlist(lapply(keyed_tables, `[[`, "date_vars"))) == 0) {
    stop("None of `vars` were found in a dataset with `", key, "`.", call. = FALSE)
  }
  # In mixed columns, bare years are kept as numbers unless the column holds
  # only years in every dataset. List columns hold Date and date-time values.
  mixed_cols <- unique(unlist(lapply(keyed_tables, function(tb) {
    cols <- tb$date_vars[!tb$list_dates[match(tb$date_vars, names(tb$df))]]
    grep("DTC$", cols, ignore.case = TRUE, value = TRUE, invert = TRUE, useBytes = TRUE)
  })))
  keyed_dfs <- lapply(keyed_tables, `[[`, "df")
  years_only_cols <- Filter(function(col) holds_only_years(keyed_dfs, col), mixed_cols)
  kept_years <- character(0)
  kept_qval_years <- character(0)

  offsets <- draw_offsets(length(keys), max_days, min_days, seed)

  log <- deid_log_start(
    "subject_dates", length(keys), key, seed,
    settings = paste0(
      "min_days = ", min_days, ", max_days = ", max_days, ", partial = \"", partial, "\"",
      if (!is.null(vars)) paste0(", vars = ", deparse_list(vars))
    )
  )
  totals <- c(no_key = 0, invalid = 0)
  shift_counts <- c("shifted", "no_key", "invalid", "partial")

  for (ds in names(tables)) {
    tabs <- tables[[ds]]
    tally <- log_tally()
    for (t in seq_along(tabs)) {
      tb <- tabs[[t]]
      if (!tb$keyed) {
        next
      }
      tabs[[t]]$idx <- table_owner(tabs, t, keys, key)
      subject_offset <- offsets[tabs[[t]]$idx]
      df <- tb$df
      shifted_cols <- integer(0)
      where <- function(col) location(ds, table_column_path(tb, match(col, names(df)), positional = TRUE))

      for (col in tb$date_vars) {
        j <- match(col, names(df))
        if (tb$list_dates[j]) {
          shifted <- shift_list_dates(df[[col]], tb$lists[[j]], subject_offset, partial)
        } else {
          keep_non_dates <- col %in% mixed_cols
          shifted <- shift_dates_by_subject(
            df[[col]], subject_offset, partial, keep_non_dates,
            keep_years = keep_non_dates && !col %in% years_only_cols,
            missing = if (is.character(df[[col]]) || is.factor(df[[col]])) table_missing(tb, df[[col]])
          )
        }
        if (shifted$kept_years > 0) {
          kept_years <- c(kept_years, where(col))
        }
        df[[col]] <- shifted$values
        shifted_cols <- c(shifted_cols, j)
        totals <- totals + c(shifted$no_key, shifted$invalid)
        tally$add(table_column_path(tb, j, positional = TRUE), "date", shifted[shift_counts])
      }

      if (is.null(vars) && all(c("QNAM", "QVAL") %in% names(df))) {
        shifted <- shift_qval_dates(df$QVAL, subject_offset, partial, missing = table_missing(tb, df$QVAL))
        if (shifted$kept_years > 0) {
          kept_qval_years <- c(kept_qval_years, where("QVAL"))
        }
        df$QVAL <- shifted$values
        shifted_cols <- c(shifted_cols, match("QVAL", names(df)))
        totals <- totals + c(shifted$no_key, shifted$invalid)
        tally$add(table_column_path(tb, match("QVAL", names(df)), positional = TRUE), "qval", shifted[shift_counts])
      }
      tabs[[t]]$df <- df
      tabs[[t]]$changed <- union(tb$changed, shifted_cols)
    }
    for (row in tally$rows()) {
      counts <- row$counts
      removed <- counts$no_key + counts$invalid + counts$partial
      if (row$kind == "date") {
        log <- deid_log_add(log, ds, row$variable, shift_detail(counts), counts$shifted, removed)
      } else if (counts$shifted + removed > 0) {
        log <- deid_log_add(
          log, ds, row$variable, paste0(shift_detail(counts), " (date values only)"), counts$shifted, removed
        )
      }
    }
    data[[ds]] <- assemble_tables(tabs)
  }
  nested_key <- setdiff(datasets_with_key(tables), keyed)
  rewritten <- rewrite_described_text(
    data, date_rewriter, log, "dates replaced with [DATE]", dates = TRUE, other = other_attributes
  )
  data <- rewritten$data
  log <- rewritten$log
  names(date_vars) <- rewritten$rename(names(date_vars))
  for (ds in names(date_vars)) {
    date_vars[[ds]] <- rewritten$rename_columns(date_vars[[ds]], ds)
  }
  keyed <- rewritten$rename(keyed)
  input$previous <- rewrite_previous_step(input$previous, rewritten)
  log <- deid_log_unkeyed(log, data, keyed, key, rewritten$rename(nested_key))

  if (totals[["no_key"]] > 0) {
    warning(totals[["no_key"]], " date value(s) in rows without a `", key, "` were removed.", call. = FALSE)
  }
  if (totals[["invalid"]] > 0) {
    warning(
      totals[["invalid"]], " value(s) are not recognised ISO 8601 dates, or would fall outside years 0001-9999, and were removed.",
      call. = FALSE
    )
  }
  if (length(kept_years) > 0) {
    warning(
      "These columns hold bare four-digit numbers that were kept, because the column ",
      "also holds other values: ", paste(locate(kept_years), collapse = ", "),
      ". If they are years, they still show real years; redact or remove them.",
      call. = FALSE
    )
  }
  if (length(kept_qval_years) > 0) {
    warning(
      "These QVAL columns hold bare four-digit numbers, which are never shifted because they can't ",
      "be told apart from other numbers: ", paste(locate(kept_qval_years), collapse = ", "),
      ". If some are years (such as a birth year), redact or remove them.",
      call. = FALSE
    )
  }
  warn_missing_code_keys(tables, keys, positional = TRUE, locate = locate)
  warn_unshifted_iso_dates(data, date_vars, key, vars, locate = locate)
  warn_unkeyed_identifiers(data, key, character(0), check_dates = TRUE, locate = locate)
  warn_kept_codes(rewritten$kept, "dates", locate = locate)
  warn_merged_codes(rewritten$merged, rewritten$deduplicated, locate = locate)
  warn_s4_columns(data, "dates", rewritten$s4, locate = locate)
  warn_opaque_columns(data, locate = locate)
  warn_owner_conflicts(tables, locate = locate)
  warn_invalid_utf8(data, rewritten$invalid, locate = locate)

  secrets <- if (isTRUE(keep_secrets)) {
    list(date_offsets = tibble::tibble(key = keys, offset_days = offsets))
  }
  settings <- list(subject_dates = list(
    key = key, also = also, vars = vars, max_days = max_days, min_days = min_days, partial = partial,
    keep_secrets = isTRUE(keep_secrets), seeded = !is.null(seed), other_attributes = other_attributes
  ))
  settings <- rewritten$settings(settings)
  new_deid_step(input$unwrap(data), log, secrets, previous = input$previous, settings = settings)
}

#' Read the secrets kept by a de-identification step
#'
#' Returns the information needed to link de-identified data back to the
#' source, such as the mapping from keys to pseudonyms or each subject's date
#' offset. It exists only when a step was run with `keep_secrets = TRUE`. Keep
#' it separate from anything you share.
#'
#' Each table has a `key` column with the subject key as it was when that step
#' ran: the original key, or the pseudonym if [anon_subject_ids()] ran first.
#'
#' @param x An object of class `"anon_deid_step"`.
#'
#' @return A named list of tibbles, e.g. `subject_ids` or `date_offsets`.
#'
#' @examples
#' data <- data.frame(USUBJID = c("a", "b"))
#' result <- anon_subject_ids(data, keep_secrets = TRUE)
#' anon_secrets(result)
#'
#' @export
anon_secrets <- function(x) {
  if (!inherits(x, "anon_deid_step")) {
    stop("`x` must be the result of a de-identification step such as anon_subject_ids().", call. = FALSE)
  }
  if (is.null(x$secrets)) {
    stop(
      "This step holds no secrets: run the steps with `keep_secrets = TRUE`. anon() returns a ",
      "copy without secrets, so read them from the step you passed to anon().",
      call. = FALSE
    )
  }
  mget(sort(ls(x$secrets)), envir = x$secrets)
}

#' @export
print.anon_deid_step <- function(x, ...) {
  cat("De-identification steps:", paste(unique(x$log$step), collapse = ", "), "\n")
  cat("Secrets:", if (is.null(x$secrets)) "not kept" else "kept (read with anon_secrets())", "\n\n")
  print(as.data.frame(x$log), row.names = FALSE)
  invisible(x)
}

#' @export
print.anon_secrets <- function(x, ...) {
  cat("<anon_secrets: ", paste(sort(ls(x)), collapse = ", "),
      "; not shown, read with anon_secrets()>\n", sep = "")
  invisible(x)
}

#' @exportS3Method
as_anon_payload.anon_deid_step <- function(x, path = "$") {
  list(
    schema_version = anon_json_schema_version(),
    kind = "anon_deid_step",
    steps = unique(x$log$step),
    log = serialize_table_payload(x$log, path = paste0(path, "$log"))
  )
}

# helpers -----------------------------------------------------------------

# Secrets live in an environment so that print(), anon() and JSON output never
# show them by accident. Logs and secrets from an earlier step are carried on.
new_deid_step <- function(data, log, secrets, previous = NULL, settings = list()) {
  if (!is.null(previous)) {
    log <- rbind(previous$log, log)
    settings <- c(previous$settings, settings)
    names(settings) <- make.unique(names(settings), sep = "_")
    if (!is.null(previous$secrets)) {
      secrets <- c(mget(ls(previous$secrets), envir = previous$secrets), secrets)
      names(secrets) <- make.unique(names(secrets), sep = "_")
    }
  }
  secrets_env <- NULL
  if (length(secrets) > 0) {
    secrets_env <- list2env(secrets, envir = new.env(parent = emptyenv()))
    class(secrets_env) <- "anon_secrets"
  }
  structure(
    list(data = data, log = log, settings = settings, secrets = secrets_env),
    class = "anon_deid_step"
  )
}

as_dataset_list <- function(data) {
  previous <- NULL
  if (inherits(data, "anon_deid_step")) {
    previous <- data
    data <- data$data
  }
  if (is.data.frame(data)) {
    unmarked <- unmark_bytes_names(list(data = data))
    check_unique_columns(unmarked$data)
    return(list(
      data = unmarked$data, unwrap = function(x) unmarked$restore(x)[[1]], previous = previous
    ))
  }
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "") ||
    !all(vapply(data, is.data.frame, logical(1)))) {
    stop("`data` must be a data frame, a named list of data frames, or a step result.", call. = FALSE)
  }
  unmarked <- unmark_bytes_names(data)
  data <- unmarked$data
  if (anyDuplicated(names(data))) {
    stop("Dataset names in `data` must be unique.", call. = FALSE)
  }
  check_unique_columns(data)
  list(data = data, unwrap = unmarked$restore, previous = previous)
}

# R can't look up or translate names marked "bytes", so a data frame with such
# a column name can't be worked on by name. While a step runs, column and
# dataset names marked "bytes" are marked "unknown" (they are then searched
# if they are valid UTF-8, and reported as text that can't be searched if
# not); `restore` marks them "bytes" again, by position, in the result.
unmark_bytes_names <- function(data) {
  bytes_at <- function(x) if (is.null(x)) integer(0) else which(Encoding(x) == "bytes")
  set_encoding <- function(x, at, encoding) {
    Encoding(x)[at] <- encoding
    x
  }
  columns <- lapply(data, function(df) bytes_at(names(df)))
  groups <- lapply(data, function(df) bytes_at(names(attr(df, "groups", exact = TRUE))))
  datasets <- bytes_at(names(data))
  if (length(datasets) == 0 && all(lengths(columns) == 0) && all(lengths(groups) == 0)) {
    return(list(data = data, restore = function(x) x))
  }
  # The grouping of a grouped tibble names its columns too.
  mark_groups <- function(df, at, encoding) {
    if (length(at) > 0) {
      g <- attr(df, "groups", exact = TRUE)
      attr(g, "names") <- set_encoding(names(g), at, encoding)
      attr(df, "groups") <- g
    }
    df
  }
  for (i in seq_along(data)) {
    if (length(columns[[i]]) > 0) {
      attr(data[[i]], "names") <- set_encoding(names(data[[i]]), columns[[i]], "unknown")
    }
    data[[i]] <- mark_groups(data[[i]], groups[[i]], "unknown")
  }
  if (length(datasets) > 0) {
    names(data) <- set_encoding(names(data), datasets, "unknown")
  }
  restore <- function(x) {
    for (i in seq_along(x)) {
      if (length(columns[[i]]) > 0) {
        attr(x[[i]], "names") <- set_encoding(names(x[[i]]), columns[[i]], "bytes")
      }
      x[[i]] <- mark_groups(x[[i]], groups[[i]], "bytes")
    }
    if (length(datasets) > 0) {
      names(x) <- set_encoding(names(x), datasets, "bytes")
    }
    x
  }
  list(data = data, restore = restore)
}

# Columns are processed by name, so a repeated name would leave all but the
# first copy untouched.
# The names can hold keys, which aren't known yet, so the dataset and column
# are given by position.
check_unique_columns <- function(data) {
  for (i in seq_along(data)) {
    dup <- which(duplicated(names(data[[i]])))
    if (length(dup) > 0) {
      stop(
        "Column names in dataset ", i, " of `data` must be unique; column ", dup[1], " repeats an earlier name.",
        call. = FALSE
      )
    }
  }
}

check_key_name <- function(key) {
  if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
    stop("`key` must be a single column name.", call. = FALSE)
  }
}

# The datasets whose rows have subjects: those with the key column, and those
# whose packed columns, or list columns holding one subject per row, have it
# (see resolve_owners()). The key may also be found only deeper in nested
# data frames.
keyed_datasets <- function(data, key, tables) {
  keyed <- names(tables)[vapply(tables, function(tabs) tabs[[1]]$keyed, logical(1))]
  if (length(datasets_with_key(tables)) == 0) {
    stop("Key column `", key, "` not found in any dataset.", call. = FALSE)
  }
  keyed
}

# The datasets that hold the key column, at the top or in nested data.
datasets_with_key <- function(tables) {
  names(tables)[vapply(tables, function(tabs) any(vapply(tabs, function(tb) tb$keyed, logical(1))), logical(1))]
}

subject_keys <- function(data, key) {
  check_key_types(data, key)
  keys <- unlist(lapply(data, function(df) normalize_keys(df[[key]])), use.names = FALSE)
  unique(keys[!is.na(keys)])
}

# Replace identifiers by subject index, keeping the column's type and
# attributes (such as labels). Rows without a subject keep their value unless
# `remove` is TRUE for them; removed text becomes `missing` (see
# fill_missing()).
replace_ids <- function(x, idx, labels, numbers, remove = rep(FALSE, length(x)), missing = NULL) {
  has <- !is.na(idx)
  if (is.numeric(x)) {
    out <- x
    out[has] <- numbers[idx[has]]
    out[remove] <- NA
    return(drop_value_labels(out))
  }
  if (is.factor(x)) {
    values <- as.character(x)
    values[has] <- labels[idx[has]]
    values <- fill_missing(values, remove, missing)
    return(with_attributes_of(factor(values), x))
  }
  out <- x
  out[has] <- labels[idx[has]]
  out <- fill_missing(out, remove, missing, as.character(x))
  drop_value_labels(out)
}

# haven user-defined missing values (labelled_spss na_values and na_range)
# become plain missing values and the codes are dropped. In identifier columns
# a code can't be told apart from a real identifier, and a kept code could
# match a new pseudonym number.
zap_user_missing <- function(x) {
  if (!inherits(x, "haven_labelled_spss")) {
    return(x)
  }
  values <- vctrs::vec_data(x)
  values[is.na(x)] <- NA
  attrs <- attributes(x)
  attrs[c("na_values", "na_range")] <- NULL
  attributes(values) <- attrs
  values
}

# zap_user_missing() on the columns of a table named in `cols`, recording
# them as changed. For the identifier columns among them (`ids`), the values
# that were missing-value codes are recorded in `tb$zapped` (column position
# and values as text), for warn_missing_code_keys().
zap_table_user_missing <- function(tb, cols, ids = character(0)) {
  for (j in which(names(tb$df) %in% cols)) {
    x <- .subset2(tb$df, j)
    if (inherits(x, "haven_labelled_spss")) {
      if (names(tb$df)[j] %in% ids) {
        values <- vctrs::vec_data(x)
        codes <- unique(normalize_keys(values[is.na(x) & !is.na(values)]))
        if (length(codes) > 0) {
          tb$zapped <- c(tb$zapped, list(list(j = j, codes = codes)))
        }
      }
      tb$df[[j]] <- zap_user_missing(x)
      tb$changed <- union(tb$changed, j)
    }
  }
  tb
}

# Warn about identifier columns whose values were missing-value codes that
# are also subject keys (found in key columns elsewhere): those values became
# missing (see zap_user_missing()), though they may name a subject. `tables`
# are the datasets' tables after zap_table_user_missing(); `positional` names
# nested columns by position, as the dates step does.
warn_missing_code_keys <- function(tables, keys, positional = FALSE, locate = identity) {
  hits <- character(0)
  for (ds in names(tables)) {
    for (tb in tables[[ds]]) {
      for (z in tb$zapped) {
        if (any(z$codes %in% keys)) {
          hits <- c(hits, location(ds, table_column_path(tb, z$j, positional)))
        }
      }
    }
  }
  if (length(hits) > 0) {
    warning(
      "These identifier columns had missing-value codes that are also subject keys elsewhere: ",
      paste(unique(locate(hits)), collapse = ", "),
      ". Those values became missing, as missing-value codes in identifier columns do; check that they really are missing.",
      call. = FALSE
    )
  }
}

# Value labels (haven "labels") describe the original values, so they are
# dropped when values are replaced or shifted.
drop_value_labels <- function(x) {
  attr(x, "labels") <- NULL
  attr(x, "na_values") <- NULL
  attr(x, "na_range") <- NULL
  x
}

# Rebuild attributes (labels, formats, extra classes) on a new factor.
with_attributes_of <- function(new, old) {
  drop <- c("levels", "class", "names", "labels", "na_values", "na_range")
  # A contrasts matrix is named by the old levels (original keys or dates)
  # and no longer fits the new ones; a contrasts function's name is kept.
  if (!is.character(attr(old, "contrasts", exact = TRUE))) {
    drop <- c(drop, "contrasts")
  }
  extra <- attributes(old)[setdiff(names(attributes(old)), drop)]
  attributes(new) <- c(attributes(new), extra)
  new
}

# The missing value a character column already uses: "" if it has blank
# strings (as in SDTM read from XPT), otherwise NA.
missing_text_like <- function(x) {
  if (any(!is.na(x) & x == "")) "" else NA_character_
}

# `x` with the values at `at` removed: set to `missing` (one value, or one per
# value of `x`, as nested data chooses it per data frame), by default the
# missing value `text` already uses.
fill_missing <- function(x, at, missing = NULL, text = as.character(x)) {
  if (is.null(missing)) {
    missing <- missing_text_like(text)
  }
  x[at] <- if (length(missing) == 1) missing else missing[at]
  x
}

check_one_value_per_subject <- function(x, idx, col) {
  value <- blank_to_na(x)
  if (is.numeric(value)) value <- key_as_character(value)
  pairs <- unique(data.frame(idx = idx, value = as.character(value))[!is.na(idx) & !is.na(value), ])
  if (anyDuplicated(pairs$idx)) {
    stop("`", col, "` has more than one value for some subjects; it can't be aligned with the pseudonyms.", call. = FALSE)
  }
}

validate_offset_range <- function(max_days, min_days) {
  whole <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x == round(x)
  if (!whole(max_days) || max_days < 1 || max_days > 3650) {
    stop("`max_days` must be a single whole number from 1 to 3650 (ten years).", call. = FALSE)
  }
  if (!whole(min_days) || min_days < 0 || min_days > max_days) {
    stop("`min_days` must be a single whole number between 0 and `max_days`.", call. = FALSE)
  }
  n_offsets <- 2 * (max_days - min_days + 1) - (min_days == 0)
  if (min_days == 0) {
    warning("`min_days = 0` lets some subjects keep their real dates.", call. = FALSE)
  }
  if (n_offsets < 30) {
    warning("Only ", n_offsets, " possible offsets; offsets are easy to guess.", call. = FALSE)
  }
}

# The date columns of a data frame: `vars` if given, otherwise Date and
# POSIXct columns and text columns named *DTC. `list_dates` marks, by
# position, the list columns that hold Date or date-time values (see
# list_column_content()), which count as date columns. With `check`, columns
# in `vars` that aren't dates are an error.
dataset_date_vars <- function(df, vars, key, dataset, locate = identity, list_dates = logical(length(df)), check = TRUE) {
  if (!is.null(vars)) {
    selected <- setdiff(intersect(vars, names(df)), key)
    for (col in if (check) selected else character(0)) {
      x <- df[[col]]
      not_dates <- if (inherits(x, c("Date", "POSIXct", "POSIXlt")) || isTRUE(list_dates[match(col, names(df))])) {
        FALSE
      } else if (is.character(x) || is.factor(x)) {
        kinds <- parse_iso8601_dates(as.character(x))$kind
        any(kinds != "empty") && all(kinds[kinds != "empty"] == "invalid")
      } else {
        TRUE
      }
      if (not_dates) {
        stop("`", col, "` in `", locate(dataset), "` is not a date column (Date, POSIXct, POSIXlt, or ISO 8601 text).", call. = FALSE)
      }
    }
    return(selected)
  }
  is_date <- vapply(df, function(col) inherits(col, c("Date", "POSIXct", "POSIXlt")), logical(1)) | list_dates
  is_text <- vapply(df, function(col) is.character(col) || is.factor(col), logical(1))
  # useBytes: a name marked "bytes" can't be translated.
  is_dtc <- grepl("DTC$", names(df), ignore.case = TRUE, useBytes = TRUE) & is_text
  setdiff(names(df)[is_date | is_dtc], key)
}

# Draw `size` indices from 1:n with replacement, restoring the global RNG state.
sample_with_seed <- function(n, size, seed = NULL) {
  if (is.null(seed)) {
    return(sample.int(n, size = size, replace = TRUE))
  }
  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  }
  set.seed(seed)
  sample.int(n, size = size, replace = TRUE)
}

# Shift each value by its subject's offset (days), keeping the column's type and
# attributes. Values without an offset, and text that is not ISO 8601, are
# removed (text becomes `missing`, see fill_missing()). Returns the new values
# and counts.
shift_dates_by_subject <- function(x, offset, partial, keep_non_dates = FALSE, keep_years = keep_non_dates, missing = NULL) {
  if (inherits(x, "haven_labelled")) {
    shifted <- shift_dates_by_subject(vctrs::vec_data(x), offset, partial, keep_non_dates, keep_years, missing)
    attrs <- attributes(x)
    attrs[c("labels", "na_values", "na_range")] <- NULL
    attributes(shifted$values) <- attrs
    return(shifted)
  }
  if (inherits(x, "POSIXlt")) {
    # Shifted as POSIXct, keeping the POSIXlt class and time zone.
    shifted <- shift_dates_by_subject(as.POSIXct(x), offset, partial)
    tz <- attr(x, "tzone", exact = TRUE)
    lt <- as.POSIXlt(shifted$values, tz = if (length(tz) == 0) "" else tz[[1]])
    for (name in setdiff(names(attributes(x)), names(attributes(lt)))) {
      attr(lt, name) <- attr(x, name, exact = TRUE)
    }
    attr(lt, "tzone") <- tz
    shifted$values <- lt
    return(shifted)
  }
  if (is.factor(x)) {
    shifted <- shift_dates_by_subject(as.character(x), offset, partial, keep_non_dates, keep_years, missing)
    shifted$values <- with_attributes_of(factor(shifted$values), x)
    return(shifted)
  }

  present <- if (is.character(x)) !is.na(x) & !grepl("^[[:space:]]*$", x, useBytes = TRUE) else !is.na(x)
  use <- ifelse(is.na(offset), 0, offset)
  invalid <- rep(FALSE, length(x))
  removed_partial <- rep(FALSE, length(x))
  unchanged <- rep(FALSE, length(x))
  year_kept <- rep(FALSE, length(x))

  out <- x
  if (is.character(x)) {
    kinds <- parse_iso8601_dates(x)$kind
    # Values with no date to shift: time-only values and durations, and in
    # mixed columns (not named *DTC) values that aren't dates at all. With
    # `keep_years`, bare years are kept too, as they are more likely numbers.
    unchanged <- present & kinds %in% c("time_only", "duration")
    if (keep_non_dates) {
      unchanged <- unchanged | (present & kinds == "invalid")
    }
    if (keep_years) {
      year_kept <- present & is_year_only_value(x, kinds)
      unchanged <- unchanged | year_kept
    }
    no_offset <- present & !unchanged & is.na(offset)
    new_values <- suppressWarnings(shift_iso8601(x, use, partial, year_step = TRUE))
    if (partial == "remove") {
      removed_partial <- present & !unchanged & !no_offset & is_partial_date(x, kinds)
    }
    invalid <- present & !unchanged & !no_offset & !removed_partial & !nzchar(new_values)
    out[] <- new_values
    out[unchanged] <- x[unchanged]
    out <- fill_missing(out, no_offset | invalid | removed_partial, missing, x)
  } else if (inherits(x, "POSIXct")) {
    no_offset <- present & is.na(offset)
    tz <- attr(x, "tzone", exact = TRUE)
    tz <- if (is.null(tz) || length(tz) == 0) "" else tz[[1]]
    lt <- as.POSIXlt(x, tz = tz)
    lt$mday <- lt$mday + use
    # One per value: a single -1 breaks a POSIXlt of no values.
    lt$isdst <- rep(-1L, length(x))
    out[] <- as.POSIXct(lt, tz = tz)
    invalid <- present & !no_offset & !year_in_range(out)
    out[no_offset | invalid] <- NA
  } else {
    no_offset <- present & is.na(offset)
    out[] <- x + use
    invalid <- present & !no_offset & !year_in_range(out)
    out[no_offset | invalid] <- NA
  }
  out <- drop_value_labels(out)

  list(
    values = out,
    shifted = sum(present & !unchanged & !no_offset & !invalid & !removed_partial),
    no_key = sum(no_offset),
    invalid = sum(invalid),
    partial = sum(removed_partial),
    kept_years = sum(year_kept)
  )
}

# Whether a text column holds only bare years (ignoring missing and blank
# values) in every dataset that has it as text, and has some value somewhere.
holds_only_years <- function(data, col) {
  checks <- vapply(data, function(df) {
    x <- df[[col]]
    if (is.null(x) || inherits(x, c("Date", "POSIXct"))) {
      return(NA)
    }
    x <- if (is.factor(x)) as.character(x) else as.character(vctrs::vec_data(x))
    x <- x[!is.na(x) & !grepl("^[[:space:]]*$", x, useBytes = TRUE)]
    if (length(x) == 0) NA else all(is_year_only_value(x))
  }, logical(1))
  any(!is.na(checks)) && all(checks, na.rm = TRUE)
}

# Whether each Date or POSIXct value falls in years 0001-9999, the range ISO
# 8601 text can hold. Missing values count as in range.
year_in_range <- function(x) {
  year <- as.integer(format(x, "%Y"))
  is.na(x) | (!is.na(year) & year >= 1 & year <= 9999)
}

# Bare years, and intervals whose date ends are bare years ("1000/2000"). In
# columns that mix dates with other values these are more likely numbers.
is_year_only_value <- function(x, kinds = parse_iso8601_dates(x)$kind) {
  out <- kinds == "year"
  ends <- interval_end_kinds(x, kinds)
  if (!is.null(ends)) {
    dated <- ends$kind != "duration"
    out[unique(ends$value)] <- vapply(
      split(ends$kind[dated] == "year", factor(ends$value[dated], levels = unique(ends$value))),
      all, logical(1)
    )
  }
  out
}

# Partial dates, including intervals with a partial end.
is_partial_date <- function(x, kinds = parse_iso8601_dates(x)$kind) {
  partial_kinds <- c("month", "month_time", "year", "year_day")
  out <- kinds %in% partial_kinds
  ends <- interval_end_kinds(x, kinds)
  if (!is.null(ends)) {
    partial_end <- tapply(ends$kind %in% partial_kinds, ends$value, any)
    out[as.integer(names(partial_end))] <- partial_end
  }
  out
}

# The kinds of the two ends of each interval value, parsed in one call:
# a data frame with the value's position and the end's kind, or NULL.
interval_end_kinds <- function(x, kinds) {
  is_interval <- which(kinds == "interval")
  if (length(is_interval) == 0) {
    return(NULL)
  }
  ends <- strsplit(x[is_interval], "/", fixed = TRUE)
  data.frame(
    value = rep(is_interval, lengths(ends)),
    kind = parse_iso8601_dates(unlist(ends, use.names = FALSE))$kind
  )
}

# Shift QVAL values that are ISO 8601 dates with more than a bare year (which
# can't be told apart from a number); other values are untouched.
shift_qval_dates <- function(x, offset, partial = "shift", missing = NULL) {
  text <- as.character(x)
  kinds <- parse_iso8601_dates(text)$kind
  year_only <- !is.na(text) & is_year_only_value(text, kinds)
  kept_years <- sum(year_only)
  is_date <- kinds %in% c("full", "month", "month_time", "year_day", "interval") & !year_only
  out <- x
  if (!any(is_date)) {
    if (is.factor(x)) out <- with_attributes_of(factor(text), x)
    return(list(values = out, shifted = 0, no_key = 0, invalid = 0, partial = 0, kept_years = kept_years))
  }
  no_offset <- is_date & is.na(offset)
  removed_partial <- if (partial == "remove") is_date & !no_offset & is_partial_date(text, kinds) else rep(FALSE, length(text))
  movable <- is_date & !no_offset & !removed_partial
  new_text <- text
  new_text[movable] <- suppressWarnings(shift_iso8601(text[movable], offset[movable], "shift", year_step = TRUE))
  failed <- movable & !nzchar(new_text)
  movable <- movable & !failed
  new_text <- fill_missing(new_text, no_offset | removed_partial | failed, missing, text)
  if (is.factor(x)) {
    out <- with_attributes_of(factor(new_text), x)
  } else {
    out[] <- new_text
  }
  list(
    values = drop_value_labels(out), shifted = sum(movable), no_key = sum(no_offset),
    invalid = sum(failed), partial = sum(removed_partial), kept_years = kept_years
  )
}

shift_detail <- function(shifted) {
  removed <- c(
    if (shifted$no_key > 0) paste0(shifted$no_key, " removed (no key)"),
    if (shifted$invalid > 0) paste0(shifted$invalid, " removed (not a recognised date, or year out of range)"),
    if (isTRUE(shifted$partial > 0)) paste0(shifted$partial, " removed (partial)")
  )
  paste(c(paste0(shifted$shifted, " value(s) shifted"), removed), collapse = ", ")
}

# The original values of `also` columns (`also_values`: a list of `column`,
# `value` as text, subject index `idx`, replacement `new` and whether the
# column is `numeric`, for the rows with a subject), for the text search and
# rewrite. Values are searched without leading or trailing spaces. A value
# equal to a searched key (`key_texts`) is left to the key. A value found in
# both a text and a numeric column becomes the text column's form (the padded
# number), whatever the order of the datasets. A value that belongs to more
# than one subject can't be given one subject's number: it becomes the name
# of its column in brackets ("[SUBJID]"). Returns the values to rewrite (`values`, valid UTF-8 and at
# least `min_searched_id_chars` characters long) with their replacements
# (`new`), the `ambiguous` ones among them, and the names of the columns they
# came from (`columns`).
related_id_map <- function(also_values, key_texts) {
  pull <- function(field) unlist(lapply(also_values, function(a) rep_len(a[[field]], length(a$value))), use.names = FALSE)
  value <- trimws(as_utf8(pull("value")))
  column <- as.character(pull("column"))
  idx <- as.integer(pull("idx"))
  new <- as.character(pull("new"))
  numeric <- as.logical(pull("numeric"))
  columns <- unique(unlist(lapply(also_values, function(a) if (length(a$value) > 0) a$column)))
  ok <- searchable_text(value) & nchar(value) >= min_searched_id_chars & !value %in% key_texts
  order <- order(numeric[ok])
  value <- value[ok][order]
  idx <- idx[ok][order]
  new <- new[ok][order]
  column <- column[ok][order]
  subjects <- tapply(idx, value, function(i) length(unique(i)))
  ambiguous <- names(subjects)[subjects > 1]
  first <- !duplicated(value)
  shared <- value %in% ambiguous
  new[shared] <- paste0("[", column[shared], "]")
  list(values = value[first], new = new[first], ambiguous = ambiguous, columns = columns)
}

# Warn when original values of `also` columns that belong to more than one
# subject (rewritten as "[SUBJID]" or the like, see related_id_map()) were
# rewritten in text that describes the data (`texts` and `name_texts`, as
# rewrite_described_text() saw them before the rewrite), found as the rewrite
# finds them (id_finder(), with every id and its `replacements`). Only their
# number is printed: they are identifiers.
warn_ambiguous_related <- function(related, also, ids, replacements, texts, name_texts) {
  if (length(related$ambiguous) == 0) {
    return(invisible())
  }
  found_ids <- function(texts, names) {
    if (length(texts) == 0) {
      return(character(0))
    }
    ids[id_finder(ids, replacements, related$values, names)(texts)$idx]
  }
  n <- length(intersect(c(found_ids(texts, FALSE), found_ids(name_texts, TRUE)), related$ambiguous))
  if (n == 0) {
    return(invisible())
  }
  warning(
    n, " original value(s) of ", paste0("`", also, "`", collapse = ", "),
    " belong to more than one subject, so they were replaced with the column's name in brackets (such as \"[SUBJID]\") in attributes and names, ",
    "not with a subject's number.",
    call. = FALSE
  )
}

# Warn about text columns, in any dataset, that still contain original keys
# (`keys`, the texts chosen by searchable_keys()) as whole values or as whole
# words inside text, or followed by digits at the end of a value (as unlist()
# names the elements of a list named by key); and, in a second warning,
# original values of `also` columns (`related`, from columns `related_cols`)
# the same way. Columns named like the key or
# the `skip` columns, in any case, are not checked: they were just replaced, or
# warn_id_name_variants() or warn_unkeyed_identifiers() names them. List
# columns and columns of nested data frames are searched too (see
# dataset_tables()). `locate` rewrites the printed locations.
warn_leftover_keys <- function(data, key, keys, skip = character(0), locate = identity,
                               related = character(0), related_cols = character(0)) {
  if (length(keys) == 0 && length(related) == 0) {
    return(invisible())
  }
  # The texts of every location are searched in one call (nested data can
  # print one location for many tables).
  where <- character(0)
  is_nested <- logical(0)
  texts <- list()
  for (ds in names(data)) {
    for (leaf in dataset_leaves(data[[ds]])) {
      if (name_key(leaf$name) %in% name_key(c(key, skip)) || length(leaf$text) == 0) {
        next
      }
      where <- c(where, location(ds, leaf$path))
      is_nested <- c(is_nested, isTRUE(leaf$nested))
      texts <- c(texts, list(leaf$text))
    }
  }
  values <- lapply(texts, valid_utf8_text)
  distinct <- unique(unlist(values, use.names = FALSE))
  # Matched as the rewrite matches names (see id_matcher()), and also
  # followed by digits at the end of a value.
  ids <- c(keys, related)
  found <- if (length(distinct) > 0) {
    id_matcher(ids, related, glued = TRUE)(distinct)
  } else {
    list(owner = integer(0), idx = integer(0))
  }
  is_key <- found$idx <= length(keys)
  located <- function(found) {
    hit <- vapply(values, function(v) any(v %in% found), logical(1))
    list(hits = unique(where[hit]), nested = any(is_nested[hit]))
  }
  keys_found <- located(distinct[unique(found$owner[is_key])])
  if (length(keys_found$hits) > 0) {
    advice <- if (keys_found$nested) {
      paste0(
        ". Add text or factor columns to `key_cols` if they hold only keys; redact or remove the others, ",
        "including list columns and the columns of nested data frames (shown as `ds.col[sub]` in packed ",
        "columns and `ds.col[[]][sub]` in list columns)."
      )
    } else {
      ". Add them to `key_cols` if they hold only keys; otherwise redact or remove them."
    }
    warning(
      "These columns still contain original `", key, "` values, possibly inside text: ",
      paste(locate(keys_found$hits), collapse = ", "), advice,
      call. = FALSE
    )
  }
  related_found <- located(distinct[unique(found$owner[!is_key])])
  if (length(related_found$hits) > 0) {
    warning(
      "These columns still contain original ", paste0("`", related_cols, "`", collapse = " or "),
      " values (related identifiers), possibly inside text: ",
      paste(locate(related_found$hits), collapse = ", "), ". Redact or remove them.",
      call. = FALSE
    )
  }
}

# Column names compared in any case. Names that can't be searched (not valid
# UTF-8, or marked "bytes") are compared as they are; the warning about
# unsearchable text names them.
name_key <- function(x) {
  x <- as_utf8(x)
  ok <- searchable_text(x)
  x[ok] <- tolower(x[ok])
  x
}

# Identifiers made only of digits (numeric keys too, written as text), and
# related identifiers (`also`, such as SUBJID) whatever they hold, are
# searched for in text only from this many characters: shorter values match
# too many unrelated numbers and codes.
min_searched_id_chars <- 5L

# Keys that are searched for in text, as a data frame of `text` (the key as
# searched: valid UTF-8, without leading or trailing spaces) and `idx` (its
# position in `keys`, the first when two keys read the same once trimmed):
# keys holding a character other than a digit, or at least
# `min_searched_id_chars` characters long. Numeric keys (written as text by
# key_as_character()) need that many characters in any case.
searchable_keys <- function(keys, numeric = FALSE) {
  text <- trimws(as_utf8(keys))
  ok <- searchable_text(text) & nzchar(text) &
    (nchar(text) >= min_searched_id_chars | (!numeric & !grepl("^[0-9]+$", text)))
  ok <- ok & !duplicated(ifelse(ok, text, NA_character_), incomparables = NA)
  data.frame(text = text[ok], idx = which(ok), stringsAsFactors = FALSE)
}

# An ICU regular expression matching any of `ids` (keys and related
# identifiers) as a whole value or a whole word inside text: not next to a
# letter or digit, so a number is found only as a whole number. Every id
# searched for holds a character other than a digit or is at least
# `min_searched_id_chars` long, so it is found next to "," or "." too (in
# lists such as "AB-1,AB-2", in `make.unique()` names such as "AB-1.1", and
# in "77010001,5", which may be a decimal: such text is ambiguous, and is
# rewritten). `ending` is what must follow an id; `glued_ending` instead
# finds ids followed by digits at the end of the text.
key_text_regex <- function(ids, ending = id_suffix) {
  # Longest first, so that an id that starts another ("S-1" and "S-1-2")
  # doesn't match inside it.
  ids <- ids[order(-nchar(ids))]
  paste0(id_prefix, "(?:", paste(escape_regex_chars(ids), collapse = "|"), ")", ending)
}
id_prefix <- "(?<![[:alnum:]])"
id_suffix <- "(?![[:alnum:]])"
glued_ending <- "[0-9]+\\z"

# Rewriting text that describes the data ----------------------------------

# The attributes of `x` that hold structure rather than text about the data,
# by the classes that use them: on any other object, an attribute with one of
# these names is ordinary text (a label or comment, or another attribute).
# Used by the steps, which never rewrite or drop them (except as noted), and
# by anon(), which doesn't redact them; the test oracle keeps its own copy.
# - always: `class`, `names`, `row.names`, `dim`, `dimnames` (names and
#   dimnames are rewritten as names, never dropped);
# - factors: `levels` (a column's values; the levels of a factor held in an
#   attribute are rewritten as text) and `contrasts` when it is a matrix or
#   names a function (other text is ordinary);
# - date-times: `tzone`; difftime and units objects: `units`; time series:
#   `tsp`; survival::Surv: `type` and `states`;
# - vctrs objects: `ptype` (never dropped; its text is rewritten, its values
#   too when it is held in an attribute, and only its attributes and names
#   on a list column of data, whose elements' values aren't rewritten);
# - grouped and rowwise tibbles: `groups`; data.tables: `sorted`, `index`
#   (column references, renamed with the columns) and `.internal.selfref`;
# - atomic vectors: SAS, SPSS and Stata formats, and a numeric
#   `display_width` (format names can't hold text about the data).
structural_attributes <- function(x) {
  out <- c("class", "names", "row.names", "dim", "dimnames")
  if (is.factor(x)) out <- c(out, "levels", if (is_structural_contrasts(attr(x, "contrasts", exact = TRUE))) "contrasts")
  if (inherits(x, "POSIXt")) out <- c(out, "tzone")
  if (inherits(x, c("difftime", "units"))) out <- c(out, "units")
  if (inherits(x, "ts")) out <- c(out, "tsp")
  if (inherits(x, "Surv")) out <- c(out, "type", "states")
  if (inherits(x, c("vctrs_list_of", "vctrs_vctr"))) out <- c(out, "ptype")
  if (inherits(x, c("grouped_df", "rowwise_df"))) out <- c(out, "groups")
  if (inherits(x, "data.table")) out <- c(out, "sorted", "index", ".internal.selfref")
  if (is.atomic(x)) {
    out <- c(out, "format.sas", "informat.sas", "format.spss", "format.stata")
    if (is.numeric(attr(x, "display_width", exact = TRUE))) out <- c(out, "display_width")
  }
  out
}

# Attributes that are always rewritten, whatever `other_attributes` says:
# variable and dataset labels, comments, and haven value labels and
# user-defined missing values. Names and dimnames are rewritten too, and are
# never dropped.
core_attributes <- c("label", "comment", "labels", "na_values", "na_range")

# Whether a factor's `contrasts` is structural: a matrix, or the name of a
# contrasts function such as "contr.sum".
is_structural_contrasts <- function(contrasts) {
  !is.character(contrasts) ||
    (length(contrasts) == 1 && !is.na(contrasts) && exists(contrasts, mode = "function"))
}

# Whether attribute `name` of data frame `x` refers to its columns by name:
# the grouping of a grouped or rowwise tibble (dplyr), or the key and indices
# of a data.table. When columns are renamed, these are renamed with them;
# their values mirror the columns. On other objects, attributes with these
# names are ordinary attributes.
is_column_reference <- function(x, name) {
  (name == "groups" && inherits(x, c("grouped_df", "rowwise_df"))) ||
    (name %in% c("sorted", "index") && inherits(x, "data.table"))
}

# A function that finds `ids` (keys and related identifiers, as chosen by
# searchable_keys() and related_id_map()) in text (valid UTF-8): as whole
# values or whole words (see key_text_regex()), longest first, and with
# `glued` also followed by digits at the end of the text ("<key>1", as
# unlist() names elements). Ids made only of digits could be part of a
# number, so these are found under two rules, and a match is kept only when
# the largest set of matches that satisfies both keeps it:
# - after "<digit>." (the fraction of a number, such as "25.701054", or
#   "1.100011" in a name), only when the match that ends just before the "."
#   is kept ("10001.10002"); after "<digit>," the text is ambiguous (a list
#   such as "9999,10000", or a decimal comma) and the id is kept;
# - for the related identifiers among them (`related`, such as SUBJID values),
#   before ".<digit>" or ",<digit>" (the integer part of a decimal, such as
#   "10001.5"), only when the match that starts just after the separator is
#   kept or, with `names`, when "." and digits follow up to the end of the
#   text (as make.unique() names elements). A key before ".<digit>" or
#   ",<digit>" ("77010001,5", "AB-1001.1") is kept: such text is ambiguous,
#   and is rewritten.
# Returns, for each match, the text it is in (`owner`), its start and end
# (`at`), the id (`idx`, its position in `ids`) and the digits that follow it
# (`tail`).
id_matcher <- function(ids, related = character(0), glued = FALSE, names = FALSE) {
  # With `glued`, one pattern for both, so that a longer id followed by digits
  # wins over a shorter id followed by a separator ("S-1-21" is "S-1-2" and
  # "1", not "S-1" and "-21").
  ending <- if (glued) paste0("(?:", id_suffix, "|", glued_ending, ")") else id_suffix
  pattern <- key_text_regex(ids, ending)
  none <- list(owner = integer(0), at = matrix(integer(0), 0, 2), idx = integer(0), tail = character(0))
  function(text) {
    located <- stringi::stri_locate_all_regex(text, pattern, omit_no_match = TRUE)
    n <- vapply(located, nrow, integer(1))
    if (sum(n) == 0) {
      return(none)
    }
    at <- do.call(rbind, located[n > 0])
    owner <- rep(which(n > 0), n[n > 0])
    m <- stringi::stri_sub(text[owner], at[, 1], at[, 2])
    idx <- match(m, ids)
    tail <- rep("", length(m))
    # An id followed by digits: the longest id the match is made of followed
    # by digits, as the pattern chose it.
    todo <- which(is.na(idx))
    if (length(todo) > 0) {
      digits <- nchar(stringi::stri_extract_first_regex(m[todo], "[0-9]+$"))
      digits[is.na(digits)] <- 0L
      for (k in seq_len(max(digits))) {
        try <- todo[is.na(idx[todo]) & digits >= k]
        j <- match(substr(m[try], 1, nchar(m[try]) - k), ids)
        idx[try] <- j
        tail[try[!is.na(j)]] <- substring(m[try[!is.na(j)]], nchar(m[try[!is.na(j)]]) - k + 1)
      }
    }
    ok <- !is.na(idx)
    g <- which(ok & grepl("^[0-9]+$", ids[idx]))
    if (length(g) > 0) {
      char <- function(i, pos) stringi::stri_sub(text[owner[i]], pos, pos)
      digit <- function(x) grepl("^[0-9]$", x)
      sep <- function(x) x %in% c(".", ",")
      start <- at[g, 1]
      end <- at[g, 2] - nchar(tail[g])
      before <- start > 2 & char(g, start - 1) == "." & digit(char(g, start - 2))
      after <- ids[idx[g]] %in% related & !nzchar(tail[g]) & sep(char(g, end + 1)) & digit(char(g, end + 2))
      if (names) {
        rest <- stringi::stri_sub(text[owner[g]], end + 1)
        after <- after & !grepl("^[.][0-9]+$", rest)
      }
      # The neighbouring matches across the separator.
      kept <- which(ok)
      prev <- kept[match(paste(owner[g], start - 2), paste(owner[kept], at[kept, 2]))]
      nxt <- kept[match(paste(owner[g], end + 2), paste(owner[kept], at[kept, 1]))]
      repeat {
        drop <- ok[g] & ((before & (is.na(prev) | !ok[prev])) | (after & (is.na(nxt) | !ok[nxt])))
        if (!any(drop)) break
        ok[g[drop]] <- FALSE
      }
    }
    list(owner = owner[ok], at = at[ok, , drop = FALSE], idx = idx[ok], tail = tail[ok])
  }
}

# The ids found in texts by the rewrite (see id_rewriter()): a function of
# texts (valid UTF-8) giving id_matcher()'s matches, with `idx` the position
# in `ids` (a make.names() form counts as its id) and `new` the
# replacement. With `names`, the make.names() forms of ids are looked for only
# in texts that could hold one (holding "X" or "."), as make.names() writes
# them with an "X" in front or "." for other characters.
id_finder <- function(ids, replacements, related = character(0), names = FALSE) {
  find <- id_matcher(ids, related, glued = names, names = names)
  find_forms <- NULL
  origin <- seq_along(ids)
  all_replacements <- replacements
  if (names) {
    variants <- make.names(ids)
    extra <- variants != ids & !variants %in% ids & !duplicated(variants)
    if (any(extra)) {
      origin <- c(origin, which(extra))
      all_replacements <- c(replacements, make.names(replacements)[extra])
      find_forms <- id_matcher(c(ids, variants[extra]), related, glued = TRUE, names = TRUE)
    }
  }
  function(text) {
    if (is.null(find_forms)) {
      found <- find(text)
    } else {
      forms <- grepl("X", text, fixed = TRUE) | grepl(".", text, fixed = TRUE)
      plain <- find(text[!forms])
      found <- find_forms(text[forms])
      plain$owner <- which(!forms)[plain$owner]
      found$owner <- which(forms)[found$owner]
      found <- list(
        owner = c(plain$owner, found$owner), at = rbind(plain$at, found$at),
        idx = c(plain$idx, found$idx), tail = c(plain$tail, found$tail)
      )
    }
    found$new <- paste0(all_replacements[found$idx], found$tail)
    found$idx <- origin[found$idx]
    found
  }
}

# A function that replaces each of `ids` found by id_matcher() (with `names`,
# also followed by digits at the end of the text, see id_matcher()) with its
# replacement (`replacements`) followed by the same digits. A name made of an
# id followed by digits is ambiguous (a key followed by a position, or
# another number that starts with a key); it is rewritten, unless the whole
# name is itself an id. With `names`, an id as make.names() writes it (as
# data.frame() names columns: "X77010001", "AB.1001") becomes its
# replacement as make.names() writes it ("X1", "SUBJ.1").
id_rewriter <- function(ids, replacements, related = character(0), names = FALSE) {
  if (length(ids) == 0) {
    return(identity)
  }
  find <- id_finder(ids, replacements, related, names)
  function(x) {
    text <- as_utf8(x)
    hit <- which(searchable_text(text))
    found <- find(text[hit])
    if (length(found$idx) == 0) {
      return(x)
    }
    by_text <- split(seq_along(found$owner), found$owner)
    owners <- as.integer(names(by_text))
    for (k in seq_along(by_text)) {
      i <- hit[[owners[[k]]]]
      for (r in rev(by_text[[k]])) {
        stringi::stri_sub(text[i], found$at[r, 1], found$at[r, 2]) <- found$new[[r]]
      }
      x[i] <- text[i]
    }
    x
  }
}

# A rewriter for the parts of printed locations ("col", "col[sub]",
# "col[[]][sub]"): each name in them goes through `rewrite_names` on its own,
# so that the end of a name is found (see id_rewriter()).
path_rewriter <- function(rewrite_names) {
  function(x) {
    tokens <- stringi::stri_extract_all_regex(x, "[^\\[\\]]+|[\\[\\]]")
    n <- lengths(tokens)
    flat <- unlist(tokens, use.names = FALSE)
    name <- !is.na(flat) & !flat %in% c("[", "]")
    flat[name] <- rewrite_names(flat[name])
    flat[is.na(flat)] <- ""
    out <- vapply(split(flat, rep(seq_along(x), n)), paste, character(1), collapse = "", USE.NAMES = FALSE)
    out[is.na(x)] <- NA_character_
    out
  }
}

# Replaces each date that has_dates_in_values() would find, and SDTM partial
# dates inside text, with "[DATE]", the package's default replacement for
# dates.
date_rewriter <- function(x) {
  text <- as_utf8(x)
  hit <- which(searchable_text(text))
  # Every date form has a digit; looking for one first is much faster than the
  # case-insensitive search.
  hit <- hit[grepl("[0-9]", text[hit])]
  opts <- stringi::stri_opts_regex(case_insensitive = TRUE)
  hit <- hit[stringi::stri_detect_regex(text[hit], date_in_text_regex, opts_regex = opts)]
  x[hit] <- stringi::stri_replace_all_regex(
    text[hit], date_in_text_regex, nlp_default_replacements()$date, opts_regex = opts
  )
  x
}

# Apply `rewrite` (text in, text out) to the text that describes the data
# rather than its values, in every dataset (see rewrite_frame()), and to the
# attributes of the list of datasets and the dataset names. With `dates`,
# Date and date-time values held in attributes become "[DATE]" too. A
# rewritten name that clashes with another is made unique with a numeric
# suffix. Adds one row per changed dataset to `log`, with counts, renames
# datasets and columns in it and rewrites its details. Returns the data, the
# log, functions that rename datasets and columns in another step's log and
# rewrite settings, the number of texts that can't be searched (not valid
# UTF-8) by location, the columns whose value-label or missing-value codes
# were kept (see rewrite_codes()), and the distinct texts searched (`texts`).
# Names (column, row, dataset and attribute names, vector and list names and
# dimnames, at any depth) go through `rewrite_names` instead.
rewrite_described_text <- function(data, rewrite, log, detail, dates = FALSE, other = "rewrite",
                                   rewrite_names = rewrite) {
  # Every text is rewritten in one call, so that regular expressions are
  # compiled once and repeated text is rewritten once.
  seen <- list(text = list(), names = list())
  collect <- function(kind) {
    function(x) {
      seen[[kind]][[length(seen[[kind]]) + 1]] <<- as.character(x)
      x
    }
  }
  rewrite_datasets(new_rewrite_context(collect("text"), dates, other, collect("names")), data)
  # A lookup from each distinct searchable text to its rewrite, for the texts
  # that change.
  rewrite_once <- function(seen, rewrite) {
    texts <- unique(as_utf8(unlist(seen, use.names = FALSE)))
    texts <- texts[searchable_text(texts)]
    new_texts <- rewrite(texts)
    changed <- new_texts != texts
    list(all = texts, texts = texts[changed], new = new_texts[changed])
  }
  lookup_of <- function(done) {
    function(x) {
      text <- as_utf8(x)
      ok <- which(searchable_text(text))
      i <- match(text[ok], done$texts)
      hit <- ok[!is.na(i)]
      x[hit] <- done$new[i[!is.na(i)]]
      x
    }
  }
  text_done <- rewrite_once(seen$text, rewrite)
  names_done <- rewrite_once(seen$names, rewrite_names)

  ctx <- new_rewrite_context(lookup_of(text_done), dates, other, lookup_of(names_done))
  ctx$relabel <- rewrite
  out <- rewrite_datasets(ctx, data)
  rename <- function(ds) {
    known <- !is.na(ds) & ds %in% out$old
    ds[known] <- out$new[match(ds[known], out$old)]
    ds
  }
  rename_columns <- function(cols, ds) {
    map <- out$columns[[ds]]
    known <- !is.na(cols) & cols %in% names(map)
    cols[known] <- unname(map[cols[known]])
    cols
  }
  # Column names in settings apply to every dataset: a name renamed the same
  # way wherever it appears takes its new name; anything else is rewritten as
  # text.
  renamed_old <- unlist(lapply(out$columns, function(map) names(map)[names(map) != map]), use.names = FALSE)
  renamed_new <- unlist(lapply(out$columns, function(map) unname(map[names(map) != map])), use.names = FALSE)
  same <- vapply(split(renamed_new, renamed_old), function(v) length(unique(v)) == 1, logical(1))
  first <- !duplicated(renamed_old) & renamed_old %in% names(same)[same]
  setting_map <- stats::setNames(renamed_new[first], renamed_old[first])
  # Settings, details and logged paths hold names: their parts go through the
  # names rewriter, so that they read as the renamed data.
  rewrite_path <- path_rewriter(rewrite_names)
  rename_settings_text <- function(x) {
    known <- !is.na(x) & x %in% names(setting_map)
    x[known] <- unname(setting_map[x[known]])
    rewrite_path(x)
  }
  # Details are the package's own text and counts, with names quoted ("") or
  # in backticks: only those are rewritten.
  rename_detail <- function(x) {
    for (old in names(setting_map)) {
      x <- gsub(paste0("\"", old, "\""), paste0("\"", setting_map[[old]], "\""), x, fixed = TRUE, useBytes = TRUE)
    }
    quoted <- "\"[^\"]*\"|`[^`]*`"
    has <- which(!is.na(x) & stringi::stri_detect_regex(x, quoted))
    for (i in has) {
      at <- stringi::stri_locate_all_regex(x[i], quoted)[[1]]
      for (j in rev(seq_len(nrow(at)))) {
        inner <- stringi::stri_sub(x[i], at[j, 1] + 1, at[j, 2] - 1)
        stringi::stri_sub(x[i], at[j, 1] + 1, at[j, 2] - 1) <- rewrite_path(inner)
      }
    }
    x
  }
  # Columns inside nested data are logged by path ("col[[]][sub]"), each name
  # in it rewritten.
  rename_log <- function(log) {
    for (r in which(!is.na(log$dataset))) {
      log$dataset[r] <- rename(log$dataset[r])
      variable <- log$variable[r]
      log$variable[r] <- if (!is.na(variable) && !variable %in% names(out$columns[[log$dataset[r]]])) {
        rewrite_path(variable)
      } else {
        rename_columns(variable, log$dataset[r])
      }
    }
    log$detail <- rename_detail(log$detail)
    log
  }
  log <- rename_log(log)
  kinds <- c(attributes = "attribute value(s)", value_labels = "value label(s) or missing-value code(s)", names = "name(s)")
  add_row <- function(log, ds, counts) {
    rewritten <- counts[names(kinds)]
    if (sum(rewritten) > 0) {
      shown <- rewritten > 0
      log <- deid_log_add(
        log, ds, NA_character_,
        paste0(detail, " in attributes and names: ", paste(rewritten[shown], kinds[shown], collapse = ", ")),
        sum(rewritten)
      )
    }
    if (counts[["dropped"]] > 0) {
      log <- deid_log_add(log, ds, NA_character_, paste0(counts[["dropped"]], " other attribute(s) dropped"), 0, counts[["dropped"]])
    }
    log
  }
  for (ds in names(out$counts)) {
    log <- add_row(log, ds, out$counts[[ds]])
  }
  log <- add_row(log, NA_character_, out$list_counts)
  list(
    data = out$data, log = log, rename = rename, rename_columns = rename_columns, rename_log = rename_log,
    settings = function(settings) rapply(settings, rename_settings_text, classes = "character", how = "replace"),
    invalid = ctx$invalid, kept = unique(ctx$kept), merged = unique(ctx$merged),
    deduplicated = unique(ctx$deduplicated), s4 = unique(ctx$s4),
    texts = text_done$all, name_texts = names_done$all
  )
}

# Earlier steps' logs and settings name the same datasets and columns, so they
# are renamed and rewritten the same way.
rewrite_previous_step <- function(previous, rewritten) {
  if (!is.null(previous)) {
    previous$log <- rewritten$rename_log(previous$log)
    previous$settings <- rewritten$settings(previous$settings)
  }
  previous
}

# Rewrites text through `rewrite`, counting changes by kind ("attributes",
# "value_labels" or "names") and other attributes dropped (with `other =
# "drop"`), and recording the texts that can't be searched by `where` (the
# location being rewritten: "dataset.column", "dataset", or "(list)" for the
# list of datasets) and where codes were kept or merged. `relabel` rewrites the
# labels joined when codes are merged. Names go through `rewrite_names`.
new_rewrite_context <- function(rewrite, dates = FALSE, other = "rewrite", rewrite_names = rewrite) {
  ctx <- new.env(parent = emptyenv())
  ctx$rewrite <- rewrite
  ctx$rewrite_names <- rewrite_names
  ctx$dates <- dates
  ctx$other <- other
  ctx$counts <- c(attributes = 0, value_labels = 0, names = 0, dropped = 0)
  ctx$invalid <- list()
  ctx$kept <- character(0)
  ctx$merged <- character(0)
  ctx$deduplicated <- character(0)
  ctx$s4 <- character(0)
  ctx$relabel <- identity
  ctx$where <- "(list)"
  ctx
}

# Rewrite text, counting the changes as `kind` (not counted when NULL). Names
# (kind "names", or `name` TRUE) go through `ctx$rewrite_names`.
rewrite_text <- function(ctx, x, kind, name = identical(kind, "names")) {
  if (!is.character(x) || length(x) == 0) {
    return(x)
  }
  bad <- unsearchable_texts(x)
  if (length(bad) > 0) {
    ctx$invalid[[ctx$where]] <- unique(c(ctx$invalid[[ctx$where]], bad))
  }
  new <- if (name) ctx$rewrite_names(x) else ctx$rewrite(x)
  if (!is.null(kind)) {
    ctx$counts[[kind]] <- ctx$counts[[kind]] + sum(!is.na(x) & x != new)
  }
  new
}

# The data with every dataset rewritten by rewrite_frame(), then the
# attributes of the list and the dataset names. Returns the data, the old and
# new dataset names, each dataset's map from old to new column names, and the
# counts of changes by dataset and on the list.
rewrite_datasets <- function(ctx, data) {
  old <- names(data)
  ctx$where <- "(list)"
  new <- unique_rewritten(rewrite_text(ctx, old, NULL, name = TRUE), old)
  counts <- list()
  columns <- list()
  for (i in seq_along(data)) {
    ctx$counts[] <- 0
    ctx$where <- new[i]
    df <- rewrite_frame(ctx, data[[i]])
    columns[[new[i]]] <- stats::setNames(names(df), names(data[[i]]))
    data[[i]] <- df
    counts[[new[i]]] <- ctx$counts + c(0, 0, old[i] != new[i], 0)
  }
  ctx$counts[] <- 0
  ctx$where <- "(list)"
  data <- rewrite_attributes(ctx, data, frame = TRUE)
  names(data) <- new
  list(data = data, old = old, new = new, columns = columns, counts = counts, list_counts = ctx$counts)
}

# Rewrite a data frame's text: the attributes of each column, column names,
# character row names, and the data frame's own attributes (renaming the
# columns in those that refer to them). Values are data and are not rewritten,
# but nested data is entered: packed data frame columns are rewritten like
# data frames, and in list columns the attributes and names of what the
# elements hold (see rewrite_list_column()). With `values` TRUE (a data frame
# held in an attribute, which is text about the data), its character values
# and factor levels are rewritten too. `nested` names the columns of nested
# data frames in locations as "where[col]" (by position in the dates step).
# Attributes are set directly, so that methods such as those of grouped
# tibbles don't drop the others.
rewrite_frame <- function(ctx, df, values = FALSE, nested = FALSE) {
  where <- ctx$where
  old_cols <- names(df)
  new_cols <- unique_rewritten(rewrite_text(ctx, old_cols, "names"), old_cols)
  df <- rewrite_attributes(ctx, df, frame = TRUE, columns = stats::setNames(new_cols, old_cols), values = values)
  cls <- oldClass(df)
  x <- unclass(df)
  for (j in seq_along(x)) {
    ctx$where <- if (nested) {
      column_path(where, new_cols[j], j, isTRUE(text_cache$.positional))
    } else {
      location(where, new_cols[j])
    }
    column <- x[[j]]
    new <- if (values) {
      rewrite_value(ctx, column)
    } else if (isS4(column)) {
      column
    } else if (is.data.frame(column)) {
      rewrite_frame(ctx, column, nested = TRUE)
    } else if (is.list(column) && !inherits(column, "POSIXlt")) {
      rewrite_list_column(ctx, column)
    } else {
      rewrite_attributes(ctx, column)
    }
    if (!identical(new, column)) {
      x[[j]] <- new
    }
  }
  ctx$where <- where
  if (!identical(new_cols, old_cols)) {
    attr(x, "names") <- new_cols
  }
  row_names <- attr(x, "row.names")
  if (is.character(row_names)) {
    new_rows <- unique_rewritten(rewrite_text(ctx, row_names, "names"), row_names)
    if (!identical(new_rows, row_names)) {
      attr(x, "row.names") <- new_rows
    }
  }
  class(x) <- cls
  # When nothing changed, the data frame itself is returned, so that what
  # refers to it (such as a data.table's .internal.selfref) stays valid.
  if (identical(x, df)) df else x
}

# Rewrite a list column: its own attributes, and the attributes and names of
# what its elements hold, at any depth through plain lists (see
# is_plain_list()), with data frames rewritten as in rewrite_frame() and
# located as "where[[]]". Text values are data and are not rewritten. Objects
# that can't be de-identified (models, environments, S4 objects and similar)
# are not entered; a list column of another class is left as it is.
rewrite_list_column <- function(ctx, column) {
  if (!is_plain_list(column)) {
    return(column)
  }
  column <- rewrite_attributes(ctx, column, ptype_values = FALSE)
  where <- ctx$where
  # Data frames that differ only in their values (as tidyr::nest() makes
  # them) are rewritten once: the rewritten attributes and names, and the
  # counts, are reused for the others.
  done <- new.env(parent = emptyenv())
  element <- function(e, id = NULL) {
    if (is.data.frame(e)) {
      id <- frame_text_id(e)
      if (!is.null(id) && !is.null(done[[id]])) {
        hit <- done[[id]]
        ctx$counts <- ctx$counts + hit$counts
        return(with_frame_text(e, hit$frame, hit$columns))
      }
      ctx$where <- paste0(where, "[[]]")
      before <- ctx$counts
      out <- rewrite_frame(ctx, e, nested = TRUE)
      ctx$where <- where
      if (!is.null(id)) {
        frame <- attributes(out)
        frame$row.names <- NULL
        assign(id, list(counts = ctx$counts - before, frame = frame, columns = lapply(unclass(out), attributes)), envir = done)
      }
      return(out)
    }
    if (is.atomic(e) && !isS4(e) && is.null(attributes(e))) {
      return(e)
    }
    # Vectors and plain lists alike in their attributes and names (`id`, see
    # text_ids()) are rewritten once, like data frames.
    if (!is.null(id) && !is.null(done[[id]])) {
      hit <- done[[id]]
      ctx$counts <- ctx$counts + hit$counts
      return(with_text_skeleton(e, hit$skeleton))
    }
    before <- ctx$counts
    out <- if (is.atomic(e) && !isS4(e)) {
      rewrite_attributes(ctx, e)
    } else if (is_plain_list(e)) {
      rewrite_attributes(ctx, elements(e))
    } else {
      e
    }
    if (!is.null(id)) {
      assign(id, list(counts = ctx$counts - before, skeleton = text_skeleton(out)), envir = done)
    }
    out
  }
  elements <- function(l, ids = NULL) {
    cls <- oldClass(l)
    x <- unclass(l)
    for (i in seq_along(x)) {
      e <- x[[i]]
      if (!is.null(e)) {
        new <- element(e, if (length(ids) > 0 && !is.na(ids[i])) ids[i])
        if (!identical(new, e)) x[[i]] <- new
      }
    }
    class(x) <- cls
    x
  }
  # A list_of's prototype follows its rewritten elements (names, labels), so
  # that they still match it.
  with_elements_ptype(elements(column, text_ids(column)))
}

# For each element of a list column, the id of its text_skeleton() when
# another element has the same one (it is then worth rewriting once), and NA
# otherwise, and for data frames (see frame_text_id()) and vectors without
# attributes.
text_ids <- function(column) {
  # Both passes of rewrite_described_text() see the same column.
  cache_id <- paste0("text_ids:", rlang::obj_address(column))
  if (isTRUE(text_cache$.active) && !is.null(text_cache[[cache_id]])) {
    return(text_cache[[cache_id]])
  }
  ids <- vapply(unclass(column), function(e) {
    if (is.null(e) || is.data.frame(e) || (is.atomic(e) && !isS4(e) && is.null(attributes(e)))) {
      return(NA_character_)
    }
    skeleton <- text_skeleton(e)
    if (isFALSE(skeleton)) NA_character_ else rlang::hash(skeleton)
  }, character(1), USE.NAMES = FALSE)
  ids[!ids %in% ids[duplicated(ids)]] <- NA_character_
  if (isTRUE(text_cache$.active)) {
    assign(cache_id, ids, envir = text_cache)
  }
  ids
}

# The attributes (names included) of a vector, or of a plain list and what it
# holds at any depth, which are all its rewrite in a list column depends on;
# FALSE when it also depends on values (data frames, value labels and
# missing-value codes compared with the values). Other objects aren't
# entered.
text_skeleton <- function(e) {
  if (is.null(e)) {
    return(list())
  }
  if (is.data.frame(e)) {
    return(FALSE)
  }
  if (is.atomic(e) && !isS4(e)) {
    a <- attributes(e)
    if (any(c("labels", "na_values", "na_range") %in% names(a))) {
      return(FALSE)
    }
    return(list(a))
  }
  if (!is_plain_list(e)) {
    return(list("not entered"))
  }
  kids <- lapply(unclass(e), text_skeleton)
  if (any(vapply(kids, isFALSE, logical(1)))) {
    return(FALSE)
  }
  list(attributes(e), kids)
}

# A vector or plain list given the attributes of another with the same
# text_skeleton() (`skeleton`, that of its rewrite).
with_text_skeleton <- function(e, skeleton) {
  if (is.null(e) || identical(skeleton, list("not entered"))) {
    return(e)
  }
  if (is.atomic(e)) {
    attributes(e) <- skeleton[[1]]
    return(e)
  }
  x <- unclass(e)
  for (i in seq_along(x)) {
    if (!is.null(x[[i]])) x[[i]] <- with_text_skeleton(x[[i]], skeleton[[2]][[i]])
  }
  attributes(x) <- skeleton[[1]]
  x
}

# An id for the text a data frame's rewrite depends on, when that is only its
# names and its own and its columns' attributes: NULL for a data frame whose
# rewrite also depends on its values (character row names, grouping, nested
# columns, value labels and missing-value codes compared with the values,
# vector names).
frame_text_id <- function(df) {
  attrs <- attributes(df)
  if (is.character(attrs$row.names) || any(c("groups", "sorted", "index") %in% names(attrs))) {
    return(NULL)
  }
  cols <- unclass(df)
  columns <- lapply(cols, attributes)
  per_value <- c("names", "dimnames", "labels", "na_values", "na_range")
  if (any(vapply(cols, function(x) is.list(x) || isS4(x), logical(1))) ||
    any(vapply(columns, function(a) any(per_value %in% names(a)), logical(1)))) {
    return(NULL)
  }
  attrs$row.names <- NULL
  rlang::hash(list(attrs, columns))
}

# A data frame given the rewritten attributes of another with the same
# frame_text_id(): its own (`frame`, without row names) and its columns'.
with_frame_text <- function(df, frame, columns) {
  x <- unclass(df)
  for (j in seq_along(x)) {
    if (!identical(attributes(x[[j]]), columns[[j]])) attributes(x[[j]]) <- columns[[j]]
  }
  attributes(x) <- c(frame, list(row.names = .row_names_info(df, 0L)))
  x
}

# Rewrite the attributes of `x` (a column, a data frame, the list of datasets,
# or a value held in an attribute): names and dimnames, value labels and
# user-defined missing values, labels and comments, and every other attribute
# that isn't structural (at any depth, see rewrite_value()), and the attribute
# names themselves. With `ctx$other` "drop", those other attributes are
# dropped instead and counted. With `frame`, `names` are column or dataset
# names, left to the caller, and `columns` maps the data frame's old column
# names to new ones, for the attributes that refer to columns; with `values`
# (a data frame held in an attribute), the values of a grouped tibble's
# `groups` are rewritten too. With `ptype_values` FALSE (a list column of
# data), a vctrs prototype's values are left as they are, as the elements'
# are. Environments, functions and S4 objects are left
# alone: they can refer to themselves, changing their attributes would change
# them for every holder, and an S4 object's slots must stay valid.
rewrite_attributes <- function(ctx, x, frame = FALSE, columns = NULL, values = FALSE, ptype_values = TRUE) {
  attrs <- attributes(x)
  if (length(attrs) == 0 || is.environment(x) || is.function(x) || isS4(x)) {
    return(x)
  }
  structural <- structural_attributes(x)
  codes <- c("labels", "na_values", "na_range")
  column_values <- NULL
  for (name in names(attrs)) {
    value <- attrs[[name]]
    new <- if (name == "names") {
      if (frame) value else rewrite_text(ctx, value, "names")
    } else if (name == "dimnames") {
      rewrite_dimnames(ctx, value)
    } else if (frame && is_column_reference(x, name)) {
      refs <- if (is.null(columns)) value else rename_column_references(name, value, columns)
      if (values && name == "groups") rewrite_group_values(ctx, refs) else refs
    } else if (name == "contrasts" && name %in% structural && !is.null(dimnames(value))) {
      # A contrasts matrix is named by the factor's levels: kept, with its
      # names rewritten as names.
      renamed <- value
      dimnames(renamed) <- rewrite_dimnames(ctx, dimnames(value))
      renamed
    } else if (name == "ptype" && name %in% structural && ptype_values) {
      # A vctrs prototype is kept, but text in it (such as factor levels) is
      # rewritten like any value held in an attribute.
      rewrite_value(ctx, value)
    } else if (name == "ptype" && name %in% structural) {
      # The prototype of a list column of data: its values (such as factor
      # levels) mirror the elements', which are data and not rewritten, so
      # only its attributes and names are.
      if (is.data.frame(value)) rewrite_frame(ctx, value, nested = TRUE) else rewrite_attributes(ctx, value)
    } else if (name %in% structural) {
      value
    } else if (name %in% codes && !frame && inherits(value, c("Date", "POSIXt"))) {
      rewrite_value(ctx, value)
    } else if (name %in% codes && !frame) {
      if (is.null(column_values)) column_values <- underlying_text(x)
      rewrite_codes(ctx, name, value, column_values)
    } else if (ctx$other == "drop" && !name %in% core_attributes) {
      ctx$counts[["dropped"]] <- ctx$counts[["dropped"]] + 1
      NULL
    } else {
      if (isS4(value)) {
        ctx$s4 <- c(ctx$s4, ctx$where)
      }
      rewrite_value(ctx, value)
    }
    if (!identical(new, value)) {
      attr(x, name) <- new
    }
  }
  old <- names(attributes(x))
  new_names <- rewrite_text(ctx, old, "names")
  if (!identical(new_names, old)) {
    new_names <- unique_rewritten(new_names, old)
    for (k in which(new_names != old)) {
      value <- attr(x, old[k], exact = TRUE)
      attr(x, old[k]) <- NULL
      attr(x, new_names[k]) <- value
    }
  }
  x
}

# The `groups` of a grouped tibble held in an attribute, with the values of
# its grouping columns rewritten like the tibble's own values (not `.rows`).
rewrite_group_values <- function(ctx, groups) {
  if (!is.data.frame(groups)) {
    return(groups)
  }
  cls <- oldClass(groups)
  g <- unclass(groups)
  for (j in which(names(g) != ".rows")) {
    g[[j]] <- rewrite_value(ctx, g[[j]])
  }
  class(g) <- cls
  g
}

# A data frame attribute that refers to columns by name (see
# is_column_reference()), with the columns renamed: the columns of `groups`,
# the names in `sorted`, and the "__col1__col2" names of the indices held in
# `index`, read through the column names (which may hold "__" themselves).
rename_column_references <- function(name, value, columns) {
  rename <- function(cols) {
    known <- !is.na(cols) & cols %in% names(columns)
    cols[known] <- unname(columns[cols[known]])
    cols
  }
  if (name == "groups" && is.data.frame(value)) {
    attr(value, "names") <- rename(names(value))
  } else if (name == "sorted" && is.character(value)) {
    value <- rename(value)
  } else if (name == "index" && !is.null(attributes(value))) {
    indices <- names(attributes(value))
    for (k in seq_along(indices)) {
      parts <- split_index_name(indices[k], names(columns))
      if (is.null(parts)) {
        # Not an index of known columns: left as it is.
        next
      }
      new <- paste0("__", paste(rename(parts), collapse = "__"))
      if (new != indices[k]) {
        attrs <- attributes(value)
        names(attrs)[names(attrs) == indices[k]] <- new
        attributes(value) <- attrs
      }
    }
  }
  value
}

# The columns named by a data.table index name ("__a__b"), matched against the
# column names `cols` so that a name holding "__" is read whole; NULL when the
# name isn't made of them.
split_index_name <- function(index, cols) {
  if (!startsWith(index, "__")) {
    return(NULL)
  }
  rest <- substring(index, 3)
  if (!nzchar(rest)) {
    return(character(0))
  }
  for (col in cols[order(-nchar(cols))]) {
    if (identical(rest, col)) {
      return(col)
    }
    if (startsWith(rest, paste0(col, "__"))) {
      tail <- split_index_name(substring(rest, nchar(col) + 1), cols)
      if (!is.null(tail)) {
        return(c(col, tail))
      }
    }
  }
  NULL
}

# Rewrite a value held in an attribute: its text (character values, factor
# levels, list elements at any depth, data frames as in rewrite_frame() with
# their values) and its own attributes. With `ctx$dates`, Date and date-time
# values become the text "[DATE]" (zero-length ones are left alone).
# Environments, functions, S4 objects and other objects that aren't vectors
# are left alone.
rewrite_value <- function(ctx, v) {
  if (!is.atomic(v) && !is.list(v) || is.null(v) || isS4(v)) {
    return(v)
  }
  if (ctx$dates && inherits(v, c("Date", "POSIXt"))) {
    if (length(v) == 0) {
      # No date to hide; a zero-length date can be a prototype.
      return(v)
    }
    out <- rep(nlp_default_replacements()$date, length(v))
    out[is.na(v)] <- NA_character_
    names(out) <- names(v)
    ctx$counts[["attributes"]] <- ctx$counts[["attributes"]] + sum(!is.na(out))
    return(rewrite_attributes(ctx, out))
  }
  if (is.data.frame(v)) {
    return(rewrite_frame(ctx, v, values = TRUE))
  }
  if (is.factor(v)) {
    new <- rewrite_text(ctx, levels(v), "attributes")
    if (anyDuplicated(new)) {
      # Levels that now read the same become text rather than be merged.
      out <- new[as.integer(v)]
      names(out) <- names(v)
      v <- out
    } else if (!identical(new, levels(v))) {
      levels(v) <- new
    }
  } else if (is.character(v)) {
    v <- rewrite_text(ctx, v, "attributes")
  } else if (is.list(v) && !inherits(v, "POSIXlt")) {
    cls <- oldClass(v)
    v <- unclass(v)
    for (i in seq_along(v)) {
      if (!is.null(v[[i]])) {
        v[[i]] <- rewrite_value(ctx, v[[i]])
      }
    }
    class(v) <- cls
  }
  rewrite_attributes(ctx, v)
}

rewrite_dimnames <- function(ctx, dimnames) {
  for (i in seq_along(dimnames)) {
    if (!is.null(dimnames[[i]])) {
      dimnames[[i]] <- rewrite_text(ctx, dimnames[[i]], "names")
    }
  }
  if (!is.null(names(dimnames))) {
    names(dimnames) <- rewrite_text(ctx, names(dimnames), "names")
  }
  dimnames
}

# Value labels and user-defined missing values. Label names are text and are
# rewritten. A text code (a label value, a missing value or an end of a
# missing range) is rewritten only when no value of the column equals the
# code or its rewrite (or, for a range, falls in either range), so that the
# mapping from values to labels, and which values are missing, don't change;
# such a kept code is recorded in `ctx$kept`. Codes that the rewrite made
# equal are merged by merge_equal_codes(), and recorded in `ctx$merged`.
rewrite_codes <- function(ctx, name, codes, values) {
  new <- codes
  new_names <- NULL
  if (name == "labels" && !is.null(names(codes))) {
    new_names <- rewrite_text(ctx, names(codes), NULL)
  }
  if (is.character(codes)) {
    rewritten <- rewrite_text(ctx, unname(codes), NULL)
    changed <- !is.na(codes) & rewritten != codes
    if (any(changed)) {
      values <- unique(values[!is.na(values)])
      apply <- if (name == "na_range") {
        in_range <- function(r) length(r) == 2 && any(values >= r[[1]] & values <= r[[2]])
        # A range whose rewritten ends aren't in ascending order isn't valid.
        ascending <- length(rewritten) != 2 || isTRUE(rewritten[[1]] <= rewritten[[2]])
        changed & !in_range(codes) & !in_range(rewritten) & ascending
      } else {
        changed & !codes %in% values & !rewritten %in% values
      }
      if (any(changed & !apply)) {
        ctx$kept <- c(ctx$kept, ctx$where)
      }
      new[apply] <- rewritten[apply]
      ctx$counts[["value_labels"]] <- ctx$counts[["value_labels"]] + sum(apply)
    }
  }
  if (!is.null(new_names)) {
    ctx$counts[["value_labels"]] <- ctx$counts[["value_labels"]] + sum(!is.na(new_names) & new_names != names(codes))
    names(new) <- new_names
  }
  if (name != "na_range" && !anyDuplicated(unname(codes))) {
    merged <- merge_equal_codes(new, ctx$relabel, names(codes))
    if (merged$merged) {
      if (name == "labels") {
        ctx$merged <- c(ctx$merged, ctx$where)
      } else {
        ctx$deduplicated <- c(ctx$deduplicated, ctx$where)
      }
      new <- merged$codes
    }
  }
  new
}

# Value-label codes that a rewrite or redaction made equal, merged: haven
# requires distinct codes (label text may repeat). One entry is kept per
# code, where the first of them was; its label joins the original labels
# (`original_labels`), in their order, with " / ", and the joined label goes
# once through `relabel` (the same rewrite or redaction). Missing-value codes
# have no labels and are deduplicated. Returns the codes and whether any were
# merged.
merge_equal_codes <- function(codes, relabel = identity, original_labels = names(codes)) {
  plain <- unname(codes)
  if (!anyDuplicated(plain)) {
    return(list(codes = codes, merged = FALSE))
  }
  group <- match(plain, plain)
  first <- which(!duplicated(plain))
  out <- codes[first]
  if (!is.null(names(codes))) {
    joined <- vapply(first, function(i) {
      members <- which(group == i)
      if (length(members) == 1) {
        names(codes)[i]
      } else {
        relabel(paste(original_labels[members], collapse = " / "))
      }
    }, character(1))
    names(out) <- joined
  }
  list(codes = out, merged = TRUE)
}

# A column's values as text, for comparing with its value-label codes.
underlying_text <- function(x) {
  if (is.list(x)) {
    return(character(0))
  }
  as.character(if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else unclass(x))
}

# Rewritten names, with those that now clash with another made unique
# ("[DATE]", "[DATE]_1"); names that were not rewritten keep their spelling.
unique_rewritten <- function(new, old) {
  if (is.null(new) || !anyDuplicated(new)) {
    return(new)
  }
  changed <- new != old
  made <- make.unique(c(new[!changed], new[changed]), sep = "_")
  new[!changed] <- made[seq_len(sum(!changed))]
  new[changed] <- made[sum(!changed) + seq_len(sum(changed))]
  new
}

# Warn about columns whose value-label or missing-value codes still hold
# `what` (original keys, or dates), kept by rewrite_codes().
warn_kept_codes <- function(kept, what, locate = identity) {
  if (length(kept) > 0) {
    warning(
      "These columns keep value-label or missing-value codes that hold ", what,
      ", because rewriting them would change which values they label or which are missing, ",
      "or leave a missing-value range in descending order: ",
      paste(sort(locate(kept)), collapse = ", "), ". Redact or remove them.",
      call. = FALSE
    )
  }
}

# Warn about columns whose value labels or missing-value codes were merged
# because the rewrite made their codes equal (see merge_equal_codes()).
warn_merged_codes <- function(merged, deduplicated = character(0), locate = identity) {
  if (length(merged) > 0) {
    warning(
      "Value labels of these columns were merged because the rewrite made their codes equal: ",
      paste(sort(locate(merged)), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (length(deduplicated) > 0) {
    warning(
      "User-defined missing values of these columns were deduplicated because the rewrite made them equal: ",
      paste(sort(locate(deduplicated)), collapse = ", "), ".",
      call. = FALSE
    )
  }
}

# Warn about text, anywhere in the data, that can't be searched (marked as
# "bytes"; other text that isn't valid UTF-8 is searched repaired), counting
# distinct texts by location: values by column (see dataset_tables()), and
# texts found while rewriting attributes and names (`invalid`, a list of texts
# by location). `locate` rewrites the printed locations, which can hold names
# inside nested data.
warn_invalid_utf8 <- function(data, invalid = list(), locate = identity) {
  texts <- invalid
  for (ds in names(data)) {
    for (leaf in dataset_leaves(data[[ds]])) {
      bad <- unsearchable_texts(leaf$text)
      if (length(bad) > 0) {
        where <- location(ds, leaf$path)
        texts[[where]] <- unique(c(texts[[where]], bad))
      }
    }
  }
  if (length(texts) > 0) {
    warning(
      length(unique(unlist(texts, use.names = FALSE))), " distinct text(s) marked as \"bytes\" can't be searched: ",
      paste(sort(locate(names(texts))), collapse = ", "),
      ". Convert them to UTF-8 (for example with iconv()) and run the step again.",
      call. = FALSE
    )
  }
}

# Warn about columns holding S4 objects, and about S4 objects held in
# attributes (`in_attributes`, by location), whose contents can't be checked
# for `what` (original keys, or dates) or rewritten.
warn_s4_columns <- function(data, what, in_attributes = character(0), locate = identity) {
  hits <- character(0)
  for (ds in names(data)) {
    for (leaf in dataset_leaves(data[[ds]])) {
      if (isTRUE(leaf$s4)) {
        hits <- c(hits, location(ds, leaf$path))
      }
    }
  }
  hits <- unique(hits)
  if (length(hits) > 0) {
    warning(
      "These columns hold S4 objects, which can't be checked for ", what, ": ",
      paste(locate(hits), collapse = ", "), ". Redact or remove them, or convert them to plain columns.",
      call. = FALSE
    )
  }
  if (length(in_attributes) > 0) {
    warning(
      "These columns or datasets hold S4 objects in attributes, which can't be checked for ", what,
      " and were kept as they are: ", paste(sort(locate(unique(in_attributes))), collapse = ", "),
      ". Use `other_attributes = \"drop\"` to remove them.",
      call. = FALSE
    )
  }
}

# Warn about list columns holding objects that can't be de-identified (see
# is_plain_list()), which the steps don't enter.
warn_opaque_columns <- function(data, locate = identity) {
  hits <- character(0)
  for (ds in names(data)) {
    for (leaf in dataset_leaves(data[[ds]])) {
      if (isTRUE(leaf$opaque)) {
        hits <- c(hits, location(ds, leaf$path))
      }
    }
  }
  hits <- unique(hits)
  if (length(hits) > 0) {
    warning(
      "These list columns hold objects that can't be de-identified (models, recipes, resampling splits, ",
      "environments, functions or similar): ", paste(locate(hits), collapse = ", "),
      ". They may hold original data; de-identify the data before building them, and remove these columns.",
      call. = FALSE
    )
  }
}

# Warn about data frames whose rows get subjects from nested data in more
# than one way that disagree (two packed columns with the key, or a packed
# column and a list column; see resolve_owners()). `tables` are the step's
# tables, by dataset, before it changed them.
warn_owner_conflicts <- function(tables, locate = identity) {
  hits <- character(0)
  for (ds in names(tables)) {
    for (tb in tables[[ds]]) {
      if (isTRUE(tb$owner_conflict)) {
        hits <- c(hits, if (nzchar(tb$path)) location(ds, tb$path) else ds)
      }
    }
  }
  if (length(hits) > 0) {
    warning(
      "These data frames get their rows' subjects from nested data whose keys disagree: ",
      paste(unique(locate(hits)), collapse = ", "), ". Subjects were taken from the first packed column ",
      "with the key, or without one, the first list column.",
      call. = FALSE
    )
  }
}

# A location: a dataset (or "dataset.column", as rewrite_frame() builds them)
# and a column inside it, printed "dataset.column" by format_locations().
location_sep <- "\x1f"
location <- function(dataset, path) paste0(dataset, location_sep, path)

# Locations (such as "ds.col" or "ds.col[sub]") as a step's warnings print
# them. Text marked "bytes" or not valid UTF-8 can't be printed, so its bytes
# are shown escaped ("<e9>"); then names, which can hold keys or dates, go
# through `rewrite` (the step's rewriter). Every step warning that names a
# location uses this.
format_locations <- function(x, rewrite = identity) {
  x <- as.character(x)
  bytes <- Encoding(x) == "bytes"
  if (any(bytes)) {
    unmarked <- x[bytes]
    Encoding(unmarked) <- "unknown"
    x[bytes] <- unmarked
  }
  x <- enc2utf8(x)
  bad <- !is.na(x) & !validUTF8(x)
  x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
  # The parts of a location (see location()) are rewritten apart, so that the
  # end of a dataset name doesn't hide a key at the start of a column name.
  parts <- strsplit(x, location_sep, fixed = TRUE)
  parts[lengths(parts) == 0] <- list("")
  n <- lengths(parts)
  if (all(n <= 1)) {
    return(rewrite(x))
  }
  flat <- rewrite(unlist(parts, use.names = FALSE))
  vapply(split(flat, rep(seq_along(x), n)), paste, character(1), collapse = ".", USE.NAMES = FALSE)
}

# Text held in the data, searched by the warnings ---------------------------

# The tables of a data frame (see nested_tables()), each with `leaves`: what
# each of its columns holds, for the warnings that search it. A leaf has
# `path` (where the column is printed: "column", or inside nested data
# "column[sub]" and "column[[]][sub]", see column_path()), `name` (its own
# name), `top` (the dataset's column it is in), `text` (the distinct text
# values, including those that can't be searched), `date` (TRUE for a Date or
# date-time column, or a list column holding such values), `sas` (TRUE for a
# number with a SAS date format), `s4` (TRUE for a column of S4 objects,
# which isn't searched), `opaque` (TRUE for a list column holding objects
# that can't be de-identified), `frame` (TRUE for a packed data frame column,
# whose columns are leaves of its own table), `nested` (TRUE inside nested
# data and for list columns, which can't be given in `key_cols`) and `keyed`
# (its table is in keyed data). A character column holds its values, a factor
# its levels, a list column the text values its elements hold (see
# list_column_content()); attributes and names are rewritten by the steps and
# not searched here. Cached while a step runs, as several warnings search the
# same data frame.
dataset_tables <- function(df) {
  id <- rlang::obj_address(df)
  cached <- text_cache[[id]]
  if (!is.null(cached)) {
    return(cached)
  }
  positional <- isTRUE(text_cache$.positional)
  tables <- nested_tables(df, text_cache$.key %||% NA_character_, positional)
  for (t in seq_along(tables)) {
    tables[[t]]$leaves <- table_leaves(tables[[t]], t, positional)
  }
  if (isTRUE(text_cache$.active)) {
    assign(id, tables, envir = text_cache)
  }
  tables
}

# The leaves of every table of a data frame (see dataset_tables()).
dataset_leaves <- function(df) {
  unlist(lapply(dataset_tables(df), `[[`, "leaves"), recursive = FALSE)
}

table_leaves <- function(tb, t, positional = FALSE) {
  df <- tb$df
  lapply(seq_along(df), function(j) {
    x <- .subset2(df, j)
    name <- names(df)[j]
    leaf <- list(
      path = column_path(tb$path, name, j, positional), name = name, top = if (t == 1) name else tb$top,
      text = NULL, date = FALSE, sas = FALSE, s4 = FALSE, opaque = FALSE, frame = FALSE,
      nested = t > 1, keyed = tb$keyed
    )
    if (isS4(x)) {
      leaf$s4 <- TRUE
    } else if (is.data.frame(x)) {
      leaf$frame <- TRUE
    } else if (inherits(x, "POSIXlt")) {
      leaf$date <- TRUE
    } else if (is.list(x)) {
      content <- tb$lists[[j]]
      leaf$text <- content$text
      leaf$date <- isTRUE(content$has_dates)
      leaf$opaque <- isTRUE(content$opaque)
      leaf$nested <- TRUE
    } else {
      leaf$text <- if (is.factor(x)) {
        levels(x)
      } else if (is.character(x)) {
        unique(as.character(x))
      } else if (inherits(x, "Surv")) {
        # A multi-state Surv's states are its values' labels, like factor levels.
        attr(x, "states", exact = TRUE)
      }
      leaf$date <- inherits(x, c("Date", "POSIXct"))
      leaf$sas <- is.numeric(x) &&
        grepl(sas_date_format_pattern, attr(x, "format.sas", exact = TRUE) %||% "", ignore.case = TRUE)
    }
    leaf
  })
}

# Leaves and tables are cached here while a step runs; see local_text_cache().
text_cache <- new.env(parent = emptyenv())

# Turn on the cache of dataset_tables() until `envir` (the step) exits. `key`
# is the step's key column, which makes nested tables keyed. With
# `positional`, columns inside nested data are named by position ("col[2]").
local_text_cache <- function(envir = parent.frame(), key = NA_character_, positional = FALSE) {
  assign(".active", TRUE, envir = text_cache)
  assign(".key", key, envir = text_cache)
  assign(".positional", positional, envir = text_cache)
  do.call(on.exit, list(quote(rm(list = ls(text_cache, all.names = TRUE), envir = text_cache)), add = TRUE), envir = envir)
}

# Text converted to UTF-8 from its declared encoding (such as latin1). Text
# marked "bytes" is left as it is: it can't be converted or searched.
as_utf8 <- function(x) {
  x <- as.character(x)
  bytes <- Encoding(x) == "bytes"
  if (any(bytes)) {
    x[!bytes] <- repair_utf8(enc2utf8(x[!bytes]))
    return(x)
  }
  repair_utf8(enc2utf8(x))
}

# Text that isn't valid UTF-8 (such as a stray Windows-1252 byte), with the
# invalid bytes written as "<92>", so that it can be searched; a rewrite
# writes back the repaired text.
repair_utf8 <- function(x) {
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) {
    x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
  }
  x
}

# Whether each text (already passed through as_utf8()) can be searched: not
# missing, valid UTF-8 and not marked "bytes".
searchable_text <- function(x) {
  !is.na(x) & Encoding(x) != "bytes" & validUTF8(x)
}

# The distinct texts, among `x`, that can't be searched.
unsearchable_texts <- function(x) {
  if (length(x) == 0) {
    return(character(0))
  }
  x <- as_utf8(x)
  unique(x[!is.na(x) & !searchable_text(x)])
}

# Non-missing values converted to UTF-8 from their declared encoding (such as
# latin1); values that can't be searched (still not valid UTF-8, or marked
# "bytes") are dropped, because regular expressions can't search them.
valid_utf8_text <- function(x) {
  x <- as_utf8(x[!is.na(x)])
  x[searchable_text(x)]
}

# Warn about columns in keyed data that hold dates but were not shifted:
# unselected date columns, and dates kept as text in the mixed columns that
# were shifted (`vars` columns not named *DTC, and QVAL when `vars` is NULL).
# `date_vars` gives the shifted columns of each keyed dataset; those of nested
# data frames in keyed data are selected again by the same rule.
warn_unshifted_iso_dates <- function(data, date_vars, key, vars, locate = identity) {
  hits <- character(0)
  sas_hits <- character(0)
  for (ds in names(data)) {
    tabs <- dataset_tables(data[[ds]])
    for (t in seq_along(tabs)) {
      tb <- tabs[[t]]
      if (!tb$keyed) {
        next
      }
      df <- tb$df
      selected <- if (t == 1) {
        date_vars[[ds]]
      } else {
        dataset_date_vars(df, vars, key, ds, list_dates = tb$list_dates, check = FALSE)
      }
      text_dates <- selected[!tb$list_dates[match(selected, names(df))]]
      mixed <- c(
        text_dates[!grepl("DTC$", text_dates, ignore.case = TRUE, useBytes = TRUE)],
        if (all(c("QNAM", "QVAL") %in% names(df)) && is.null(vars)) "QVAL"
      )
      for (leaf in tb$leaves) {
        where <- location(ds, leaf$path)
        if (leaf$name %in% c(key, selected, mixed)) {
          # List columns' text isn't shifted, only their Date and date-time
          # values.
          unshifted <- if (leaf$name %in% mixed || is.list(df[[leaf$name]])) {
            has_dates_in_values(non_date_values(leaf$text, only_non_dates = leaf$name %in% mixed))
          } else {
            FALSE
          }
          if (unshifted) hits <- c(hits, where)
        } else if (leaf$sas) {
          sas_hits <- c(sas_hits, where)
        } else if (leaf$date || has_dates_in_values(valid_utf8_text(leaf$text))) {
          hits <- c(hits, where)
        }
      }
    }
  }
  hits <- unique(hits)
  if (length(hits) > 0) {
    warning(
      "These columns contain dates that were not shifted: ",
      paste(locate(hits), collapse = ", "),
      ". If they hold ISO 8601 dates, list them in `vars` together with every other date column ",
      "(`vars` replaces the default selection); dates inside free text or in other formats ",
      "(such as 05JAN2014) can't be shifted, so redact or remove them.",
      call. = FALSE
    )
  }
  sas_hits <- unique(sas_hits)
  if (length(sas_hits) > 0) {
    warning(
      "These numeric columns have a SAS date or datetime format and were not shifted: ",
      paste(locate(sas_hits), collapse = ", "),
      ". Convert them to Date or POSIXct (SAS counts from 1960-01-01) and list them in `vars`, or remove them.",
      call. = FALSE
    )
  }
}

# Text values that can be searched, without those recognised as ISO 8601
# dates (which were shifted) when `only_non_dates` is TRUE.
non_date_values <- function(text, only_non_dates = FALSE) {
  values <- valid_utf8_text(text)
  if (only_non_dates && length(values) > 0) {
    values <- values[parse_iso8601_dates(values)$kind == "invalid"]
  }
  values
}

# Paths of the columns of a table (see dataset_tables()), among `cols`, that
# hold dates: Date or POSIXct columns, list columns holding such values,
# numeric columns with a SAS date or datetime format, and text, factor or list
# columns with dates as whole values or inside text.
iso_date_columns <- function(tb, cols) {
  hits <- character(0)
  for (leaf in tb$leaves) {
    if (leaf$name %in% cols && (leaf$date || leaf$sas || has_dates_in_values(valid_utf8_text(leaf$text)))) {
      hits <- c(hits, leaf$path)
    }
  }
  hits
}


# Whether text values hold ISO 8601-like dates as whole values, or dates
# inside text: ISO 8601, with a month name (05JAN2014, 5 Jan 2014, Jan 5,
# 2014, JAN2014) or numeric with separators (01/05/2014, 01.05.2014,
# 2014/01/05).
has_dates_in_values <- function(values) {
  length(values) > 0 && any(
    grepl(iso_date_like_pattern, values, useBytes = TRUE) |
      grepl(embedded_date_pattern, values, useBytes = TRUE) |
      grepl(month_name_date_pattern, values, ignore.case = TRUE, useBytes = TRUE) |
      grepl(slash_date_pattern, values, useBytes = TRUE)
  )
}

iso_date_like_pattern <- "^[0-9]{4}-([0-9]{2}(-[0-9]{2}|--T)|-{2}[0-9]{2})|^[0-9]{4}-[0-9]{2}$"

# A year-month or full date inside text, e.g. "Seen on 2014-01-05".
embedded_date_core <- "[0-9]{4}-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?"
embedded_date_pattern <- paste0("(^|[^0-9-])", embedded_date_core, "([^0-9-]|$)")

month_names_pattern <- paste0(
  "(JAN(UARY)?|FEB(RUARY)?|MAR(CH)?|APR(IL)?|MAY|JUNE?|JULY?|AUG(UST)?|",
  "SEPT?(EMBER)?|OCT(OBER)?|NOV(EMBER)?|DEC(EMBER)?)"
)

# A date with a month name, as a whole value or inside text: "05JAN2014",
# "05-Jan-14", "05/JAN/2014", "UNK-JAN-2014", "5 January 2014",
# "5th January 2014", "Jan 5, 2014", "Jan 2014", "JAN2014", "2014JAN05". With
# spaces as separators a four-digit year is required, so "1 MAY 12 HRS" does
# not match.
month_name_date_core <- local({
  day <- "([0-9]{1,2}(ST|ND|RD|TH)?|UNK?|UN)"
  paste0(
    day, "[-/]?", month_names_pattern, "[-/]?([0-9]{4}|[0-9]{2})",
    "|", day, " ", month_names_pattern, ",? [0-9]{4}",
    "|", month_names_pattern, "\\.? [0-9]{1,2}(ST|ND|RD|TH)?,? [0-9]{4}",
    "|", month_names_pattern, "[- ]?[0-9]{4}",
    "|[0-9]{4}", month_names_pattern, "[0-9]{2}"
  )
})
month_name_date_pattern <- paste0("(^|[^[:alnum:]])(", month_name_date_core, ")([^[:alnum:]]|T[0-9]|$)")

# A numeric date with separators: day, month and a two- or four-digit year
# ("01/05/2014", "1/5/14", "01.05.2014", "01-05-2014"), or a four-digit year
# first ("2014/01/05").
slash_date_core <- paste0(
  "[0-9]{1,2}/[0-9]{1,2}/([0-9]{4}|[0-9]{2})",
  "|[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}",
  "|[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}",
  "|[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}"
)
slash_date_pattern <- paste0("(^|[^0-9/.-])(", slash_date_core, ")([^0-9/.-]|$)")

# The dates found by has_dates_in_values() (ISO 8601 full and year-month dates,
# SDTM partial dates, month-name and numeric forms), anywhere in text, as one
# ICU regular expression whose boundaries are lookarounds, so that a
# replacement keeps the text around each date. Used case-insensitively, for
# month names.
date_in_text_regex <- paste0(
  # SDTM partial dates: year and month with a time ("2014-01--T08:00"; the
  # time is kept), and year and day ("2014---15").
  "(?<![0-9-])[0-9]{4}-(0[1-9]|1[0-2])--(?=T)",
  "|(?<![0-9-])[0-9]{4}---(0[1-9]|[12][0-9]|3[01])(?![0-9-])",
  "|(?<![0-9-])(?:", embedded_date_core, ")(?![0-9-])",
  "|(?<![[:alnum:]])(?:", month_name_date_core, ")(?=[^[:alnum:]]|T[0-9]|$)",
  "|(?<![0-9/.-])(?:", slash_date_core, ")(?![0-9/.-])"
)

# SAS formats that display dates or date-times, or parts of a date: families
# with variants (E8601DA., YYMMDD10., NLDATE., EURDFDD10., ...), and names
# matched exactly, with an optional width, so that user-defined formats such
# as DAYGRP. or YEARCAT. don't match.
sas_date_format_pattern <- paste0(
  "^((E8601D|B8601D|IS8601D|YYMMDD|MMDDYY|DDMMYY|YYMM|MMYY|YYQ|NLDATE|NLDATM|EURDF).*",
  "|(DATE|DATETIME|DATEAMPM|MDYAMPM|DAY|MONTH|YEAR|QTRR?|MONNAME|DOWNAME|WEEKDAY|JULDAY|JULIAN|",
  "MONYY|YYMON|WEEK[UVW]|YYWEEK[UVW]|WEEKDATE|WEEKDATX|WORDDATE|WORDDATX|NENGO|MINGUO|HDATE|",
  "HEBDATE|DTDATE|DTWKDATX|DTYEAR|DTMONYY|DTYYQC)[0-9]*\\.?[0-9]*)$"
)

# Warn about datasets without the key that hold identifiers or dates, which
# pass through unchanged: their columns named like the key or `id_cols` in any
# case, and with `check_dates` their columns holding dates. Inside them,
# nested data frames with the key are de-identified, and only the others are
# checked.
warn_unkeyed_identifiers <- function(data, key, id_cols, check_dates, locate = identity) {
  hits <- character(0)
  nested <- FALSE
  for (ds in names(data)) {
    flagged <- character(0)
    for (tb in dataset_tables(data[[ds]])) {
      if (tb$keyed) {
        next
      }
      before <- length(flagged)
      names_ <- vapply(tb$leaves, `[[`, character(1), "name")
      paths <- vapply(tb$leaves, `[[`, character(1), "path")
      cols <- if (check_dates) dataset_date_vars(tb$df, NULL, key, ds, list_dates = tb$list_dates) else character(0)
      flagged <- c(
        flagged,
        paths[name_key(names_) %in% name_key(c(key, id_cols))],
        paths[match(cols, names_)],
        if (check_dates) iso_date_columns(tb, names_)
      )
      nested <- nested || (nzchar(tb$path) && length(flagged) > before)
    }
    if (length(flagged) > 0) {
      hits <- c(hits, paste0(locate(ds), " (", paste(locate(unique(flagged)), collapse = ", "), ")"))
    }
  }
  if (length(hits) > 0) {
    warning(
      "Datasets without `", key, "`",
      if (nested) ", or data frames nested in them without it,",
      " kept their values unchanged but contain identifiers or dates: ",
      paste(hits, collapse = "; "), ".",
      call. = FALSE
    )
  }
}

# Warn about columns in keyed data named like `key`, `also` or `key_cols` in
# another case (such as `usubjid` beside `USUBJID`), which were not replaced.
warn_id_name_variants <- function(data, id_cols, locate = identity) {
  hits <- character(0)
  for (ds in names(data)) {
    for (leaf in dataset_leaves(data[[ds]])) {
      if (leaf$keyed && name_key(leaf$name) %in% name_key(id_cols) && !leaf$name %in% id_cols) {
        hits <- c(hits, location(ds, leaf$path))
      }
    }
  }
  hits <- unique(hits)
  if (length(hits) > 0) {
    warning(
      "These columns are named like identifier columns in another case and were not replaced: ",
      paste(locate(hits), collapse = ", "),
      ". Add them to `also` or `key_cols`, or remove them.",
      call. = FALSE
    )
  }
}

deid_log_start <- function(step, n_subjects, key, seed, settings = NULL) {
  detail <- paste0(n_subjects, " subject(s) by `", key, "`")
  if (!is.null(settings)) {
    detail <- paste0(detail, "; ", settings)
  }
  if (!is.null(seed)) {
    detail <- paste0(detail, "; seed set, so the result is reproducible from the seed")
  }
  tibble::tibble(
    step = step, dataset = NA_character_, variable = NA_character_,
    n_changed = NA_integer_, n_removed = NA_integer_, detail = detail
  )
}

deid_log_add <- function(log, dataset, variable, detail, n_changed = NA, n_removed = 0) {
  rbind(log, tibble::tibble(
    step = log$step[1], dataset = dataset, variable = variable,
    n_changed = as.integer(n_changed), n_removed = as.integer(n_removed), detail = detail
  ))
}

deparse_list <- function(x) {
  if (length(x) == 0) "none" else paste0("c(", paste0("\"", x, "\"", collapse = ", "), ")")
}

any_numeric_key <- function(data, key) {
  any(vapply(data, function(df) is.numeric(df[[key]]), logical(1)))
}

# Sort rows by the new key so row order no longer follows the original keys.
# The order of rows within a subject, and of rows without a key, is kept. Row
# names are reset (see reset_row_names()).
sort_by_new_key <- function(df, idx, numbers) {
  if (nrow(df) >= 2) {
    ord <- order(numbers[idx], seq_len(nrow(df)), na.last = TRUE)
    df <- vctrs::vec_slice(df, ord)
  }
  # Row names can hold original keys or reveal the original order.
  reset_row_names(df)
}

# A data frame (a tibble too) without row names; with `packed`, its packed
# data frame columns too, whose rows move with its own when it is sorted.
# Automatic row names are left as they are.
reset_row_names <- function(df, packed = TRUE) {
  if (.row_names_info(df) > 0L) {
    attr(df, "row.names") <- .set_row_names(nrow_of(df))
  }
  if (packed) {
    for (j in which(vapply(df, is.data.frame, logical(1)))) {
      df <- set_column(df, j, reset_row_names(.subset2(df, j)))
    }
  }
  df
}

# Whether a data frame's row names are text rather than row positions
# (integer, or text made only of digits).
has_text_row_names <- function(df) {
  rn <- .row_names_info(df, 0L)
  is.character(rn) && !all(grepl("^[0-9]+$", rn))
}

# One offset per subject, drawn uniformly from the whole numbers between
# min_days and max_days in either direction (0 counted once).
draw_offsets <- function(n, max_days, min_days, seed) {
  width <- max_days - min_days + 1
  total <- 2 * width - (min_days == 0)
  idx <- sample_with_seed(total, n, seed = seed)
  ifelse(
    idx <= width,
    min_days - 1 + idx,
    -(min_days - 1 + idx - width + (min_days == 0))
  )
}

# Log the datasets whose rows have no subject; `nested` names those holding
# nested data frames with the key, which were de-identified.
deid_log_unkeyed <- function(log, data, keyed, key, nested = character(0)) {
  for (ds in setdiff(names(data), keyed)) {
    detail <- if (ds %in% nested) {
      paste0("no `", key, "` column; values left unchanged, apart from nested data frames with it")
    } else {
      paste0("no `", key, "` column; values left unchanged")
    }
    log <- deid_log_add(log, ds, NA_character_, detail, 0, 0)
  }
  log
}
