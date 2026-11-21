# Nested data in the subject steps ------------------------------------------
#
# Packed data frame columns and list columns of data frames are de-identified
# like datasets. Each dataset is split into "tables": the dataset itself, each
# packed column, and, for each list column, the data frames it holds (at any
# depth inside plain lists), bound into one table per shape so that each step
# processes them once rather than element by element. A table row's subject
# (its owner) comes from the table's own key column when it has one, and
# otherwise from the row of the parent table it belongs to. After the steps
# have changed the tables, assemble_tables() writes them back into their
# parents.

# Whether `x` is a plain list, which the steps walk into: an unclassed list,
# or one marked only as AsIs or as a vctrs list. Other lists (fitted models,
# recipes, resampling splits, POSIXlt) are objects that can't be
# de-identified.
is_plain_list <- function(x) {
  is.list(x) && !isS4(x) && !is.data.frame(x) &&
    all(oldClass(x) %in% c("AsIs", "list", "vctrs_list_of", "vctrs_vctr"))
}

# Where a column is printed: by name at the top level, and inside nested data
# as "parent[name]", or "parent[2]" with `positional` (for the dates step,
# which can't recognise original keys in nested names once the IDs step ran).
column_path <- function(parent, name, j, positional = FALSE) {
  if (!nzchar(parent)) {
    return(name)
  }
  paste0(parent, "[", if (positional) j else name, "]")
}

# The number of rows of a data frame.
nrow_of <- function(df) .row_names_info(df, 2L)

# What a list column holds, walked one depth at a time through plain lists:
# the data frames and the Date or date-time (POSIXct, POSIXlt) vectors in it,
# each with the row
# of the column it belongs to (`row`) and its position inside that row's
# element (`slot`, empty for the element itself); `text`, the distinct text
# values (character values and factor levels, not names or attributes, which
# the steps rewrite); `has_dates`; and `opaque`, whether it holds objects that
# can't be de-identified (see is_plain_list()). Data frames are not entered:
# they are tables of their own.
list_column_content <- function(x) {
  level <- unclass(x)
  attributes(level) <- NULL
  row <- seq_along(level)
  slot <- rep(list(integer(0)), length(level))
  out <- list(
    frames = list(), frame_row = integer(0), frame_slot = list(),
    dates = list(), date_row = integer(0), date_slot = list(), opaque = FALSE
  )
  text <- list()
  while (length(level) > 0) {
    present <- !vapply(level, is.null, logical(1))
    level <- level[present]
    row <- row[present]
    slot <- slot[present]
    if (length(level) == 0) {
      break
    }
    s4 <- vapply(level, isS4, logical(1))
    df <- !s4 & vapply(level, is.data.frame, logical(1))
    atomic <- !s4 & vapply(level, is.atomic, logical(1))
    lt <- !s4 & vapply(level, inherits, logical(1), what = "POSIXlt")
    plain <- !s4 & !df & vapply(level, is_plain_list, logical(1))
    out$opaque <- out$opaque || any(!(df | atomic | plain | lt))
    is_date <- (atomic & vapply(level, inherits, logical(1), what = c("Date", "POSIXct"))) | lt
    out$frames <- c(out$frames, level[df])
    out$frame_row <- c(out$frame_row, row[df])
    out$frame_slot <- c(out$frame_slot, slot[df])
    out$dates <- c(out$dates, level[is_date])
    out$date_row <- c(out$date_row, row[is_date])
    out$date_slot <- c(out$date_slot, slot[is_date])
    is_factor <- atomic & vapply(level, is.factor, logical(1))
    is_text <- atomic & vapply(level, is.character, logical(1))
    text[[length(text) + 1]] <- unlist(lapply(level[is_factor], levels), use.names = FALSE)
    text[[length(text) + 1]] <- unlist(level[is_text], use.names = FALSE)
    parents <- level[plain]
    n <- lengths(parents)
    level <- unlist(lapply(parents, function(p) {
      attributes(p) <- NULL
      p
    }), recursive = FALSE, use.names = FALSE)
    slot <- unlist(Map(function(s, k) lapply(seq_len(k), function(i) c(s, i)), slot[plain], n), recursive = FALSE)
    row <- rep(row[plain], n)
  }
  out$text <- unique(as.character(unlist(text, use.names = FALSE)))
  out$has_dates <- length(out$dates) > 0
  out
}

# A description of a vector's type and attributes (names and row names
# aside), so that pieces with the same shape can be concatenated without
# loss; NULL when `x` can't be (an S4 object, a matrix, a POSIXlt, or not a
# vector).
piece_shape <- function(x) {
  if (isS4(x) || inherits(x, "POSIXlt")) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    cols <- lapply(x, piece_shape)
    if (any(vapply(cols, is.null, logical(1)))) {
      return(NULL)
    }
    attrs <- attributes(x)
    attrs[c("names", "row.names")] <- NULL
    return(list("data.frame", names(x), attrs, cols))
  }
  if (!is.null(dim(x)) || !(is.atomic(x) || is.list(x))) {
    return(NULL)
  }
  attrs <- attributes(x)
  attrs$names <- NULL
  list(typeof(x), attrs)
}

# Pieces of one shape (see piece_shape()) concatenated: their values, with
# the attributes of the first and the names of all. A data frame's columns
# are concatenated in turn.
bind_pieces <- function(pieces) {
  first <- pieces[[1]]
  if (length(pieces) == 1) {
    return(first)
  }
  if (is.data.frame(first)) {
    out <- lapply(seq_along(first), function(j) bind_pieces(lapply(pieces, .subset2, j)))
    attributes(out) <- attributes(first)
    attr(out, "row.names") <- .set_row_names(sum(vapply(pieces, nrow_of, integer(1))))
    return(out)
  }
  values <- unlist(lapply(pieces, function(p) {
    attributes(p) <- NULL
    p
  }), recursive = FALSE, use.names = FALSE)
  if (is.null(values)) {
    values <- vector(typeof(first), 0)
  }
  attrs <- attributes(first)
  attrs$names <- NULL
  named <- !vapply(pieces, function(p) is.null(names(p)), logical(1))
  if (any(named)) {
    attrs$names <- unlist(lapply(pieces, function(p) if (is.null(names(p))) rep("", length(p)) else names(p)), use.names = FALSE)
  }
  attributes(values) <- attrs
  values
}

# A function that takes rows of a concatenated vector or data frame (made by
# bind_pieces()) with its attributes, stripping it once rather than for every
# piece.
piece_slicer <- function(v) {
  if (is.data.frame(v)) {
    slicers <- lapply(v, piece_slicer)
    attrs <- attributes(v)
    return(function(rows) {
      x <- lapply(slicers, function(s) s(rows))
      attributes(x) <- attrs
      attr(x, "row.names") <- .set_row_names(length(rows))
      x
    })
  }
  attrs <- attributes(v)
  nm <- attrs$names
  attrs$names <- NULL
  data <- v
  attributes(data) <- NULL
  function(rows) {
    out <- data[rows]
    attributes(out) <- c(attrs, if (!is.null(nm)) list(names = nm[rows]))
    out
  }
}

# Pieces grouped by shape, each group's pieces concatenated with
# `bind`; returns, per group, the members' positions in `pieces`, their
# sizes and the concatenation.
group_pieces <- function(pieces, shape, bind) {
  if (length(pieces) == 0) {
    return(list())
  }
  shapes <- lapply(pieces, shape)
  # Pieces that can't be concatenated are groups of their own.
  sig <- vapply(seq_along(pieces), function(i) {
    if (is.null(shapes[[i]])) paste0("#", i) else rlang::hash(shapes[[i]])
  }, character(1))
  groups <- split(seq_along(pieces), factor(sig, levels = unique(sig)))
  lapply(unname(groups), function(g) list(members = g, bound = bind(pieces[g])))
}

# The shape of a data frame held in a list column, by its columns only (its
# own class and attributes are kept on each element when it is written back).
frame_shape <- function(df) {
  cols <- lapply(df, piece_shape)
  if (any(vapply(cols, is.null, logical(1)))) {
    return(NULL)
  }
  list(names(df), cols)
}

# A data frame's columns as a plain data frame, with names marked "bytes"
# marked "unknown" so that they can be looked up (the original keeps its
# names). Names must be unique, as at the top level; `where` locates the data
# frame in the error, by position.
plain_frame <- function(df, where) {
  cols <- unclass(df)
  attributes(cols) <- NULL
  nm <- names(df)
  bytes <- Encoding(nm) == "bytes"
  Encoding(nm)[bytes] <- "unknown"
  if (anyDuplicated(nm)) {
    stop(
      "Column names inside nested data frames must be unique; in ", where, ", column ",
      which(duplicated(nm))[1], " repeats an earlier name.",
      call. = FALSE
    )
  }
  names(cols) <- nm
  vctrs::new_data_frame(cols, n = nrow_of(df))
}

# Data frames held in a list column bound into one plain data frame per shape.
bind_frames <- function(frames, where) {
  group_pieces(frames, frame_shape, function(pieces) {
    first <- plain_frame(pieces[[1]], where)
    if (length(pieces) == 1) {
      return(first)
    }
    cols <- lapply(seq_along(first), function(j) bind_pieces(lapply(pieces, .subset2, j)))
    names(cols) <- names(first)
    vctrs::new_data_frame(cols, n = sum(vapply(pieces, nrow_of, integer(1))))
  })
}

# The tables of a dataset (see the top of this file): a list whose first
# element is the dataset itself. Each table has `df` (the dataset, or a plain
# data frame of a packed column or of the data frames of one shape in a list
# column), `kind` ("top", "packed" or "list"), `path` (where it is printed, ""
# for the dataset: "col" for a packed column, "col[[]]" for the data frames in
# list column `col`, deeper "col[[]][sub]"), `pos` (the same by column
# position, "[2][[]]", for errors, which can't print names before keys are
# known), `top` (the dataset's column it is in), `parent` (the index of its
# parent table), `column` (its column in the parent), `parent_row` (the
# parent's row of each of its rows), `piece` (for each row, which of the data
# frames it was made of the row comes from), `members` (those data frames:
# each `template` with the `row` and `slot` where it sits in the parent's
# column and its `rows` in the table), `has_key` (it has the key column),
# `owners` and `owns` (see resolve_owners()), `keyed` (its rows have subjects,
# its own or those of the rows they belong to), `lists` (by column position,
# what each list column holds, see list_column_content()) and `list_dates`
# (by column position, whether a list column holds dates). `dataset` (the
# dataset's position) is used in errors.
nested_tables <- function(df, key, positional = FALSE, dataset = NULL) {
  new_table <- function(df, kind, path, pos, top, parent) {
    list(
      df = df, kind = kind, path = path, pos = pos, top = top, parent = parent,
      has_key = !is.na(key) && key %in% names(df),
      lists = list(), list_dates = logical(length(df)), changed = integer(0)
    )
  }
  where <- function(pos) paste0(if (!is.null(dataset)) paste0("dataset ", dataset, " of `data`, at "), "`", pos, "`")
  tables <- list(new_table(df, "top", "", "", NULL, 0L))
  tables[[1]]$piece <- rep(1L, nrow_of(df))
  t <- 1L
  while (t <= length(tables)) {
    tb <- tables[[t]]
    x <- tb$df
    for (j in seq_along(x)) {
      col <- .subset2(x, j)
      if (isS4(col) || !is.list(col) || inherits(col, "POSIXlt")) {
        next
      }
      path <- column_path(tb$path, names(x)[j], j, positional)
      pos <- paste0(tb$pos, "[", j, "]")
      top <- if (is.null(tb$top)) names(x)[j] else tb$top
      if (is.data.frame(col)) {
        child <- new_table(plain_frame(col, where(pos)), "packed", path, pos, top, t)
        child$column <- j
        child$parent_row <- seq_len(nrow_of(col))
        child$piece <- tb$piece
        child$members <- list(list(template = col, rows = NULL))
        tables <- c(tables, list(child))
        next
      }
      if (!is_plain_list(col)) {
        tables[[t]]$lists[[j]] <- list(text = NULL, opaque = TRUE, has_dates = FALSE)
        next
      }
      content <- list_column_content(col)
      tables[[t]]$list_dates[j] <- content$has_dates
      list_path <- paste0(path, "[[]]")
      list_pos <- paste0(pos, "[[]]")
      for (g in bind_frames(content$frames, where(list_pos))) {
        sizes <- vapply(content$frames[g$members], nrow_of, integer(1))
        ends <- cumsum(sizes)
        child <- new_table(g$bound, "list", list_path, list_pos, top, t)
        child$column <- j
        child$parent_row <- rep(content$frame_row[g$members], sizes)
        child$piece <- rep(seq_along(g$members), sizes)
        single <- length(g$members) == 1
        child$members <- lapply(seq_along(g$members), function(k) {
          m <- g$members[[k]]
          list(
            template = content$frames[[m]], row = content$frame_row[[m]], slot = content$frame_slot[[m]],
            rows = if (!single) seq.int(ends[[k]] - sizes[[k]] + 1L, length.out = sizes[[k]])
          )
        })
        tables <- c(tables, list(child))
      }
      content$frames <- NULL
      tables[[t]]$lists[[j]] <- content
    }
    t <- t + 1L
  }
  resolve_owners(tables, key)
}

# Who owns the rows of each table. A table owns its rows (`owns`, with
# `owners`, each row's normalized key) when it has the key column; when one
# of its packed columns does (each packed row belongs to its outer row, so the
# outer row has that subject); or, when its rows don't already belong to rows
# of a parent table with subjects, when the data frames in one of its list
# columns hold one subject per row (such as data split by subject), each row
# then having that subject. Otherwise its rows belong to the rows of its
# parent table, if those have subjects (`keyed`).
resolve_owners <- function(tables, key) {
  n <- length(tables)
  source <- character(n)
  owners <- vector("list", n)
  parents <- vapply(tables, function(x) as.integer(x$parent), integer(1))
  kinds <- vapply(tables, `[[`, character(1), "kind")
  child_of <- split(seq_len(n), factor(parents, levels = seq_len(n)))
  for (t in rev(seq_len(n))) {
    tb <- tables[[t]]
    children <- child_of[[t]]
    if (tb$has_key) {
      owners[[t]] <- normalize_keys(tb$df[[key]])
      source[t] <- "own"
      next
    }
    packed <- children[kinds[children] == "packed" & source[children] %in% c("own", "packed")]
    if (length(packed) > 0) {
      owners[[t]] <- owners[[packed[1]]]
      source[t] <- "packed"
      next
    }
    listed <- children[kinds[children] == "list" & nzchar(source[children])]
    for (j in unique(vapply(tables[listed], `[[`, integer(1), "column"))) {
      from <- listed[vapply(tables[listed], function(x) x$column == j, logical(1))]
      row <- unlist(lapply(tables[from], `[[`, "parent_row"), use.names = FALSE)
      k <- unlist(owners[from], use.names = FALSE)
      pairs <- unique(data.frame(row = row, k = k)[!is.na(k), , drop = FALSE])
      if (nrow(pairs) > 0 && !anyDuplicated(pairs$row)) {
        owner <- rep(NA_character_, nrow_of(tb$df))
        owner[pairs$row] <- pairs$k
        owners[[t]] <- owner
        source[t] <- "list"
        break
      }
    }
  }
  for (t in seq_len(n)) {
    parent_keyed <- tables[[t]]$parent > 0 && tables[[tables[[t]]$parent]]$keyed
    owns <- source[t] %in% c("own", "packed") || (source[t] == "list" && !parent_keyed)
    tables[[t]]$owns <- owns
    tables[[t]]$owners <- if (owns) owners[[t]]
    tables[[t]]$keyed <- owns || parent_keyed
  }
  tables
}

# Each row's subject, as its index in `keys`: its table's own (see
# resolve_owners()), or that of the row of its parent table it belongs to.
table_owner <- function(tables, t, keys, key) {
  tb <- tables[[t]]
  if (tb$owns) {
    return(match(tb$owners, keys))
  }
  tables[[tb$parent]]$idx[tb$parent_row]
}

# The missing value for each row of text column `x` of a table, as
# missing_text_like() chooses it, for each data frame the table was made of.
table_missing <- function(tb, x) {
  x <- as.character(if (inherits(x, "haven_labelled")) vctrs::vec_data(x) else x)
  if (length(tb$piece) == 0 || all(tb$piece == tb$piece[1])) {
    return(missing_text_like(x))
  }
  blank <- tapply(!is.na(x) & x == "", tb$piece, any)
  ifelse(blank[as.character(tb$piece)], "", NA_character_)
}

# The data frames that hold the key column, at any depth, named by where they
# are: the datasets first, then nested ones (by position, "ds[2][[]]", as
# their names can hold keys). Several may have one name.
key_frames <- function(tables, key) {
  out <- list()
  labels <- character(0)
  for (ds in names(tables)) {
    for (tb in tables[[ds]]) {
      if (tb$has_key) {
        out <- c(out, list(tb$df))
        labels <- c(labels, paste0(ds, tb$pos))
      }
    }
  }
  names(out) <- labels
  out
}

# A data frame with column `j` set to `value`, keeping its class and
# attributes (methods such as those of grouped tibbles are not called).
set_column <- function(df, j, value) {
  cls <- oldClass(df)
  x <- unclass(df)
  x[[j]] <- value
  class(x) <- cls
  x
}

# A data frame held in nested data (`template`) with its columns `changed`
# taken from a processed table: the whole column, or `rows` of it.
restore_frame <- function(template, slicers, bound, rows, changed) {
  cls <- oldClass(template)
  x <- unclass(template)
  for (j in changed) {
    x[[j]] <- if (is.null(rows)) .subset2(bound, j) else slicers[[as.character(j)]](rows)
  }
  class(x) <- cls
  x
}

# Write each changed table back into its parent, deepest first, and return
# the dataset. Tables marked `touch` are written back even when unchanged.
# `finish(df, table, member)` is applied to each data frame written back (the
# IDs step sorts data frames in list columns that own their rows by the new
# key, and resets row names). A grouped or rowwise data frame whose grouping
# columns changed is regrouped. The tables of one list column (one per shape)
# are written back together, so that the column is copied once. A vctrs
# list_of column's prototype follows its new elements (see
# with_elements_ptype()).
assemble_tables <- function(tables, finish = function(df, tb, m) df) {
  restore <- function(tb, m, slicers) {
    e <- restore_frame(m$template, slicers, tb$df, m$rows, tb$changed)
    e <- finish(e, tb, m)
    if (inherits(e, c("grouped_df", "rowwise_df")) &&
      any(names(e)[tb$changed] %in% names(attr(e, "groups", exact = TRUE)))) {
      e <- vctrs::vec_slice(e, seq_len(nrow_of(e)))
    }
    e
  }
  t <- length(tables)
  while (t > 1) {
    # The run of tables made from one column of one parent.
    first <- t
    while (first > 2 && tables[[first - 1]]$parent == tables[[t]]$parent &&
      identical(tables[[first - 1]]$column, tables[[t]]$column)) {
      first <- first - 1
    }
    run <- Filter(function(tb) length(tb$changed) > 0 || isTRUE(tb$touch), tables[first:t])
    t <- first - 1
    if (length(run) == 0) {
      next
    }
    p <- run[[1]]$parent
    j <- run[[1]]$column
    parent <- tables[[p]]
    if (run[[1]]$kind == "packed") {
      col <- restore(run[[1]], run[[1]]$members[[1]], list())
    } else {
      col <- .subset2(parent$df, j)
      attrs <- attributes(col)
      x <- unclass(col)
      for (tb in run) {
        slicers <- lapply(stats::setNames(tb$changed, tb$changed), function(k) piece_slicer(.subset2(tb$df, k)))
        for (m in tb$members) {
          x[[c(m$row, m$slot)]] <- restore(tb, m, slicers)
        }
      }
      attributes(x) <- attrs
      col <- with_elements_ptype(x)
    }
    parent$df <- set_column(parent$df, j, col)
    parent$changed <- union(parent$changed, j)
    tables[[p]] <- parent
  }
  tables[[1]]$df
}

# A vctrs list_of column's prototype for its new elements: found as
# vctrs::list_of() finds it, keeping the attributes of the old prototype's
# columns that don't define their type (such as labels, which tidyr::unnest()
# gives the unnested columns). The old prototype is kept when the elements
# still match it, and when they have none in common.
with_elements_ptype <- function(x) {
  if (!inherits(x, "vctrs_list_of")) {
    return(x)
  }
  elements <- unname(Filter(Negate(is.null), unclass(x)))
  old <- attr(x, "ptype", exact = TRUE)
  if (length(elements) == 0) {
    return(x)
  }
  ptype <- tryCatch(do.call(vctrs::vec_ptype_common, elements), error = function(e) NULL)
  if (!is.null(ptype)) {
    ptype <- with_ptype_attributes(ptype, old)
    if (!identical(ptype, old)) attr(x, "ptype") <- ptype
  }
  x
}

# `new` given the attributes of `old` that aren't structural (see
# structural_attributes()) and that it lacks, and, for data frames, the same
# for each column found in both by name.
with_ptype_attributes <- function(new, old) {
  if (is.null(old)) {
    return(new)
  }
  if (is.data.frame(new) && is.data.frame(old)) {
    for (j in which(names(new) %in% names(old))) {
      new <- set_column(new, j, with_ptype_attributes(.subset2(new, j), .subset2(old, names(new)[j])))
    }
  }
  extra <- setdiff(names(attributes(old)), c(structural_attributes(old), names(attributes(new))))
  for (name in extra) {
    attr(new, name) <- attr(old, name, exact = TRUE)
  }
  new
}

# Shift the Date and date-time vectors held in list column `j` of a table
# (see list_column_content()) by their row's offset, concatenated by shape and
# shifted in one call per shape. Returns the column and the counts, as
# shift_dates_by_subject() does.
shift_list_dates <- function(col, content, row_offset, partial) {
  counts <- list(shifted = 0, no_key = 0, invalid = 0, partial = 0, kept_years = 0)
  attrs <- attributes(col)
  x <- unclass(col)
  for (g in group_pieces(content$dates, piece_shape, bind_pieces)) {
    sizes <- lengths(content$dates[g$members])
    offset <- rep(row_offset[content$date_row[g$members]], sizes)
    shifted <- shift_dates_by_subject(g$bound, offset, partial)
    for (field in names(counts)) {
      counts[[field]] <- counts[[field]] + shifted[[field]]
    }
    if (length(g$members) == 1) {
      m <- g$members[[1]]
      x[[c(content$date_row[[m]], content$date_slot[[m]])]] <- shifted$values
      next
    }
    slice <- piece_slicer(shifted$values)
    ends <- cumsum(sizes)
    for (k in seq_along(g$members)) {
      m <- g$members[[k]]
      x[[c(content$date_row[[m]], content$date_slot[[m]])]] <- slice(seq.int(ends[[k]] - sizes[[k]] + 1L, length.out = sizes[[k]]))
    }
  }
  attributes(x) <- attrs
  c(list(values = x), counts)
}

# Log rows of a step for one dataset, summed over the tables printed at the
# same column path: `add(variable, kind, counts)` adds counts (a named list of
# numbers); `rows()` returns, in the order first seen, each variable, kind and
# summed counts.
log_tally <- function() {
  entries <- list()
  list(
    add = function(variable, kind, counts) {
      id <- paste(variable, kind, sep = "\r")
      if (is.null(entries[[id]])) {
        entries[[id]] <<- list(variable = variable, kind = kind, counts = counts)
      } else {
        for (field in names(counts)) {
          entries[[id]]$counts[[field]] <<- entries[[id]]$counts[[field]] + counts[[field]]
        }
      }
    },
    rows = function() unname(entries)
  )
}

# The printed path of column `j` of a table.
table_column_path <- function(tb, j, positional = FALSE) {
  column_path(tb$path, names(tb$df)[j], j, positional)
}
