#' Legacy deserialization for old block formats
#'
#' Restores boards saved by older blockr.dplyr versions. Two historical
#' formats are handled:
#'
#' 1. **Individual-parameter format** (oldest): each state field was a
#'    top-level payload entry, sometimes under different names or shapes
#'    (e.g. filter conditions without a `type`, summarize `summaries` as a
#'    named list). The per-block `state_builder` normalizes these to the
#'    current flat field set.
#' 2. **Single-`state` blob format** (interim): the whole state was nested
#'    under one `state` payload key. This is unwrapped back to flat fields.
#'
#' The current format already serializes flat top-level fields that map
#' straight onto the constructors' flat arguments, so those pass through the
#' (idempotent) builders unchanged.
#'
#' Drop this file when backwards compatibility is no longer needed.
#'
#' @name legacy-deser
#' @keywords internal
NULL

# Convert an old payload to the constructor's flat arguments and build the
# block. `state_builder` normalizes the individual-parameter format and must
# be idempotent on already-flat (current) payloads. The single-`state` blob
# is unwrapped directly (its contents already use the modern field shapes).
# The original class from the JSON is restored so blockr.core's class check
# passes.
legacy_deser_block <- function(data, ctor_name, state_builder = identity) {
  # Old constructor may no longer exist (e.g. new_filter_expr_block ->
  # new_filter_block)
  ctor <- tryCatch(
    blockr_deser(data[["constructor"]]),
    error = function(e) NULL
  )
  payload <- data[["payload"]]
  orig_class <- data[["object"]]

  flat <- if ("state" %in% names(payload)) {
    # Interim single-blob format: contents already use modern field shapes.
    payload[["state"]]
  } else {
    # Individual-parameter (oldest) or current flat format.
    state_builder(payload)
  }

  # Block attributes (`block_name`, ...) are serialized as sibling payload
  # entries next to the state fields; carry them through to the constructor
  # so e.g. custom block names survive restore.
  extras <- payload[setdiff(names(payload), c(names(flat), "state"))]

  args <- c(flat, extras, list(
    ctor = if (!is.null(ctor)) coal(ctor_name(ctor), ctor_name) else ctor_name,
    ctor_pkg = if (!is.null(ctor)) ctor_pkg(ctor) else utils::packageName()
  ))
  res <- do.call(ctor_name, args)

  # Restore original class so blockr.core's class check passes
  if (!is.null(orig_class) && !identical(class(res), orig_class)) {
    class(res) <- orig_class
  }
  res
}

# --- Filter (old: conditions without type + operator) ---

#' @export
blockr_deser.filter_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_filter_block", function(p) {
    list(
      conditions = lapply(p$conditions %||% list(), function(c) {
        # Modern conditions already carry a `type`; leave them untouched.
        if (!is.null(c$type)) return(c)
        # Old format had column + values + mode -> type = "values"
        list(
          type = "values",
          column = c$column %||% "",
          values = c$values %||% list(),
          mode = c$mode %||% "include"
        )
      }),
      operator = p$operator %||% "&",
      preserve_order = isTRUE(p$preserve_order)
    )
  })
}

# --- Filter expr (old: exprs as string) ---

#' @export
blockr_deser.filter_expr_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_filter_block", function(p) {
    list(
      conditions = list(list(type = "expr", expr = p$exprs %||% "TRUE")),
      operator = "&",
      preserve_order = FALSE
    )
  })
}

# --- Mutate (interim single-state blob; flat passthrough) ---

#' @export
blockr_deser.mutate_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_mutate_block", function(p) {
    list(
      mutations = p$mutations %||% list(),
      by = p$by %||% list()
    )
  })
}

# --- Mutate expr (old: exprs as named list + by) ---

#' @export
blockr_deser.mutate_expr_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_mutate_block", function(p) {
    exprs <- p$exprs %||% list()
    mutations <- mapply(function(nm, ex) list(name = nm, expr = ex),
                        names(exprs), unname(exprs), SIMPLIFY = FALSE,
                        USE.NAMES = FALSE)
    list(mutations = mutations, by = p$by %||% list())
  })
}

# --- Summarize (old no-code: summaries as named list + by) ---

#' @export
blockr_deser.summarize_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_summarize_block", function(p) {
    summaries <- p$summaries %||% list()
    # Old no-code format: a NAMED list of list(func, col). Modern format is
    # an unnamed array of records each carrying `name`/`type`.
    is_old_named <- length(summaries) &&
      !is.null(names(summaries)) && any(nzchar(names(summaries)))
    if (is_old_named) {
      summaries <- mapply(function(nm, s) {
        list(type = "simple", name = nm,
             func = s$func %||% "mean", col = s$col %||% "")
      }, names(summaries), summaries, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    }
    list(summaries = summaries, by = p$by %||% list())
  })
}

# --- Summarize expr (old: exprs as named list + by) ---

#' @export
blockr_deser.summarize_expr_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_summarize_block", function(p) {
    exprs <- p$exprs %||% list()
    summaries <- mapply(
      function(nm, ex) list(type = "expr", name = nm, expr = ex),
      names(exprs), unname(exprs),
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
    list(summaries = summaries, by = p$by %||% list())
  })
}

# --- Select (old: columns + exclude + distinct) ---

#' @export
blockr_deser.select_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_select_block", function(p) {
    list(
      columns = p$columns %||% list(),
      exclude = isTRUE(p$exclude),
      distinct = isTRUE(p$distinct)
    )
  })
}

# --- Arrange (old: columns as list of column+direction) ---

#' @export
blockr_deser.arrange_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_arrange_block", function(p) {
    list(columns = p$columns %||% list())
  })
}

# --- Rename (old: renames as named list) ---

#' @export
blockr_deser.rename_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_rename_block", function(p) {
    list(renames = p$renames %||% list())
  })
}

# --- Slice (old: individual params) ---

#' @export
blockr_deser.slice_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_slice_block", function(p) {
    list(
      type = p$type %||% "head",
      n = p$n %||% 5L,
      prop = p$prop,
      order_by = p$order_by %||% "",
      with_ties = p$with_ties %||% TRUE,
      weight_by = p$weight_by %||% "",
      replace = isTRUE(p$replace),
      rows = p$rows %||% "1:5",
      by = p$by %||% list()
    )
  })
}

# --- Join (old: by as a simple column list; modern: keys) ---

#' @export
blockr_deser.join_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_join_block", function(p) {
    # Modern payloads carry `keys` directly; only derive them from the old
    # `by` column list when `keys` is absent.
    keys <- p$keys
    if (is.null(keys)) {
      keys <- lapply(p$by %||% list(), function(col) {
        if (is.list(col) && !is.null(col$xCol)) {
          col  # already key-shaped
        } else {
          list(xCol = as.character(col), op = "==", yCol = as.character(col))
        }
      })
    }
    list(
      type = p$type %||% "left_join",
      keys = keys,
      exprs = p$exprs %||% list(),
      suffix_x = p$suffix_x %||% ".x",
      suffix_y = p$suffix_y %||% ".y"
    )
  })
}

# --- Bind rows (old: id_name) ---

#' @export
blockr_deser.bind_rows_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_bind_rows_block", function(p) {
    list(id_name = p$id_name %||% "")
  })
}

# --- Pivot longer (old: individual params) ---

#' @export
blockr_deser.pivot_longer_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_pivot_longer_block", function(p) {
    list(
      cols = p$cols %||% list(),
      names_to = p$names_to %||% "name",
      values_to = p$values_to %||% "value",
      values_drop_na = isTRUE(p$values_drop_na),
      names_prefix = p$names_prefix %||% ""
    )
  })
}

# --- Pivot wider (old: individual params) ---

#' @export
blockr_deser.pivot_wider_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_pivot_wider_block", function(p) {
    list(
      names_from = p$names_from %||% list(),
      values_from = p$values_from %||% list(),
      id_cols = p$id_cols %||% list(),
      values_fill = p$values_fill,
      names_sep = p$names_sep %||% "_",
      names_prefix = p$names_prefix %||% "",
      values_fn = p$values_fn %||% ""
    )
  })
}

# --- Unite (old: na.rm; modern: na_rm) ---

#' @export
blockr_deser.unite_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_unite_block", function(p) {
    list(
      col = p$col %||% "united",
      cols = p$cols %||% list(),
      sep = p$sep %||% "_",
      remove = p$remove %||% TRUE,
      na_rm = p$na_rm %||% p$na.rm %||% FALSE
    )
  })
}

# --- Separate (old: individual params) ---

#' @export
blockr_deser.separate_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_separate_block", function(p) {
    list(
      col = p$col %||% "",
      into = p$into %||% list(),
      sep = p$sep %||% "[^[:alnum:]]+",
      remove = p$remove %||% TRUE,
      convert = isTRUE(p$convert),
      extra = p$extra %||% "warn",
      fill = p$fill %||% "warn"
    )
  })
}
