#' Legacy deserialization for old block formats
#'
#' Converts old serialized block payloads (individual parameters) to the
#' new single-state format. This allows boards saved with old blockr.dplyr
#' versions to be restored.
#'
#' Drop this file when backwards compatibility is no longer needed.
#'
#' @name legacy-deser
#' @keywords internal
NULL

# Helper: convert old payload to new state-based args and call constructor
legacy_deser_block <- function(data, ctor_name, state_builder) {
  ctor <- blockr_deser(data[["constructor"]])
  payload <- data[["payload"]]

  # If payload already has "state", it's the new format — pass through
  if ("state" %in% names(payload)) {
    args <- c(payload, list(
      ctor = coal(ctor_name(ctor), ctor_name),
      ctor_pkg = ctor_pkg(ctor)
    ))
    return(do.call(ctor_name, args))
  }

  # Old format: convert individual params to state
  state <- state_builder(payload)
  args <- list(
    state = state,
    ctor = coal(ctor_name(ctor), ctor_name),
    ctor_pkg = ctor_pkg(ctor)
  )
  do.call(ctor_name, args)
}

# --- Filter (old: conditions + preserve_order) ---

#' @export
blockr_deser.filter_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_filter_block", function(p) {
    list(
      conditions = lapply(p$conditions %||% list(), function(c) {
        # Old format had column + values + mode, convert to type = "values"
        list(
          type = "values",
          column = c$column %||% "",
          values = c$values %||% list(),
          mode = c$mode %||% "include"
        )
      }),
      operator = p$operator %||% "&"
    )
  })
}

# --- Filter expr (old: exprs as string) ---

#' @export
blockr_deser.filter_expr_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_filter_block", function(p) {
    expr_str <- p$exprs %||% "TRUE"
    list(
      conditions = list(list(type = "expr", expr = expr_str)),
      operator = "&"
    )
  })
}

# --- Mutate expr (old: exprs as named list + by) ---

#' @export
blockr_deser.mutate_expr_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_mutate_block", function(p) {
    exprs <- p$exprs %||% list()
    rows <- mapply(function(nm, ex) list(name = nm, expr = ex),
                   names(exprs), unname(exprs), SIMPLIFY = FALSE,
                   USE.NAMES = FALSE)
    list(rows = rows, by = p$by %||% list())
  })
}

# --- Summarize (old no-code: summaries as named list + by) ---

#' @export
blockr_deser.summarize_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_summarize_block", function(p) {
    if (!is.null(p$summaries) && is.list(p$summaries)) {
      # Old no-code format: named list of list(func, col)
      summaries <- mapply(function(nm, s) {
        list(type = "simple", name = nm,
             func = s$func %||% "mean", col = s$col %||% "")
      }, names(p$summaries), p$summaries, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    } else {
      summaries <- list()
    }
    list(summaries = summaries, by = p$by %||% list())
  })
}

# --- Summarize expr (old: exprs as named list + by + unpack) ---

#' @export
blockr_deser.summarize_expr_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_summarize_block", function(p) {
    exprs <- p$exprs %||% list()
    summaries <- mapply(function(nm, ex) list(type = "expr", name = nm, expr = ex),
                        names(exprs), unname(exprs), SIMPLIFY = FALSE,
                        USE.NAMES = FALSE)
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
      by = p$by %||% list()
    )
  })
}

# --- Join (old: type + by as simple column list) ---

#' @export
blockr_deser.join_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_join_block", function(p) {
    # Old format: by = c("col1", "col2") — convert to keys with op = "=="
    by <- p$by %||% list()
    keys <- lapply(by, function(col) {
      if (is.list(col) && !is.null(col$xCol)) {
        col  # Already new format
      } else {
        list(xCol = as.character(col), op = "==", yCol = as.character(col))
      }
    })
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
      names_prefix = p$names_prefix %||% ""
    )
  })
}

# --- Unite (old: individual params) ---

#' @export
blockr_deser.unite_block <- function(x, data, ...) {
  legacy_deser_block(data, "new_unite_block", function(p) {
    list(
      col = p$col %||% "united",
      cols = p$cols %||% list(),
      sep = p$sep %||% "_",
      remove = p$remove %||% TRUE,
      na.rm = p$na.rm %||% FALSE
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
