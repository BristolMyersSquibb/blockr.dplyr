#' Expression builders for JS-driven blocks
#'
#' These functions build proper R language objects using bbquote() from
#' blockr.core, replacing the old glue/parse/paste pattern.
#'
#' @name expr-builders
#' @keywords internal
NULL

#' Build a dplyr::filter expression from JS conditions
#'
#' @param conditions List of condition objects from JS. Each has a `type`
#'   ("values", "numeric", or "expr") and type-specific fields.
#' @param operator Global operator: "&" or "|"
#' @return A language object like `dplyr::filter(.(data), ...)`
#' @noRd
make_filter_expr <- function(conditions,
                             operator = "&",
                             preserve_order = FALSE) {
  if (length(conditions) == 0) {
    return(bbquote(dplyr::filter(.(data), TRUE)))
  }

  parts <- lapply(conditions, make_filter_part)
  parts <- Filter(Negate(is.null), parts)

  if (length(parts) == 0) {
    return(bbquote(dplyr::filter(.(data), TRUE)))
  }

  combined <- Reduce(
    function(a, b) call(operator, a, b),
    parts
  )

  filter_expr <- bbquote(
    dplyr::filter(.(data), .(combined)),
    list(combined = combined)
  )

  if (!isTRUE(preserve_order)) return(filter_expr)

  # Build arrange(match(col, c(v1, v2, ...))) for pick order
  val_cond <- Filter(
    function(c) {
      identical(c$type, "values") && length(c$values) > 0
    },
    conditions
  )
  if (length(val_cond) == 0) return(filter_expr)

  vc <- val_cond[[1]]
  col_sym <- as.name(vc$column)
  vals <- unlist(vc$values)
  vals <- vals[!vals %in% c("<NA>", "<empty>")]
  if (length(vals) == 0) return(filter_expr)

  nums <- suppressWarnings(as.numeric(vals))
  val_vec <- if (all(!is.na(nums))) nums else vals

  match_expr <- call("match", col_sym, val_vec)
  as.call(list(
    str2lang("dplyr::arrange"),
    filter_expr,
    match_expr
  ))
}

#' Dispatch a single filter condition to the appropriate builder
#' @noRd
make_filter_part <- function(cond) {
  switch(cond$type,
    "values" = make_values_part(cond),
    "numeric" = make_numeric_part(cond),
    "expr" = make_expr_part(cond),
    NULL
  )
}

#' Build filter part for a "values" condition (%in%)
#' @noRd
make_values_part <- function(cond) {
  column <- cond$column
  values <- unlist(cond$values)
  mode <- cond$mode %||% "include"

  if (is.null(column) || is.null(values) || length(values) == 0) {
    return(NULL)
  }

  has_na <- "<NA>" %in% values
  has_empty <- "<empty>" %in% values
  regular <- values[!values %in% c("<NA>", "<empty>")]

  col_sym <- as.name(column)
  include <- identical(mode, "include")
  parts <- list()

  if (length(regular) > 0) {
    # Attempt numeric coercion
    nums <- suppressWarnings(as.numeric(regular))
    val_vec <- if (all(!is.na(nums))) nums else regular

    in_expr <- call("%in%", col_sym, val_vec)
    if (!include) in_expr <- call("!", in_expr)
    parts <- c(parts, list(in_expr))
  }

  if (has_na) {
    na_expr <- call("is.na", col_sym)
    if (!include) na_expr <- call("!", na_expr)
    parts <- c(parts, list(na_expr))
  }

  if (has_empty) {
    empty_expr <- if (include) {
      call("==", col_sym, "")
    } else {
      call("!=", col_sym, "")
    }
    parts <- c(parts, list(empty_expr))
  }

  if (length(parts) == 0) return(NULL)
  if (length(parts) == 1) return(parts[[1]])

  combine_op <- if (include) "|" else "&"
  Reduce(function(a, b) call(combine_op, a, b), parts)
}

#' Build filter part for a "numeric" condition (comparison operator)
#' @noRd
make_numeric_part <- function(cond) {
  column <- cond$column
  op <- cond$op
  value <- cond$value

  if (is.null(column) || is.null(op) || is.null(value)) return(NULL)

  r_op <- switch(op,
    "is" = "==",
    "is not" = "!=",
    ">=" = ">=",
    "<=" = "<=",
    ">" = ">",
    "<" = "<",
    "\u2265" = ">=",
    "\u2264" = "<=",
    "\u2260" = "!=",
    op
  )

  call(r_op, as.name(column), as.numeric(value))
}

#' Build filter part for an "expr" condition (user-supplied R expression)
#' @noRd
make_expr_part <- function(cond) {
  expr_str <- trimws(cond$expr %||% "")
  if (nchar(expr_str) == 0) return(NULL)
  tryCatch(str2lang(expr_str), error = function(e) NULL)
}

# =========================================================================
# Mutate
# =========================================================================

#' Build a dplyr::mutate expression from JS rows
#' @param rows List of row objects, each with `name` and `expr` strings
#' @param by Character vector of grouping column names (optional)
#' @return A language object
#' @noRd
make_mutate_expr <- function(rows, by = character()) {
  rows <- Filter(
    function(r) nzchar(r$name %||% "") && nzchar(r$expr %||% ""),
    rows
  )

  if (length(rows) == 0) {
    return(bbquote(.(data)))
  }

  args <- lapply(rows, function(r) {
    tryCatch(str2lang(r$expr), error = function(e) NULL)
  })
  names(args) <- vapply(rows, function(r) r$name, character(1))
  args <- Filter(Negate(is.null), args)

  if (length(args) == 0) return(bbquote(.(data)))

  expr <- quote(dplyr::mutate(.(data)))
  for (nm in names(args)) {
    expr[[backtick_if_needed(nm)]] <- args[[nm]]
  }

  # Add .by if grouping columns
  if (length(by) > 0) {
    by_syms <- lapply(by, as.name)
    expr[[".by"]] <- as.call(c(quote(c), by_syms))
  }

  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Summarize
# =========================================================================

#' Map short function names to R calls for summarize
#' @noRd
summarize_func_map <- function(func) {
  switch(func,
    "mean" = "mean",
    "median" = "stats::median",
    "sd" = "stats::sd",
    "min" = "min",
    "max" = "max",
    "sum" = "sum",
    "n" = "dplyr::n",
    "n_distinct" = "dplyr::n_distinct",
    "first" = "dplyr::first",
    "last" = "dplyr::last",
    func
  )
}

#' Build a dplyr::summarize expression from JS summaries
#' @param summaries List of summary objects with type, name, func/expr, col
#' @param by Character vector of grouping column names
#' @return A language object
#' @noRd
make_summarize_expr <- function(summaries, by = character()) {
  if (length(summaries) == 0) {
    return(bbquote(dplyr::summarize(.(data))))
  }

  expr <- quote(dplyr::summarize(.(data)))

  for (s in summaries) {
    nm <- backtick_if_needed(s$name %||% "")
    if (!nzchar(nm)) next

    if (identical(s$type, "simple")) {
      func_full <- summarize_func_map(s$func %||% "")
      col <- s$col %||% ""
      if (func_full == "dplyr::n") {
        call_expr <- str2lang(paste0(func_full, "()"))
      } else if (nzchar(col)) {
        call_expr <- str2lang(paste0(
          func_full, "(", backtick_if_needed(col), ")"
        ))
      } else {
        next
      }
    } else if (identical(s$type, "expr")) {
      expr_str <- trimws(s$expr %||% "")
      if (!nzchar(expr_str)) next
      call_expr <- tryCatch(str2lang(expr_str), error = function(e) NULL)
      if (is.null(call_expr)) next
    } else {
      next
    }

    expr[[nm]] <- call_expr
  }

  # Add .by if grouping columns
  if (length(by) > 0) {
    by_syms <- lapply(by, as.name)
    expr[[".by"]] <- as.call(c(quote(c), by_syms))
  }

  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Join
# =========================================================================

#' Build a dplyr join expression from JS payload
#' @param type Join type (e.g., "left_join")
#' @param keys List of key objects with xCol, op, yCol
#' @param exprs Character vector of R expression strings
#' @param suffix_x Suffix for x columns
#' @param suffix_y Suffix for y columns
#' @return A language object
#' @noRd
make_join_expr <- function(type = "left_join",
                           keys = list(),
                           exprs = character(),
                           suffix_x = ".x",
                           suffix_y = ".y") {
  join_fn <- paste0("dplyr::", type)

  has_non_equi <- any(vapply(keys, function(k) k$op != "==", logical(1)))
  has_exprs <- length(exprs) > 0

  if (has_non_equi || has_exprs) {
    # Use dplyr::join_by()
    jb_parts <- character()
    for (k in keys) {
      x <- backtick_if_needed(k$xCol)
      y <- backtick_if_needed(k$yCol)
      jb_parts <- c(jb_parts, paste0(x, " ", k$op, " ", y))
    }
    for (e in exprs) {
      if (nzchar(trimws(e))) jb_parts <- c(jb_parts, trimws(e))
    }

    if (length(jb_parts) == 0) {
      # Natural join (no by argument)
      text <- paste0(join_fn, "(.(x), .(y))")
    } else {
      jb_text <- paste(jb_parts, collapse = ", ")
      text <- paste0(
        join_fn, "(.(x), .(y), by = dplyr::join_by(",
        jb_text, "))"
      )
    }
  } else if (length(keys) > 0) {
    # Equi-join with named character vector
    by_parts <- vapply(keys, function(k) {
      paste0(backtick_if_needed(k$xCol), ' = "', k$yCol, '"')
    }, character(1))
    by_text <- paste(by_parts, collapse = ", ")
    text <- paste0(join_fn, "(.(x), .(y), by = c(", by_text, "))")
  } else {
    # No keys selected yet: pass through x until user configures
    return(bbquote(.(x)))
  }

  # Add suffix for non-semi/anti joins
  if (!type %in% c("semi_join", "anti_join") &&
        (suffix_x != ".x" || suffix_y != ".y")) {
    text <- sub(
      "\\)$",
      paste0(', suffix = c("', suffix_x, '", "', suffix_y, '"))'),
      text
    )
  }

  parsed <- tryCatch(str2lang(text), error = function(e) NULL)
  if (is.null(parsed)) return(bbquote(dplyr::left_join(.(x), .(y))))
  parsed
}

# =========================================================================
# Select
# =========================================================================

#' Build a dplyr::select expression
#' @param columns Character vector of column names
#' @param exclude Logical, if TRUE use negative selection
#' @param distinct Logical, if TRUE chain distinct()
#' @return A language object
#' @noRd
make_select_expr <- function(columns, exclude = FALSE, distinct = FALSE) {
  exclude <- isTRUE(exclude)
  distinct <- isTRUE(distinct)

  if (length(columns) == 0) {
    base <- bbquote(dplyr::select(.(data), dplyr::everything()))
  } else {
    col_syms <- lapply(columns, as.name)
    cols_call <- as.call(c(quote(c), col_syms))

    if (exclude) {
      base <- bbquote(
        dplyr::select(.(data), -.(cols)),
        list(cols = cols_call)
      )
    } else {
      base <- bbquote(
        dplyr::select(.(data), ..(cols)),
        list(cols = col_syms),
        splice = TRUE
      )
    }
  }

  if (distinct) {
    bbquote(dplyr::distinct(.(base)), list(base = base))
  } else {
    base
  }
}

# =========================================================================
# Arrange
# =========================================================================

#' Build a dplyr::arrange expression
#' @param columns List of objects with `column` and `direction` ("asc"/"desc")
#' @return A language object
#' @noRd
make_arrange_expr <- function(columns) {
  if (length(columns) == 0) return(bbquote(.(data)))

  args <- lapply(columns, function(c) {
    col <- as.name(c$column)
    if (identical(c$direction, "desc")) {
      as.call(list(str2lang("dplyr::desc"), col))
    } else {
      col
    }
  })

  expr <- as.call(c(quote(dplyr::arrange), quote(.(data)), args))
  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Rename
# =========================================================================

#' Build a dplyr::rename expression
#' @param renames Named list where names = new names, values = old names
#' @return A language object
#' @noRd
make_rename_expr <- function(renames) {
  if (length(renames) == 0) return(bbquote(.(data)))

  expr <- quote(dplyr::rename(.(data)))
  for (new_nm in names(renames)) {
    old_nm <- renames[[new_nm]]
    if (nzchar(new_nm) && nzchar(old_nm)) {
      expr[[backtick_if_needed(new_nm)]] <- as.name(old_nm)
    }
  }

  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Slice
# =========================================================================

#' Build a dplyr::slice_* expression
#' @param type Slice type: "head", "tail", "min", "max", "sample"
#' @param n Number of rows
#' @param prop Proportion (alternative to n)
#' @param order_by Column for min/max
#' @param with_ties Keep tied values (min/max)
#' @param weight_by Column for weighted sampling
#' @param replace Sample with replacement
#' @param by Grouping columns
#' @return A language object
#' @noRd
make_slice_expr <- function(type = "head", n = 5L, prop = NULL,
                            order_by = "", with_ties = TRUE,
                            weight_by = "", replace = FALSE,
                            by = character()) {
  fn_str <- switch(type,
    "head" = "dplyr::slice_head",
    "tail" = "dplyr::slice_tail",
    "min" = "dplyr::slice_min",
    "max" = "dplyr::slice_max",
    "sample" = "dplyr::slice_sample",
    "dplyr::slice_head"
  )

  expr <- as.call(list(str2lang(fn_str), quote(.(data))))

  if (is.numeric(prop) && length(prop) == 1 && prop > 0 && prop <= 1) {
    expr[["prop"]] <- prop
  } else {
    expr[["n"]] <- as.integer(n %||% 5L)
  }

  if (type %in% c("min", "max") && nzchar(order_by %||% "")) {
    expr[["order_by"]] <- as.name(order_by)
    if (!isTRUE(with_ties)) expr[["with_ties"]] <- FALSE
  }

  if (type == "sample") {
    if (nzchar(weight_by %||% "")) expr[["weight_by"]] <- as.name(weight_by)
    if (isTRUE(replace)) expr[["replace"]] <- TRUE
  }

  if (length(by) > 0) {
    by_syms <- lapply(by, as.name)
    expr[[".by"]] <- as.call(c(quote(c), by_syms))
  }

  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Pivot longer
# =========================================================================

#' Build a tidyr::pivot_longer expression
#' @noRd
make_pivot_longer_expr <- function(
    cols, names_to = "name",
    values_to = "value",
    values_drop_na = FALSE,
    names_prefix = "") {
  if (length(cols) == 0) return(bbquote(.(data)))

  col_syms <- lapply(cols, as.name)
  cols_call <- as.call(c(quote(c), col_syms))

  expr <- as.call(list(
    str2lang("tidyr::pivot_longer"),
    quote(.(data)), cols_call
  ))
  expr[["names_to"]] <- names_to %||% "name"
  expr[["values_to"]] <- values_to %||% "value"
  if (isTRUE(values_drop_na)) {
    expr[["values_drop_na"]] <- TRUE
  }
  if (nzchar(names_prefix %||% "")) {
    expr[["names_prefix"]] <- names_prefix
  }

  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Pivot wider
# =========================================================================

#' Build a tidyr::pivot_wider expression
#' @noRd
make_pivot_wider_expr <- function(
    names_from, values_from,
    id_cols = character(),
    values_fill = NULL,
    names_sep = "_",
    names_prefix = "") {
  if (length(names_from) == 0 || length(values_from) == 0) {
    return(bbquote(.(data)))
  }

  expr <- as.call(list(str2lang("tidyr::pivot_wider"), quote(.(data))))

  if (length(names_from) == 1) {
    expr[["names_from"]] <- as.name(names_from)
  } else {
    expr[["names_from"]] <- as.call(c(quote(c), lapply(names_from, as.name)))
  }

  if (length(values_from) == 1) {
    expr[["values_from"]] <- as.name(values_from)
  } else {
    expr[["values_from"]] <- as.call(c(quote(c), lapply(values_from, as.name)))
  }

  if (length(id_cols) > 0) {
    # Exclude columns already used by names_from / values_from
    id_cols <- setdiff(id_cols, c(names_from, values_from))
    if (length(id_cols) > 0) {
      expr[["id_cols"]] <- as.call(
        c(quote(c), lapply(id_cols, as.name))
      )
    }
  }

  if (!is.null(values_fill) && length(values_fill) > 0) {
    expr[["values_fill"]] <- values_fill
  }
  if (!identical(names_sep, "_")) expr[["names_sep"]] <- names_sep
  if (nzchar(names_prefix %||% "")) expr[["names_prefix"]] <- names_prefix

  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Unite
# =========================================================================

#' Build a tidyr::unite expression
#' @noRd
make_unite_expr <- function(col, cols, sep = "_", # nolint: object_name_linter.
                            remove = TRUE,
                            na.rm = FALSE) { # nolint: object_name_linter.
  if (length(cols) == 0 || !nzchar(col %||% "")) return(bbquote(.(data)))

  col_syms <- lapply(cols, as.name)
  expr <- as.call(c(quote(tidyr::unite), quote(.(data)), col, col_syms))
  if (!identical(sep, "_")) expr[["sep"]] <- sep
  if (!isTRUE(remove)) expr[["remove"]] <- FALSE
  if (isTRUE(na.rm)) expr[["na.rm"]] <- TRUE

  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Separate
# =========================================================================

#' Build a tidyr::separate expression
#' @noRd
make_separate_expr <- function(col, into, sep = "[^[:alnum:]]+",
                               remove = TRUE, convert = FALSE,
                               extra = "warn", fill = "warn") {
  if (!nzchar(col %||% "") || length(into) == 0) return(bbquote(.(data)))

  expr <- as.call(list(
    str2lang("tidyr::separate"), quote(.(data)),
    as.name(col), into
  ))
  if (!identical(sep, "[^[:alnum:]]+")) expr[["sep"]] <- sep
  if (!isTRUE(remove)) expr[["remove"]] <- FALSE
  if (isTRUE(convert)) expr[["convert"]] <- TRUE
  if (!identical(extra, "warn")) expr[["extra"]] <- extra
  if (!identical(fill, "warn")) expr[["fill"]] <- fill

  bbquote(.(expr), list(expr = expr))
}

# =========================================================================
# Bind rows / cols
# =========================================================================

#' Build a dplyr::bind_rows expression (variadic)
#' @param id_name Optional id column name
#' @param arg_names Named character vector of input names
#' @return A language object
#' @noRd
make_bind_rows_expr <- function(id_name = "", arg_names = character()) {
  data_args <- lapply(arg_names, as.name)
  expr <- as.call(c(quote(dplyr::bind_rows), data_args))
  if (nzchar(id_name %||% "")) expr[[".id"]] <- id_name
  expr
}

#' Build a dplyr::bind_cols expression (variadic)
#' @param arg_names Named character vector of input names
#' @return A language object
#' @noRd
make_bind_cols_expr <- function(arg_names = character()) {
  as.call(c(quote(dplyr::bind_cols), lapply(arg_names, as.name)))
}

# =========================================================================
# Shared utilities
# =========================================================================

#' Build column metadata for JS
#'
#' Computes type, range (numeric), unique values, and NA/empty presence
#' for each column in a data frame. Result is sent to JS via sendCustomMessage.
#'
#' @param df A data frame
#' @return A list of column info objects
#' @noRd
build_column_meta <- function(df) {
  lapply(colnames(df), function(col) {
    vals <- df[[col]]
    type <- if (is.numeric(vals)) {
      "numeric"
    } else if (is.integer(vals)) {
      "integer"
    } else if (is.logical(vals)) {
      "logical"
    } else {
      "character"
    }

    info <- list(name = col, type = type, hasNA = anyNA(vals))

    if (type %in% c("numeric", "integer")) {
      info$min <- min(vals, na.rm = TRUE)
      info$max <- max(vals, na.rm = TRUE)
      info$uniqueValues <- sort(unique(vals[!is.na(vals)]))
    } else {
      uv <- sort(unique(as.character(vals[!is.na(vals)])))
      info$values <- uv
      info$hasEmpty <- any(vals == "", na.rm = TRUE)
    }
    info
  })
}
