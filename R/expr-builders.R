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
make_filter_expr <- function(conditions, operator = "&") {
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

  bbquote(
    dplyr::filter(.(data), .(combined)),
    list(combined = combined)
  )
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
