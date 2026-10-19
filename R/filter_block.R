#' Filter block (JS-driven)
#'
#' Keeps the rows for which its predicates are `TRUE`. The UI offers value
#' pickers, comparisons and free R expressions; its state is the filter step
#' of the block IR (see [blockr.ui::ir_to_expr()]), which R translates to
#' `dplyr::filter()` and blockx to SQL.
#'
#' @param pred A list of IR predicates, ANDed. A predicate is an IR
#'   expression: `list(call = "%in%", args = list(list(col = "Species"),
#'   list(list = list("setosa"))))`, or R code as typed,
#'   `list(code = "Sepal.Length > 5", lang = "r")`.
#' @param preserve_order If `TRUE`, rows are returned in the order their
#'   values were picked rather than the original data order.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()].
#'   `conditions` and `operator`, the arguments of earlier versions, are read
#'   and turned into `pred`, so saved boards keep working.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_filter_block(
#'       pred = list(list(
#'         call = "%in%",
#'         args = list(list(col = "Species"), list(list = list("setosa")))
#'       ))
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList req
#' @importFrom htmltools htmlDependency
#'
#' @export
new_filter_block <- function(
  pred = list(),
  preserve_order = FALSE,
  ...
) {
  args <- list(...)
  if (!is.null(args$conditions) || !is.null(args$operator)) {
    pred <- filter_conditions_to_ir(args$conditions, args$operator %||% "&")
  }
  args$conditions <- NULL
  args$operator <- NULL

  do.call(
    blockr.ui::new_js_block,
    c(
      list(
        "filter",
        state = list(pred = pred, preserve_order = preserve_order),
        class = "filter_block",
        pkg = "blockr.dplyr"
      ),
      args
    )
  )
}

#' The filter state of earlier versions as IR
#'
#' `conditions` (values, numeric and expression rows) combined by
#' `operator`, as the filter block's UI writes them (inst/blocks/filter,
#' toIR()). Only for reading boards saved before the state was IR.
#' @noRd
filter_conditions_to_ir <- function(conditions, operator = "&") {
  conds <- as_record_list(conditions %||% list())
  # & and | take two or more arguments, so each row stays one argument
  fold <- function(op, parts) {
    if (length(parts) == 1L) parts[[1L]] else list(call = op, args = parts)
  }
  col <- function(x) list(col = x)

  part <- function(c) {
    if (!is.list(c) || is.null(c$type)) return(NULL)
    switch(
      c$type,
      expr = {
        text <- trimws(c$expr %||% "")
        if (nzchar(text)) list(code = text, lang = "r")
      },
      numeric = {
        op <- c$op %||% ""
        op <- switch(op, "is" = "==", "is not" = "!=", op)
        if (!is.null(c$column) && !is.null(c$value) &&
              op %in% c(">", ">=", "<", "<=", "==", "!=")) {
          list(call = op, args = list(col(c$column),
                                      list(lit = as.numeric(c$value))))
        }
      },
      values = {
        values <- as.character(unlist(c$values))
        if (is.null(c$column) || !length(values)) return(NULL)
        include <- !identical(c$mode, "exclude")
        regular <- setdiff(values, c("<NA>", "<empty>"))
        parts <- list()
        if (length(regular)) {
          inn <- list(call = "%in%", args = list(
            col(c$column),
            list(list = as.list(convert_filter_values(regular, c$colType)))
          ))
          parts <- c(parts, list(if (include) inn else list(call = "!", args = list(inn))))
        }
        if ("<NA>" %in% values) {
          na <- list(call = "is.na", args = list(col(c$column)))
          parts <- c(parts, list(if (include) na else list(call = "!", args = list(na))))
        }
        if ("<empty>" %in% values) {
          parts <- c(parts, list(list(call = if (include) "==" else "!=",
                                      args = list(col(c$column), list(lit = "")))))
        }
        if (length(parts)) fold(if (include) "|" else "&", parts)
      },
      NULL
    )
  }

  parts <- Filter(Negate(is.null), lapply(conds, part))
  if (identical(operator, "|") && length(parts) > 1L) list(fold("|", parts)) else parts
}

#' HTML dependency for filter block JS + CSS
#'
#' Exported for reuse by blockr packages that embed the filter block's JS UI
#' (e.g. blockr.dm's dm filter block).
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @keywords internal
#' @export
filter_block_dep <- function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    blockr.ui::js_block_dep(),
    htmltools::htmlDependency(
      name = "blockr.dplyr-filter-block",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("blocks", "filter", package = "blockr.dplyr"),
      script = "filter.js",
      stylesheet = "filter.css"
    )
  )
}
