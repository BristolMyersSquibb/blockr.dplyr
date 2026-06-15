#' Slice block (JS-driven)
#'
#' Subset rows by position using dplyr::slice_* family of functions.
#' Single select for type (head/tail/min/max/sample), number input for n,
#' optional column selects for order_by and weight_by, toggle pills for
#' with_ties and replace, multi-select for grouping (.by).
#' Auto-submits on any change.
#'
#' @param type Slice type: one of `"head"`, `"tail"`, `"min"`, `"max"`,
#'   `"sample"`.
#' @param n Integer number of rows to keep.
#' @param prop Proportion of rows to keep (numeric or `NULL`); takes
#'   precedence over `n` when set.
#' @param order_by Column name used to order rows for `min`/`max` slices.
#' @param with_ties Logical; whether to keep tied rows for `min`/`max`.
#' @param weight_by Column name used as sampling weights for `sample`.
#' @param replace Logical; whether to sample with replacement.
#' @param rows Row index expression (e.g. `"1:5"`) for positional slicing.
#' @param by Character vector (or array) of grouping columns (`.by`).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_slice_block(
#'       type = "head",
#'       n = 10L,
#'       prop = NULL,
#'       order_by = "",
#'       with_ties = TRUE,
#'       weight_by = "",
#'       replace = FALSE,
#'       by = list()
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#'
#' @export
new_slice_block <- function(
  type = "head",
  n = 5L,
  prop = NULL,
  order_by = "",
  with_ties = TRUE,
  weight_by = "",
  replace = FALSE,
  rows = "1:5",
  by = list(),
  ...
) {
  new_js_transform_block(
    class = "slice_block",
    name = "slice",
    state = list(
      type = type,
      n = n,
      prop = prop,
      order_by = order_by,
      with_ties = with_ties,
      weight_by = weight_by,
      replace = replace,
      rows = rows,
      by = by
    ),
    expr_fn = function(s) {
      make_slice_expr(
        s$type %||% "head",
        s$n %||% 5L,
        s$prop,
        s$order_by %||% "",
        s$with_ties %||% TRUE,
        s$weight_by %||% "",
        s$replace %||% FALSE,
        s$rows %||% "1:5",
        s$by %||% list()
      )
    },
    ...
  )
}
