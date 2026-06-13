#' Slice block (JS-driven)
#'
#' Subset rows by position using dplyr::slice_* family of functions.
#' Single select for type (head/tail/min/max/sample), number input for n,
#' optional column selects for order_by and weight_by, toggle pills for
#' with_ties and replace, multi-select for grouping (.by).
#' Auto-submits on any change.
#'
#' @param state List with `type` (string: "head", "tail", "min", "max",
#'   "sample"), `n` (integer), `prop` (numeric or NULL), `order_by` (string),
#'   `with_ties` (logical), `weight_by` (string), `replace` (logical), and
#'   `by` (character vector).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_slice_block(
#'       state = list(
#'         type = "head",
#'         n = 10L,
#'         prop = NULL,
#'         order_by = "",
#'         with_ties = TRUE,
#'         weight_by = "",
#'         replace = FALSE,
#'         by = list()
#'       )
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
  state = list(
    type = "head",
    n = 5L,
    prop = NULL,
    order_by = "",
    with_ties = TRUE,
    weight_by = "",
    replace = FALSE,
    rows = "1:5",
    by = list()
  ),
  ...
) {
  new_js_transform_block(
    class = "slice_block",
    name = "slice",
    state = state,
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
