#' Select block (JS-driven)
#'
#' Column selection block with reorderable multi-select, exclude toggle,
#' and distinct toggle. Auto-submits on any change.
#'
#' @param columns Character vector of column names to select (or reorder).
#' @param exclude If `TRUE`, the listed `columns` are dropped instead of kept.
#' @param distinct If `TRUE`, return only distinct rows of the selection.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_select_block(
#'       columns = list("Sepal.Length", "Species"),
#'       exclude = FALSE,
#'       distinct = FALSE
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
new_select_block <- function(
  columns = list(),
  exclude = FALSE,
  distinct = FALSE,
  ...
) {
  new_js_transform_block(
    class = "select_block",
    name = "select",
    state = list(
      columns = columns,
      exclude = exclude,
      distinct = distinct
    ),
    expr_fn = function(s) {
      make_select_expr(
        s$columns %||% list(),
        s$exclude %||% FALSE,
        s$distinct %||% FALSE
      )
    },
    ...
  )
}
