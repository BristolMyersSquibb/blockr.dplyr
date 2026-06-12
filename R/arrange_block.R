#' Arrange block (JS-driven)
#'
#' Sort/arrange block with dynamic rows. Each row has a column picker and
#' a direction toggle (ascending/descending). Auto-submits on any change.
#'
#' @param state List with `columns` (list of objects with `column` and
#'   `direction` fields, where direction is "asc" or "desc").
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_arrange_block(
#'       state = list(
#'         columns = list(
#'           list(column = "Sepal.Length", direction = "desc")
#'         )
#'       )
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
new_arrange_block <- function(
  state = list(columns = list()),
  ...
) {
  new_js_transform_block(
    class = "arrange_block",
    name = "arrange",
    state = state,
    expr_fn = function(s) {
      make_arrange_expr(s$columns %||% list())
    },
    ...
  )
}
