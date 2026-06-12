#' Select block (JS-driven)
#'
#' Column selection block with reorderable multi-select, exclude toggle,
#' and distinct toggle. Auto-submits on any change.
#'
#' @param state List with `columns` (character vector of column names),
#'   `exclude` (logical), and `distinct` (logical).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_select_block(
#'       state = list(
#'         columns = list("Sepal.Length", "Species"),
#'         exclude = FALSE,
#'         distinct = FALSE
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
new_select_block <- function(
  state = list(columns = list(), exclude = FALSE, distinct = FALSE),
  ...
) {
  new_js_transform_block(
    class = "select_block",
    name = "select",
    state = state,
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
