#' Mutate block (JS-driven)
#'
#' JS-driven mutate block with dynamic mutation rows of name + expression pairs.
#' Each mutation defines a new or modified column using an R expression.
#'
#' @param mutations Array of objects with `name` and `expr` strings. Each
#'   mutation becomes a `dplyr::mutate()` argument creating or modifying a
#'   column.
#' @param by Optional array of grouping column names for grouped mutation.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_mutate_block(
#'       mutations = list(
#'         list(name = "Sepal.Ratio", expr = "Sepal.Length / Sepal.Width")
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
new_mutate_block <- function(
  mutations = list(list(name = "", expr = "")),
  by = list(),
  ...
) {
  new_js_transform_block(
    class = "mutate_block",
    name = "mutate",
    state = list(
      mutations = mutations,
      by = by
    ),
    expr_fn = function(s) {
      make_mutate_expr(
        s$mutations %||% list(),
        s$by %||% character()
      )
    },
    shared_deps = c("select", "input"),
    ...
  )
}
