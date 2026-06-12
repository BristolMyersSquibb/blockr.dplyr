#' Pivot longer block (JS-driven)
#'
#' Reshape data from wide to long format using tidyr::pivot_longer.
#' Multi-select column picker with text inputs for names_to, values_to,
#' names_prefix, and a toggle for values_drop_na. Auto-submits on any change.
#'
#' @param state List with `cols` (character vector of columns to pivot),
#'   `names_to` (string), `values_to` (string), `values_drop_na` (logical),
#'   and `names_prefix` (string).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_pivot_longer_block(
#'       state = list(
#'         cols = list("Sepal.Length", "Sepal.Width"),
#'         names_to = "measurement",
#'         values_to = "value",
#'         values_drop_na = FALSE,
#'         names_prefix = ""
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
new_pivot_longer_block <- function(
  state = list(
    cols = list(),
    names_to = "name",
    values_to = "value",
    values_drop_na = FALSE,
    names_prefix = ""
  ),
  ...
) {
  new_js_transform_block(
    class = "pivot_longer_block",
    name = "pivot-longer",
    state = state,
    expr_fn = function(s) {
      make_pivot_longer_expr(
        s$cols %||% list(),
        s$names_to %||% "name",
        s$values_to %||% "value",
        s$values_drop_na %||% FALSE,
        s$names_prefix %||% ""
      )
    },
    ...
  )
}
