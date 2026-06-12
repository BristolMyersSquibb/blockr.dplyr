#' Pivot wider block (JS-driven)
#'
#' Reshape data from long to wide format using tidyr::pivot_wider.
#' Multi-selects for names_from, values_from, and id_cols (optional).
#' Text inputs for values_fill, names_sep, names_prefix.
#' Auto-submits on any change.
#'
#' @param state List with `names_from` (character vector), `values_from`
#'   (character vector), `id_cols` (character vector, optional),
#'   `values_fill` (value or NULL), `names_sep` (string), and
#'   `names_prefix` (string).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_pivot_wider_block(
#'       state = list(
#'         names_from = list("Species"),
#'         values_from = list("Sepal.Length"),
#'         id_cols = list(),
#'         values_fill = NULL,
#'         names_sep = "_",
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
new_pivot_wider_block <- function(
  state = list(
    names_from = list(),
    values_from = list(),
    id_cols = list(),
    values_fill = NULL,
    names_sep = "_",
    names_prefix = ""
  ),
  ...
) {
  new_js_transform_block(
    class = "pivot_wider_block",
    name = "pivot-wider",
    state = state,
    expr_fn = function(s) {
      make_pivot_wider_expr(
        s$names_from %||% list(),
        s$values_from %||% list(),
        s$id_cols %||% list(),
        s$values_fill,
        s$names_sep %||% "_",
        s$names_prefix %||% "",
        s$values_fn %||% ""
      )
    },
    ...
  )
}
