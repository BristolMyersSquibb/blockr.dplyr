#' Pivot wider block (JS-driven)
#'
#' Reshape data from long to wide format using tidyr::pivot_wider.
#' Multi-selects for names_from, values_from, and id_cols (optional).
#' Text inputs for values_fill, names_sep, names_prefix.
#' Auto-submits on any change.
#'
#' @param names_from Character vector (or array) of columns whose values become
#'   the new column names.
#' @param values_from Character vector (or array) of columns whose values fill
#'   the new cells.
#' @param id_cols Character vector (or array) of identifier columns (optional);
#'   when empty, inferred from the remaining columns.
#' @param values_fill Value used to fill missing cells (value or `NULL`).
#' @param names_sep Separator string joining `names_from` columns into the new
#'   column names.
#' @param names_prefix String prefix prepended to the new column names.
#' @param values_fn Aggregation function applied to duplicate cells, as a
#'   string (e.g. `"sum"`); `""` means none.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_pivot_wider_block(
#'       names_from = list("Species"),
#'       values_from = list("Sepal.Length"),
#'       id_cols = list(),
#'       values_fill = NULL,
#'       names_sep = "_",
#'       names_prefix = ""
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
  names_from = list(),
  values_from = list(),
  id_cols = list(),
  values_fill = NULL,
  names_sep = "_",
  names_prefix = "",
  values_fn = "",
  ...
) {
  new_js_transform_block(
    class = "pivot_wider_block",
    name = "pivot-wider",
    state = list(
      names_from = names_from,
      values_from = values_from,
      id_cols = id_cols,
      values_fill = values_fill,
      names_sep = names_sep,
      names_prefix = names_prefix,
      values_fn = values_fn
    ),
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
