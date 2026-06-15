#' Unite block (JS-driven)
#'
#' Paste together multiple columns into one using tidyr::unite.
#' Text input for new column name, multi-select for columns to unite,
#' text input for separator, toggle pills for remove and na.rm.
#' Auto-submits on any change.
#'
#' @param col Name of the new united column.
#' @param cols Character vector of columns to paste together.
#' @param sep Separator placed between values.
#' @param remove If `TRUE`, remove the input columns from the output.
#' @param na_rm If `TRUE`, drop missing values before uniting.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_unite_block(
#'       col = "full_name",
#'       cols = list("Sepal.Length", "Sepal.Width"),
#'       sep = "_",
#'       remove = TRUE,
#'       na_rm = FALSE
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
new_unite_block <- function(
  col = "united",
  cols = list(),
  sep = "_",
  remove = TRUE,
  na_rm = FALSE,
  ...
) {
  new_js_transform_block(
    class = "unite_block",
    name = "unite",
    state = list(
      col = col,
      cols = cols,
      sep = sep,
      remove = remove,
      na_rm = na_rm
    ),
    expr_fn = function(s) {
      make_unite_expr(
        s$col %||% "united",
        s$cols %||% list(),
        s$sep %||% "_",
        s$remove %||% TRUE,
        s$na_rm %||% FALSE
      )
    },
    ...
  )
}
