#' Pivot longer block (JS-driven)
#'
#' Reshape data from wide to long format using tidyr::pivot_longer.
#' Multi-select column picker with text inputs for names_to, values_to,
#' names_prefix, and a toggle for values_drop_na. Auto-submits on any change.
#'
#' @param cols Character vector of columns to pivot into longer format.
#' @param names_to Name of the new column holding the pivoted column names.
#' @param values_to Name of the new column holding the pivoted values.
#' @param values_drop_na If `TRUE`, drop rows with `NA` values.
#' @param names_prefix Regular expression prefix stripped from column names.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_pivot_longer_block(
#'       cols = list("Sepal.Length", "Sepal.Width"),
#'       names_to = "measurement",
#'       values_to = "value",
#'       values_drop_na = FALSE,
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
new_pivot_longer_block <- function(
  cols = list(),
  names_to = "name",
  values_to = "value",
  values_drop_na = FALSE,
  names_prefix = "",
  ...
) {
  new_js_transform_block(
    class = "pivot_longer_block",
    name = "pivot-longer",
    state = list(
      cols = cols,
      names_to = names_to,
      values_to = values_to,
      values_drop_na = values_drop_na,
      names_prefix = names_prefix
    ),
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
