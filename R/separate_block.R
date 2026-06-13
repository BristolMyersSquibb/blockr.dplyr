#' Separate block (JS-driven)
#'
#' Split a single column into multiple columns using tidyr::separate.
#' Single select for source column, text input for into (comma-separated
#' new column names), text input for sep, toggle pills for remove and convert.
#' Auto-submits on any change.
#'
#' @param state List with `col` (string, source column), `into` (character
#'   vector of new column names), `sep` (string or regex), `remove` (logical),
#'   `convert` (logical), `extra` (string), and `fill` (string).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   df <- data.frame(x = c("a-1", "b-2", "c-3"), stringsAsFactors = FALSE)
#'   serve(
#'     new_separate_block(
#'       state = list(
#'         col = "x",
#'         into = list("letter", "number"),
#'         sep = "-",
#'         remove = TRUE,
#'         convert = FALSE,
#'         extra = "warn",
#'         fill = "warn"
#'       )
#'     ),
#'     data = list(data = df)
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#'
#' @export
new_separate_block <- function(
  state = list(
    col = "",
    into = list(),
    sep = "[^[:alnum:]]+",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
  ),
  ...
) {
  new_js_transform_block(
    class = "separate_block",
    name = "separate",
    state = state,
    expr_fn = function(s) {
      make_separate_expr(
        s$col %||% "",
        s$into %||% list(),
        s$sep %||% "[^[:alnum:]]+",
        s$remove %||% TRUE,
        s$convert %||% FALSE
      )
    },
    ...
  )
}
