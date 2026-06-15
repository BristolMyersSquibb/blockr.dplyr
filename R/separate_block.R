#' Separate block (JS-driven)
#'
#' Split a single column into multiple columns using tidyr::separate.
#' Single select for source column, text input for into (comma-separated
#' new column names), text input for sep, toggle pills for remove and convert.
#' Auto-submits on any change.
#'
#' @param col Source column name to split.
#' @param into Character vector (or array) of new column names.
#' @param sep Separator string or regular expression used to split `col`.
#' @param remove Logical; whether to drop the source column.
#' @param convert Logical; whether to run type conversion on the new columns.
#' @param extra How to handle extra pieces (e.g. `"warn"`, `"drop"`, `"merge"`).
#' @param fill How to handle too few pieces (e.g. `"warn"`, `"right"`, `"left"`).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   df <- data.frame(x = c("a-1", "b-2", "c-3"), stringsAsFactors = FALSE)
#'   serve(
#'     new_separate_block(
#'       col = "x",
#'       into = list("letter", "number"),
#'       sep = "-",
#'       remove = TRUE,
#'       convert = FALSE,
#'       extra = "warn",
#'       fill = "warn"
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
  col = "",
  into = list(),
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
  ...
) {
  new_js_transform_block(
    class = "separate_block",
    name = "separate",
    state = list(
      col = col,
      into = into,
      sep = sep,
      remove = remove,
      convert = convert,
      extra = extra,
      fill = fill
    ),
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
