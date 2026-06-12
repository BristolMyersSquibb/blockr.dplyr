#' Rename block (JS-driven)
#'
#' Column rename block with dynamic rows. Each row maps an old column name
#' to a new name. Auto-submits on any change (300ms debounce for text input).
#'
#' @param state List with `renames` (named list where names are new column
#'   names and values are old column names).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_rename_block(
#'       state = list(
#'         renames = list(sepal_len = "Sepal.Length")
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
new_rename_block <- function(
  state = list(renames = list()),
  ...
) {
  new_js_transform_block(
    class = "rename_block",
    name = "rename",
    state = state,
    expr_fn = function(s) {
      make_rename_expr(s$renames %||% list())
    },
    ...
  )
}
