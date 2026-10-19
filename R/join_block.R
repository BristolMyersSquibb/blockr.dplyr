#' Join block (JS-driven)
#'
#' Binary join block combining key-based joins (equi and non-equi) with
#' expression mode (free R code for join conditions) in a single JS-driven UI.
#' Takes two data inputs (x and y).
#'
#' @param type Join function name ("left_join", "inner_join", "right_join",
#'   "full_join", "semi_join", "anti_join").
#' @param keys List of key objects, each with `xCol`, `op`, `yCol`.
#' @param exprs Character vector of R expression strings for `join_by()`.
#' @param suffix_x Suffix for x columns (default ".x").
#' @param suffix_y Suffix for y columns (default ".y").
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_join_block(
#'       type = "left_join",
#'       keys = list(
#'         list(xCol = "id", op = "==", yCol = "id")
#'       )
#'     ),
#'     data = list(x = iris, y = iris)
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList req
#' @importFrom htmltools htmlDependency
#'
#' @export
new_join_block <- function(
  type = "left_join",
  keys = list(),
  exprs = list(),
  suffix_x = ".x",
  suffix_y = ".y",
  ...
) {
  blockr.ui::new_js_block(
    "join",
    state = list(type = type, keys = keys, exprs = exprs,
                 suffix_x = suffix_x, suffix_y = suffix_y),
    expr = function(s) {
      make_join_expr(s$type, s$keys, s$exprs, s$suffix_x, s$suffix_y)
    },
    class = "join_block",
    pkg = "blockr.dplyr",
    dat_valid = function(x, y) {
      stopifnot(is.data.frame(x), is.data.frame(y))
    },
    ...
  )
}
