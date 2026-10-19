#' Filter block (JS-driven)
#'
#' Unified filter block combining simple mode (column + operator + values)
#' and expression mode (free R code) in a single JS-driven UI.
#'
#' @param conditions Array of condition objects combined by `operator`.
#'   Condition types:
#'   - `values`: column, values (character vector), mode ("include"/"exclude")
#'   - `numeric`: column, op (">", ">=", "<", "<=", "is", "is not"), value
#'   - `expr`: expr (R expression as string)
#' @param operator How conditions are combined: `"&"` (all) or `"|"` (any).
#' @param preserve_order If `TRUE`, rows are returned in the order matched by
#'   the conditions rather than the original data order.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_filter_block(
#'       conditions = list(
#'         list(type = "values", column = "Species",
#'              values = list("setosa"), mode = "include")
#'       ),
#'       operator = "&"
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
new_filter_block <- function(
  conditions = list(),
  operator = "&",
  preserve_order = FALSE,
  ...
) {
  blockr.ui::new_js_block(
    "filter",
    state = list(
      conditions = conditions,
      operator = operator,
      preserve_order = preserve_order
    ),
    expr = function(s) {
      make_filter_expr(s$conditions, s$operator, s$preserve_order)
    },
    class = "filter_block",
    pkg = "blockr.dplyr",
    ...
  )
}

#' HTML dependency for filter block JS + CSS
#'
#' Exported for reuse by blockr packages that embed the filter block's JS UI
#' (e.g. blockr.dm's dm filter block).
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @keywords internal
#' @export
filter_block_dep <- function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    blockr.ui::js_block_dep(),
    htmltools::htmlDependency(
      name = "blockr.dplyr-filter-block",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("blocks", "filter", package = "blockr.dplyr"),
      script = "filter.js",
      stylesheet = "filter.css"
    )
  )
}
