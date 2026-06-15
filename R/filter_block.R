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
  new_js_transform_block(
    class = "filter_block",
    name = "filter",
    state = list(
      conditions = conditions,
      operator = operator,
      preserve_order = preserve_order
    ),
    expr_fn = function(s) {
      make_filter_expr(
        s$conditions %||% list(),
        s$operator %||% "&",
        isTRUE(s$preserve_order)
      )
    },
    # Lightweight summary (names + types); unique values load on demand
    columns_meta = build_column_summary,
    setup = function(input, session, ns, data, input_name) {
      # Columns with more distinct values than this switch to server-side
      # search: the dropdown gets the first `limit` values plus a total
      # count, and typing re-queries R instead of filtering client-side.
      limit <- blockr_option("dplyr.max_filter_values", 1000)

      send_values <- function(col, query = "") {
        if (!is.null(col) && col %in% colnames(data())) {
          meta <- build_column_values(data(), col, limit = limit,
                                      query = query)
          session$sendCustomMessage(
            "filter-column-values",
            list(id = ns(input_name), column = meta)
          )
        }
      }

      # On-demand: JS requests unique values for a specific column
      observeEvent(input[[paste0(input_name, "_request_values")]], {
        send_values(input[[paste0(input_name, "_request_values")]])
      })

      # Server-side search on truncated (high-cardinality) columns
      observeEvent(input[[paste0(input_name, "_search_values")]], {
        req_ <- input[[paste0(input_name, "_search_values")]]
        send_values(req_$column, req_$query %||% "")
      })
    },
    normalize_state = normalize_filter_state_for_js,
    shared_deps = c("select", "input"),
    ...
  )
}

#' Normalize filter state before sending to JS
#'
#' Shiny's `sendCustomMessage` serializes with `auto_unbox = TRUE`, which
#' flattens length-1 character vectors to JSON scalars. The JS filter block
#' then treats a scalar `values` as an iterable string and renders one chip
#' per character. Wrap `values` in `as.list()` so JSON always emits an array,
#' regardless of length.
#' @noRd
normalize_filter_state_for_js <- function(state) {
  if (is.null(state)) return(state)
  conds <- state$conditions %||% list()
  state$conditions <- lapply(conds, function(cond) {
    if (!is.null(cond$values)) cond$values <- as.list(cond$values)
    cond
  })
  state
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
  js_block_dep("filter")
}
