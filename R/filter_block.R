#' Filter block (JS-driven)
#'
#' Unified filter block combining simple mode (column + operator + values)
#' and expression mode (free R code) in a single JS-driven UI.
#'
#' @param state List with `conditions` (array of condition objects) and
#'   `operator` ("&" or "|"). Condition types:
#'   - `values`: column, values (character vector), mode ("include"/"exclude")
#'   - `numeric`: column, op (">", ">=", "<", "<=", "is", "is not"), value
#'   - `expr`: expr (R expression as string)
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_filter_block(
#'       state = list(
#'         conditions = list(
#'           list(type = "values", column = "Species",
#'                values = list("setosa"), mode = "include")
#'         ),
#'         operator = "&"
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
new_filter_block <- function(
  state = list(conditions = list(), operator = "&"),
  ...
) {
  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- reactiveVal(state)

        # Bidirectional sync: self_write tracks UI-initiated changes
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        # Send lightweight column summary (names + types) on data change
        observeEvent(data(), {
          meta <- build_column_summary(data())
          session$sendCustomMessage(
            "filter-columns",
            list(id = ns("filter_input"), columns = meta)
          )
        })

        # On-demand: JS requests unique values for a specific column
        observeEvent(input$filter_input_request_values, {
          col <- input$filter_input_request_values
          if (!is.null(col) && col %in% colnames(data())) {
            meta <- build_column_values(data(), col)
            session$sendCustomMessage(
              "filter-column-values",
              list(id = ns("filter_input"), column = meta)
            )
          }
        })

        # JS -> R: user changed the filter
        observeEvent(input$filter_input, {
          self_write$active <- TRUE
          r_state(input$filter_input)
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "filter-block-update",
              list(
                id = ns("filter_input"),
                state = normalize_filter_state_for_js(r_state())
              )
            )
          }
        })

        list(
          expr = reactive({
            s <- r_state()
            make_filter_expr(
              s$conditions %||% list(),
              s$operator %||% "&",
              isTRUE(s$preserveOrder)
            )
          }),
          state = list(state = r_state)
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    function(id) {
      tagList(
        blockr_core_js_dep(),
        blockr_blocks_css_dep(),
        blockr_select_dep(),
        blockr_input_dep(),
        filter_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "filter_input"),
            class = "filter-block-container"
          )
        )
      )
    },
    class = "filter_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
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
#' @export
filter_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "filter-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "filter-block.js"
    ),
    htmltools::htmlDependency(
      name = "filter-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "filter-block.css"
    )
  )
}
