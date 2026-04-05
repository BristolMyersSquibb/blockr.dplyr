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

        # Send column metadata to JS when data changes
        observeEvent(data(), {
          meta <- build_column_meta(data())
          session$sendCustomMessage(
            "filter-columns",
            list(id = ns("filter_input"), columns = meta)
          )
        })

        # JS -> R: user changed the filter
        observeEvent(input$filter_input, {
          self_write$active <- TRUE
          r_state(input$filter_input)
          self_write$active <- FALSE
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            # Skip: change originated from JS input
          } else {
            session$sendCustomMessage(
              "filter-block-update",
              list(id = ns("filter_input"), state = r_state())
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

#' HTML dependency for filter block JS + CSS
#' @noRd
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
