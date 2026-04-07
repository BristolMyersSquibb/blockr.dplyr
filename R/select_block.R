#' Select block (JS-driven)
#'
#' Column selection block with reorderable multi-select, exclude toggle,
#' and distinct toggle. Auto-submits on any change.
#'
#' @param state List with `columns` (character vector of column names),
#'   `exclude` (logical), and `distinct` (logical).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_select_block(
#'       state = list(
#'         columns = list("Sepal.Length", "Species"),
#'         exclude = FALSE,
#'         distinct = FALSE
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
new_select_block <- function(
  state = list(columns = list(), exclude = FALSE, distinct = FALSE),
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

        # Send column names to JS when data changes
        observeEvent(data(), {
          col_names <- as.list(colnames(data()))
          session$sendCustomMessage(
            "select-columns",
            list(id = ns("select_input"), columns = col_names)
          )
        })

        # JS -> R: user changed the selection
        observeEvent(input$select_input, {
          self_write$active <- TRUE
          r_state(input$select_input)
          self_write$active <- FALSE
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            # Skip: change originated from JS input
          } else {
            session$sendCustomMessage(
              "select-block-update",
              list(id = ns("select_input"), state = r_state())
            )
          }
        })

        list(
          expr = reactive({
            s <- r_state()
            make_select_expr(
              s$columns %||% list(),
              s$exclude %||% FALSE,
              s$distinct %||% FALSE
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
        select_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "select_input"),
            class = "select-block-container"
          )
        )
      )
    },
    class = "select_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for select block JS + CSS
#' @noRd
select_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "select-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "select-block.js"
    ),
    htmltools::htmlDependency(
      name = "select-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "select-block.css"
    )
  )
}
