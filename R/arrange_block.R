#' Arrange block (JS-driven)
#'
#' Sort/arrange block with dynamic rows. Each row has a column picker and
#' a direction toggle (ascending/descending). Auto-submits on any change.
#'
#' @param state List with `columns` (list of objects with `column` and
#'   `direction` fields, where direction is "asc" or "desc").
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_arrange_block(
#'       state = list(
#'         columns = list(
#'           list(column = "Sepal.Length", direction = "desc")
#'         )
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
new_arrange_block <- function(
  state = list(columns = list()),
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

        # Send column summary (name + label + type) to JS when data changes
        observeEvent(data(), {
          meta <- build_column_picker_meta(data())
          session$sendCustomMessage(
            "arrange-columns",
            list(id = ns("arrange_input"), columns = meta)
          )
        })

        # JS -> R: user changed the arrangement
        observeEvent(input$arrange_input, {
          self_write$active <- TRUE
          r_state(input$arrange_input)
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "arrange-block-update",
              list(id = ns("arrange_input"), state = r_state())
            )
          }
        })

        list(
          expr = reactive({
            s <- r_state()
            make_arrange_expr(s$columns %||% list())
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
        arrange_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "arrange_input"),
            class = "arrange-block-container"
          )
        )
      )
    },
    class = "arrange_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for arrange block JS + CSS
#' @noRd
arrange_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "arrange-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "arrange-block.js"
    ),
    htmltools::htmlDependency(
      name = "arrange-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "arrange-block.css"
    )
  )
}
