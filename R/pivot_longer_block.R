#' Pivot longer block (JS-driven)
#'
#' Reshape data from wide to long format using tidyr::pivot_longer.
#' Multi-select column picker with text inputs for names_to, values_to,
#' names_prefix, and a toggle for values_drop_na. Auto-submits on any change.
#'
#' @param state List with `cols` (character vector of columns to pivot),
#'   `names_to` (string), `values_to` (string), `values_drop_na` (logical),
#'   and `names_prefix` (string).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_pivot_longer_block(
#'       state = list(
#'         cols = list("Sepal.Length", "Sepal.Width"),
#'         names_to = "measurement",
#'         values_to = "value",
#'         values_drop_na = FALSE,
#'         names_prefix = ""
#'       )
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#'
#' @export
new_pivot_longer_block <- function(
  state = list(
    cols = list(),
    names_to = "name",
    values_to = "value",
    values_drop_na = FALSE,
    names_prefix = ""
  ),
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
          col_names <- colnames(data())
          session$sendCustomMessage(
            "pivot-longer-columns",
            list(id = ns("pivot_longer_input"), columns = col_names)
          )
        })

        # JS -> R: user changed the state
        observeEvent(input$pivot_longer_input, {
          self_write$active <- TRUE
          r_state(input$pivot_longer_input)
          self_write$active <- FALSE
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            # Skip: change originated from JS input
          } else {
            session$sendCustomMessage(
              "pivot-longer-block-update",
              list(id = ns("pivot_longer_input"), state = r_state())
            )
          }
        }, ignoreInit = TRUE)

        list(
          expr = reactive({
            s <- r_state()
            make_pivot_longer_expr(
              s$cols %||% list(),
              s$names_to %||% "name",
              s$values_to %||% "value",
              s$values_drop_na %||% FALSE,
              s$names_prefix %||% ""
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
        pivot_longer_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "pivot_longer_input"),
            class = "pivot-longer-block-container"
          )
        )
      )
    },
    class = "pivot_longer_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for pivot longer block JS + CSS
#' @noRd
pivot_longer_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "pivot-longer-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "pivot-longer-block.js"
    ),
    htmltools::htmlDependency(
      name = "pivot-longer-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "pivot-longer-block.css"
    )
  )
}
