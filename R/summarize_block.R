#' Summarize block (JS-driven)
#'
#' JS-driven summarize block with two row types: simple (function + column)
#' and expression (free R code). Includes a group-by section for grouped
#' summaries.
#'
#' @param state List with `summaries` (array of summary objects) and `by`
#'   (character vector of grouping columns). Summary types:
#'   - `simple`: name, func (e.g. "mean"), col (column name)
#'   - `expr`: name, expr (R expression string)
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_summarize_block(
#'       state = list(
#'         summaries = list(
#'           list(type = "simple", name = "avg_sl", func = "mean",
#'                col = "Sepal.Length")
#'         ),
#'         by = list("Species")
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
new_summarize_block <- function(
  state = list(summaries = list(), by = list()),
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

        # Send column names (character vector) to JS when data changes
        observeEvent(data(), {
          session$sendCustomMessage(
            "summarize-columns",
            list(id = ns("summarize_input"), columns = colnames(data()))
          )
        })

        # JS -> R: user changed the summarize config
        observeEvent(input$summarize_input, {
          self_write$active <- TRUE
          r_state(input$summarize_input)
          self_write$active <- FALSE
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            # Skip: change originated from JS input
          } else {
            session$sendCustomMessage(
              "summarize-block-update",
              list(id = ns("summarize_input"), state = r_state())
            )
          }
        }, ignoreInit = TRUE)

        list(
          expr = reactive({
            s <- r_state()
            make_summarize_expr(
              s$summaries %||% list(),
              s$by %||% character()
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
        summarize_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "summarize_input"),
            class = "summarize-block-container"
          )
        )
      )
    },
    class = "summarize_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for summarize block JS + CSS
#' @noRd
summarize_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "summarize-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "summarize-block.js"
    ),
    htmltools::htmlDependency(
      name = "summarize-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "summarize-block.css"
    )
  )
}
