#' Mutate block (JS-driven)
#'
#' JS-driven mutate block with dynamic mutation rows of name + expression pairs.
#' Each mutation defines a new or modified column using an R expression.
#'
#' @param state List with `mutations` (array of objects with `name` and `expr`
#'   strings) and optional `by` (array of grouping column names).
#'   Each mutation becomes a `dplyr::mutate()` argument.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_mutate_block(
#'       state = list(
#'         mutations = list(
#'           list(name = "Sepal.Ratio", expr = "Sepal.Length / Sepal.Width")
#'         )
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
new_mutate_block <- function(
  state = list(mutations = list(list(name = "", expr = "")), by = list()),
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
          session$sendCustomMessage(
            "mutate-columns",
            list(id = ns("mutate_input"), columns = as.list(colnames(data())))
          )
        })

        # Send initial mutations on first data arrival
        observeEvent(data(), {
          s <- r_state()
          session$sendCustomMessage(
            "mutate-set-mutations",
            list(id = ns("mutate_input"), mutations = s$mutations %||% list())
          )
        }, once = TRUE)

        # JS -> R: user changed the mutations
        observeEvent(input$mutate_input, {
          self_write$active <- TRUE
          r_state(input$mutate_input)
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "mutate-block-update",
              list(id = ns("mutate_input"), state = r_state())
            )
          }
        })

        list(
          expr = reactive({
            s <- r_state()
            make_mutate_expr(
              s$mutations %||% list(),
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
        mutate_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "mutate_input"),
            class = "mutate-block-container"
          )
        )
      )
    },
    class = "mutate_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for mutate block JS + CSS
#' @noRd
mutate_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "mutate-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "mutate-block.js"
    ),
    htmltools::htmlDependency(
      name = "mutate-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "mutate-block.css"
    )
  )
}
