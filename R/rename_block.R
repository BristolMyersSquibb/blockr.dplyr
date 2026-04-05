#' Rename block (JS-driven)
#'
#' Column rename block with dynamic rows. Each row maps an old column name
#' to a new name. Auto-submits on any change (300ms debounce for text input).
#'
#' @param state List with `renames` (named list where names are new column
#'   names and values are old column names).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_rename_block(
#'       state = list(
#'         renames = list(sepal_len = "Sepal.Length")
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
new_rename_block <- function(
  state = list(renames = list()),
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
            "rename-columns",
            list(id = ns("rename_input"), columns = col_names)
          )
        })

        # JS -> R: user changed the renames
        observeEvent(input$rename_input, {
          self_write$active <- TRUE
          r_state(input$rename_input)
          self_write$active <- FALSE
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            # Skip: change originated from JS input
          } else {
            session$sendCustomMessage(
              "rename-block-update",
              list(id = ns("rename_input"), state = r_state())
            )
          }
        })

        list(
          expr = reactive({
            s <- r_state()
            make_rename_expr(s$renames %||% list())
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
        rename_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "rename_input"),
            class = "rename-block-container"
          )
        )
      )
    },
    class = "rename_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for rename block JS + CSS
#' @noRd
rename_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "rename-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "rename-block.js"
    ),
    htmltools::htmlDependency(
      name = "rename-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "rename-block.css"
    )
  )
}
