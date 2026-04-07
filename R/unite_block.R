#' Unite block (JS-driven)
#'
#' Paste together multiple columns into one using tidyr::unite.
#' Text input for new column name, multi-select for columns to unite,
#' text input for separator, toggle pills for remove and na.rm.
#' Auto-submits on any change.
#'
#' @param state List with `col` (string, new column name), `cols` (character
#'   vector of columns to unite), `sep` (string), `remove` (logical), and
#'   `na.rm` (logical).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_unite_block(
#'       state = list(
#'         col = "full_name",
#'         cols = list("Sepal.Length", "Sepal.Width"),
#'         sep = "_",
#'         remove = TRUE,
#'         na.rm = FALSE
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
new_unite_block <- function(
  state = list(
    col = "united",
    cols = list(),
    sep = "_",
    remove = TRUE,
    na_rm = FALSE
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
          col_names <- as.list(colnames(data()))
          session$sendCustomMessage(
            "unite-columns",
            list(id = ns("unite_input"), columns = col_names)
          )
        })

        # JS -> R: user changed the state
        observeEvent(input$unite_input, {
          self_write$active <- TRUE
          r_state(input$unite_input)
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "unite-block-update",
              list(id = ns("unite_input"), state = r_state())
            )
          }
        })

        list(
          expr = reactive({
            s <- r_state()
            make_unite_expr(
              s$col %||% "united",
              s$cols %||% list(),
              s$sep %||% "_",
              s$remove %||% TRUE,
              s$na_rm %||% FALSE
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
        unite_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "unite_input"),
            class = "unite-block-container"
          )
        )
      )
    },
    class = "unite_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for unite block JS + CSS
#' @noRd
unite_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "unite-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "unite-block.js"
    ),
    htmltools::htmlDependency(
      name = "unite-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "unite-block.css"
    )
  )
}
