#' Separate block (JS-driven)
#'
#' Split a single column into multiple columns using tidyr::separate.
#' Single select for source column, text input for into (comma-separated
#' new column names), text input for sep, toggle pills for remove and convert.
#' Auto-submits on any change.
#'
#' @param state List with `col` (string, source column), `into` (character
#'   vector of new column names), `sep` (string or regex), `remove` (logical),
#'   `convert` (logical), `extra` (string), and `fill` (string).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   df <- data.frame(x = c("a-1", "b-2", "c-3"), stringsAsFactors = FALSE)
#'   serve(
#'     new_separate_block(
#'       state = list(
#'         col = "x",
#'         into = list("letter", "number"),
#'         sep = "-",
#'         remove = TRUE,
#'         convert = FALSE,
#'         extra = "warn",
#'         fill = "warn"
#'       )
#'     ),
#'     data = list(data = df)
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#'
#' @export
new_separate_block <- function(
  state = list(
    col = "",
    into = list(),
    sep = "[^[:alnum:]]+",
    remove = TRUE,
    convert = FALSE,
    extra = "warn",
    fill = "warn"
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
            "separate-columns",
            list(id = ns("separate_input"), columns = col_names)
          )
        })

        # JS -> R: user changed the state
        observeEvent(input$separate_input, {
          self_write$active <- TRUE
          r_state(input$separate_input)
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "separate-block-update",
              list(id = ns("separate_input"), state = r_state())
            )
          }
        }, ignoreInit = TRUE)

        list(
          expr = reactive({
            s <- r_state()
            make_separate_expr(
              s$col %||% "",
              s$into %||% list(),
              s$sep %||% "[^[:alnum:]]+",
              s$remove %||% TRUE,
              s$convert %||% FALSE,
              s$extra %||% "warn",
              s$fill %||% "warn"
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
        separate_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "separate_input"),
            class = "separate-block-container"
          )
        )
      )
    },
    class = "separate_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for separate block JS + CSS
#' @noRd
separate_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "separate-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "separate-block.js"
    ),
    htmltools::htmlDependency(
      name = "separate-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "separate-block.css"
    )
  )
}
