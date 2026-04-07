#' Slice block (JS-driven)
#'
#' Subset rows by position using dplyr::slice_* family of functions.
#' Single select for type (head/tail/min/max/sample), number input for n,
#' optional column selects for order_by and weight_by, toggle pills for
#' with_ties and replace, multi-select for grouping (.by).
#' Auto-submits on any change.
#'
#' @param state List with `type` (string: "head", "tail", "min", "max",
#'   "sample"), `n` (integer), `prop` (numeric or NULL), `order_by` (string),
#'   `with_ties` (logical), `weight_by` (string), `replace` (logical), and
#'   `by` (character vector).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_slice_block(
#'       state = list(
#'         type = "head",
#'         n = 10L,
#'         prop = NULL,
#'         order_by = "",
#'         with_ties = TRUE,
#'         weight_by = "",
#'         replace = FALSE,
#'         by = list()
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
new_slice_block <- function(
  state = list(
    type = "head",
    n = 5L,
    prop = NULL,
    order_by = "",
    with_ties = TRUE,
    weight_by = "",
    replace = FALSE,
    rows = "1:5",
    by = list()
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
            "slice-columns",
            list(id = ns("slice_input"), columns = col_names)
          )
        })

        # JS -> R: user changed the state
        observeEvent(input$slice_input, {
          self_write$active <- TRUE
          r_state(input$slice_input)
          self_write$active <- FALSE
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            # Skip: change originated from JS input
          } else {
            session$sendCustomMessage(
              "slice-block-update",
              list(id = ns("slice_input"), state = r_state())
            )
          }
        })

        list(
          expr = reactive({
            s <- r_state()
            make_slice_expr(
              s$type %||% "head",
              s$n %||% 5L,
              s$prop,
              s$order_by %||% "",
              s$with_ties %||% TRUE,
              s$weight_by %||% "",
              s$replace %||% FALSE,
              s$rows %||% "1:5",
              s$by %||% list()
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
        slice_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "slice_input"),
            class = "slice-block-container"
          )
        )
      )
    },
    class = "slice_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for slice block JS + CSS
#' @noRd
slice_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "slice-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "slice-block.js"
    ),
    htmltools::htmlDependency(
      name = "slice-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "slice-block.css"
    )
  )
}
