#' Pivot wider block (JS-driven)
#'
#' Reshape data from long to wide format using tidyr::pivot_wider.
#' Multi-selects for names_from, values_from, and id_cols (optional).
#' Text inputs for values_fill, names_sep, names_prefix.
#' Auto-submits on any change.
#'
#' @param state List with `names_from` (character vector), `values_from`
#'   (character vector), `id_cols` (character vector, optional),
#'   `values_fill` (value or NULL), `names_sep` (string), and
#'   `names_prefix` (string).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_pivot_wider_block(
#'       state = list(
#'         names_from = list("Species"),
#'         values_from = list("Sepal.Length"),
#'         id_cols = list(),
#'         values_fill = NULL,
#'         names_sep = "_",
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
new_pivot_wider_block <- function(
  state = list(
    names_from = list(),
    values_from = list(),
    id_cols = list(),
    values_fill = NULL,
    names_sep = "_",
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
            "pivot-wider-columns",
            list(id = ns("pivot_wider_input"), columns = col_names)
          )
        })

        # JS -> R: user changed the state
        observeEvent(input$pivot_wider_input, {
          self_write$active <- TRUE
          r_state(input$pivot_wider_input)
          self_write$active <- FALSE
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            # Skip: change originated from JS input
          } else {
            session$sendCustomMessage(
              "pivot-wider-block-update",
              list(id = ns("pivot_wider_input"), state = r_state())
            )
          }
        }, ignoreInit = TRUE)

        list(
          expr = reactive({
            s <- r_state()
            make_pivot_wider_expr(
              s$names_from %||% list(),
              s$values_from %||% list(),
              s$id_cols %||% list(),
              s$values_fill,
              s$names_sep %||% "_",
              s$names_prefix %||% "",
              s$values_fn %||% ""
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
        pivot_wider_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "pivot_wider_input"),
            class = "pivot-wider-block-container"
          )
        )
      )
    },
    class = "pivot_wider_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for pivot wider block JS + CSS
#' @noRd
pivot_wider_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "pivot-wider-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "pivot-wider-block.js"
    ),
    htmltools::htmlDependency(
      name = "pivot-wider-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "pivot-wider-block.css"
    )
  )
}
