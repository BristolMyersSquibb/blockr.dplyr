#' Join block (JS-driven)
#'
#' Binary join block combining key-based joins (equi and non-equi) with
#' expression mode (free R code for join conditions) in a single JS-driven UI.
#' Takes two data inputs (x and y).
#'
#' @param state List with:
#'   - `type`: join function name ("left_join", "inner_join", "right_join",
#'     "full_join", "semi_join", "anti_join")
#'   - `keys`: list of key objects, each with `xCol`, `op`, `yCol`
#'   - `exprs`: character vector of R expression strings for join_by()
#'   - `suffix_x`: suffix for x columns (default ".x")
#'   - `suffix_y`: suffix for y columns (default ".y")
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_join_block(
#'       state = list(
#'         type = "left_join",
#'         keys = list(
#'           list(xCol = "id", op = "==", yCol = "id")
#'         ),
#'         exprs = list(),
#'         suffix_x = ".x",
#'         suffix_y = ".y"
#'       )
#'     ),
#'     data = list(x = iris, y = iris)
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList req
#' @importFrom htmltools htmlDependency
#'
#' @export
new_join_block <- function(
  state = list(type = "left_join", keys = list(), exprs = list(),
               suffix_x = ".x", suffix_y = ".y"),
  ...
) {
  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, x, y) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- reactiveVal(state)

        # Bidirectional sync: self_write tracks UI-initiated changes
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        # Send column metadata to JS when either data input changes
        observeEvent(list(x(), y()), {
          session$sendCustomMessage(
            "join-columns",
            list(
              id = ns("join_input"),
              xColumns = colnames(x()),
              yColumns = colnames(y())
            )
          )
        })

        # JS -> R: user changed the join config
        observeEvent(input$join_input, {
          self_write$active <- TRUE
          r_state(input$join_input)
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "join-block-update",
              list(id = ns("join_input"), state = r_state())
            )
          }
        }, ignoreInit = TRUE)

        list(
          expr = reactive({
            s <- r_state()
            make_join_expr(
              s$type %||% "left_join",
              s$keys %||% list(),
              s$exprs %||% list(),
              s$suffix_x %||% ".x",
              s$suffix_y %||% ".y"
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
        join_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "join_input"),
            class = "join-block-container"
          )
        )
      )
    },
    dat_valid = function(x, y) {
      stopifnot(is.data.frame(x), is.data.frame(y))
    },
    class = "join_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for join block JS + CSS
#' @noRd
join_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "join-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "join-block.js"
    ),
    htmltools::htmlDependency(
      name = "join-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "join-block.css"
    )
  )
}
