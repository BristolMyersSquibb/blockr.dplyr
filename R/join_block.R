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

        # Send column summary (name + label + type) when either input changes
        observeEvent(list(x(), y()), {
          session$sendCustomMessage(
            "join-columns",
            list(
              id = ns("join_input"),
              xColumns = build_column_picker_meta(x()),
              yColumns = build_column_picker_meta(y())
            )
          )
        })

        js_block_sync(input, session, "join", "join_input", r_state)

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
    js_block_ui("join", c("select", "input")),
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
