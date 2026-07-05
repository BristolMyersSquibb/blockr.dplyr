#' Bind rows block (JS-driven, variadic)
#'
#' Stack multiple data frames on top of each other using dplyr::bind_rows.
#' Simple UI with an optional text input for .id column name.
#' Variadic block: accepts any number of data inputs via the `...args` pattern.
#'
#' @param id_name Optional name for the `.id` column identifying the source
#'   data frame of each row (empty string = no id column).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_bind_rows_block(
#'       id_name = "source"
#'     ),
#'     data = list(a = iris[1:5, ], b = iris[6:10, ])
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#' @importFrom stats setNames
#'
#' @export
new_bind_rows_block <- function(
  id_name = "",
  ...
) {
  state <- list(id_name = id_name)
  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, ...args) { # nolint: object_name_linter.
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        sync <- js_block_state(input, session, "bind-rows",
                               "bind_rows_input", state)

        # Eval-env symbols for the connected inputs (.arg1, .arg2, ... for
        # unnamed DAG-UI slots, else the link name); reactive on the link set.
        arg_names <- reactive(
          dot_arg_refs(...args)
        )

        list(
          expr = reactive({
            s <- sync$state()
            id_name <- s$id_name %||% ""

            data_args <- lapply(arg_names(), as_dot_call)
            expr <- as.call(c(quote(dplyr::bind_rows), data_args))
            if (nzchar(id_name)) expr[[".id"]] <- id_name

            expr
          }),
          state = sync$fields
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    js_block_ui("bind-rows", character(0)),
    dat_valid = function(...args) { # nolint: object_name_linter.
      stopifnot(length(...args) >= 1L)
    },
    class = "bind_rows_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = TRUE,
    ...
  )
}
