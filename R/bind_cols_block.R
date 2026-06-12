#' Bind cols block (JS-driven, variadic)
#'
#' Combine multiple data frames side-by-side using dplyr::bind_cols.
#' Minimal UI with no user-configurable parameters.
#' Variadic block: accepts any number of data inputs via the `...args` pattern.
#'
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_bind_cols_block(),
#'     data = list(
#'       a = data.frame(x = 1:3),
#'       b = data.frame(y = 4:6)
#'     )
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#' @importFrom stats setNames
#'
#' @export
new_bind_cols_block <- function(...) {
  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, ...args) { # nolint: object_name_linter.
      moduleServer(id, function(input, output, session) {
        arg_names <- reactive(
          setNames(names(...args), dot_args_names(...args))
        )

        list(
          expr = reactive({
            data_args <- lapply(arg_names(), as_dot_call)
            as.call(c(quote(dplyr::bind_cols), data_args))
          }),
          state = list()
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    js_block_ui("bind-cols", character(0)),
    dat_valid = function(...args) { # nolint: object_name_linter.
      stopifnot(length(...args) >= 1L)
    },
    class = "bind_cols_block",
    expr_type = "bquoted",
    allow_empty_state = TRUE,
    ...
  )
}
