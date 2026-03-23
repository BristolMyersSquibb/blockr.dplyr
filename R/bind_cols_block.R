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
    function(id, ...args) {
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
    function(id) {
      tagList(
        blockr_core_js_dep(),
        blockr_blocks_css_dep(),
        bind_cols_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "bind_cols_input"),
            class = "bind-cols-block-container"
          )
        )
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    class = "bind_cols_block",
    expr_type = "quoted",
    allow_empty_state = TRUE,
    ...
  )
}

#' HTML dependency for bind cols block JS + CSS
#' @noRd
bind_cols_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "bind-cols-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "bind-cols-block.js"
    ),
    htmltools::htmlDependency(
      name = "bind-cols-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "bind-cols-block.css"
    )
  )
}
