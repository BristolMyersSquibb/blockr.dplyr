#' Bind rows block (JS-driven, variadic)
#'
#' Stack multiple data frames on top of each other using dplyr::bind_rows.
#' Simple UI with an optional text input for .id column name.
#' Variadic block: accepts any number of data inputs via the `...args` pattern.
#'
#' @param state List with `id_name` (string, optional .id column name).
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_bind_rows_block(
#'       state = list(id_name = "source")
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
  state = list(id_name = ""),
  ...
) {
  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, ...args) { # nolint: object_name_linter.
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r_state <- reactiveVal(state)

        # Bidirectional sync: self_write tracks UI-initiated changes
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        # JS -> R: user changed the state
        observeEvent(input$bind_rows_input, {
          self_write$active <- TRUE
          r_state(input$bind_rows_input)
          self_write$active <- FALSE
        })

        # R -> JS: external control changed the state
        observeEvent(r_state(), {
          if (self_write$active) {
            # Skip: change originated from JS input
          } else {
            session$sendCustomMessage(
              "bind-rows-block-update",
              list(id = ns("bind_rows_input"), state = r_state())
            )
          }
        }, ignoreInit = TRUE)

        arg_names <- reactive(
          setNames(names(...args), dot_args_names(...args))
        )

        list(
          expr = reactive({
            s <- r_state()
            id_name <- s$id_name %||% ""

            data_args <- lapply(arg_names(), as_dot_call)
            expr <- as.call(c(quote(dplyr::bind_rows), data_args))
            if (nzchar(id_name)) expr[[".id"]] <- id_name

            expr
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
        bind_rows_block_dep(),
        div(
          class = "block-container",
          div(
            id = NS(id, "bind_rows_input"),
            class = "bind-rows-block-container"
          )
        )
      )
    },
    dat_valid = function(...args) { # nolint: object_name_linter.
      stopifnot(length(...args) >= 1L)
    },
    class = "bind_rows_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

#' HTML dependency for bind rows block JS + CSS
#' @noRd
bind_rows_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "bind-rows-block-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "bind-rows-block.js"
    ),
    htmltools::htmlDependency(
      name = "bind-rows-block-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "bind-rows-block.css"
    )
  )
}
