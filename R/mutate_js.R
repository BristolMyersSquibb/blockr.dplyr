#' JS-driven mutate expression block (prototype)
#'
#' Like [new_mutate_expr_block()] but all row management happens in JavaScript.
#' R is only contacted on Submit — to parse and evaluate the expressions.
#'
#' @param exprs Named list of expressions (name = column name, value = R expression)
#' @param ... Additional arguments forwarded to [new_block()]
#' @return A block object for mutate operations
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent req NS div tagList
#' @importFrom htmltools htmlDependency tags
#' @importFrom jsonlite toJSON
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_mutate_js_block(), data = list(data = mtcars))
#' }
#' @export
new_mutate_js_block <- function(exprs = list(new_col = "1"), ...) {
  initial_json <- jsonlite::toJSON(exprs, auto_unbox = TRUE)

  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_exprs_rv <- reactiveVal(exprs)

        # Push column names to JS
        observeEvent(colnames(data()), {
          session$sendCustomMessage(
            "multi-expr-update-columns",
            list(id = ns("expr_input"), columns = colnames(data()))
          )
        })

        # Receive kvexpr from JS on Submit
        observeEvent(input$expr_input, {
          new_exprs <- input$expr_input
          if (is.null(new_exprs) || length(new_exprs) == 0) return()

          # Convert from list to named character vector
          expr_vec <- unlist(new_exprs)

          # Validate parse only — eval is handled by the block framework
          expr <- try(parse_mutate(expr_vec), silent = TRUE)
          if (inherits(expr, "try-error")) {
            shiny::showNotification(as.character(expr), type = "error", duration = 5)
            return()
          }
          r_exprs_rv(as.list(expr_vec))
        })

        list(
          expr = reactive(parse_mutate(unlist(r_exprs_rv()))),
          state = list(exprs = r_exprs_rv)
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    function(id) {
      tagList(
        multi_expr_dep(),
        css_responsive_grid(),
        css_single_column("mutate"),
        div(
          class = "block-container mutate-block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  tags$p("Create or modify columns with R expressions (JS prototype).")
                ),
                div(
                  id = NS(id, "expr_input"),
                  class = "multi-expr-container",
                  `data-mode` = "kvexpr",
                  `data-value` = initial_json,
                  `data-add-label` = "Add expression"
                )
              )
            )
          )
        )
      )
    },
    class = "mutate_js_block",
    ...
  )
}
