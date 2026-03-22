#' JS-driven expression filter block (prototype)
#'
#' Like [new_filter_expr_block()] but all condition-row management (add, remove,
#' AND/OR toggle, expression editing) happens client-side in JavaScript.
#' R is only contacted when the user clicks **Submit** — to parse and evaluate
#' the composed expression against the data.
#'
#' @inheritParams new_filter_expr_block
#' @return A block object for expression-based filter operations
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent req NS
#'   actionButton div tagList
#' @importFrom htmltools htmlDependency tags
#' @importFrom glue glue
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_board(
#'       blocks = list(
#'         a = new_dataset_block(),
#'         b = new_filter_js_block()
#'       ),
#'       links = links(from = "a", to = "b")
#'     )
#'   )
#' }
#' @export
new_filter_js_block <- function(exprs = "TRUE", ...) {
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
            list(id = ns("filter_input"), columns = colnames(data()))
          )
        })

        # Receive composed expression from JS on Submit
        observeEvent(input$filter_input, {
          new_expr <- input$filter_input
          if (is.null(new_expr) || trimws(new_expr) == "") {
            new_expr <- "TRUE"
          }
          # Validate parse only — eval is handled by the block framework
          expr <- try(parse_filter_expr(new_expr), silent = TRUE)
          if (inherits(expr, "try-error")) {
            shiny::showNotification(as.character(expr), type = "error", duration = 5)
            return()
          }
          r_exprs_rv(new_expr)
        })

        list(
          expr = reactive(parse_filter_expr(r_exprs_rv())),
          state = list(exprs = r_exprs_rv)
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    function(id) {
      tagList(
        multi_expr_dep(),
        css_responsive_grid(),
        css_single_column("filter"),
        div(
          class = "block-container filter-block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  tags$p(
                    "Filter rows with R expressions (JS prototype). ",
                    "Use Ctrl+Space for autocomplete."
                  )
                ),
                div(
                  id = NS(id, "filter_input"),
                  class = "multi-expr-container",
                  `data-mode` = "expr",
                  `data-value` = exprs,
                  `data-add-label` = "Add condition"
                )
              )
            )
          )
        )
      )
    },
    class = "filter_js_block",
    ...
  )
}

#' HTML dependencies for the multi-expr JS input binding
#' @noRd
multi_expr_dep <- function() {
  ace_dir <- system.file("www", package = "shinyAce")

  tagList(
    htmlDependency(
      name = "ace",
      version = utils::packageVersion("shinyAce"),
      src = ace_dir,
      script = c("ace/ace.js", "ace/ext-language_tools.js")
    ),
    htmlDependency(
      name = "multi-expr-input",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("assets", package = "blockr.dplyr"),
      script = "js/multi-expr-input.js",
      stylesheet = "css/multi-expr.css"
    )
  )
}
