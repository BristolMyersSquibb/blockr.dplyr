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

        # Push column names to JS whenever upstream data changes
        observeEvent(colnames(data()), {
          session$sendCustomMessage(
            "filter-js-update-columns",
            list(
              id = ns("filter_input"),
              columns = colnames(data())
            )
          )
        })

        # Receive the composed expression from JS on Submit
        observeEvent(input$filter_input, {
          new_expr <- input$filter_input
          if (is.null(new_expr) || trimws(new_expr) == "") {
            new_expr <- "TRUE"
          }
          # Validate: try to parse and eval against the actual data
          expr <- try(parse_filter_expr(new_expr), silent = TRUE)
          if (inherits(expr, "try-error")) {
            shiny::showNotification(
              as.character(expr),
              type = "error",
              duration = 5
            )
            return()
          }
          ans <- try(eval(expr, list(data = data())), silent = TRUE)
          if (inherits(ans, "try-error")) {
            shiny::showNotification(
              as.character(ans),
              type = "error",
              duration = 5
            )
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
        filter_js_dep(),
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
                # The JS input binding owns this div
                div(
                  id = NS(id, "filter_input"),
                  class = "filter-js-container",
                  `data-value` = exprs
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

#' HTML dependency for the filter-js input binding
#' @noRd
filter_js_dep <- function() {
  ace_dir <- system.file("www", package = "shinyAce")

  tagList(
    # ACE editor + language tools (from shinyAce)
    htmlDependency(
      name = "ace",
      version = utils::packageVersion("shinyAce"),
      src = ace_dir,
      script = c("ace/ace.js", "ace/ext-language_tools.js")
    ),
    # Our input binding + styles
    htmlDependency(
      name = "filter-js-input",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("assets", package = "blockr.dplyr"),
      script = "js/filter-js-input.js",
      stylesheet = "css/filter-js.css"
    )
  )
}
