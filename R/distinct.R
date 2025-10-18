#' Distinct block constructor (DEPRECATED)
#'
#' **This block is deprecated.** Use [new_select_block()] with `distinct = TRUE` instead.
#'
#' The distinct functionality has been merged into the select block for a more
#' streamlined workflow. Instead of using two blocks (`select` + `distinct`),
#' you can now use a single select block with the distinct checkbox.
#'
#' @param columns Character vector of column names to check for uniqueness.
#'   If empty, removes duplicate rows across all columns.
#' @param ... Forwarded to [new_block()]
#'
#' @section Migration:
#' Old approach (two blocks):
#' ```r
#' new_select_block(columns = c("Species", "Sepal.Length"))
#' new_distinct_block()
#' ```
#'
#' New approach (one block):
#' ```r
#' new_select_block(columns = c("Species", "Sepal.Length"), distinct = TRUE)
#' ```
#'
#' @seealso [new_select_block()] for the replacement
#'
#' @importFrom dplyr distinct
#' @importFrom glue glue
#'
#' @export
new_distinct_block <- function(
  columns = character(),
  ...
) {
  .Deprecated(
    "new_select_block",
    package = "blockr.dplyr",
    msg = "new_distinct_block() is deprecated. Use new_select_block(columns, distinct = TRUE) instead."
  )
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # Reactive for selected columns
          selected_columns <- reactiveVal(columns)

          # Get available columns from data
          available_columns <- reactive({
            req(data())
            colnames(data())
          })

          # Update selected columns from input
          observeEvent(input$columns, {
            selected_columns(input$columns)
          })

          # UI output
          output$distinct_ui <- renderUI({
            req(available_columns())

            div(
              class = "mb-3",
              selectInput(
                ns("columns"),
                label = "Select columns for uniqueness:",
                choices = available_columns(),
                selected = selected_columns(),
                multiple = TRUE,
                width = "100%"
              ),
              helpText("Leave empty to remove duplicate rows")
            )
          })

          # Return the standard blockr.core interface
          list(
            expr = reactive({
              # Build the distinct expression
              if (length(selected_columns()) == 0) {
                # If no columns selected, remove duplicates across all columns
                parse(text = "dplyr::distinct(data)")[[1]]
              } else {
                # Remove duplicates based on selected columns
                # Use backtick_if_needed to avoid double backticking
                cols_expr <- paste(
                  backtick_if_needed(selected_columns()),
                  collapse = ", "
                )
                parse(
                  text = glue::glue("dplyr::distinct(data, {cols_expr})")
                )[[1]]
              }
            }),
            state = list(
              columns = selected_columns
            )
          )
        }
      )
    },
    function(id) {
      ns <- shiny::NS(id)
      uiOutput(ns("distinct_ui"))
    },
    submit = "none",
    class = "distinct_block",
    allow_empty_state = "columns",
    ...
  )
}
