#' Unified Group By Column Selector Module
#'
#' A reusable Shiny module for selecting group by columns across all blocks
#' that support the .by parameter in dplyr operations.
#'
#' @param id Character string. Module ID.
#' @param label Character string. Label for the selector. Default: "Group by columns (optional)"
#'
#' @return For UI function, returns a shiny tag. For server function, returns a reactive
#'   containing selected column names.
#'
#' @export
mod_by_selector_ui <- function(id, label = "Group by columns (optional)") {
  ns <- NS(id)
  selectInput(
    inputId = ns("by_columns"),
    label = label,
    choices = character(),
    selected = character(),
    multiple = TRUE
  )
}

#' Group By Column Selector Server Module
#'
#' @param id Character string. Module ID.
#' @param get_cols Reactive function that returns available column names.
#' @param initial_value Character vector. Initial selected columns.
#'
#' @export
mod_by_selector_server <- function(id, get_cols, initial_value = character()) {
  moduleServer(id, function(input, output, session) {

    # Reactive to store current selection
    r_by_selection <- reactiveVal(initial_value)

    # Update choices when data changes
    observe({
      req(get_cols)
      current_cols <- get_cols()
      if (length(current_cols) > 0) {
        updateSelectInput(
          session,
          inputId = "by_columns",
          choices = current_cols,
          selected = r_by_selection()
        )
      }
    })

    # Update reactive value when selection changes
    observeEvent(input$by_columns, {
      r_by_selection(input$by_columns %||% character())
    }, ignoreNULL = FALSE)

    # Return the reactive selection
    r_by_selection
  })
}

#' Helper function for NULL default
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
