#' Generic Multi-Column Selector Module (Internal)
#'
#' A reusable Shiny module for selecting multiple columns from a dataset.
#' This is the base module used by more specialized selectors.
#'
#' @param id Character string. Module ID.
#' @param label Label for the selector (character or tag).
#' @param initial_choices Character vector. Initial choices for the selector.
#' @param initial_selected Character vector. Initial selected values.
#' @param width Width of the input (e.g., "100%", "300px", NULL for default).
#'
#' @return For UI function, returns a shiny tag. For server function, returns a reactive
#'   containing selected column names.
#'
#' @noRd
#' @noRd
mod_column_selector_ui <- function(
  id,
  label,
  initial_choices = character(),
  initial_selected = character(),
  width = NULL
) {
  ns <- NS(id)
  selectInput(
    inputId = ns("columns"),
    label = label,
    choices = initial_choices,
    selected = initial_selected,
    multiple = TRUE,
    width = width
  )
}

#' Generic Multi-Column Selector Server Module (Internal)
#'
#' @param id Character string. Module ID.
#' @param get_cols Reactive function that returns available column names.
#' @param initial_value Character vector. Initial selected columns.
#'
#' @noRd
#' @noRd
mod_column_selector_server <- function(id, get_cols, initial_value = character()) {
  moduleServer(id, function(input, output, session) {
    # Reactive to store current selection
    r_selection <- reactiveVal(initial_value)

    # Update reactive value when selection changes
    observeEvent(
      input$columns,
      {
        r_selection(input$columns %||% character())
      }
    )

    # Update choices when data changes, preserving selection
    observeEvent(get_cols(), {
      current_cols <- get_cols()
      if (length(current_cols) > 0) {
        updateSelectInput(
          session,
          inputId = "columns",
          choices = current_cols,
          selected = r_selection()
        )
      }
    })

    # Return the reactive selection
    r_selection
  })
}

#' Null coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
