#' Column Picker Module
#'
#' A reusable Shiny module for selecting multiple columns with bulk actions
#' (Select All / Select None) and drag-and-drop reordering.
#'
#' This module is intended to eventually move to a \code{blockr.ui} package.
#' For now it lives in blockr.dplyr so that blocks here (and blockr.bi
#' via import) can use it.
#'
#' @param id Module ID.
#' @param label Label text displayed above the selectize input.
#' @param choices Initial choices (character vector).
#' @param selected Initially selected values (character vector).
#' @param width CSS width for the selectize input (default \code{"100\%"}).
#' @param plugins Selectize plugins to enable (default: drag_drop, remove_button).
#' @param placeholder Placeholder text when no columns are selected.
#' @param bulk_actions Logical. Show "All | None" action links (default TRUE).
#'
#' @return \code{column_picker_ui} returns a \code{shiny.tag}.
#'
#' @name column_picker
#' @importFrom shiny actionLink reactiveVal
#' @export
column_picker_ui <- function(
  id,
  label,
  choices = character(),
  selected = character(),
  width = "100%",
  plugins = c("drag_drop", "remove_button"),
  placeholder = "Select columns...",
  bulk_actions = TRUE
) {
  ns <- NS(id)

  picker_label <- if (bulk_actions) {
    tagList(
      span(label),
      span(
        class = "blockr-column-picker-actions",
        actionLink(ns("select_all"), "All"),
        actionLink(ns("select_none"), "None")
      )
    )
  } else {
    label
  }

  div(
    class = "blockr-column-picker",

    tags$style(HTML("
      .blockr-column-picker {
        display: contents;
      }
      .blockr-column-picker .control-label {
        width: 100%;
      }
      .blockr-column-picker-actions {
        float: right;
      }
      .blockr-column-picker-actions a {
        margin-left: 6px;
        color: #9ca3af;
        cursor: pointer;
      }
      .blockr-column-picker-actions a:hover {
        color: #2563eb;
        text-decoration: underline;
      }
    ")),

    selectizeInput(
      ns("selection"),
      label = picker_label,
      choices = choices,
      selected = selected,
      multiple = TRUE,
      width = width,
      options = list(
        plugins = as.list(plugins),
        persist = FALSE,
        placeholder = placeholder
      )
    )
  )
}

#' Column Picker Server
#'
#' @param id Module ID (must match the \code{id} passed to
#'   \code{column_picker_ui}).
#' @param get_choices Reactive returning available column names
#'   (e.g., \code{reactive(colnames(data()))}).
#' @param initial_value Character vector or \code{reactiveVal}. When a
#'   \code{reactiveVal} is supplied the module two-way-binds to it
#'   (external changes update the UI).
#'
#' @return A \code{reactiveVal} holding the current column selection.
#'
#' @rdname column_picker
#' @export
column_picker_server <- function(id, get_choices, initial_value = character()) {
  moduleServer(id, function(input, output, session) {
    r_selection <- if (inherits(initial_value, "reactiveVal")) {
      initial_value
    } else {
      reactiveVal(initial_value)
    }

    r_initialized <- reactiveVal(FALSE)

    # input -> state sync
    observeEvent(input$selection, {
      r_selection(input$selection)
    })

    # state -> UI reverse sync (for external_ctrl)
    observeEvent(r_selection(), {
      if (r_initialized()) {
        updateSelectizeInput(session, "selection",
          choices = get_choices(), selected = r_selection()
        )
      }
    }, ignoreInit = TRUE)

    # Initialize on first data availability
    observe({
      choices <- get_choices()
      if (!r_initialized() && length(choices) > 0) {
        updateSelectizeInput(session, "selection",
          choices = choices, selected = r_selection()
        )
        r_initialized(TRUE)
      }
    })

    # Update choices when data changes
    observeEvent(get_choices(), {
      if (r_initialized()) {
        req(get_choices())
        updateSelectizeInput(session, "selection",
          choices = get_choices(), selected = r_selection()
        )
      }
    }, ignoreNULL = FALSE)

    # Bulk actions
    observeEvent(input$select_all, {
      r_selection(get_choices())
    })

    observeEvent(input$select_none, {
      r_selection(character())
    })

    r_selection
  })
}
