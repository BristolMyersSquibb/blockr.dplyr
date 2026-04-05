#' Column Picker UI / Server
#'
#' Simple selectize-based column picker module used by downstream packages
#' (e.g. blockr.extra).
#'
#' @param id Module namespace id
#' @param label Label for the input
#' @param choices Initial choices
#' @param selected Initial selection
#'
#' @return `column_picker_ui` returns a `tagList`; `column_picker_server`
#'   returns a reactive holding the current selection.
#'
#' @export
column_picker_ui <- function(id, label = "Columns", choices = NULL,
                             selected = NULL) {
  shiny::selectizeInput(
    inputId = shiny::NS(id, "cols"),
    label = label,
    choices = choices %||% character(),
    selected = selected,
    multiple = TRUE,
    width = "100%"
  )
}

#' @param get_choices Reactive returning the available column names.
#' @param initial_value ReactiveVal holding the initial selection.
#' @rdname column_picker_ui
#' @export
column_picker_server <- function(id, get_choices, initial_value = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveVal(
      if (is.reactive(initial_value)) initial_value() else character()
    )

    shiny::observe({
      ch <- get_choices()
      sel <- rv()
      sel <- intersect(sel, ch)
      shiny::updateSelectizeInput(
        session, "cols",
        choices = ch,
        selected = sel,
        server = FALSE
      )
    })

    shiny::observeEvent(input$cols, {
      rv(input$cols)
    }, ignoreNULL = FALSE)

    rv
  })
}
