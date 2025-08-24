#' Value expression server module
#'
#' A Shiny module that manages a single expression value.
#' Handles user input and autocompletion.
#'
#' @param id The module ID
#' @param get_value Function that returns initial value
#' @param get_cols Function that returns column names for autocompletion
#'
#' @return A reactive expression containing the current expression value
#' @importFrom shiny req showNotification NS moduleServer reactive
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' serve(new_filter_block(), list(data = mtcars))
#' }
#' @keywords internal
mod_vexpr_server <- function(
  id,
  get_value,
  get_cols
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    r_value <- reactiveVal({
      get_value()
    })
    r_cols <- reactive({
      get_cols()
    })

    # Track if value was set by user
    r_user_input <- reactiveVal(FALSE)

    # Initialize ace editor with custom completions
    observe({
      initialize_ace_editor(session, ns("pl_val"), r_cols())
    })

    # Update editor values for non-user changes
    observe({
      val <- r_value()
      if (!r_user_input()) {
        shinyAce::updateAceEditor(
          session,
          editorId = "pl_val",
          value = val
        )
      }
    })

    # Handle user input
    observe({
      val <- input$pl_val

      if (!is.null(val)) {
        r_user_input(TRUE)
        r_value(val)
      }
    })

    # Return reactive value
    reactive(r_value())
  })
}

#' Create value expression UI module
#'
#' @param id The module ID
#' @return A div containing the UI elements
#' @keywords internal
mod_vexpr_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      .filter-expression .shiny-ace {
        border: none;
        margin: 7px;
        margin-bottom: 7.5px;
      }

      .filter-expression .filter-code {
        flex: 1;
      }

      .filter-expression .input-group {
        border: none !important;
      }

      .input-group.filter-expression {
        height: 38px !important;
      }
    "
    ),
    div(
      id = ns("pl"),
      class = paste(
        "input-group mb-3",
        "filter-expression border border-dark-subtle rounded"
      ),
      div(
        class = "filter-code",
        setup_ace_editor(ns("pl_val"))
      )
    )
  )
}

#' Run example app demonstrating value expression functionality
#'
#' This function launches a Shiny app that demonstrates the value expression module
#' functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all()
#' run_vexpr_example()
#' }
#' @keywords internal
run_vexpr_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      mod_vexpr_ui("v"),
      verbatimTextOutput("value")
    ),
    server = function(input, output, session) {
      r_ans <- mod_vexpr_server(
        "v",
        get_value = function() "x > 1",
        get_cols = function() c("x", "y", "z")
      )

      output$value <- renderPrint({
        val <- r_ans()
        cat(sprintf('"%s"', val))
      })
    }
  )
}
