#' Mutate key-value server module
#'
#' A Shiny module that manages a key-value pair for mutate expressions.
#' Handles user input, autocompletion, and value updates.
#'
#' @param id The module ID
#' @param get_value Function that returns initial value
#' @param get_cols Function that returns column names for autocompletion
#'
#' @return A reactive expression containing the current key-value pair
#' @importFrom shiny req showNotification NS moduleServer reactive
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' serve(new_mutate_block(), list(data = mtcars))
#' }
#' @export
mod_kvexpr_server <- function(
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
          value = unname(val)
        )
        shinyAce::updateAceEditor(
          session,
          editorId = "pl_name",
          value = names(val)
        )
      }
    })

    # Handle user input
    observe({
      name <- input$pl_name
      val <- input$pl_val

      if (!is.null(name) && !is.null(val)) {
        r_user_input(TRUE)
        r_value(setNames(val, name))
      }
    })

    # Return reactive value
    reactive(r_value())
  })
}

#' Create key-value UI module
#'
#' @param id The module ID
#' @return A div containing the UI elements
#' @export
mod_kvexpr_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style("
      .mutate-expression .shiny-ace {
        border: none;
        margin: 7px;
        margin-bottom: 7.5px;   /* to align with select box */
      }

      .mutate-expression .mutate-column {
        width: 15%;
      }

      .mutate-expression .mutate-code {
        flex: 1;
      }

      .mutate-expression .mutate-equal {
        background-color: #e9ecef;
        border-color: #dee2e6;
        padding-left: 0.75rem;
        padding-right: 0.75rem;
      }

      .mutate-expression .input-group {
        border: none !important;
      }

      .input-group.mutate-expression {
        height: 38px !important;
      }

     .input-group .input-group-text {
        padding-top: 0;
        padding-bottom: 0;
        height: 36px;
      }

    "),
    div(
      id = ns("pl"),
      class = paste(
        "input-group mb-3",
        "mutate-expression border border-dark-subtle rounded"
      ),
      div(
        class = "mutate-column",
        shinyAce::aceEditor(
          outputId = ns("pl_name"),
          debounce = 300,
          value = "newcol",
          mode = "r",
          autoComplete = "disabled",
          height = "20px",
          showPrintMargin = FALSE,
          highlightActiveLine = FALSE,
          tabSize = 2,
          theme = "tomorrow",
          maxLines = 1,
          fontSize = 14,
          showLineNumbers = FALSE
        )
      ),
      div(
        class = "input-group-text mutate-equal",
        icon("equals")
      ),
      div(
        class = "mutate-code",
        setup_ace_editor(ns("pl_val"))
      )
    )
  )
}

#' Run example app demonstrating key-value functionality
#'
#' This function launches a Shiny app that demonstrates the key-value module
#' functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all();run_kvexpr_example()
#' }
#' @export
run_kvexpr_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      mod_kvexpr_ui("kv"),
      verbatimTextOutput("value")
    ),
    server = function(input, output, session) {
      r_ans <- mod_kvexpr_server(
        "kv",
        get_value = function() c(newcol = "x + 1"),
        get_cols = function() c("x", "y", "z")
      )

      output$value <- renderPrint({
        val <- r_ans()
        cat(names(val), "\n")
        cat(sprintf('"%s"', unname(val)))
      })
    }
  )
}
