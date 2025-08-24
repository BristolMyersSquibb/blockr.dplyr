#' Convert expression to selectable columns
#'
#' @param expr Expression string
#' @param cols Available column names
#' @return Character vector of valid column names or NULL if invalid format
#' @examples
#' # Valid cases (return column names)
#' df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
#' expr_to_cols("x, y", colnames(df)) # c("x", "y")
#' expr_to_cols("z", colnames(df)) # "z"
#'
#' # Invalid cases (return NULL)
#' expr_to_cols("paste(x, y)", colnames(df)) # NULL (not comma-separated)
#' expr_to_cols("foo, bar", colnames(df)) # NULL (invalid columns)
#' expr_to_cols("", colnames(df)) # NULL (empty)
#' expr_to_cols(NULL, colnames(df)) # NULL
#' @noRd
expr_to_cols <- function(expr, cols) {
  if (is.null(expr) || !nzchar(expr)) {
    return(NULL)
  }

  # Split by comma and trim whitespace
  potential_cols <- trimws(strsplit(expr, ",")[[1]])

  # Check if all are valid columns
  if (all(potential_cols %in% cols)) {
    return(potential_cols)
  }
  NULL
}

#' Flexible expression server module
#'
#' A Shiny module that manages flexible expressions.
#'
#' @param id The module ID
#' @param get_value Function that returns initial values
#' @param get_cols Function that returns column names for autocompletion
#' @param get_cols_expr Function that returns column names for expression autocompletion (defaults to get_cols)
#' @param submit Whether to show a submit button (defaults to TRUE)
#'
#' @return A reactive expression containing the current expressions
#' @importFrom shiny req showNotification NS moduleServer reactive updateSelectInput
#' @importFrom glue glue
#' @examples
#' \dontrun{
#' serve(new_flexpr_block(), list(data = mtcars))
#' }
#' @export
mod_flexpr_server <- function(
    id,
    get_value,
    get_cols,
    get_cols_expr = get_cols,
    submit = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize ace editor with custom completions
    observe({
      initialize_ace_editor(session, ns("expr"), get_cols_expr())
    })

    # Initialize reactive values
    r_value <- reactiveVal()
    observe({
      r_value(get_value())
    })

    # Initialize tooltips
    observe({
      shinyjs::runjs("$('[data-bs-toggle=\"tooltip\"]').tooltip()")
    })

    # Set initial mode based on value format
    observe({
      initial_mode <- !is.null(expr_to_cols(get_value(), get_cols()))
      updateCheckboxInput(session, "use_expr", value = !initial_mode)
    })

    # Update expression input when switching to expression mode
    observeEvent(input$use_expr, {
      if (isTruthy(input$use_expr)) {
        shinyAce::updateAceEditor(
          session,
          "expr",
          value = r_value()
        )
      }
    })

    # Update select input when switching to select mode
    observeEvent(input$use_expr, {
      if (!isTruthy(input$use_expr)) {
        selected <- expr_to_cols(r_value(), get_cols())
        if (!is.null(selected)) {
          updateSelectInput(
            session,
            "select",
            choices = get_cols(),
            selected = selected
          )
        }
      }
    })

    # Get current value based on mode
    r_current <- reactive({
      if (isTruthy(input$use_expr)) {
        if (!is.null(input$expr)) input$expr else ""
      } else {
        if (!is.null(input$select) && length(input$select) > 0) {
          paste(input$select, collapse = ", ")
        } else {
          ""
        }
      }
    })

    # Update value when mode changes
    observeEvent(r_current(),
      {
        r_value(r_current())
      },
      ignoreInit = TRUE
    )

    reactive(r_value())
  })
}

#' Create flexible expression UI module
#'
#' @param ns Namespace function
#'
#' @return A div containing the UI elements
#' @importFrom shiny NS actionButton icon div selectInput checkboxInput
#' @importFrom htmltools tagList tags
#' @export
mod_flexpr_ui <- function(ns = function(x) x) {
  div(
    div(
      class = "input-group mb-3",
      div(
        class = "form-control p-0",
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("use_expr")),
          selectInput(
            ns("select"),
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            selected = NULL,
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("use_expr")),
          setup_ace_editor(ns("expr"))
        )
      ),
      div(
        class = "input-group-text",
        div(
          class = "mode-toggle form-check form-switch m-0 p-0",
          icon("code"),
          tags$input(
            class = "form-check-input",
            type = "checkbox",
            id = ns("use_expr"),
            role = "switch",
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "left",
            title = "Toggle between selection mode (off) and expression mode (on)"
          )
        )
      )
    ),

    # Add custom CSS
    tags$style("
      .mode-toggle {
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }
      .mode-toggle .form-check-input {
        cursor: pointer;
        width: 2em;
        margin: 0;
      }
      .mode-toggle .fa-code {
        color: var(--bs-gray-600);
        font-size: 0.9em;
      }
      .input-group .mutate-expression {
        border: none !important;
        height: 38px;
      }
      .input-group {
        height: 40px;
      }
      .input-group .input-group-text {
        padding-top: 0;
        padding-bottom: 0;
        height: 40px;
      }
      .input-group .form-control {
        height: 40px;
      }
      .input-group .selectize-input {
        min-height: 30px;
        padding-top: 6px;
        border: none !important;
        box-shadow: none !important;
      }
    ")
  )
}

#' Run example app demonstrating flexible expression functionality
#'
#' This function launches a Shiny app that demonstrates the flexible expression
#' module functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all()
#' run_flexpr_example()
#' }
#' @keywords internal
run_flexpr_example <- function() {
  df <- data.frame(x = 1:10, y = 11:20, z = 21:30)

  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      div(
        class = "container mt-3",
        mod_flexpr_ui(ns = NS("flexpr")),
        verbatimTextOutput("value")
      )
    ),
    server = function(input, output, session) {
      r_ans <- mod_flexpr_server(
        "flexpr",
        get_value = function() "x, y",
        get_cols = function() colnames(df)
      )

      output$value <- renderPrint({
        r_ans()
      })
    }
  )
}
