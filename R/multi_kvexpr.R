#' Multi key-value expression module for multiple expressions
#'
#' A Shiny module that manages multiple key-value pairs for expressions.
#' Supports adding and removing expressions dynamically.
#'
#' @param id The module ID
#' @param get_value Function that returns initial values as a named list/vector
#' @param get_cols Function that returns column names for autocompletion
#'
#' @return A reactive expression containing the current key-value pairs
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @noRd
#' @noRd
mod_multi_kvexpr_server <- function(id, get_value, get_cols,
                                    external_value = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    # Handle both list and named vector inputs
    initial_values <- get_value()
    if (is.character(initial_values) && !is.null(names(initial_values))) {
      # Convert named vector to list
      initial_values <- as.list(initial_values)
    }
    if (length(initial_values) == 0 || is.null(initial_values)) {
      initial_values <- list(new_col = "1")
    }

    # Store expressions as reactive value
    r_expressions <- reactiveVal(initial_values)
    r_cols <- reactive(get_cols())

    # Track which expression indices exist
    r_expr_indices <- reactiveVal(seq_along(initial_values))
    r_next_index <- reactiveVal(length(initial_values) + 1)

    # Track external updates so r_output reads r_expressions (not stale
    # form inputs) until the UI has re-rendered.
    r_external_pending <- reactiveVal(FALSE)

    # Watch for external updates (from external_ctrl / AI)
    if (!is.null(external_value)) {
      observeEvent(external_value(), {
        new_val <- external_value()
        if (is.list(new_val) && length(new_val) > 0) {
          r_external_pending(TRUE)
          r_expressions(new_val)
          r_expr_indices(seq_along(new_val))
          r_next_index(length(new_val) + 1)
        }
      }, ignoreInit = TRUE)
    }

    # Initialize ACE editors for existing expressions
    observe({
      indices <- r_expr_indices()
      for (i in indices) {
        initialize_ace_editor(session, ns(paste0("expr_", i, "_val")), r_cols())
      }
    })

    # Collect current values from all inputs
    get_current_expressions <- function() {
      indices <- r_expr_indices()
      if (length(indices) == 0) {
        return(list())
      }

      result <- list()
      for (i in indices) {
        name_id <- paste0("expr_", i, "_name")
        val_id <- paste0("expr_", i, "_val")

        name <- input[[name_id]]
        val <- input[[val_id]]

        # Allow empty names (for unnamed expressions that unpack multiple columns)
        # Only require that val is not empty
        if (!is.null(val) && val != "") {
          # Use empty string for name if it's NULL or empty
          if (is.null(name) || name == "") {
            name <- ""
          }
          result[[name]] <- val
        }
      }

      if (length(result) == 0) {
        result <- list(new_col = "1")
      }

      result
    }

    # Add new expression
    observeEvent(input$add_expr, {
      current_indices <- r_expr_indices()
      new_index <- r_next_index()

      # Add new index
      r_expr_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Get current expressions and add new one
      current <- get_current_expressions()

      # Generate unique name
      new_name <- "new_col"
      i <- 1
      while (new_name %in% names(current)) {
        new_name <- paste0("new_col_", i)
        i <- i + 1
      }
      current[[new_name]] <- "1"
      r_expressions(current)
    })

    # Remove expression handlers - create them dynamically
    observe({
      indices <- r_expr_indices()

      lapply(indices, function(i) {
        observeEvent(input[[paste0("expr_", i, "_remove")]], {
          current_indices <- r_expr_indices()

          if (length(current_indices) > 1) {
            # Remove this index
            new_indices <- setdiff(current_indices, i)
            r_expr_indices(new_indices)

            # Update expressions
            current <- get_current_expressions()
            r_expressions(current)
          }
        })
      })
    })

    # Render UI dynamically
    output$expressions_ui <- renderUI({
      indices <- r_expr_indices()
      exprs <- r_expressions()

      if (length(indices) == 0) {
        return(NULL)
      }

      expr_names <- names(exprs)
      expr_values <- unname(exprs)

      # Create UI for each expression
      tagList(
        lapply(seq_along(indices), function(j) {
          i <- indices[j]
          name <- if (j <= length(expr_names)) expr_names[j] else "new_col"
          value <- if (j <= length(expr_values)) expr_values[j] else "1"

          multi_kvexpr_row_ui(
            ns(paste0("expr_", i)),
            name = name,
            value = value,
            show_remove = (length(indices) > 1)
          )
        })
      )
    })

    # Initialize ACE editors when new ones are added
    observeEvent(r_expr_indices(), {
      indices <- r_expr_indices()
      for (i in indices) {
        if (!paste0("expr_", i, "_val") %in% names(input)) {
          # New editor, initialize it
          initialize_ace_editor(
            session,
            ns(paste0("expr_", i, "_val")),
            r_cols()
          )
        }
      }
    })

    # Return the reactive expressions
    reactive({
      # After an external update (AI ctrl), use r_expressions() directly
      # until the UI re-renders with new inputs.
      if (r_external_pending()) {
        r_external_pending(FALSE)
        return(r_expressions())
      }

      get_current_expressions()
    })
  })
}

#' Create multi key-value UI module
#'
#' @param id The module ID
#' @param extra_button Optional UI element (e.g., submit button) to display on the right side
#' @return A div containing the UI elements
#' @noRd
#' @noRd
mod_multi_kvexpr_ui <- function(id, extra_button = NULL) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      /* Style input-group to match modern form controls */
      .input-group.multi-kvexpr-expression {
        background-color: var(--blockr-color-bg-input, #f9fafb);
        border: 1px solid var(--blockr-color-border, #e5e7eb) !important;
        border-radius: 8px !important;
        height: 38px !important;
        transition: border-color 0.15s ease, box-shadow 0.15s ease, background-color 0.15s ease;
      }

      .input-group.multi-kvexpr-expression:focus-within {
        background-color: #ffffff;
        border-color: var(--blockr-color-primary, #2563eb) !important;
        box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
      }

      /* Remove border from nested mutate-expression created by setup_ace_editor */
      .multi-kvexpr-expression .mutate-expression {
        border: none !important;
        background: transparent !important;
        margin: 0 !important;
        padding: 0 !important;
      }

      .multi-kvexpr-expression .shiny-ace,
      .multi-kvexpr-expression .ace_editor,
      .multi-kvexpr-expression .ace-tomorrow,
      .multi-kvexpr-expression .ace_scroller,
      .multi-kvexpr-expression .ace_content,
      .multi-kvexpr-expression .ace_gutter {
        border: none !important;
        box-shadow: none !important;
        background: transparent !important;
        outline: none !important;
      }

      .multi-kvexpr-expression .shiny-ace {
        margin: 7px;
        margin-bottom: 7.5px;
      }

      .multi-kvexpr-expression .expr-column {
        width: 20%;
      }

      .multi-kvexpr-expression .expr-code {
        flex: 1;
      }

      .multi-kvexpr-expression .expr-equal {
        background-color: transparent;
        border: none;
        color: var(--blockr-grey-400, #9ca3af);
        padding-left: 0.5rem;
        padding-right: 0.5rem;
      }

      .multi-kvexpr-expression .blockr-btn-icon {
        margin-top: 2px;
        margin-right: 2px;
      }

      .input-group .input-group-text {
        padding-top: 0;
        padding-bottom: 0;
        height: 36px;
      }

      .multi-kvexpr-actions {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-top: 1.5rem;
      }

    "
    ),
    div(
      class = "multi-kvexpr-container",
      uiOutput(ns("expressions_ui")),
      div(
        class = "multi-kvexpr-actions mt-4 mb-1",
        actionButton(
          ns("add_expr"),
          label = "Add Expression",
          icon = icon("plus"),
          class = "btn btn-outline-secondary btn-sm"
        ),
        extra_button
      )
    )
  )
}

#' Create UI for a single expression row
#'
#' @param id Row identifier
#' @param name Column name
#' @param value Expression value
#' @param show_remove Whether to show remove button
#' @return A div containing the row UI
#' @noRd
#' @noRd
multi_kvexpr_row_ui <- function(
  id,
  name = "new_col",
  value = "1",
  show_remove = TRUE
) {
  div(
    class = paste(
      "input-group mb-2",
      "multi-kvexpr-expression border border-dark-subtle rounded"
    ),
    div(
      class = "expr-column",
      shinyAce::aceEditor(
        outputId = paste0(id, "_name"),
        debounce = 300,
        value = name,
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
      class = "input-group-text expr-equal",
      icon("equals")
    ),
    div(
      class = "expr-code",
      setup_ace_editor(paste0(id, "_val"), value = value)
    ),
    if (show_remove) {
      actionButton(
        paste0(id, "_remove"),
        label = NULL,
        icon = bsicons::bs_icon("x-lg"),
        class = "blockr-btn-icon",
        title = "Remove expression"
      )
    }
  )
}

#' Run example app demonstrating multi key-value functionality
#'
#' @noRd
run_multi_kvexpr_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      h3("Multi Expression Example"),
      mod_multi_kvexpr_ui("mkv"),
      hr(),
      h4("Current Values:"),
      verbatimTextOutput("values"),
      h4("Generated Code:"),
      verbatimTextOutput("code")
    ),
    server = function(input, output, session) {
      r_result <- mod_multi_kvexpr_server(
        "mkv",
        get_value = function() {
          list(
            mpg_double = "mpg * 2",
            hp_per_100 = "hp / 100"
          )
        },
        get_cols = function() c("mpg", "cyl", "hp", "wt", "am", "gear")
      )

      output$values <- renderPrint({
        vals <- r_result()
        if (length(vals) > 0) {
          for (name in names(vals)) {
            cat(sprintf("%s = %s\n", name, vals[[name]]))
          }
        }
      })

      output$code <- renderPrint({
        vals <- r_result()
        if (length(vals) > 0) {
          exprs <- paste(
            sprintf("%s = %s", names(vals), vals),
            collapse = ",\n  "
          )
          cat(sprintf("dplyr::mutate(data,\n  %s\n)", exprs))
        }
      })
    }
  )
}
