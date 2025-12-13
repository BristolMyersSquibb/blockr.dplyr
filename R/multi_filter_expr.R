#' Multi filter condition module for multiple filter expressions
#'
#' A Shiny module that manages multiple filter conditions with AND/OR logic.
#' Supports adding and removing conditions dynamically.
#'
#' @param id The module ID
#' @param get_value Function that returns initial values as a character vector or list
#' @param get_cols Function that returns column names for autocompletion
#'
#' @return A reactive expression containing the current filter conditions
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @noRd
#' @noRd
mod_multi_filter_server <- function(id, get_value, get_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    initial_value <- get_value()
    if (is.character(initial_value) && length(initial_value) == 1) {
      # Parse single string into conditions (basic implementation)
      if (initial_value == "" || is.null(initial_value)) {
        initial_conditions <- list("TRUE")
      } else {
        # Preserve the actual condition
        initial_conditions <- list(initial_value)
      }
    } else if (is.list(initial_value)) {
      initial_conditions <- initial_value
    } else {
      initial_conditions <- list("TRUE")
    }

    # Store conditions as reactive value
    r_conditions <- reactiveVal(initial_conditions)
    r_cols <- reactive(get_cols())

    # Track which condition indices exist
    r_condition_indices <- reactiveVal(seq_along(initial_conditions))
    r_next_index <- reactiveVal(length(initial_conditions) + 1)

    # Track AND/OR logic between conditions
    r_logic_operators <- reactiveVal(rep(
      "&",
      max(0, length(initial_conditions) - 1)
    ))

    # Initialize ACE editors for existing conditions
    observe({
      indices <- r_condition_indices()
      for (i in indices) {
        initialize_ace_editor(session, ns(paste0("condition_", i)), r_cols())
      }
    })

    # Collect current values from all inputs
    get_current_conditions <- function() {
      indices <- r_condition_indices()
      if (length(indices) == 0) {
        return(list())
      }

      result <- list()
      for (i in indices) {
        condition_id <- paste0("condition_", i)
        condition <- input[[condition_id]]

        if (!is.null(condition) && condition != "") {
          result <- append(result, condition)
        }
      }

      if (length(result) == 0) {
        result <- list("TRUE")
      }

      result
    }

    # Get current logic operators
    get_current_logic <- function() {
      indices <- r_condition_indices()
      if (length(indices) <= 1) {
        return(character(0))
      }

      operators <- character(0)
      for (i in seq_len(length(indices) - 1)) {
        logic_id <- paste0("logic_", i)
        op <- input[[logic_id]]
        if (is.null(op)) {
          op <- "&"
        } # Default to AND
        operators <- c(operators, op)
      }
      operators
    }

    # Add new condition
    observeEvent(input$add_condition, {
      current_indices <- r_condition_indices()
      new_index <- r_next_index()

      # Add new index
      r_condition_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Add new logic operator if we have more than one condition
      current_logic <- r_logic_operators()
      if (length(current_indices) >= 1) {
        r_logic_operators(c(current_logic, "&")) # Default to AND
      }

      # Update conditions
      current <- get_current_conditions()
      current <- append(current, "TRUE")
      r_conditions(current)
    })

    # Remove condition handlers - create them dynamically
    observe({
      indices <- r_condition_indices()

      lapply(indices, function(i) {
        observeEvent(input[[paste0("condition_", i, "_remove")]], {
          current_indices <- r_condition_indices()

          if (length(current_indices) > 1) {
            # Remove this index
            new_indices <- setdiff(current_indices, i)
            r_condition_indices(new_indices)

            # Update logic operators (remove one if needed)
            current_logic <- r_logic_operators()
            if (length(current_logic) >= length(new_indices)) {
              # Remove the last operator
              r_logic_operators(current_logic[-length(current_logic)])
            }

            # Update conditions
            current <- get_current_conditions()
            r_conditions(current)
          }
        })
      })
    })

    # Update logic operators when they change
    observe({
      indices <- r_condition_indices()
      if (length(indices) > 1) {
        new_logic <- get_current_logic()
        r_logic_operators(new_logic)
      }
    })

    # Render UI dynamically
    output$conditions_ui <- renderUI({
      indices <- r_condition_indices()
      conditions <- r_conditions()
      logic_ops <- r_logic_operators()

      if (length(indices) == 0) {
        return(NULL)
      }

      # Create UI for each condition
      ui_elements <- list()

      for (j in seq_along(indices)) {
        i <- indices[j]
        condition <- if (j <= length(conditions)) conditions[[j]] else "TRUE"

        # Add the condition row
        ui_elements <- append(
          ui_elements,
          list(
            multi_filter_condition_ui(
              ns(paste0("condition_", i)),
              value = condition,
              show_remove = (length(indices) > 1)
            )
          )
        )

        # Add logic operator dropdown between conditions (except after last)
        if (j < length(indices)) {
          logic_value <- if (j <= length(logic_ops)) logic_ops[j] else "&"
          ui_elements <- append(
            ui_elements,
            list(
              div(
                class = "d-flex justify-content-start my-2", # Changed from center to start
                selectInput(
                  ns(paste0("logic_", j)),
                  label = NULL,
                  choices = list("AND" = "&", "OR" = "|"),
                  selected = logic_value,
                  width = "80px"
                )
              )
            )
          )
        }
      }

      tagList(ui_elements)
    })

    # Initialize ACE editors when new ones are added
    observeEvent(r_condition_indices(), {
      indices <- r_condition_indices()
      for (i in indices) {
        condition_id <- paste0("condition_", i)
        if (!condition_id %in% names(input)) {
          # New editor, initialize it
          initialize_ace_editor(session, ns(condition_id), r_cols())
        }
      }
    })

    # Return the reactive conditions as a combined string
    reactive({
      # Check if any inputs exist yet - if not, use stored conditions
      indices <- r_condition_indices()
      has_inputs <- any(sapply(indices, function(i) {
        paste0("condition_", i) %in% names(input)
      }))

      if (has_inputs) {
        # Use current input values
        conditions <- get_current_conditions()
        logic_ops <- r_logic_operators()
      } else {
        # Use stored conditions (for initialization)
        conditions <- r_conditions()
        logic_ops <- r_logic_operators()
      }

      if (length(conditions) == 0) {
        return("TRUE")
      } else if (length(conditions) == 1) {
        return(conditions[[1]])
      } else {
        # Combine conditions with logic operators
        result <- conditions[[1]]
        for (i in seq_len(length(conditions) - 1)) {
          op <- if (i <= length(logic_ops)) logic_ops[i] else "&"
          result <- paste(result, op, conditions[[i + 1]])
        }
        return(result)
      }
    })
  })
}

#' Create multi filter UI module
#'
#' @param id The module ID
#' @param extra_button Optional extra button (e.g., submit button) to place next to Add button
#' @return A div containing the UI elements
#' @noRd
#' @noRd
mod_multi_filter_ui <- function(id, extra_button = NULL) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      .multi-filter-container {
        margin-top: -8px;
      }

      .multi-filter-condition .shiny-ace {
        border: none;
        margin: 7px;
        margin-bottom: 7.5px;
      }

      .multi-filter-condition .condition-code {
        flex: 1;
      }

      .multi-filter-condition .condition-delete {
        border-top-left-radius: 0;
        border-bottom-left-radius: 0;
        height: 38px;
        width: 45px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #6c757d;
        border: none;
        background: transparent;
        padding: 0;
      }

      .multi-filter-condition .condition-delete:hover {
        color: #dc3545;
        background: rgba(220, 53, 69, 0.1);
      }

      .multi-filter-condition .input-group {
        border: none !important;
      }

      .input-group.multi-filter-condition {
        height: 38px !important;
      }

      .multi-filter-actions {
        margin-top: 15px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      .multi-filter-actions .btn-outline-secondary {
        border-color: #dee2e6;
        color: #6c757d;
      }

      .multi-filter-actions .btn-outline-secondary:hover {
        background-color: #f8f9fa;
        border-color: #adb5bd;
        color: #495057;
      }

      .multi-filter-container .selectize-control.single .selectize-input {
        border-color: #dee2e6;
        color: #6c757d;
        font-size: 0.875rem;
        padding: 0.25rem 0.5rem;
        min-height: calc(1.5em + 0.5rem + 2px);
      }
    "
    ),
    div(
      class = "multi-filter-container",
      uiOutput(ns("conditions_ui")),
      div(
        class = "multi-filter-actions mt-2 mb-1",
        actionButton(
          ns("add_condition"),
          label = "Add Condition",
          icon = icon("plus"),
          class = "btn btn-outline-secondary btn-sm"
        ),
        if (!is.null(extra_button)) extra_button else NULL
      )
    )
  )
}

#' Create UI for a single filter condition row
#'
#' @param id Row identifier
#' @param value Condition value
#' @param show_remove Whether to show remove button
#' @return A div containing the row UI
#' @noRd
#' @noRd
multi_filter_condition_ui <- function(id, value = "TRUE", show_remove = TRUE) {
  div(
    class = paste(
      "input-group mb-2",
      "multi-filter-condition border border-dark-subtle rounded"
    ),
    div(
      class = "condition-code",
      setup_ace_editor(id, value = value)
    ),
    if (show_remove) {
      actionButton(
        paste0(id, "_remove"),
        label = NULL,
        icon = icon("xmark"),
        class = "btn condition-delete"
      )
    }
  )
}

#' Run example app demonstrating multi filter functionality
#'
#' @noRd
run_multi_filter_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      h3("Multi Filter Example"),
      mod_multi_filter_ui("mf"),
      hr(),
      h4("Current Filter:"),
      verbatimTextOutput("filter"),
      h4("Generated Code:"),
      verbatimTextOutput("code")
    ),
    server = function(input, output, session) {
      r_result <- mod_multi_filter_server(
        "mf",
        get_value = function() "mpg > 20",
        get_cols = function() c("mpg", "cyl", "hp", "wt", "am", "gear")
      )

      output$filter <- renderPrint({
        filter_str <- r_result()
        cat(sprintf('"%s"', filter_str))
      })

      output$code <- renderPrint({
        filter_str <- r_result()
        cat(sprintf("dplyr::filter(data, %s)", filter_str))
      })
    }
  )
}
