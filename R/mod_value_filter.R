#' Value filter condition module for selecting values from columns
#'
#' A Shiny module that manages value-based filter conditions. Users can select
#' columns and choose specific values to include or exclude without writing
#' R expressions. Supports multiple conditions with AND/OR logic.
#'
#' @param id The module ID
#' @param get_value Function that returns initial conditions as a list
#' @param get_data Function that returns the data frame for extracting unique values
#'
#' @return A reactive expression containing the current filter conditions
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div selectInput radioButtons updateSelectInput
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @keywords internal
mod_value_filter_server <- function(id, get_value, get_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    initial_value <- get_value()
    if (!is.list(initial_value)) {
      initial_conditions <- list()
    } else {
      initial_conditions <- initial_value
    }
    
    # Add default condition if none provided
    if (length(initial_conditions) == 0) {
      initial_conditions <- list(
        list(column = NULL, values = character(0), mode = "include")
      )
    }

    # Store conditions as reactive value
    r_conditions <- reactiveVal(initial_conditions)
    r_data <- get_data

    # Track which condition indices exist
    r_condition_indices <- reactiveVal(seq_along(initial_conditions))
    r_next_index <- reactiveVal(length(initial_conditions) + 1)

    # Track AND/OR logic between conditions  
    r_logic_operators <- reactiveVal(rep(
      "&",
      max(0, length(initial_conditions) - 1)
    ))

    # Get available columns
    available_columns <- reactive({
      req(r_data())
      colnames(r_data())
    })

    # Get unique values for a column
    get_unique_values <- function(column) {
      req(r_data(), column)
      if (!column %in% colnames(r_data())) {
        return(character(0))
      }
      
      values <- unique(r_data()[[column]])
      # Remove NA values for display
      values <- values[!is.na(values)]
      
      # Convert factors to character for consistent handling
      if (is.factor(values)) {
        values <- as.character(values)
      }
      
      # Sort values for better UX
      if (is.numeric(values)) {
        sort(values)
      } else {
        sort(as.character(values))
      }
    }

    # Collect current values from all inputs
    get_current_conditions <- function() {
      indices <- r_condition_indices()
      if (length(indices) == 0) {
        return(list())
      }

      result <- list()
      for (i in indices) {
        column_id <- paste0("condition_", i, "_column")
        values_id <- paste0("condition_", i, "_values")
        mode_id <- paste0("condition_", i, "_mode")

        column <- input[[column_id]]
        values <- input[[values_id]]
        mode <- input[[mode_id]] %||% "include"

        if (!is.null(column) && !is.null(values) && length(values) > 0) {
          result <- append(result, list(list(
            column = column,
            values = values,
            mode = mode
          )))
        }
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
        }
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
        r_logic_operators(c(current_logic, "&"))
      }

      # Update conditions
      current <- get_current_conditions()
      current <- append(current, list(list(
        column = NULL,
        values = character(0),
        mode = "include"
      )))
      r_conditions(current)
    })

    # Remove condition handlers
    observe({
      indices <- r_condition_indices()

      lapply(indices, function(i) {
        observeEvent(input[[paste0("condition_", i, "_remove")]], {
          current_indices <- r_condition_indices()

          if (length(current_indices) > 1) {
            # Remove this index
            new_indices <- setdiff(current_indices, i)
            r_condition_indices(new_indices)

            # Update logic operators
            current_logic <- r_logic_operators()
            if (length(current_logic) >= length(new_indices)) {
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
      cols <- available_columns()

      if (length(indices) == 0) {
        return(NULL)
      }

      # Create UI for each condition
      ui_elements <- list()

      for (j in seq_along(indices)) {
        i <- indices[j]
        condition <- if (j <= length(conditions)) conditions[[j]] else list(
          column = NULL, values = character(0), mode = "include"
        )

        # Add the condition row
        ui_elements <- append(
          ui_elements,
          list(
            value_filter_condition_ui(
              ns(paste0("condition_", i)),
              column = condition$column,
              values = condition$values,
              mode = condition$mode %||% "include",
              available_columns = cols,
              get_unique_values = get_unique_values,
              show_remove = (length(indices) > 1)
            )
          )
        )

        # Add logic operator dropdown between conditions
        if (j < length(indices)) {
          logic_value <- if (j <= length(logic_ops)) logic_ops[j] else "&"
          ui_elements <- append(
            ui_elements,
            list(
              div(
                class = "d-flex justify-content-start my-2",
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

    # Update value choices when column selection changes
    observe({
      indices <- r_condition_indices()
      
      for (i in indices) {
        local({
          index <- i
          column_id <- paste0("condition_", index, "_column")
          values_id <- paste0("condition_", index, "_values")
          
          observeEvent(input[[column_id]], {
            column <- input[[column_id]]
            if (!is.null(column) && column != "" && column %in% available_columns()) {
              unique_vals <- get_unique_values(column)
              updateSelectInput(
                session,
                values_id,
                choices = unique_vals,
                selected = character(0)  # Reset selection when column changes
              )
            } else {
              updateSelectInput(
                session,
                values_id,
                choices = list(),
                selected = character(0)
              )
            }
          })
        })
      }
    })
    
    # Initialize value choices for existing conditions - simple blockr.core pattern
    observe({
      indices <- r_condition_indices()
      conditions <- r_conditions()
      
      for (j in seq_along(indices)) {
        local({
          index <- indices[j]
          condition <- if (j <= length(conditions)) conditions[[j]] else NULL
          values_id <- paste0("condition_", index, "_values")
          
          if (!is.null(condition) && !is.null(condition$column) && condition$column != "") {
            unique_vals <- get_unique_values(condition$column)
            updateSelectInput(
              session,
              values_id,
              choices = unique_vals,
              selected = condition$values
            )
          }
        })
      }
    })

    # Return the reactive conditions
    reactive({
      # Check if any inputs exist yet
      indices <- r_condition_indices()
      has_inputs <- any(sapply(indices, function(i) {
        paste0("column_", i) %in% names(input)
      }))

      if (has_inputs) {
        get_current_conditions()
      } else {
        r_conditions()
      }
    })
  })
}

#' Create value filter UI module
#'
#' @param id The module ID
#' @return A div containing the UI elements
#' @keywords internal
mod_value_filter_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      .value-filter-condition {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
        background-color: #f8f9fa;
      }
      
      .value-filter-condition .condition-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 10px;
      }
      
      .value-filter-condition .condition-controls {
        display: flex;
        gap: 15px;
        align-items: end;
      }
      
      .value-filter-condition .column-selector {
        flex: 1;
        min-width: 150px;
      }
      
      .value-filter-condition .values-selector {
        flex: 2;
        min-width: 200px;
      }
      
      .value-filter-condition .mode-selector {
        flex: 1;
        min-width: 120px;
      }
      
      .condition-remove {
        background-color: #dc3545;
        border-color: #dc3545;
        color: white;
        border-radius: 50%;
        width: 30px;
        height: 30px;
        padding: 0;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .condition-remove:hover {
        background-color: #c82333;
        border-color: #bd2130;
      }
      
      .condition-count {
        font-size: 0.875rem;
        color: #6c757d;
        font-style: italic;
      }
      "
    ),
    div(
      class = "value-filter-container",
      uiOutput(ns("conditions_ui")),
      div(
        class = "d-flex justify-content-start mt-3",
        actionButton(
          ns("add_condition"),
          label = "Add Condition",
          icon = icon("plus"),
          class = "btn btn-success btn-sm"
        )
      )
    )
  )
}

#' Create UI for a single value filter condition row
#'
#' @param id Row identifier
#' @param column Selected column name
#' @param values Selected values
#' @param mode Include or exclude mode
#' @param available_columns Available column choices
#' @param get_unique_values Function to get unique values for a column
#' @param show_remove Whether to show remove button
#' @return A div containing the row UI
value_filter_condition_ui <- function(id, column = NULL, values = character(0), 
                                     mode = "include", available_columns = character(0),
                                     get_unique_values = function(col) character(0),
                                     show_remove = TRUE) {
  
  # Initialize with empty list() to match blockr.core pattern
  # Values will be populated by server-side updates
  unique_values <- list()
  
  # Count selected values for display
  values_count <- if (length(values) > 0) {
    glue::glue("({length(values)} selected)")
  } else {
    ""
  }
  
  div(
    class = "value-filter-condition",
    div(
      class = "condition-header",
      tags$strong("Filter Condition"),
      if (show_remove) {
        actionButton(
          paste0(id, "_remove"),
          label = NULL,
          icon = icon("times"),
          class = "condition-remove",
          title = "Remove this condition"
        )
      }
    ),
    div(
      class = "condition-controls",
      div(
        class = "column-selector",
        selectInput(
          paste0(id, "_column"),
          label = "Column:",
          choices = c("Select column..." = "", available_columns),
          selected = column,
          width = "100%"
        )
      ),
      div(
        class = "values-selector",
        selectInput(
          paste0(id, "_values"),
          label = glue::glue("Values {values_count}:"),
          choices = unique_values,
          selected = values,
          multiple = TRUE,
          width = "100%"
        )
      ),
      div(
        class = "mode-selector",
        radioButtons(
          paste0(id, "_mode"),
          label = "Mode:",
          choices = list("Include" = "include", "Exclude" = "exclude"),
          selected = mode,
          inline = FALSE
        )
      )
    )
  )
}

#' Run example app demonstrating value filter functionality
#'
#' @examples
#' \dontrun{
#' run_value_filter_example()
#' }
#' @keywords internal
run_value_filter_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      h3("Value Filter Example"),
      mod_value_filter_ui("vf"),
      hr(),
      h4("Current Conditions:"),
      verbatimTextOutput("conditions"),
      h4("Generated Code:"),
      verbatimTextOutput("code")
    ),
    server = function(input, output, session) {
      r_result <- mod_value_filter_server(
        "vf",
        get_value = function() list(),
        get_data = function() iris
      )

      output$conditions <- renderPrint({
        conditions <- r_result()
        str(conditions)
      })

      output$code <- renderPrint({
        conditions <- r_result()
        expr <- parse_value_filter(conditions)
        cat(deparse(expr))
      })
    }
  )
}