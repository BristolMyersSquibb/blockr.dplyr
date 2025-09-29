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
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div selectInput checkboxInput updateSelectInput shinyApp sliderInput conditionalPanel updateSliderInput outputOptions
#' @importFrom utils str
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags tagList
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

    # Track which conditions use sliders (for numeric columns)
    # Initialize based on condition types
    initial_use_slider <- list()
    for (i in seq_along(initial_conditions)) {
      if (
        !is.null(initial_conditions[[i]]$type) &&
          initial_conditions[[i]]$type == "range"
      ) {
        initial_use_slider[[as.character(i)]] <- TRUE
      }
    }
    r_use_slider <- reactiveVal(initial_use_slider)

    # Track which condition indices exist
    r_condition_indices <- reactiveVal(seq_along(initial_conditions))
    r_next_index <- reactiveVal(length(initial_conditions) + 1)

    # Get available columns
    available_columns <- reactive({
      req(r_data())
      colnames(r_data())
    })

    # Check if a column is numeric
    is_numeric_column <- function(column) {
      if (is.null(column) || column == "" || !column %in% colnames(r_data())) {
        return(FALSE)
      }
      is.numeric(r_data()[[column]])
    }

    # Get range for numeric column
    get_numeric_range <- function(column) {
      if (!is_numeric_column(column)) {
        return(c(0, 1))
      }
      range(r_data()[[column]], na.rm = TRUE)
    }

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
      use_slider <- r_use_slider()

      for (idx in seq_along(indices)) {
        i <- indices[idx]
        column_id <- paste0("condition_", i, "_column")

        column <- input[[column_id]]

        # Get operator if not the first condition
        operator <- if (idx > 1) {
          operator_id <- paste0("operator_", i)
          input[[operator_id]] %||% "&"
        } else {
          NULL
        }

        # Check if using slider for this condition
        if (
          !is.null(use_slider[[as.character(i)]]) &&
            use_slider[[as.character(i)]]
        ) {
          # Get slider values
          slider_id <- paste0("condition_", i, "_slider")
          slider_val <- input[[slider_id]]
          mode_id <- paste0("condition_", i, "_mode")
          mode <- if (isTRUE(input[[mode_id]])) "exclude" else "include"

          if (!is.null(column) && column != "" && !is.null(slider_val)) {
            condition_data <- list(
              column = column,
              values = slider_val, # Store range as values
              mode = mode,
              type = "range" # Mark this as a range condition
            )
            if (!is.null(operator)) {
              condition_data$operator <- operator
            }
            result <- append(result, list(condition_data))
          }
        } else {
          # Original multi-select logic
          values_id <- paste0("condition_", i, "_values")
          mode_id <- paste0("condition_", i, "_mode")

          values <- input[[values_id]]
          mode <- if (isTRUE(input[[mode_id]])) "exclude" else "include"

          # Include condition if column is selected, even if values is empty/NULL
          if (!is.null(column) && column != "") {
            # Ensure values is a character vector, even if empty
            values <- if (is.null(values) || length(values) == 0) {
              character(0)
            } else {
              values
            }

            condition_data <- list(
              column = column,
              values = values,
              mode = mode,
              type = "values" # Mark this as a values condition
            )
            if (!is.null(operator)) {
              condition_data$operator <- operator
            }
            result <- append(result, list(condition_data))
          }
        }
      }

      result
    }

    # Add new condition
    observeEvent(input$add_condition, {
      current_indices <- r_condition_indices()
      new_index <- r_next_index()

      # Add new index
      r_condition_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Update conditions
      current <- get_current_conditions()
      new_condition <- list(
        column = NULL,
        values = character(0),
        mode = "include"
      )
      # Add operator if this is not the first condition
      if (length(current) > 0) {
        new_condition$operator <- "&"
      }
      current <- append(current, list(new_condition))
      r_conditions(current)
    })

    # Track use_slider checkbox changes
    observe({
      indices <- r_condition_indices()
      lapply(indices, function(i) {
        observeEvent(input[[paste0("condition_", i, "_use_slider")]], {
          use_slider <- r_use_slider()
          use_slider[[as.character(i)]] <- input[[paste0(
            "condition_",
            i,
            "_use_slider"
          )]]
          r_use_slider(use_slider)
        })
      })
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

            # Update conditions
            current <- get_current_conditions()
            r_conditions(current)

            # Clean up use_slider tracking
            use_slider <- r_use_slider()
            use_slider[[as.character(i)]] <- NULL
            r_use_slider(use_slider)
          }
        })
      })
    })

    # Render UI dynamically
    output$conditions_ui <- renderUI({
      indices <- r_condition_indices()
      conditions <- r_conditions()
      cols <- available_columns()

      if (length(indices) == 0) {
        return(NULL)
      }

      # Create UI for each condition
      ui_elements <- list()

      for (j in seq_along(indices)) {
        i <- indices[j]
        condition <- if (j <= length(conditions)) {
          conditions[[j]]
        } else {
          list(
            column = NULL,
            values = character(0),
            mode = "include"
          )
        }

        # Add operator dropdown before condition (except for first)
        if (j > 1) {
          operator_value <- condition$operator %||% "&"
          ui_elements <- append(
            ui_elements,
            list(
              div(
                class = "d-flex justify-content-start my-2",
                selectInput(
                  ns(paste0("operator_", i)),
                  label = NULL,
                  choices = list("AND" = "&", "OR" = "|"),
                  selected = operator_value,
                  width = "80px"
                )
              )
            )
          )
        }

        # Add the condition row with standard UI
        ui_elements <- append(
          ui_elements,
          list(
            value_filter_condition_ui(
              ns(paste0("condition_", i)),
              column = condition$column,
              values = condition$values,
              mode = condition$mode %||% "include",
              type = condition$type %||% "values", # Pass the type
              available_columns = cols,
              get_unique_values = get_unique_values,
              show_remove = (length(indices) > 1),
              ns = ns # Pass namespace function
            )
          )
        )
      }

      tagList(ui_elements)
    })

    # Create reactive outputs for numeric column detection
    observe({
      indices <- r_condition_indices()
      for (i in indices) {
        local({
          index <- i
          column_id <- paste0("condition_", index, "_column")
          is_numeric_id <- paste0("condition_", index, "_is_numeric")

          output[[is_numeric_id]] <- reactive({
            column <- input[[column_id]]
            if (!is.null(column) && column != "") {
              is_numeric_column(column)
            } else {
              FALSE
            }
          })
          outputOptions(output, is_numeric_id, suspendWhenHidden = FALSE)
        })
      }
    })

    # Handle dynamic updates for each condition
    observe({
      indices <- r_condition_indices()

      for (i in indices) {
        local({
          index <- i
          column_id <- paste0("condition_", index, "_column")
          values_id <- paste0("condition_", index, "_values")
          use_slider_id <- paste0("condition_", index, "_use_slider")
          slider_id <- paste0("condition_", index, "_slider")

          # Update select input choices when column changes
          observeEvent(input[[column_id]], {
            column <- input[[column_id]]
            if (!is.null(column) && column != "") {
              unique_vals <- get_unique_values(column)
              # Get current condition to preserve selected values if they exist
              current_conds <- isolate(r_conditions())
              saved_values <- NULL
              for (j in seq_along(current_conds)) {
                if (j <= length(isolate(r_condition_indices()))) {
                  if (isolate(r_condition_indices())[j] == index) {
                    if (
                      current_conds[[j]]$column == column &&
                        !is.null(current_conds[[j]]$type) &&
                        current_conds[[j]]$type == "values"
                    ) {
                      saved_values <- current_conds[[j]]$values
                    }
                    break
                  }
                }
              }
              updateSelectInput(
                session,
                values_id,
                choices = unique_vals,
                selected = saved_values
              )

              # Update slider range if numeric
              if (is_numeric_column(column)) {
                range_vals <- get_numeric_range(column)
                # Check if we have saved range values
                saved_range <- NULL
                for (j in seq_along(current_conds)) {
                  if (j <= length(isolate(r_condition_indices()))) {
                    if (isolate(r_condition_indices())[j] == index) {
                      if (
                        current_conds[[j]]$column == column &&
                          !is.null(current_conds[[j]]$type) &&
                          current_conds[[j]]$type == "range"
                      ) {
                        saved_range <- current_conds[[j]]$values
                      }
                      break
                    }
                  }
                }
                updateSliderInput(
                  session,
                  slider_id,
                  min = range_vals[1],
                  max = range_vals[2],
                  value = saved_range %||% range_vals
                )
              }
            } else {
              # Clear selections when no column selected
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

    # Return the reactive conditions
    list(
      conditions = reactive({
        # Force reactivity on all condition inputs
        indices <- r_condition_indices()

        # Touch all relevant input values to create dependencies
        for (i in indices) {
          input[[paste0("condition_", i, "_column")]]
          input[[paste0("condition_", i, "_values")]]
          input[[paste0("condition_", i, "_mode")]]
          # Also track operator changes
          if (i != indices[1]) {
            input[[paste0("operator_", i)]]
          }
        }

        # Check if any inputs exist yet
        has_inputs <- any(sapply(indices, function(i) {
          paste0("condition_", i, "_column") %in% names(input)
        }))

        if (has_inputs) {
          get_current_conditions()
        } else {
          r_conditions()
        }
      })
    )
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
        margin-bottom: 10px;
        background: none;
      }
      
      .value-filter-condition .condition-controls {
        display: flex;
        gap: 15px;
        align-items: end;
      }
      
      .value-filter-condition .column-selector {
        flex: 1;
        min-width: 120px;
      }
      
      .value-filter-condition .values-selector {
        flex: 2;
        min-width: 180px;
      }
      
      .value-filter-condition .mode-selector {
        flex: 0 0 auto;
        min-width: 80px;
      }
      
      .value-filter-condition .delete-selector {
        flex: 0 0 auto;
        display: flex;
        align-items: end;
        padding-bottom: 5px;
      }
      
      .value-filter-condition label {
        font-size: 0.75rem;
        color: #6c757d;
        margin-bottom: 2px;
      }
      
      .condition-remove {
        background: none;
        border: none;
        color: #6c757d;
        padding: 6px;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 32px;
      }
      
      .condition-remove:hover {
        color: #dc3545;
        background: none;
        border: none;
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
#' @param type Type of filter ("values" or "range")
#' @param available_columns Available column choices
#' @param get_unique_values Function to get unique values for a column
#' @param show_remove Whether to show remove button
#' @param ns Namespace function for conditional panels
#' @return A div containing the row UI
#' @importFrom shiny sliderInput conditionalPanel
value_filter_condition_ui <- function(
  id,
  column = NULL,
  values = character(0),
  mode = "include",
  type = "values", # Add type parameter
  available_columns = character(0),
  get_unique_values = function(col) character(0),
  show_remove = TRUE,
  ns = function(x) x # Allow namespace to be passed in
) {
  # Initialize choices - populate with actual values if we have a column
  unique_values <- if (!is.null(column) && column != "") {
    get_unique_values(column)
  } else {
    list()
  }

  # Determine initial values for UI elements based on type
  is_range <- (type == "range")
  initial_use_slider <- is_range

  # Check if column is numeric (for showing checkbox)
  column_is_numeric <- if (!is.null(column) && column != "") {
    vals <- get_unique_values(column)
    is.numeric(vals)
  } else {
    FALSE
  }

  # For slider, use saved values or full range
  slider_values <- if (is_range && length(values) == 2) {
    values
  } else if (!is.null(column) && column != "") {
    # Get range for the column if numeric
    tryCatch(
      {
        vals <- get_unique_values(column)
        if (is.numeric(vals) && length(vals) > 0) {
          range(vals, na.rm = TRUE)
        } else {
          c(0, 1)
        }
      },
      error = function(e) c(0, 1)
    )
  } else {
    c(0, 1)
  }

  # For select, always populate with values if they exist
  # Convert numeric values to character for display if needed
  select_values <- if (!is_range && length(values) > 0) {
    values
  } else if (is_range && length(values) == 2) {
    # For range, show the range values in select as fallback
    as.character(values)
  } else {
    character(0)
  }

  # Extract base ID for consistent naming
  base_id <- gsub(".*-", "", id)

  div(
    class = "value-filter-condition",
    # Checkbox for numeric columns - show if column is numeric
    if (column_is_numeric) {
      div(
        class = "mb-2",
        checkboxInput(
          paste0(id, "_use_slider"),
          label = "Use range slider",
          value = initial_use_slider
        )
      )
    } else {
      # Empty div to maintain structure
      div(style = "display: none;")
    },
    div(
      class = "condition-controls",
      div(
        class = "column-selector",
        selectInput(
          paste0(id, "_column"),
          label = "Column",
          choices = c("Select column..." = "", available_columns),
          selected = column,
          width = "100%"
        )
      ),
      # Values selector - show appropriate input based on type
      div(
        class = "values-selector",
        if (column_is_numeric) {
          # For numeric columns, show both but use conditionalPanel
          tagList(
            conditionalPanel(
              condition = sprintf("!input['%s']", paste0(id, "_use_slider")),
              selectInput(
                paste0(id, "_values"),
                label = "Values",
                choices = unique_values,
                selected = if (!is_range) values else NULL,
                multiple = TRUE,
                width = "100%"
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s']", paste0(id, "_use_slider")),
              sliderInput(
                paste0(id, "_slider"),
                label = "Range",
                min = slider_values[1],
                max = slider_values[2],
                value = slider_values,
                width = "100%"
              )
            )
          )
        } else {
          # For non-numeric columns, just show the multi-select
          selectInput(
            paste0(id, "_values"),
            label = "Values",
            choices = unique_values,
            selected = values,
            multiple = TRUE,
            width = "100%"
          )
        }
      ),
      div(
        class = "mode-selector",
        checkboxInput(
          paste0(id, "_mode"),
          label = "Exclude",
          value = (mode == "exclude")
        )
      ),
      if (show_remove) {
        div(
          class = "delete-selector",
          actionButton(
            paste0(id, "_remove"),
            label = NULL,
            icon = icon("trash-can"),
            class = "condition-remove",
            title = "Remove this condition"
          )
        )
      }
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
        get_data = function() datasets::iris
      )

      output$conditions <- renderPrint({
        conditions <- r_result()$conditions()
        utils::str(conditions)
      })

      output$code <- renderPrint({
        conditions <- r_result()$conditions()
        expr <- parse_value_filter(conditions)
        cat(deparse(expr))
      })
    }
  )
}
