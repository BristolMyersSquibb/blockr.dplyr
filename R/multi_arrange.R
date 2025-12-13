#' Multi arrange module for multiple column sorting
#'
#' A Shiny module that manages multiple column sorting with individual ASC/DESC controls.
#' Supports adding, removing, and reordering sort columns dynamically.
#'
#' @param id The module ID
#' @param get_value Function that returns initial values as a list of (column, direction) pairs
#' @param get_cols Function that returns column names for dropdown selection
#'
#' @return A reactive expression containing the current arrange configuration
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div selectInput
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @noRd
#' @noRd
mod_multi_arrange_server <- function(id, get_value, get_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    initial_value <- get_value()
    if (is.character(initial_value) && length(initial_value) > 0) {
      # Convert character vector to list of arrange specs
      initial_arranges <- lapply(initial_value, function(col) {
        list(column = col, direction = "asc")
      })
    } else if (is.list(initial_value) && length(initial_value) > 0) {
      initial_arranges <- initial_value
    } else {
      initial_arranges <- list(list(column = "", direction = "asc"))
    }

    # Store arranges as reactive value
    r_arranges <- reactiveVal(initial_arranges)
    r_cols <- reactive(get_cols())

    # Track which arrange indices exist
    r_arrange_indices <- reactiveVal(seq_along(initial_arranges))
    r_next_index <- reactiveVal(length(initial_arranges) + 1)

    # Collect current values from all inputs
    get_current_arranges <- function() {
      indices <- r_arrange_indices()
      if (length(indices) == 0) {
        return(list())
      }

      result <- list()
      for (i in indices) {
        column_id <- paste0("arrange_", i, "_column")
        direction_id <- paste0("arrange_", i, "_direction")

        column <- input[[column_id]]
        direction_checkbox <- input[[direction_id]]

        # Convert checkbox to direction: TRUE = "desc", FALSE/NULL = "asc"
        direction <- if (isTRUE(direction_checkbox)) "desc" else "asc"

        if (!is.null(column) && column != "") {
          result <- append(
            result,
            list(list(column = column, direction = direction))
          )
        }
      }

      if (length(result) == 0) {
        available_cols <- r_cols()
        if (length(available_cols) > 0) {
          result <- list(list(column = available_cols[1], direction = "asc"))
        } else {
          result <- list(list(column = "", direction = "asc"))
        }
      }

      result
    }

    # Add new arrange column
    observeEvent(input$add_arrange, {
      current_indices <- r_arrange_indices()
      new_index <- r_next_index()

      # Add new index
      r_arrange_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Get current arranges and add new one
      current <- get_current_arranges()

      # Choose first available column not already used
      available_cols <- r_cols()
      used_cols <- sapply(current, function(x) x$column)
      available_cols <- setdiff(available_cols, used_cols)
      new_column <- if (length(available_cols) > 0) {
        available_cols[1]
      } else {
        r_cols()[1]
      }

      current <- append(
        current,
        list(list(column = new_column, direction = "asc"))
      )
      r_arranges(current)
    })

    # Remove arrange handlers - create them dynamically
    observe({
      indices <- r_arrange_indices()

      lapply(indices, function(i) {
        observeEvent(input[[paste0("arrange_", i, "_remove")]], {
          current_indices <- r_arrange_indices()

          if (length(current_indices) > 1) {
            # Remove this index
            new_indices <- setdiff(current_indices, i)
            r_arrange_indices(new_indices)

            # Update arranges
            current <- get_current_arranges()
            r_arranges(current)
          }
        })
      })
    })

    # Render UI dynamically
    output$arranges_ui <- renderUI({
      indices <- r_arrange_indices()
      arranges <- r_arranges()
      available_cols <- r_cols()

      if (length(indices) == 0) {
        return(NULL)
      }

      # Create UI for each arrange specification
      tagList(
        lapply(seq_along(indices), function(j) {
          i <- indices[j]
          arrange_spec <- if (j <= length(arranges)) {
            arranges[[j]]
          } else {
            list(column = "", direction = "asc")
          }

          multi_arrange_row_ui(
            ns(paste0("arrange_", i)),
            column = arrange_spec$column,
            direction = arrange_spec$direction,
            available_cols = available_cols,
            position = j,
            show_remove = (length(indices) > 1)
          )
        })
      )
    })

    # Return the reactive arranges with initialization handling
    reactive({
      # Check if any inputs exist yet - if not, use stored arranges
      indices <- r_arrange_indices()
      has_inputs <- any(sapply(indices, function(i) {
        paste0("arrange_", i, "_column") %in%
          names(input) &&
          paste0("arrange_", i, "_direction") %in% names(input)
      }))

      if (has_inputs) {
        # Use current input values
        get_current_arranges()
      } else {
        # Use stored arranges (for initialization)
        r_arranges()
      }
    })
  })
}

#' Create multi arrange UI module
#'
#' @param id The module ID
#' @return A div containing the UI elements
#' @noRd
#' @noRd
mod_multi_arrange_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      /* Grid layout for arrange rows - matches filter block pattern */
      .arrange-grid {
        display: grid;
        grid-template-columns: 1fr auto;
        gap: 0 15px;
        align-items: end;
      }

      .arrange-grid .arrange-column { grid-column: 1; grid-row: 1; }
      .arrange-grid .arrange-column-full { grid-column: 1 / -1; }
      .arrange-grid .arrange-delete-cell {
        grid-column: 2;
        grid-row: 1;
        height: 38px;
        display: flex;
        align-items: center;
      }
      .arrange-grid .arrange-direction { grid-column: 1 / -1; grid-row: 2; }

      /* Remove Shiny's default input margins */
      .arrange-condition .shiny-input-container {
        margin-bottom: 0 !important;
      }

      /* Consistent height for form controls */
      .arrange-condition .selectize-input {
        min-height: 38px;
      }

      .multi-arrange-container .btn-outline-secondary {
        border-color: #dee2e6;
        color: #6c757d;
      }

      .multi-arrange-container .btn-outline-secondary:hover {
        border-color: #adb5bd;
        background-color: #f8f9fa;
        color: #495057;
      }
    "
    ),
    div(
      class = "multi-arrange-container",
      uiOutput(ns("arranges_ui")),
      div(
        class = "d-flex justify-content-start mt-2 mb-1",
        actionButton(
          ns("add_arrange"),
          label = "Add Sort Column",
          icon = icon("plus"),
          class = "btn btn-outline-secondary btn-sm"
        )
      )
    )
  )
}

#' Create UI for a single arrange row
#'
#' @param id Row identifier
#' @param column Column name to sort by
#' @param direction Sort direction ("asc" or "desc")
#' @param available_cols Available column names for dropdown
#' @param position Position in sort order (1, 2, 3, ...)
#' @param show_remove Whether to show remove button
#' @return A div containing the row UI
#' @noRd
#' @noRd
multi_arrange_row_ui <- function(
  id,
  column = "",
  direction = "asc",
  available_cols = character(),
  position = 1,
  show_remove = TRUE
) {
  div(
    class = "arrange-condition mb-1",
    div(
      class = "arrange-grid",
      div(
        class = if (show_remove) "arrange-column" else "arrange-column arrange-column-full",
        selectInput(
          paste0(id, "_column"),
          label = "Column",
          choices = available_cols,
          selected = column,
          width = "100%"
        )
      ),
      if (show_remove) {
        div(
          class = "arrange-delete-cell d-flex align-items-center",
          actionButton(
            paste0(id, "_remove"),
            label = NULL,
            icon = icon("xmark"),
            class = "btn btn-link text-muted p-0",
            title = "Remove"
          )
        )
      },
      div(
        class = "arrange-direction d-flex align-items-center mt-1",
        checkboxInput(
          paste0(id, "_direction"),
          label = "Descending",
          value = (direction == "desc")
        )
      )
    )
  )
}

#' Run example app demonstrating multi arrange functionality
#'
#' @noRd
run_multi_arrange_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      h3("Multi Arrange Example"),
      mod_multi_arrange_ui("ma"),
      hr(),
      h4("Current Sort Order:"),
      verbatimTextOutput("arranges"),
      h4("Generated Code:"),
      verbatimTextOutput("code")
    ),
    server = function(input, output, session) {
      r_result <- mod_multi_arrange_server(
        "ma",
        get_value = function() {
          list(
            list(column = "mpg", direction = "desc"),
            list(column = "cyl", direction = "asc")
          )
        },
        get_cols = function() c("mpg", "cyl", "hp", "wt", "am", "gear")
      )

      output$arranges <- renderPrint({
        arranges <- r_result()
        if (length(arranges) > 0) {
          for (i in seq_along(arranges)) {
            arr <- arranges[[i]]
            direction_label <- if (arr$direction == "desc") "DESC" else "ASC"
            cat(sprintf("%d. %s %s\n", i, arr$column, direction_label))
          }
        }
      })

      output$code <- renderPrint({
        arranges <- r_result()
        if (length(arranges) > 0) {
          arrange_exprs <- sapply(arranges, function(arr) {
            if (arr$direction == "desc") {
              sprintf("desc(%s)", arr$column)
            } else {
              arr$column
            }
          })
          arrange_str <- paste(arrange_exprs, collapse = ", ")
          cat(sprintf("dplyr::arrange(data, %s)", arrange_str))
        }
      })
    }
  )
}
