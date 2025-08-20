#' Multi select module for enhanced column selection
#'
#' A Shiny module that provides an enhanced column selection interface with
#' visual column cards, search functionality, and column information display.
#'
#' @param id The module ID
#' @param get_value Function that returns initial selected columns as character vector
#' @param get_cols Function that returns available column names
#' @param get_data_preview Function that returns sample data for column preview
#'
#' @return A reactive expression containing the currently selected columns
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div textInput checkboxInput
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @export
mod_multi_select_server <- function(id, get_value, get_cols, get_data_preview) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    initial_columns <- get_value()
    if (!is.character(initial_columns)) {
      initial_columns <- character(0)
    }

    # Store selected columns as reactive value - validate against available columns
    r_selected <- reactiveVal()
    r_cols <- reactive(get_cols())
    r_data_preview <- reactive(get_data_preview())

    # Track initialization to avoid overwriting user selections
    r_initialized <- reactiveVal(FALSE)

    # Initialize selected columns once we have available columns
    observe({
      if (!r_initialized()) {
        available_cols <- r_cols()
        if (length(available_cols) > 0) {
          # Ensure initial columns exist in the data
          valid_initial <- intersect(initial_columns, available_cols)
          r_selected(valid_initial)
          r_initialized(TRUE)
        }
      }
    })

    # Search filter
    r_search <- reactiveVal("")

    # Update search when input changes
    observeEvent(input$search, {
      r_search(input$search)
    })

    # Get filtered columns based on search
    r_filtered_cols <- reactive({
      cols <- r_cols()
      search_term <- r_search()

      if (search_term == "" || is.null(search_term)) {
        return(cols)
      }

      # Case-insensitive search
      grep(search_term, cols, value = TRUE, ignore.case = TRUE)
    })

    # Handle individual column selection
    observe({
      cols <- r_cols()

      # Watch for changes in any column checkbox
      lapply(cols, function(col) {
        input_id <- paste0("col_", make.names(col))

        observeEvent(input[[input_id]], {
          current_selected <- r_selected()

          if (isTRUE(input[[input_id]])) {
            # Add column if not already selected
            if (!col %in% current_selected) {
              r_selected(c(current_selected, col))
            }
          } else {
            # Remove column if currently selected
            r_selected(setdiff(current_selected, col))
          }
        })
      })
    })

    # Select All button
    observeEvent(input$select_all, {
      r_selected(r_filtered_cols())
    })

    # Select None button
    observeEvent(input$select_none, {
      r_selected(character(0))
    })

    # Invert Selection button
    observeEvent(input$invert_selection, {
      current_selected <- r_selected()
      available_cols <- r_filtered_cols()
      new_selection <- setdiff(available_cols, current_selected)
      r_selected(new_selection)
    })

    # Get column information for display
    get_column_info <- function(col) {
      data_preview <- r_data_preview()

      if (is.null(data_preview) || !col %in% names(data_preview)) {
        return(list(
          type = "unknown",
          sample = "",
          na_count = 0,
          unique_count = 0
        ))
      }

      col_data <- data_preview[[col]]

      # Get column type
      col_type <- class(col_data)[1]

      # Get sample values (first 3 non-NA values)
      sample_vals <- head(col_data[!is.na(col_data)], 3)
      sample_text <- if (length(sample_vals) > 0) {
        paste(sample_vals, collapse = ", ")
      } else {
        "All NA"
      }

      # Get NA count
      na_count <- sum(is.na(col_data))

      # Get unique count
      unique_count <- length(unique(col_data))

      list(
        type = col_type,
        sample = sample_text,
        na_count = na_count,
        unique_count = unique_count,
        total_count = length(col_data)
      )
    }

    # Render column selection UI
    output$columns_ui <- renderUI({
      filtered_cols <- r_filtered_cols()
      selected_cols <- r_selected()

      # Handle NULL selected_cols
      if (is.null(selected_cols)) {
        selected_cols <- character(0)
      }

      if (length(filtered_cols) == 0) {
        return(div(
          class = "text-muted text-center p-3",
          "No columns match your search criteria"
        ))
      }

      # Create column cards
      column_cards <- lapply(filtered_cols, function(col) {
        col_info <- get_column_info(col)
        is_selected <- col %in% selected_cols

        multi_select_column_card(
          ns(paste0("col_", make.names(col))),
          column_name = col,
          column_info = col_info,
          is_selected = is_selected
        )
      })

      tagList(column_cards)
    })

    # Render selection summary
    output$selection_summary <- renderUI({
      selected_cols <- r_selected()
      total_cols <- length(r_cols())

      # Handle NULL selected_cols
      if (is.null(selected_cols)) {
        selected_cols <- character(0)
      }

      if (length(selected_cols) == 0) {
        return(div(
          class = "alert alert-info",
          "No columns selected"
        ))
      }

      div(
        class = "selection-summary p-2 border rounded",
        div(
          class = "fw-bold mb-2",
          sprintf("Selected %d of %d columns:", length(selected_cols), total_cols)
        ),
        div(
          class = "selected-columns",
          lapply(seq_along(selected_cols), function(i) {
            col <- selected_cols[i]
            span(
              class = "badge bg-primary me-1 mb-1",
              sprintf("%d. %s", i, col)
            )
          })
        )
      )
    })

    # Return the reactive selected columns
    reactive({
      selected <- r_selected()
      if (is.null(selected)) {
        character(0)
      } else {
        selected
      }
    })
  })
}

#' Create multi select UI module
#'
#' @param id The module ID
#' @return A div containing the UI elements
#' @export
mod_multi_select_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style("
      .multi-select-container {
        max-height: 600px;
        overflow-y: auto;
      }

      .column-card {
        border: 1px solid var(--bs-border-color);
        border-radius: 0.375rem;
        padding: 0.75rem;
        margin-bottom: 0.5rem;
        transition: all 0.2s ease;
        cursor: pointer;
      }

      .column-card:hover {
        border-color: var(--bs-primary);
        box-shadow: 0 0 0 0.2rem rgba(var(--bs-primary-rgb), 0.25);
      }

      .column-card.selected {
        border-color: var(--bs-primary);
        background-color: rgba(var(--bs-primary-rgb), 0.1);
      }

      .column-header {
        display: flex;
        align-items: center;
        font-weight: 600;
        margin-bottom: 0.25rem;
      }

      .column-checkbox {
        margin-right: 0.5rem;
      }

      .column-type {
        font-size: 0.875rem;
        color: var(--bs-secondary);
        margin-bottom: 0.25rem;
      }

      .column-sample {
        font-size: 0.75rem;
        color: var(--bs-muted);
        font-family: monospace;
      }

      .column-stats {
        font-size: 0.75rem;
        color: var(--bs-secondary);
        margin-top: 0.25rem;
      }

      .selection-controls {
        display: flex;
        gap: 0.5rem;
        margin-bottom: 1rem;
        flex-wrap: wrap;
      }

      .search-box {
        margin-bottom: 1rem;
      }

      .selected-columns {
        display: flex;
        flex-wrap: wrap;
        gap: 0.25rem;
      }

      .selection-summary {
        margin-top: 1rem;
        background-color: var(--bs-light);
      }
    "),
    div(
      class = "multi-select-container",
      div(
        class = "search-box",
        textInput(
          ns("search"),
          label = "Search columns:",
          placeholder = "Type to filter columns...",
          width = "100%"
        )
      ),
      div(
        class = "selection-controls",
        actionButton(
          ns("select_all"),
          "Select All",
          class = "btn btn-outline-primary btn-sm",
          icon = icon("check-square")
        ),
        actionButton(
          ns("select_none"),
          "Select None",
          class = "btn btn-outline-secondary btn-sm",
          icon = icon("square")
        ),
        actionButton(
          ns("invert_selection"),
          "Invert",
          class = "btn btn-outline-info btn-sm",
          icon = icon("exchange-alt")
        )
      ),
      uiOutput(ns("columns_ui")),
      uiOutput(ns("selection_summary"))
    )
  )
}

#' Create UI for a single column selection card
#'
#' @param id Checkbox input identifier
#' @param column_name Column name to display
#' @param column_info List with column information (type, sample, stats)
#' @param is_selected Whether the column is currently selected
#' @return A div containing the column card UI
multi_select_column_card <- function(id, column_name, column_info, is_selected = FALSE) {
  card_class <- paste("column-card", if (is_selected) "selected" else "")

  div(
    class = card_class,
    onclick = sprintf("document.getElementById('%s').click()", id),
    div(
      class = "column-header",
      checkboxInput(
        id,
        label = NULL,
        value = is_selected,
        width = "auto"
      ),
      span(class = "column-name", column_name)
    ),
    div(
      class = "column-type",
      sprintf("%s (%d obs)", column_info$type, column_info$total_count)
    ),
    if (nchar(column_info$sample) > 0 && column_info$sample != "All NA") {
      div(
        class = "column-sample",
        sprintf("Sample: %s",
          if (nchar(column_info$sample) > 50) {
            paste0(substr(column_info$sample, 1, 47), "...")
          } else {
            column_info$sample
          }
        )
      )
    },
    div(
      class = "column-stats",
      sprintf("Unique: %d | NA: %d",
              column_info$unique_count,
              column_info$na_count)
    )
  )
}

#' Run example app demonstrating multi select functionality
#'
#' @examples
#' \dontrun{
#' run_multi_select_example()
#' }
#' @export
run_multi_select_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      h3("Multi Select Example"),
      mod_multi_select_ui("ms"),
      hr(),
      h4("Selected Columns:"),
      verbatimTextOutput("selected"),
      h4("Generated Code:"),
      verbatimTextOutput("code")
    ),
    server = function(input, output, session) {
      r_result <- mod_multi_select_server(
        "ms",
        get_value = function() c("mpg", "cyl"),
        get_cols = function() colnames(datasets::mtcars),
        get_data_preview = function() datasets::mtcars
      )

      output$selected <- renderPrint({
        selected <- r_result()
        if (length(selected) > 0) {
          cat(paste(selected, collapse = ", "))
        } else {
          cat("No columns selected")
        }
      })

      output$code <- renderPrint({
        selected <- r_result()
        if (length(selected) > 0) {
          cols_str <- paste(selected, collapse = ", ")
          cat(sprintf("dplyr::select(data, %s)", cols_str))
        } else {
          cat("# No columns selected")
        }
      })
    }
  )
}
