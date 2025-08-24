#' Table select module for compact column selection
#'
#' A Shiny module that provides a compact table-based column selection interface
#' with optional selected columns display on top.
#'
#' @param id The module ID
#' @param get_value Function that returns initial selected columns as character vector
#' @param get_cols Function that returns available column names
#' @param get_data_preview Function that returns sample data for column preview
#' @param show_selected_on_top Logical, whether to show selected columns section on top
#'
#' @return A reactive expression containing the currently selected columns
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div textInput checkboxInput icon
#' @importFrom DT DTOutput renderDT datatable formatStyle
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @importFrom utils head
#' @export
mod_table_select_server <- function(
  id,
  get_value,
  get_cols,
  get_data_preview,
  show_selected_on_top = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    initial_columns <- get_value()
    if (!is.character(initial_columns)) {
      initial_columns <- character(0)
    }

    # Store selected columns as reactive value
    r_selected <- reactiveVal()
    r_cols <- reactive(get_cols())
    r_data_preview <- reactive(get_data_preview())

    # Track initialization
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

    # Get column type with color mapping
    get_type_info <- function(col_class) {
      type_colors <- list(
        "numeric" = list(class = "bg-primary", label = "num"),
        "integer" = list(class = "bg-info", label = "int"),
        "character" = list(class = "bg-success", label = "chr"),
        "factor" = list(class = "bg-warning", label = "fct"),
        "logical" = list(class = "bg-secondary", label = "lgl"),
        "Date" = list(class = "bg-primary", label = "date"),
        "POSIXct" = list(class = "bg-primary", label = "dttm"),
        "POSIXlt" = list(class = "bg-primary", label = "dttm"),
        "matrix" = list(class = "bg-dark", label = "mat"),
        "list" = list(class = "bg-dark", label = "list"),
        "data.frame" = list(class = "bg-dark", label = "df"),
        "unknown" = list(class = "bg-light text-dark", label = "?")
      )

      # Default for unknown types
      info <- type_colors[["unknown"]]

      # Find matching type
      for (type_name in names(type_colors)) {
        if (type_name %in% col_class) {
          info <- type_colors[[type_name]]
          break
        }
      }

      info
    }

    # Get column information for all columns
    get_columns_data <- reactive({
      cols <- r_cols()
      data_preview <- r_data_preview()
      selected_cols <- r_selected()

      if (is.null(selected_cols)) {
        selected_cols <- character(0)
      }

      # Create data frame for table display
      data.frame(
        Selected = cols %in% selected_cols,
        Column = cols,
        Type = sapply(cols, function(col) {
          if (is.null(data_preview) || !col %in% names(data_preview)) {
            col_class <- "unknown"
          } else {
            col_class <- class(data_preview[[col]])
          }

          type_info <- get_type_info(col_class)
          sprintf(
            '<span class="badge %s" style="font-size: 0.75rem;">%s</span>',
            type_info$class,
            type_info$label
          )
        }),
        Sample = sapply(cols, function(col) {
          if (is.null(data_preview) || !col %in% names(data_preview)) {
            return('<span class="text-muted font-monospace">\u2014</span>')
          }
          col_data <- data_preview[[col]]
          sample_vals <- head(col_data[!is.na(col_data)], 3)
          if (length(sample_vals) > 0) {
            sample_str <- paste(sample_vals, collapse = ", ")
            if (nchar(sample_str) > 40) {
              sample_str <- paste0(substr(sample_str, 1, 37), "\u2026")
            }
            sprintf(
              '<span class="font-monospace text-muted">%s</span>',
              sample_str
            )
          } else {
            '<span class="text-muted font-monospace">All NA</span>'
          }
        }),
        stringsAsFactors = FALSE
      )
    })

    # Render the data table
    output$columns_table <- renderDT({
      columns_data <- get_columns_data()
      search_term <- r_search()

      # Apply search filter
      if (!is.null(search_term) && search_term != "") {
        filtered_rows <- grepl(
          search_term,
          columns_data$Column,
          ignore.case = TRUE
        )
        columns_data <- columns_data[filtered_rows, , drop = FALSE]
      }

      datatable(
        columns_data,
        selection = "none",
        options = list(
          paging = FALSE, # Disable pagination for infinite scroll
          scrollY = "400px", # Set fixed height for scrolling
          scrollCollapse = TRUE, # Allow table to shrink if fewer rows
          lengthChange = FALSE,
          searching = FALSE, # We handle search ourselves
          info = TRUE,
          ordering = TRUE,
          rowCallback = DT::JS(
            "
            function(row, data) {
              if (data[0] === true) {
                $(row).addClass('selected-row');
              } else {
                $(row).removeClass('selected-row');
              }
            }
          "
          ),
          columnDefs = list(
            list(
              targets = 0, # Selected column
              searchable = FALSE,
              orderable = TRUE, # Enable sorting on selection column
              className = "dt-center",
              width = "60px",
              render = DT::JS(
                "
                function(data, type, row, meta) {
                  var checked = data ? 'checked' : '';
                  return '<input type=\"checkbox\" class=\"column-checkbox\" data-column=\"' +
                         row[1] + '\" ' + checked + '>';
                }
              "
              )
            ),
            list(
              targets = 1, # Column name
              width = "25%",
              className = "dt-left font-weight-bold"
            ),
            list(
              targets = 2, # Type
              width = "15%",
              className = "dt-center"
            ),
            list(
              targets = 3, # Sample
              width = "60%",
              className = "dt-left"
            )
          )
        ),
        escape = FALSE,
        rownames = FALSE,
        colnames = c("", "Column", "Type", "Sample")
      )
    })

    # Handle checkbox clicks in the table
    observeEvent(input$columns_table_cell_clicked, {
      clicked <- input$columns_table_cell_clicked

      # Validate that we have all necessary information
      if (
        !is.null(clicked) &&
          !is.null(clicked$col) &&
          !is.null(clicked$row) &&
          length(clicked$col) > 0 &&
          length(clicked$row) > 0 &&
          clicked$col == 0
      ) {
        # Checkbox column

        row_index <- clicked$row
        columns_data <- get_columns_data()
        search_term <- r_search()

        # Apply same filtering as in table render
        if (!is.null(search_term) && search_term != "") {
          filtered_rows <- grepl(
            search_term,
            columns_data$Column,
            ignore.case = TRUE
          )
          columns_data <- columns_data[filtered_rows, , drop = FALSE]
        }

        if (row_index > 0 && row_index <= nrow(columns_data)) {
          clicked_column <- columns_data$Column[row_index]
          current_selected <- r_selected()

          if (
            !is.null(current_selected) && clicked_column %in% current_selected
          ) {
            # Remove column
            r_selected(setdiff(current_selected, clicked_column))
          } else {
            # Add column
            if (is.null(current_selected)) {
              r_selected(clicked_column)
            } else {
              r_selected(c(current_selected, clicked_column))
            }
          }
        }
      }
    })

    # Select All button
    observeEvent(input$select_all, {
      r_selected(r_cols())
    })

    # Select None button
    observeEvent(input$select_none, {
      r_selected(character(0))
    })

    # Invert Selection button
    observeEvent(input$invert_selection, {
      current_selected <- r_selected()
      available_cols <- r_cols()
      new_selection <- setdiff(available_cols, current_selected)
      r_selected(new_selection)
    })

    # Render selected columns on top (if enabled)
    output$selected_columns_top <- renderUI({
      if (!show_selected_on_top) {
        return(NULL)
      }

      selected_cols <- r_selected()

      if (is.null(selected_cols) || length(selected_cols) == 0) {
        return(div(
          class = "alert alert-info",
          icon("info-circle"),
          " No columns selected"
        ))
      }

      div(
        class = "selected-columns-top mb-3 p-3 border rounded bg-light",
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          h6(
            class = "mb-0",
            sprintf("Selected Columns (%d)", length(selected_cols))
          ),
          tags$small(
            class = "text-muted",
            "Order determines final column sequence"
          )
        ),
        div(
          class = "selected-chips",
          lapply(seq_along(selected_cols), function(i) {
            col <- selected_cols[i]
            span(
              class = "badge bg-primary me-2 mb-1 position-relative",
              style = "font-size: 0.8rem; padding: 0.4rem 0.7rem;",
              sprintf("%d. %s", i, col),
              tags$button(
                type = "button",
                class = "btn-close btn-close-white ms-1",
                style = "font-size: 0.6rem;",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                  ns("remove_column"),
                  col
                ),
                `aria-label` = "Remove"
              )
            )
          })
        )
      )
    })

    # Handle remove column from selected chips
    observeEvent(input$remove_column, {
      current_selected <- r_selected()
      r_selected(setdiff(current_selected, input$remove_column))
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

#' Create table select UI module
#'
#' @param id The module ID
#' @param show_selected_on_top Logical, whether to show selected columns section on top
#' @return A div containing the UI elements
#' @export
mod_table_select_ui <- function(id, show_selected_on_top = TRUE) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      .table-select-container {
        /* Removed max-height and overflow-y as DataTable handles scrolling internally */
      }

      .selected-columns-top {
        max-height: 150px;
        overflow-y: auto;
      }

      .selected-chips {
        display: flex;
        flex-wrap: wrap;
        gap: 0.25rem;
      }

      .column-checkbox {
        cursor: pointer;
        transform: scale(1.1);
      }

      .search-box {
        margin-bottom: 1rem;
      }

      .selection-controls {
        display: flex;
        gap: 0.5rem;
        margin-bottom: 1rem;
        flex-wrap: wrap;
      }

      .dataTables_wrapper .dataTables_info {
        font-size: 0.875rem;
        color: var(--bs-secondary);
      }

      .dataTables_wrapper .dataTables_paginate {
        margin-top: 0.5rem;
      }

      .dataTables_wrapper table.dataTable thead th {
        border-bottom: 2px solid var(--bs-border-color);
        font-weight: 600;
        background-color: var(--bs-light);
      }

      .dataTables_wrapper table.dataTable tbody tr:hover {
        background-color: rgba(var(--bs-primary-rgb), 0.05) !important;
      }

      .dataTables_wrapper table.dataTable tbody tr.selected-row {
        background-color: rgba(var(--bs-success-rgb), 0.08) !important;
        border-left: 3px solid var(--bs-success) !important;
      }

      .dataTables_wrapper table.dataTable tbody tr.selected-row:hover {
        background-color: rgba(var(--bs-success-rgb), 0.15) !important;
      }

      .table-select-container .badge {
        font-weight: 500;
        letter-spacing: 0.025em;
      }
    "
    ),

    # Selected columns on top (if enabled)
    if (show_selected_on_top) {
      uiOutput(ns("selected_columns_top"))
    },
    div(
      class = "table-select-container",
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
      DTOutput(ns("columns_table"))
    )
  )
}
