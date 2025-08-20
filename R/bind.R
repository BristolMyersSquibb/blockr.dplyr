#' Bind Rows Block Constructor
#'
#' This block allows for row-wise combination of two or more data frames
#' using [dplyr::bind_rows()]. It stacks data frames vertically, matching
#' columns by name and filling missing columns with NA values.
#'
#' @param add_id Logical, whether to add a column identifying source data frames
#' @param id_name Character string, name for the ID column (if add_id = TRUE)
#' @param ... Forwarded to [new_block()]
#'
#' @return A block object for bind_rows operations
#' @export
new_bind_rows_block <- function(
  add_id = FALSE,
  id_name = ".id",
  ...
) {
  new_transform_block(
    function(id, x, y) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_add_id <- reactiveVal(add_id)
          r_id_name <- reactiveVal(id_name)
          
          # Update settings when inputs change
          observeEvent(input$add_id, {
            r_add_id(input$add_id)
          })
          
          observeEvent(input$id_name, {
            if (nzchar(input$id_name)) {
              r_id_name(input$id_name)
            }
          })
          
          # Generate data preview
          output$data_preview <- renderText({
            req(x(), y())
            x_data <- x()
            y_data <- y()
            
            x_cols <- colnames(x_data)
            y_cols <- colnames(y_data)
            common_cols <- intersect(x_cols, y_cols)
            x_only <- setdiff(x_cols, y_cols)
            y_only <- setdiff(y_cols, x_cols)
            
            preview_text <- paste0(
              "Left dataset: ", nrow(x_data), " rows, ", ncol(x_data), " columns\n",
              "Right dataset: ", nrow(y_data), " rows, ", ncol(y_data), " columns\n",
              "Result: ", nrow(x_data) + nrow(y_data), " rows, ", 
              length(union(x_cols, y_cols)), " columns\n\n"
            )
            
            if (length(common_cols) > 0) {
              preview_text <- paste0(preview_text, 
                "Common columns: ", paste(common_cols, collapse = ", "), "\n")
            }
            
            if (length(x_only) > 0) {
              preview_text <- paste0(preview_text, 
                "Left-only columns: ", paste(x_only, collapse = ", "), 
                " (will be filled with NA in right data)\n")
            }
            
            if (length(y_only) > 0) {
              preview_text <- paste0(preview_text, 
                "Right-only columns: ", paste(y_only, collapse = ", "), 
                " (will be filled with NA in left data)\n")
            }
            
            if (r_add_id()) {
              preview_text <- paste0(preview_text, "\n",
                "ID column '", r_id_name(), "' will be added to identify source datasets")
            }
            
            preview_text
          })
          
          # Build bind_rows expression
          build_bind_expr <- function(add_id, id_name) {
            if (add_id) {
              # Create named list with ID labels
              bquote(
                dplyr::bind_rows(
                  `1` = x, 
                  `2` = y, 
                  .id = .(id_col)
                ),
                list(id_col = id_name)
              )
            } else {
              # Simple bind without ID
              quote(dplyr::bind_rows(x, y))
            }
          }
          
          list(
            expr = eventReactive(input$submit, {
              build_bind_expr(r_add_id(), r_id_name())
            }),
            state = list(
              add_id = r_add_id,
              id_name = r_id_name
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        # Configuration options
        div(
          class = "mb-3",
          h5("Bind Rows Configuration", style = "margin-bottom: 15px;"),
          
          # Add ID column option
          div(
            class = "form-group",
            checkboxInput(
              NS(id, "add_id"),
              label = "Add ID column to identify source datasets",
              value = add_id
            )
          ),
          
          # ID column name input
          conditionalPanel(
            condition = sprintf("input['%s']", NS(id, "add_id")),
            div(
              class = "form-group",
              textInput(
                NS(id, "id_name"),
                label = "ID column name:",
                value = id_name,
                placeholder = "Enter column name"
              )
            )
          )
        ),
        
        # Data preview
        div(
          class = "data-preview mb-3 p-3",
          style = "background-color: #f8f9fa; border-radius: 4px; border: 1px solid #dee2e6;",
          h6("Operation Preview:", style = "margin-bottom: 10px; color: #495057;"),
          verbatimTextOutput(NS(id, "data_preview"))
        ),
        
        # Submit button
        div(
          class = "d-flex justify-content-end",
          actionButton(
            inputId = NS(id, "submit"),
            label = "Apply Bind Rows",
            class = "btn-primary"
          )
        )
      )
    },
    class = "bind_rows_block",
    ...
  )
}

#' Bind Columns Block Constructor
#'
#' This block allows for column-wise combination of two data frames
#' using [dplyr::bind_cols()]. It combines data frames side-by-side,
#' requiring them to have the same number of rows.
#'
#' @param suffix Character vector of length 2, suffixes to add to duplicate column names
#' @param ... Forwarded to [new_block()]
#'
#' @return A block object for bind_cols operations
#' @export
new_bind_cols_block <- function(...) {
  new_transform_block(
    function(id, x, y) {
      moduleServer(
        id,
        function(input, output, session) {
          # Check row count compatibility
          rows_compatible <- reactive({
            req(x(), y())
            nrow(x()) == nrow(y())
          })
          
          # Generate data preview
          output$data_preview <- renderText({
            req(x(), y())
            x_data <- x()
            y_data <- y()
            
            x_cols <- colnames(x_data)
            y_cols <- colnames(y_data)
            duplicate_cols <- intersect(x_cols, y_cols)
            
            if (!rows_compatible()) {
              return(paste0(
                "⚠️ ERROR: Row count mismatch!\n",
                "Left dataset: ", nrow(x_data), " rows\n",
                "Right dataset: ", nrow(y_data), " rows\n",
                "bind_cols() requires both datasets to have the same number of rows."
              ))
            }
            
            preview_text <- paste0(
              "Left dataset: ", nrow(x_data), " rows, ", ncol(x_data), " columns\n",
              "Right dataset: ", nrow(y_data), " rows, ", ncol(y_data), " columns\n",
              "Result: ", nrow(x_data), " rows, ", ncol(x_data) + ncol(y_data), " columns\n"
            )
            
            if (length(duplicate_cols) > 0) {
              preview_text <- paste0(preview_text, "\n",
                "Duplicate column names found: ", paste(duplicate_cols, collapse = ", "), "\n",
                "dplyr will automatically rename them (e.g., ", duplicate_cols[1], "...1, ", duplicate_cols[1], "...2)"
              )
            } else {
              preview_text <- paste0(preview_text, "\n", "No duplicate column names.")
            }
            
            preview_text
          })
          
          # Enable/disable submit based on row compatibility
          observe({
            shinyjs::toggleState("submit", condition = rows_compatible())
          })
          
          list(
            expr = eventReactive(input$submit, {
              quote(dplyr::bind_cols(x, y))
            }),
            state = list()
          )
        }
      )
    },
    function(id) {
      tagList(
        # Data preview
        div(
          class = "data-preview mb-3 p-3",
          style = "background-color: #f8f9fa; border-radius: 4px; border: 1px solid #dee2e6;",
          h6("Operation Preview:", style = "margin-bottom: 10px; color: #495057;"),
          verbatimTextOutput(NS(id, "data_preview"))
        ),
        
        # Submit button
        div(
          class = "d-flex justify-content-end",
          actionButton(
            inputId = NS(id, "submit"),
            label = "Apply Bind Columns",
            class = "btn-primary"
          )
        )
      )
    },
    class = "bind_cols_block",
    ...
  )
}