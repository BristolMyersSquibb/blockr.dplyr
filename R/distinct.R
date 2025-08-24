#' Distinct block constructor
#'
#' This block allows for removing duplicate rows from a data.frame
#' based on selected columns (see [dplyr::distinct()]).
#'
#' @param columns Character vector of column names to check for uniqueness
#' @param .keep_all Logical. If TRUE, keep all columns in the output. If FALSE,
#'   only keep the columns used for determining uniqueness.
#' @param ... Forwarded to [new_block()]
#'
#' @importFrom dplyr distinct
#' @importFrom glue glue
#'
#' @export
new_distinct_block <- function(
    columns = character(),
    .keep_all = TRUE,
    ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          # Reactive for selected columns
          selected_columns <- reactiveVal(columns)
          keep_all <- reactiveVal(.keep_all)

          # Get available columns from data
          available_columns <- reactive({
            req(data())
            colnames(data())
          })

          # Count duplicates for preview
          duplicate_count <- reactive({
            req(data())
            df <- data()

            total_rows <- nrow(df)
            cols <- selected_columns()

            if (length(cols) == 0) {
              # No columns selected: check duplicates across all columns
              unique_rows <- nrow(unique(df))
            } else {
              # Selected columns determine uniqueness
              unique_rows <- nrow(unique(df[, cols, drop = FALSE]))
            }

            total_rows - unique_rows
          })

          # Update selected columns from input
          observeEvent(input$columns, {
            selected_columns(input$columns)
          })

          # Update keep_all from input
          observeEvent(input$keep_all, {
            keep_all(input$keep_all)
          })

          # UI output
          output$distinct_ui <- renderUI({
            req(available_columns())

            tagList(
              div(
                class = "mb-3",
                selectInput(
                  ns("columns"),
                  label = "Select columns for uniqueness check:",
                  choices = available_columns(),
                  selected = selected_columns(),
                  multiple = TRUE,
                  width = "100%"
                ),
                helpText("Leave empty to check all columns for duplicates")
              ),
              div(
                class = "mb-3",
                checkboxInput(
                  ns("keep_all"),
                  label = "Keep all columns in output",
                  value = keep_all()
                )
              ),
              div(
                class = "alert alert-info",
                icon("info-circle"),
                " ",
                if (duplicate_count() > 0) {
                  paste0("Found ", duplicate_count(), " duplicate row(s) that will be removed")
                } else {
                  "No duplicate rows found"
                }
              )
            )
          })

          # Return the standard blockr.core interface
          list(
            expr = reactive({
              # Build the distinct expression
              if (length(selected_columns()) == 0) {
                # If no columns selected, remove duplicates across all columns
                parse(text = "dplyr::distinct(data)")[[1]]
              } else {
                # Remove duplicates based on selected columns
                cols_expr <- paste(sprintf("`%s`", selected_columns()), collapse = ", ")
                if (keep_all()) {
                  parse(text = glue::glue("dplyr::distinct(data, {cols_expr}, .keep_all = TRUE)"))[[1]]
                } else {
                  parse(text = glue::glue("dplyr::distinct(data, {cols_expr}, .keep_all = FALSE)"))[[1]]
                }
              }
            }),
            state = list(
              columns = selected_columns,
              .keep_all = keep_all
            )
          )
        }
      )
    },
    function(id) {
      ns <- shiny::NS(id)
      uiOutput(ns("distinct_ui"))
    },
    submit = "none",
    class = "distinct_block",
    allow_empty_state = "columns",
    ...
  )
}
