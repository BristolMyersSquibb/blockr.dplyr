# Helper function to extract argument names for variadic blocks
# Copied from blockr.core:::dot_args_names (not exported)
dot_args_names <- function(x) {
  res <- names(x)
  unnamed <- grepl("^[1-9][0-9]*$", res)

  if (all(unnamed)) {
    return(NULL)
  }

  if (any(unnamed)) {
    return(replace(res, unnamed, ""))
  }

  res
}

#' Bind Rows Block Constructor
#'
#' This block allows for row-wise combination of two or more data frames
#' using [dplyr::bind_rows()]. It stacks data frames vertically, matching
#' columns by name and filling missing columns with NA values.
#'
#' @param ... Forwarded to [new_block()]
#'
#' @return A block object for bind_rows operations
#' @export
new_bind_rows_block <- function(...) {
  new_transform_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {

          arg_names <- reactive(
            set_names(names(...args), dot_args_names(...args))
          )

          list(
            expr = reactive(
              bquote(
                dplyr::bind_rows(..(dat)),
                list(dat = lapply(arg_names(), as.name)),
                splice = TRUE
              )
            ),
            state = list()
          )
        }
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = c("bind_rows_block", "rbind_block"),
    ...
  )
}

#' Bind Rows Block Constructor (OLD IMPLEMENTATION - ARCHIVED)
#'
#' This is the old implementation kept for reference.
#' Use new_bind_rows_block() instead.
#'
#' @param add_id Logical, whether to add a column identifying source data frames
#' @param id_name Character string, name for the ID column (if add_id = TRUE)
#' @param ... Forwarded to [new_block()]
#'
#' @keywords internal
new_bind_rows_block_old <- function(
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

          # Build bind_rows expression
          build_bind_expr <- function(add_id, id_name) {
            if (add_id) {
              # Create named list with ID labels
              parse(
                text = glue::glue(
                  "dplyr::bind_rows(`1` = x, `2` = y, .id = \"{id_name}\")"
                )
              )[[1]]
            } else {
              # Simple bind without ID
              quote(dplyr::bind_rows(x, y))
            }
          }

          list(
            expr = reactive({
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

          # Add ID column option
          div(
            class = "form-group",
            checkboxInput(
              NS(id, "add_id"),
              label = tags$small(
                class = "text-muted",
                "Add column to identify dataset"
              ),
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
                label = tags$small(
                  class = "text-muted",
                  "ID column name:"
                ),
                value = id_name,
                placeholder = "Enter column name"
              )
            )
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
#' requiring them to have the same number of rows. Duplicate column names
#' are automatically handled by dplyr.
#'
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

          list(
            expr = reactive({
              req(rows_compatible()) # Only proceed if rows are compatible
              quote(dplyr::bind_cols(x, y))
            }),
            state = list()
          )
        }
      )
    },
    function(id) {
      tagList()
    },
    class = "bind_cols_block",
    ...
  )
}
