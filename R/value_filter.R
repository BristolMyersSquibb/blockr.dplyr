#' Value Filter block constructor
#'
#' This block allows filtering rows in a data frame by selecting specific values
#' from columns (see [dplyr::filter()]). Provides a visual interface where users
#' can select columns and choose which values to include or exclude without
#' writing R expressions. Supports multiple conditions with AND/OR logic.
#'
#' @param conditions List of filter conditions. Each condition should be a list
#'   with elements: column (character), values (vector), mode ("include" or "exclude")
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for value-based filter operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon div
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_value_filter_block(), list(data = mtcars))
#'
#' # With initial condition
#' serve(new_value_filter_block(
#'   conditions = list(
#'     list(column = "cyl", values = c(4, 6), mode = "include")
#'   )
#' ), list(data = mtcars))
#'
#' # Connected blocks example
#' serve(
#'   new_board(
#'     blocks = list(
#'       a = new_dataset_block(),
#'       b = new_value_filter_block()
#'     ),
#'     links = links(
#'       from = c("a"),
#'       to = c("b")
#'     )
#'   )
#' )
#' }
#' @export
new_value_filter_block <- function(conditions = list(), ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Use value filter interface
          r_conditions <- mod_value_filter_server(
            id = "vf",
            get_value = \() conditions,
            get_data = data
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_value_filter(list()))
          r_conditions_validated <- reactiveVal(conditions)

          # Validate and update on submit
          observeEvent(input$submit, {
            apply_value_filter(
              data(),
              r_conditions(),
              r_expr_validated,
              r_conditions_validated
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              conditions = r_conditions_validated
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        # Use value filter UI
        mod_value_filter_ui(NS(id, "vf")),
        div(
          style = "text-align: right; margin-top: 10px;",
          actionButton(
            NS(id, "submit"),
            "Submit",
            icon = icon("paper-plane"),
            class = "btn-primary"
          )
        )
      )
    },
    class = "value_filter_block",
    ...
  )
}

#' Parse value filter conditions into dplyr expression
#'
#' @param conditions List of filter conditions
#' @return A parsed expression object
parse_value_filter <- function(conditions = list()) {
  if (length(conditions) == 0) {
    # No conditions - return identity filter
    text <- "dplyr::filter(data, TRUE)"
  } else {
    # Build filter expressions from conditions
    filter_parts <- character(0)
    
    for (i in seq_along(conditions)) {
      condition <- conditions[[i]]
      
      if (is.null(condition$column) || is.null(condition$values) || 
          length(condition$values) == 0) {
        next  # Skip invalid conditions
      }
      
      column <- condition$column
      values <- condition$values
      mode <- condition$mode %||% "include"
      
      # Format values for R expression
      if (is.numeric(values)) {
        values_str <- paste(values, collapse = ", ")
      } else {
        values_str <- paste(sprintf('"%s"', values), collapse = ", ")
      }
      
      # Build the condition string
      if (mode == "include") {
        filter_part <- glue::glue("`{column}` %in% c({values_str})")
      } else {
        filter_part <- glue::glue("!(`{column}` %in% c({values_str}))")
      }
      
      filter_parts <- c(filter_parts, filter_part)
    }
    
    if (length(filter_parts) == 0) {
      text <- "dplyr::filter(data, TRUE)"
    } else {
      # Combine with AND logic (can be extended for OR logic later)
      combined_filter <- paste(filter_parts, collapse = " & ")
      text <- glue::glue("dplyr::filter(data, {combined_filter})")
    }
  }
  
  parse(text = text)[1]
}

#' Apply value filter conditions with validation
#'
#' @param data Input data frame
#' @param conditions List of filter conditions
#' @param r_expr_validated Reactive value for validated expression
#' @param r_conditions_validated Reactive value for validated conditions
apply_value_filter <- function(data, conditions, r_expr_validated, r_conditions_validated) {
  # Validate conditions
  if (!is.list(conditions)) {
    showNotification(
      "Invalid filter conditions format",
      type = "error",
      duration = 5
    )
    return()
  }
  
  # Parse and validate expression
  expr <- try(parse_value_filter(conditions))
  if (inherits(expr, "try-error")) {
    showNotification(
      paste("Filter parsing error:", expr),
      type = "error",
      duration = 5
    )
    return()
  }
  
  # Test evaluation
  ans <- try(eval(expr))
  if (inherits(ans, "try-error")) {
    showNotification(
      paste("Filter evaluation error:", ans),
      type = "error",
      duration = 5
    )
    return()
  }
  
  # Update reactive values
  r_expr_validated(expr)
  r_conditions_validated(conditions)
}

#' Default value operator
#' @param x First value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}