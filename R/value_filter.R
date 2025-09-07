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
          filter_result <- mod_value_filter_server(
            id = "vf",
            get_value = \() conditions,
            get_data = data
          )

          r_conditions <- filter_result$conditions
          r_logic_operators <- filter_result$logic_operators

          # Reactive expression that updates automatically when conditions change
          r_expr <- reactive({
            current_conditions <- r_conditions()
            current_logic <- r_logic_operators()

            # Always try to parse, even with empty/invalid conditions
            tryCatch(
              {
                parse_value_filter(current_conditions, current_logic)
              },
              error = function(e) {
                # Fallback to identity filter if parsing fails
                parse(text = "dplyr::filter(data, TRUE)")[1]
              }
            )
          })

          list(
            expr = r_expr,
            state = list(
              conditions = r_conditions
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        # Use value filter UI - no Submit button needed for reactive filtering
        mod_value_filter_ui(NS(id, "vf"))
      )
    },
    class = "value_filter_block",
    ...
  )
}

#' Parse value filter conditions into dplyr expression
#'
#' @param conditions List of filter conditions
#' @param logic_operators Character vector of logic operators ("&" or "|")
#' @return A parsed expression object
parse_value_filter <- function(
  conditions = list(),
  logic_operators = character()
) {
  if (length(conditions) == 0) {
    # No conditions - return identity filter
    text <- "dplyr::filter(data, TRUE)"
  } else {
    # Build filter expressions from conditions
    filter_parts <- character(0)

    for (i in seq_along(conditions)) {
      condition <- conditions[[i]]

      if (is.null(condition$column) || condition$column == "") {
        next # Skip conditions without a column
      }

      column <- condition$column
      values <- condition$values
      mode <- condition$mode %||% "include"

      # Handle empty values
      if (is.null(values) || length(values) == 0) {
        values <- character(0)
      }

      # Format values for R expression
      if (length(values) == 0) {
        values_str <- "" # Empty vector: c()
      } else if (is.numeric(values)) {
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
      # Combine with logic operators
      if (
        length(logic_operators) == 0 ||
          length(logic_operators) != (length(filter_parts) - 1)
      ) {
        # Default to AND logic if no operators or wrong count
        combined_filter <- paste(filter_parts, collapse = " & ")
      } else {
        # Use provided logic operators
        combined_filter <- filter_parts[1]
        for (i in seq_along(logic_operators)) {
          op <- if (logic_operators[i] == "|") " | " else " & "
          combined_filter <- paste(combined_filter, op, filter_parts[i + 1])
        }
      }
      text <- glue::glue("dplyr::filter(data, {combined_filter})")
    }
  }

  parse(text = text)[1]
}


#' Default value operator
#' @param x First value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
