#' Value Filter block constructor
#'
#' This block allows filtering rows in a data frame by selecting specific values
#' from columns (see [dplyr::filter()]). Provides a visual interface where users
#' can select columns and choose which values to include or exclude without
#' writing R expressions. Supports multiple conditions with AND/OR logic.
#'
#' @param conditions List of filter conditions. Each condition should be a list
#'   with elements: column (character), values (vector), mode ("include" or "exclude"),
#'   optionally type ("values" or "range") for numeric filters, and optionally
#'   operator ("&" or "|") specifying how this condition connects to the previous one
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

          # Reactive expression that updates automatically when conditions change
          r_expr <- reactive({
            current_conditions <- r_conditions()

            # Always try to parse, even with empty/invalid conditions
            tryCatch(
              {
                parse_value_filter(current_conditions)
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

      if (is.null(condition$column) || condition$column == "") {
        next # Skip conditions without a column
      }

      column <- condition$column
      values <- condition$values
      mode <- condition$mode %||% "include"
      type <- condition$type %||% "values"

      # Handle range conditions (from slider)
      if (type == "range" && length(values) == 2 && is.numeric(values)) {
        min_val <- values[1]
        max_val <- values[2]

        if (mode == "include") {
          filter_part <- glue::glue(
            "`{column}` >= {min_val} & `{column}` <= {max_val}"
          )
        } else {
          filter_part <- glue::glue(
            "!(`{column}` >= {min_val} & `{column}` <= {max_val})"
          )
        }
      } else {
        # Handle empty values - skip conditions with no values
        if (is.null(values) || length(values) == 0) {
          next # Skip conditions without values
        }

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
      }

      filter_parts <- c(filter_parts, filter_part)
    }

    if (length(filter_parts) == 0) {
      text <- "dplyr::filter(data, TRUE)"
    } else if (length(filter_parts) == 1) {
      # Single condition - no operators needed
      text <- glue::glue("dplyr::filter(data, {filter_parts[1]})")
    } else {
      # Multiple conditions - combine with operators from conditions
      combined_filter <- filter_parts[1]
      for (i in 2:length(filter_parts)) {
        # Get operator from the condition (default to AND if not specified)
        operator <- conditions[[i]]$operator %||% "&"
        op <- if (operator == "|") " | " else " & "
        combined_filter <- paste0(combined_filter, op, filter_parts[i])
      }
      text <- glue::glue("dplyr::filter(data, {combined_filter})")
    }
  }

  parse(text = text)[1]
}
