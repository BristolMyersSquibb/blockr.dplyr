#' Value Filter block constructor
#'
#' This block allows filtering rows in a data frame by selecting specific values
#' from columns (see [dplyr::filter()]). Provides a visual interface where users
#' can select columns and choose which values to include or exclude without
#' writing R expressions. Supports multiple conditions with AND/OR logic.
#'
#' @param conditions List of filter conditions. Each condition should be a list
#'   with elements: column (character), values (vector), mode ("include" or "exclude"),
#'   and optionally operator ("&" or "|") specifying how this condition connects to
#'   the previous one
#' @param preserve_order Logical. If TRUE, preserves the order of selected values
#'   in the filtered output (default: FALSE)
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
new_value_filter_block <- function(conditions = list(), preserve_order = FALSE, ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Use value filter interface
          filter_result <- mod_value_filter_server(
            id = "vf",
            get_value = \() conditions,
            get_data = data,
            preserve_order = preserve_order
          )

          r_conditions <- filter_result$conditions
          r_preserve_order <- filter_result$preserve_order

          # Reactive expression that updates automatically when conditions change
          r_expr <- reactive({
            current_conditions <- r_conditions()
            current_preserve_order <- r_preserve_order()

            # Always try to parse, even with empty/invalid conditions
            tryCatch(
              {
                parse_value_filter(current_conditions, preserve_order = current_preserve_order)
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
              conditions = r_conditions,
              preserve_order = r_preserve_order
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
#' @param preserve_order Logical. If TRUE, preserves order of selected values
#' @return A parsed expression object
parse_value_filter <- function(conditions = list(), preserve_order = FALSE) {
  if (length(conditions) == 0) {
    # No conditions - return identity filter
    text <- "dplyr::filter(data, TRUE)"
  } else {
    # Build filter expressions from conditions
    filter_parts <- character(0)
    # Track conditions that need order preservation
    order_columns <- character(0)
    order_values_list <- list()

    for (i in seq_along(conditions)) {
      condition <- conditions[[i]]

      if (is.null(condition$column) || condition$column == "") {
        next # Skip conditions without a column
      }

      column <- condition$column
      values <- condition$values
      mode <- condition$mode %||% "include"

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
        # Track this for order preservation if enabled
        if (preserve_order) {
          order_columns <- c(order_columns, column)
          order_values_list[[column]] <- values_str
        }
      } else {
        filter_part <- glue::glue("!(`{column}` %in% c({values_str}))")
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

    # Add arrange() to preserve order if requested
    if (preserve_order && length(order_columns) > 0) {
      # Build arrange expressions for each column that needs ordering
      arrange_parts <- character(0)
      for (col in order_columns) {
        values_str <- order_values_list[[col]]
        arrange_parts <- c(
          arrange_parts,
          glue::glue("match(`{col}`, c({values_str}))")
        )
      }

      if (length(arrange_parts) > 0) {
        arrange_expr <- paste(arrange_parts, collapse = ", ")
        text <- glue::glue("{text} |> dplyr::arrange({arrange_expr})")
      }
    }
  }

  parse(text = text)[1]
}
