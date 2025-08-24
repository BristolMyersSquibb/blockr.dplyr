#' Arrange block constructor
#'
#' This block allows allows you to order the rows of a data frame by the values
#' of selected columns (see [dplyr::arrange()]).
#'
#' @param columns Columns to arrange by. Can be a character vector (ascending order)
#'   or a list of specifications with column and direction.
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_arrange_block <- function(columns = character(), ...) {
  # Convert inputs to arrange specifications
  if (is.character(columns) && length(columns) > 0) {
    # Simple character vector - default to ascending order
    initial_arranges <- lapply(columns, function(col) {
      list(column = col, direction = "asc")
    })
  } else if (is.list(columns)) {
    # Already in list format
    initial_arranges <- columns
  } else {
    # Empty or invalid input
    initial_arranges <- list()
  }

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          cols <- reactive(colnames(data()))

          # Use the multi arrange module
          r_arranges <- mod_multi_arrange_server(
            "multi_arrange",
            get_value = function() initial_arranges,
            get_cols = cols
          )

          list(
            expr = reactive({
              arranges <- r_arranges()

              if (length(arranges) == 0) {
                # No arrange specifications - return identity
                parse(text = "data")[1]
              } else {
                # Build arrange expressions
                arrange_exprs <- sapply(arranges, function(arr) {
                  if (arr$direction == "desc") {
                    sprintf("dplyr::desc(%s)", arr$column)
                  } else {
                    arr$column
                  }
                })

                arrange_str <- paste(arrange_exprs, collapse = ", ")
                parse(text = glue::glue("dplyr::arrange(data, {arrange_str})"))[
                  1
                ]
              }
            }),
            state = list(
              arranges = r_arranges,
              columns = reactive({
                arranges <- r_arranges()
                if (length(arranges) > 0) {
                  sapply(arranges, function(arr) arr$column)
                } else {
                  character(0)
                }
              })
            )
          )
        }
      )
    },
    function(id) {
      mod_multi_arrange_ui(NS(id, "multi_arrange"))
    },
    class = "arrange_block",
    ...
  )
}
