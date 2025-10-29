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
                parse(text = "data")[[1]]
              } else {
                # Build arrange expressions
                arrange_exprs <- sapply(arranges, function(arr) {
                  # Apply backticks to non-syntactic column names
                  col_name <- backtick_if_needed(arr$column)
                  if (arr$direction == "desc") {
                    sprintf("dplyr::desc(%s)", col_name)
                  } else {
                    col_name
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
      tagList(
        shinyjs::useShinyjs(),

        # Add CSS
        css_responsive_grid(),
        css_single_column("arrange"),

        # Block-specific CSS
        tags$style(HTML(
          "
          .arrange-block-container .block-help-text p {
            margin-bottom: 0;
          }
          "
        )),

        div(
          class = "block-container arrange-block-container",

          div(
            class = "block-form-grid",

            # Arrange Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  p(
                    "Sort rows by columns. First row has highest priority. ",
                    tags$a(
                      href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#arrange-block",
                      target = "_blank",
                      style = "text-decoration: none; font-size: 0.9em;",
                      "\u2197"
                    )
                  )
                ),
                mod_multi_arrange_ui(NS(id, "multi_arrange"))
              )
            )
          )
        )
      )
    },
    class = "arrange_block",
    ...
  )
}
