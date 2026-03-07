#' Arrange block constructor
#'
#' This block allows allows you to order the rows of a data frame by the values
#' of selected columns (see [dplyr::arrange()]).
#'
#' @param columns Columns to arrange by. Can be a character vector (ascending order)
#'   or a list of specifications with column and direction.
#' @param ... Forwarded to [new_block()]
#'
#' @return A transform block object of class `arrange_block`.
#'
#' @examples
#' # Create an arrange block
#' new_arrange_block(columns = "mpg")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dplyr)
#'
#'   # Basic usage - single column ascending
#'   serve(new_arrange_block(columns = "mpg"), list(data = mtcars))
#'
#'   # Multiple columns with custom directions
#'   serve(
#'     new_arrange_block(
#'       columns = list(
#'         list(column = "cyl", direction = "asc"),
#'         list(column = "mpg", direction = "desc")
#'       )
#'     ),
#'     list(data = mtcars)
#'   )
#'
#'   # Connected blocks - sort after categorizing
#'   serve(
#'     new_board(
#'       blocks = list(
#'         data = new_dataset_block(dataset = "mtcars"),
#'         categorized = new_mutate_block(
#'           exprs = list(
#'             car_type = paste0(
#'               "dplyr::case_when(cyl <= 4 ~ 'Economy', ",
#'               "cyl <= 6 ~ 'Standard', TRUE ~ 'Performance')"
#'             )
#'           )
#'         ),
#'         sorted = new_arrange_block(
#'           columns = list(
#'             list(column = "car_type", direction = "asc"),
#'             list(column = "mpg", direction = "desc"),
#'             list(column = "hp", direction = "desc")
#'           )
#'         )
#'       ),
#'       links = links(
#'         from = c("data", "categorized"),
#'         to = c("categorized", "sorted")
#'       )
#'     )
#'   )
#' }
#'
#' @export
new_arrange_block <- function(columns = character(), ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          r_columns <- reactiveVal(columns)
          cols <- reactive(colnames(data()))

          # Compute initial arranges from current value
          init_val <- r_columns()
          initial_arranges <- if (is.character(init_val) && length(init_val) > 0) {
            lapply(init_val, function(col) {
              list(column = col, direction = "asc")
            })
          } else if (is.list(init_val)) {
            init_val
          } else {
            list()
          }

          # Use the multi arrange module
          r_arranges <- mod_multi_arrange_server(
            "multi_arrange",
            get_value = function() initial_arranges,
            get_cols = cols,
            external_value = r_columns
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
              columns = r_columns
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
                mod_multi_arrange_ui(NS(id, "multi_arrange"))
              )
            )
          )
        )
      )
    },
    class = "arrange_block",
    external_ctrl = TRUE,
    allow_empty_state = "columns",
    ...
  )
}
