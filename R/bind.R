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
#' @param id_name Character string, name for the ID column. If non-empty, adds
#'   a column identifying source data frames. Default "" (disabled).
#' @param ... Forwarded to [new_block()]
#'
#' @return A block object for bind_rows operations
#'
#' @examples
#' \dontrun{
#' library(blockr.core)
#' library(blockr.dplyr)
#'
#' # Basic usage - stack filtered datasets
#' serve(
#'   new_board(
#'     blocks = list(
#'       iris_data = new_dataset_block(dataset = "iris"),
#'       setosa = new_filter_expr_block(exprs = list("Species == 'setosa'")),
#'       versicolor = new_filter_expr_block(exprs = list("Species == 'versicolor'")),
#'       combined = new_bind_rows_block()
#'     ),
#'     links = links(
#'       from = c("iris_data", "iris_data", "setosa", "versicolor"),
#'       to = c("setosa", "versicolor", "combined", "combined"),
#'       input = c("data", "data", "1", "2")
#'     )
#'   )
#' )
#'
#' # With ID column to track source
#' serve(
#'   new_board(
#'     blocks = list(
#'       iris_data = new_dataset_block(dataset = "iris"),
#'       setosa = new_filter_expr_block(exprs = list("Species == 'setosa'")),
#'       versicolor = new_filter_expr_block(exprs = list("Species == 'versicolor'")),
#'       combined = new_bind_rows_block(id_name = "source")
#'     ),
#'     links = links(
#'       from = c("iris_data", "iris_data", "setosa", "versicolor"),
#'       to = c("setosa", "versicolor", "combined", "combined"),
#'       input = c("data", "data", "1", "2")
#'     )
#'   )
#' )
#' }
#'
#' @export
new_bind_rows_block <- function(id_name = "", ...) {
  new_transform_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          arg_names <- reactive(
            set_names(names(...args), dot_args_names(...args))
          )

          # Reactive value for id_name
          r_id_name <- reactiveVal(id_name)

          # Debounced version of input (800ms delay)
          id_name_debounced <- debounce(reactive(input$id_name), 800)

          # Update reactive value from debounced input
          observeEvent(id_name_debounced(), {
            r_id_name(id_name_debounced() %||% "")
          })

          list(
            expr = reactive({
              current_id_name <- r_id_name()

              # Build base expression with bquote
              base_expr <- bquote(
                dplyr::bind_rows(..(dat)),
                list(dat = lapply(arg_names(), as.name)),
                splice = TRUE
              )

              # If id_name is provided and non-empty, add .id parameter using call()
              if (length(current_id_name) > 0 && nzchar(current_id_name)) {
                # Modify the call to add .id parameter
                base_expr[[".id"]] <- current_id_name
                base_expr
              } else {
                base_expr
              }
            }),
            state = list(
              id_name = r_id_name
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        # Add CSS
        css_responsive_grid(),
        css_advanced_toggle(NS(id, "advanced-options")),

        # Block section with help text
        div(
          class = "block-section",
          div(
            class = "block-section-grid",
            div(
              class = "block-help-text",
              p(
                "Stack datasets vertically. Columns are matched by name. ",
                tags$a(
                  href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#bind-rows-block",
                  target = "_blank",
                  style = "text-decoration: none; font-size: 0.9em;",
                  "\u2197"
                )
              )
            )
          )
        ),

        # Toggle button
        div(
          class = "block-advanced-toggle text-muted",
          id = NS(id, "advanced-toggle"),
          onclick = sprintf(
            "
            const section = document.getElementById('%s');
            const chevron = document.querySelector('#%s .block-chevron');
            section.classList.toggle('expanded');
            chevron.classList.toggle('rotated');
          ",
            NS(id, "advanced-options"),
            NS(id, "advanced-toggle")
          ),
          tags$span(class = "block-chevron", "\u203A"),
          "Show advanced options"
        ),

        # Advanced Options Section (Collapsible)
        div(
          id = NS(id, "advanced-options"),
          div(
            class = "mb-3",
            textInput(
              NS(id, "id_name"),
              label = tags$small(
                class = "text-muted",
                "ID column name (leave empty to disable):"
              ),
              value = id_name,
              placeholder = "e.g., .id or source"
            )
          )
        )
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

#' Bind Columns Block Constructor
#'
#' This block allows for column-wise combination of two or more data frames
#' using [dplyr::bind_cols()]. It combines data frames side-by-side,
#' requiring them to have the same number of rows. Duplicate column names
#' are automatically handled by dplyr.
#'
#' @param ... Forwarded to [new_block()]
#'
#' @return A block object for bind_cols operations
#'
#' @examples
#' \dontrun{
#' library(blockr.core)
#' library(blockr.dplyr)
#'
#' # Basic usage - combine different datasets horizontally
#' serve(
#'   new_board(
#'     blocks = list(
#'       iris_data = new_dataset_block(dataset = "iris"),
#'       mtcars_data = new_dataset_block(dataset = "mtcars"),
#'       head1 = new_slice_block(type = "head", n = 5),
#'       head2 = new_slice_block(type = "head", n = 5),
#'       combined = new_bind_cols_block()
#'     ),
#'     links = links(
#'       from = c("iris_data", "mtcars_data", "head1", "head2"),
#'       to = c("head1", "head2", "combined", "combined"),
#'       input = c("data", "data", "1", "2")
#'     )
#'   )
#' )
#'
#' # Combine selected columns from same dataset
#' serve(
#'   new_board(
#'     blocks = list(
#'       mtcars_data = new_dataset_block(dataset = "mtcars"),
#'       engine_cols = new_select_block(columns = c("mpg", "cyl", "hp")),
#'       weight_cols = new_select_block(columns = c("wt", "qsec")),
#'       combined = new_bind_cols_block()
#'     ),
#'     links = links(
#'       from = c("mtcars_data", "mtcars_data", "engine_cols", "weight_cols"),
#'       to = c("engine_cols", "weight_cols", "combined", "combined"),
#'       input = c("data", "data", "1", "2")
#'     )
#'   )
#' )
#' }
#'
#' @export
new_bind_cols_block <- function(...) {
  new_transform_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          arg_names <- reactive(
            set_names(names(...args), dot_args_names(...args))
          )

          list(
            expr = reactive({
              bquote(
                dplyr::bind_cols(..(dat)),
                list(dat = lapply(arg_names(), as.name)),
                splice = TRUE
              )
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        # Add responsive CSS
        css_responsive_grid(),

        # Block section with help text
        div(
          class = "block-section",
          div(
            class = "block-section-grid",
            div(
              class = "block-help-text",
              p(
                "Combine datasets horizontally. All datasets must have the same number of rows. ",
                tags$a(
                  href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#bind-columns-block",
                  target = "_blank",
                  style = "text-decoration: none; font-size: 0.9em;",
                  "\u2197"
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = c("bind_cols_block", "rbind_block"),
    ...
  )
}
