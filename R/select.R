#' Select block constructor
#'
#' This block allows performing column subsetting on `data.frame` objects (see
#' [dplyr::select()]). Columns can be selected and reordered by dragging, and
#' an exclude mode allows for negative selection using dplyr's minus syntax.
#'
#' @param columns Selected columns (character vector)
#' @param exclude Logical. If TRUE, uses exclude mode (dplyr minus syntax: `-c(col1, col2)`).
#'   If FALSE (default), uses include mode (selects specified columns).
#' @param ... Forwarded to [new_transform_block()]
#'
#' @details
#' The select block provides a sortable multi-select interface where columns can be:
#' - Selected/deselected by clicking
#' - Reordered by dragging (order is preserved in output)
#' - Removed individually using the × button
#'
#' **Include mode (exclude = FALSE, default):**
#' - Selected columns are included in output
#' - Empty selection = select nothing (`select(data, -everything())`)
#'
#' **Exclude mode (exclude = TRUE):**
#' - Selected columns are excluded from output using minus syntax
#' - Empty selection = select all (`select(data)`)
#' - Efficient for large datasets when you want to remove just a few columns
#'
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_select_block(), list(data = mtcars))
#'
#' # With initial column selection
#' serve(new_select_block(columns = c("mpg", "cyl", "hp")), list(data = mtcars))
#'
#' # Exclude mode (select all except specified columns)
#' serve(new_select_block(columns = c("gear", "carb"), exclude = TRUE), list(data = mtcars))
#'
#' # Connected blocks example
#' serve(
#'   new_board(
#'     blocks = list(
#'       a = new_dataset_block(),
#'       b = new_select_block()
#'     ),
#'     links = links(
#'       from = c("a"),
#'       to = c("b")
#'     )
#'   )
#' )
#' }
#' @export
new_select_block <- function(
  columns = character(),
  exclude = FALSE,
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_columns <- reactiveVal(columns)
          r_exclude <- reactiveVal(exclude)
          r_initialized <- reactiveVal(FALSE)

          # Update reactive values when inputs change
          observeEvent(input$columns, {
            r_columns(input$columns)
          }, ignoreNULL = FALSE)

          observeEvent(input$exclude, {
            r_exclude(input$exclude)
          }, ignoreNULL = FALSE)

          # Restore initial selection once on startup (like slice block)
          observe({
            if (!r_initialized() && length(colnames(data())) > 0) {
              updateSelectizeInput(
                session,
                "columns",
                choices = colnames(data()),
                selected = r_columns()
              )
              r_initialized(TRUE)
            }
          })

          # Update choices when data changes, preserve current selection
          observeEvent(colnames(data()), {
            if (r_initialized()) {
              req(data())
              cols <- colnames(data())

              updateSelectizeInput(
                session,
                "columns",
                choices = cols,
                selected = r_columns()  # Preserve current selection
              )
            }
          }, ignoreNULL = FALSE)

          list(
            expr = reactive({
              cols <- r_columns()
              is_exclude <- isTRUE(r_exclude())

              if (length(cols) == 0) {
                if (is_exclude) {
                  # Exclude nothing = select all
                  parse(text = "dplyr::select(data)")[[1]]
                } else {
                  # Include nothing = select nothing
                  parse(text = "dplyr::select(data, -dplyr::everything())")[[1]]
                }
              } else {
                # Build column names with backticks if needed
                cols_str <- paste(
                  backtick_if_needed(cols),
                  collapse = ", "
                )

                if (is_exclude) {
                  # Exclude mode: use minus syntax
                  parse(text = glue::glue("dplyr::select(data, -c({cols_str}))"))[[1]]
                } else {
                  # Include mode: regular select
                  parse(text = glue::glue("dplyr::select(data, {cols_str})"))[[1]]
                }
              }
            }),
            state = list(
              columns = r_columns,
              exclude = r_exclude
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # Add responsive CSS
        block_responsive_css(),

        # Force single column layout like filter/value_filter blocks
        tags$style(HTML(
          "
          .select-block-container .block-form-grid {
            grid-template-columns: 1fr !important;
          }
          .select-block-container .block-help-text {
            margin-bottom: 0;
            margin-top: -8px;
          }
          .select-block-container .block-help-text p {
            margin-bottom: 0;
          }
          "
        )),

        div(
          class = "block-container select-block-container",
          div(
            class = "block-form-grid",

            # Select Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  p(
                    "Select and reorder columns. Drag to change order in output."
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(
                    NS(id, "columns"),
                    label = NULL,  # No label
                    choices = columns,       # Initialize with constructor parameter
                    selected = columns,      # Pre-select constructor parameter
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                      plugins = list("drag_drop", "remove_button"),
                      persist = FALSE,
                      placeholder = "Select columns..."
                    )
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    NS(id, "exclude"),
                    label = "Exclude selected columns",
                    value = exclude
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "select_block",
    ...
  )
}
