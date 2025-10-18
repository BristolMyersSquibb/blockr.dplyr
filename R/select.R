#' Select block constructor
#'
#' This block allows performing column subsetting on `data.frame` objects (see
#' [dplyr::select()]). Columns can be selected and reordered by dragging, and
#' an exclude mode allows for negative selection using dplyr's minus syntax.
#' Optionally, distinct rows can be kept after selection.
#'
#' **Note**: This block replaces the deprecated `new_distinct_block()`. Use the
#' `distinct` parameter to get unique rows after column selection.
#'
#' @param columns Selected columns (character vector). If empty, selects all columns.
#' @param exclude Logical. If TRUE, uses exclude mode (dplyr minus syntax: `-c(col1, col2)`).
#'   If FALSE (default), uses include mode (selects specified columns).
#' @param distinct Logical. If TRUE, keeps only distinct/unique rows after selecting columns.
#'   If FALSE (default), returns all rows. This replaces the old `new_distinct_block()` functionality.
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
#' **Distinct mode (distinct = TRUE):**
#' - Keeps only distinct rows after column selection
#' - Equivalent to piping `select()` output to `distinct()`
#' - Useful for finding unique combinations of selected columns
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
#' # Select with distinct (unique combinations)
#' serve(new_select_block(columns = c("cyl", "gear"), distinct = TRUE), list(data = mtcars))
#'
#' # Full deduplication (distinct on all columns)
#' serve(new_select_block(distinct = TRUE), list(data = mtcars))
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
  distinct = FALSE,
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
          r_distinct <- reactiveVal(distinct)
          r_initialized <- reactiveVal(FALSE)

          # Update reactive values when inputs change
          observeEvent(input$columns, {
            r_columns(input$columns)
          }, ignoreNULL = FALSE)

          observeEvent(input$exclude, {
            r_exclude(input$exclude)
          }, ignoreNULL = FALSE)

          observeEvent(input$distinct, {
            r_distinct(input$distinct)
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
              is_distinct <- isTRUE(r_distinct())

              # Special case: no columns + distinct = just distinct()
              if (length(cols) == 0 && is_distinct) {
                return(parse(text = "dplyr::distinct(data)")[[1]])
              }

              # Build select expression
              if (length(cols) == 0) {
                # Empty selection = select all columns (pass-through)
                # Works for both include and exclude mode
                select_expr <- "dplyr::select(data, dplyr::everything())"
              } else {
                # Build column names with backticks if needed
                cols_str <- paste(
                  backtick_if_needed(cols),
                  collapse = ", "
                )

                if (is_exclude) {
                  # Exclude mode: use minus syntax
                  select_expr <- glue::glue("dplyr::select(data, -c({cols_str}))")
                } else {
                  # Include mode: regular select
                  select_expr <- glue::glue("dplyr::select(data, {cols_str})")
                }
              }

              # Chain distinct if checkbox is checked
              if (is_distinct) {
                final_expr <- glue::glue("({select_expr}) |> dplyr::distinct()")
              } else {
                final_expr <- select_expr
              }

              parse(text = as.character(final_expr))[[1]]
            }),
            state = list(
              columns = r_columns,
              exclude = r_exclude,
              distinct = r_distinct
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

          /* Inline layout for columns input + exclude checkbox (match slice block) */
          .select-columns-exclude-wrapper {
            display: flex;
            align-items: flex-end;
            gap: 4px;
            flex-wrap: nowrap;
          }

          .select-columns-exclude-wrapper > div:first-child {
            flex: 1;
            min-width: 0;
          }

          /* Exclude checkbox styling (match slice block proportion checkbox) */
          .select-exclude-checkbox {
            display: flex;
            align-items: center;
            margin-left: 0;
            margin-bottom: 5px;
          }

          .select-exclude-checkbox .shiny-input-container {
            width: auto !important;
            max-width: none !important;
            margin-bottom: 0 !important;
          }

          .select-exclude-checkbox .checkbox {
            margin-bottom: 0;
            margin-top: 0;
          }

          .select-exclude-checkbox label {
            font-size: 0.75rem;
            color: #6c757d;
            font-weight: normal;
            margin-bottom: 0;
            padding-left: 4px;
          }

          .select-exclude-checkbox input[type='checkbox'] {
            margin-top: 0;
            margin-right: 4px;
          }

          /* Distinct checkbox - keep as full-width option below */
          .select-block-container .select-distinct-checkbox {
            margin-top: 8px;
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
                  div(
                    class = "select-columns-exclude-wrapper",
                    div(
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
                      class = "select-exclude-checkbox",
                      checkboxInput(
                        NS(id, "exclude"),
                        label = "Exclude",
                        value = exclude
                      )
                    )
                  )
                ),
                div(
                  class = "block-input-wrapper select-distinct-checkbox",
                  checkboxInput(
                    NS(id, "distinct"),
                    label = "Keep distinct rows only",
                    value = distinct
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "select_block",
    allow_empty_state = "columns",
    ...
  )
}
