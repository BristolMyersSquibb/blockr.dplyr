#' Pivot Longer block constructor
#'
#' This block reshapes data from wide to long format by pivoting multiple columns
#' into two columns: one containing the original column names and another containing
#' the values (see [tidyr::pivot_longer()]).
#'
#' @param cols Character vector of column names to pivot into longer format.
#'   If empty, all columns will be available for selection.
#' @param names_to Name of the new column to create from the column names.
#'   Default is "name".
#' @param values_to Name of the new column to create from the values.
#'   Default is "value".
#' @param values_drop_na If TRUE, rows with NA values will be dropped.
#'   Default is FALSE.
#' @param names_prefix Optional prefix to remove from column names before storing
#'   in the names_to column. For example, "col_" would remove that prefix from
#'   column names like "col_a", "col_b".
#' @param ... Additional arguments forwarded to [new_transform_block()]
#'
#' @return A block object for pivot_longer operations
#' @importFrom shiny req showNotification NS moduleServer reactive observeEvent textInput checkboxInput tagList tags HTML div p
#' @importFrom glue glue
#' @importFrom tidyr pivot_longer
#' @seealso [new_transform_block()], [tidyr::pivot_longer()]
#' @examples
#' \dontrun{
#' # Basic usage with wide format data
#' library(blockr.core)
#' wide_data <- data.frame(
#'   id = 1:3,
#'   measurement_a = c(10, 20, 30),
#'   measurement_b = c(15, 25, 35),
#'   measurement_c = c(12, 22, 32)
#' )
#' serve(
#'   new_pivot_longer_block(
#'     cols = c("measurement_a", "measurement_b", "measurement_c"),
#'     names_to = "measurement_type",
#'     values_to = "value"
#'   ),
#'   data = list(data = wide_data)
#' )
#'
#' # With names_prefix to clean column names
#' serve(
#'   new_pivot_longer_block(
#'     cols = c("measurement_a", "measurement_b", "measurement_c"),
#'     names_to = "type",
#'     values_to = "measurement",
#'     names_prefix = "measurement_"
#'   ),
#'   data = list(data = wide_data)
#' )
#' }
#' @export
new_pivot_longer_block <- function(
  cols = character(),
  names_to = "name",
  values_to = "value",
  values_drop_na = FALSE,
  names_prefix = "",
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Column selector using unified component
          r_cols_selection <- mod_by_selector_server(
            id = "cols_selector",
            get_cols = \() colnames(data()),
            initial_value = cols
          )

          # Text inputs for names_to and values_to
          r_names_to <- reactiveVal(names_to)
          r_values_to <- reactiveVal(values_to)
          r_values_drop_na <- reactiveVal(values_drop_na)
          r_names_prefix <- reactiveVal(names_prefix)

          # Update reactive values when inputs change
          observeEvent(input$names_to, {
            r_names_to(input$names_to)
          })

          observeEvent(input$values_to, {
            r_values_to(input$values_to)
          })

          observeEvent(input$values_drop_na, {
            r_values_drop_na(input$values_drop_na)
          }, ignoreNULL = FALSE)

          observeEvent(input$names_prefix, {
            r_names_prefix(input$names_prefix)
          }, ignoreNULL = FALSE)

          list(
            expr = reactive({
              selected_cols <- r_cols_selection()

              # Handle empty selection
              if (length(selected_cols) == 0) {
                # If no columns selected, return data unchanged
                return(parse(text = "data")[[1]])
              }

              # Build column selection with backticks if needed
              cols_str <- paste(backtick_if_needed(selected_cols), collapse = ", ")

              # Build the pivot_longer expression
              args <- list()
              args$cols <- glue("c({cols_str})")
              args$names_to <- glue('"{r_names_to()}"')
              args$values_to <- glue('"{r_values_to()}"')

              # Add optional parameters
              if (isTRUE(r_values_drop_na())) {
                args$values_drop_na <- "TRUE"
              }

              if (nzchar(r_names_prefix())) {
                args$names_prefix <- glue('"{r_names_prefix()}"')
              }

              # Build argument string
              args_str <- paste(
                names(args),
                "=",
                unlist(args),
                collapse = ", "
              )

              text <- glue("tidyr::pivot_longer(data, {args_str})")
              parse(text = as.character(text))[[1]]
            }),
            state = list(
              cols = r_cols_selection,
              names_to = r_names_to,
              values_to = r_values_to,
              values_drop_na = r_values_drop_na,
              names_prefix = r_names_prefix
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # Add CSS
        css_responsive_grid(),
        css_advanced_toggle(NS(id, "advanced-options"), use_subgrid = TRUE),
        css_doc_links(),

        # Block-specific CSS
        tags$style(HTML(
          "
          .pivot_longer-block-container .block-help-text p {
            margin-bottom: 0;
          }
          .pivot_longer-block-container .control-label {
            font-size: 0.875rem;
            color: #666;
            margin-bottom: 4px;
            font-weight: normal;
          }
          /* Checkbox styling - smaller font and bottom alignment */
          .pivot_longer-block-container .block-input-wrapper:has(input[type='checkbox']) {
            align-self: flex-end;
          }
          .pivot_longer-block-container input[type='checkbox'] + span {
            font-size: 0.8rem;
          }
          .pivot_longer-block-container .checkbox label {
            font-size: 0.8rem;
          }
          /* Advanced toggle spans full width */
          .pivot_longer-block-container .block-advanced-toggle {
            grid-column: 1 / -1;
          }
          "
        )),

        div(
          class = "block-container pivot_longer-block-container",

          div(
            class = "block-form-grid",

            # Help text
            div(
              class = "block-help-text",
              p(
                "Reshape data from wide to long format. Select columns to pivot into rows. ",
                tags$a(
                  href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html",
                  target = "_blank",
                  style = "text-decoration: none; font-size: 0.9em;",
                  "\u2197"
                )
              )
            ),

            # Main Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Columns to pivot
                div(
                  class = "block-input-wrapper",
                  mod_by_selector_ui(
                    NS(id, "cols_selector"),
                    label = "Columns to pivot",
                    initial_choices = cols,
                    initial_selected = cols
                  )
                ),

                # Names to
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "names_to"),
                    label = "New name column",
                    value = names_to,
                    placeholder = "name"
                  )
                ),

                # Values to
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "values_to"),
                    label = "New value column",
                    value = values_to,
                    placeholder = "value"
                  )
                )
              )
            ),

            # Toggle button for advanced options
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

            # Advanced options section (collapsible)
            div(
              id = NS(id, "advanced-options"),
              div(
                class = "block-section",
                div(
                  class = "block-section-grid",

                  # Names prefix
                  div(
                    class = "block-input-wrapper",
                    textInput(
                      NS(id, "names_prefix"),
                      label = "Remove prefix from names",
                      value = names_prefix,
                      placeholder = "e.g., 'col_'"
                    )
                  ),

                  # Drop NAs
                  div(
                    class = "block-input-wrapper",
                    checkboxInput(
                      NS(id, "values_drop_na"),
                      label = "Drop rows with NA values",
                      value = values_drop_na
                    )
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "pivot_longer_block",
    allow_empty_state = c("cols", "names_prefix"),
    ...
  )
}
