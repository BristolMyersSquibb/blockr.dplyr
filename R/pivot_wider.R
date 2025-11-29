#' Pivot Wider block constructor
#'
#' This block reshapes data from long to wide format by pivoting column values
#' into new columns (see [tidyr::pivot_wider()]). This is the inverse operation
#' of pivot_longer.
#'
#' @param names_from Character vector specifying which column(s) to use for new
#'   column names. Can be a single column or multiple columns.
#' @param values_from Character vector specifying which column(s) to use for cell
#'   values. Can be a single column or multiple columns.
#' @param id_cols Character vector of columns that uniquely identify each row.
#'   If empty (default), uses all columns not specified in names_from or values_from.
#' @param values_fill Optional value to use for missing combinations. Can be a
#'   single value like "0" or "NA". Leave empty to keep missing values as NA.
#' @param names_sep Separator to use when names_from specifies multiple columns.
#'   Default is "_".
#' @param names_prefix Optional prefix to add to all new column names.
#' @param ... Additional arguments forwarded to [new_transform_block()]
#'
#' @return A block object for pivot_wider operations
#' @importFrom shiny req showNotification NS moduleServer reactive observeEvent textInput selectInput tagList tags HTML div p
#' @importFrom glue glue
#' @importFrom tidyr pivot_wider
#' @seealso [new_transform_block()], [tidyr::pivot_wider()]
#' @examples
#' # Create a pivot wider block
#' new_pivot_wider_block()
#'
#' if (interactive()) {
#'   # Basic usage with long format data
#'   library(blockr.core)
#'   long_data <- data.frame(
#'     id = rep(1:3, each = 3),
#'     measurement_type = rep(c("a", "b", "c"), 3),
#'     value = c(10, 15, 12, 20, 25, 22, 30, 35, 32)
#'   )
#'   serve(
#'     new_pivot_wider_block(
#'       names_from = "measurement_type",
#'       values_from = "value"
#'     ),
#'     data = list(data = long_data)
#'   )
#'
#'   # With values_fill to replace NAs
#'   serve(
#'     new_pivot_wider_block(
#'       names_from = "measurement_type",
#'       values_from = "value",
#'       values_fill = "0"
#'     ),
#'     data = list(data = long_data)
#'   )
#'
#'   # With custom names_prefix
#'   serve(
#'     new_pivot_wider_block(
#'       names_from = "measurement_type",
#'       values_from = "value",
#'       names_prefix = "measure_"
#'     ),
#'     data = list(data = long_data)
#'   )
#' }
#' @export
new_pivot_wider_block <- function(
  names_from = character(),
  values_from = character(),
  id_cols = character(),
  values_fill = "",
  names_sep = "_",
  names_prefix = "",
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Column selectors
          r_names_from <- mod_by_selector_server(
            id = "names_from_selector",
            get_cols = \() colnames(data()),
            initial_value = names_from
          )

          r_values_from <- mod_by_selector_server(
            id = "values_from_selector",
            get_cols = \() colnames(data()),
            initial_value = values_from
          )

          r_id_cols <- mod_by_selector_server(
            id = "id_cols_selector",
            get_cols = \() colnames(data()),
            initial_value = id_cols
          )

          # Text inputs
          r_values_fill <- reactiveVal(values_fill)
          r_names_sep <- reactiveVal(names_sep)
          r_names_prefix <- reactiveVal(names_prefix)

          # Update reactive values when inputs change
          # Note: Removed ignoreNULL = FALSE to prevent overwriting initial values
          # in testServer context where inputs may not be initialized
          observeEvent(input$values_fill, {
            r_values_fill(input$values_fill)
          })

          observeEvent(input$names_sep, {
            r_names_sep(input$names_sep)
          })

          observeEvent(input$names_prefix, {
            r_names_prefix(input$names_prefix)
          })

          list(
            expr = reactive({
              names_from_cols <- r_names_from()
              values_from_cols <- r_values_from()

              # If parameters not yet configured, return data unchanged (pass-through)
              if (
                length(names_from_cols) == 0 || length(values_from_cols) == 0
              ) {
                # Empty parameters are a valid initial state, not an error
                return(parse(text = "identity(data)")[[1]])
              }

              # Build column selection with backticks if needed
              names_from_str <- if (length(names_from_cols) == 1) {
                backtick_if_needed(names_from_cols)
              } else {
                paste0(
                  "c(",
                  paste(backtick_if_needed(names_from_cols), collapse = ", "),
                  ")"
                )
              }

              values_from_str <- if (length(values_from_cols) == 1) {
                backtick_if_needed(values_from_cols)
              } else {
                paste0(
                  "c(",
                  paste(backtick_if_needed(values_from_cols), collapse = ", "),
                  ")"
                )
              }

              # Build the pivot_wider expression
              args <- list()
              args$names_from <- names_from_str
              args$values_from <- values_from_str

              # Add optional parameters
              id_cols_selected <- r_id_cols()
              if (length(id_cols_selected) > 0) {
                id_cols_str <- paste0(
                  "c(",
                  paste(backtick_if_needed(id_cols_selected), collapse = ", "),
                  ")"
                )
                args$id_cols <- id_cols_str
              }

              if (nzchar(r_values_fill())) {
                # Try to parse as numeric, otherwise treat as character
                fill_val <- r_values_fill()
                if (fill_val == "NA") {
                  args$values_fill <- "NA"
                } else if (fill_val == "NULL") {
                  args$values_fill <- "NULL"
                } else if (!is.na(suppressWarnings(as.numeric(fill_val)))) {
                  args$values_fill <- fill_val
                } else {
                  args$values_fill <- glue('"{fill_val}"')
                }
              }

              if (nzchar(r_names_sep()) && r_names_sep() != "_") {
                args$names_sep <- glue('"{r_names_sep()}"')
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

              text <- glue("tidyr::pivot_wider(data, {args_str})")
              parse(text = as.character(text))[[1]]
            }),
            state = list(
              names_from = r_names_from,
              values_from = r_values_from,
              id_cols = r_id_cols,
              values_fill = r_values_fill,
              names_sep = r_names_sep,
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

        # Block-specific CSS (layout only)
        tags$style(HTML(
          "
          /* Advanced toggle spans full width */
          .pivot_wider-block-container .block-advanced-toggle {
            grid-column: 1 / -1;
          }
          "
        )),

        div(
          class = "block-container pivot_wider-block-container",

          div(
            class = "block-form-grid",

            # Main Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Names from
                div(
                  class = "block-input-wrapper",
                  mod_by_selector_ui(
                    NS(id, "names_from_selector"),
                    label = "Get new column names from",
                    initial_choices = names_from,
                    initial_selected = names_from
                  )
                ),

                # Values from
                div(
                  class = "block-input-wrapper",
                  mod_by_selector_ui(
                    NS(id, "values_from_selector"),
                    label = "Get values from",
                    initial_choices = values_from,
                    initial_selected = values_from
                  )
                ),

                # ID columns
                div(
                  class = "block-input-wrapper",
                  mod_by_selector_ui(
                    NS(id, "id_cols_selector"),
                    label = "ID columns (optional)",
                    initial_choices = id_cols,
                    initial_selected = id_cols
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

                  # Values fill
                  div(
                    class = "block-input-wrapper",
                    textInput(
                      NS(id, "values_fill"),
                      label = "Fill missing values with",
                      value = values_fill,
                      placeholder = "e.g., 0 or NA"
                    )
                  ),

                  # Names prefix
                  div(
                    class = "block-input-wrapper",
                    textInput(
                      NS(id, "names_prefix"),
                      label = "Add prefix to column names",
                      value = names_prefix,
                      placeholder = "e.g., 'col_'"
                    )
                  ),

                  # Names separator
                  div(
                    class = "block-input-wrapper",
                    textInput(
                      NS(id, "names_sep"),
                      label = "Separator for multiple names",
                      value = names_sep,
                      placeholder = "_"
                    )
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "pivot_wider_block",
    allow_empty_state = c("names_from", "values_from", "id_cols", "values_fill", "names_prefix"),
    ...
  )
}
