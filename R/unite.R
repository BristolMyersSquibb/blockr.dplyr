#' Unite block constructor
#'
#' This block combines multiple columns into a single column by pasting their
#' values together (see [tidyr::unite()]). This is useful for creating composite
#' identifiers or labels from multiple fields.
#'
#' @param col Name for the new united column. Default is "united".
#' @param cols Character vector of column names to unite together.
#'   If empty (default), all columns will be available for selection.
#' @param sep Separator to use between values. Default is "_".
#' @param remove If TRUE (default), remove input columns from output data frame.
#' @param na.rm If TRUE, missing values will be removed prior to uniting each row.
#'   Default is FALSE.
#' @param ... Additional arguments forwarded to [new_transform_block()]
#'
#' @return A block object for unite operations
#' @importFrom shiny req showNotification NS moduleServer reactive observeEvent textInput checkboxInput tagList tags HTML div p
#' @importFrom glue glue
#' @importFrom tidyr unite
#' @seealso [new_transform_block()], [tidyr::unite()]
#' @examples
#' # Create a unite block
#' new_unite_block()
#'
#' if (interactive()) {
#'   # Basic usage - combine first and last name
#'   library(blockr.core)
#'   people_data <- data.frame(
#'     first_name = c("John", "Jane", "Bob"),
#'     last_name = c("Doe", "Smith", "Johnson"),
#'     age = c(30, 25, 35)
#'   )
#'   serve(
#'     new_unite_block(
#'       col = "full_name",
#'       cols = c("first_name", "last_name"),
#'       sep = " "
#'     ),
#'     data = list(data = people_data)
#'   )
#'
#'   # With custom separator
#'   serve(
#'     new_unite_block(
#'       col = "id",
#'       cols = c("first_name", "last_name"),
#'       sep = "-",
#'       remove = TRUE
#'     ),
#'     data = list(data = people_data)
#'   )
#'
#'   # With NA removal
#'   data_with_na <- data.frame(
#'     prefix = c("Dr.", NA, "Prof."),
#'     first = c("John", "Jane", "Bob"),
#'     last = c("Doe", "Smith", "Johnson")
#'   )
#'   serve(
#'     new_unite_block(
#'       col = "full_name",
#'       cols = c("prefix", "first", "last"),
#'       sep = " ",
#'       na.rm = TRUE
#'     ),
#'     data = list(data = data_with_na)
#'   )
#' }
#' @export
new_unite_block <- function(
  col = "united",
  cols = character(),
  sep = "_",
  remove = TRUE,
  na.rm = FALSE,
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Column selector for columns to unite
          r_cols_selection <- mod_column_selector_server(
            id = "cols_selector",
            get_cols = \() colnames(data()),
            initial_value = cols
          )

          # Reactive values for other parameters
          r_col <- reactiveVal(col)
          r_sep <- reactiveVal(sep)
          r_remove <- reactiveVal(remove)
          r_na_rm <- reactiveVal(na.rm)

          # Update reactive values when inputs change
          observeEvent(input$col, {
            r_col(input$col)
          })

          observeEvent(input$sep, {
            r_sep(input$sep)
          })

          observeEvent(input$remove, {
            r_remove(input$remove)
          })

          observeEvent(input$na_rm, {
            r_na_rm(input$na_rm)
          })

          list(
            expr = reactive({
              selected_cols <- r_cols_selection()
              new_col_name <- r_col()

              # Require at least 2 columns to unite and a valid column name
              req(length(selected_cols) >= 2)
              req(nzchar(new_col_name))

              # Build column selection with backticks if needed
              cols_str <- paste(
                backtick_if_needed(selected_cols),
                collapse = ", "
              )

              # Build the unite expression
              # unite(data, col, ..., sep, remove, na.rm)
              # New column name (needs backticks if non-syntactic)
              col_arg <- backtick_if_needed(new_col_name)

              # Build optional parameters
              optional_args <- c()

              # sep parameter
              if (r_sep() != "_") {
                optional_args <- c(optional_args, glue('sep = "{r_sep()}"'))
              }

              # remove parameter
              if (!isTRUE(r_remove())) {
                optional_args <- c(optional_args, "remove = FALSE")
              }

              # na.rm parameter
              if (isTRUE(r_na_rm())) {
                optional_args <- c(optional_args, "na.rm = TRUE")
              }

              # Combine all parts
              if (length(optional_args) > 0) {
                args_str <- glue("{col_arg}, {cols_str}, {paste(optional_args, collapse = ', ')}")
              } else {
                args_str <- glue("{col_arg}, {cols_str}")
              }

              text <- glue("tidyr::unite(data, {args_str})")
              parse(text = as.character(text))[[1]]
            }),
            state = list(
              col = r_col,
              cols = r_cols_selection,
              sep = r_sep,
              remove = r_remove,
              na.rm = r_na_rm
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

        # Block-specific CSS
        tags$style(HTML(
          "
          /* Checkbox styling - smaller font and bottom alignment */
          .unite-block-container .block-input-wrapper:has(input[type='checkbox']) {
            align-self: flex-end;
          }
          .unite-block-container input[type='checkbox'] + span {
            font-size: 0.8rem;
          }
          .unite-block-container .checkbox label {
            font-size: 0.8rem;
          }
          /* Advanced toggle spans full width */
          .unite-block-container .block-advanced-toggle {
            grid-column: 1 / -1;
          }
          "
        )),

        div(
          class = "block-container unite-block-container",

          div(
            class = "block-form-grid",

            # Main Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Columns to unite
                div(
                  class = "block-input-wrapper",
                  mod_column_selector_ui(
                    NS(id, "cols_selector"),
                    label = "Columns to unite (select 2+)",
                    initial_choices = cols,
                    initial_selected = cols,
                    width = "100%"
                  )
                ),

                # New column name
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "col"),
                    label = "New column name",
                    value = col,
                    placeholder = "united",
                    width = "100%"
                  )
                ),

                # Separator
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "sep"),
                    label = "Separator",
                    value = sep,
                    placeholder = "_",
                    width = "100%"
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

                  # Remove input columns
                  div(
                    class = "block-input-wrapper",
                    checkboxInput(
                      NS(id, "remove"),
                      label = "Remove input columns",
                      value = remove
                    )
                  ),

                  # Remove NAs
                  div(
                    class = "block-input-wrapper",
                    checkboxInput(
                      NS(id, "na_rm"),
                      label = "Remove NA values before uniting",
                      value = na.rm
                    )
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "unite_block",
    allow_empty_state = c("cols"),
    ...
  )
}
