#' Separate block constructor
#'
#' This block separates a single character column into multiple columns by
#' splitting on a separator pattern (see [tidyr::separate()]). This is the
#' inverse operation of unite.
#'
#' @param col Character string specifying which column to separate.
#'   If empty (default), all columns will be available for selection.
#' @param into Character vector of names for the new columns. Can be specified
#'   as a character vector or a comma-separated string (e.g., "col1, col2, col3").
#'   Default is c("col1", "col2").
#' @param sep Separator between columns. Can be a regular expression or numeric
#'   positions. Default is `"[^[:alnum:]]+"` (any non-alphanumeric character).
#' @param remove If TRUE (default), remove input column from output data frame.
#' @param convert If TRUE, will run type.convert() with as.is = TRUE on new
#'   columns. Default is FALSE.
#' @param extra How to handle extra pieces when there are too many:
#'   "warn" (default), "drop", or "merge".
#' @param fill How to handle missing pieces when there are too few:
#'   "warn" (default), "right", or "left".
#' @param ... Additional arguments forwarded to [new_transform_block()]
#'
#' @return A block object for separate operations
#' @importFrom shiny req showNotification NS moduleServer reactive observeEvent textInput checkboxInput selectInput tagList tags HTML div p
#' @importFrom glue glue
#' @importFrom tidyr separate
#' @seealso [new_transform_block()], [tidyr::separate()]
#' @examples
#' # Create a separate block
#' new_separate_block()
#'
#' if (interactive()) {
#'   # Basic usage - separate full name into first and last
#'   library(blockr.core)
#'   people_data <- data.frame(
#'     full_name = c("John Doe", "Jane Smith", "Bob Johnson"),
#'     age = c(30, 25, 35)
#'   )
#'   serve(
#'     new_separate_block(
#'       col = "full_name",
#'       into = c("first_name", "last_name"),
#'       sep = " "
#'     ),
#'     data = list(data = people_data)
#'   )
#'
#'   # Separate date components
#'   date_data <- data.frame(
#'     date_string = c("2024-01-15", "2024-02-20", "2024-03-25")
#'   )
#'   serve(
#'     new_separate_block(
#'       col = "date_string",
#'       into = c("year", "month", "day"),
#'       sep = "-",
#'       convert = TRUE
#'     ),
#'     data = list(data = date_data)
#'   )
#'
#'   # Using regex separator
#'   mixed_data <- data.frame(
#'     mixed_col = c("a-b", "c_d", "e.f")
#'   )
#'   serve(
#'     new_separate_block(
#'       col = "mixed_col",
#'       into = c("col1", "col2"),
#'       sep = "[-_.]"
#'     ),
#'     data = list(data = mixed_data)
#'   )
#' }
#' @export
new_separate_block <- function(
  col = character(),
  into = c("col1", "col2"),
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Single column selector (as_rv supports external_ctrl injection)
          r_col <- as_rv(col)

          # Update column selection
          observeEvent(input$col, {
            r_col(input$col)
          })

          # Update choices when data changes
          observeEvent(colnames(data()), {
            updateSelectInput(
              session,
              inputId = "col",
              choices = colnames(data()),
              selected = r_col()
            )
          })

          # Reactive values (as_rv supports external_ctrl injection)
          r_into <- as_rv(into)
          r_sep <- as_rv(sep)
          r_remove <- as_rv(remove)
          r_convert <- as_rv(convert)
          r_extra <- as_rv(extra)
          r_fill <- as_rv(fill)

          # Update reactive values when inputs change
          observeEvent(input$into, {
            r_into(input$into)
          })

          observeEvent(input$sep, {
            r_sep(input$sep)
          })

          observeEvent(input$remove, {
            r_remove(input$remove)
          })

          observeEvent(input$convert, {
            r_convert(input$convert)
          })

          observeEvent(input$extra, {
            r_extra(input$extra)
          })

          observeEvent(input$fill, {
            r_fill(input$fill)
          })

          # Reverse sync: external_ctrl -> UI
          observeEvent(r_col(), {
            if (!identical(input$col, r_col())) {
              updateSelectInput(session, "col", selected = r_col())
            }
          }, ignoreInit = TRUE)

          observeEvent(r_into(), {
            new_val <- if (is.character(r_into())) {
              paste(r_into(), collapse = ", ")
            } else {
              r_into()
            }
            if (!identical(input$into, new_val)) {
              updateTextInput(session, "into", value = new_val)
            }
          }, ignoreInit = TRUE)

          observeEvent(r_sep(), {
            if (!identical(input$sep, r_sep())) {
              updateTextInput(session, "sep", value = r_sep())
            }
          }, ignoreInit = TRUE)

          observeEvent(r_remove(), {
            if (!identical(input$remove, r_remove())) {
              shiny::updateCheckboxInput(session, "remove", value = r_remove())
            }
          }, ignoreInit = TRUE)

          observeEvent(r_convert(), {
            if (!identical(input$convert, r_convert())) {
              shiny::updateCheckboxInput(session, "convert", value = r_convert())
            }
          }, ignoreInit = TRUE)

          observeEvent(r_extra(), {
            if (!identical(input$extra, r_extra())) {
              shiny::updateSelectInput(session, "extra", selected = r_extra())
            }
          }, ignoreInit = TRUE)

          observeEvent(r_fill(), {
            if (!identical(input$fill, r_fill())) {
              shiny::updateSelectInput(session, "fill", selected = r_fill())
            }
          }, ignoreInit = TRUE)

          list(
            expr = reactive({
              col_to_sep <- r_col()
              into_value <- r_into()

              # Require column selection and into names
              req(length(col_to_sep) > 0)
              req(length(into_value) > 0)

              # Parse into parameter (handle both character vector and comma-separated string)
              if (is.character(into_value) && length(into_value) == 1) {
                # If it's a single string, try to split by comma
                if (grepl(",", into_value)) {
                  into_names <- trimws(strsplit(into_value, ",")[[1]])
                } else {
                  into_names <- into_value
                }
              } else {
                into_names <- into_value
              }

              req(length(into_names) >= 1)

              # Build column reference with backticks if needed
              col_str <- backtick_if_needed(col_to_sep)

              # Build into parameter with backticks if needed
              into_str <- paste0(
                "c(",
                paste(
                  sprintf('"%s"', into_names),
                  collapse = ", "
                ),
                ")"
              )

              # Build the separate expression
              args <- list()
              args$col <- col_str
              args$into <- into_str
              args$sep <- glue('"{r_sep()}"')

              # Add optional parameters if they differ from defaults
              if (!isTRUE(r_remove())) {
                args$remove <- "FALSE"
              }

              if (isTRUE(r_convert())) {
                args$convert <- "TRUE"
              }

              if (r_extra() != "warn") {
                args$extra <- glue('"{r_extra()}"')
              }

              if (r_fill() != "warn") {
                args$fill <- glue('"{r_fill()}"')
              }

              # Build argument string
              args_str <- paste(
                names(args),
                "=",
                unlist(args),
                collapse = ", "
              )

              text <- glue("tidyr::separate(data, {args_str})")
              parse(text = as.character(text))[[1]]
            }),
            state = list(
              col = r_col,
              into = r_into,
              sep = r_sep,
              remove = r_remove,
              convert = r_convert,
              extra = r_extra,
              fill = r_fill
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
          .separate-block-container .block-advanced-toggle {
            grid-column: 1 / -1;
          }
          "
        )),

        div(
          class = "block-container separate-block-container",

          div(
            class = "block-form-grid",

            # Main Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Column to separate
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "col"),
                    label = "Column to separate",
                    choices = col,
                    selected = col,
                    width = "100%"
                  )
                ),

                # New column names
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "into"),
                    label = "New column names (comma-separated)",
                    value = paste(into, collapse = ", "),
                    placeholder = "col1, col2",
                    width = "100%"
                  )
                ),

                # Separator
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "sep"),
                    label = "Separator (regex or character)",
                    value = sep,
                    placeholder = "[^[:alnum:]]+",
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

                  # Remove input column
                  div(
                    class = "block-input-wrapper",
                    checkboxInput(
                      NS(id, "remove"),
                      label = "Remove input column",
                      value = remove
                    )
                  ),

                  # Convert types
                  div(
                    class = "block-input-wrapper",
                    checkboxInput(
                      NS(id, "convert"),
                      label = "Auto-convert column types",
                      value = convert
                    )
                  ),

                  # Extra pieces handling
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      NS(id, "extra"),
                      label = "Handle extra pieces",
                      choices = c("warn", "drop", "merge"),
                      selected = extra,
                      width = "100%"
                    )
                  ),

                  # Missing pieces handling
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      NS(id, "fill"),
                      label = "Handle missing pieces",
                      choices = c("warn", "right", "left"),
                      selected = fill,
                      width = "100%"
                    )
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "separate_block",
    external_ctrl = TRUE,
    allow_empty_state = c("col"),
    ...
  )
}
