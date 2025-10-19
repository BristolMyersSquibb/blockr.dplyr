#' Slice block constructor
#'
#' This block allows row selection using various dplyr slice functions
#' (see [dplyr::slice()], [dplyr::slice_head()], [dplyr::slice_tail()],
#' [dplyr::slice_min()], [dplyr::slice_max()], [dplyr::slice_sample()]).
#' Features reactive UI with immediate updates and comprehensive grouping support.
#'
#' @param type Character string specifying slice type: "head", "tail", "min", "max", "sample", or "custom"
#' @param n Number of rows to select (integer)
#' @param prop Proportion of rows to select (0 to 1)
#' @param order_by Column name to order by (for slice_min/slice_max)
#' @param with_ties Logical, whether to include ties (for slice_min/slice_max)
#' @param weight_by Column name for weighted sampling (for slice_sample)
#' @param replace Logical, whether to sample with replacement (for slice_sample)
#' @param rows Custom row positions (for slice)
#' @param by Character vector of column names for grouping
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for slice operations
#' @importFrom shiny NS moduleServer reactive req div conditionalPanel radioButtons numericInput selectInput checkboxInput textInput observeEvent updateSelectInput
#' @importFrom dplyr slice slice_head slice_tail slice_min slice_max slice_sample
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage
#' library(blockr.core)
#' serve(new_slice_block(), list(data = mtcars))
#'
#' # Select first 5 rows
#' serve(new_slice_block(type = "head", n = 5), list(data = mtcars))
#'
#' # Select rows with highest mpg values
#' serve(new_slice_block(type = "max", order_by = "mpg", n = 3), list(data = mtcars))
#'
#' # Random sampling
#' serve(new_slice_block(type = "sample", n = 10, replace = FALSE), list(data = mtcars))
#' }
#' @export
new_slice_block <- function(
  type = "head",
  n = 5,
  prop = 0.1,
  order_by = character(),
  with_ties = TRUE,
  weight_by = character(),
  replace = FALSE,
  rows = "1:5",
  by = character(),
  ...
) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize state reactives for all parameters
          r_type <- reactiveVal(type)
          r_n <- reactiveVal(n)
          r_prop <- reactiveVal(prop)
          r_order_by <- reactiveVal(order_by)
          r_with_ties <- reactiveVal(with_ties)
          r_weight_by <- reactiveVal(weight_by)
          r_replace <- reactiveVal(replace)
          r_rows <- reactiveVal(rows)

          # Group by selector using unified componen
          r_by_selection <- mod_by_selector_server(
            id = "by_selector",
            get_cols = \() {
              req(data())
              cols <- colnames(data())
              cols[nzchar(cols)] # Filter out empty column names
            },
            initial_value = by
          )

          # Update column choices when data changes
          observeEvent(
            data(),
            {
              req(data())
              cols <- colnames(data())

              # Filter out empty column names
              valid_cols <- cols[nzchar(cols)]

              # Create choices safely
              order_choices <- c("", valid_cols)
              weight_choices <- c("", valid_cols)

              updateSelectInput(session, "order_by", choices = order_choices)
              updateSelectInput(session, "weight_by", choices = weight_choices)
            },
            ignoreNULL = FALSE
          )

          # Update reactiveVals when inputs change
          observeEvent(input$type, {
            r_type(input$type)
          })

          observeEvent(
            input$n,
            {
              r_n(input$n)
            },
            ignoreNULL = FALSE
          )

          observeEvent(
            input$prop,
            {
              r_prop(input$prop)
            },
            ignoreNULL = FALSE
          )

          observeEvent(
            input$order_by,
            {
              r_order_by(input$order_by)
            },
            ignoreNULL = FALSE
          )

          observeEvent(
            input$with_ties,
            {
              r_with_ties(input$with_ties)
            },
            ignoreNULL = FALSE
          )

          observeEvent(
            input$weight_by,
            {
              r_weight_by(input$weight_by)
            },
            ignoreNULL = FALSE
          )

          observeEvent(
            input$replace,
            {
              r_replace(input$replace)
            },
            ignoreNULL = FALSE
          )

          observeEvent(
            input$rows,
            {
              r_rows(input$rows)
            },
            ignoreNULL = FALSE
          )

          # Restore type selector on initialization
          observe({
            updateSelectInput(
              session,
              inputId = "type",
              selected = r_type()
            )
          })

          # Helper function to build slice expression
          build_slice_expr <- function(
            type_val,
            n_val,
            prop_val,
            use_prop_val,
            order_by_val,
            with_ties_val,
            weight_by_val,
            replace_val,
            rows_val,
            by_val
          ) {
            # Validate inputs
            if (is.null(n_val) || n_val <= 0) {
              n_val <- 1
            }
            if (is.null(prop_val) || prop_val <= 0) {
              prop_val <- 0.1
            }

            # Format by/.by parameter
            # Helper functions (slice_head, slice_tail, slice_min, slice_max, slice_sample) use 'by'
            # Base slice() function uses '.by'
            format_by <- function(use_dot = FALSE) {
              if (
                length(by_val) > 0 && !all(by_val == "") && !all(is.na(by_val))
              ) {
                by_cols <- by_val[by_val != "" & !is.na(by_val)]
                if (length(by_cols) > 0) {
                  # Apply backticks to non-syntactic column names
                  backticked_cols <- backtick_if_needed(by_cols)
                  param_name <- if (use_dot) ".by" else "by"
                  return(paste0(
                    param_name,
                    " = c(",
                    paste0(backticked_cols, collapse = ", "),
                    ")"
                  ))
                }
              }
              NULL
            }

            # Build expression based on type
            if (type_val == "head") {
              args <- if (use_prop_val) {
                paste0("prop = ", prop_val)
              } else {
                paste0("n = ", n_val)
              }
              by_arg <- format_by(use_dot = FALSE)
              if (!is.null(by_arg)) {
                args <- paste(args, by_arg, sep = ", ")
              }
              return(parse(text = sprintf("dplyr::slice_head(data, %s)", args)))
            } else if (type_val == "tail") {
              args <- if (use_prop_val) {
                paste0("prop = ", prop_val)
              } else {
                paste0("n = ", n_val)
              }
              by_arg <- format_by(use_dot = FALSE)
              if (!is.null(by_arg)) {
                args <- paste(args, by_arg, sep = ", ")
              }
              return(parse(text = sprintf("dplyr::slice_tail(data, %s)", args)))
            } else if (type_val %in% c("min", "max")) {
              if (is.null(order_by_val) || order_by_val == "") {
                return(parse(text = "data[0, , drop = FALSE]")) # Return empty data frame
              }

              func <- if (type_val == "min") "slice_min" else "slice_max"
              # Apply backticks to non-syntactic column names
              args <- backtick_if_needed(order_by_val)
              if (use_prop_val) {
                args <- paste0(args, ", prop = ", prop_val)
              } else {
                args <- paste0(args, ", n = ", n_val)
              }
              args <- paste0(
                args,
                ", with_ties = ",
                if (with_ties_val) "TRUE" else "FALSE"
              )
              by_arg <- format_by(use_dot = FALSE)
              if (!is.null(by_arg)) {
                args <- paste(args, by_arg, sep = ", ")
              }
              return(parse(text = sprintf("dplyr::%s(data, %s)", func, args)))
            } else if (type_val == "sample") {
              args <- if (use_prop_val) {
                paste0("prop = ", prop_val)
              } else {
                paste0("n = ", n_val)
              }
              if (!is.null(weight_by_val) && weight_by_val != "") {
                # Apply backticks to non-syntactic column names
                args <- paste0(
                  args,
                  ", weight_by = ",
                  backtick_if_needed(weight_by_val)
                )
              }
              args <- paste0(
                args,
                ", replace = ",
                if (replace_val) "TRUE" else "FALSE"
              )
              by_arg <- format_by(use_dot = FALSE)
              if (!is.null(by_arg)) {
                args <- paste(args, by_arg, sep = ", ")
              }
              return(parse(
                text = sprintf("dplyr::slice_sample(data, %s)", args)
              ))
            } else if (type_val == "custom") {
              if (is.null(rows_val) || rows_val == "") {
                return(parse(text = "data[0, , drop = FALSE]")) # Return empty data frame
              }
              args <- rows_val
              by_arg <- format_by(use_dot = TRUE)
              if (!is.null(by_arg)) {
                args <- paste(args, by_arg, sep = ", ")
              }
              return(parse(text = sprintf("dplyr::slice(data, %s)", args)))
            }

            # Default fallback
            parse(text = "data[0, , drop = FALSE]")
          }

          # Reactive expression that updates immediately
          slice_expr <- reactive({
            req(input$type)

            # Get current input values with defaults
            n_val <- if (is.null(input$n)) n else as.integer(input$n)
            prop_val <- if (is.null(input$prop)) {
              prop
            } else {
              as.numeric(input$prop)
            }
            use_prop_val <- isTRUE(input$use_prop)
            order_by_val <- if (is.null(input$order_by)) {
              order_by
            } else {
              input$order_by
            }
            with_ties_val <- if (is.null(input$with_ties)) {
              with_ties
            } else {
              input$with_ties
            }
            weight_by_val <- if (is.null(input$weight_by)) {
              weight_by
            } else {
              input$weight_by
            }
            replace_val <- if (is.null(input$replace)) {
              replace
            } else {
              input$replace
            }
            rows_val <- if (is.null(input$rows)) rows else input$rows
            by_val <- r_by_selection()

            build_slice_expr(
              input$type,
              n_val,
              prop_val,
              use_prop_val,
              order_by_val,
              with_ties_val,
              weight_by_val,
              replace_val,
              rows_val,
              by_val
            )
          })

          # Return reactive expression and state
          list(
            expr = slice_expr,
            state = list(
              type = r_type,
              n = r_n,
              prop = r_prop,
              order_by = r_order_by,
              with_ties = r_with_ties,
              weight_by = r_weight_by,
              replace = r_replace,
              rows = r_rows,
              by = r_by_selection
            )
          )
        }
      )
    },
    function(id) {
      ns <- NS(id) # Create namespace function

      # NOTE: The slice block follows the ggplot block's responsive grid pattern,
      # unlike other dplyr blocks which use single-column full-width layouts.
      # This allows inputs to dynamically arrange into columns based on block width.
      # All inputs participate in the responsive grid via block-input-wrapper,
      # without forcing grid-column: 1 / -1.

      tagList(
        shinyjs::useShinyjs(),

        # Add CSS
        css_responsive_grid(),
        css_inline_checkbox(),

        # Block-specific CSS
        tags$style(HTML(
          "
          /* Slice block uses responsive grid layout - all inputs participate via block-input-wrapper */

          /* Control label styling */
          .slice-block-container .control-label {
            font-size: 0.875rem;
            color: #666;
            margin-bottom: 4px;
            font-weight: normal;
          }

          /* Type-specific checkboxes (Include ties, With replacement) */
          .block-section-grid .block-input-wrapper .checkbox {
            display: flex;
            align-items: center;
            margin-bottom: 0;
            margin-top: auto;
          }

          .block-section-grid .block-input-wrapper .checkbox label {
            font-size: 0.75rem;
            color: #6c757d;
            font-weight: normal;
            margin-bottom: 0;
            padding-left: 4px;
          }

          .block-section-grid .block-input-wrapper .checkbox input[type='checkbox'] {
            margin-top: 0;
            margin-right: 4px;
          }
          "
        )),

        div(
          class = "block-container slice-block-container",
          div(
            class = "block-form-grid",

            # Help text
            div(
              class = "block-help-text",
              p("Select rows by position, sampling, or extreme values.")
            ),

            # Slice Configuration Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Main slice type selector
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    ns("type"),
                    label = "Slice type",
                    choices = list(
                      "First rows" = "head",
                      "Last rows" = "tail",
                      "Smallest values" = "min",
                      "Largest values" = "max",
                      "Random sample" = "sample",
                      "Custom positions" = "custom"
                    ),
                    selected = type
                  )
                ),

                # Right column: Number/Proportion input + checkbox
                div(
                  class = "block-input-wrapper",
                  conditionalPanel(
                    condition = sprintf("input['%s'] != 'custom'", ns("type")),
                    div(
                      class = "block-inline-checkbox-wrapper",
                      div(
                        # Number of rows input
                        conditionalPanel(
                          condition = sprintf("!input['%s']", ns("use_prop")),
                          numericInput(
                            ns("n"),
                            label = "Number of rows",
                            value = n,
                            min = 1,
                            step = 1
                          )
                        ),
                        # Proportion input
                        conditionalPanel(
                          condition = sprintf("input['%s']", ns("use_prop")),
                          numericInput(
                            ns("prop"),
                            label = "Proportion (0 to 1)",
                            value = prop,
                            min = 0,
                            max = 1,
                            step = 0.01
                          )
                        )
                      ),
                      div(
                        class = "block-inline-checkbox",
                        checkboxInput(
                          ns("use_prop"),
                          label = "Proportion",
                          value = FALSE
                        )
                      )
                    )
                  )
                )
              )
            ),

            # Group by columns section
            div(
              class = "block-section",
              # Hack to avoid that the section participates in grid above
              tags$h4(""),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  style = "margin-top: -20px; padding-top: 0px;",
                  mod_by_selector_ui(
                    ns("by_selector"),
                    label = "Columns to group by (optional)",
                    initial_choices = by,
                    initial_selected = by
                  )
                )
              )
            ),

            # Type-specific options section
            div(
              class = "block-section",
              # Section header - only shown when there are type-specific options
              conditionalPanel(
                condition = sprintf(
                  "input['%s'] == 'min' || input['%s'] == 'max' || input['%s'] == 'sample' || input['%s'] == 'custom'",
                  ns("type"),
                  ns("type"),
                  ns("type"),
                  ns("type")
                ),
                tags$h4("Type-specific options")
              ),
              div(
                class = "block-section-grid",

                # Order by column (for min/max)
                conditionalPanel(
                  condition = sprintf(
                    "input['%s'] == 'min' || input['%s'] == 'max'",
                    ns("type"),
                    ns("type")
                  ),
                  div(
                    class = "block-input-wrapper",
                    div(
                      class = "block-inline-checkbox-wrapper",
                      div(
                        selectInput(
                          ns("order_by"),
                          label = "Order by column",
                          choices = character(),
                          selected = order_by
                        )
                      ),
                      div(
                        class = "block-inline-checkbox",
                        checkboxInput(
                          ns("with_ties"),
                          label = "Include ties",
                          value = with_ties
                        )
                      )
                    )
                  )
                ),

                # Weight by column (for sample)
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'sample'", ns("type")),
                  div(
                    class = "block-input-wrapper",
                    div(
                      class = "block-inline-checkbox-wrapper",
                      div(
                        selectInput(
                          ns("weight_by"),
                          label = "Weight by column (optional)",
                          choices = character(),
                          selected = weight_by
                        )
                      ),
                      div(
                        class = "block-inline-checkbox",
                        checkboxInput(
                          ns("replace"),
                          label = "Sample with replacement",
                          value = replace
                        )
                      )
                    )
                  )
                ),

                # Custom row positions
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'custom'", ns("type")),
                  div(
                    class = "block-input-wrapper",
                    style = "grid-column: 1 / -1;",
                    textInput(
                      ns("rows"),
                      label = "Row positions (e.g., 1:5, c(1,3,5), -c(2,4))",
                      value = rows,
                      placeholder = "1:5"
                    )
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "slice_block",
    allow_empty_state = c("order_by", "weight_by", "prop", "by"),
    ...
  )
}
