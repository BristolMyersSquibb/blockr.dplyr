#' Slice block constructor
#'
#' This block allows row selection using various dplyr slice functions
#' (see [dplyr::slice()], [dplyr::slice_head()], [dplyr::slice_tail()],
#' [dplyr::slice_min()], [dplyr::slice_max()], [dplyr::slice_sample()]).
#' Features reactive UI with immediate updates and comprehensive grouping support.
#'
#' @param type Character string specifying slice type: "head", "tail", "min", "max", "sample", or "custom"
#' @param n Number of rows to select (default: 5). Mutually exclusive with prop.
#' @param prop Proportion of rows to select (0 to 1, default: NULL). When specified, n is ignored.
#' @param order_by Column name to order by (for slice_min/slice_max)
#' @param with_ties Logical, whether to include ties (for slice_min/slice_max)
#' @param weight_by Column name for weighted sampling (for slice_sample)
#' @param replace Logical, whether to sample with replacement (for slice_sample)
#' @param rows Custom row positions (for slice)
#' @param by Character vector of column names for grouping
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for slice operations
#' @importFrom shiny NS moduleServer reactive req div conditionalPanel radioButtons numericInput selectInput checkboxInput textInput observeEvent updateSelectInput updateNumericInput
#' @importFrom dplyr slice slice_head slice_tail slice_min slice_max slice_sample
#' @seealso [new_transform_block()]
#' @examples
#' # Create a slice block
#' new_slice_block(type = "head", n = 5)
#'
#' if (interactive()) {
#'   # Basic usage
#'   library(blockr.core)
#'   serve(new_slice_block(), list(data = mtcars))
#'
#'   # Select first 5 rows
#'   serve(new_slice_block(type = "head", n = 5), list(data = mtcars))
#'
#'   # Select rows with highest mpg values
#'   serve(new_slice_block(type = "max", order_by = "mpg", n = 3), list(data = mtcars))
#'
#'   # Random sampling
#'   serve(new_slice_block(type = "sample", n = 10, replace = FALSE), list(data = mtcars))
#' }
#' @export
new_slice_block <- function(
  type = "head",
  n = 5,
  prop = NULL,
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

          # Determine initial mode based on constructor params
          # Use checkbox state: TRUE = proportion mode, FALSE = count mode
          r_use_prop <- reactiveVal(!is.null(prop))

          # Group by selector using unified componen
          r_by_selection <- mod_column_selector_server(
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

              updateSelectInput(session, "order_by",
                                choices = order_choices,
                                selected = r_order_by())
              updateSelectInput(session, "weight_by",
                                choices = weight_choices,
                                selected = r_weight_by())
            },
            ignoreNULL = FALSE
          )

          # Update reactiveVals when inputs change
          # Note: Removed ignoreNULL = FALSE to prevent overwriting initial values
          # in testServer context where inputs may not be initialized
          observeEvent(input$type, {
            r_type(input$type)
          })

          # Unified input handles both n and prop based on mode
          observeEvent(input$n, {
            if (!r_use_prop()) {
              r_n(input$n)
            } else {
              r_prop(input$n)
            }
          })

          # Handle proportion checkbox toggle
          observeEvent(input$use_prop, {
            r_use_prop(input$use_prop)
            # Update numericInput constraints and label based on mode
            if (isTRUE(input$use_prop)) {
              # Switching to proportion mode: 0 to 1, step 0.1
              # Clamp current value to valid range
              new_val <- min(1, max(0, input$n %||% 0.1))
              updateNumericInput(session, "n", min = 0, max = 1, step = 0.1, value = new_val)
              shinyjs::html("n_label", "Proportion (0 to 1)")
              r_prop(new_val)
            } else {
              # Switching to count mode: 1 to Inf, step 1
              # If current value < 1, set to 1
              new_val <- max(1, ceiling(input$n %||% 5))
              updateNumericInput(session, "n", min = 1, max = NA, step = 1, value = new_val)
              shinyjs::html("n_label", "Number of rows")
              r_n(new_val)
            }
          })

          observeEvent(input$order_by, {
            r_order_by(input$order_by)
          })

          observeEvent(input$with_ties, {
            r_with_ties(input$with_ties)
          })

          observeEvent(input$weight_by, {
            r_weight_by(input$weight_by)
          })

          observeEvent(input$replace, {
            r_replace(input$replace)
          })

          observeEvent(input$rows, {
            r_rows(input$rows)
          })

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
            order_by_val,
            with_ties_val,
            weight_by_val,
            replace_val,
            rows_val,
            by_val
          ) {
            # Determine mode: if prop is NULL, use n
            use_prop <- !is.null(prop_val)

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
              args <- if (use_prop) {
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
              args <- if (use_prop) {
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
              if (use_prop) {
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
              args <- if (use_prop) {
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
            # Use state reactiveVals with fallback to constructor parameters
            # This ensures the expression works in both testServer and production
            type_val <- r_type()
            n_val <- r_n()
            prop_val <- r_prop()
            order_by_val <- r_order_by()
            with_ties_val <- r_with_ties()
            weight_by_val <- r_weight_by()
            replace_val <- r_replace()
            rows_val <- r_rows()
            by_val <- r_by_selection()

            # Set prop_val to NULL if in count mode, n_val to NULL if in prop mode
            if (!r_use_prop()) {
              prop_val <- NULL
            } else {
              n_val <- NULL
            }

            build_slice_expr(
              type_val,
              n_val,
              prop_val,
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
              use_prop = r_use_prop,
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

          /* Integrated value input group - numericInput + checkbox together */
          .slice-value-group {
            width: 100%;
          }
          .slice-value-group .slice-value-label {
            font-size: 0.875rem;
            color: #666;
            margin-bottom: 4px;
            font-weight: normal;
            display: block;
          }
          .slice-value-group .input-group {
            display: flex;
            flex-wrap: nowrap;
            align-items: stretch;
            height: 38px; /* Match selectize input height */
          }
          /* The numericInput wrapper - hide its label, make input fill space */
          .slice-value-group .numeric-input-wrapper {
            flex: 1;
            min-width: 0;
          }
          .slice-value-group .numeric-input-wrapper .shiny-input-container {
            width: 100% !important;
            margin-bottom: 0;
            height: 100%;
          }
          .slice-value-group .numeric-input-wrapper .shiny-input-container > label {
            display: none;
          }
          .slice-value-group .numeric-input-wrapper input.form-control {
            border-top-right-radius: 0;
            border-bottom-right-radius: 0;
            border-right: none;
            height: 38px; /* Match selectize input height */
          }
          /* The checkbox container - styled as input-group-text */
          .slice-value-group .input-group-text {
            display: flex;
            align-items: center;
            padding: 0 0.5rem; /* Reduced padding */
            background-color: #e9ecef;
            border: 1px solid rgb(141, 149, 158); /* Match input border */
            border-top-right-radius: 3px; /* Match Bootstrap form-control */
            border-bottom-right-radius: 3px; /* Match Bootstrap form-control */
            height: 38px; /* Match input height */
          }
          .slice-value-group .input-group-text .shiny-input-container {
            margin-bottom: 0;
            width: auto !important;
          }
          .slice-value-group .input-group-text .checkbox {
            margin: 0;
            display: flex;
            align-items: center;
          }
          .slice-value-group .input-group-text .checkbox label {
            margin-bottom: 0;
            font-weight: 500;
            color: #6c757d;
            padding-left: 2px; /* Reduced padding */
          }
          .slice-value-group .input-group-text .checkbox input[type='checkbox'] {
            margin: 0;
          }
          "
        )),

        div(
          class = "block-container slice-block-container",

          div(
            class = "block-form-grid",

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
                    selected = type,
                    width = "100%"
                  )
                ),

                # Number/Proportion input with integrated checkbox
                div(
                  class = "block-input-wrapper",
                  conditionalPanel(
                    condition = sprintf("input['%s'] != 'custom'", ns("type")),
                    div(
                      class = "slice-value-group",
                      tags$label(
                        id = ns("n_label"),
                        class = "slice-value-label",
                        if (is.null(prop)) "Number of rows" else "Proportion (0 to 1)"
                      ),
                      div(
                        class = "input-group",
                        div(
                          class = "numeric-input-wrapper",
                          numericInput(
                            ns("n"),
                            label = "",
                            value = if (is.null(prop)) n else prop,
                            min = 0,
                            step = if (is.null(prop)) 1 else 0.1,
                            width = "100%"
                          )
                        ),
                        div(
                          class = "input-group-text",
                          checkboxInput(
                            ns("use_prop"),
                            label = "%",
                            value = !is.null(prop)
                          )
                        )
                      )
                    )
                  )
                ),

                # Order by column (for min/max) - shown right after slice type
                conditionalPanel(
                  condition = sprintf(
                    "input['%s'] == 'min' || input['%s'] == 'max'",
                    ns("type"),
                    ns("type")
                  ),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      ns("order_by"),
                      label = "Order by column",
                      choices = character(),
                      selected = order_by,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    style = "align-self: end;",
                    checkboxInput(
                      ns("with_ties"),
                      label = "Include ties",
                      value = with_ties
                    )
                  )
                ),

                # Weight by column (for sample)
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'sample'", ns("type")),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      ns("weight_by"),
                      label = "Weight by column (optional)",
                      choices = character(),
                      selected = weight_by,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    style = "align-self: end;",
                    checkboxInput(
                      ns("replace"),
                      label = "Sample with replacement",
                      value = replace
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
                      placeholder = "1:5",
                      width = "100%"
                    )
                  )
                ),

                # Group by columns - at the bottom (fixed width like mutate/summarize)
                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  mod_column_selector_ui(
                    ns("by_selector"),
                    label = "Columns to group by (optional)",
                    initial_choices = by,
                    initial_selected = by
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
