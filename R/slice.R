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

            # Format .by parameter
            format_by <- function() {
              if (
                length(by_val) > 0 && !all(by_val == "") && !all(is.na(by_val))
              ) {
                by_cols <- by_val[by_val != "" & !is.na(by_val)]
                if (length(by_cols) > 0) {
                  return(paste0(
                    ".by = c(",
                    paste0('"', by_cols, '"', collapse = ", "),
                    ")"
                  ))
                }
              }
              NULL
            }

            by_arg <- format_by()

            # Build expression based on type
            if (type_val == "head") {
              args <- if (use_prop_val) {
                paste0("prop = ", prop_val)
              } else {
                paste0("n = ", n_val)
              }
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
              if (!is.null(by_arg)) {
                args <- paste(args, by_arg, sep = ", ")
              }
              return(parse(text = sprintf("dplyr::slice_tail(data, %s)", args)))
            } else if (type_val %in% c("min", "max")) {
              if (is.null(order_by_val) || order_by_val == "") {
                return(parse(text = "data[0, , drop = FALSE]")) # Return empty data frame
              }

              func <- if (type_val == "min") "slice_min" else "slice_max"
              args <- paste0('"', order_by_val, '"')
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
                args <- paste0(args, ', weight_by = "', weight_by_val, '"')
              }
              args <- paste0(
                args,
                ", replace = ",
                if (replace_val) "TRUE" else "FALSE"
              )
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
            use_prop_val <- if (is.null(input$use_prop)) {
              FALSE
            } else {
              (input$use_prop == "prop")
            }
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
              type = type,
              n = n,
              prop = prop,
              order_by = order_by,
              with_ties = with_ties,
              weight_by = weight_by,
              replace = replace,
              rows = rows,
              by = r_by_selection
            )
          )
        }
      )
    },
    function(id) {
      ns <- NS(id) # Create namespace function
      div(
        class = "m-3",

        # Main slice type selector
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
        ),

        # Selection method for applicable types
        conditionalPanel(
          condition = sprintf("input['%s'] != 'custom'", ns("type")),
          radioButtons(
            ns("use_prop"),
            label = "Selection method",
            choices = list("Number of rows" = "n", "Proportion" = "prop"),
            selected = "n",
            inline = TRUE
          )
        ),

        # Number of rows inpu
        conditionalPanel(
          condition = sprintf(
            "input['%s'] != 'custom' && (input['%s'] == 'n' || !input['%s'])",
            ns("type"),
            ns("use_prop"),
            ns("use_prop")
          ),
          numericInput(
            ns("n"),
            label = "Number of rows",
            value = n,
            min = 1,
            step = 1
          )
        ),

        # Proportion inpu
        conditionalPanel(
          condition = sprintf(
            "input['%s'] != 'custom' && input['%s'] == 'prop'",
            ns("type"),
            ns("use_prop")
          ),
          numericInput(
            ns("prop"),
            label = "Proportion (0 to 1)",
            value = prop,
            min = 0,
            max = 1,
            step = 0.01
          )
        ),

        # Order by column (for min/max)
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'min' || input['%s'] == 'max'",
            ns("type"),
            ns("type")
          ),
          selectInput(
            ns("order_by"),
            label = "Order by column",
            choices = character(),
            selected = order_by
          ),
          checkboxInput(
            ns("with_ties"),
            label = "Include ties",
            value = with_ties
          )
        ),

        # Weight by column (for sample)
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sample'", ns("type")),
          selectInput(
            ns("weight_by"),
            label = "Weight by column (optional)",
            choices = character(),
            selected = weight_by
          ),
          checkboxInput(
            ns("replace"),
            label = "Sample with replacement",
            value = replace
          )
        ),

        # Custom row positions
        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("type")),
          textInput(
            ns("rows"),
            label = "Row positions (e.g., 1:5, c(1,3,5), -c(2,4))",
            value = rows,
            placeholder = "1:5"
          )
        ),

        # Group by columns using unified componen
        mod_by_selector_ui(
          ns("by_selector"),
          initial_choices = by,
          initial_selected = by
        )
      )
    },
    class = "slice_block",
    allow_empty_state = c("order_by", "weight_by", "prop", "by"),
    ...
  )
}
