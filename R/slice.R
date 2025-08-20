#' Slice block constructor
#'
#' This block allows row selection using various dplyr slice functions
#' (see [dplyr::slice()], [dplyr::slice_head()], [dplyr::slice_tail()],
#' [dplyr::slice_min()], [dplyr::slice_max()], [dplyr::slice_sample()]).
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
#' @importFrom shiny NS moduleServer reactive observeEvent updateSelectInput updateNumericInput updateRadioButtons updateCheckboxInput div conditionalPanel radioButtons numericInput selectInput checkboxInput textInput uiOutput renderUI reactiveVal
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
          
          # Reactive values for all parameters
          r_type <- reactiveVal(type)
          r_n <- reactiveVal(n)
          r_prop <- reactiveVal(prop)
          r_use_prop <- reactiveVal(!is.null(prop) && prop != 0)
          r_order_by <- reactiveVal(order_by)
          r_with_ties <- reactiveVal(with_ties)
          r_weight_by <- reactiveVal(weight_by)
          r_replace <- reactiveVal(replace)
          r_rows <- reactiveVal(rows)
          r_by <- reactiveVal(by)
          
          # Update reactive values from inputs
          observeEvent(input$type, r_type(input$type))
          observeEvent(input$n, r_n(input$n))
          observeEvent(input$prop, r_prop(input$prop))
          observeEvent(input$use_prop, r_use_prop(input$use_prop == "prop"))
          observeEvent(input$order_by, r_order_by(input$order_by))
          observeEvent(input$with_ties, r_with_ties(input$with_ties))
          observeEvent(input$weight_by, r_weight_by(input$weight_by))
          observeEvent(input$replace, r_replace(input$replace))
          observeEvent(input$rows, r_rows(input$rows))
          observeEvent(input$by, r_by(input$by))
          
          # Update column choices when data changes
          observeEvent(colnames(data()), {
            cols <- colnames(data())
            updateSelectInput(session, "order_by", choices = c("", cols), selected = r_order_by())
            updateSelectInput(session, "weight_by", choices = c("", cols), selected = r_weight_by())
            updateSelectInput(session, "by", choices = cols, selected = r_by())
          })
          
          # Dynamic UI rendering based on type
          output$dynamic_ui <- shiny::renderUI({
            type_val <- r_type()
            
            if (type_val %in% c("head", "tail")) {
              div(
                radioButtons(
                  NS(id, "use_prop"),
                  label = "Selection method",
                  choices = list("Number of rows" = "n", "Proportion" = "prop"),
                  selected = if(r_use_prop()) "prop" else "n",
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'n'", NS(id, "use_prop")),
                  numericInput(
                    NS(id, "n"),
                    label = "Number of rows",
                    value = r_n(),
                    min = 1,
                    step = 1
                  )
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'prop'", NS(id, "use_prop")),
                  numericInput(
                    NS(id, "prop"),
                    label = "Proportion (0 to 1)",
                    value = r_prop() %||% 0.1,
                    min = 0,
                    max = 1,
                    step = 0.01
                  )
                )
              )
            } else if (type_val %in% c("min", "max")) {
              div(
                selectInput(
                  NS(id, "order_by"),
                  label = "Order by column",
                  choices = c("", colnames(data())),
                  selected = r_order_by()
                ),
                radioButtons(
                  NS(id, "use_prop"),
                  label = "Selection method",
                  choices = list("Number of rows" = "n", "Proportion" = "prop"),
                  selected = if(r_use_prop()) "prop" else "n",
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'n'", NS(id, "use_prop")),
                  numericInput(
                    NS(id, "n"),
                    label = "Number of rows",
                    value = r_n(),
                    min = 1,
                    step = 1
                  )
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'prop'", NS(id, "use_prop")),
                  numericInput(
                    NS(id, "prop"),
                    label = "Proportion (0 to 1)",
                    value = r_prop() %||% 0.1,
                    min = 0,
                    max = 1,
                    step = 0.01
                  )
                ),
                checkboxInput(
                  NS(id, "with_ties"),
                  label = "Include ties",
                  value = r_with_ties()
                )
              )
            } else if (type_val == "sample") {
              div(
                radioButtons(
                  NS(id, "use_prop"),
                  label = "Selection method",
                  choices = list("Number of rows" = "n", "Proportion" = "prop"),
                  selected = if(r_use_prop()) "prop" else "n",
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'n'", NS(id, "use_prop")),
                  numericInput(
                    NS(id, "n"),
                    label = "Number of rows",
                    value = r_n(),
                    min = 1,
                    step = 1
                  )
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'prop'", NS(id, "use_prop")),
                  numericInput(
                    NS(id, "prop"),
                    label = "Proportion (0 to 1)",
                    value = r_prop() %||% 0.1,
                    min = 0,
                    max = 1,
                    step = 0.01
                  )
                ),
                selectInput(
                  NS(id, "weight_by"),
                  label = "Weight by column (optional)",
                  choices = c("", colnames(data())),
                  selected = r_weight_by()
                ),
                checkboxInput(
                  NS(id, "replace"),
                  label = "Sample with replacement",
                  value = r_replace()
                )
              )
            } else if (type_val == "custom") {
              div(
                textInput(
                  NS(id, "rows"),
                  label = "Row positions (e.g., 1:5, c(1,3,5), -c(2,4))",
                  value = r_rows(),
                  placeholder = "1:5"
                )
              )
            }
          })
          
          # Build the expression
          expr <- reactive({
            type_val <- r_type()
            by_val <- r_by()
            
            # Helper to format .by parameter
            format_by <- function() {
              if (length(by_val) > 0 && !all(by_val == "")) {
                paste0(".by = c(", paste0('"', by_val, '"', collapse = ", "), ")")
              } else {
                NULL
              }
            }
            
            if (type_val == "head") {
              args <- if (r_use_prop() && !is.null(r_prop())) {
                paste0("prop = ", r_prop())
              } else {
                paste0("n = ", r_n())
              }
              by_arg <- format_by()
              if (!is.null(by_arg)) args <- paste(args, by_arg, sep = ", ")
              parse(text = sprintf("dplyr::slice_head(data, %s)", args))
              
            } else if (type_val == "tail") {
              args <- if (r_use_prop() && !is.null(r_prop())) {
                paste0("prop = ", r_prop())
              } else {
                paste0("n = ", r_n())
              }
              by_arg <- format_by()
              if (!is.null(by_arg)) args <- paste(args, by_arg, sep = ", ")
              parse(text = sprintf("dplyr::slice_tail(data, %s)", args))
              
            } else if (type_val == "min") {
              order_col <- r_order_by()
              if (order_col == "") {
                parse(text = "dplyr::slice(data, 0)")  # Return empty if no order column
              } else {
                args <- paste0("order_by = ", order_col)
                if (r_use_prop() && !is.null(r_prop())) {
                  args <- paste0(args, ", prop = ", r_prop())
                } else {
                  args <- paste0(args, ", n = ", r_n())
                }
                args <- paste0(args, ", with_ties = ", if(r_with_ties()) "TRUE" else "FALSE")
                by_arg <- format_by()
                if (!is.null(by_arg)) args <- paste(args, by_arg, sep = ", ")
                parse(text = sprintf("dplyr::slice_min(data, %s)", args))
              }
              
            } else if (type_val == "max") {
              order_col <- r_order_by()
              if (order_col == "") {
                parse(text = "dplyr::slice(data, 0)")  # Return empty if no order column
              } else {
                args <- paste0("order_by = ", order_col)
                if (r_use_prop() && !is.null(r_prop())) {
                  args <- paste0(args, ", prop = ", r_prop())
                } else {
                  args <- paste0(args, ", n = ", r_n())
                }
                args <- paste0(args, ", with_ties = ", if(r_with_ties()) "TRUE" else "FALSE")
                by_arg <- format_by()
                if (!is.null(by_arg)) args <- paste(args, by_arg, sep = ", ")
                parse(text = sprintf("dplyr::slice_max(data, %s)", args))
              }
              
            } else if (type_val == "sample") {
              args <- if (r_use_prop() && !is.null(r_prop())) {
                paste0("prop = ", r_prop())
              } else {
                paste0("n = ", r_n())
              }
              weight_col <- r_weight_by()
              if (weight_col != "") {
                args <- paste0(args, ", weight_by = ", weight_col)
              }
              args <- paste0(args, ", replace = ", if(r_replace()) "TRUE" else "FALSE")
              by_arg <- format_by()
              if (!is.null(by_arg)) args <- paste(args, by_arg, sep = ", ")
              parse(text = sprintf("dplyr::slice_sample(data, %s)", args))
              
            } else if (type_val == "custom") {
              rows_expr <- r_rows()
              if (rows_expr == "") {
                parse(text = "dplyr::slice(data, 0)")
              } else {
                by_arg <- format_by()
                if (!is.null(by_arg)) {
                  parse(text = sprintf("dplyr::slice(data, %s, %s)", rows_expr, by_arg))
                } else {
                  parse(text = sprintf("dplyr::slice(data, %s)", rows_expr))
                }
              }
            } else {
              parse(text = "dplyr::slice(data, 0)")  # Fallback
            }
          })
          
          # Return the expression and state
          list(
            expr = expr,
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
              by = r_by
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        selectInput(
          NS(id, "type"),
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
        uiOutput(NS(id, "dynamic_ui")),
        selectInput(
          NS(id, "by"),
          label = "Group by columns (optional)",
          choices = character(),
          selected = by,
          multiple = TRUE
        )
      )
    },
    class = "slice_block",
    ...
  )
}

# Helper function for NULL default
`%||%` <- function(x, y) if (is.null(x)) y else x