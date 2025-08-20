#' Join block constructor
#'
#' This block allows for joining of two `data.frame` objects with advanced
#' multi-column support including same-name and different-name joins (see
#' [dplyr::left_join()], [dplyr::inner_join()], etc.).
#'
#' @param type Join type (left_join, inner_join, right_join, full_join, semi_join, anti_join)
#' @param by Column(s) to join on - can be character vector for same-name joins
#'   or named list for different-name joins
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_join_block <- function(
  type = character(),
  by = character(),
  ...
) {
  join_types <- c(
    "left_join" = "Left Join - Keep all rows from left dataset",
    "inner_join" = "Inner Join - Keep only matching rows from both datasets",
    "right_join" = "Right Join - Keep all rows from right dataset",
    "full_join" = "Full Join - Keep all rows from both datasets",
    "semi_join" = "Semi Join - Keep left rows that have matches in right",
    "anti_join" = "Anti Join - Keep left rows that have no matches in right"
  )

  if (length(type)) {
    type <- match.arg(type, names(join_types))
  } else {
    type <- "left_join"  # Default to left_join (most common)
  }

  new_transform_block(
    function(id, x, y) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize state reactives
          r_join_type <- reactiveVal(type)
          r_join_keys <- reactiveVal(by)

          # Column name providers for join keys module
          get_x_cols <- reactive({
            req(x())
            colnames(x())
          })

          get_y_cols <- reactive({
            req(y())
            colnames(y())
          })

          # Initialize join keys module
          join_keys <- mod_join_keys_server(
            "join_keys",
            get_x_cols = get_x_cols,
            get_y_cols = get_y_cols,
            initial_keys = by
          )

          # Update join type when input changes
          observeEvent(input$type, {
            if (input$type %in% names(join_types)) {
              r_join_type(input$type)
            }
          })

          # Update join type selector when data changes
          observe({
            updateSelectInput(
              session,
              inputId = "type",
              choices = join_types,
              selected = r_join_type()
            )
          })

          # Build join expression
          build_join_expr <- function(join_type, keys) {
            # Handle different join key formats
            if (is.character(keys) && length(keys) > 0) {
              # Simple character vector - natural join
              keys_str <- deparse(keys)
              parse(text = glue::glue("dplyr::{join_type}(x, y, by = {keys_str})"))[[1]]
            } else if (is.list(keys) && length(keys) > 0) {
              # Complex join - handle different name mappings
              # Convert to dplyr join_by() compatible format
              join_spec <- if (length(keys) == 1 && is.character(keys[[1]])) {
                keys[[1]]
              } else {
                keys
              }
              join_spec_str <- deparse(join_spec)
              parse(text = glue::glue("dplyr::{join_type}(x, y, by = {join_spec_str})"))[[1]]
            } else {
              # Fallback to natural join on common columns
              parse(text = glue::glue("dplyr::{join_type}(x, y, by = intersect(colnames(x), colnames(y)))"))[[1]]
            }
          }

          list(
            expr = reactive({
              keys <- join_keys()
              # Only build expression if we have valid keys
              req(length(keys) > 0)
              if (is.list(keys)) {
                req(any(vapply(keys, function(k) length(k) > 0, logical(1))))
              }
              build_join_expr(r_join_type(), keys)
            }),
            state = list(
              type = r_join_type,
              by = join_keys
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        # Enhanced join type selector with descriptions
        div(
          class = "mb-3",
          selectInput(
            inputId = NS(id, "type"),
            label = "Join Type",
            choices = character(),
            width = "100%"
          )
        ),

        # Join keys configuration module
        mod_join_keys_ui(NS(id, "join_keys"), label = "Join Configuration"),

      )
    },
    class = "join_block",
    ...
  )
}
