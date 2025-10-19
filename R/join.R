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
    type <- "left_join" # Default to left_join (most common)
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

          # Sync module reactive back to state reactive
          observeEvent(
            join_keys(),
            {
              r_join_keys(join_keys())
            },
            ignoreInit = TRUE
          )

          # Update join type when input changes
          observeEvent(
            input$type,
            {
              r_join_type(input$type)
            },
            ignoreInit = TRUE
          )

          # Build join expression
          build_join_expr <- function(join_type, keys) {
            # Handle different join key formats
            if (is.character(keys) && length(keys) > 0) {
              # Character vector - natural join like c("name", "band")
              keys_str <- deparse(keys)
              parse(
                text = glue::glue("dplyr::{join_type}(x, y, by = {keys_str})")
              )[[1]]
            } else if (is.list(keys) && length(keys) > 0) {
              # Named list format - convert to dplyr's named character vector
              by_vec <- unlist(keys)
              if (length(by_vec) > 0) {
                keys_str <- deparse(by_vec)
                parse(
                  text = glue::glue("dplyr::{join_type}(x, y, by = {keys_str})")
                )[[1]]
              } else {
                # Empty - fallback to natural join
                parse(
                  text = glue::glue(
                    "dplyr::{join_type}(x, y, by = intersect(colnames(x), colnames(y)))"
                  )
                )[[1]]
              }
            } else {
              # Fallback to natural join on common columns
              parse(
                text = glue::glue(
                  "dplyr::{join_type}(x, y, by = intersect(colnames(x), colnames(y)))"
                )
              )[[1]]
            }
          }

          list(
            expr = reactive({
              keys <- r_join_keys()
              # Only build expression if we have valid keys
              req(length(keys) > 0)
              if (is.list(keys)) {
                req(any(vapply(keys, function(k) length(k) > 0, logical(1))))
              }
              build_join_expr(r_join_type(), keys)
            }),
            state = list(
              type = r_join_type,
              by = r_join_keys
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # Add CSS
        css_responsive_grid(),
        css_single_column("join"),
        css_inline_checkbox(),

        # Block-specific CSS
        tags$style(HTML(
          "
          .join-block-container .block-help-text {
            margin-bottom: 0.5rem;
          }
          .join-block-container .block-help-text p {
            margin-top: 0;
            margin-bottom: 0;
          }
          .join-block-container select {
            max-width: 500px;
          }
          "
        )),

        div(
          class = "block-container join-block-container",
          div(
            class = "block-form-grid",

            # Help text
            div(
              class = "block-help-text",
              p("Combine two datasets by matching rows on join keys.")
            ),

            # Join Type Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Join type selector
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "type"),
                    label = "Join Type",
                    choices = c(
                      "Left Join - Keep all rows from left dataset" = "left_join",
                      "Inner Join - Keep only matching rows from both datasets" = "inner_join",
                      "Right Join - Keep all rows from right dataset" = "right_join",
                      "Full Join - Keep all rows from both datasets" = "full_join",
                      "Semi Join - Keep left rows that have matches in right" = "semi_join",
                      "Anti Join - Keep left rows that have no matches in right" = "anti_join"
                    ),
                    selected = type,
                    width = "100%"
                  )
                )
              )
            ),

            # Join Configuration Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Join keys configuration module
                mod_join_keys_ui(
                  NS(id, "join_keys"),
                  label = "Join Configuration"
                )
              )
            )
          )
        )
      )
    },
    allow_empty_state = "by",
    class = "join_block",
    ...
  )
}
