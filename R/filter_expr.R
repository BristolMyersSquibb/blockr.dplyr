#' Expression filter block constructor
#'
#' This block allows filtering rows in a data frame based on R expressions
#' (see [dplyr::filter()]). Supports multiple conditions with AND/OR logic.
#' Changes are applied after clicking the submit button.
#'
#' @param exprs Reactive expression returning character vector of
#'   filter conditions (default: "TRUE" for no filtering)
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for expression-based filter operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon div
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' # Create a filter block
#' new_filter_expr_block("mpg > 20")
#'
#' if (interactive()) {
#'   # Basic usage with mtcars dataset
#'   library(blockr.core)
#'   serve(new_filter_expr_block(), list(data = mtcars))
#'
#'   # With custom initial condition
#'   serve(new_filter_expr_block("mpg > 20"), list(data = mtcars))
#'
#'   # Connected blocks example
#'   serve(
#'     new_board(
#'       blocks = list(
#'         a = new_dataset_block(),
#'         b = new_filter_expr_block()
#'       ),
#'       links = links(
#'         from = c("a"),
#'         to = c("b")
#'       )
#'     )
#'   )
#' }
#' @export
new_filter_expr_block <- function(exprs = "TRUE", ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Use multi-condition filter interface
          r_exprs <- mod_multi_filter_server(
            id = "mf",
            get_value = \() exprs,
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_filter_expr(exprs))
          r_exprs_validated <- reactiveVal(exprs)

          # Validate and update on submit
          observeEvent(input$submit, {
            apply_filter_expr(
              data(),
              r_exprs(),
              r_expr_validated,
              r_exprs_validated
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              exprs = r_exprs_validated
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
        css_single_column("filter"),
        css_doc_links(),

        div(
          class = "block-container filter-block-container",

          div(
            class = "block-form-grid",

            # Filter Conditions Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  p(
                    "Filter rows with R expressions. Use Ctrl+Space for autocomplete. ",
                    tags$a(
                      href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#filter-block",
                      target = "_blank",
                      style = "text-decoration: none; font-size: 0.9em;",
                      "\u2197"
                    )
                  ),
                  div(
                    class = "expression-help-link",
                    tags$a(
                      href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/expression-helpers.html#filter-helpers-if_any-and-if_all",
                      target = "_blank",
                      title = "Learn about filter helpers: if_any(), if_all(), and logical operators",
                      "Expression helpers guide \u2197"
                    )
                  )
                ),
                mod_multi_filter_ui(
                  NS(id, "mf"),
                  extra_button = actionButton(
                    NS(id, "submit"),
                    "Submit",
                    class = "btn-primary btn-sm"
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "filter_expr_block",
    ...
  )
}

parse_filter_expr <- function(filter_string = "") {
  text <- if (filter_string == "") {
    "dplyr::filter(data)"
  } else {
    glue::glue("dplyr::filter(data, {filter_string})")
  }
  parse(text = text)[[1]]
}

apply_filter_expr <- function(
  data,
  exprs,
  r_expr_validated,
  r_exprs_validated
) {
  # If empty or only whitespace, return simple filter
  if (trimws(exprs) == "") {
    expr <- parse_filter_expr("")
    r_expr_validated(expr)
    return()
  }

  req(exprs)
  stopifnot(is.character(exprs))

  expr <- try(parse_filter_expr(exprs))
  # Validation
  if (inherits(expr, "try-error")) {
    showNotification(
      expr,
      type = "error",
      duration = 5
    )
    return()
  }

  ans <- try(eval(expr))
  if (inherits(ans, "try-error")) {
    showNotification(
      ans,
      type = "error",
      duration = 5
    )
    return()
  }
  r_expr_validated(expr)
  r_exprs_validated(exprs)
}
