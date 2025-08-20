#' Filter block constructor
#'
#' This block allows filtering rows in a data frame based on conditions
#' (see [dplyr::filter()]). Supports multiple conditions with AND/OR logic.
#' Changes are applied after clicking the submit button.
#'
#' @param string Reactive expression returning character vector of
#'   filter conditions (default: "TRUE" for no filtering)
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for filter operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon div
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_filter_block(), list(data = mtcars))
#'
#' # With custom initial condition
#' serve(new_filter_block("mpg > 20"), list(data = mtcars))
#'
#' # Connected blocks example
#' serve(
#'   new_board(
#'     blocks = list(
#'       a = new_dataset_block(),
#'       b = new_filter_block()
#'     ),
#'     links = links(
#'       from = c("a"),
#'       to = c("b")
#'     )
#'   )
#' )
#' }
#' @export
new_filter_block <- function(string = "TRUE", ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          # Use multi-condition filter interface
          r_string <- mod_multi_filter_server(
            id = "mf",
            get_value = \() string,
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_filter(string))
          r_string_validated <- reactiveVal(string)

          # Validate and update on submit
          observeEvent(input$submit, {
            apply_filter(
              data(),
              r_string(),
              r_expr_validated,
              r_string_validated
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              string = r_string_validated
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        # Use multi-condition filter UI
        mod_multi_filter_ui(NS(id, "mf")),
        div(
          style = "text-align: right; margin-top: 10px;",
          actionButton(
            NS(id, "submit"),
            "Submit",
            icon = icon("paper-plane"),
            class = "btn-primary"
          )
        )
      )
    },
    class = "filter_block",
    ...
  )
}

parse_filter <- function(filter_string = "") {
  text <- if (filter_string == "") {
    "dplyr::filter(data)"
  } else {
    glue::glue("dplyr::filter(data, {filter_string})")
  }
  parse(text = text)[1]
}

apply_filter <- function(data, string, r_expr_validated, r_string_validated) {
  # If empty or only whitespace, return simple filter
  if (trimws(string) == "") {
    expr <- parse_filter("")
    r_expr_validated(expr)
    return()
  }

  req(string)
  stopifnot(is.character(string))

  expr <- try(parse_filter(string))
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
  r_string_validated(string)
}
