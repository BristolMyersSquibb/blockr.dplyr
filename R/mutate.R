#' Mutate block constructor
#'
#' This block allows to add new variables and preserve existing ones
#' (see [dplyr::mutate()]). Changes are applied after clicking the submit button.
#'
#' @param string Reactive expression returning character vector of
#'   expressions
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for mutate operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_mutate_block(), list(data = mtcars))
#'
#' # With a custom dataset
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' serve(new_mutate_block(), list(data = df))
#' }
#' @export
new_mutate_block <- function(
  string = list(new_col = "1"),
  ...
) {
  # as discussed in https://github.com/cynkra/blockr.dplyr/issues/16
  # the state must be a list with named elements, rather than a named vector.
  # internally, we still work with named vectors.
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          r_string <- mod_multi_kvexpr_server(
            id = "mkv",
            get_value = \() string,
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_mutate(string))
          r_string_validated <- reactiveVal(string)

          # Validate and update on submit
          observeEvent(input$submit, {
            apply_mutate(
              data(),
              r_string(),
              r_expr_validated,
              r_string_validated
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              string = reactive(as.list(r_string_validated()))
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        mod_multi_kvexpr_ui(NS(id, "mkv")),
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
    class = "mutate_block",
    ...
  )
}
# serve(new_mutate_block(), list(data = mtcars))

parse_mutate <- function(mutate_string = "") {
  text <- if (identical(unname(mutate_string), "")) {
    "dplyr::mutate(data)"
  } else {
    mutate_string <- glue::glue("{names(mutate_string)} = {unname(mutate_string)}")
    mutate_string <- glue::glue_collapse(mutate_string, sep = ", ")
    glue::glue("dplyr::mutate(data, {mutate_string})")
  }
  parse(text = text)[1]
}

apply_mutate <- function(data, string, r_expr_validated, r_string_validated) {
  # Convert list to character vector if needed (for compatibility with multi_kvexpr)
  if (is.list(string)) {
    string <- unlist(string)
  }
  
  # If empty or only whitespace, return simple mutate
  if (all(trimws(unname(string)) == "")) {
    expr <- parse_mutate(string)
    r_expr_validated(expr)
    return()
  }

  req(string)
  stopifnot(is.character(string), !is.null(names(string)))
  expr <- try(parse_mutate(string))
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
