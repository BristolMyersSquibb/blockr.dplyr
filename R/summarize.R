#' Summarize block constructor
#'
#' This block allows to add new variables by summarizing over groups
#' (see [dplyr::summarize()]). Changes are applied after clicking the submit button.
#'
#' @param string Reactive expression returning character vector of
#'   expressions
#' @param by Columns to define grouping
#' @param ... Additional arguments forwarded to [new_block()]
#
#' @return A block object for summarize operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_summarize_block(), list(data = mtcars))
#'
#' # With a custom dataset
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' serve(new_summarize_block(), list(data = df))
#' }
#' @export
new_summarize_block <- function(
  string = list(count = "dplyr::n()"),
  by = character(),
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
          # Group by selector using unified component
          r_by_selection <- mod_by_selector_server(
            id = "by_selector",
            get_cols = \() colnames(data()),
            initial_value = by
          )

          r_string <- mod_multi_kvexpr_server(
            id = "mkv",
            get_value = \() string,
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_summarize(string, by))
          r_string_validated <- reactiveVal(string)

          # Validate and update on submit
          observeEvent(input$submit, {
            apply_summarize(
              data(),
              r_string(),
              r_expr_validated,
              r_string_validated,
              r_by_selection()
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              string = reactive(as.list(r_string_validated())),
              by = r_by_selection
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        mod_multi_kvexpr_ui(NS(id, "mkv")),
        mod_by_selector_ui(
          NS(id, "by_selector"),
          initial_choices = by,
          initial_selected = by
        ),
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
    class = "summarize_block",
    ...
  )
}

parse_summarize <- function(summarize_string = "", by_selection = character()) {
  text <- if (identical(unname(summarize_string), "")) {
    "dplyr::summarize(data)"
  } else {
    summarize_string <- glue::glue(
      "{names(summarize_string)} = {unname(summarize_string)}"
    )
    summarize_string <- glue::glue_collapse(summarize_string, sep = ", ")
    if (length(by_selection) > 0 && !all(by_selection == "")) {
      by_selection <- paste0("\"", by_selection, "\"", collapse = ", ")
      glue::glue(
        "dplyr::summarize(data, {summarize_string}, .by = c({by_selection}))"
      )
    } else {
      glue::glue("dplyr::summarize(data, {summarize_string})")
    }
  }
  parse(text = text)[1]
}

apply_summarize <- function(
  data,
  string,
  r_expr_validated,
  r_string_validated,
  by_selection
) {
  # Convert list to character vector if needed (for compatibility with multi_kvexpr)
  if (is.list(string)) {
    string <- unlist(string)
  }

  # If empty or only whitespace, return simple summarize
  if (all(trimws(unname(string)) == "")) {
    expr <- parse_summarize(string)
    r_expr_validated(expr)
    return()
  }

  req(string)
  stopifnot(is.character(string), !is.null(names(string)))
  expr <- try(parse_summarize(string, by_selection))
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
