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
  string = list(newcol = "paste('type', 'here')"),
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

          r_by_selection <- reactiveVal(by)
          observeEvent(input$by, r_by_selection(input$by))

          # Update by cols when upstream data change
          observeEvent(
            colnames(data()),
            {
              updateSelectInput(
                session,
                inputId = "by",
                choices = colnames(data()),
                selected = r_by_selection()
              )
            }
          )

          r_string <- mod_kvexpr_server(
            id = "kv",
            get_value = \() unlist(string),
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
        mod_kvexpr_ui(NS(id, "kv")),
        selectInput(
          inputId = NS(id, "by"),
          label = "By columns",
          choices = list(),
          multiple = TRUE
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
  print(summarize_string)
  by_selection <- paste0("\"", by_selection, "\"", collapse = ", ")
  text <- if (identical(unname(summarize_string), "")) {
    "dplyr::summarize(data)"
  } else {
    summarize_string <- glue::glue("{names(summarize_string)} = {unname(summarize_string)}")
    glue::glue("dplyr::summarize(data, {summarize_string}, .by = c({by_selection}))")
  }
  parse(text = text)[1]
}

apply_summarize <- function(data, string, r_expr_validated, r_string_validated, by_selection) {
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
  print(expr)
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
