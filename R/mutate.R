#' Mutate block constructor
#'
#' This block allows to add new variables and preserve existing ones
#' (see [dplyr::mutate()]). Changes are applied after clicking the submit button.
#'
#' @param string Reactive expression returning character vector of
#'   expressions
#' @param by Character vector of column names for grouping. Default is empty.
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for mutate operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars datase
#' library(blockr.core)
#' serve(new_mutate_block(), list(data = mtcars))
#'
#' # With a custom datase
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' serve(new_mutate_block(), list(data = df))
#' }
#' @export
new_mutate_block <- function(
  string = list(new_col = "1"),
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
          r_string <- mod_multi_kvexpr_server(
            id = "mkv",
            get_value = \() string,
            get_cols = \() colnames(data())
          )

          # Group by selector
          r_by_selection <- mod_by_selector_server(
            id = "by_selector",
            get_cols = \() colnames(data()),
            initial_value = by
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_mutate(string, by))
          r_string_validated <- reactiveVal(string)
          r_by_validated <- reactiveVal(by)

          # Validate and update on submi
          observeEvent(input$submit, {
            apply_mutate(
              data(),
              r_string(),
              r_by_selection(),
              r_expr_validated,
              r_string_validated,
              r_by_validated
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              string = reactive(as.list(r_string_validated())),
              by = r_by_validated
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
    class = "mutate_block",
    allow_empty_state = c("by"),
    ...
  )
}
# serve(new_mutate_block(), list(data = mtcars))

parse_mutate <- function(mutate_string = "", by_selection = character()) {
  # Handle empty string case
  if (identical(unname(mutate_string), "")) {
    text <- "dplyr::mutate(data)"
  } else {
    mutate_string <- glue::glue(
      "{names(mutate_string)} = {unname(mutate_string)}"
    )
    mutate_string <- glue::glue_collapse(mutate_string, sep = ", ")

    # Add .by parameter if columns are selected
    if (length(by_selection) > 0 && !all(by_selection == "")) {
      by_cols <- by_selection[by_selection != ""]
      if (length(by_cols) > 0) {
        by_string <- paste0('c("', paste(by_cols, collapse = '", "'), '")')
        text <- glue::glue(
          "dplyr::mutate(data, {mutate_string}, .by = {by_string})"
        )
      } else {
        text <- glue::glue("dplyr::mutate(data, {mutate_string})")
      }
    } else {
      text <- glue::glue("dplyr::mutate(data, {mutate_string})")
    }
  }
  parse(text = text)[1]
}

apply_mutate <- function(
  data,
  string,
  by_selection,
  r_expr_validated,
  r_string_validated,
  r_by_validated
) {
  # Convert list to character vector if needed (for compatibility with multi_kvexpr)
  if (is.list(string)) {
    string <- unlist(string)
  }

  # If empty or only whitespace, return simple mutate
  if (all(trimws(unname(string)) == "")) {
    expr <- parse_mutate(string, by_selection)
    r_expr_validated(expr)
    r_by_validated(by_selection)
    return()
  }

  req(string)
  stopifnot(is.character(string), !is.null(names(string)))
  expr <- try(parse_mutate(string, by_selection))
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
  r_by_validated(by_selection)
}
