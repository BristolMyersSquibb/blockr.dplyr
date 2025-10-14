#' Summarize block constructor
#'
#' This block allows to add new variables by summarizing over groups
#' (see [dplyr::summarize()]). Changes are applied after clicking the submit button.
#'
#' @param string Reactive expression returning character vector of
#'   expressions. Names can be empty strings (`""`) to create unnamed expressions,
#'   which allows helper functions to unpack multiple columns directly into the result.
#' @param by Columns to define grouping
#' @param ... Additional arguments forwarded to [new_block()]
#
#' @return A block object for summarize operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#'
#' @section Named vs Unnamed Expressions:
#' **Named expressions** (`name = expression`) create a single column that may contain
#' nested data frames if the expression returns multiple columns:
#' ```r
#' string = list(result = "helper_func(...)")  # Creates 1 column named "result"
#' ```
#'
#' **Unnamed expressions** (`"" = expression`) allow the result to unpack multiple
#' columns directly into the output:
#' ```r
#' string_unnamed <- "helper_func(...)"
#' names(string_unnamed) <- ""  # Creates multiple columns from helper_func
#' ```
#'
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_summarize_block(), list(data = mtcars))
#'
#' # With a custom dataset
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' serve(new_summarize_block(), list(data = df))
#'
#' # Using a helper function that returns multiple columns
#' # Define the helper in your environment first
#' calc_stats <- function(df) {
#'   data.frame(
#'     mean_x = mean(df$x),
#'     mean_y = mean(df$y),
#'     sum_x = sum(df$x),
#'     sum_y = sum(df$y)
#'   )
#' }
#'
#' # Create unnamed expression to unpack all columns
#' my_string <- "calc_stats(pick(everything()))"
#' names(my_string) <- ""  # Empty name = unpack columns
#'
#' serve(
#'   new_summarize_block(string = my_string, by = "group"),
#'   list(data = data.frame(x = 1:6, y = 10:15, group = rep(c("A", "B"), 3)))
#' )
#' # Result: group, mean_x, mean_y, sum_x, sum_y (5 columns, unpacked)
#'
#' # Alternative: Pass columns directly (simpler, no pick() needed)
#' calc_stats_cols <- function(x, y) {
#'   data.frame(mean_x = mean(x), mean_y = mean(y))
#' }
#'
#' my_string2 <- "calc_stats_cols(x, y)"
#' names(my_string2) <- ""
#'
#' serve(
#'   new_summarize_block(string = my_string2, by = "group"),
#'   list(data = data.frame(x = 1:6, y = 10:15, group = rep(c("A", "B"), 3)))
#' )
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
    # Detect which expressions are unnamed (empty string or NA)
    expr_names <- names(summarize_string)
    is_unnamed <- is.na(expr_names) | expr_names == ""

    # Build each expression part
    expr_parts <- character(length(summarize_string))
    for (i in seq_along(summarize_string)) {
      if (is_unnamed[i]) {
        # Unnamed: just the expression (allows unpacking multi-column results)
        expr_parts[i] <- unname(summarize_string)[i]
      } else {
        # Named: use name = expression format
        expr_parts[i] <- glue::glue(
          "{backtick_if_needed(expr_names[i])} = {unname(summarize_string)[i]}"
        )
      }
    }

    # Combine all parts
    summarize_string <- glue::glue_collapse(expr_parts, sep = ", ")

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
