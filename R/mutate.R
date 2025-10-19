#' Mutate block constructor
#'
#' This block allows to add new variables and preserve existing ones
#' (see [dplyr::mutate()]). Changes are applied after clicking the submit button.
#'
#' @param exprs Reactive expression returning character vector of
#'   expressions
#' @param by Character vector of column names for grouping. Default is empty.
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for mutate operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon tagList tags HTML
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars datase
#' library(blockr.core)
#' serve(new_mutate_block(), data = list(data = mtcars))
#'
#' # With a custom datase
#' df <- tibble::tibble(x = 1:5, `2025 Sales` = letters[1:5], .name_repair = "minimal")
#' serve(new_mutate_block(), data = list(data = df))
#' }
#' @export
new_mutate_block <- function(
  exprs = list(new_col = "1"),
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
          r_exprs <- mod_multi_kvexpr_server(
            id = "mkv",
            get_value = \() exprs,
            get_cols = \() colnames(data())
          )

          # Group by selector
          r_by_selection <- mod_by_selector_server(
            id = "by_selector",
            get_cols = \() colnames(data()),
            initial_value = by
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_mutate(exprs, by))
          r_exprs_validated <- reactiveVal(exprs)
          r_by_validated <- reactiveVal(by)

          # Auto-update when grouping changes
          observeEvent(r_by_selection(), {
            # Only update if we have validated expressions
            if (length(r_exprs_validated()) > 0) {
              apply_mutate(
                data(),
                r_exprs_validated(),
                r_by_selection(),
                r_expr_validated,
                r_exprs_validated,
                r_by_validated
              )
            }
          })

          # Validate and update on submit (for expression changes)
          observeEvent(input$submit, {
            apply_mutate(
              data(),
              r_exprs(),
              r_by_selection(),
              r_expr_validated,
              r_exprs_validated,
              r_by_validated
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              exprs = reactive(as.list(r_exprs_validated())),
              by = r_by_validated
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
        css_single_column("mutate"),
        css_doc_links(),

        # Block-specific CSS
        tags$style(HTML(
          "
          .mutate-block-container .checkbox {
            font-size: 0.875rem;
          }
          "
        )),

        div(
          class = "block-container mutate-block-container",

          div(
            class = "block-form-grid",

            # Mutate Expressions Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  p(
                    "Create or modify columns with R expressions. Use Ctrl+Space for autocomplete. ",
                    tags$a(
                      href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#mutate-block",
                      target = "_blank",
                      style = "text-decoration: none; font-size: 0.9em;",
                      "\u2197"
                    )
                  ),
                  div(
                    class = "expression-help-link",
                    tags$a(
                      href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/expression-helpers.html#useful-functions-for-mutate",
                      target = "_blank",
                      title = "Learn about common functions: lag(), lead(), case_when(), if_else(), and more",
                      "Expression helpers guide \u2197"
                    )
                  )
                ),
                mod_multi_kvexpr_ui(
                  NS(id, "mkv"),
                  extra_button = actionButton(
                    NS(id, "submit"),
                    "Submit",
                    class = "btn-primary btn-sm"
                  )
                )
              )
            ),

            # Grouping Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  style = "grid-column: 1 / -1;",
                  mod_by_selector_ui(
                    NS(id, "by_selector"),
                    label = tags$span(
                      "Columns to group by (optional)",
                      style = "font-size: 0.875rem; color: #666; font-weight: normal;"
                    ),
                    initial_choices = by,
                    initial_selected = by
                  )
                )
              )
            )
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
    # Apply backticks to non-syntactic column names on the left side
    new_names <- backtick_if_needed(names(mutate_string))

    mutate_string <- glue::glue(
      "{new_names} = {unname(mutate_string)}"
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
  exprs,
  by_selection,
  r_expr_validated,
  r_exprs_validated,
  r_by_validated
) {
  # Convert list to character vector if needed (for compatibility with multi_kvexpr)
  if (is.list(exprs)) {
    exprs <- unlist(exprs)
  }

  # If empty or only whitespace, return simple mutate
  if (all(trimws(unname(exprs)) == "")) {
    expr <- parse_mutate(exprs, by_selection)
    r_expr_validated(expr)
    r_by_validated(by_selection)
    return()
  }

  req(exprs)
  stopifnot(is.character(exprs), !is.null(names(exprs)))
  expr <- try(parse_mutate(exprs, by_selection))
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
  r_by_validated(by_selection)
}
