#' Summarize block constructor
#'
#' This block allows to add new variables by summarizing over groups
#' (see [dplyr::summarize()]). Changes are applied after clicking the submit button.
#'
#' @param string Reactive expression returning character vector of
#'   expressions
#' @param by Columns to define grouping
#' @param unpack Logical flag to unpack data frame columns from helper functions.
#'   When `TRUE`, expressions that return data frames will have their columns
#'   unpacked into separate columns. When `FALSE`, data frames are kept as nested
#'   list-columns. Default is `FALSE`.
#' @param ... Additional arguments forwarded to [new_block()]
#
#' @return A block object for summarize operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#'
#' @section Unpacking Helper Function Results:
#' When `unpack = TRUE`, helper functions that return data frames will have their
#' columns unpacked into separate columns in the result. This is useful for helper
#' functions like `stat_label()` that return multiple statistics as columns.
#'
#' ```r
#' # Without unpacking (default)
#' new_summarize_block(
#'   string = list(stats = "helper_func(...)"),
#'   unpack = FALSE
#' )
#' # Result: Creates nested list-column "stats" containing the data frame
#'
#' # With unpacking
#' new_summarize_block(
#'   string = list(stats = "helper_func(...)"),
#'   unpack = TRUE
#' )
#' # Result: Columns from helper_func() are unpacked into separate columns
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
#' # Using unpack to expand helper function results
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
#' # With unpacking enabled
#' serve(
#'   new_summarize_block(
#'     string = list(stats = "calc_stats(pick(everything()))"),
#'     by = "group",
#'     unpack = TRUE
#'   ),
#'   list(data = data.frame(x = 1:6, y = 10:15, group = rep(c("A", "B"), 3)))
#' )
#' # Result: group, mean_x, mean_y, sum_x, sum_y (columns unpacked)
#' }
#' @export
new_summarize_block <- function(
  string = list(count = "dplyr::n()"),
  by = character(),
  unpack = FALSE,
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

          # Unpack reactive value
          r_unpack <- reactiveVal(unpack)

          # Observe unpack checkbox changes and update reactively
          observeEvent(
            input$unpack,
            {
              r_unpack(input$unpack %||% FALSE)
              # Auto-update when unpack changes
              apply_summarize(
                data(),
                r_string_validated(),
                r_expr_validated,
                r_string_validated,
                r_by_selection(),
                r_unpack()
              )
            },
            ignoreNULL = FALSE
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_summarize(string, by, unpack))
          r_string_validated <- reactiveVal(string)

          # Auto-update when grouping changes
          observeEvent(r_by_selection(), {
            # Only update if we have validated expressions
            if (length(r_string_validated()) > 0) {
              apply_summarize(
                data(),
                r_string_validated(),
                r_expr_validated,
                r_string_validated,
                r_by_selection(),
                r_unpack()
              )
            }
          })

          # Validate and update on submit (for expression changes)
          observeEvent(input$submit, {
            apply_summarize(
              data(),
              r_string(),
              r_expr_validated,
              r_string_validated,
              r_by_selection(),
              r_unpack()
            )
          })

          list(
            expr = r_expr_validated,
            state = list(
              string = reactive(as.list(r_string_validated())),
              by = r_by_selection,
              unpack = r_unpack
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # Add responsive CSS
        block_responsive_css(),

        # Override grid to force single column for summarize block
        tags$style(HTML(sprintf(
          "
          .summarize-block-container .block-form-grid {
            grid-template-columns: 1fr !important;
          }
          .summarize-block-container .checkbox {
            font-size: 0.875rem;
          }
          #%1$s-advanced-options {
            max-height: 0;
            overflow: hidden;
            transition: max-height 0.3s ease-out;
            grid-column: 1 / -1;
            display: grid;
            grid-template-columns: subgrid;
            gap: 15px;
          }
          #%1$s-advanced-options.expanded {
            max-height: 200px;
            overflow: visible;
            transition: max-height 0.5s ease-in;
          }
          .advanced-toggle {
            cursor: pointer;
            user-select: none;
            padding: 8px 0;
            display: flex;
            align-items: center;
            gap: 6px;
            grid-column: 1 / -1;
            color: #6c757d;
            font-size: 0.875rem;
          }
          .advanced-toggle .chevron {
            transition: transform 0.2s;
            display: inline-block;
            font-size: 14px;
            font-weight: bold;
          }
          .advanced-toggle .chevron.rotated {
            transform: rotate(90deg);
          }
          ",
          id
        ))),

        div(
          class = "block-container summarize-block-container",
          div(
            class = "block-form-grid",

            # Summary Expressions Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  p(
                    "Create summary columns with R expressions. Use Ctrl+Space for autocomplete."
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
            ),

            # Advanced Options Toggle
            div(
              class = "block-section",
              div(
                class = "advanced-toggle text-muted",
                id = NS(id, "advanced-toggle"),
                onclick = sprintf(
                  "
                  const section = document.getElementById('%s');
                  const chevron = document.querySelector('#%s .chevron');
                  section.classList.toggle('expanded');
                  chevron.classList.toggle('rotated');
                  ",
                  NS(id, "advanced-options"),
                  NS(id, "advanced-toggle")
                ),
                tags$span(class = "chevron", "\u203A"),
                "Show advanced options"
              )
            ),

            # Advanced Options Section (Collapsible)
            div(
              id = NS(id, "advanced-options"),
              div(
                class = "block-section",
                div(
                  class = "block-section-grid",
                  div(
                    class = "block-input-wrapper",
                    checkboxInput(
                      NS(id, "unpack"),
                      "Unpack columns from data frame results",
                      value = unpack
                    ),
                    div(
                      class = "block-help-text",
                      style = "margin-top: 5px; font-size: 0.8rem;",
                      p(
                        "Useful with ", tags$code("across()"), " to apply functions across columns, e.g., ",
                        tags$code("across(where(is.numeric), mean)"), " computes means for all numeric columns."
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "summarize_block",
    ...
  )
}

parse_summarize <- function(
  summarize_string = "",
  by_selection = character(),
  unpack = FALSE
) {
  text <- if (identical(unname(summarize_string), "")) {
    "dplyr::summarize(data)"
  } else {
    # Build each expression part
    expr_parts <- character(length(summarize_string))
    expr_names <- names(summarize_string)

    for (i in seq_along(summarize_string)) {
      if (unpack) {
        # When unpacking: bare expression without name (unpacks data frame columns)
        expr_parts[i] <- unname(summarize_string)[i]
      } else {
        # Normal mode: use name = expression format
        # Only add name if it's not empty/NA
        if (!is.na(expr_names[i]) && expr_names[i] != "") {
          expr_parts[i] <- glue::glue(
            "{backtick_if_needed(expr_names[i])} = {unname(summarize_string)[i]}"
          )
        } else {
          expr_parts[i] <- unname(summarize_string)[i]
        }
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
  by_selection,
  unpack = FALSE
) {
  # Convert list to character vector if needed (for compatibility with multi_kvexpr)
  if (is.list(string)) {
    string <- unlist(string)
  }

  # If empty or only whitespace, return simple summarize
  if (all(trimws(unname(string)) == "")) {
    expr <- parse_summarize(string, character(), unpack)
    r_expr_validated(expr)
    return()
  }

  req(string)
  stopifnot(is.character(string), !is.null(names(string)))
  expr <- try(parse_summarize(string, by_selection, unpack))
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
