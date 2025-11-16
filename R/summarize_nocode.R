#' Get available summary functions for no-code summarize
#'
#' Returns a named vector of common summary functions with proper namespacing.
#' This helper function centralizes the definition of available summary functions
#' and allows extension via the `blockr.dplyr.summary_functions` option.
#'
#' @section Extending with custom functions:
#' You can extend the available summary functions by setting the
#' `blockr.dplyr.summary_functions` option. Custom functions should be provided
#' as a named character vector where names are display labels (descriptions)
#' and values are the function calls with proper namespacing.
#'
#' @section Example:
#' \preformatted{
#' # Add custom functions from blockr.topline package
#' options(
#'   blockr.dplyr.summary_functions = c(
#'     "extract parentheses (paren_num)" = "blockr.topline::paren_num",
#'     "first number (first_num)" = "blockr.topline::first_num"
#'   )
#' )
#' }
#'
#' If no description is provided (i.e., names are empty), the function name
#' will be used as the display label.
#'
#' @return Named character vector where names are display names and values are
#'   fully qualified function calls
#' @keywords internal
#' @noRd
get_summary_functions <- function() {
  # Default summary functions
  default_funcs <- c(
    # Center
    "mean" = "mean",
    "median" = "median",

    # Spread
    "standard deviation (sd)" = "sd",
    "IQR" = "IQR",
    "median absolute deviation (mad)" = "mad",

    # Range
    "minimum (min)" = "min",
    "maximum (max)" = "max",

    # Position
    "first" = "dplyr::first",
    "last" = "dplyr::last",

    # Count
    "count rows (n)" = "dplyr::n",
    "count distinct (n_distinct)" = "dplyr::n_distinct",

    # Sums and products
    "sum" = "sum",
    "product (prod)" = "prod"
  )

  # Get custom functions from blockr option
  custom_funcs <- blockr_option("dplyr.summary_functions", NULL)

  # Validate custom functions
  if (!is.null(custom_funcs)) {
    if (!is.character(custom_funcs)) {
      warning(
        "blockr.dplyr.summary_functions must be a character vector. Ignoring custom functions.",
        call. = FALSE
      )
      return(default_funcs)
    }

    # If names are missing or empty, use the function name as label
    if (is.null(names(custom_funcs))) {
      names(custom_funcs) <- custom_funcs
    } else {
      # Fill in missing names with function values
      empty_names <- names(custom_funcs) == "" | is.na(names(custom_funcs))
      names(custom_funcs)[empty_names] <- custom_funcs[empty_names]
    }

    # Merge: custom functions come first, then defaults not overridden
    return(c(custom_funcs, default_funcs[!names(default_funcs) %in% names(custom_funcs)]))
  }

  default_funcs
}

#' Multi summarize module for no-code summary operations
#'
#' A Shiny module that manages multiple new_name ~ function(column) mappings
#' for no-code summarization. Supports adding and removing summary pairs dynamically.
#'
#' @param id The module ID
#' @param get_value Function that returns initial values as a named list
#' @param get_cols Function that returns column names for dropdown selection
#'
#' @return A reactive expression containing the current summary specifications
#' @importFrom shiny req NS moduleServer reactive actionButton observeEvent renderUI uiOutput tagList div selectInput textInput
#' @importFrom shinyjs useShinyjs
#' @importFrom htmltools tags
#' @keywords internal
#' @noRd
mod_multi_summarize_server <- function(id, get_value, get_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with values from get_value
    # Structure: list(new_col = list(func = "mean", col = "mpg"))
    initial_values <- get_value()

    if (length(initial_values) == 0 || is.null(initial_values)) {
      available_cols <- get_cols()
      default_col <- if (length(available_cols) > 0) {
        available_cols[1]
      } else {
        ""
      }
      initial_values <- list(
        count = list(func = "dplyr::n", col = "")
      )
    } else {
      # Normalize initial values - ensure col and func are valid
      initial_values <- lapply(initial_values, function(spec) {
        if (is.null(spec$col) || length(spec$col) == 0) {
          spec$col <- ""
        }
        if (is.null(spec$func) || length(spec$func) == 0) {
          spec$func <- "dplyr::n"
        }
        spec
      })
    }

    # Store summaries as reactive value
    r_summaries <- reactiveVal(initial_values)
    r_cols <- reactive(get_cols())

    # Track which summary indices exist
    r_summary_indices <- reactiveVal(seq_along(initial_values))
    r_next_index <- reactiveVal(length(initial_values) + 1)

    # Collect current values from all inputs
    get_current_summaries <- function() {
      indices <- r_summary_indices()
      if (length(indices) == 0) {
        return(list())
      }

      result <- list()
      for (i in indices) {
        new_name_id <- paste0("summary_", i, "_new")
        func_id <- paste0("summary_", i, "_func")
        col_id <- paste0("summary_", i, "_col")

        new_name <- input[[new_name_id]]
        func <- input[[func_id]]
        col <- input[[col_id]]

        if (
          !is.null(new_name) &&
            !is.null(func) &&
            !is.null(col) &&
            new_name != "" &&
            func != "" &&
            (col != "" || func == "dplyr::n")
        ) {
          result[[new_name]] <- list(func = func, col = col)
        }
      }

      if (length(result) == 0) {
        available_cols <- r_cols()
        default_col <- if (length(available_cols) > 0) {
          available_cols[1]
        } else {
          "col"
        }
        result <- list(
          summary_col = list(func = "mean", col = default_col)
        )
      }

      result
    }

    # Add new summary pair
    observeEvent(input$add_summary, {
      current_indices <- r_summary_indices()
      new_index <- r_next_index()

      # Add new index
      r_summary_indices(c(current_indices, new_index))
      r_next_index(new_index + 1)

      # Get current summaries and add new one
      current <- get_current_summaries()

      # Generate unique new name
      available_cols <- r_cols()
      new_name <- "summary_col"
      i <- 1
      while (new_name %in% names(current)) {
        new_name <- paste0("summary_col_", i)
        i <- i + 1
      }

      # Choose first available column not already used
      used_cols <- sapply(current, function(x) x$col)
      available_remaining <- setdiff(available_cols, used_cols)
      col_name <- if (length(available_remaining) > 0) {
        available_remaining[1]
      } else {
        available_cols[1]
      }

      current[[new_name]] <- list(func = "mean", col = col_name)
      r_summaries(current)
    })

    # Remove summary handlers - create them dynamically
    observe({
      indices <- r_summary_indices()

      lapply(indices, function(i) {
        observeEvent(input[[paste0("summary_", i, "_remove")]], {
          current_indices <- r_summary_indices()

          if (length(current_indices) > 1) {
            # Remove this index
            new_indices <- setdiff(current_indices, i)
            r_summary_indices(new_indices)

            # Update summaries
            current <- get_current_summaries()
            r_summaries(current)
          }
        })
      })
    })

    # Render UI dynamically
    output$summaries_ui <- renderUI({
      indices <- r_summary_indices()
      summaries <- r_summaries()
      available_cols <- r_cols()
      available_funcs <- get_summary_functions()

      # Require data to be available before rendering
      req(length(available_cols) > 0)

      if (length(indices) == 0) {
        return(NULL)
      }

      summary_names <- names(summaries)

      # Create UI for each summary pair
      tagList(
        lapply(seq_along(indices), function(j) {
          i <- indices[j]
          new_name <- if (j <= length(summary_names)) {
            summary_names[j]
          } else {
            "summary_col"
          }

          spec <- if (j <= length(summaries)) {
            summaries[[j]]
          } else {
            list(func = "mean", col = available_cols[1])
          }

          # Ensure col is a valid string for UI
          col_value <- spec$col
          if (is.null(col_value) || length(col_value) == 0 || col_value == "") {
            col_value <- if (length(available_cols) > 0) available_cols[1] else ""
          }

          # Ensure func is valid
          func_value <- spec$func
          if (is.null(func_value) || length(func_value) == 0 || func_value == "") {
            func_value <- "mean"
          }

          multi_summarize_row_ui(
            ns(paste0("summary_", i)),
            new_name = new_name,
            func = func_value,
            col = col_value,
            available_funcs = available_funcs,
            available_cols = available_cols,
            show_remove = (length(indices) > 1)
          )
        })
      )
    })

    # Return the reactive summaries
    reactive({
      # Check if any inputs exist yet - if not, use stored summaries
      indices <- r_summary_indices()
      has_inputs <- any(sapply(indices, function(i) {
        paste0("summary_", i, "_new") %in%
          names(input) &&
          paste0("summary_", i, "_func") %in% names(input) &&
          paste0("summary_", i, "_col") %in% names(input)
      }))

      if (has_inputs) {
        # Use current input values
        get_current_summaries()
      } else {
        # Use stored summaries (for initialization)
        r_summaries()
      }
    })
  })
}

#' Create multi summarize UI module
#'
#' @param id The module ID
#' @param extra_button Optional UI element (e.g., submit button) to display on the right side
#' @return A div containing the UI elements
#' @keywords internal
#' @noRd
mod_multi_summarize_ui <- function(id, extra_button = NULL) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(
      "
      .multi-summarize-container {
        margin-top: -8px;
      }

      .multi-summarize-pair {
        display: flex;
        width: 100%;
        align-items: stretch;
        gap: 4px;
        margin-bottom: 8px;
      }

      .multi-summarize-pair .summarize-new {
        flex: 0 0 25%;
      }

      .multi-summarize-pair .summarize-equals {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        justify-content: center;
        color: var(--bs-gray-400);
        font-size: 0.9em;
        width: 25px;
      }

      .multi-summarize-pair .summarize-func {
        flex: 0 0 30%;
      }

      .multi-summarize-pair .summarize-paren-open {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        justify-content: center;
        color: var(--bs-gray-400);
        font-size: 1.1em;
        width: 20px;
      }

      .multi-summarize-pair .summarize-col {
        flex: 1;
      }

      .multi-summarize-pair .summarize-paren-close {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        justify-content: center;
        color: var(--bs-gray-400);
        font-size: 1.1em;
        width: 20px;
      }

      .multi-summarize-pair .summarize-delete {
        flex: 0 0 auto;
        height: 38px;
        width: 35px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #6c757d;
        border: none;
        background: transparent;
        padding: 0;
      }

      .multi-summarize-pair .summarize-delete:hover {
        color: #dc3545;
        background: rgba(220, 53, 69, 0.1);
      }

      /* Remove default margins from Shiny inputs */
      .multi-summarize-pair .shiny-input-container {
        margin-bottom: 0 !important;
      }

      /* Ensure inputs and selects fill their containers and align properly */
      .multi-summarize-pair .form-control,
      .multi-summarize-pair .selectize-control,
      .multi-summarize-pair .selectize-input {
        width: 100% !important;
        height: 38px !important;
        margin-bottom: 0 !important;
      }

      .multi-summarize-pair .selectize-input {
        min-height: 38px;
        line-height: 24px;
        padding-top: 4px;
        padding-bottom: 4px;
        display: flex;
        align-items: center;
      }

      .multi-summarize-actions {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-top: 0.5rem;
      }

      .multi-summarize-actions .btn-outline-secondary {
        border-color: #dee2e6;
        color: #6c757d;
      }

      .multi-summarize-actions .btn-outline-secondary:hover {
        border-color: #adb5bd;
        background-color: #f8f9fa;
        color: #495057;
      }
    "
    ),
    div(
      class = "multi-summarize-container",
      uiOutput(ns("summaries_ui")),
      div(
        class = "multi-summarize-actions",
        actionButton(
          ns("add_summary"),
          label = "Add Summary",
          icon = icon("plus"),
          class = "btn btn-outline-secondary btn-sm"
        ),
        extra_button
      )
    )
  )
}

#' Create UI for a single summary row
#'
#' @param id Row identifier
#' @param new_name New column name
#' @param func Summary function
#' @param col Column to summarize
#' @param available_funcs Available summary functions
#' @param available_cols Available column names for dropdown
#' @param show_remove Whether to show remove button
#' @return A div containing the row UI
#' @keywords internal
#' @noRd
multi_summarize_row_ui <- function(
  id,
  new_name = "summary_col",
  func = "mean",
  col = "",
  available_funcs = get_summary_functions(),
  available_cols = character(),
  show_remove = TRUE
) {
  div(
    class = "multi-summarize-pair",
    div(
      class = "summarize-new",
      textInput(
        paste0(id, "_new"),
        label = NULL,
        value = new_name,
        placeholder = "New column"
      )
    ),
    div(
      class = "summarize-equals",
      "="
    ),
    div(
      class = "summarize-func",
      selectInput(
        paste0(id, "_func"),
        label = NULL,
        choices = available_funcs,
        selected = func,
        width = "100%"
      )
    ),
    div(
      class = "summarize-paren-open",
      "("
    ),
    div(
      class = "summarize-col",
      selectInput(
        paste0(id, "_col"),
        label = NULL,
        choices = available_cols,
        selected = col,
        width = "100%"
      )
    ),
    div(
      class = "summarize-paren-close",
      ")"
    ),
    if (show_remove) {
      actionButton(
        paste0(id, "_remove"),
        label = NULL,
        icon = icon("xmark"),
        class = "btn btn-sm summarize-delete",
        title = "Remove summary"
      )
    }
  )
}

#' Summarize no-code block constructor
#'
#' This block provides a no-code interface for summarizing data (see [dplyr::summarize()]).
#' Instead of writing expressions, users select summary functions from dropdowns
#' (mean, median, sum, etc.), choose columns to summarize, and specify new column names.
#'
#' @section Extending available functions:
#' The list of available summary functions can be extended using the
#' \code{blockr.dplyr.summary_functions} option. Set this option to a named
#' character vector where names are display labels and values are function calls:
#'
#' \preformatted{
#' options(
#'   blockr.dplyr.summary_functions = c(
#'     "extract parentheses (paren_num)" = "blockr.topline::paren_num"
#'   )
#' )
#' }
#'
#' If a description is not provided (empty name), the function name will be
#' used as the display label.
#'
#' @param summaries Named list where each element is a list with 'func' and 'col' elements.
#'   For example: list(avg_mpg = list(func = "mean", col = "mpg"))
#' @param by Columns to define grouping
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for no-code summarize operations
#' @importFrom shiny req showNotification NS moduleServer reactive observeEvent
#' @importFrom glue glue
#' @importFrom blockr.core blockr_option
#' @seealso [new_transform_block()], [new_summarize_block()]
#'
#' @examples
#' # Create a summarize no-code block
#' new_summarize_nocode_block()
#'
#' if (interactive()) {
#'   # Basic usage with mtcars dataset
#'   library(blockr.core)
#'   serve(new_summarize_nocode_block(), data = list(data = mtcars))
#'
#'   # With predefined summaries
#'   serve(
#'     new_summarize_nocode_block(
#'       summaries = list(
#'         avg_mpg = list(func = "mean", col = "mpg"),
#'         max_hp = list(func = "max", col = "hp")
#'       )
#'     ),
#'     data = list(data = mtcars)
#'   )
#'
#'   # With grouping
#'   serve(
#'     new_summarize_nocode_block(
#'       summaries = list(avg_mpg = list(func = "mean", col = "mpg")),
#'       by = "cyl"
#'     ),
#'     data = list(data = mtcars)
#'   )
#' }
#' @export
new_summarize_nocode_block <- function(
  summaries = list(count = list(func = "dplyr::n", col = "")),
  by = character(),
  ...
) {
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

          r_summaries <- mod_multi_summarize_server(
            id = "ms",
            get_value = \() summaries,
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_summarize_nocode(summaries, by))
          r_summaries_validated <- reactiveVal(summaries)

          # Auto-update when summaries change
          observeEvent(
            r_summaries(),
            {
              apply_summarize_nocode(
                data(),
                r_summaries(),
                r_expr_validated,
                r_summaries_validated,
                r_by_selection(),
                session
              )
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )

          # Auto-update when grouping changes
          observeEvent(
            r_by_selection(),
            {
              # Only update if we have validated summaries
              if (length(r_summaries_validated()) > 0) {
                apply_summarize_nocode(
                  data(),
                  r_summaries_validated(),
                  r_expr_validated,
                  r_summaries_validated,
                  r_by_selection(),
                  session
                )
              }
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )

          list(
            expr = r_expr_validated,
            state = list(
              summaries = reactive(r_summaries_validated()),
              by = r_by_selection
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
        css_single_column("summarize-nocode"),
        css_doc_links(),

        div(
          class = "block-container summarize-nocode-block-container",

          div(
            class = "block-form-grid",

            # Summary Specifications Section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  p(
                    "Select functions and columns to create summaries. No coding required. ",
                    tags$a(
                      href = "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#summarize-block",
                      target = "_blank",
                      style = "text-decoration: none; font-size: 0.9em;",
                      "\u2197"
                    )
                  )
                ),
                mod_multi_summarize_ui(
                  NS(id, "ms")
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
    class = "summarize_nocode_block",
    allow_empty_state = c("by"),
    ...
  )
}

#' Parse summary specifications into dplyr expression
#'
#' @param summaries Named list of summary specifications
#' @param by_selection Character vector of grouping columns
#' @return Parsed expression for dplyr::summarize()
#' @noRd
parse_summarize_nocode <- function(
  summaries = list(),
  by_selection = character()
) {
  if (length(summaries) == 0) {
    return(parse(text = "dplyr::summarize(data)")[[1]])
  }

  # Build each summary expression
  expr_parts <- character()
  summary_names <- names(summaries)

  for (i in seq_along(summaries)) {
    spec <- summaries[[i]]
    new_name <- summary_names[i]
    func <- spec$func
    col <- spec$col

    # Skip entries with empty or NA names
    if (is.null(new_name) || is.na(new_name) || new_name == "") {
      next
    }

    # Skip entries with empty or NA functions
    if (is.null(func) || is.na(func) || func == "") {
      next
    }

    # Handle special cases for functions that don't take a column argument
    if (func == "dplyr::n") {
      expr_parts <- c(expr_parts, glue::glue(
        "{backtick_if_needed(new_name)} = {func}()"
      ))
    } else if (func == "dplyr::n_distinct") {
      # n_distinct needs a column - skip if column is empty
      if (is.null(col) || length(col) == 0 || col == "") {
        next
      }
      expr_parts <- c(expr_parts, glue::glue(
        "{backtick_if_needed(new_name)} = {func}({backtick_if_needed(col)})"
      ))
    } else {
      # Regular functions with column argument - skip if column is empty
      if (is.null(col) || length(col) == 0 || col == "") {
        next
      }
      expr_parts <- c(expr_parts, glue::glue(
        "{backtick_if_needed(new_name)} = {func}({backtick_if_needed(col)})"
      ))
    }
  }

  # If no valid expressions after filtering, return empty summarize
  if (length(expr_parts) == 0) {
    return(parse(text = "dplyr::summarize(data)")[[1]])
  }

  # Combine all parts
  summarize_string <- glue::glue_collapse(expr_parts, sep = ", ")

  if (length(by_selection) > 0 && !all(by_selection == "")) {
    by_selection <- paste0("\"", by_selection, "\"", collapse = ", ")
    text <- glue::glue(
      "dplyr::summarize(data, {summarize_string}, .by = c({by_selection}))"
    )
  } else {
    text <- glue::glue("dplyr::summarize(data, {summarize_string})")
  }

  parse(text = text)[[1]]
}

#' Apply summarize no-code operation with validation
#'
#' @param data Input data frame
#' @param summaries Summary specifications to apply
#' @param r_expr_validated Reactive value for validated expression
#' @param r_summaries_validated Reactive value for validated summaries
#' @param by_selection Grouping columns
#' @param session Shiny session object (optional, for showing notifications)
#' @noRd
apply_summarize_nocode <- function(
  data,
  summaries,
  r_expr_validated,
  r_summaries_validated,
  by_selection = character(),
  session = NULL
) {
  if (length(summaries) == 0) {
    expr <- parse_summarize_nocode(list(), character())
    r_expr_validated(expr)
    r_summaries_validated(summaries)
    return()
  }

  # Validation: check if columns exist
  cols_to_check <- sapply(summaries, function(x) x$col)
  # Filter out empty columns and functions that don't need columns
  funcs <- sapply(summaries, function(x) x$func)
  cols_to_check <- cols_to_check[funcs != "dplyr::n"]

  if (length(cols_to_check) > 0) {
    data_cols <- colnames(data)
    missing_cols <- setdiff(cols_to_check, data_cols)

    if (length(missing_cols) > 0) {
      if (!is.null(session)) {
        showNotification(
          sprintf(
            "Column(s) not found in data: %s",
            paste(missing_cols, collapse = ", ")
          ),
          type = "error",
          duration = 5
        )
      }
      return()
    }
  }

  # Check for empty new names
  new_names <- names(summaries)
  if (any(new_names == "" | is.na(new_names))) {
    if (!is.null(session)) {
      showNotification(
        "All new column names must be provided",
        type = "error",
        duration = 5
      )
    }
    return()
  }

  expr <- try(parse_summarize_nocode(summaries, by_selection))

  # Validation
  if (inherits(expr, "try-error")) {
    if (!is.null(session)) {
      showNotification(
        paste("Parse error:", expr),
        type = "error",
        duration = 5
      )
    }
    return()
  }

  # Test the expression - provide data in evaluation environment
  ans <- try(eval(expr, envir = list(data = data)))
  if (inherits(ans, "try-error")) {
    if (!is.null(session)) {
      showNotification(
        paste("Evaluation error:", ans),
        type = "error",
        duration = 5
      )
    }
    return()
  }

  r_expr_validated(expr)
  r_summaries_validated(summaries)
}
