#' Unified JS-driven filter block (prototype)
#'
#' Auto-detects column types: multi-select for categorical columns,
#' min/max range inputs for numeric columns. Advanced expression mode
#' available via a disclosure toggle at the bottom.
#'
#' @param conditions Initial filter conditions (currently unused in prototype)
#' @param ... Additional arguments forwarded to [new_block()]
#' @return A block object for filter operations
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div tagList
#' @importFrom htmltools htmlDependency tags
#' @importFrom jsonlite toJSON
#' @importFrom glue glue
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_filter_unified_block(), data = list(data = iris))
#' }
#' @export
new_filter_unified_block <- function(exprs = "TRUE", ...) {
  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Store the filter expression as text, parse in the reactive
        r_exprs_rv <- reactiveVal(exprs)
        r_expr_text <- reactiveVal(
          as.character(glue::glue("dplyr::filter(data, {exprs})"))
        )

        # Compute and send column metadata to JS
        observeEvent(data(), {
          d <- data()
          meta <- lapply(colnames(d), function(col) {
            vals <- d[[col]]
            type <- if (is.numeric(vals)) "numeric"
                    else if (is.integer(vals)) "integer"
                    else if (is.logical(vals)) "logical"
                    else "character"
            info <- list(
              name = col,
              type = type,
              hasNA = anyNA(vals)
            )
            if (type %in% c("numeric", "integer")) {
              info$min <- min(vals, na.rm = TRUE)
              info$max <- max(vals, na.rm = TRUE)
            } else {
              uv <- sort(unique(as.character(vals[!is.na(vals)])))
              info$values <- uv
              info$hasEmpty <- any(vals == "", na.rm = TRUE)
            }
            info
          })
          session$sendCustomMessage(
            "filter-unified-update-columns",
            list(
              id = ns("filter_input"),
              columns = meta
            )
          )
        })

        # Receive conditions from JS on Submit
        observeEvent(input$filter_input, {
          payload <- input$filter_input
          if (is.null(payload)) return()

          conds <- payload$conditions
          op <- payload$operator %||% "&"

          if (is.null(conds) || length(conds) == 0) {
            r_expr_text("dplyr::filter(data, TRUE)")
            return()
          }

          expr_text <- build_unified_filter(conds, op)
          # Validate parse
          parsed <- try(parse(text = expr_text)[[1]], silent = TRUE)
          if (inherits(parsed, "try-error")) {
            shiny::showNotification(as.character(parsed), type = "error", duration = 5)
            return()
          }
          r_expr_text(expr_text)
          r_exprs_rv(expr_text)
        })

        list(
          expr = reactive(parse(text = r_expr_text())[[1]]),
          state = list(
            exprs = r_exprs_rv
          )
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    function(id) {
      tagList(
        filter_unified_dep(),
        css_responsive_grid(),
        css_single_column("filter"),
        div(
          class = "block-container filter-block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  tags$p("Filter rows by selecting values or setting ranges. ",
                         "Use expression mode for advanced filters.")
                ),
                div(
                  id = NS(id, "filter_input"),
                  class = "filter-unified-container"
                )
              )
            )
          )
        )
      )
    },
    class = "filter_unified_block",
    ...
  )
}

#' Build dplyr::filter expression from mixed condition types
#' @param conditions List of conditions from JS
#' @param operator Global operator ("&" or "|")
#' @return Character string with the full dplyr::filter expression
#' @noRd
build_unified_filter <- function(conditions, operator = "&") {
  parts <- character(0)

  for (cond in conditions) {
    part <- NULL

    if (cond$type == "values") {
      part <- build_value_part(cond)
    } else if (cond$type == "range") {
      part <- build_range_part(cond)
    } else if (cond$type == "expr") {
      expr_val <- trimws(cond$expr)
      if (nchar(expr_val) > 0) part <- expr_val
    }

    if (!is.null(part)) parts <- c(parts, part)
  }

  if (length(parts) == 0) return("dplyr::filter(data, TRUE)")

  op_str <- if (operator == "|") " | " else " & "
  combined <- paste(parts, collapse = op_str)
  glue::glue("dplyr::filter(data, {combined})")
}

#' Build filter part for a values condition
#' @noRd
build_value_part <- function(cond) {
  column <- cond$column
  values <- unlist(cond$values)
  mode <- cond$mode %||% "include"

  if (is.null(column) || is.null(values) || length(values) == 0) return(NULL)

  has_na <- "<NA>" %in% values
  has_empty <- "<empty>" %in% values
  regular <- values[!values %in% c("<NA>", "<empty>")]

  filter_parts <- character(0)

  if (length(regular) > 0) {
    numeric_vals <- suppressWarnings(as.numeric(regular))
    if (all(!is.na(numeric_vals))) {
      values_str <- paste(numeric_vals, collapse = ", ")
    } else {
      values_str <- paste(sprintf('"%s"', regular), collapse = ", ")
    }

    if (mode == "include") {
      filter_parts <- c(filter_parts, glue::glue("`{column}` %in% c({values_str})"))
    } else {
      filter_parts <- c(filter_parts, glue::glue("!(`{column}` %in% c({values_str}))"))
    }
  }

  if (has_na) {
    if (mode == "include") {
      filter_parts <- c(filter_parts, glue::glue("is.na(`{column}`)"))
    } else {
      filter_parts <- c(filter_parts, glue::glue("!is.na(`{column}`)"))
    }
  }

  if (has_empty) {
    if (mode == "include") {
      filter_parts <- c(filter_parts, glue::glue('`{column}` == ""'))
    } else {
      filter_parts <- c(filter_parts, glue::glue('`{column}` != ""'))
    }
  }

  if (length(filter_parts) == 0) return(NULL)
  if (length(filter_parts) == 1) return(filter_parts)

  combine_op <- if (mode == "include") " | " else " & "
  paste0("(", paste(filter_parts, collapse = combine_op), ")")
}

#' Build filter part for a range condition
#' @noRd
build_range_part <- function(cond) {
  column <- cond$column
  cmin <- cond$min
  cmax <- cond$max
  mode <- cond$mode %||% "include"

  if (is.null(column)) return(NULL)

  parts <- character(0)

  if (!is.null(cmin) && !is.na(cmin)) {
    if (mode == "include") {
      parts <- c(parts, glue::glue("`{column}` >= {cmin}"))
    } else {
      parts <- c(parts, glue::glue("`{column}` < {cmin}"))
    }
  }

  if (!is.null(cmax) && !is.na(cmax)) {
    if (mode == "include") {
      parts <- c(parts, glue::glue("`{column}` <= {cmax}"))
    } else {
      parts <- c(parts, glue::glue("`{column}` > {cmax}"))
    }
  }

  if (length(parts) == 0) return(NULL)

  combine_op <- if (mode == "include") " & " else " | "
  if (length(parts) == 1) return(parts)
  paste0("(", paste(parts, collapse = combine_op), ")")
}

#' HTML dependency for filter-unified
#' @noRd
filter_unified_dep <- function() {
  ace_dir <- system.file("www", package = "shinyAce")

  tagList(
    htmlDependency(
      name = "ace",
      version = utils::packageVersion("shinyAce"),
      src = ace_dir,
      script = c("ace/ace.js", "ace/ext-language_tools.js")
    ),
    htmlDependency(
      name = "filter-unified-input",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("assets", package = "blockr.dplyr"),
      script = "js/filter-unified-input.js",
      stylesheet = "css/filter-unified.css"
    )
  )
}
