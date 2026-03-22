#' Unified JS-driven summarize block
#'
#' Combines simple (dropdown) and expression modes in a single interface.
#' Simple mode: pick a function and column from dropdowns.
#' Expression mode: write arbitrary R summarize expressions with ACE editor.
#' Optional `.by` grouping via multi-select below the card.
#'
#' @param exprs Initial expression (currently unused in prototype)
#' @param ... Additional arguments forwarded to [new_block()]
#' @return A block object for summarize operations
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div tagList
#' @importFrom htmltools htmlDependency tags
#' @importFrom jsonlite toJSON
#' @importFrom glue glue
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_summarize_unified_block(), data = list(data = mtcars))
#' }
#' @export
new_summarize_unified_block <- function(exprs = "dplyr::summarize(data)", ...) {
  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_exprs_rv <- reactiveVal(exprs)
        r_expr_text <- reactiveVal(exprs)

        # Send column names to JS on data change
        observeEvent(data(), {
          d <- data()
          session$sendCustomMessage(
            "summarize-unified-update-columns",
            list(
              id = ns("summarize_input"),
              columns = colnames(d)
            )
          )
        })

        # Receive payload from JS on submit
        observeEvent(input$summarize_input, {
          payload <- input$summarize_input
          if (is.null(payload)) return()

          summaries <- payload$summaries
          by_cols <- payload[["by"]]

          if (is.null(summaries) || length(summaries) == 0) {
            r_expr_text("dplyr::summarize(data)")
            return()
          }

          expr_text <- build_unified_summarize(summaries, by_cols)

          # Validate parse
          parsed <- try(parse(text = expr_text)[[1]], silent = TRUE)
          if (inherits(parsed, "try-error")) {
            shiny::showNotification(
              as.character(parsed), type = "error", duration = 5
            )
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
        summarize_unified_dep(),
        css_responsive_grid(),
        css_single_column("summarize"),
        div(
          class = "block-container summarize-block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  id = NS(id, "summarize_input"),
                  class = "summarize-unified-container"
                )
              )
            )
          )
        )
      )
    },
    class = "summarize_unified_block",
    ...
  )
}

#' Build dplyr::summarize expression from mixed summary types
#' @param summaries List of summary specs from JS
#' @param by_cols Character vector of grouping columns (or NULL)
#' @return Character string with the full dplyr::summarize expression
#' @noRd
build_unified_summarize <- function(summaries, by_cols = NULL) {
  # Map short function names to proper R calls
  func_map <- list(
    mean = "mean",
    median = "stats::median",
    sd = "stats::sd",
    min = "min",
    max = "max",
    sum = "sum",
    n = "dplyr::n",
    n_distinct = "dplyr::n_distinct",
    first = "dplyr::first",
    last = "dplyr::last"
  )

  parts <- character(0)

  for (s in summaries) {
    part <- NULL
    name <- trimws(s$name %||% "")
    if (nchar(name) == 0) next

    bt_name <- backtick_if_needed(name)

    if (identical(s$type, "simple")) {
      func_key <- s$func %||% ""
      col <- s$col %||% ""
      if (nchar(func_key) == 0) next

      func_call <- func_map[[func_key]] %||% func_key

      if (func_key == "n") {
        part <- glue::glue("{bt_name} = {func_call}()")
      } else {
        if (nchar(col) == 0) next
        bt_col <- backtick_if_needed(col)
        part <- glue::glue("{bt_name} = {func_call}({bt_col})")
      }
    } else if (identical(s$type, "expr")) {
      expr_val <- trimws(s$expr %||% "")
      if (nchar(expr_val) > 0) {
        part <- glue::glue("{bt_name} = {expr_val}")
      }
    }

    if (!is.null(part)) parts <- c(parts, part)
  }

  if (length(parts) == 0) return("dplyr::summarize(data)")

  summarize_string <- paste(parts, collapse = ", ")

  if (!is.null(by_cols) && length(by_cols) > 0 &&
      !all(by_cols == "")) {
    by_str <- paste0('"', by_cols, '"', collapse = ", ")
    text <- glue::glue(
      "dplyr::summarize(data, {summarize_string}, .by = c({by_str}))"
    )
  } else {
    text <- glue::glue("dplyr::summarize(data, {summarize_string})")
  }

  as.character(text)
}

#' HTML dependency for summarize-unified
#' @noRd
summarize_unified_dep <- function() {
  ace_dir <- system.file("www", package = "shinyAce")
  jqui_dir <- system.file("www/shared/jqueryui", package = "shiny")

  tagList(
    # jQuery UI (for selectize drag_drop plugin)
    htmlDependency(
      name = "jqueryui",
      version = utils::packageVersion("shiny"),
      src = jqui_dir,
      script = "jquery-ui.min.js"
    ),
    # ACE editor
    htmlDependency(
      name = "ace",
      version = utils::packageVersion("shinyAce"),
      src = ace_dir,
      script = c("ace/ace.js", "ace/ext-language_tools.js")
    ),
    # Our input binding + styles
    htmlDependency(
      name = "summarize-unified-input",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("assets", package = "blockr.dplyr"),
      script = "js/summarize-unified-input.js",
      stylesheet = "css/summarize-unified.css"
    )
  )
}
