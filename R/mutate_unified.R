#' Unified JS-driven mutate block
#'
#' Expression-only mutate block with a unified visual design matching
#' the filter unified block. Each row has a name input and an ACE editor
#' for the R expression. Changes require explicit Enter confirmation.
#'
#' @param exprs Named list of expressions (e.g., `list(mpg2 = "mpg * 2")`)
#' @param ... Additional arguments forwarded to [new_block()]
#' @return A block object for mutate operations
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div tagList
#' @importFrom htmltools htmlDependency tags
#' @importFrom jsonlite toJSON
#' @importFrom glue glue
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_mutate_unified_block(), data = list(data = mtcars))
#' }
#' @export
new_mutate_unified_block <- function(exprs = list(new_col = "1"), ...) {
  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_exprs_rv <- reactiveVal(exprs)
        r_expr_text <- reactiveVal(
          as.character(glue::glue(
            "dplyr::mutate(data, {paste(backtick_if_needed(names(exprs)), '=', unname(exprs), collapse = ', ')})"
          ))
        )

        # Send column names on data change for ACE autocomplete
        observeEvent(data(), {
          d <- data()
          session$sendCustomMessage(
            "mutate-unified-update-columns",
            list(
              id = ns("mutate_input"),
              columns = colnames(d)
            )
          )
        })

        # Send initial rows to JS on first data arrival
        sent_init <- reactiveVal(FALSE)
        observeEvent(data(), {
          if (!sent_init()) {
            sent_init(TRUE)
            init_cols <- lapply(seq_along(exprs), function(i) {
              list(name = names(exprs)[[i]], expr = unname(exprs)[[i]])
            })
            session$sendCustomMessage(
              "mutate-unified-set-rows",
              list(
                id = ns("mutate_input"),
                columns = init_cols
              )
            )
          }
        })

        # Receive payload from JS
        observeEvent(input$mutate_input, {
          payload <- input$mutate_input
          if (is.null(payload)) return()

          columns <- payload$columns
          if (is.null(columns) || length(columns) == 0) {
            r_expr_text("dplyr::mutate(data)")
            r_exprs_rv(list())
            return()
          }

          # Build named list from JS payload
          new_exprs <- list()
          for (col in columns) {
            nm <- trimws(col$name %||% "")
            ex <- trimws(col$expr %||% "")
            if (nchar(nm) > 0 && nchar(ex) > 0) {
              new_exprs[[nm]] <- ex
            }
          }

          if (length(new_exprs) == 0) {
            r_expr_text("dplyr::mutate(data)")
            r_exprs_rv(list())
            return()
          }

          # Use parse_mutate from mutate_expr.R for building expression
          expr_text <- as.character(glue::glue(
            "dplyr::mutate(data, {paste(backtick_if_needed(names(new_exprs)), '=', unname(new_exprs), collapse = ', ')})"
          ))

          # Parse-only validation
          parsed <- try(parse(text = expr_text)[[1]], silent = TRUE)
          if (inherits(parsed, "try-error")) {
            shiny::showNotification(as.character(parsed), type = "error", duration = 5)
            return()
          }

          r_expr_text(expr_text)
          r_exprs_rv(new_exprs)
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
        mutate_unified_dep(),
        css_responsive_grid(),
        css_single_column("mutate"),
        div(
          class = "block-container mutate-block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  id = NS(id, "mutate_input"),
                  class = "mutate-unified-container"
                )
              )
            )
          )
        )
      )
    },
    class = "mutate_unified_block",
    ...
  )
}

#' HTML dependency for mutate-unified
#' @noRd
mutate_unified_dep <- function() {
  ace_dir <- system.file("www", package = "shinyAce")
  jqui_dir <- system.file("www/shared/jqueryui", package = "shiny")

  tagList(
    # jQuery UI
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
      name = "mutate-unified-input",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("assets", package = "blockr.dplyr"),
      script = "js/mutate-unified-input.js",
      stylesheet = "css/mutate-unified.css"
    )
  )
}
