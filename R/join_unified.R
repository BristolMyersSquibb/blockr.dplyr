#' Unified JS-driven join block with non-equi join support
#'
#' A binary block that joins two data frames with a JS-driven UI.
#' Supports all dplyr join types, multiple key pairs with non-equi
#' operators (==, >=, >, <=, <), expression-based join conditions,
#' and configurable column suffixes.
#'
#' @param type Join type (left_join, inner_join, right_join, full_join,
#'   semi_join, anti_join)
#' @param by Column join specification (currently unused in initial state;
#'   configured interactively via JS UI)
#' @param ... Additional arguments forwarded to [new_block()]
#' @return A transform block object of class `join_unified_block`.
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div tagList
#' @importFrom htmltools htmlDependency tags
#' @importFrom glue glue
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dplyr)
#'
#'   serve(
#'     new_board(
#'       blocks = list(
#'         data1 = new_dataset_block(dataset = "band_members"),
#'         data2 = new_dataset_block(dataset = "band_instruments"),
#'         joined = new_join_unified_block()
#'       ),
#'       links = links(
#'         from = c("data1", "data2"),
#'         to = c("joined", "joined"),
#'         input = c("x", "y")
#'       )
#'     )
#'   )
#' }
#' @export
new_join_unified_block <- function(
  type = "left_join",
  by = character(),
  ...
) {
  type <- match.arg(
    type,
    c("left_join", "inner_join", "right_join", "full_join", "semi_join", "anti_join")
  )

  new_transform_block(
    # -- server ---------------------------------------------------------------
    function(id, x, y) {
      moduleServer(id, function(input, output, session) {
        ns <- session$ns

        r_type_rv <- reactiveVal(type)
        r_by_rv <- reactiveVal(by)
        r_expr_text <- reactiveVal(NULL)

        # Send column names from both x and y when either changes
        observe({
          x_data <- x()
          y_data <- y()
          req(x_data, y_data)

          session$sendCustomMessage(
            "join-unified-update-columns",
            list(
              id = ns("join_input"),
              xColumns = colnames(x_data),
              yColumns = colnames(y_data)
            )
          )
        })

        # Receive join config from JS
        observeEvent(input$join_input, {
          payload <- input$join_input
          if (is.null(payload)) return()

          expr_text <- build_join_unified_expr(payload)

          # Validate parse
          parsed <- try(parse(text = expr_text)[[1]], silent = TRUE)
          if (inherits(parsed, "try-error")) {
            shiny::showNotification(
              as.character(parsed),
              type = "error",
              duration = 5
            )
            return()
          }

          r_expr_text(expr_text)
          r_type_rv(payload$joinType %||% type)
          r_by_rv(expr_text)
        })

        list(
          expr = reactive({
            req(r_expr_text())
            parse(text = r_expr_text())[[1]]
          }),
          state = list(
            type = r_type_rv,
            by = r_by_rv
          )
        )
      })
    },
    # -- ui -------------------------------------------------------------------
    function(id) {
      tagList(
        join_unified_dep(),
        css_responsive_grid(),
        css_single_column("join-unified"),
        div(
          class = "block-container join-unified-block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  id = NS(id, "join_input"),
                  class = "join-unified-container"
                )
              )
            )
          )
        )
      )
    },
    allow_empty_state = "by",
    class = "join_unified_block",
    ...
  )
}

#' Build dplyr join expression from JS payload
#'
#' Constructs a character string representing a dplyr join call.
#' Supports equi-joins (using `by = c(...)`) and non-equi joins
#' (using `join_by()`).
#'
#' @param payload List from JS with joinType, keys, exprs, suffixX, suffixY
#' @return Character string with the full dplyr join expression
#' @noRd
build_join_unified_expr <- function(payload) {
  join_type <- payload$joinType %||% "left_join"
  keys <- payload$keys %||% list()
  exprs <- payload$exprs %||% list()
  suffix_x <- payload$suffixX %||% ".x"
  suffix_y <- payload$suffixY %||% ".y"

  # Determine if we have non-equi operators
  has_non_equi <- any(vapply(keys, function(k) {
    !is.null(k$op) && k$op != "=="
  }, logical(1)))

  has_exprs <- length(exprs) > 0 && any(
    nchar(trimws(vapply(exprs, as.character, character(1)))) > 0
  )

  # Suffix arg (not used for semi_join/anti_join)
  needs_suffix <- !join_type %in% c("semi_join", "anti_join")
  suffix_arg <- if (needs_suffix) {
    sprintf(', suffix = c("%s", "%s")', suffix_x, suffix_y)
  } else {
    ""
  }

  if (has_non_equi || has_exprs) {
    # Non-equi: use join_by()
    parts <- character(0)
    for (key in keys) {
      x_col <- key$xCol
      y_col <- key$yCol
      if (is.null(x_col) || is.null(y_col) ||
          nchar(x_col) == 0 || nchar(y_col) == 0) next
      r_op <- switch(
        key$op %||% "==",
        "==" = "==",
        ">=" = ">=",
        ">" = ">",
        "<=" = "<=",
        "<" = "<",
        "=="
      )
      parts <- c(parts, glue::glue("`{x_col}` {r_op} `{y_col}`"))
    }
    for (expr in exprs) {
      expr_str <- trimws(as.character(expr))
      if (nchar(expr_str) > 0) parts <- c(parts, expr_str)
    }

    if (length(parts) == 0) {
      # No valid parts: natural join
      glue::glue("dplyr::{join_type}(x, y{suffix_arg})")
    } else {
      join_by_str <- paste(parts, collapse = ", ")
      glue::glue(
        "dplyr::{join_type}(x, y, dplyr::join_by({join_by_str}){suffix_arg})"
      )
    }
  } else if (length(keys) > 0) {
    # Equi-only: use by = c(x_col = "y_col") format
    by_parts <- character(0)
    for (key in keys) {
      x_col <- key$xCol
      y_col <- key$yCol
      if (is.null(x_col) || is.null(y_col) ||
          nchar(x_col) == 0 || nchar(y_col) == 0) next
      if (x_col == y_col) {
        by_parts <- c(by_parts, sprintf('"%s"', x_col))
      } else {
        by_parts <- c(by_parts, sprintf('`%s` = "%s"', x_col, y_col))
      }
    }
    if (length(by_parts) == 0) {
      # No valid keys: natural join
      glue::glue("dplyr::{join_type}(x, y{suffix_arg})")
    } else {
      by_str <- paste(by_parts, collapse = ", ")
      glue::glue(
        "dplyr::{join_type}(x, y, by = c({by_str}){suffix_arg})"
      )
    }
  } else {
    # No keys: natural join
    glue::glue("dplyr::{join_type}(x, y{suffix_arg})")
  }
}

#' HTML dependency for join-unified
#' @noRd
join_unified_dep <- function() {
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
      name = "join-unified-input",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("assets", package = "blockr.dplyr"),
      script = "js/join-unified-input.js",
      stylesheet = "css/join-unified.css"
    )
  )
}
