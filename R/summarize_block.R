#' Get available summary functions for the summarize block
#'
#' Returns the list of summary functions available in simple mode.
#' Merges built-in defaults with any custom functions registered via
#' the `blockr.dplyr.summary_functions` option.
#'
#' @section Extending with custom functions:
#' Set the `blockr.dplyr.summary_functions` option to a named character vector
#' where names are display labels and values are namespaced function calls:
#'
#' \preformatted{
#' options(
#'   blockr.dplyr.summary_functions = c(
#'     "extract parentheses (paren_num)" = "blockr.topline::paren_num",
#'     "first number (first_num)" = "blockr.topline::first_num"
#'   )
#' )
#' }
#'
#' @return A named character vector where names are short function names
#'   (for display) and values are fully qualified function calls.
#' @noRd
get_summary_functions <- function() {
  default_funcs <- c(
    "mean" = "mean",
    "median" = "stats::median",
    "sd" = "stats::sd",
    "min" = "min",
    "max" = "max",
    "sum" = "sum",
    "n" = "dplyr::n",
    "n_distinct" = "dplyr::n_distinct",
    "first" = "dplyr::first",
    "last" = "dplyr::last"
  )

  custom_funcs <- blockr_option("dplyr.summary_functions", NULL)

  if (is.null(custom_funcs) || !is.character(custom_funcs)) {
    return(default_funcs)
  }

  # Extract short names from namespaced values (e.g. "blockr.topline::paren_num" -> "paren_num")
  short_names <- sub(".*::", "", custom_funcs)

  # Use user-provided labels if available, otherwise use short names
  if (!is.null(names(custom_funcs))) {
    labels <- ifelse(
      names(custom_funcs) == "" | is.na(names(custom_funcs)),
      short_names,
      names(custom_funcs)
    )
  } else {
    labels <- short_names
  }

  custom <- stats::setNames(custom_funcs, labels)

  c(default_funcs, custom[!custom %in% default_funcs])
}

#' Summarize block (JS-driven)
#'
#' JS-driven summarize block with two row types: simple (function + column)
#' and expression (free R code). Includes a group-by section for grouped
#' summaries.
#'
#' @section Extending available functions:
#' The list of available summary functions can be extended using the
#' \code{blockr.dplyr.summary_functions} option. Set this option to a named
#' character vector where names are display labels and values are function
#' calls with proper namespacing:
#'
#' \preformatted{
#' options(
#'   blockr.dplyr.summary_functions = c(
#'     "extract parentheses (paren_num)" = "blockr.topline::paren_num"
#'   )
#' )
#' }
#'
#' @param summaries Array of summary objects. Summary types:
#'   - `simple`: name, func (e.g. "mean"), col (column name)
#'   - `expr`: name, expr (R expression string)
#' @param by Character vector (or array) of grouping columns for grouped
#'   summaries.
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_summarize_block(
#'       summaries = list(
#'         list(type = "simple", name = "avg_sl", func = "mean",
#'              col = "Sepal.Length")
#'       ),
#'       by = list("Species")
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#'
#' @export
new_summarize_block <- function(
  summaries = list(),
  by = list(),
  ...
) {
  new_js_transform_block(
    class = "summarize_block",
    name = "summarize",
    state = list(
      summaries = summaries,
      by = by
    ),
    expr_fn = function(s) {
      make_summarize_expr(
        s$summaries %||% list(),
        s$by %||% character()
      )
    },
    setup = function(input, session, ns, data, input_name) {
      # Send available summary functions to JS on init
      all_funcs <- get_summary_functions()
      func_info <- lapply(names(all_funcs), function(label) {
        list(value = unname(all_funcs[[label]]), label = label)
      })
      session$sendCustomMessage(
        "summarize-functions",
        list(id = ns(input_name), functions = func_info)
      )
    },
    shared_deps = c("select", "input"),
    ...
  )
}
