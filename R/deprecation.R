#' Show deprecation message in Shiny context
#'
#' Internal helper to show deprecation warnings both in console and as Shiny
#' notifications when running in a Shiny app. Used by deprecated block constructors
#' to guide users to the new function names.
#'
#' @param old_name Character, the deprecated function name (e.g., "new_value_filter_block()")
#' @param new_name Character, the replacement function name (e.g., "new_filter_block()")
#' @param version Character, version when deprecated (optional)
#'
#' @return Invisible NULL
#' @noRd
#' @noRd
show_block_deprecation <- function(old_name, new_name, version = NULL) {
  # Build message
  msg <- sprintf(
    "%s is deprecated. Please use %s instead.",
    old_name, new_name
  )

  if (!is.null(version)) {
    msg <- paste0(msg, sprintf(" (deprecated in version %s)", version))
  }

  # Console warning always
  warning(msg, call. = FALSE)

  # Shiny notification if in Shiny context
  if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
    shiny::showNotification(
      ui = shiny::div(
        shiny::tags$strong("Deprecated Block"),
        shiny::tags$br(),
        sprintf("Using deprecated %s", old_name),
        shiny::tags$br(),
        sprintf("Please switch to: %s", new_name)
      ),
      type = "warning",
      duration = NULL,  # Keep visible until user dismisses
      closeButton = TRUE
    )
  }

  invisible(NULL)
}
