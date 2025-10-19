#' Create a documentation link for blocks
#'
#' Creates a styled external link that opens block documentation in a new tab.
#' Used to provide easy access to reference documentation and guides from within blocks.
#'
#' @param text Character string for the link text
#' @param url Character string for the documentation URL
#' @param icon_name Character string for Font Awesome icon name (default: "arrow-up-right-from-square")
#' @param tooltip Optional character string for hover tooltip
#' @param class Additional CSS classes to add
#'
#' @return An HTML link element with icon
#' @importFrom shiny icon tags
#' @keywords internal
#' @examples
#' \dontrun{
#' # Basic usage
#' doc_link(
#'   "Documentation",
#'   "https://bristolmyerssquibb.github.io/blockr.dplyr/reference/new_mutate_block.html"
#' )
#'
#' # With tooltip
#' doc_link(
#'   "Expression helpers",
#'   "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/expression-helpers.html",
#'   tooltip = "Learn about common functions"
#' )
#' }
doc_link <- function(
  text,
  url,
  icon_name = "arrow-up-right-from-square",
  tooltip = NULL,
  class = ""
) {
  link <- tags$a(
    href = url,
    target = "_blank",
    rel = "noopener noreferrer",  # Security best practice
    class = paste("block-doc-link", class),
    span(text, class = "link-text"),
    " ",
    icon(icon_name, class = "icon-sm"),
    style = "text-decoration: none; color: #6c757d; font-size: 0.875rem; display: inline-flex; align-items: center; gap: 0.25rem; transition: color 0.2s ease;"
  )

  # Add tooltip if provided
  if (!is.null(tooltip)) {
    link <- bslib::tooltip(link, tooltip, placement = "top")
  }

  link
}

#' Create inline CSS for documentation links
#'
#' Returns CSS styling for documentation links used in blocks.
#' Call this once in the block UI to ensure proper styling.
#'
#' @return HTML style tag with CSS
#' @importFrom shiny tags HTML
#' @keywords internal
css_doc_links <- function() {
  tags$style(HTML("
    .block-doc-link:hover {
      color: #0d6efd !important;
    }

    .block-doc-link .icon-sm {
      font-size: 0.75rem;
    }

    .block-header-with-doc {
      display: flex;
      justify-content: flex-end;
      align-items: center;
      margin-bottom: 0.5rem;
      padding: 0.5rem 0;
    }

    .block-header-title {
      font-weight: 600;
      font-size: 1rem;
    }

    .expression-help-link {
      margin-top: 0.25rem;
      margin-bottom: 0.5rem;
      display: block;
    }
  "))
}
