#' Utility functions for blockr.dplyr
#'
#' @description
#' Internal utility functions used across multiple blocks in blockr.dplyr
#' @keywords internal
#' @name utils

#' Check if names need backticks for dplyr operations (vectorized)
#'
#' @param names Character vector of names to check
#' @return Logical vector indicating if backticks are needed
#' @noRd
#' @examples
#' needs_backticks(c("normal_name", "2025 Sales", "Product-Name"))
#' # Returns: c(FALSE, TRUE, TRUE)
needs_backticks <- function(names) {
  # Check which names are non-syntactic
  needs_bt <- make.names(names) != names
  # Empty or NA names don't need backticks (handled separately)
  needs_bt[is.na(names) | names == ""] <- FALSE
  needs_bt
}

#' Wrap names in backticks if needed (vectorized)
#'
#' @param names Character vector of names to potentially wrap
#' @return Character vector with non-syntactic names wrapped in backticks
#' @noRd
#' @examples
#' backtick_if_needed(c("normal_name", "2025 Sales", "Product-Name"))
#' # Returns: c("normal_name", "`2025 Sales`", "`Product-Name`")
backtick_if_needed <- function(names) {
  needs_bt <- needs_backticks(names)
  names[needs_bt] <- sprintf("`%s`", names[needs_bt])
  names
}

#' Generate responsive CSS for blockr blocks
#'
#' Creates CSS for responsive grid layout using 'block-' prefix.
#' Can be reused across different blockr packages.
#'
#' @return HTML style tag with responsive CSS
#' @noRd
block_responsive_css <- function() {
  tags$style(HTML(
    "
    .block-container {
      width: 100%;
      margin: 0px;
      padding: 0px;
      padding-bottom: 15px;
    }

    /* One shared grid across the whole form */
    .block-form-grid {
      display: grid;
      gap: 15px;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    }

    /* Flatten wrappers so all controls share the same tracks */
    .block-section,
    .block-section-grid {
      display: contents;
    }

    /* Headings/help span full width */
    .block-section h4,
    .block-help-text {
      grid-column: 1 / -1;
    }

    .block-section h4 {
      margin-top: 5px;
      margin-bottom: 0px;
      font-size: 1.1rem;
      font-weight: 600;
      color: #333;
    }

    .block-section:not(:first-child) {
      margin-top: 20px;
    }

    .block-input-wrapper {
      width: 100%;
    }

    .block-input-wrapper .form-group {
      margin-bottom: 10px;
    }

    .block-help-text {
      margin-top: -10px;  # offset the margin-bottom of the previous element
      padding-top: 0px;
      font-size: 0.875rem;
      color: #666;
    }
    "
  ))
}