#' Utility functions for blockr.dplyr
#'
#' @description
#' Internal utility functions used across multiple blocks in blockr.dplyr
#' @keywords internal
#' @name utils

#' Check if a name needs backticks for dplyr operations
#'
#' @param name Character string to check
#' @return Logical indicating if backticks are needed
#' @noRd
#' @examples
#' needs_backticks("normal_name")  # FALSE
#' needs_backticks("2025 Sales")   # TRUE
#' needs_backticks("Product-Name") # TRUE
#' needs_backticks("Total $")      # TRUE
needs_backticks <- function(name) {
  # Empty or NA names always need special handling
  if (is.na(name) || name == "") {
    return(FALSE) # These are handled separately
  }
  # If make.names changes it, it's non-syntactic
  make.names(name) != name
}

#' Wrap name in backticks if needed
#'
#' @param name Character string to potentially wrap
#' @return Name with backticks if needed, unchanged otherwise
#' @noRd
#' @examples
#' backtick_if_needed("normal_name")  # "normal_name"
#' backtick_if_needed("2025 Sales")   # "`2025 Sales`"
#' backtick_if_needed("Product-Name") # "`Product-Name`"
backtick_if_needed <- function(name) {
  if (needs_backticks(name)) {
    sprintf("`%s`", name)
  } else {
    name
  }
}

#' Wrap column names in backticks for autocompletion
#'
#' This function prepares column names for ACE editor autocompletion,
#' wrapping non-syntactic names in backticks so they can be properly
#' used in dplyr expressions.
#'
#' @param column_names Character vector of column names
#' @return Character vector with non-syntactic names wrapped in backticks
#' @noRd
#' @examples
#' backtick_for_completion(c("normal", "2025 Sales", "Product-Name"))
#' # Returns: c("normal", "`2025 Sales`", "`Product-Name`")
backtick_for_completion <- function(column_names) {
  vapply(column_names, backtick_if_needed, character(1), USE.NAMES = FALSE)
}