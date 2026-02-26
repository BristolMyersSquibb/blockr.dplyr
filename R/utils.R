#' Utility functions for blockr.dplyr
#'
#' @description
#' Internal utility functions used across multiple blocks in blockr.dplyr
#' @noRd
#' @name utils

#' Check if names need backticks for dplyr operations (vectorized)
#'
#' @param names Character vector of names to check
#' @return Logical vector indicating if backticks are needed
#' @noRd
needs_backticks <- function(names) {
  # Check which names are non-syntactic
  needs_bt <- make.names(names) != names
  # Empty or NA names don't need backticks (handled separately)
  needs_bt[is.na(names) | names == ""] <- FALSE
  needs_bt
}

#' Ensure a value is a reactiveVal
#'
#' If \code{x} is already a \code{reactiveVal} (e.g., injected by
#' \code{external_ctrl}), return it unchanged.
#' Otherwise, create a new \code{reactiveVal} with \code{default} as its
#' initial value.
#'
#' @param x Value to check.
#' @param default Initial value for the new reactiveVal (defaults to \code{x}).
#' @return A \code{reactiveVal}.
#' @noRd
as_rv <- function(x, default = x) {
  if (inherits(x, "reactiveVal")) x else shiny::reactiveVal(default)
}

#' Wrap names in backticks if needed (vectorized)
#'
#' @param names Character vector of names to potentially wrap
#' @return Character vector with non-syntactic names wrapped in backticks
#' @noRd
backtick_if_needed <- function(names) {
  needs_bt <- needs_backticks(names)
  names[needs_bt] <- sprintf("`%s`", names[needs_bt])
  names
}
