#' Utility functions for blockr.dplyr
#'
#' @description
#' Internal utility functions used across multiple blocks in blockr.dplyr
#' @noRd
#' @name utils

#' Clean names from variadic ...args (strips numeric-only names)
#' @noRd
dot_args_names <- function(x) {
  res <- names(x)
  unnamed <- grepl("^[1-9][0-9]*$", res)
  if (all(unnamed)) return(NULL)
  if (any(unnamed)) return(replace(res, unnamed, ""))
  res
}

#' Create a .(name) call for bquote substitution
#' @noRd
as_dot_call <- function(x) {
  call(".", as.name(x))
}
