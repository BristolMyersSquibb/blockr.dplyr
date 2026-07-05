#' Utility functions for blockr.dplyr
#'
#' @description
#' Internal utility functions used across multiple blocks in blockr.dplyr
#' @noRd
#' @name utils

#' Eval-env reference names for variadic ...args
#'
#' Mirrors blockr.core's (unexported) `dot_arg_refs()`: a variadic block server
#' receives `...args` as a `reactives` object whose unnamed slots (added by
#' dragging an edge in the DAG UI) have no display name, so `names(...args)`
#' returns "" for them, or NULL when every slot is unnamed. Each slot is bound
#' in the eval environment under a symbol — the link name for named slots,
#' `.arg1`, `.arg2`, ... for unnamed ones. Returns those symbols (values) keyed
#' by display name; this is what `as_dot_call()` must reference so an unnamed
#' DAG-UI input is not silently dropped. Keep in sync with blockr.core
#' R/utils-misc.R.
#' @noRd
dot_sym <- function(i) {
  paste0(".arg", i)
}

arg_refs <- function(nms) {
  unnamed <- !nzchar(nms)
  replace(nms, unnamed, dot_sym(seq_len(sum(unnamed))))
}

dot_arg_refs <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    nms <- character(length(x))
  }
  setNames(arg_refs(nms), nms)
}

#' Create a .(name) call for bquote substitution
#' @noRd
as_dot_call <- function(x) {
  call(".", as.name(x))
}
