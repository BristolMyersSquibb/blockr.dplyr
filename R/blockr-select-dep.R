#' HTML dependency for blockr-select component
#'
#' Exported for reuse by other blockr packages that embed the shared
#' select component (e.g. blockr.dm's table pickers). `Blockr.Select` lives in
#' blockr.ui; this returns [blockr.ui::controls_dep()] together with
#' blockr-core.js.
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @keywords internal
#' @export
blockr_select_dep <- memoise0(function() {
  blockr_core_js_dep()
})
