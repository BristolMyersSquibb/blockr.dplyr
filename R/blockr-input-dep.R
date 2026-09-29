#' HTML dependency for blockr-input component (code autocomplete)
#'
#' Exported for reuse by other blockr packages that embed the shared
#' expression-input component. `Blockr.Input` lives in blockr.ui; this returns
#' [blockr.ui::controls_dep()] together with blockr-core.js.
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @keywords internal
#' @export
blockr_input_dep <- memoise0(function() {
  blockr_core_js_dep()
})
