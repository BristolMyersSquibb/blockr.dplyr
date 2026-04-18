#' HTML dependency for blockr-input component (code autocomplete)
#'
#' Exported for reuse by other blockr packages that embed the shared
#' expression-input component.
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @export
blockr_input_dep <- function() {
  htmltools::tagList(
    blockr_core_js_dep(),
    htmltools::htmlDependency(
      name = "blockr-input-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "blockr-input.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-input-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "blockr-input.css"
    )
  )
}
