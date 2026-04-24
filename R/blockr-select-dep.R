#' HTML dependency for blockr-select component
#'
#' Exported for reuse by other blockr packages that embed the shared
#' select component (e.g. blockr.dm's table pickers).
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @export
blockr_select_dep <- function() {
  htmltools::tagList(
    blockr_core_js_dep(),
    htmltools::htmlDependency(
      name = "blockr-select-js",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "blockr-select.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-select-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "blockr-select.css"
    )
  )
}
