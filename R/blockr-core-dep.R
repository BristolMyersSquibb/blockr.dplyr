#' HTML dependency for blockr-core.js (namespace + shared utilities)
#'
#' Exported for reuse by other blockr packages that build on the shared
#' JS namespace (e.g. blockr.dm).
#'
#' @return An `htmltools::htmlDependency`.
#' @export
blockr_core_js_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-core-js",
    version = utils::packageVersion("blockr.dplyr"),
    src = system.file("js", package = "blockr.dplyr"),
    script = "blockr-core.js"
  )
}

#' HTML dependency for blockr-blocks.css (shared block layout styles)
#'
#' Exported for reuse by other blockr packages that reuse the shared
#' block-container / row / popover styles.
#'
#' @return An `htmltools::htmlDependency`.
#' @export
blockr_blocks_css_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-blocks-css",
    version = utils::packageVersion("blockr.dplyr"),
    src = system.file("css", package = "blockr.dplyr"),
    stylesheet = "blockr-blocks.css"
  )
}
