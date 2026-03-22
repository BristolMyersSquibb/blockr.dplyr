#' HTML dependency for blockr-select component
#' @noRd
blockr_select_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-select",
    version = utils::packageVersion("blockr.dplyr"),
    src = system.file("assets", package = "blockr.dplyr"),
    script = "js/blockr-select.js",
    stylesheet = "css/blockr-select.css"
  )
}
