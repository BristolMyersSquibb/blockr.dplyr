#' HTML dependency for blockr-input component
#' @noRd
blockr_input_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-input",
    version = utils::packageVersion("blockr.dplyr"),
    src = system.file("assets", package = "blockr.dplyr"),
    script = "js/blockr-input.js",
    stylesheet = "css/blockr-input.css"
  )
}
