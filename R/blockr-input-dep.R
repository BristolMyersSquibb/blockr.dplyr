#' HTML dependency for blockr-input component
#' @noRd
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
