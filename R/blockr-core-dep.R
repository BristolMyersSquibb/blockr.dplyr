#' HTML dependency for blockr-core.js (namespace + shared utilities)
#'
#' Exported for reuse by other blockr packages that build on the shared
#' JS namespace (e.g. blockr.dm).
#'
#' @return An `htmltools::htmlDependency`.
#' @keywords internal
#' @export
blockr_core_js_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-core-js",
    version = utils::packageVersion("blockr.dplyr"),
    src = system.file("js", package = "blockr.dplyr"),
    script = "blockr-core.js"
  )
}

#' HTML dependency for the settings band + checkbox assets
#'
#' Vendored verbatim from blockr.viz (the canonical source until the shared
#' layer moves to blockr.ui): the in-flow gear settings band CSS and the
#' `Blockr.checkbox` factory. Distinct dependency name per package so a
#' stale copy can never shadow a fresh one on a mixed dashboard.
#'
#' @return An `htmltools::htmlDependency`.
#' @noRd
settings_band_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-dplyr-settings-band",
    # Bump the suffix on every settings-band.css/js edit (asset cache).
    version = paste0(utils::packageVersion("blockr.dplyr"), ".1"),
    src = system.file(package = "blockr.dplyr"),
    script = "js/settings-band.js",
    stylesheet = "css/settings-band.css"
  )
}

#' HTML dependency for blockr-blocks.css (shared block layout styles)
#'
#' Exported for reuse by other blockr packages that reuse the shared
#' block-container / row / popover styles.
#'
#' @return An `htmltools::htmlDependency`.
#' @keywords internal
#' @export
blockr_blocks_css_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-blocks-css",
    version = utils::packageVersion("blockr.dplyr"),
    src = system.file("css", package = "blockr.dplyr"),
    stylesheet = "blockr-blocks.css"
  )
}
