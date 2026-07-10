# Process-level cache for the htmlDependency builders below. Each takes no
# data-dependent args (js_block_dep is keyed by block name) and returns the same
# object for the life of the R process, yet was re-running disk I/O (read.dcf via
# packageVersion + system.file) on every block construction/render -- ~95% of a
# js block's construction cost (see blockr.viz/dev/block-build-cost-findings.md).
# htmlDependency objects are immutable value lists htmltools already shares
# across sessions, so a process-level cache is correct. Called at RUNTIME, so
# collation order does not matter.
.dep_cache <- new.env(parent = emptyenv())

#' @noRd
dep_cached <- function(key, build) {
  if (!exists(key, envir = .dep_cache, inherits = FALSE)) {
    assign(key, build(), envir = .dep_cache)
  }
  get(key, envir = .dep_cache, inherits = FALSE)
}

#' HTML dependency for blockr-core.js (namespace + shared utilities)
#'
#' Exported for reuse by other blockr packages that build on the shared
#' JS namespace (e.g. blockr.dm).
#'
#' @return An `htmltools::htmlDependency`.
#' @keywords internal
#' @export
blockr_core_js_dep <- function() dep_cached("blockr_core_js_dep", function() {
  htmltools::htmlDependency(
    name = "blockr-core-js",
    # Bump the suffix on every blockr-core.js edit (version-pinned cache).
    version = paste0(utils::packageVersion("blockr.dplyr"), ".1"),
    src = system.file("js", package = "blockr.dplyr"),
    script = "blockr-core.js"
  )
})

#' HTML dependency for the settings band + checkbox assets
#'
#' Vendored verbatim from blockr.viz (the canonical source until the shared
#' layer moves to blockr.ui): the in-flow gear settings band CSS and the
#' `Blockr.checkbox` factory. Distinct dependency name per package so a
#' stale copy can never shadow a fresh one on a mixed dashboard.
#'
#' @return An `htmltools::htmlDependency`.
#' @noRd
settings_band_dep <- function() dep_cached("settings_band_dep", function() {
  htmltools::htmlDependency(
    name = "blockr-dplyr-settings-band",
    # Bump the suffix on every settings-band.css/js edit (asset cache).
    version = paste0(utils::packageVersion("blockr.dplyr"), ".1"),
    src = system.file(package = "blockr.dplyr"),
    script = "js/settings-band.js",
    stylesheet = "css/settings-band.css"
  )
})

#' HTML dependency for blockr-blocks.css (shared block layout styles)
#'
#' Exported for reuse by other blockr packages that reuse the shared
#' block-container / row / popover styles.
#'
#' @return An `htmltools::htmlDependency`.
#' @keywords internal
#' @export
blockr_blocks_css_dep <- function() dep_cached("blockr_blocks_css_dep", function() {
  htmltools::htmlDependency(
    name = "blockr-blocks-css",
    # Bump the suffix on every blockr-blocks.css edit (version-pinned cache).
    version = paste0(utils::packageVersion("blockr.dplyr"), ".1"),
    src = system.file("css", package = "blockr.dplyr"),
    stylesheet = "blockr-blocks.css"
  )
})
