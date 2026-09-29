# The htmlDependency builders here are memoised (memoise0, R/aaa-memoise.R):
# each takes no data-dependent args and returns the same immutable object for
# the life of the process, but was re-running disk I/O (packageVersion ->
# read.dcf + system.file) on every block construction/render -- ~95% of a js
# block's construction cost. See blockr.viz/dev/block-build-cost-findings.md.
# (js_block_dep is keyed by block name, so it keeps its own keyed cache.)

#' HTML dependency for blockr-core.js (the block protocol)
#'
#' Exported for reuse by other blockr packages that build on the shared
#' JS namespace (e.g. blockr.dm). The namespace, DOM helpers, icons and the
#' shared controls come from [blockr.ui::controls_dep()], which this returns
#' first; blockr-core.js adds the block protocol on top.
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @keywords internal
#' @export
blockr_core_js_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    htmltools::htmlDependency(
      name = "blockr-core-js",
      # Bump the suffix on every blockr-core.js edit (version-pinned cache).
      version = paste0(utils::packageVersion("blockr.dplyr"), ".1"),
      src = system.file("js", package = "blockr.dplyr"),
      script = "blockr-core.js"
    )
  )
})

#' HTML dependency for the shared block styles
#'
#' Exported for reuse by other blockr packages that reuse the shared
#' block-container / row styles. Those are blockr.ui's (in
#' [blockr.ui::controls_dep()]); this adds the `.blockr-popover-*` rules that
#' the older engines in other packages still draw.
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @keywords internal
#' @export
blockr_blocks_css_dep <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    htmltools::htmlDependency(
      name = "blockr-dplyr-popover-css",
      version = utils::packageVersion("blockr.dplyr"),
      src = system.file("css", package = "blockr.dplyr"),
      stylesheet = "blockr-popover.css"
    )
  )
})
