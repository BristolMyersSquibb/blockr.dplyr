#' HTML dependency for Blockr.Pick
#'
#' `Blockr.Pick` is a multi-select that shows each pick once: the field shows
#' the picks as text, the open list has them on top, and the list head has All
#' and None. It takes the same config and returns the same handle as
#' `Blockr.Select.multi()` and the multi form of `Blockr.Select.menu()`. It is
#' on trial here before it moves to blockr.ui.
#'
#' @return An `htmltools::tagList` of `htmlDependency` objects.
#' @keywords internal
#' @export
blockr_pick_dep <- memoise0(function() {
  htmltools::tagList(
    blockr_core_js_dep(),
    htmltools::htmlDependency(
      name = "blockr-pick",
      # Bump the suffix on every blockr-pick.js / .css edit (version-pinned
      # cache).
      version = paste0(utils::packageVersion("blockr.dplyr"), ".3"),
      src = system.file(package = "blockr.dplyr"),
      script = "js/blockr-pick.js",
      stylesheet = "css/blockr-pick.css",
      all_files = FALSE
    )
  )
})
