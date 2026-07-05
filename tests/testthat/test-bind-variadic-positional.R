# Regression: connecting a block to bind_rows / bind_cols by dragging an edge in
# the DAG UI adds an *unnamed* link, which a live board stores as a positional
# slot in the `...args` reactives object. `names()` is then NULL, which used to
# collapse arg_names() to NULL -> `dplyr::bind_rows()` / `dplyr::bind_cols()`
# with no arguments (an empty tibble), silently dropping the connected input.
# The fix references the slot as `.arg1` (the symbol core binds an unnamed input
# under). reactiveValues() can only hold *named* slots, so build the positional
# `reactives` object the live board actually produces.
positional_args <- function(...) {
  fns <- lapply(list(...), function(v) function() v)
  shiny::isolate({
    ra <- blockr.core:::reactives()
    for (fn in fns) blockr.core:::append_reactive(ra, fn)
    ra
  })
}

test_that("bind_rows_block references an unnamed (DAG-UI) slot as .arg1", {
  blk <- new_bind_rows_block()
  args_obj <- positional_args(mtcars[1:3, 1:2])

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(...args = args_obj)),
    {
      session$flushReact()
      code <- paste(deparse(session$returned$expr()), collapse = " ")
      # The unnamed input must appear as a `.()`-wrapped .arg1 reference, not be
      # dropped into an argument-less bind_rows().
      expect_match(code, "bind_rows")
      expect_match(code, "\\.arg1")
    }
  )
})

test_that("bind_cols_block references an unnamed (DAG-UI) slot as .arg1", {
  blk <- new_bind_cols_block()
  args_obj <- positional_args(mtcars[1:3, 1:2])

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(...args = args_obj)),
    {
      session$flushReact()
      code <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_match(code, "bind_cols")
      expect_match(code, "\\.arg1")
    }
  )
})
