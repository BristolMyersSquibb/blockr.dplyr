test_that("register_dplyr_blocks runs without error", {
  register_fn <- get(
    "register_dplyr_blocks",
    envir = asNamespace("blockr.dplyr")
  )
  expect_silent(register_fn())
})

test_that("all expected blocks are registered", {
  blocks <- blockr.core::available_blocks()
  block_ctors <- vapply(
    blocks,
    function(b) attr(b, "ctor_name") %||% "",
    character(1)
  )

  expected <- c(
    "new_select_block", "new_join_block", "new_arrange_block",
    "new_mutate_block", "new_summarize_block", "new_filter_block",
    "new_bind_rows_block", "new_bind_cols_block", "new_rename_block",
    "new_slice_block", "new_pivot_longer_block", "new_pivot_wider_block",
    "new_unite_block", "new_separate_block"
  )

  for (nm in expected) {
    expect_true(nm %in% block_ctors, info = paste(nm, "not found in registry"))
  }
})
