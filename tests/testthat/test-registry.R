test_that("register_dplyr_blocks runs without error", {
  # Simply verify that the registry function can be called
  register_fn <- get("register_dplyr_blocks", envir = asNamespace("blockr.dplyr"))
  expect_silent(register_fn())
})
