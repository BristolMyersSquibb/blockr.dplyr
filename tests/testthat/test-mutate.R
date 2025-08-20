test_that("mutate block constructor", {
  blk <- new_mutate_block()
  expect_s3_class(blk, c("mutate_block", "transform_block", "block"))
})