test_that("join block constructor", {
  blk <- new_join_block()
  expect_s3_class(blk, c("join_block", "transform_block", "block"))
})

