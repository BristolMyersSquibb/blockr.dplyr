test_that("arrange block constructor", {
  # Test basic constructor
  blk <- new_arrange_block()
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test constructor with single column
  blk <- new_arrange_block("mpg")
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test constructor with multiple columns
  blk <- new_arrange_block(c("mpg", "cyl"))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))
})

test_that("arrange block with character input", {
  # Test single column
  blk <- new_arrange_block("mpg")
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test multiple columns
  blk <- new_arrange_block(c("mpg", "cyl", "hp"))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test with multiple columns (defaults to ascending)
  blk <- new_arrange_block(c("mpg", "cyl"))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))
})

test_that("arrange block with list input", {
  # Test list of arrange specifications
  arrange_specs <- list(
    list(column = "mpg", direction = "desc"),
    list(column = "cyl", direction = "asc"),
    list(column = "hp", direction = "desc")
  )

  blk <- new_arrange_block(arrange_specs)
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))
})

test_that("arrange block with empty input", {
  # Test empty character vector
  blk <- new_arrange_block(character(0))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test empty list
  blk <- new_arrange_block(list())
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # Test NULL
  blk <- new_arrange_block(NULL)
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))
})

test_that("multi arrange module functionality", {
  # Test module server function exists
  expect_true(exists("mod_multi_arrange_server"))
  expect_true(is.function(mod_multi_arrange_server))

  # Test module UI function exists
  expect_true(exists("mod_multi_arrange_ui"))
  expect_true(is.function(mod_multi_arrange_ui))

  # Test example function exists
  expect_true(exists("run_multi_arrange_example"))
  expect_true(is.function(run_multi_arrange_example))
})

test_that("arrange block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Create test data
  test_data <- data.frame(
    a = c(3, 1, 2),
    b = c("x", "y", "z"),
    c = c(10, 30, 20)
  )

  # Test basic arrange block functionality
  blk <- new_arrange_block(c("a", "c"))
  expect_s3_class(blk, c("arrange_block", "transform_block", "block"))

  # The block should be properly structured (internal functions, not exposed)
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
})

test_that("arrange specifications format", {
  # Test ascending specifications
  specs_asc <- list(
    list(column = "mpg", direction = "asc"),
    list(column = "cyl", direction = "asc")
  )

  # Test mixed ascending and descending specifications
  specs_mixed <- list(
    list(column = "mpg", direction = "desc"),
    list(column = "cyl", direction = "asc")
  )

  # Blocks should handle both specification formats
  blk1 <- new_arrange_block(specs_asc)
  expect_s3_class(blk1, c("arrange_block", "transform_block", "block"))

  blk2 <- new_arrange_block(specs_mixed)
  expect_s3_class(blk2, c("arrange_block", "transform_block", "block"))
})
