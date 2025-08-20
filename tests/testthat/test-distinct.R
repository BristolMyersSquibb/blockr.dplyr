test_that("distinct block basic functionality works", {
  # Test basic block creation
  block <- new_distinct_block(columns = character(), .keep_all = TRUE)
  expect_true(inherits(block, "distinct_block"))
  
  # Test that block has required components
  expect_true("expr_server" %in% names(block))
  expect_true("expr_ui" %in% names(block))
})

test_that("distinct block UI renders correctly", {
  block <- new_distinct_block()
  layout <- attr(block, "layout")("test")
  
  # Check that layout returns a UI element
  expect_s3_class(layout, "shiny.tag")
})

test_that("distinct block integrates with blockr.core", {
  # Test that block can be created with blockr.core conventions
  block <- new_distinct_block(columns = "category", .keep_all = TRUE)
  
  # Verify block properties
  expect_true(inherits(block, "distinct_block"))
  expect_true("expr_server" %in% names(block))
  expect_true("expr_ui" %in% names(block))
  
  # Test attributes
  expect_equal(attr(block, "submit"), "none")
  expect_equal(attr(block, "ctor"), "new_distinct_block")
})

test_that("distinct block can be created with different parameters", {
  # Test with empty columns
  block1 <- new_distinct_block()
  expect_true(inherits(block1, "distinct_block"))
  
  # Test with specific columns
  block2 <- new_distinct_block(columns = c("a", "b"), .keep_all = FALSE)
  expect_true(inherits(block2, "distinct_block"))
  
  # Test with keep_all = TRUE
  block3 <- new_distinct_block(columns = "category", .keep_all = TRUE)
  expect_true(inherits(block3, "distinct_block"))
})

test_that("distinct block is registered correctly", {
  skip_if_not_installed("blockr.core")
  
  # This test verifies the block is properly registered in the system
  block <- new_distinct_block()
  expect_equal(attr(block, "name"), "Distinct block")
})