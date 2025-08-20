test_that("slice block initialization works", {
  block <- new_slice_block()
  expect_s3_class(block, "slice_block")
  expect_s3_class(block, "transform_block")
  
  # Test with custom initial values
  block2 <- new_slice_block(type = "tail", n = 10)
  expect_s3_class(block2, "slice_block")
  
  block3 <- new_slice_block(type = "sample", prop = 0.5, replace = TRUE)
  expect_s3_class(block3, "slice_block")
})

test_that("slice block with different types", {
  # Test head type
  block <- new_slice_block(type = "head", n = 5)
  expect_s3_class(block, "slice_block")
  
  # Test tail type
  block <- new_slice_block(type = "tail", prop = 0.2)
  expect_s3_class(block, "slice_block")
  
  # Test min type
  block <- new_slice_block(type = "min", order_by = "mpg", n = 3)
  expect_s3_class(block, "slice_block")
  
  # Test max type
  block <- new_slice_block(type = "max", order_by = "hp", prop = 0.1)
  expect_s3_class(block, "slice_block")
  
  # Test sample type
  block <- new_slice_block(type = "sample", n = 10, replace = TRUE)
  expect_s3_class(block, "slice_block")
  
  # Test custom type
  block <- new_slice_block(type = "custom", rows = "1:5")
  expect_s3_class(block, "slice_block")
})

test_that("slice block with grouping", {
  block <- new_slice_block(type = "head", n = 2, by = "cyl")
  expect_s3_class(block, "slice_block")
  
  block2 <- new_slice_block(type = "max", order_by = "mpg", n = 1, by = c("cyl", "am"))
  expect_s3_class(block2, "slice_block")
})

test_that("slice block with weights", {
  block <- new_slice_block(type = "sample", n = 5, weight_by = "hp")
  expect_s3_class(block, "slice_block")
})

test_that("slice block with ties", {
  block <- new_slice_block(type = "min", order_by = "mpg", n = 3, with_ties = TRUE)
  expect_s3_class(block, "slice_block")
  
  block2 <- new_slice_block(type = "max", order_by = "hp", n = 2, with_ties = FALSE)
  expect_s3_class(block2, "slice_block")
})

test_that("slice block with negative values", {
  # Negative n for "all but n"
  block <- new_slice_block(type = "head", n = -2)
  expect_s3_class(block, "slice_block")
  
  # Negative prop for "all but prop"
  block2 <- new_slice_block(type = "tail", prop = -0.1)
  expect_s3_class(block2, "slice_block")
})

test_that("slice block with custom row expressions", {
  # Range
  block <- new_slice_block(type = "custom", rows = "1:10")
  expect_s3_class(block, "slice_block")
  
  # Specific rows
  block2 <- new_slice_block(type = "custom", rows = "c(1, 3, 5, 7)")
  expect_s3_class(block2, "slice_block")
  
  # Negative selection
  block3 <- new_slice_block(type = "custom", rows = "-c(1:3)")
  expect_s3_class(block3, "slice_block")
  
  # Complex expression
  block4 <- new_slice_block(type = "custom", rows = "c(1:5, 10:15)")
  expect_s3_class(block4, "slice_block")
})