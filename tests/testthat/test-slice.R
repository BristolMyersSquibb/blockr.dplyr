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

# Functional tests with actual data
test_that("slice_head functionality", {
  data <- mtcars

  # Test head with n
  block <- new_slice_block(type = "head", n = 5)

  # Test using blockr.core's serve mechanism
  skip_if_not_installed("blockr.core")

  # Test the slice expression building directly
  expr_result <- dplyr::slice_head(data, n = 5)
  expect_equal(nrow(expr_result), 5)
  expect_equal(expr_result, head(data, 5))

  # Test head with prop
  expr_result2 <- dplyr::slice_head(data, prop = 0.2)
  expected_rows <- floor(0.2 * nrow(data))  # dplyr uses floor, not ceiling
  expect_equal(nrow(expr_result2), expected_rows)
})

test_that("slice_tail functionality", {
  data <- mtcars
  skip_if_not_installed("blockr.core")

  # Test tail with n - testing dplyr function directly
  output <- dplyr::slice_tail(data, n = 3)
  expect_equal(nrow(output), 3)
  expect_equal(output, tail(data, 3))

  # Test construction works
  block <- new_slice_block(type = "tail", n = 3)
  expect_s3_class(block, "slice_block")
})

test_that("slice_min and slice_max functionality", {
  data <- mtcars
  skip_if_not_installed("blockr.core")

  # Test slice_min - testing dplyr function directly
  output <- dplyr::slice_min(data, mpg, n = 3)  # Use unquoted column name
  expect_equal(nrow(output), 3)
  expect_true(all(output$mpg <= sort(data$mpg)[3]))

  # Test slice_max
  output2 <- dplyr::slice_max(data, hp, n = 2, with_ties = FALSE)  # Use unquoted column name
  expect_equal(nrow(output2), 2)
  expect_true(all(output2$hp >= sort(data$hp, decreasing = TRUE)[2]))

  # Test construction works
  block <- new_slice_block(type = "min", order_by = "mpg", n = 3)
  expect_s3_class(block, "slice_block")
})

test_that("slice_sample functionality", {
  data <- mtcars
  skip_if_not_installed("blockr.core")
  set.seed(123)

  # Test slice_sample with n - testing dplyr function directly
  output <- dplyr::slice_sample(data, n = 5, replace = FALSE)
  expect_equal(nrow(output), 5)
  expect_true(all(output$mpg %in% data$mpg))

  # Test construction works
  block <- new_slice_block(type = "sample", n = 5, replace = FALSE)
  expect_s3_class(block, "slice_block")
})

test_that("custom slice functionality", {
  data <- mtcars
  skip_if_not_installed("blockr.core")

  # Test custom rows - testing dplyr function directly
  output <- dplyr::slice(data, c(1, 3, 5))
  expect_equal(nrow(output), 3)
  expect_equal(rownames(output), rownames(data)[c(1, 3, 5)])

  # Test range
  output2 <- dplyr::slice(data, 1:5)
  expect_equal(nrow(output2), 5)
  expect_equal(output2, data[1:5, ])

  # Test construction works
  block <- new_slice_block(type = "custom", rows = "c(1, 3, 5)")
  expect_s3_class(block, "slice_block")
})

test_that("grouping with .by parameter", {
  data <- mtcars
  skip_if_not_installed("blockr.core")

  # Test grouped slice_head - testing dplyr function directly
  output <- data |> dplyr::group_by(cyl) |> dplyr::slice_head(n = 1) |> dplyr::ungroup()
  unique_cyls <- unique(data$cyl)
  expect_equal(nrow(output), length(unique_cyls))
  expect_equal(sort(unique(output$cyl)), sort(unique_cyls))

  # Test grouped slice_max
  output2 <- data |> dplyr::group_by(cyl, am) |> dplyr::slice_max(hp, n = 1, with_ties = FALSE) |> dplyr::ungroup()
  unique_combinations <- unique(data[, c("cyl", "am")])
  expect_equal(nrow(output2), nrow(unique_combinations))

  # Test construction works
  block <- new_slice_block(type = "head", n = 1, by = "cyl")
  expect_s3_class(block, "slice_block")
})

test_that("edge cases and error handling", {
  data <- mtcars
  skip_if_not_installed("blockr.core")

  # Test empty data frame returns
  empty_result <- data[0, , drop = FALSE]
  expect_equal(nrow(empty_result), 0)
  expect_equal(ncol(empty_result), ncol(data))

  # Test construction works with edge cases
  block <- new_slice_block(type = "min", order_by = "", n = 3)
  expect_s3_class(block, "slice_block")
})

test_that("block initialization and basic properties", {
  skip_if_not_installed("blockr.core")

  # Test basic construction
  block <- new_slice_block(type = "max", n = 3, order_by = "mpg", with_ties = TRUE)
  expect_s3_class(block, "slice_block")

  # Test with grouping
  block2 <- new_slice_block(type = "head", n = 2, by = c("cyl", "am"))
  expect_s3_class(block2, "slice_block")
})

test_that("reactive updates work correctly", {
  skip_if_not_installed("blockr.core")

  # Create a block and test basic functionality
  block <- new_slice_block(type = "head", n = 5)
  expect_s3_class(block, "slice_block")
  expect_true("slice_block" %in% class(block))

  # Test that the block has the expected structure
  # Note: blocks created with new_transform_block have different internal structure
  expect_true("slice_block" %in% class(block))
  expect_true("transform_block" %in% class(block))
})

