test_that("summarize block constructor", {
  blk <- new_summarize_block()
  expect_s3_class(blk, c("summarize_block", "transform_block", "block"))
})

test_that("parse_summarize handles single expression", {
  # Test with single expression
  single_expr <- list(count = "n()")
  result <- parse_summarize(single_expr)

  expect_type(result, "expression")
  code <- deparse(result[[1]])
  expect_true(grepl("dplyr::summarize\\(data, count = n\\(\\)", code))
})

test_that("parse_summarize handles multiple expressions", {
  # Test with multiple expressions
  multi_expr <- list(
    mean_mpg = "mean(mpg)",
    sum_hp = "sum(hp)",
    count = "n()"
  )
  result <- parse_summarize(multi_expr)

  expect_type(result, "expression")
  code <- paste(deparse(result[[1]]), collapse = " ")
  # Check that all expressions are included
  expect_true(grepl("dplyr::summarize", code))
  expect_true(grepl("mean_mpg", code))
  expect_true(grepl("sum_hp", code))
  expect_true(grepl("count", code))
})

test_that("parse_summarize handles grouping", {
  # Test with grouping
  expr <- list(mean_mpg = "mean(mpg)")
  result <- parse_summarize(expr, by_selection = c("cyl", "am"))

  expect_type(result, "expression")
  code <- paste(deparse(result[[1]]), collapse = " ")
  expect_true(grepl('\\.by = c\\("cyl", "am"\\)', code))
})

test_that("summarize block creates successfully", {
  # Test block creation
  block1 <- new_summarize_block()
  expect_s3_class(block1, "summarize_block")
  expect_s3_class(block1, "transform_block")
  expect_s3_class(block1, "block")

  # Test with custom expressions
  block2 <- new_summarize_block(
    string = list(mean_val = "mean(x)"),
    by = c("group")
  )
  expect_s3_class(block2, "summarize_block")
})

test_that("summarize block handles execution", {
  library(dplyr)

  # Test single expression execution
  single_expr <- list(mean_mpg = "mean(mpg)")
  expr <- parse_summarize(single_expr)
  data <- mtcars[1:10, c("mpg", "hp", "cyl")]
  result <- eval(expr)

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 1)
  expect_equal(result$mean_mpg, mean(data$mpg))

  # Test multiple expression execution (n() will be converted to dplyr::n())
  multi_expr <- list(
    mean_mpg = "mean(mpg)",
    sum_hp = "sum(hp)",
    count = "n()"
  )
  expr <- parse_summarize(multi_expr)
  result <- eval(expr)

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)
  expect_equal(result$mean_mpg, mean(data$mpg))
  expect_equal(result$sum_hp, sum(data$hp))
  expect_equal(result$count, 10)
})

test_that("summarize block handles grouped execution", {
  library(dplyr)

  # Test with grouping
  expr_list <- list(
    mean_mpg = "mean(mpg)",
    count = "n()"
  )
  expr <- parse_summarize(expr_list, by_selection = "cyl")
  data <- mtcars[1:10, c("mpg", "hp", "cyl")]
  result <- eval(expr)

  # Check that we get one row per cylinder group
  expect_equal(nrow(result), length(unique(data$cyl)))
  expect_true("cyl" %in% names(result))
  expect_true("mean_mpg" %in% names(result))
  expect_true("count" %in% names(result))

  # Verify the calculations are correct for one group
  cyl_4_data <- data[data$cyl == 4, ]
  cyl_4_result <- result[result$cyl == 4, ]
  if (nrow(cyl_4_data) > 0 && nrow(cyl_4_result) > 0) {
    expect_equal(cyl_4_result$mean_mpg, mean(cyl_4_data$mpg))
    expect_equal(cyl_4_result$count, nrow(cyl_4_data))
  }
})

test_that("summarize block handles multiple grouping columns", {
  library(dplyr)

  # Create test data with multiple grouping columns
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(10, 20, 30, 40, 50, 60),
    group1 = c("A", "A", "B", "B", "C", "C"),
    group2 = c("X", "Y", "X", "Y", "X", "Y")
  )

  expr_list <- list(
    mean_x = "mean(x)",
    sum_y = "sum(y)"
  )
  expr <- parse_summarize(expr_list, by_selection = c("group1", "group2"))
  result <- eval(expr)

  # Should have one row per unique combination of group1 and group2
  expect_equal(nrow(result), 6) # All combinations are unique in this case
  expect_true(all(c("group1", "group2", "mean_x", "sum_y") %in% names(result)))
})
