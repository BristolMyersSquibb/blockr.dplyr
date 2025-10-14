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

test_that("parse_summarize handles unnamed expressions", {
  library(dplyr)

  # Define helper function that returns multiple columns
  calc_stats <- function(df) {
    data.frame(
      mean_x = mean(df$x),
      mean_y = mean(df$y)
    )
  }

  # Test unnamed expression with empty string name
  string_unnamed <- "calc_stats(pick(everything()))"
  names(string_unnamed) <- ""

  result <- parse_summarize(string_unnamed, by_selection = "group1")
  expect_type(result, "expression")

  code <- paste(deparse(result[[1]]), collapse = " ")
  # Should NOT have "name =" prefix
  expect_false(grepl("\\w+ = calc_stats", code))
  # Should have the expression directly
  expect_true(grepl("calc_stats\\(pick\\(everything\\(\\)\\)\\)", code))
  expect_true(grepl('\\.by = c\\("group1"\\)', code))
})

test_that("unnamed expressions unpack multi-column results", {
  library(dplyr)

  # Define helper function
  calc_stats <- function(df) {
    data.frame(
      mean_x = mean(df$x),
      mean_y = mean(df$y)
    )
  }

  # Create test data
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(10, 20, 30, 40, 50, 60),
    group1 = c("A", "A", "B", "B", "C", "C")
  )

  # Test unnamed expression (unpacks columns)
  string_unnamed <- "calc_stats(pick(everything()))"
  names(string_unnamed) <- ""
  expr_unnamed <- parse_summarize(string_unnamed, by_selection = "group1")
  result_unnamed <- eval(expr_unnamed)

  # Should have 3 columns: group1, mean_x, mean_y (unpacked)
  expect_equal(ncol(result_unnamed), 3)
  expect_true(all(c("group1", "mean_x", "mean_y") %in% names(result_unnamed)))

  # Test named expression (creates nested df)
  string_named <- c(result = "calc_stats(pick(everything()))")
  expr_named <- parse_summarize(string_named, by_selection = "group1")
  result_named <- eval(expr_named)

  # Should have 2 columns: group1, result (where result is nested df)
  expect_equal(ncol(result_named), 2)
  expect_true(all(c("group1", "result") %in% names(result_named)))
})

test_that("parse_summarize handles mixed named and unnamed expressions", {
  library(dplyr)

  # Define helper function
  calc_means <- function(x, y) {
    data.frame(mean_x = mean(x), mean_y = mean(y))
  }

  # Create mixed expressions: one named, one unnamed
  string_mixed <- c("calc_means(x, y)")
  names(string_mixed) <- c("")
  string_mixed <- c(string_mixed, count = "n()")

  result <- parse_summarize(string_mixed, by_selection = "group1")
  expect_type(result, "expression")

  code <- paste(deparse(result[[1]]), collapse = " ")
  # Should have unnamed expression without "name ="
  expect_true(grepl("calc_means\\(x, y\\)", code))
  # Should have named expression with "name ="
  expect_true(grepl("count = n\\(\\)", code))

  # Test execution
  data <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(10, 20, 30, 40),
    group1 = c("A", "A", "B", "B")
  )

  result_data <- eval(result)

  # Should have 4 columns: group1, mean_x, mean_y (unpacked), count
  expect_equal(ncol(result_data), 4)
  expect_true(all(c("group1", "mean_x", "mean_y", "count") %in% names(result_data)))
})

test_that("unnamed expressions work with column-based helpers", {
  library(dplyr)

  # Helper that takes individual columns (simpler, no pick() needed)
  calc_stats_cols <- function(x, y) {
    data.frame(
      mean_x = mean(x),
      mean_y = mean(y),
      sum_x = sum(x),
      sum_y = sum(y)
    )
  }

  # Create unnamed expression
  string_unnamed <- "calc_stats_cols(x, y)"
  names(string_unnamed) <- ""

  expr <- parse_summarize(string_unnamed, by_selection = "group1")

  # Test execution
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(10, 20, 30, 40, 50, 60),
    group1 = c("A", "A", "B", "B", "C", "C")
  )

  result <- eval(expr)

  # Should have 5 columns: group1 + 4 unpacked columns
  expect_equal(ncol(result), 5)
  expect_true(all(c("group1", "mean_x", "mean_y", "sum_x", "sum_y") %in% names(result)))

  # Verify calculations
  expect_equal(result$mean_x[result$group1 == "A"], mean(c(1, 2)))
  expect_equal(result$mean_y[result$group1 == "A"], mean(c(10, 20)))
})
