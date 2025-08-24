test_that("mutate block constructor", {
  blk <- new_mutate_block()
  expect_s3_class(blk, c("mutate_block", "transform_block", "block"))
})

test_that("mutate block constructor with by parameter", {
  blk <- new_mutate_block(by = c("cyl", "am"))
  expect_s3_class(blk, c("mutate_block", "transform_block", "block"))
})

test_that("parse_mutate handles .by parameter", {
  # Test without grouping
  expr1 <- parse_mutate(c(new_col = "mpg * 2"))
  expect_type(expr1, "expression")
  code1 <- deparse(expr1)
  expect_true(grepl("dplyr::mutate\\(data, new_col = mpg \\* 2\\)", code1))
  expect_false(grepl("\\.by", code1))

  # Test with grouping
  expr2 <- parse_mutate(c(avg_mpg = "mean(mpg)"), c("cyl"))
  expect_type(expr2, "expression")
  code2 <- deparse(expr2)
  expect_true(grepl("dplyr::mutate.*\\.by = c\\(\"cyl\"\\)", code2))

  # Test with multiple grouping columns
  expr3 <- parse_mutate(c(avg_mpg = "mean(mpg)"), c("cyl", "am"))
  expect_type(expr3, "expression")
  code3 <- deparse(expr3)
  expect_true(grepl('\\.by = c\\("cyl".*"am"\\)', paste(code3, collapse = " ")))
})

test_that("mutate block with .by executes correctly", {
  library(dplyr)

  # Test grouped mutate
  expr <- parse_mutate(c(avg_mpg = "mean(mpg)"), c("cyl"))
  data <- mtcars[1:10, c("mpg", "cyl", "hp")]
  result <- eval(expr)

  # Should have original columns plus new column
  expect_equal(ncol(result), 4) # mpg, cyl, hp, avg_mpg
  expect_true("avg_mpg" %in% names(result))

  # Check that grouping worked correctly
  # avg_mpg should be the same for all rows with same cyl value
  unique_cyls <- unique(result$cyl)
  for (cyl_val in unique_cyls) {
    cyl_rows <- result[result$cyl == cyl_val, ]
    expect_true(all(cyl_rows$avg_mpg == cyl_rows$avg_mpg[1]),
      info = paste("avg_mpg should be consistent within cyl =", cyl_val)
    )
  }
})

test_that("mutate block handles empty .by parameter", {
  # Test with empty character vector
  expr1 <- parse_mutate(c(new_col = "mpg * 2"), character())
  expect_type(expr1, "expression")
  code1 <- deparse(expr1)
  expect_false(grepl("\\.by", code1))

  # Test with empty string in by parameter
  expr2 <- parse_mutate(c(new_col = "mpg * 2"), c(""))
  expect_type(expr2, "expression")
  code2 <- deparse(expr2)
  expect_false(grepl("\\.by", code2))
})
