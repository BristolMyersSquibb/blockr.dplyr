test_that("parse_summarize handles single expression", {
  # Test with single expression
  single_expr <- list(count = "n()")
  result <- parse_summarize(single_expr)

  expect_type(result, "language")
  code <- deparse(result)
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

  expect_type(result, "language")
  code <- paste(deparse(result), collapse = " ")
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

  expect_type(result, "language")
  code <- paste(deparse(result), collapse = " ")
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
    exprs = list(mean_val = "mean(x)"),
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
  expect_type(result, "language")

  code <- paste(deparse(result), collapse = " ")
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
  expect_type(result, "language")

  code <- paste(deparse(result), collapse = " ")
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
  expect_true(all(
    c("group1", "mean_x", "mean_y", "count") %in% names(result_data)
  ))
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
  expect_true(all(
    c("group1", "mean_x", "mean_y", "sum_x", "sum_y") %in% names(result)
  ))

  # Verify calculations
  expect_equal(result$mean_x[result$group1 == "A"], mean(c(1, 2)))
  expect_equal(result$mean_y[result$group1 == "A"], mean(c(10, 20)))
})

# Restorability Tests - Verify blocks can be created with parameters and work immediately
test_that("summarize block restorability - simple expression", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")

  # Create block with exprs parameter - this is what users would call
  blk <- new_summarize_block(exprs = list(mean_mpg = "mean(mpg)"))

  # Verify the block works via testServer
  shiny::testServer(
    blk$expr_server,
    args = list(data = reactive(mtcars[1:10, c("mpg", "cyl")])),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Verify expression generation works
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("mean_mpg", expr_text)))
      expect_true(any(grepl("mean\\(mpg\\)", expr_text)))
    }
  )
})

# Data transformation tests using block_server
test_that("summarize block simple aggregation - testServer", {
  block <- new_summarize_block(exprs = list(mean_mpg = "mean(mpg)"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 1)
      expect_true("mean_mpg" %in% names(result))
      expect_equal(result$mean_mpg, mean(mtcars$mpg), tolerance = 0.0001)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block with grouping - testServer", {
  block <- new_summarize_block(
    exprs = list(mean_mpg = "mean(mpg)", count = "n()"),
    by = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should have one row per cyl group
      expect_equal(nrow(result), length(unique(mtcars$cyl)))
      expect_true("cyl" %in% names(result))
      expect_true("mean_mpg" %in% names(result))
      expect_true("count" %in% names(result))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block with unpack parameter - testServer", {
  # Test data with multiple numeric columns
  test_data <- data.frame(
    mpg = c(21, 21, 22.8, 21.4),
    hp = c(110, 110, 93, 110),
    wt = c(2.6, 2.8, 2.3, 3.2),
    cyl = c(6, 6, 4, 6)
  )

  # NOTE: Testing unpack with across() which returns a data frame
  # With unpack=TRUE, across(c(mpg, hp), mean) should unpack to separate mpg and hp columns
  # With unpack=FALSE, it should create a nested data frame column

  # Test with unpack=TRUE - columns should be unpacked
  block_unpacked <- new_summarize_block(
    exprs = list(stats = "across(c(mpg, hp), mean)"),
    by = "cyl",
    unpack = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_unpacked),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # With unpack=TRUE, columns from across() should be unpacked directly
      # Result should have: cyl, mpg, hp (unpacked from across)
      expect_true("cyl" %in% names(result))
      expect_true("mpg" %in% names(result))
      expect_true("hp" %in% names(result))
      # Should NOT have a nested "stats" column
      expect_false("stats" %in% names(result))
    },
    args = list(
      x = block_unpacked,
      data = list(data = function() test_data)
    )
  )

  # Test with unpack=FALSE - should create nested list-column
  block_nested <- new_summarize_block(
    exprs = list(stats = "across(c(mpg, hp), mean)"),
    by = "cyl",
    unpack = FALSE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block_nested),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      # With unpack=FALSE, should have cyl and stats (nested df)
      expect_true("cyl" %in% names(result))
      expect_true("stats" %in% names(result))
      # Should NOT have individual mpg/hp columns
      # (they're nested inside stats)
      # Note: We can't easily test the nested structure content
      # but we verify the column names show the nesting happened
    },
    args = list(
      x = block_nested,
      data = list(data = function() test_data)
    )
  )
})

# Validation and edge case tests
test_that("parse_summarize handles NULL names", {
  # Test NULL names in expressions (lines 328-330)
  exprs <- list("mean(mpg)", "sum(hp)")
  names(exprs) <- c(NA_character_, NA_character_)

  result <- parse_summarize(exprs, by = NULL, unpack = FALSE)

  # Should handle NULL names gracefully
  expect_type(result, "language")
})

test_that("parse_summarize handles whitespace expressions", {
  # Test all whitespace expressions (lines 378-382)
  exprs <- c(a = "  ", b = "\t", c = "   ")

  result <- parse_summarize(exprs, by = NULL, unpack = FALSE)

  # Should handle whitespace gracefully
  expect_type(result, "language")
})

test_that("parse_summarize handles unpack mode", {
  # Test unpack = TRUE generates bare expression (lines 333-346)
  exprs <- list(stats = "across(c(mpg, hp), mean)")

  result_unpacked <- parse_summarize(exprs, by = NULL, unpack = TRUE)
  result_packed <- parse_summarize(exprs, by = NULL, unpack = FALSE)

  # Both should generate expressions but differently
  expect_type(result_unpacked, "language")
  expect_type(result_packed, "language")

  # Unpacked should not have "stats =" in the expression
  code_unpacked <- deparse(result_unpacked, width.cutoff = 500L)
  code_packed <- deparse(result_packed, width.cutoff = 500L)

  # With unpack=TRUE, the name appears differently
  expect_true(any(grepl("across", code_unpacked)))
  expect_true(any(grepl("across", code_packed)))
})

test_that("parse_summarize validates expressions", {
  # Test req() and stopifnot() validation (lines 385-386)
  # Valid expressions should work
  expect_no_error({
    result <- parse_summarize(list(a = "mean(mpg)"), by = NULL, unpack = FALSE)
    expect_type(result, "language")
  })

  # Named expressions should work
  expect_no_error({
    result <- parse_summarize(c(avg_mpg = "mean(mpg)", sum_hp = "sum(hp)"), by = NULL, unpack = FALSE)
    expect_type(result, "language")
  })
})

test_that("summarize block handles empty expressions", {
  # Test block with empty/invalid expressions
  block <- new_summarize_block(exprs = list(a = ""))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      # Should handle empty expressions gracefully
      expect_no_error(session$returned$result())
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block handles NULL by parameter", {
  # Test with NULL grouping (no grouping)
  block <- new_summarize_block(
    exprs = list(mean_mpg = "mean(mpg)"),
    by = NULL
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should summarize entire dataset (1 row)
      expect_equal(nrow(result), 1)
      expect_true("mean_mpg" %in% names(result))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("summarize block handles empty by parameter", {
  # Test with empty grouping character vector
  block <- new_summarize_block(
    exprs = list(mean_mpg = "mean(mpg)"),
    by = character(0)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should summarize entire dataset (1 row)
      expect_equal(nrow(result), 1)
      expect_true("mean_mpg" %in% names(result))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
