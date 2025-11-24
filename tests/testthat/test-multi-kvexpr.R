test_that("multi_kvexpr handles single expression", {
  # Test parse_mutate with single expression
  single_expr <- list(new_col = "mpg + 1")
  result <- parse_mutate(single_expr)

  expect_type(result, "language")
  code <- deparse(result)
  expect_true(grepl("dplyr::mutate\\(data, new_col = mpg \\+ 1\\)", code))
})

test_that("multi_kvexpr handles multiple expressions", {
  # Test parse_mutate with multiple expressions
  multi_expr <- list(
    col1 = "mpg * 2",
    col2 = "hp / 100",
    col3 = "wt + 1"
  )
  result <- parse_mutate(multi_expr)

  expect_type(result, "language")
  code <- paste(deparse(result), collapse = " ")
  # Check that all expressions are included
  expect_true(grepl("dplyr::mutate", code))
  expect_true(grepl("col1", code))
  expect_true(grepl("col2", code))
  expect_true(grepl("col3", code))
})

test_that("mutate block creates successfully", {
  # Test block creation
  block1 <- new_mutate_expr_block()
  expect_s3_class(block1, "mutate_expr_block")
  expect_s3_class(block1, "transform_block")
  expect_s3_class(block1, "block")

  # Test with custom expressions
  block2 <- new_mutate_expr_block(exprs = list(test_col = "1 + 1"))
  expect_s3_class(block2, "mutate_expr_block")
})

test_that("mutate block handles execution", {
  library(dplyr)

  # Test single expression execution
  single_expr <- list(double_mpg = "mpg * 2")
  expr <- parse_mutate(single_expr)
  data <- mtcars[1:3, c("mpg", "hp")]
  result <- eval(expr)

  expect_equal(ncol(result), 3) # Original 2 + 1 new
  expect_equal(result$double_mpg, data$mpg * 2)

  # Test multiple expression execution
  multi_expr <- list(
    double_mpg = "mpg * 2",
    hp_per_100 = "hp / 100"
  )
  expr <- parse_mutate(multi_expr)
  result <- eval(expr)

  expect_equal(ncol(result), 4) # Original 2 + 2 new
  expect_equal(result$double_mpg, data$mpg * 2)
  expect_equal(result$hp_per_100, data$hp / 100)
})

# Module tests for multi_kvexpr coverage - testing initialization paths
test_that("mod_multi_kvexpr_server initializes without error from named vector", {
  # Test named vector to list conversion (lines 22-26)
  expect_no_error({
    testServer(mod_multi_kvexpr_server, args = list(
      get_value = function() c(col1 = "mpg * 2", col2 = "hp / 100"),
      get_cols = function() c("mpg", "hp", "wt")
    ), {
      session$flushReact()
      result <- session$returned()
      # Module should initialize and return a list
      expect_type(result, "list")
    })
  })
})

test_that("mod_multi_kvexpr_server initializes with empty list", {
  # Test empty initialization fallback (lines 27-29)
  expect_no_error({
    testServer(mod_multi_kvexpr_server, args = list(
      get_value = function() list(),
      get_cols = function() c("mpg", "hp", "wt")
    ), {
      session$flushReact()
      result <- session$returned()
      # Should fallback to default
      expect_type(result, "list")
      expect_length(result, 1)
    })
  })
})

test_that("mod_multi_kvexpr_server initializes with NULL", {
  # Test NULL initialization (lines 27-29)
  expect_no_error({
    testServer(mod_multi_kvexpr_server, args = list(
      get_value = function() NULL,
      get_cols = function() c("mpg", "hp", "wt")
    ), {
      session$flushReact()
      result <- session$returned()
      # Should fallback to default
      expect_type(result, "list")
      expect_length(result, 1)
    })
  })
})

test_that("mod_multi_kvexpr_server handles empty name expressions", {
  # Test empty name handling (lines 62-69)
  empty_name_list <- list("across(everything(), mean)")
  names(empty_name_list) <- ""

  expect_no_error({
    testServer(mod_multi_kvexpr_server, args = list(
      get_value = function() empty_name_list,
      get_cols = function() c("mpg", "hp", "wt")
    ), {
      session$flushReact()
      result <- session$returned()
      # Should allow empty names for unpacking expressions
      expect_type(result, "list")
    })
  })
})

test_that("mod_multi_kvexpr_server tests add functionality", {
  # Test add operation setup (lines 81-101)
  expect_no_error({
    testServer(mod_multi_kvexpr_server, args = list(
      get_value = function() list(col1 = "1"),
      get_cols = function() c("mpg", "hp", "wt")
    ), {
      session$flushReact()

      # Simulate clicking add button
      session$setInputs(add_expr = 1)
      session$flushReact()

      # Module should handle add without error
      result <- session$returned()
      expect_type(result, "list")
    })
  })
})
