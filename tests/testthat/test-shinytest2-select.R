test_that("select block selects single column correctly", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create temporary app
  app_dir <- create_test_app(
    block_code = 'serve(new_select_block(columns = "mpg"), data = list(data = mtcars))'
  )

  # Launch app
  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "select_single"
  )

  # Wait for app to be ready
  app$wait_for_idle()

  # Get exported values
  values <- app$get_values(export = TRUE)

  # The result should be exported via exportTestValues
  result_data <- values$export$result

  # Verify we got a data frame
  expect_true(is.data.frame(result_data))

  # Verify only mpg column is present
  expect_equal(ncol(result_data), 1)
  expect_equal(names(result_data), "mpg")

  # Verify correct number of rows (same as mtcars)
  expect_equal(nrow(result_data), nrow(mtcars))

  # Cleanup
  cleanup_test_app(app_dir, app)
})

test_that("select block selects multiple columns in order", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create app with multiple columns
  app_dir <- create_test_app(
    block_code = 'serve(new_select_block(columns = c("mpg", "cyl", "hp")), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "select_multiple"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify column count and names (should preserve order)
  expect_equal(ncol(result_data), 3)
  expect_equal(names(result_data), c("mpg", "cyl", "hp"))

  # Verify row count unchanged
  expect_equal(nrow(result_data), nrow(mtcars))

  cleanup_test_app(app_dir, app)
})

test_that("select block exclude mode works correctly", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test exclude mode - exclude "gear" and "carb"
  app_dir <- create_test_app(
    block_code = 'serve(new_select_block(columns = c("gear", "carb"), exclude = TRUE), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "select_exclude"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify "gear" and "carb" are NOT in the output
  expect_false("gear" %in% names(result_data))
  expect_false("carb" %in% names(result_data))

  # Verify we have the right number of columns (11 - 2 = 9)
  expect_equal(ncol(result_data), ncol(mtcars) - 2)

  # Verify row count unchanged
  expect_equal(nrow(result_data), nrow(mtcars))

  cleanup_test_app(app_dir, app)
})

test_that("select block with distinct returns unique rows", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test data with duplicates
  test_data_code <- "test_data <- data.frame(
    cyl = c(4, 4, 4, 6, 6, 8),
    gear = c(4, 4, 5, 4, 4, 3),
    mpg = c(21, 21, 22, 18, 18, 15)
  )"

  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_select_block(columns = c("cyl", "gear"), distinct = TRUE), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "select_distinct"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we have only the selected columns
  expect_equal(ncol(result_data), 2)
  expect_equal(names(result_data), c("cyl", "gear"))

  # Verify we have only unique combinations (should be 4 unique rows)
  expect_equal(nrow(result_data), 4)

  # Verify no duplicate rows
  expect_equal(nrow(result_data), nrow(unique(result_data)))

  cleanup_test_app(app_dir, app)
})

test_that("select block with empty selection selects all columns", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Empty selection should select all columns
  app_dir <- create_test_app(
    block_code = 'serve(new_select_block(columns = character(0)), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "select_empty"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify all columns are present
  expect_equal(ncol(result_data), ncol(mtcars))
  expect_equal(names(result_data), names(mtcars))

  # Verify row count unchanged
  expect_equal(nrow(result_data), nrow(mtcars))

  cleanup_test_app(app_dir, app)
})

test_that("select block UI interaction changes output", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Start with one column
  app_dir <- create_test_app(
    block_code = 'serve(new_select_block(columns = "mpg"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "select_interaction"
  )
  app$wait_for_idle()

  # Initial state - only mpg
  values <- app$get_values(export = TRUE)
  result_data <- values$export$result
  expect_equal(names(result_data), "mpg")

  # Change selection to multiple columns via UI
  app$set_inputs(`block-expr-columns` = c("mpg", "cyl", "hp"))
  app$wait_for_idle()

  # Verify output updated
  values <- app$get_values(export = TRUE)
  result_data <- values$export$result
  expect_equal(names(result_data), c("mpg", "cyl", "hp"))

  # Toggle exclude mode
  app$set_inputs(`block-expr-exclude` = TRUE)
  app$wait_for_idle()

  # Now should exclude those columns
  values <- app$get_values(export = TRUE)
  result_data <- values$export$result
  expect_false("mpg" %in% names(result_data))
  expect_false("cyl" %in% names(result_data))
  expect_false("hp" %in% names(result_data))

  cleanup_test_app(app_dir, app)
})

test_that("select block with distinct checkbox toggle", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test data with duplicates
  test_data_code <- "test_data <- data.frame(
    x = c(1, 1, 2, 2, 3),
    y = c('a', 'a', 'b', 'b', 'c')
  )"

  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_select_block(columns = "x", distinct = FALSE), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "select_distinct_toggle"
  )
  app$wait_for_idle()

  # Initial state - distinct = FALSE, should have 5 rows
  values <- app$get_values(export = TRUE)
  result_data <- values$export$result
  expect_equal(nrow(result_data), 5)

  # Toggle distinct = TRUE
  app$set_inputs(`block-expr-distinct` = TRUE)
  app$wait_for_idle()

  # Should now have only unique values (3 rows: 1, 2, 3)
  values <- app$get_values(export = TRUE)
  result_data <- values$export$result
  expect_equal(nrow(result_data), 3)
  expect_equal(sort(result_data$x), c(1, 2, 3))

  cleanup_test_app(app_dir, app)
})

test_that("select block full restorability - all parameters (columns + exclude + distinct)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test all restorable parameters together:
  # - Multiple columns (columns)
  # - Exclude mode (exclude = TRUE)
  # - Distinct rows (distinct = TRUE)
  # This is the comprehensive restorability test

  # Create test data with some duplicate rows after exclusion
  test_data_code <- "test_data <- data.frame(
    mpg = c(21, 21, 22, 22, 23),
    cyl = c(4, 4, 6, 6, 8),
    hp = c(110, 110, 120, 120, 130),
    gear = c(4, 4, 4, 4, 3),
    carb = c(2, 2, 4, 4, 4)
  )"

  # Exclude gear and carb, then get distinct rows
  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_select_block(
      columns = c("gear", "carb"),
      exclude = TRUE,
      distinct = TRUE
    ), data = list(data = test_data))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "select_full_restore"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify columns parameter worked (exclude mode)
  expect_false("gear" %in% names(result_data))
  expect_false("carb" %in% names(result_data))
  expect_true("mpg" %in% names(result_data))
  expect_true("cyl" %in% names(result_data))
  expect_true("hp" %in% names(result_data))

  # Should have 3 columns (mpg, cyl, hp)
  expect_equal(ncol(result_data), 3)

  # Verify distinct parameter worked - should have unique rows only
  # After excluding gear and carb, we have: (21,4,110), (21,4,110), (22,6,120), (22,6,120), (23,8,130)
  # With distinct, should reduce to 3 unique rows
  expect_equal(nrow(result_data), 3)
  expect_equal(nrow(unique(result_data)), nrow(result_data))

  # Verify the unique combinations
  expect_true(all(c(21, 22, 23) %in% result_data$mpg))
  expect_true(all(c(4, 6, 8) %in% result_data$cyl))

  cleanup_test_app(app_dir, app)
})
