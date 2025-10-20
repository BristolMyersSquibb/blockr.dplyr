test_that("slice block selects first rows (head)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test slice_head first 5 rows
  app_dir <- create_test_app(
    block_code = 'serve(new_slice_block(type = "head", n = 5), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "slice_head"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))

  # Verify correct number of rows
  expect_equal(nrow(result_data), 5)

  # Verify all columns still present
  expect_equal(ncol(result_data), ncol(mtcars))

  # Verify it's the first 5 rows (same data)
  expect_equal(result_data, mtcars[1:5, ])

  cleanup_test_app(app_dir, app)
})

test_that("slice block full restorability - tail with n parameter", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test slice_tail with n parameter
  app_dir <- create_test_app(
    block_code = 'serve(new_slice_block(type = "tail", n = 3), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "slice_tail"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got last 3 rows
  expect_equal(nrow(result_data), 3)
  expect_equal(result_data, mtcars[(nrow(mtcars) - 2):nrow(mtcars), ])

  cleanup_test_app(app_dir, app)
})

test_that("slice block full restorability - with grouping (by parameter)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test slice with by (grouping) parameter - take first 2 rows per group
  # Note: Similar to mutate/summarize, the `by` parameter uses a sub-module
  # which is properly initialized in production but requires shinytest2 to verify
  app_dir <- create_test_app(
    block_code = 'serve(new_slice_block(type = "head", n = 2, by = "cyl"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "slice_grouped"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 2 rows per unique cyl value
  n_groups <- length(unique(mtcars$cyl))
  expect_equal(nrow(result_data), n_groups * 2)

  # Verify each group has exactly 2 rows
  for (cyl_val in unique(result_data$cyl)) {
    cyl_count <- sum(result_data$cyl == cyl_val)
    expect_equal(cyl_count, 2)
  }

  cleanup_test_app(app_dir, app)
})

test_that("slice block full restorability - custom rows parameter", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test custom slice with rows parameter
  app_dir <- create_test_app(
    block_code = 'serve(new_slice_block(type = "custom", rows = "c(1, 5, 10, 15)"), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "slice_custom"
  )
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have exactly 4 rows (positions 1, 5, 10, 15)
  expect_equal(nrow(result_data), 4)

  # Verify the rows match
  expect_equal(result_data, mtcars[c(1, 5, 10, 15), ])

  cleanup_test_app(app_dir, app)
})
