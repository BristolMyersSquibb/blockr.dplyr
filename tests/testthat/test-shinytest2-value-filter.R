test_that("value_filter block filters by selected values", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test value filter on cyl column, selecting only 4 and 6 cylinders
  app_dir <- create_test_app(
    block_code = 'serve(new_value_filter_block(conditions = list(list(column = "cyl", values = c(4, 6), mode = "include"))), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "value_filter")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))

  # Verify only selected values are present
  expect_true(all(result_data$cyl %in% c(4, 6)))

  # Verify no 8 cylinder cars (which exist in mtcars)
  expect_false(any(result_data$cyl == 8))

  # Verify all columns still present
  expect_equal(ncol(result_data), ncol(mtcars))

  cleanup_test_app(app_dir, app)
})
