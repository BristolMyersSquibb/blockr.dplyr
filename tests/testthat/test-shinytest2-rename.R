test_that("rename block renames columns", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test rename mpg to miles_per_gallon
  app_dir <- create_test_app(
    block_code = 'serve(new_rename_block(list(miles_per_gallon = "mpg")), data = list(data = mtcars))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "rename_column")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))

  # Verify new column name exists
  expect_true("miles_per_gallon" %in% names(result_data))

  # Verify old column name is gone
  expect_false("mpg" %in% names(result_data))

  # Verify data is unchanged (just renamed)
  expect_equal(result_data$miles_per_gallon, mtcars$mpg)

  # Verify same number of columns
  expect_equal(ncol(result_data), ncol(mtcars))

  cleanup_test_app(app_dir, app)
})
