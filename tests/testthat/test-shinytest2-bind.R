test_that("bind_rows block combines datasets vertically", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test data
  test_data_code <- "
    data_x <- data.frame(id = c(1, 2), name = c('Alice', 'Bob'))
    data_y <- data.frame(id = c(3, 4), name = c('Charlie', 'David'))
  "

  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_bind_rows_block(), data = list(x = data_x, y = data_y))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "bind_rows")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))

  # Verify rows were combined (2 + 2 = 4)
  expect_equal(nrow(result_data), 4)

  # Verify columns are present
  expect_true(all(c("id", "name") %in% names(result_data)))

  cleanup_test_app(app_dir, app)
})

test_that("bind_cols block combines datasets horizontally", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test data with same number of rows
  test_data_code <- "
    data_x <- data.frame(id = c(1, 2, 3))
    data_y <- data.frame(name = c('Alice', 'Bob', 'Charlie'))
  "

  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_bind_cols_block(), data = list(x = data_x, y = data_y))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "bind_cols")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))

  # Verify columns were combined
  expect_equal(ncol(result_data), 2)
  expect_true(all(c("id", "name") %in% names(result_data)))

  # Verify row count unchanged
  expect_equal(nrow(result_data), 3)

  cleanup_test_app(app_dir, app)
})
