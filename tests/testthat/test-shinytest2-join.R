test_that("join block joins two datasets", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test data for join
  test_data_code <- "
    data_x <- data.frame(id = c(1, 2, 3), name = c('Alice', 'Bob', 'Charlie'))
    data_y <- data.frame(id = c(1, 2, 4), age = c(25, 30, 35))
  "

  # Test left_join
  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_join_block(type = "left_join", by = list("id" = "id")), data = list(x = data_x, y = data_y))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "join_left")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Verify we got data
  expect_true(is.data.frame(result_data))

  # Verify joined columns are present
  expect_true("name" %in% names(result_data))
  expect_true("age" %in% names(result_data))

  # Left join should keep all rows from x (3 rows)
  expect_equal(nrow(result_data), 3)

  cleanup_test_app(app_dir, app)
})

test_that("join block full restorability - different join types", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test different join types to verify type parameter restorability
  test_data_code <- "
    data_x <- data.frame(id = c(1, 2, 3), x_val = c(10, 20, 30))
    data_y <- data.frame(id = c(2, 3, 4), y_val = c(200, 300, 400))
  "

  # Test inner_join - should only keep matching rows (2 rows)
  app_dir_inner <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_join_block(type = "inner_join", by = list("id" = "id")), data = list(x = data_x, y = data_y))'
  )

  app <- shinytest2::AppDriver$new(app_dir_inner, timeout = 30000, name = "join_inner")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Inner join should only have matching rows (id 2 and 3)
  expect_equal(nrow(result_data), 2)
  expect_true(all(c("x_val", "y_val") %in% names(result_data)))

  cleanup_test_app(app_dir_inner, app)
})

test_that("join block full restorability - multiple join keys", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test multiple join keys to verify by parameter restorability
  test_data_code <- "
    data_x <- data.frame(
      id = c(1, 1, 2, 2),
      group = c('A', 'B', 'A', 'B'),
      x_val = c(10, 20, 30, 40)
    )
    data_y <- data.frame(
      id = c(1, 1, 2, 3),
      group = c('A', 'B', 'A', 'A'),
      y_val = c(100, 200, 300, 400)
    )
  "

  # Join on both id AND group
  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_join_block(
      type = "left_join",
      by = list("id" = "id", "group" = "group")
    ), data = list(x = data_x, y = data_y))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "join_multi_key")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 4 rows (all from data_x)
  expect_equal(nrow(result_data), 4)

  # Verify both join keys are present
  expect_true("id" %in% names(result_data))
  expect_true("group" %in% names(result_data))
  expect_true("x_val" %in% names(result_data))
  expect_true("y_val" %in% names(result_data))

  # Verify the join worked correctly
  # id=1, group=A should match (has y_val)
  expect_false(is.na(result_data[result_data$id == 1 & result_data$group == "A", "y_val"]))

  cleanup_test_app(app_dir, app)
})

test_that("join block full restorability - different column names", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Test joining on columns with different names
  test_data_code <- "
    data_x <- data.frame(
      user_id = c(1, 2, 3),
      user_name = c('Alice', 'Bob', 'Charlie')
    )
    data_y <- data.frame(
      id = c(1, 2, 4),
      age = c(25, 30, 35)
    )
  "

  # Join where x$user_id matches y$id
  app_dir <- create_test_app(
    data_code = test_data_code,
    block_code = 'serve(new_join_block(
      type = "left_join",
      by = list("user_id" = "id")
    ), data = list(x = data_x, y = data_y))'
  )

  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000, name = "join_diff_names")
  app$wait_for_idle()

  values <- app$get_values(export = TRUE)
  result_data <- values$export$result

  # Should have 3 rows (all from data_x)
  expect_equal(nrow(result_data), 3)

  # Verify columns are present
  expect_true("user_id" %in% names(result_data))
  expect_true("user_name" %in% names(result_data))
  expect_true("age" %in% names(result_data))

  # Verify join worked - user_id 1 and 2 should have age, 3 should be NA
  expect_false(is.na(result_data[result_data$user_id == 1, "age"]))
  expect_false(is.na(result_data[result_data$user_id == 2, "age"]))
  expect_true(is.na(result_data[result_data$user_id == 3, "age"]))

  cleanup_test_app(app_dir, app)
})
